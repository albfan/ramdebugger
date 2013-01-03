
package require tdom
package require htmlparse

namespace eval html2tdom {
    variable xsltcmd
    variable topdir [file dirname [info script]]
}

proc html2tdom::go { args } {
    
    set optional {
	{ -no_tables "" 0 }
	{ -statustextcmd cmd "" }
	{ -baseurl url "" }
	{ -images imagesDict "" }
    }
    set compulsory "html"
    parse_args $optional $compulsory $args
    
    set doc [parse $html]
    
    regexp {(?in)^\s*SourceURL:\s*(.+)$} [string range $html 0 1000] {} baseurl
    if { [regexp {^file://(.*)} $baseurl {} path] && ![regexp {^file:///} $baseurl] } {
	set baseurl "file:///$path"
    }
#     if {[regexp {^file://(.*)} $baseurl {} path] } {
#         set baseurl "file://[file normalize $path]"
#     }
    foreach entryNode [$doc selectNodes //entry] {
	if { [$entryNode selectNodes ancestor::row] ne "" } { continue }
	set rowNode [$doc createElement row]
	[$entryNode parentNode] insertBefore $rowNode $entryNode
	$rowNode appendChild $entryNode
	foreach entryNode_i [$rowNode selectNodes following-sibling::entry] {
	    $rowNode appendChild $entryNode_i
	}
    }
    foreach rowNode [$doc selectNodes //row] {
	if { [$rowNode selectNodes ancestor::table] ne "" } { continue }
	set tableNode [$doc createElement table]
	[$rowNode parentNode] insertBefore $tableNode $rowNode
	$tableNode appendChild $rowNode
	foreach rowNode_i [$tableNode selectNodes following-sibling::row] {
	    $tableNode appendChild $rowNode_i
	}
    }
    foreach i [$doc selectNodes "//mediaobject/imageobject/imagedata"] {
	set fileref [uri::resolve $baseurl [$i @fileref]]
	if { ![file exists $fileref] } {
	    set fileref [uri::resolve $baseurl $fileref]
	    #regsub {///} $fileref {//} fileref
	    
	    if { [regexp {^file://(.*)} $fileref {} filename] } {
		regsub -all {[^/]+/../} $filename {} filename
		regsub {^/} $filename {} filenameR
		if { [dict exists $images $filename] } {
		    set img [dict get $images $filename]
		} elseif { [dict exists $images $filenameR] } {
		    set img [dict get $images $filenameR]
		} else {
		    set img [image create photo -file $filename]
		}
	    } else {
		if { $statustextcmd ne "" } {
		    uplevel #0 $statustextcmd [list \
		            [_ "Retrieving '%s'..." $fileref]]
		}
		set err [catch { uri::geturl $fileref -timeout 4000 } tok]
		if { $err } {
		    error [_ "error: could not resolve image '%s' as '%s'" [$i @fileref] $fileref]
		}
		if { $statustextcmd ne "" } {
		    uplevel #0 $statustextcmd [list ""]
		}
		if { [info exists ${tok}(data)] } {
		    set img [image create photo -data [set ${tok}(data)]]
		} elseif { [info exists ${tok}(body)] } {
		    set img [image create photo -data [set ${tok}(body)]]
		} else {
		    error [_ "error: could not resolve image '%s'" $fileref]
		}
		array unset tok
	    }
	}
	$i setAttribute fileref $img
    }
    if { $no_tables } {
	foreach tableNode [$doc selectNodes //table] {
	    set parentNode [$tableNode parentNode]
	    foreach entryNode [$tableNode selectNodes row/entry|tgroup/*/row/entry] {
		foreach node [$entryNode childNodes] {
		    $parentNode insertBefore $node $tableNode
		}
		$entryNode delete
	    }
	    $tableNode delete
	}
    }
    return $doc
}

proc html2tdom::parse { html } {
    variable xsltcmd
    variable topdir

    set baseurl ""
    regexp {(?in)^\s*SourceURL:\s*(.+)$} [string range $html 0 200] {} baseurl
    regsub -all {<!\[if\s+?(!supportLists|gte vml 1)\].*?<!\[endif\]>} $html {} html
    regsub -all {<!\[if\s+?[^\]]*?\]>} $html {} html
    regsub -all {<!\[endif\]>} $html {} html
    regsub -all {<!--StartFragment-->} $html {<StartFragment></StartFragment>} html
    regsub -all {<!--EndFragment-->} $html {<EndFragment></EndFragment>} html

    catch {html2tdom::tags destroy}

    ::struct::stack html2tdom::tags

    set doc [dom createDocument html]
    html2tdom::tags push [$doc documentElement]
    ::htmlparse::parse -cmd [list html2tdom::2tdomCallback $doc] $html
    Reorder [$doc documentElement]
    html2tdom::tags destroy

    foreach node [$doc selectNodes {//*[@style]}] {
	set font-family [_give_syle_item [$node @style] font-family]
	if { ${font-family} eq "symbol" } {
	    foreach descendantNode [$node selectNodes .//text()] {
		package require xml2pdf
		set t [xml2pdf::symbol2unicode [$descendantNode nodeValue]]
		$descendantNode nodeValue $t
	    }
	}
    }
    set root [$doc documentElement]
    if { [$root selectNodes body|BODY] eq "" } {
	set body [$doc createElement body]
	$root appendChild $body
	foreach node [$root childNodes] {
	    if { $node eq $body } { continue }
	    $body appendChild $node
	} 
    }
    
    #unset -nocomplain xsltcmd; #DEBUG
    if { ![info exists xsltcmd] } {
	set filename [file join $topdir html2docbook.xslt]
	set xsltdoc [cu::dom::parse [tDOM::xmlReadFile $filename]]
	set xsltcmd [$xsltdoc toXSLTcmd]
    }
    set newdoc [$xsltcmd $doc]
    set root [$newdoc documentElement]
    
    set paraNode ""
    foreach node [$root childNodes] {
	if { [$node nodeType] ne "TEXT_NODE" } {
	    set paraNode ""
	    continue
	}
	if { $paraNode eq "" } {
	    set paraNode [$newdoc createElement para]
	    $root insertBefore $paraNode $node
	}
	$paraNode appendChild $node
    }
    
    set todelete ""
    foreach node [$newdoc selectNodes //br] {
	set paraNode [$node selectNodes {ancestor::para[1]}]
	if { $paraNode ne "" && [$paraNode parentNode] ne "" } {
	    set para2Node [$newdoc createElement para]
	    [$paraNode parentNode] insertAfter $para2Node $paraNode
	    foreach n [$node selectNodes following-sibling::node()] {
		$para2Node appendChild $n
	    }
	    if { [$paraNode selectNodes {normalize-space(string(.))}] eq "" } {
		lappend todelete $paraNode
		[$paraNode parentNode] removeChild $paraNode
	    }
	    if { [$para2Node selectNodes {normalize-space(string(.))}] eq "" } {
		lappend todelete $para2Node
		[$para2Node parentNode] removeChild $para2Node
	    }
	}
	$node delete
    }
    foreach i $todelete { $i delete }
    $doc delete

    # some clean up
    
    set xp {//listitem/*[last() and name()='para']}
    foreach node [$newdoc selectNodes $xp] {
	$node delete
    }
    
    foreach node [$newdoc selectNodes {//text()}] {
	regsub -all {\s+} [$node nodeValue] { } t
	if { [$node selectNodes ancestor::para] eq "" &&
	    [string trim $t] eq "" } {
	    $node delete
	} else {
	    $node nodeValue $t
	}
    }
    
    foreach tableNode [$newdoc selectNodes //table] {
	set xp "row|tgroup/*/row"
	set rows [$tableNode selectNodes $xp]
	set nrows [llength $rows]
	set ncols 0
	foreach row $rows {
	    set entryNode [$row selectNodes entry]
	    set ncols_i [llength $entryNode]
	    if { $ncols_i > $ncols } { set ncols $ncols_i }
	}
	if { $nrows == 1 && $ncols == 1 } {
	    foreach n [$entryNode childNodes] {
		[$tableNode parentNode] insertBefore $n $tableNode
	    }
	    $tableNode delete
	}
    }
    
    return $newdoc
}

proc html2tdom::_give_syle_item { style item } {
    foreach i [split $style ";"] {
	foreach "n v" [split $i :] {
	    set n [string trim [string tolower $n]]
	    set v [string trim [string tolower $v]]
	    if { $item eq $n } {
		return $v
	    }
	}
    }
    return ""
}

proc html2tdom::2tdomCallback { doc tag slash param textBehindTheTag } {

    # Normalize tag information for later comparisons. Also remove
    # superfluous whitespace. Don't forget to decode the standard
    # entities.

    set tag [string tolower $tag]
    if { $tag eq "hmstart" } { return }
    if { $tag eq "?xml" } { return }
    if {$textBehindTheTag ne "" } {
	set textBehindTheTag [htmlparse::mapEscapes $textBehindTheTag]
	regsub -all {\s*\n\s*} $textBehindTheTag { } textBehindTheTag

    }

    if { $slash eq "/"} {
	# Handle closing tags. Standard operation is to pop the tag
	# from the stack of open tags. We don't do this for </p> and
	# </li>. As they were optional they were never pushed onto the
	# stack (Well, actually they are just popped immediately after
	# they were pusheed, see below).

	switch -exact -- $tag {
	    base - option - meta - li - p {
		# Ignore, nothing to do.                
	    }
	    default {
		# The moment we get a closing tag which does not match
		# the tag on the stack we have two possibilities on how
		# this came into existence to choose from:
		#
		# a) A tag is now closed but was never opened.
		# b) A tag requiring an end tag was opened but the end
		#    tag was omitted and we now are at a tag which was
		#    opened before the one with the omitted end tag.

		# NOTE:
		# Pages delivered from the amazon.uk site contain both
		# cases: </a> without opening, <b> & <font> without
		# closing. Another error: <a><b></a></b>, i.e. overlapping
		# tags. Fortunately this can be handled by the algorithm
		# below, in two cycles, one of which is case (b), followed
		# by case (a). It seems as if Amazon/UK believes that visual
		# markup like <b> and <font> is an option (switch-on) instead
		# of a region.

		# Algorithm used here to deal with these:
		# 1) Search whole stack for the matching opening tag.
		#    If there is one assume case (b) and pop everything
		#    until and including this opening tag.
		# 2) If no matching opening tag was found assume case
		#    (a) and ignore the tag.
		#
		# Part (1) also subsumes the normal case, i.e. the
		# matching tag is at the top of the stack.

		set nodes [html2tdom::tags peek [html2tdom::tags size]]
		# Note: First item is top of stack, last item is bottom of stack !
		# (This behaviour of tcllib stacks is not documented
		# -> we should update the manpage).
		
#                 foreach n $nodes {lappend tstring [$n nodeName]}
#                 puts stderr ----[join $tstring]----

		set level 1
		set found 0
		foreach n $nodes {
		    set name [$n nodeName]
		    if {0 == [string compare $tag $name]} {
		        # Found an earlier open tag -> (b).
		        set found 1
		        break
		    }
		    incr level
		}
		if {$found} {
		    html2tdom::tags pop $level
		    if {$level > 1} {
		        #foreach n $nodes {lappend tstring [$tree get $n type]}
		        #puts stderr "\tdesync at <$tag> ($tstring) => pop $level"
		    }
		} else {
		    #foreach n $nodes {lappend tstring [$tree get $n type]}
		    #puts stderr "\tdesync at <$tag> ($tstring) => ignore"
		}
	    }
	}

	# If there is text behind a closing tag X it belongs to the
	# parent tag of X.

	if {$textBehindTheTag ne "" } {
	    # Attach the text behind the closing tag to the reopened
	    # context.

	    set parentNode [html2tdom::tags peek]
	    set newTextNode [$doc createTextNode $textBehindTheTag]
	    $parentNode appendChild $newTextNode
	}

    } else {
	# Handle opening tags. The standard operation for most is to
	# push them onto the stack and thus open a nested context.
	# This does not happen for both the optional tags (p, li) and
	# the ones which don't have closing tags (meta, br, option,
	# input, area, img).
	#
	# The text coming with the tag will be added after the tag if
	# it is a tag without a matching close, else it will be added
	# as a node below the tag (as it is the region between the
	# opening and closing tag and thus nested inside). Empty text
	# is ignored under all circcumstances.

	set parentNode [html2tdom::tags peek]

	set tagsList [list td tr]
	while 1 {
	    set pos1 [lsearch -exact $tagsList $tag]
	    set pos2 [lsearch -exact $tagsList [$parentNode nodeName]]
	    if { $pos1 == -1 || $pos2 == -1 || $pos1 < $pos2 } { break }
	    html2tdom::tags pop
	    set parentNode [html2tdom::tags peek]
	}

	set rex {(\w+)\s*=\s*([^\s\"]+|\"[^\"]*\"|\'[^\']*\')}
	foreach "- name value" [regexp -inline -all $rex $param] {
	    set atts($name) [string trim [htmlparse::mapEscapes $value] '\"]
	}
	set rex {mso-list:\S+\s+level(\d+)}
	if { $tag eq "html" } {
	    set newNode $parentNode
	    if { [array size atts] } { eval [list $newNode setAttribute] [array get atts] }
	} elseif { [info exists atts(style)] && [regexp $rex $atts(style) {} level] } {
	    set newNode $parentNode
	    for { set i 1 } { $i < $level } { incr i } {
		$newNode appendChild [set n [$doc createElement blockquote]]
		set newNode $n
	    }
	    $newNode appendChild [set n [$doc createElement ul]]
	    set newNode $n
	    if { [array size atts] } { eval [list $newNode setAttribute] [array get atts] }
	    $newNode appendChild [set n [$doc createElement li]]
	    set newNode $n
	} elseif { [regexp {^\?} $tag] } {
	    set newNode [$doc createElement span]
	    $parentNode appendChild $newNode
	} else {
	    regsub -all {\W} $tag {_} tag
	    set newNode [$doc createElement $tag]
	    if { [array size atts] } { eval [list $newNode setAttribute] [array get atts] }
	    $parentNode appendChild $newNode
	}

	if {$textBehindTheTag ne "" } {
	    switch -exact -- $tag {
		input - area - img - br {
		    set parent $parentNode
		}
		default {
		    set parent $newNode
		}
	    }
	    set newTextNode [$doc createTextNode $textBehindTheTag]
	    $parent appendChild $newTextNode
	}
	html2tdom::tags push $newNode

	# Special handling: <p>, <li> may have no closing tag => pop
	#                 : them immediately.
	#
	# Special handling: <meta>, <br>, <option>, <input>, <area>,
	#                 : <img>: no closing tags for these.

	switch -exact -- $tag {
	    hr - base - meta - li - br - option - input - area - img - p - h1 - h2 - h3 - h4 - h5 - h6 {
		html2tdom::tags pop
	    }
	    default {}
	}
    }
}

proc html2tdom::Reorder { node } {
    set tags [list h1 h2 h3 h4 h5 h6 p li]
    switch -exact -- [$node nodeName] {
	p - li {
	    # h1 - h2 - h3 - h4 - h5 - h6 - p - li 
	    # Look for right siblings until the next node with the
	    # same type (or end of level) and move these below this
	    # node.

	    while {1} {
		set sibling [$node nextSibling]
		if { $sibling eq "" || [lsearch -exact $tags [$sibling nodeName]] != -1 } {
		    break
		}
		$node appendChild $sibling
	    }
	}
	default {
	    # Ignore tag
	}
    }
    foreach childNode [$node childNodes] {
	Reorder $childNode
    }
}

namespace eval docbook2txt {
    variable xsltcmd_txt
    variable xsltcmd_wiki
    variable topdir [file dirname [info script]]
}

proc docbook2txt::go { args } {
    variable xsltcmd_txt
    variable xsltcmd_wiki
    variable topdir
    
    set optional {
	{ -output_style text|wiki text }
	{ -debug "" 0 }
	{ -title title "" }
    }
    set compulsory "xml"
    parse_args $optional $compulsory $args

    set doc [cu::dom::parse $xml]
    
    switch $output_style {
	text {
	    upvar 0 xsltcmd_txt xsltcmd
	    set file docbook2txt.xslt
	}
	wiki {
	    upvar 0 xsltcmd_wiki xsltcmd
	    set file docbook2wiki.xslt
	}
    }
    
    if { [$doc selectNodes {//emphasis}] eq "" } {
	set xml_title "<para><emphasis role='header'>[lindex [split $title >] end]</emphasis></para>"
	::dom::domNode::appendFirstXML [$doc firstChild] $xml_title
    }    

    if { ![info exists xsltcmd] } {
	set filename [file join $topdir $file]
	set xsltdoc [cu::dom::parse [tDOM::xmlReadFile $filename]]
	set xsltcmd [$xsltdoc toXSLTcmd]
    }
    
    set newdoc [$xsltcmd $doc]
    $doc delete
    set txt [$newdoc asText]
    
    if { $debug } {
	$xsltcmd delete
	unset xsltcmd
    }

    # this is a dirty trick because tdom XSLT does not work
    # well when there is only space inside <xsl:text>
    regsub -all {[\n]} $txt {} txt
    regsub -all -- {---NEWLINE---} $txt "\n" txt
    $newdoc delete
    return $txt
}

proc docbook2txt::_wiki2docbook_para { txt } {
    variable listStack
    
    set _ ""
    if { [regexp {^\s*(\*+)\s*(\S.*)} $txt {} stars txt] } {
	set level [string length $stars]
	set lType itemizedlist
    } elseif { [regexp {^\s*(\#+)s*(\S.*)} $txt {} stars txt] } {
	set level [string length $stars]
	set lType orderedlist
    } elseif { [regexp {^\s*==\s*(\S.*\S)==} $txt {} txt] } {
	set level 0
	set lType header
    } else {
	set level 0
	set lType para
    }
    while { [llength $listStack] > $level } {
	append _ "</[lindex $listStack end]>"
	set listStack [lrange $listStack 0 end-1]
    }
    while { [llength $listStack] < $level } {
	append _ "<$lType>"
	lappend listStack $lType
    }
    if { $level > 0 } {
	append _ "<listitem>"
    } else {
	append _ "<para>"
    }
    if { $lType eq "header" } {
	append _ "<emphasis role='header'>"
    }
    set map [list "&#58;" : "&#124;" | "&#93;" "\]" "&#32;" " "]
    
    set rex1 {\[\[(.*?)\]\]}
    set rex2 {\[([^\[].*?)\]}
    set rex3 {(<(tcl|formula)\s.*?>.*?</\2>)}
    set rex4 {''(.+?)''}
    set rex "(?:$rex1|$rex2|$rex3|$rex4)"
    
    while { [set ret [regexp -inline -indices $rex $txt]] ne "" } {
	lassign [lindex $ret 0] i0 i1
	append _ [xml_map1 [string range $txt 0 $i0-1]]
	
	if { [lindex $ret 1 0] != -1 } {
	    set link [string range $txt {*}[lindex $ret 1]]
	    if { [regexp {^(File|Localfile|Indexterm|Image|ImageFile):([^|]+)} $link {} lt _link] } {
		switch $lt {
		    File { set linktype file }
		    Localfile { set linktype localfile }
		    Indexterm { set linktype indexterm }
		    Image { set linktype image }
		    ImageFile { set linktype imagefile }
		}
	    } else {
		set linktype local
	    }
	    
	    #In order to preserve the original size of the linked object
	    #if { ![regexp {(Align):([^\s]+)} $link {} attr align] } { set align "" }
	    #if { ![regexp {(Width):([^\s]+)} $link {} attr width] } { set width "" }
	    set width ""
	    set align ""
	    if { [llength [split $link |]] == 2 } {
	       set width [lindex [split $link |] end]
	    } else {
	       set width [lindex [split $link |] 1]
	       set align [lindex [split $link |] 2]
	       if { $align eq "middle" } { set align "center" }
	    }
	    
	    if { ![regexp {(.*)\|(.*)} $_link {} _link text] } { set text "" }
	    set link [string map $map $_link]
	    set text [string map $map $text]
	    if { $linktype in "image imagefile" } {
		if { $linktype eq "image" } { set link "local://$link" }
		append _ "<mediaobject><imageobject>"
		#append _ [format_xml "<imagedata fileref='%s'/></imageobject>" $link]
		append _ [format_xml "<imagedata fileref='%s' align='%s' \
		        width='%s'/></imageobject>" $link $align $width]
		if { $text ne "" } {
		    append _ "<caption>[xml_map1 $text]</caption>"
		}
		append _ "</mediaobject>"                
	    } else {
		if { [string trim $text] eq "" } { set text $link }
		append _ [format_xml "<ulink linktype='%s' url='%s'>" $linktype $link]
		append _ [xml_map1 $text] "</ulink>"
	    }
	} elseif { [lindex $ret 2 0] != -1 } {
	    set link [string range $txt {*}[lindex $ret 2]]
	    if { ![regexp {^(\S+)\s(.*)} $link {} link text] } {
		set text ""
	    }
	    set link [string map $map $link]
	    set text [string map $map $text]
	    if { [string trim $text] eq "" } { set text $link }
	    append _ [format_xml "<ulink linktype='url' url='%s'>" $link]
	    append _ [xml_map1 $text] "</ulink>"
	} elseif { [lindex $ret 3 0] != -1 } {
	    set xml [string range $txt {*}[lindex $ret 3]]
	    set err [catch { cu::dom::parse $xml } doc]
	    if { !$err } {
		append _ [$doc asXML]
	    } else {
		append _ [xml_map1 $xml]
	    }
	    $doc delete
	} elseif { [lindex $ret 5 0] != -1 } {
	    set text [string range $txt {*}[lindex $ret 5]]
	    if { [regexp {^'(.*)'$} $text {} text] } {
		set role strong
	    } else {
		set role italic
	    }
	    append _ [format_xml "<emphasis role='%s'>" $role] \
		[xml_map1 $text] "</emphasis>"
	}
	set txt [string range $txt $i1+1 end]
    }
    regsub -all {[\s]*$} $txt {} txt
    append _ [xml_map1 $txt]
	
    if { $lType eq "header" } {
	append _ "</emphasis>"
    }
    if { $level > 0 } {
	append _ "</listitem>"
    } else {
	append _ "</para>"  
    }

    return $_
}

proc docbook2txt::wiki2docbook { args } {
    variable listStack
    
    set optional {
	{ -include_main_node boolean 1 }
    }
    set compulsory "txt"
    parse_args $optional $compulsory $args

    
    if { $include_main_node } {
	set _ "<lognoter>"
    } else {
	set _ ""
    }
    
    lassign "" last_txt last_para listStack
    
    foreach line [split $txt \n] {
	if { [string trim $line] eq "" && ![regexp {^\s*==.*==} $last_para]} {
	    set ispara 1
	} elseif { [regexp {^\s*\*} $line] } {
	    set ispara 1
	} elseif { [regexp {^\s*==.*==} $line] } {
	    set ispara 1
	} else {
	    set ispara 0
	}
	if { $ispara } {
	    if { $last_txt ne "" } { append last_txt " " }
	    append last_txt [string trim $line]
	    append _ [_wiki2docbook_para $last_txt]
	    set last_para $last_txt
	    set last_txt ""
	} else {
	    set last_para ""
	    set last_txt $line
	}
    }

    append _ [_wiki2docbook_para $last_txt]

    if { $include_main_node } {
	append _ "</lognoter>"
    }

     return $_
}


#This is probably also a dirty trick intended to preserve relation of spaces between 
#wiki text and html text

proc docbook2txt::wiki-html_spaces { dom } {
    
    set prevent_space 0
    foreach node [$dom selectNodes {//para}] {
	if { $node eq [lindex [$dom selectNodes {//para}] 0] } { continue }
	if { ![$node hasChildNodes] && ![[$node previousSibling] hasChildNodes] } {
	    if { !$prevent_space } {
		set prevent_space 1
		$node delete
	    } else {
		set prevent_space 0
	    }
	} else {
	    set prevent_space 0
	}    
     }
     set last_node [lindex [$dom selectNodes {//para}] end]
     if { ![$last_node hasChildNodes]} {
	$last_node delete
     }
    
     return $dom   
}



















