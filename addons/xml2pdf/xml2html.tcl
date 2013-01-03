
#package require resizer
package require pdfwriter
package require tdom
package require compass_utils

package provide xml2pdf 1.3
#source [file join [file dirname [info script]] tdom-mod.tcl]
#source [file join [file dirname [info script]] rtflib.tcl]

# if { [llength [info command lrepeat]] == 0 } {
#     proc lrepeat { count element } {
#         set retval ""
#         for { set i 0 } { $i < $count } { incr i } {
#             lappend retval $element
#         }
#         return $retval
#     }
# }

namespace eval xml2html {
    variable itemslist
    variable image_counter
}

proc xml2html::_InitializeItemsList { } {
    variable itemslist 
    set itemslist [lsort -ascii [list \#text blockquote caption emphasis \
		imagedata imageobject itemizedlist listitem literal mediaobject \
		orderedlist para subscript superscript sub sup \
		table ulink beginpage]]
    #ulink and beginpage were in xml2pdf, they must be also considered for html?    
}

proc xml2html::image_base_name { what } {
    variable image_counter
    
    switch $what {
	"reset" {
	    set image_counter 0
	}
	"get" {
	    return image[format "%02d" [incr image_counter]]
	}
    }
}

proc xml2html::TextdataToHTML { domNodes htmlNode } {
    variable state
    variable indexterm
    variable idpages
    variable indexterm

    set len [llength $domNodes]
    for { set i 0 } { $i < $len } { incr i } {
	set domNode [lindex $domNodes $i]
	switch [$domNode nodeName] {
	    "blockquote" {
		if { [$domNode childNodes] ne "" } {
		    set blockquote [[$htmlNode ownerDocument] createElement blockquote]
		    $htmlNode appendChild $blockquote
		    TextdataToHTML [$domNode childNodes] $blockquote
		}
	    }
	    "itemizedlist" {
		set ul [[$htmlNode ownerDocument] createElement ul]
		$htmlNode appendChild $ul
		TextdataToHTML [$domNode childNodes] $ul
	    }
	    "orderedlist" {
		switch [$domNode getAttribute numeration arabic] {
		    arabic { set type 1 }
		    loweralpha  { set type a }
		    upperalpha { set type A }
		    lowerroman { set type i }
		    upperroman { set type I }
		}
		if { ![info exists state(orderedlist_numList)] } {
		    set state(orderedlist_numList) 0
		}
		if { [string tolower [$domNode @continuation restarts]] eq "restarts" } {
		    set num 1
		} else {
		    set num [lindex $state(orderedlist_numList) end]
		    incr num
		}
		set ul [[$htmlNode ownerDocument] createElement ol]
		$htmlNode appendChild $ul
		$ul setAttribute type $type start $num
		set state(orderedlist_numList) [lrange $state(orderedlist_numList) 0 end-1]
		lappend state(orderedlist_numList) $num 0
		TextdataToHTML [$domNode childNodes] $ul
		set state(orderedlist_numList) [lrange $state(orderedlist_numList) 0 end-1]
	    }          
	    "listitem" {
		set li [[$htmlNode ownerDocument] createElement li]
		$htmlNode appendChild $li
		TextdataToHTML [$domNode childNodes] $li
	    }
	    "para" {
		set p [[$htmlNode ownerDocument] createElement p]
		$htmlNode appendChild $p
		TextdataToHTML [$domNode childNodes] $p
	    }
	    "emphasis" {
		set done 0
		if { [$domNode getAttribute role ""] eq "header" } {
		    set tag [[$htmlNode ownerDocument] createElement h3]
		} elseif { [$domNode getAttribute role ""] in "strong bold" } {
		    set tag [[$htmlNode ownerDocument] createElement b]
		} elseif { [$domNode getAttribute role ""] == "warning" } {
		    set tag [[$htmlNode ownerDocument] createElement font]
		    $tag setAttribute color red
		} elseif { [$domNode getAttribute role ""] eq "formula" } {
		    set myformula ""
		    foreach n [$domNode childNodes] { append myformula [$n asXML -indent none] }
		    
		    if { [drawformula::is_complex_formula $myformula] } {
		        set file [file join $state(imagestodir) [image_base_name get]].png
		        if { [set! state(output_style)] ne "epub" } {
		            set gd_file $file
		        } else {
		            set gd_file [cu::file::tempfile drawformulas .png]
		        }
		        set dr [drawformula::Init image "" Arial 10 black]
		        $dr configure -image_gd_file $gd_file
		        lassign [$dr draw 10 $myformula 1 0 0 400 \
		                40] wL hup hdown
		        lassign [$dr draw 10 $myformula 1 0 0 400 \
		                [expr {$hup+$hdown}]] wL hup hdown
		                                
		        
		        if { [set! state(output_style)] eq "epub" } {
		            set err [catch {
		                    set fin [open $gd_file r]
		                    fconfigure $fin -translation binary
		                    set data [read $fin]
		                    close $fin
		                    file delete $gd_file
		                    cu::zip set $state(zipid) $file -level 9 -time $state(zipnow)
		                    cu::zip write $state(zipid) $data
		                }]
		        }
		        set file [cu::file::make_path_relative $state(basedir_dest) $file]

		        set img [[$htmlNode ownerDocument] createElement img]
		        $img setAttribute src $file
		        $htmlNode appendChild $img
		        set done 1
		    } else {
		        set tag [[$htmlNode ownerDocument] createElement i]
		    }
		} else {
		    set tag [[$htmlNode ownerDocument] createElement i]
		}
		if { !$done } {
		    $htmlNode appendChild $tag
		    TextdataToHTML [$domNode childNodes] $tag
		}
	    }
	    "superscript" - sup {
		set sup [[$htmlNode ownerDocument] createElement sup]
		$htmlNode appendChild $sup
		TextdataToHTML [$domNode childNodes] $sup
	    }
	    "subscript" - sub {
		set sub [[$htmlNode ownerDocument] createElement sub]
		$htmlNode appendChild $sub
		TextdataToHTML [$domNode childNodes] $sub
	    }
	    "table" {
		set center [[$htmlNode ownerDocument] createElement center]
		$htmlNode appendChild $center
		set tableNode [[$htmlNode ownerDocument] createElement table]
		#$tableNode setAttribute style "border:solid #6699cc 1.0pt;" border 1
		$tableNode setAttribute border 0
		$center appendChild $tableNode
		foreach j [$domNode selectNodes row|tgroup/*/row] {
		    set tr [[$htmlNode ownerDocument] createElement tr]
		    $tableNode appendChild $tr
		    foreach k [$j selectNodes entry] {
		        set td [[$htmlNode ownerDocument] createElement td]
		        $tr appendChild $td
		        TextdataToHTML [$k childNodes] $td
		    }
		}
	    }
	    "mediaobject" {
		set data_type ""
		set width ""
		set file ""
		set align center
		set image_width ""
		foreach imagedataNode [$domNode getElementsByTagName imagedata] {
		    if { [$imagedataNode @inlinedata ""] != "" } {
		        set data_type inlineimage
		        set data [$imagedataNode @inlinedata]
		    } elseif { [$imagedataNode @fileref ""] != "" } {
		        set file [$imagedataNode @fileref ""]
		        if { [lsearch [image names] $file] == -1 } {
		            set file [file normalize [file join $state(basedir) $file]]
		            if { ![file exists $file] } {
		                set file no_image
		                set data_type image
		            } else {
		                set data_type file
		                set img [image create photo -file $file]
		                set image_width [image width $img]
		                image delete $img
		            }
		        } else {
		            set data_type image
		        }
		        set data $file
		        set width [$imagedataNode @width ""]
		    }
		    set align [$imagedataNode @align "center"]
		}
		set ns { svg http://www.w3.org/2000/svg }
		foreach svgNode [$domNode selectNodes -namespaces $ns imageobject/svg:svg] {
		    set data_type svg
		    set data $svgNode
		    set width [$svgNode @width]
		}
		if { $data_type ne "file" } {
		    set bname [file join $state(imagestodir) [image_base_name get]]
		    switch $data_type {
		        "inlineimage" - "image" {
		            switch $data_type {
		                "inlineimage" { set img [image create photo -data $data] }
		                "image" { set img $data }
		            }
		            foreach format [list png gif jpeg] {
		                switch $format {
		                    png { set imgfile $bname.png }
		                    gif { set imgfile $bname.gif }
		                    jpeg { set imgfile $bname.jpg }
		                }
		                if { [set! state(output_style)] ne "epub" } {
		                    set err [catch { $img write $imgfile -format $format } ]
		                    if { $err } { file delete $imgfile }
		                } else {
		                    set err [catch {
		                            set data [base64::decode [$img data -format $format]]
		                            cu::zip set $state(zipid) $imgfile -level 9 -time $state(zipnow)
		                            cu::zip write $state(zipid) $data
		                        }]
		                }
		                if { !$err } { break }
		            }
		            set image_width [image width $img]
		            if { $data_type eq "inlineimage" } { image delete $img }
		            set data $imgfile
		        }
		        "svg" {
		            set svgNode $data
		            set width [$svgNode @width ""]
		            if { $width eq "" } { set width "100%" }
		            set height [$svgNode @height ""]
		            if { $height eq "" } {
		                set viewBox [$svgNode @viewBox]
		                set height [expr {$width*[lindex $viewBox 3]/
		                        [lindex $viewBox 2]}]
		            }
		            set parent_tableNode [$domNode selectNodes ancestor::table]
		            set pagewidth 800
		            set pagewidth_med 420
		            if { $parent_tableNode ne "" } {
		                set numcols [llength [$parent_tableNode selectNodes \
		                            {.//row[1]/entry}]]
		                if { $numcols < 1 } { set numcols 1 }
		                set pagewidth [expr {$pagewidth/double($numcols)}]
		                set pagewidth_med [expr {$pagewidth_med/double($numcols)}]
		            }                            
		            if { [regexp {([\d.]+)%} $width {} width_percent] } {
		                set width [expr {int($width_percent/100.0*$pagewidth_med)}]
		            }
		            if { [regexp {([\d.]+)%} $height {} height_percent] } {
		                set height [expr {int($height_percent/100.0*$pagewidth_med)}]
		            }
		            if { [string is integer $width] && $width > $pagewidth } {
		                set width $pagewidth
		            }
		            if { [string is integer $height] && $height > $pagewidth } {
		                set height $pagewidth
		            }
		            $svgNode setAttribute width $width
		            $svgNode setAttribute height $height

		            set svg2draw [svg2draw %AUTO% -svgnode $data]
		            if { [set! state(output_style)] ne "epub" } {
		                $svg2draw draw -image_gd_file $bname.png
		            } else {
		                lassign [GiveTempFile svgfile .svg] filename fileid
		                close $fileid
		                $svg2draw draw -image_gd_file $filename
		                set data [base64::decode [$img data -format $format]]
		                cu::zip set $state(zipid) $bname.png -level 9 -time $state(zipnow)
		                set fin [open $filename r]
		                fconfigure $fin -translation binary
		                cu::zip write $state(zipid) [read $fin]
		                close $fin
		                file delete $filename
		            }
		            $svg2draw destroy
		            set data $bname.png
		        }
		        default {
		            error "error: data_type=$data_type"
		        }
		    }
		}
		set captionNode [lindex [$domNode getElementsByTagName caption] 0]
		if { $captionNode != "" } {
		    set caption [[$domNode getElementsByTagName caption] text]
		} else { set caption "" }
		lappend state(mediaobjectstack) $data $caption $width $image_width
		
		if { $i == $len-1 || [[lindex $domNodes [expr {$i+1}]] nodeName] != "mediaobject" } {
		    set table [[$htmlNode ownerDocument] createElement table]
		    if { $align eq "center" } {
		        set center [[$htmlNode ownerDocument] createElement center]
		        $htmlNode appendChild $center
		        $center appendChild $table
		    } else {
		        $htmlNode appendChild $table
		    }
		    set tr [[$htmlNode ownerDocument] createElement tr]
		    $table appendChild $tr
		    foreach "file caption width image_width" $state(mediaobjectstack) {
		        if { [set! state(output_style)] ne "epub" } {
		            if { ![string equal -nocase [file normalize [file dirname $file]] \
		                [file normalize $state(imagestodir)]] } {
		                file copy -force $file $state(imagestodir)
		            }
		        }
		        set file [file join $state(imagestodir) [file tail $file]]

		        set file [cu::file::make_path_relative $state(basedir_dest) $file]
#                         set l1 [file split $state(basedir_dest)]
#                         set l2 [file split $file]
#                         if { $l1 eq [lrange $l2 0 [expr {[llength $l1]-1}]] } {
#                             set file [eval file join [lrange $l2 [llength $l1] end]]
#                         }
		        set td [[$htmlNode ownerDocument] createElement td]
		        $tr appendChild $td
		        set img [[$htmlNode ownerDocument] createElement img]
		        $img setAttribute src $file

		        if { $width eq "" } { set width "100%" }

		        if { $width != "" } {
		            set parent_tableNode [$domNode selectNodes ancestor::table]
		            set pagewidth 800
		            set pagewidth_med 420
		            #if { $parent_tableNode ne "" } {
		            #    set numcols [llength [$parent_tableNode selectNodes \
		            #                {.//row[1]/entry}]]
		            #    if { $numcols < 1 } { set numcols 1 }
		            #    set pagewidth [expr {$pagewidth/double($numcols)}]
		            #    set pagewidth_med [expr {$pagewidth_med/double($numcols)}]
		            #}
		            if { $parent_tableNode ne "" } {
		                if { [llength $parent_tableNode] == 1 } {
		                    set numcols [llength [$parent_tableNode selectNodes \
		                            {.//row[1]/entry}]]
		                    if { $numcols < 1 } { set numcols 1 }
		                    set pagewidth [expr {$pagewidth/double($numcols)}]
		                    set pagewidth_med [expr {$pagewidth_med/double($numcols)}]   
		                } else {    
		                    set min_pagewidth 0
		                    foreach p_parent_tableNode $parent_tableNode {
		                       set numcols [llength [$p_parent_tableNode selectNodes \
		                                    {.//row[1]/entry}]]
		                       if { $numcols < 1 } { set numcols 1 }
		                       set p_pagewidth [expr {$pagewidth/double($numcols)}]
		                       set p_pagewidth_med [expr {$pagewidth_med/double($numcols)}]
		                       if { $p_pagewidth > $min_pagewidth } {
		                          set min_pagewidth $p_pagewidth
		                          set pagewidth $p_pagewidth
		                          set pagewidth_med $p_pagewidth_med
		                       }  
		                    }     
		                }
		            }    
		            if { [regexp {([\d.]+)%} $width {} width_percent] } {
		                set width [expr {int($width_percent/100.0*$pagewidth_med)}]
		                if { $width > $image_width } {
		                    set width $image_width
		                }
		            }
		            if { [string is double $width] && $width > $pagewidth } {
		                set width $pagewidth
		            }
		            $img setAttribute width [expr {round($width)}]
		        }
		        if { $align eq "center" } {
		            set center [[$htmlNode ownerDocument] createElement center]
		            $td appendChild $center
		            set pt $center
		        } else {
		            set pt $td
		        }
		        $pt appendChild $img
		        if { $caption ne "" } {
		            set p [[$htmlNode ownerDocument] createElement p]
		            set c [[$htmlNode ownerDocument] createElement center]
		            $p appendChild $c
		            $c appendChild [[$htmlNode ownerDocument] createTextNode \
		                    $caption]
		            $pt appendChild $p
		        }
		    }
		    set tr [[$htmlNode ownerDocument] createElement tr]
		    $table appendChild $tr
		     set td [[$htmlNode ownerDocument] createElement td]
		    $tr appendChild $td
		    $td setAttribute colspan [expr {[llength $state(mediaobjectstack)]/2}]

		    set state(mediaobjectstack) ""
		}
	    }
	    "indexterm" {
		if { [llength [$domNode childNodes]] != 1 } {
		    #puts stderr "indexterm with [llength [$domNode childNodes]] children"
		}
		set primary [$domNode getElementsByTagName "primary"]
		if { [llength $primary] != 1 } {
		    #puts stderr "indexterm with no primary"
		} else {
		    if {[info exists state(currentid)] } {
		        set indexterm([$primary text]) $state(currentid)
		    }
		}
	    }
	    "xref" {
		set link [$domNode @linkend ""]
		set a [[$htmlNode ownerDocument] createElement a]
		$htmlNode appendChild $a
		$a setAttribute tmphref $link
	    }
	    "ulink" {
		set linktype [$domNode @linktype url]
		set url [$domNode @url]
		switch $linktype {
		    local - local_ext {
		        set tag [[$htmlNode ownerDocument] createElement a]
		        $tag setAttribute tmphref $url
		        #$a setAttribute href $idpages($link)
		        if { $linktype eq "local_ext" } {
		            $tag setAttribute target "_blank"
		        }
		    }
		    file {
		        set tag [[$htmlNode ownerDocument] createElement a]
		        $tag setAttribute href file://$url
		    }
		    url - url_ext {
		        set tag [[$htmlNode ownerDocument] createElement a]
		        $tag setAttribute href $url
		        if { $linktype eq "url_ext" } {
		            $tag setAttribute target "_blank"
		        }
		    }
		    default {
		        set tag [[$htmlNode ownerDocument] createElement b]
		    }
		}
		$htmlNode appendChild $tag
		if { [$domNode childNodes] ne "" } {
		    TextdataToHTML [$domNode childNodes] $tag
		} else {
		    $tag appendChild [[$htmlNode ownerDocument] createTextNode $url]
		}
		
	    }
	    "\#text" {
		$htmlNode appendChild [[$htmlNode ownerDocument] createTextNode [$domNode nodeValue]]
	    }
	    "literal" { 
		#kike
		set tag [[$htmlNode ownerDocument] createElement code]
		$htmlNode appendChild $tag
		TextdataToHTML [$domNode childNodes] $tag
	    }
	    "\#comment" { 
		#kike
		continue
	    }
	    "user_defined" {
		# nothing
	    }
	    default {
		#puts stderr "lost tag 2: [$domNode nodeName] value: [$domNode text]"
	    }
	}
    }
}

proc xml2html::PrintIndex { htmlNode htmltocNode } {
    variable indexterm
    variable state

    if { [llength [array names indexterm]] == 0 } { return }
    
    set title "INDEX"
    set sect [lindex $state(section) end]
    incr sect
    set state(section) [lreplace $state(section) end end $sect 0]
    set id "index"

    set htmlNode [_create_section_before $htmlNode $htmltocNode $id $title]

    set lastletter ""
    set isopen 0
    foreach i [lsort -dictionary [array names indexterm]] {
	if { $lastletter ne [string toupper [string index $i 0]] } {
	    if { $isopen } {
		append xml "</dir>\n"
		$htmlNode appendXML $xml
		set isopen 0
	    }
	    set lastletter [string toupper [string index $i 0]]
	    $htmlNode appendXML "<h2>[string map $state(map) $lastletter]</h2>\n"
	    set xml "<dir>\n"
	    set isopen 1
	}
	append xml "<li><a href='[string map $state(map) $indexterm($i)]'>"\
	    "[string map $state(map) $i]</a></li>\n"
    }
    if { $isopen } {
	append xml "</dir>\n"
	$htmlNode appendXML $xml
	set isopen 0
    }
    set state(section) [lreplace $state(section) end end]
}


proc xml2html::GetulNode { htmltocBody state } {   
    set ulNode ""
    set parent $htmltocBody
    foreach level [lrange $state 0 end-1] {
	incr level -1
	set parent [lindex [$parent selectNodes ul/li] $level]
	if { $parent == "" } {
	    break         
	}
    }
    if { $parent != "" } {
	set ulNode [$parent selectNodes ul]
	if { $ulNode == "" } {
	    set ulNode [[$htmltocBody ownerDocument] createElement ul]
	    $parent appendChild $ulNode
	}
    }
    return $ulNode
}

proc xml2html::_create_section_before { htmlNode htmltocNode id title } {
    variable state
    variable idpages

    if { ![llength [$htmlNode childNodes]] } {
	set isvoid 1
    } else {
	set isvoid 0
    }
    if { !$isvoid && $state(multiplesfiles) && ([llength $state(section)] == 2 ||
	[llength $state(section)] == 3)} {
	regexp {(.*?)(\d+)(\..*)$} $state(file) {} fileroot filenum ext
	set filenum [scan $filenum %d]
	incr filenum
	set state(file) $fileroot[format "%02d" $filenum]$ext
	set htmldoc [dom createDocument html]
	set htmlNode [$htmldoc documentElement]

	set file [file join $state(basedir_dest) $state(file)]
	lappend state(files_domtrees) [list $file $htmlNode]
	# the space is necessary after -
	$htmlNode appendChild [[$htmlNode ownerDocument] createComment "-*- coding: utf-8;-*- "]
	set head [[$htmlNode ownerDocument] createElement head]
	$htmlNode appendChild $head
	set meta [[$htmlNode ownerDocument] createElement meta]
	$meta setAttribute http-equiv "content-type" content "text/html; charset=UTF-8"
	$head appendChild $meta
	ApplyStyles $head
	
	set titlec [[$htmlNode ownerDocument] createElement title]
	$head appendChild $titlec
	$titlec appendChild [[$htmlNode ownerDocument] createTextNode $title]
	
	set body [[$htmlNode ownerDocument] createElement body]
	$htmlNode appendChild $body
	set htmlNode $body
	
	if { [set! state(output_style)] eq "epub" } {
	    set ns { opf http://www.idpf.org/2007/opf }
	    set xml "<item id='[xml_map [file root $state(file)]]' "
	    append xml "href='[xml_map $state(file)]' media-type='application/xhtml+xml' " \
		"xmlns='http://www.idpf.org/2007/opf'/>"
	    set node [$htmltocNode selectNodes -namespaces $ns opf:manifest]
	    $node appendXML $xml
	    set xml "<itemref idref='[xml_map [file root $state(file)]]' linear='yes' "
	    append xml "xmlns='http://www.idpf.org/2007/opf'/>"
	    set node [$htmltocNode selectNodes -namespaces $ns opf:spine]
	    $node appendXML $xml
	}
    }
    if { $id != "" } {
	set a [[$htmlNode ownerDocument] createElement a]
	$htmlNode appendChild $a
	$a setAttribute name $id
	set idpages($id) $state(file)#$id
	set state(currentid) $state(file)#$id
	
	if { [set! state(output_style)] ne "epub" } {
	    set htmltocBody [$htmltocNode selectNodes body]
	    set ulNode [GetulNode $htmltocBody [lrange $state(section) 0 end-1]]        
	    if { $ulNode != "" } {                    
		set li [[$htmltocBody ownerDocument] createElement li]
		$ulNode appendChild $li
		set a [[$htmltocBody ownerDocument] createElement a]
		$a setAttribute href $state(file)#$id
		$li appendChild $a
		$a appendChild [[$htmltocBody ownerDocument] createTextNode $title]
	    }
	} else {
	    set ns { opf http://www.idpf.org/2007/opf }            
	    set xml "<reference type='text' title='[xml_map $title]' "
	    append xml "href='[xml_map $state(file)]' xmlns='http://www.idpf.org/2007/opf'/>"
	    set node [$htmltocNode selectNodes -namespaces $ns opf:guide]
	    $node appendXML $xml            
	}
    }
    if { $title != "" } {
	set tag h[llength $state(section)]
	set tagNode [[$htmlNode ownerDocument] createElement $tag]
	$htmlNode appendChild $tagNode
	$tagNode appendChild [[$htmlNode ownerDocument] createTextNode $title]
    }
    return $htmlNode
}
    

proc xml2html::PrintDomAsXML { root filename } {
   variable state
   
    if { [set! state(output_style)] ne "epub" } { 
	set fout [open $filename w]
	fconfigure $fout -encoding utf-8
	$root asXML -channel $fout -escapeNonASCII
	close $fout
    } else {
	cu::zip set $state(zipid) $filename -level 9 -time $state(zipnow)
	set data {<?xml version="1.0" encoding="UTF-8" ?>}
	append data "\n" [$root asXML -escapeNonASCII]
	cu::zip write $state(zipid) [encoding convertto utf-8 $data]
    }
}

proc xml2html::_TextDataToHTMLDom { domNodes htmlNode htmltocNode } {
    variable itemslist
    variable state
    variable idpages
    
    if { ![info exists itemslist] } {
	_InitializeItemsList
    }
    foreach domNode $domNodes {
	if { [info exists itemslist] && [lsearch -ascii -sorted $itemslist [$domNode nodeName]] != -1 } {
	    xml2html::TextdataToHTML $domNode $htmlNode
	} elseif { [$domNode nodeName] == "section" || [$domNode nodeName] == "chapter" } {
	    set title [string trim [[lindex [$domNode selectNodes title] 0] text]]
	    if { $title == "" } { set title [$domNode @id ""] }
	    set title [lindex [split $title >] end]
	    set sect [lindex $state(section) end]
	    incr sect
	    set state(section) [lreplace $state(section) end end $sect 0]
	    set id [$domNode @id ""]
	    if { $id == "" && $title != "" } {
		regsub -all {\s+} $title _ id
	    }
	    set htmlNode [_create_section_before $htmlNode $htmltocNode $id $title]
	    _TextDataToHTMLDom [$domNode childNodes] $htmlNode $htmltocNode

	    set state(section) [lreplace $state(section) end end]
	} else {
	    if { [$domNode nodeName] == "title" } { continue }
	    #puts stderr "lost tag when printing: [$domNode nodeName] [$domNode text]"
	    #_TextDataToHTMLDom [$domNode childNodes]
	}
    }
}



# id in symbol font - letter occidental - unicode
set greekletters {
	41 A 391     61 a 3b1
	42 B 392     62 b 3b2   
	43 C 3a7     63 c 3c7
	44 D 394     64 d 3b4
	45 E 395     65 e 3b5
	46 F 3a6     66 f 3a6
	47 G 393     67 g 3b3
	48 H 397     68 h 3b7
	49 I 399     69 i 3b9
	4a J -       6a j 3c6  
	4b K 39a     6b k 3ba 
	4c L -       6c l 3bb
	4d M 39c     6d m 3bc
	4e N 39d     6e n 3bd
	4f O 39f     6f o 3bf
	50 P 3a0     70 p 3c0
	51 Q 398     71 q 3b8 
	52 R 3a1     72 r 3c1 
	53 S 3a3     73 s 3c3
	54 T 3a4     74 t 3c4 
	55 U 3a5     75 u 3c5 
	56 V -       76 v 3c9
	57 W 3a9     77 w 3c9
	58 X 39e     78 x 3be 
	59 Y 3a8     79 y 3c8
	5a Z 396     7a z 3b6
	-  ^1 0b9
	-  ^2 0b2
	-  ^3 0b3
	-  ^4 2074
	b0 degrees  00b0
	b1 +- 0b1
	ce pertenece 2208
	d5 productorio 220f
	e5 sum 2211
	d6 raiz 221a
	a5 inf 221e
	f2 int 222b
	b9 diferente 2260
	a3 semejante 2248
	a3 <= 2264
	b3 >= 2265
	-  !<= 226e
	-  !>= 226f
	-  libra 00a3
	-  euro 20ac
}

proc xml2html::GiveTempDir {} {
    variable tmpdir

    if { [info exists tmpdir] } {
	return $tmpdir
    }

    if { [info exists ::env(TEMP)] && [file isdir $::env(TEMP)] } {
	set tmpdir $::env(TEMP)
    } elseif { [info exists ::env(TMP)] && [file isdir $::env(TMP)] } {
	set tmpdir $::env(TMP)
    } elseif { [info exists ::env(WINDIR)] && [file isdir [file join $::env(WINDIR) temp]] } {
	set tmpdir [file join $::env(WINDIR) temp]
    } elseif { [file isdir /tmp] } {
	set tmpdir /tmp
    } else {
	set tmpdir /
    }
    return $tmpdir
}

# returns name and fileid
proc xml2html::GiveTempFile { basename extension } {
    
    set ic 0
    while { $ic < 100 } {
	set file [file join [GiveTempDir] $basename$ic$extension]
	if { ![catch {open $file w} fileid] } {
	    return [list $file $fileid]
	}
	incr ic
    }
    if { $ic == 100 } {
	WarnWin [_ "Error: could not open temporal file"]
	return
    }
}

proc xml2html::ApplyStyles { htmlNode } {
    variable state
    
    if { [set! state(output_style)] ne "epub" } {
	set styles {
	    p { text-align:justify; }
	    body { font-family: Verdana, Arial, Helvetica, Geneva; font-size 8px; }
	    a { text-decoration:none }
	    pre { font-family:Courier New, Courier, Mono; }
	    h1 { font-family:Arial; font-size: 28; font-weight: bold; color:DarkBlue; }
	    h2 { font-family:Arial; font-size: 24; font-weight: bold; color:DarkBlue; }
	    h3 { font-family:Arial; font-size: 22; font-weight: bold; color:DarkBlue; }
	    h4 { font-family:Arial; font-size: 18; font-weight: bold; color:DarkBlue; }
	    h4 { font-family:Arial; font-size: 18; font-weight: bold; color:DarkBlue; }
	    h4 { font-family:Arial; font-size: 18; font-weight: bold; color:DarkBlue; }
	    P { text-align:justify; font-family:Arial, Helvetica, sans-serif; font-size: 17; }
	    code { font-family:Courier New, Courier, Mono; font-size: 16; }
	    li { text-align:justify; font-family:Arial, Helvetica, sans-serif; font-size: 17; 
		margin-top: 7px; }
	    ol { text-align:justify; font-family:Arial, Helvetica, sans-serif; font-size: 17;  
		margin-top: 7px; }
	    ul { text-align:justify; font-family:Arial, Helvetica, sans-serif; font-size: 17; 
		margin-top: 7px; }
	}
    } else {
	set styles {
	    body { font-family: Verdana, Arial, Helvetica, Geneva; font-size 8px; }
	    a { text-decoration:none }
	    pre { font-family:Courier New, Courier, Mono; }
	    h1 { font-family:Arial; font-size: 12; font-weight: bold; color:DarkBlue; }
	    h2 { font-family:Arial; font-size: 12; font-weight: bold; color:DarkBlue; }
	    h3 { font-family:Arial; font-size: 12; font-weight: bold; color:DarkBlue; }
	    h4 { font-family:Arial; font-size: 12; font-weight: bold; color:DarkBlue; }
	    h5 { font-family:Arial; font-size: 12; font-weight: bold; color:DarkBlue; }
	    h6 { font-family:Arial; font-size: 12; font-weight: bold; color:DarkBlue; }
	    p { text-align:justify;
		font-family:Arial, Helvetica, sans-serif; font-size: 12; }
	    center {  font-size: 12; }
	    code { font-family:Courier New, Courier, Mono; font-size: 11; }
	    li { text-align:justify;font-family:Arial, Helvetica, sans-serif; font-size: 12; 
		margin-top: 7px; }
	    ol { text-align:justify;font-family:Arial, Helvetica, sans-serif; font-size: 12; 
		margin-top: 7px; }
	    ul { text-align:justify;font-family:Arial, Helvetica, sans-serif; font-size: 12; 
		margin-top: 7px; }
	}
    }
    set style [[$htmlNode ownerDocument] createElement style]
    $style setAttribute type text/css
    $htmlNode appendChild $style
    $style appendChild [[$htmlNode ownerDocument] createTextNode $styles]
}

proc xml2html::PrintHTMLOld { T } {
    variable domNodes
    variable state
    variable currentfile

    UpdateDisplayedNode $T ""
    array unset state
    array unset indexterm

    set document $domNodes(document)

    #foreach "file fileid" [GiveTempFile htmfile .html] break
    set file [file root $currentfile].html
}

proc xml2html::give_active_language {} {
    variable state
    
    set lognoter_db $state(lognoter_db)
    return [$lognoter_db cget -language]
}

proc xml2html::sql { cmd } {
    variable state

    set lognoter_db $state(lognoter_db)
    set err [catch { $lognoter_db sql onecolumn $cmd } ret]
    if { $err } { set ret "ERROR!! executing 'sql $cmd' ($ret)" }
    return $ret
}

proc xml2html::PrintHTML { xml opts } {
    variable domNodes
    variable state
    variable indexterm
    variable idpages

    array unset state
    array unset indexterm
    set root [[dom parse $xml] documentElement]

    set state(multiplesfiles) 0
    image_base_name reset

    foreach "name value" $opts { set state($name) $value }
    
    if { [set! state(output_style)] eq "epub" } {
	if { [set! state(filename)] ne "" } {
	    set filename $state(filename)
	} else {
	    lassign [GiveTempFile htmfile .epub] filename fileid
	    close $fileid
	}
	set state(zipid) [cu::zip open $filename w]
	set state(zipfilename) $filename
	set state(zipnow) [clock seconds]
	cu::zip set $state(zipid) mimetype -level 9 -time $state(zipnow)
	cu::zip write $state(zipid) "application/epub+zip\n"
	cu::zip set $state(zipid) META-INF/container.xml -level 9 -time $state(zipnow)
	set xml1 {<?xml version="1.0" encoding="UTF-8" ?>
	    <container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
	    <rootfiles>
	    <rootfile full-path="OPS/main.opf" media-type="application/oebps-package+xml"/>
	    </rootfiles>
	    </container>}
	cu::zip write $state(zipid) $xml1
	
	set filename OPS/main.xml
	set tocfile OPS/main.opf
	set state(imagestodir) OPS/images
    } else {
	if { [set! state(filename)] ne "" } {
	    set filename $state(filename)
	    set state(imagestodir) [file join [file root $filename]_images]
	} else {
	    lassign [GiveTempFile htmfile .html] filename fileid
	    set state(imagestodir) [file root $filename]_images
	    close $fileid
	}
	set state(imagestodir) [file normalize $state(imagestodir)]
	set tocfile [file normalize [file root $filename]_toc.html]
	file mkdir $state(imagestodir)
    }
    foreach "name value" $opts {
	if { $name in "imagestodir" && $value eq "" } { continue }
	set state($name) $value
    }
    
    if { [set! state(output_style)] eq "epub" } {
	set state(multiplesfiles) 1
    }
    if { $state(multiplesfiles) } {
	set file [file root $filename]01[file extension $filename]
    } else {
	set file $filename
    }

    set htmldoc [dom createDocument html]
    set htmlNode [$htmldoc documentElement]
    
    if { [set! state(output_style)] ne "epub" } {
	set htmltocdoc [dom createDocument html]
    } else {
	set xml1 {<package version="2.0" unique-identifier="LognoterId" 
	    xmlns="http://www.idpf.org/2007/opf"/>}
	set htmltocdoc [dom parse $xml1]
    }
    set htmltocNode [$htmltocdoc documentElement]

    set state(files_domtrees) [list \
	    [list $file $htmlNode] \
	    [list $tocfile $htmltocNode]]

    set state(map) [list "&gt;" > "&lt;" < "&amp;" & "&quot;" \" "&apos;" ' \u201c \"]

    set state(section) 0
    set state(file) [file tail $file]
    set state(basedir_dest) [file dirname $file]
    if { ![info exists state(basedir)] } {
	set state(basedir) $state(basedir_dest)
    }
    set head [[$htmlNode ownerDocument] createElement head]
    # the space is necessary after -
    $htmlNode appendChild [[$htmlNode ownerDocument] createComment "-*- coding: utf-8;-*- "]
    $htmlNode appendChild $head
    set meta [[$htmlNode ownerDocument] createElement meta]
    $meta setAttribute http-equiv "content-type" content "text/html; charset=UTF-8"
    $head appendChild $meta
    ApplyStyles $head
    set body [[$htmlNode ownerDocument] createElement body]
    $htmlNode appendChild $body

    if { [set! state(output_style)] ne "epub" } {
	set tochead [[$htmltocNode ownerDocument] createElement head]
	# the space is necessary after -
	$htmltocNode appendChild [[$htmlNode ownerDocument] createComment "-*- coding: utf-8;-*- "]
	$htmltocNode appendChild $tochead
	set meta [[$htmlNode ownerDocument] createElement meta]
	$meta setAttribute http-equiv "content-type" content "text/html; charset=UTF-8"
	$tochead appendChild $meta
	ApplyStyles $tochead
	set tocbody [[$htmltocNode ownerDocument] createElement body]
	$htmltocNode appendChild $tocbody
    } else {
	set xml1 {<metadata xmlns:dc="http://purl.org/dc/elements/1.1/"
	    xmlns="http://www.idpf.org/2007/opf"/>}
	$htmltocNode appendXML $xml1
	set xml1 {<manifest xmlns="http://www.idpf.org/2007/opf"/>}
	$htmltocNode appendXML $xml1
	set xml1 {<spine toc="ncx" xmlns="http://www.idpf.org/2007/opf"/>}
	$htmltocNode appendXML $xml1
	set xml1 {<guide xmlns="http://www.idpf.org/2007/opf"/>}
	$htmltocNode appendXML $xml1
	
	set ns { opf http://www.idpf.org/2007/opf }
	set xml "<item id='[xml_map [file root $state(file)]]' "
	append xml "href='[xml_map $state(file)]' media-type='application/xhtml+xml' " \
	    "xmlns='http://www.idpf.org/2007/opf'/>"
	set node [$htmltocNode selectNodes -namespaces $ns opf:manifest]
	$node appendXML $xml
	
	set xml "<itemref idref='[xml_map [file root $state(file)]]' linear='yes' "
	append xml "xmlns='http://www.idpf.org/2007/opf'/>"
	set node [$htmltocNode selectNodes -namespaces $ns opf:spine]
	$node appendXML $xml
    }
    set titleNode [$root selectNodes title] 
    if { $titleNode != "" } {
	set title [string trim [$titleNode text]]
	set titlec [[$htmlNode ownerDocument] createElement title]
	$head appendChild $titlec
	$titlec appendChild [[$htmlNode ownerDocument] createTextNode $title]
	if { [set! state(output_style)] ne "epub" } {
	    set titlec [[$htmltocNode ownerDocument] createElement title]
	    $tochead appendChild $titlec
	    $titlec appendChild [[$htmltocNode ownerDocument] createTextNode $title]
	    set h1 [[$htmltocNode ownerDocument] createElement h1]
	    $h1 appendChild [[$htmltocNode ownerDocument] createTextNode $title]
	    $tochead appendChild $h1
	} else {
	    set ns { opf http://www.idpf.org/2007/opf }
	    set node [$htmltocNode selectNodes -namespaces $ns opf:metadata]
	    set xml1 "<dc:title xmlns:dc='http://purl.org/dc/elements/1.1/'>"
	    append xml1 "[xml_map1 $title]</dc:title>"
	    $node appendXML $xml1
	}
    }
    if { [set! state(output_style)] eq "epub" } {
	set ns { opf http://www.idpf.org/2007/opf }
	set node [$htmltocNode selectNodes -namespaces $ns opf:metadata]
	set xml1 "<dc:language xmlns:dc='http://purl.org/dc/elements/1.1/'>"
	append xml1 "[xml_map1 $state(language)]</dc:language>"
	$node appendXML $xml1
    }
    _TextDataToHTMLDom [$root childNodes] $body $htmltocNode

    PrintIndex $body $htmltocNode

    # fix a problem with consecutive <ol>

    while 1 {
	set repeat 0
	foreach olNode [$body selectNodes //ol] {
	    set num [$olNode @start 1]
	    incr num [llength [$olNode selectNodes li]]
	    set olNodeN [$olNode selectNodes {following-sibling::*[1]/descendant-or-self::ol[1]}]
	    if { $olNodeN ne "" && [$olNodeN @type ""] eq [$olNode @type ""] &&
		[$olNodeN @start 1] == $num } {
		foreach liNode [$olNodeN selectNodes li] {
		    $olNode appendChild $liNode
		}
		$olNodeN delete
		set repeat 1
		break
	    }
	}
	if { !$repeat } { break }
    }

    if { [info exists state(title_database_table)] } {
	set table $state(title_database_table)
    } else {
	set table ""
    }
    if { $table ne "" && $state(multiplesfiles) } {
	set file [file root $filename]_title[file extension $file]
	set title [sql "select \"Title\" from $table"]
	set desc [sql "select \"Title description\" from $table"]
	set img_data [cu::inflate [sql "select \"Title image__contents\" from $table"]]

	set doc [dom createDocument html]
	set root [$doc documentElement]
	set head [$root appendChildTag head]
	set meta [$head appendChildTag meta]
	$meta setAttribute http-equiv "content-type" content "text/html; charset=UTF-8"
	ApplyStyles $head
	set body [$root appendChildTag body]

	set img [image create photo -data $img_data]
	foreach format [list png gif jpeg] {
	    switch $format {
		png { set imgfile $state(imagestodir)/image_title.png }
		gif { set imgfile $state(imagestodir)/image_title.gif }
		jpeg { set imgfile $state(imagestodir)/image_title.jpg }
	    }
	    if { [set! state(output_style)] ne "epub" } {
		set err [catch { $img write $imgfile -format $format } ]
		if { $err } { file delete $imgfile }
	    } else {
		set err [catch {
		        set data [base64::decode [$img data -format $format]]
		        cu::zip set $state(zipid) $imgfile -level 9 -time $state(zipnow)
		        cu::zip write $state(zipid) $data
		    }]
	    }
	    if { !$err } { break }
	}
	image delete $img

	$body appendChildTag h1 [list text() $title]
	set doc_desc [dom parse $desc]
	set doc_root [$doc_desc documentElement]
	_TextDataToHTMLDom [$doc_root childNodes] $body ""
	$doc_desc delete

	set imgfile [cu::file::make_path_relative $state(basedir_dest) \
		$imgfile]
	$body appendChildTag img [list attributes() src $imgfile]

	lappend state(files_domtrees) [list $file $root]
	#PrintDomAsXML $root $file        
	#$doc delete
	
	if { [set! state(output_style)] eq "epub" } {
	    set ns { opf http://www.idpf.org/2007/opf }
	    set xml1 "<reference type='cover' title='[xml_map [_ "Title page"]]' "
	    append xml1 "href='[xml_map [file tail $file]]' xmlns='http://www.idpf.org/2007/opf'/>"
	    set node [$htmltocNode selectNodes -namespaces $ns opf:guide]
	    $node appendXML $xml1
	    
	    set xml1 "<item id='[xml_map [file tail $imgfile]]' "
	    append xml1 "href='[xml_map $imgfile]' media-type='image/$format' " \
		"xmlns='http://www.idpf.org/2007/opf'/>"
	    set node [$htmltocNode selectNodes -namespaces $ns opf:manifest]
	    $node appendXML $xml1

	    set node [$htmltocNode selectNodes -namespaces $ns opf:metadata]
	    set xml1 "<meta name='cover' content='[xml_map [file tail $imgfile]]'/>"
	    $node appendXML $xml1
	}
	set tocfile $file
    }
    
    foreach i $state(files_domtrees) {
	lassign $i file rootNode
	foreach domNode [$rootNode selectNodes {//a[@tmphref]}] {
	    set url [$domNode @tmphref]
	    $domNode removeAttribute tmphref
	    if { [info exists idpages($url)] } {
		$domNode setAttribute href $idpages($url)
	    }
	}
	PrintDomAsXML $rootNode $file        
	[$rootNode ownerDocument] delete
    }
    
    if { [set! state(output_style)] eq "epub" } {
	cu::zip close $state(zipid)
	set tocfile $state(zipfilename)
    }
    if { [info exists state(outputrtf)] && $state(outputrtf) } {
	if 0 {
	    set tofile [file root $file].rtf
	    rtf:HTML2RTF $file $tofile "Title"
	    set tocfile $tofile
	}
	package require tcom
	set WordApp [tcom::ref createobject "Word.Application"]
	$WordApp Visible 0
	set WordDoc [[$WordApp Documents] Open $file]
	[[$WordApp ActiveWindow] View] Type 3
	tcom::foreach i [$WordDoc InlineShapes] {
	    [$i LinkFormat] BreakLink
	}
	# 0 is .doc format
	$WordDoc SaveAs [file root $file].doc 0
	$WordApp Quit
	unset WordApp WordDoc
	set tocfile [file root $file].doc
    }
    if { [set! state(output_style)] ne "epub" } {
	set err [catch { package require helpviewer }]
	if { !$err } {
	    HelpViewer::HelpWindow $tocfile
	} else {
	    cu::file::execute url $tocfile
	    #exec rundll32 url.dll,FileProtocolHandler $tocfile &
	}
	#eval exec [auto_execok start] [list [file native $tocfile]] &
    } else {
	exec {*}[auto_execok start] "" [file native $tocfile] &
    }
}



















