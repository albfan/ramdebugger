
package require snit
package require struct

# package require compass_utils
# mylog::init -view_binding <Control-L> debug

package provide drawformulas 1.3

proc drawformulas { args } {} ;# for auto_mkindex

snit::type drawformulas {
    option -type "" ;# canvas, image or pdf
    option -font ""
    option -defaultfontsize ""
    option -color black
    option -canvas ""
    option -image_gd_file ""
    option -disable_cache 0
    option -indent_second_line 1

    variable image_gd ""

    variable fonts
    variable bold_or_italic ""
    variable lastusedfont
    variable cache
    variable formulas_cache ""
    variable tree
    variable greekletters {
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
	a0  euro 20ac
	-  derivative 0307
	-  derivative2 0308
	b7 middledot 0b7
	d1 gradient 2207
	b6 partial_deriv 2202
	bc ... 2026
	cf no_pertenece 2209
	3a vdots 22ee
	3a ddots 22f1
    }
    variable cursorblink_after ""
    variable lastcursor_pos 0

    constructor args {
	$self configurelist $args
    }
    destructor {
#         if { $options(-type) eq "pdf" } {
#             PDFWriter::End
#         }
    }
    onconfigure -type {value} {
	set options(-type) $value
	$self changedtype
    }
    onconfigure -defaultfontsize {value} {
	set options(-defaultfontsize) $value
	$self changedtype
    }
    onconfigure -canvas {value} {
	set options(-canvas) $value
	$self changedtype
    }
    onconfigure -image_gd_file {value} {
	set options(-image_gd_file) $value
	package require gd
	$self changedtype
    }
    method changedtype {} {
	if { $options(-type) eq "canvas" } {
	    if { $options(-font) eq "" || $options(-defaultfontsize) eq "" } { return }
	    set size $options(-defaultfontsize)
	    $self createfonts [expr {$size-4}] [expr {$size+40}]
	}
	unset -nocomplain cache
    }
    method createfonts { size1 size2 } {
	set actual [font actual $options(-font)]
	if { [info exists lastusedfont] && $actual eq [lindex $lastusedfont 0] } {
	    if { $size1 >= [lindex $lastusedfont 1] && $size2 <= [lindex $lastusedfont 2] && [info exists fonts($size1)] } {
		return
	    }
	}
	for { set i $size1 } { $i <= $size2 } { incr i 2 } {
	    regsub -- {-size\s+[0-9]+} $actual "-size $i" sfont
	    set fonts($i) [font create {*}$sfont]
	}
	set l [lsort -integer [array names fonts]]
	set lastusedfont [list $actual [lindex $l 0] [lindex $l end]]
    }
    method give_font { size } {
	if { [info exists fonts($size)] } {
	    return $fonts($size)
	}
	$self createfonts $size [expr {$size+4}]
	if { [info exists fonts($size)] } {
	    return $fonts($size)
	}
	for { set i [expr {$size+1}] } { $i < $size+50 } { incr i } {
	    if { [info exists fonts($i)] } {
		return $fonts($i)
	    }
	}
	error "error in give_font size=$size -- [lsort -integer [array names fonts]]"
    }
    method draw { fsize formula { doprint 1 } { x -1 }  { y -1 } { maxwidth -1 } { minheight -1 } } {

	set formula [string trim $formula]
	regsub -all {\s+/} $formula {/} formula
	set formula [string map [list \u2074 <sup>4</sup>] $formula]

	set map [list "&gt;" > "&lt;" < "&amp;" & "&quot;" \" "&apos;" ' \u201c \"]
	set formula [string map $map $formula]

	set save_color $options(-color)
	if { [regexp {(?i)\(fail\)\s*$} $formula] } { set $options(-color) red }
	set bold_or_italic ""
	
	switch $options(-type) {
	    canvas {
		set leftmargin [expr {[$options(-canvas) cget -bd]+2}]
		set upmargin [expr {[$options(-canvas) cget -bd]+2}]
	    }
	    pdf {
		set leftmargin 0
		set upmargin 0
	    }
	    image {
		set leftmargin 0
		set upmargin 0
		# image only used to measure text
		set image_gd [gd open -width 20 \
		        -height 20 -background [gd_color "" white]]
	    }
	}
	set key [list $formula $fsize $maxwidth]
	if { $options(-disable_cache) || [set ipos [lsearch-index 0 $formulas_cache $key]] == -1 } {
	    #if { [info exists tree] } { $tree destroy }
	    set tree [::struct::tree]
	    set root [$tree rootname]
	    foreach "var val" [list x $x y $y w 0 hup 0 hdown 0 drawstack "" \
		    drawafterstack "" txt $formula fposini 0 ] {
		$tree set root $var $val
	    }
	    foreach "w hup hdown -" [$self buildtree $fsize $formula 0 $root "" $x $y] break
	    
	    set x_min [$self _give_minimum_norecursive [$tree children $root] x]
	    if { $x_min ne "" && $x_min < [$tree get $root x] } {
		set d [expr {[$tree get $root x]-$x_min}]
		set w [$tree get $root w]
		$tree set $root x $x_min
		$tree set $root w [expr {$w+$d}]
	    }
	    if { [$tree get $root x] < $x } {
		$self _addtotreekey_recursive root x [expr {$x-[$tree get $root x]}]
	    }

	    if { $maxwidth != -1 } {
		set maxwidth [expr {$maxwidth-2*$leftmargin}]
		foreach "w hup hdown" [$self _correctformaxwidth root $x $maxwidth \
		        $hup $hdown] break
	    }
	    $tree set root w $w
	    $tree set root hup $hup
	    $tree set root hdown $hdown
	} else {
	    foreach "- tree" [lindex $formulas_cache $ipos] break
	    set x_old [$tree get root x]
	    set y_old [$tree get root y]
	    $self _addtotreekey_recursive root x [expr {$x-$x_old}]
	    $self _addtotreekey_recursive root y [expr {$y-$y_old}]
	    set w [$tree get root w]
	    set hup [$tree get root hup]
	    set hdown [$tree get root hdown]
	}
	if { $options(-type) eq "canvas" } {
	    $self _addtotreekey_recursive root x $leftmargin
	    $self _addtotreekey_recursive root y [expr {$hup+$upmargin}]
	    
	    set delta [expr {$w*0.10}]
	    if { $delta > 10 } { set delta 10 }
	    $options(-canvas) configure -width [expr {round($w+$delta)}]
	    $options(-canvas) configure -height [expr {$hup+$hdown+1}]
	    if { $minheight != -1 && [$options(-canvas) cget -height] < $minheight } {
		$options(-canvas) configure -height $minheight
	    }
	} elseif { $options(-type) eq "image" } {
	    gd close $image_gd
	    set delta [expr {$w*0.10}]
	    if { $delta > 10 } { set delta 10 }

	    set height [expr {$hup+$hdown+1}]
	    if { $minheight != -1 && $height < $minheight } {
		set height $minheight
	    }
	    set image_gd [gd open -width [expr {round($w+$delta)}] \
		    -height $height -background [gd_color "" white]]
	    gd config $image_gd -transparent [gd_color "" white]

	    if { $::tcl_platform(platform) eq "unix" } {
		if { ![info exists ::Freetype_Font_Aliases] } {
		    array set ::Freetype_Font_Aliases [ gd fontalias [ glob -nocomplain /usr/share/fonts/truetype/freefont/*.ttf ] ]
		}
		gd config $image_gd -fonttable ::Freetype_Font_Aliases
	    }
	    $self _addtotreekey_recursive root y [expr {$hup}]
	} else {
	    $self _addtotreekey_recursive root y [expr {-1*$hup}]
	}
	if { $doprint } {
	    #$self printtree
	    #$self printtree_xml
	    $self drawtree
	    
#             if { $options(-type) eq "pdf" } {
#                 PDFWriter::CreateLine $x $y [expr {$x+$w}] $y 1 green
#                 PDFWriter::CreateLine $x $y $x [expr {$y+$hup}] 1 green
#                 PDFWriter::CreateLine $x $y $x [expr {$y-$hdown}] 1 blue
#             }
	}
	if { 1 } {
	    if { [set ipos [lsearch-index 0 $formulas_cache $key]] == -1 } {
		if { [llength $formulas_cache] > 20 } {
		    set formulas_cache [lrange $formulas_cache 0 18]
		}
		lappend formulas_cache [list $key $tree]
	    }
	    #unset tree
	}
	set options(-color) $save_color
	
	if { $options(-type) eq "image" } {
	    if { $::tcl_platform(platform) eq "windows" } {
		set file [file attributes $options(-image_gd_file) -shortname]
	    } else {
		set file $options(-image_gd_file)
	    }
	    gd close $image_gd -save -file $file
	    set image_gd ""
	}
	return [list $w $hup $hdown]
    }
    method give_greekletters {} { return $greekletters }
    method canbeformula { formula } {
	if { [regexp {(\u222b|\u2211|/|<sup>|<sub>).*\(.*\)\s*$} $formula] } { return 1 }
	return 0
    }
    method _createnode { parentnode x y } {
	set node [$tree insert $parentnode end]
	$tree set $node x $x
	$tree set $node y $y
	foreach "var val" [list w 0 hup 0 hdown 0 drawstack "" drawafterstack "" txt ""] {
	    $tree set $node $var $val
	}
	return $node
    }
    method printtree { { node root } } {
	set _ ""
	$tree walk $node i {
	    append _ "$i [$tree parent $i] [$tree getall $i]\n"
	}
	return $_
    }
    proc xml { args } {
	set map [list > "&gt;" < "&lt;" & "&amp;" \" "&quot;" ' "&apos;"]
	return [string map $map [join $args ""]]
    }
    method printtree_xml { { node root } } {
	package require tdom
	set _ ""
	$tree walk $node -order both "action i" {
	    if { $action eq "enter" } {
		append _ "<$i "
		foreach "n v" [$tree getall $i] {
		    if { [string is integer $n] } {
		        set n integer_$n
		    }
		    if { [string is double -strict $v] } {
		        set v [format %.3g $v]
		    }
		    append _ "$n='[xml $v]' "
		}
		append _ ">"
	    } else {
		append _ "</$i>"
	    }
	}
	dom parse $_ doc
	
	package require RamDebugger
	interp eval ramdebugger [list RamDebugger::OpenFileSaveHandler \
		"*formula (xml)*" [$doc asXML] ""]

    }
    method _correctformaxwidth { parent x maxwidth hup hdown } {

	if { $options(-indent_second_line) == 1 } {
	    set indent 20
	} else {
	    set indent 0
	}
	
	switch $options(-type) {
	    canvas - image { set subfac [expr {1.0}] ; set supfac [expr {-1.0}] }
	    pdf { set subfac [expr {-1.0}] ; set supfac [expr {1.0}] }
	}
	set dx 0
	set nodes [$tree children $parent]
	set lines_ini 0
	for { set i 1 } { $i < [llength $nodes] } { incr i } {
	    set node [lindex $nodes $i]
	    if { $dx != 0 } {
		$self _addtotreekey_recursive $node x $dx
	    }
	    set prevtxt [$tree get [lindex $nodes [expr {$i-1}]] txt]
	    set currtxt [$tree get $node txt]

#             if { [$tree get $node w] >= $maxwidth } {
#                 foreach "w_in hup_in hdown_in" [$self _correctformaxwidth $node $x \
#                         $maxwidth $hup $hdown] break
#                 if { $hup_in > $hup } { set hup $hup_in }
#                 if { $hdown_in > $hdown } { set hdown $hdown_in }
#             }

	    if { $prevtxt eq "/" || [regexp {^/$|^<su} $currtxt] } { continue }
	    if { $prevtxt eq "\n" || [$tree get $node x]-$x+[$tree get $node w] >= $maxwidth } {
		set anysign -1
		set jmax [expr {$i-1}]
		set jmin [lindex $lines_ini end]
		for { set j $jmax } { $j >= $jmin } { incr j -1 } {
		    set txt [$tree get [lindex $nodes $j] txt]
		    
		    # \u2248 = ? (semejante) ;\u2260 (diferente)
		    # \u2264 (<=) ; \u2265 (>=) ; \u226e (!<=) ; \u226f (!>=)
		    # \u22ee vdots
		    
		    if { $txt in [list "=" "\n" \u2248 \u2260 \u22ee] } {
		        if { $j > $jmin+($jmax-$jmin)/2.0 } {
		            set anysign $j
		            break
		        } elseif { $anysign == -1 } { set anysign $j }
		    } elseif { $txt in [list "-" "+" "\u00b7" "*" "," \u2264 \u2265 \u226e \u226f] } {
		        if { $anysign == -1 } { set anysign $j }
		    }
		}
		if { $anysign != -1 } {
		    incr anysign
		    for { set j $i } { $j > $anysign } { incr j -1 } {
		        $self _addtotreekey_recursive [lindex $nodes $j] x \
		            [expr {-1*$dx}]
		    }
		    set i $anysign
		    set node [lindex $nodes $i]
		}
		lappend lines_ini [expr {$i-1}] $i
		set dx [expr {$dx+$x-[$tree get $node x]+$indent}]
		$self _addtotreekey_recursive $node x \
		    [expr {$x-[$tree get $node x]+$indent}]
	    }
	}
	set max_x [$tree get $parent x]
	for { set i 0 } { $i < [llength $nodes] } { incr i } {
	    set node [lindex $nodes $i]
	    set max_x_i [expr {[$tree get $node x]+[$tree get $node w]}]
	    if { $max_x_i > $max_x } { set max_x $max_x_i }
	}
	set w [expr {$max_x-[$tree get $parent x]}]
	lappend lines_ini [expr {[llength $nodes]-1}]
	set iline 0
	set dy 0
	foreach "li le" [lrange $lines_ini 0 end] {
	    foreach "hup_line hdown_line" [list 0 0] break
	    for { set i $li } { $i <= $le } { incr i } {
		set node_in [lindex $nodes $i]
		if { [$tree get $node_in hup] > $hup_line } {
		    set hup_line [$tree get $node_in hup]
		}
		if { [$tree get $node_in hdown] > $hdown_line } {
		    set hdown_line [$tree get $node_in hdown]
		}
	    }
	    if { $iline > 0 } {
		set dy [expr {$dy-$hup_line-4}]
		for { set i $li } { $i <= $le } { incr i } {
		    set node [lindex $nodes $i]
		    $self _addtotreekey_recursive $node y [expr {$supfac*$dy}]
		    $self _addtotreekey_recursive $node hup $dy
		    $self _addtotreekey_recursive $node hdown [expr {-1*$dy}]
		    if { [$tree get $node hup] > $hup } { set hup [$tree get $node hup] }
		    if { [$tree get $node hdown] > $hdown } {
		        set hdown [$tree get $node hdown]
		    }
		}
	    }
	    set dy [expr {$dy-$hdown_line}]
	    incr iline
	}
	return [list $w $hup $hdown]
    }
    # nodes in nodes list should be independent (no parent-son relationship)
    method _addtotreekey_recursive { nodes key value } {
	foreach node $nodes {
	    $tree walk $node i {
		$tree set $i $key [expr {[$tree get $i $key]+$value}]
	    }
	}
    }
    method _addtotreekey_norecursive { nodes key value } {
	foreach node $nodes {
	    $tree set $node $key [expr {[$tree get $node $key]+$value}]
	}
    }
    method _set_key { nodes key value } {
	foreach node $nodes {
	    $tree set $node $key $value
	}
    }
    method _give_minimum_norecursive { nodes key } {
	set min ""
	foreach node $nodes {
	    set v [$tree get $node $key]
	    if { $min eq "" || $v < $min } { set min $v }
	}
	return $min
    }
    method _isexponentialnumber { txt ipos } {
	if { ![regexp {[-+]} [string index $txt $ipos]] } { return 0 }
	if { ![regexp {[-+.\d][eE]$} [string range $txt 0 [expr {$ipos-1}]]] } { return 0 }
	if { ![regexp {^\d} [string range $txt [expr {$ipos+1}] end]] } { return 0 }
	return 1
    }
    method _findmatchingparentheses { par txt } {
	
	if { $par eq "(" } { set p ")" } else { set p "]" }
	set n 0
	set is_quote 0
	set len [string length $txt]
	for { set i 0 } { $i < $len } { incr i } {
	    set r [regexp {\\+$} [string range $txt 0 $i-1] r0]
	    if { $r && [string length $r0]%2 == 1 } { continue }
	    
	    if { $n <= 1 && [string index $txt $i] == "\"" } {

		if { !$is_quote } {
		    if { [regexp {(^|\(|\[||,)$} [string range $txt 0 $i-1]] } {
		        set is_quote 1
		    }
		} elseif { [regexp {^($|\)|\]||,)} [string range $txt $i+1 end]] } {
		    set is_quote 0
		}
	    } elseif { !$is_quote && [string index $txt $i] == $par } {
		incr n
	    } elseif { !$is_quote && [string index $txt $i] == $p } {
		incr n -1
		if { $n == 0 } {
		    return [expr {$i+1}]
		}
	    }
	}
	return 0
    }
    method _add_row_to_vector_or_matrix { txt can_be_matrix num_cols } {

	set p [string index $txt 0]
	set found 0
	if { $p in "( \[" } {
	    set len [$self _findmatchingparentheses $p $txt]
	    if { $len == [string length $txt] } {
		set txt [string range $txt 1 end-1]
		set found 1
	    }
	}
	if { !$can_be_matrix } {
	    return $txt
	}
	if { !$found } {
	    set list [list $txt]
	    if { $num_cols >= 2 } {
		lappend list {*}[lrepeat [expr {$num_cols-1}] ""]
	    }
	    return $list
	}
	set list [$self _find_vector_or_matrix $txt 0]
	if { $list eq "" } {
	    if { [string index $txt 0] eq "\"" && [string index $txt end] eq "\"" } {
		set txt [string map [chars_map] \
		        [string trim [string range $txt 1 end-1]]]
	    }
	    set list [list $txt]
	}
	if { $num_cols == -1 } { return $list }
	
	set delta [expr {$num_cols-[llength $list]}]
#         if { $delta < 0 } {
#             if { [string index $txt 0] eq "\"" && [string index $txt end] eq "\"" } {
#                 set txt [string map [list \\\" \" \\\\ \\] \
#                         [string trim [string range $txt 1 end-1]]]
#             }
#             set list [list $txt]
#         }
	if { $delta >= 1 } {
	    lappend list {*}[lrepeat $delta ""]
	}
	return $list
    }
    proc chars_map {} {
	set map ""
	foreach c [list \" \[ \] ( ) \{ \} \\ ,] {
	    lappend map \\$c $c
	}
	return $map
    }
    method _find_vector_or_matrix { txt { can_be_matrix 1 } } {

	#mylog::debugoff "ENTER cbm=$can_be_matrix txt=$txt"
	lassign [list 0 "" "" -1 0] lastidx ret stack num_cols is_quote
	set len [string length $txt]
	for { set i 0 } { $i < $len } { incr i } {
	    
	    set r [regexp {\\+$} [string range $txt 0 $i-1] r0]
	    if { $r && [string length $r0]%2 == 1 } { continue }

	    switch -- [string index $txt $i] {
		"\"" {
		    if { $stack ne "" } { continue }
		    
		    if { !$is_quote } {
		        if { [regexp {(^|\(|\[||,)$} [string range $txt 0 $i-1]] } {
		            set is_quote 1
		        }
		    } elseif { [regexp {^($|\)|\]||,)} [string range $txt $i+1 end]] } {
		        set is_quote 0
		    }
		}
		"(" { if { !$is_quote } { lappend stack "(" } }
		"\[" { if { !$is_quote } { lappend stack "\[" } }
		")" {
		    if { !$is_quote } {
		        if { [lindex $stack end] ne "(" } {
		            #mylog::debugoff "FAILED) $txt"
		            return ""
		        }
		        set stack [lreplace $stack end end]
		    }
		}
		"\]" {
		    if { !$is_quote } {
		        if { [lindex $stack end] ne "\[" } {
		            #mylog::debugoff "FAILED\] $txt"
		            return ""
		        }
		        set stack [lreplace $stack end end]
		    }
		}
		"," {
		    if { $stack eq "" && !$is_quote } {
		        if { [string index $txt $lastidx] eq "\"" } {
		            set txt_in [string map [chars_map] \
		                    [string trim [string range $txt $lastidx+1 $i-2]]]
		        } else {
		            set txt_in [string trim [string range $txt $lastidx $i-1]]
		        }
		        if { $can_be_matrix != 1 } {
		            set ret_in $txt_in
		        } else {
		            set ret_in [$self _add_row_to_vector_or_matrix $txt_in $can_be_matrix \
		                    $num_cols]
		            if { $can_be_matrix } { set num_cols [llength $ret_in] }
		        }
		        lappend ret $ret_in
		        set lastidx [expr {$i+1}]
		    }
		}
	    }
	}
	if { $stack eq "" && !$is_quote } {
	    if { [string index $txt $lastidx] eq "\"" } {
		set txt_in [string map [chars_map] \
		        [string trim [string range $txt $lastidx+1 end-1]]]
	    } else {
		set txt_in [string trim [string range $txt $lastidx end]]
	    }
	    if { $can_be_matrix != 1 } {
		set ret_in $txt_in
	    } else {
		set ret_in [$self _add_row_to_vector_or_matrix $txt_in $can_be_matrix \
		        $num_cols]
		if { $can_be_matrix } { set num_cols [llength $ret_in] }
	    }
	    lappend ret $ret_in
	}
	if { $ret eq "" } {
	    #mylog::debugoff "FAILED END $txt"
	    return ""
	}
	
	if { $can_be_matrix == 1 } {
	    set nrows [llength $ret]
	    set cols ""
	    set align ""
	    for { set col 0 } { $col < $num_cols } { incr col } {
		set colList ""
		set isnumber 1
		set islong 0
		for { set row 0 } { $row < $nrows } { incr row } {
		    lappend colList [lindex $ret $row $col]
		    if { ![string is double -strict [lindex $ret $row $col]] } {
		        set isnumber 0
		    }
		    if { [string length [lindex $ret $row $col]] > 10 } {
		        set islong 1
		    }
		}
		lappend cols $colList
		if { $isnumber } {
		    lappend align .
		} elseif { $islong } {
		    lappend align l
		} else {
		    lappend align c
		}
	    }
	    #mylog::debugoff "END1 txt=$txt cols=$cols"

	    return [list $cols $align]
	} else {
	    #mylog::debugoff "END2 txt=$txt ret=$ret"
	    return $ret
	}
    }
    method _isfunc_argument { txt pos } {
	return [regexp {^\w+\([\w.+-]*\)} [string range $txt $pos end]]
    }
    method _cleanparenthesesdraw { node } {

	set nodes [$tree children $node]
	if { [llength $nodes] < 2 } {
	    if { [llength $nodes] == 1 && [llength [$tree children $nodes]] >= 2 } {
		return [$self _cleanparenthesesdraw $nodes]
	    }
	    return
	}
	foreach "has_start has_end" [list 0 0] break
	if { [regexp {^[\[(]$} [$tree get [lindex $nodes 0] txt]] } {
	    set has_start 1
	}
	if { [regexp {^[\])]$} [$tree get [lindex $nodes end] txt]] } {
	    set has_end 1
	}
	if { $has_start && $has_end } {
	    $tree delete [lindex $nodes 0]
	    $tree delete [lindex $nodes end]
	}
    }
    method _is_emphasis { txt } {
	
	set r1 [regexp -indices {^<\s*emphasis([^>]*)>} $txt idx1 idx_a]
	if { $r1 } {
	    set r2 [regexp -indices {<\s*/\s*emphasis\s*>} $txt idx2]
	} else {
	    set r2 0
	}
	if { $r2 } {
	    set bi italic
	    if { [regexp {role\s*=\s*["']strong['"]} [string range $txt {*}$idx_a]] } {
		set bi bold
	    }
	    return [list $bi {*}$idx1 {*}$idx2]
	}
    }
    method _is_subandorsup { txt } {
	set subtag {<\s*?(sub)(?:script)?\s*?>}
	set suptag {<\s*?(sup)(?:erscript)?\s*?>}
	set esubtag {<\s*?(/sub)(?:script)?\s*?>}
	set esuptag {<\s*?(/sup)(?:erscript)?\s*?>}
	set anytag "\\s*(?:$subtag|$esubtag|$suptag|$esuptag)"

	if { ![regexp -indices ^($subtag|$suptag) $txt] } { return [list ""] }

	set stackbase ""
	set stack ""
	set idx 0
	while { [regexp -indices -start $idx $anytag $txt tag sub esub sup esup] } {
	    set tags [list $sub $esub $sup $esup]
	    set i [lsearch -regexp $tags {^[^-]}]
	    set type [eval [list string range $txt] [lindex $tags $i]]
	    if { [string match /* $type] } {
		if { [lindex $stack end 0] ne [string range $type 1 end] } { return [list ""] }
		if { [llength $stack] > 1 } {
		    set stack [lrange $stack 0 end-1]
		} else {
		    set sb [lindex $stack 0]
		    lappend sb $tag
		    lappend stackbase $sb
		    set stack ""
		    if { [llength $stackbase] == 2 || [lindex $stackbase 0 0] eq "sup" } { break }
		}
	    } else {
		if { [llength $stackbase] == 1 && [llength $stack] == 0 && \
		         ( $type ne "sup" || [lindex $tag 0] != [lindex $stackbase end 2 1]+1) } {
		    break
		}
		lappend stack [list $type $tag]
	    }
	    set idx [expr {[lindex $tag 1]+1}]
	}
	if { [llength $stack] > 0 } { return [list ""] }
	switch [llength $stackbase] {
	    0 { return [list ""] }
	    1 { 
		set t [string range $txt [lindex $stackbase 0 1 0] [lindex $stackbase 0 2 1]]
		set t_in [string range $txt [expr {[lindex $stackbase 0 1 1]+1}] \
		              [expr {[lindex $stackbase 0 2 0]-1}]]
		set idx_in [expr {[lindex $stackbase 0 1 1]+1}]
		return [list [lindex $stackbase 0 0] $t $t_in $idx_in]
	    }
	    2 {
		set t [string range $txt [lindex $stackbase 0 1 0] [lindex $stackbase 1 2 1]]
		set t_in1 [string range $txt [expr {[lindex $stackbase 0 1 1]+1}] \
		               [expr {[lindex $stackbase 0 2 0]-1}]]
		set t_in2 [string range $txt [expr {[lindex $stackbase 1 1 1]+1}] \
		               [expr {[lindex $stackbase 1 2 0]-1}]]
		set idx_in1 [expr {[lindex $stackbase 0 1 1]+1}]
		set idx_in2 [expr {[lindex $stackbase 1 1 1]+1}]
		return [list subsup $t $t_in1 $t_in2 $idx_in1 $idx_in2]
	    }
	}
    }
    proc find_vector_specs { txt } {
	if { [string index $txt 0] ne "(" } { return "" }
	if { [regexp {^\((?:|list)\)(.*)} $txt {} txt_in] } {
	    return [list $txt_in - - "" 0]
	}
	set txt [string range $txt 1 end]
	
	set d [dict create open - close - align ""]
	set found 0
	while { [regexp -indices {^\s*(open|close|align)\s*=\s*"([^"]*)"} $txt idxs idxs1 idxs2] } {
	    set what [string range $txt {*}$idxs1]
	    set v [string range $txt {*}$idxs2]
	    dict set d $what $v
	    set txt [string range $txt [lindex $idxs 1]+1 end]
	    set found 1
	}
	if { !$found } { return "" }
	if { ![regexp -indices {\s*\)} $txt idxs] } { return "" }
	set txt [string range $txt [lindex $idxs 1]+1 end]
	return [list $txt [dict get $d open] [dict get $d close] [dict get $d align] 1]
    }
    method buildtree { fsize formula fposini parentnode curr_node x y } {
	switch $options(-type) {
	    canvas - image { set subfac [expr {1.0}] ; set supfac [expr {-1.0}] }
	    pdf { set subfac [expr {-1.0}] ; set supfac [expr {1.0}] }
	}

	set curr_hup [expr {int(.5*$fsize)}]
	set curr_hdown [expr {$fsize-$curr_hup}]
	set curr_w 0
	set curr_nodes ""
	set curr_node_can_append 0

	for { set i 0 } { $i < [string length $formula] } { incr i } {
	    set frest [string range $formula $i end]
	    set suborsub [$self _is_subandorsup $frest]
	    set is_emphasis [$self _is_emphasis $frest]
	    if { $is_emphasis ne "" } {
		foreach "bi i1 i2 i3 i4" $is_emphasis break
		set last_node $curr_node
		set curr_node [$self _createnode $parentnode $x $y]
		lappend curr_nodes $curr_node
		set txt [string range $frest $i2+1 $i3-1]
		foreach "w hup hdown nodes" [$self buildtree $fsize $txt \
		        [expr {$i2+1}] $curr_node $last_node $x $y] break
		$tree set $curr_node w $w
		$tree lappend $curr_node drawstack [list set_bold_or_italic $bi]
		$tree lappend $curr_node drawafterstack [list set_bold_or_italic ""]
		
		$tree set $curr_node fposini [expr {$fposini+$i}]
		$tree append $curr_node txt [string range $frest $i1 $i4]
		$tree set $curr_node hdown $hdown
		$tree set $curr_node hup $hup
		if { $hdown > $curr_hdown } { set curr_hdown $hdown }
		if { $hup > $curr_hup } { set curr_hup $hup }
		
		if { [llength $nodes] } {
		    set curr_node [lindex $nodes end]
		}
		set curr_w [expr {$curr_w+$w}]
		set x [expr {$x+$w}]
		incr i [expr {$i4}]
	    } elseif { [lindex $suborsub 0] eq "subsup" } {
		foreach "- txt txt_in1 txt_in2 txt_in1_idx txt_in2_idx" $suborsub break
		set smallsize [expr {$fsize-2}]
		set last_node $curr_node
		if { $last_node eq "" || [$tree parent $last_node] ne $parentnode } {
		    set curr_node [$self _createnode $parentnode $x $y]
		    lappend curr_nodes $curr_node
		}
		foreach "w1 hup1 hdown1 nds1" [$self buildtree $smallsize $txt_in1 \
		        [lindex $txt_in1_idx 0]\
		        $curr_node "" $x $y] break
		foreach "w2 hup2 hdown2 nds2" [$self buildtree $smallsize $txt_in2 \
		        [lindex $txt_in2_idx 0]\
		        $curr_node "" $x $y] break
		set nds [concat $nds1 $nds2]
		set wmax [expr {($w1>$w2)?$w1:$w2}]
#                 if { $last_node eq "" } {
#                     $tree set $curr_node w $wmax
#                 }  

		if { $last_node ne "" && [regexp {(\u222b|\u2211)$} [$tree get $last_node txt]] } {
		    if { [string index [$tree get $last_node txt] end] eq "\u222b" } {
		        set fac -.25 ;# integral
		        set facy1 .7
		    } else { set fac -.5 ; set facy1 1.0 }
		    set w [$tree get $last_node w]
		    set deltax1 [expr {$fac*($w+(($w1<$w)?$w1:$w))}]
		    set deltax2 [expr {$fac*($w+(($w2<$w)?$w2:$w))}]
		    set deltax_max [expr {($deltax1<$deltax2)?$deltax1:$deltax2}]
		    $self _addtotreekey_recursive $nds1 x $deltax1
		    $self _addtotreekey_recursive $nds2 x $deltax2

		    set x_min [$self _give_minimum_norecursive $nds x]
		    if { $x_min ne "" && $x_min < [$tree get $last_node x] } {
		        set d [expr {[$tree get $last_node x]-$x_min}]
		        set w [$tree get $last_node w]
		        $tree set $last_node x $x_min
		        $tree set $last_node w [expr {$w+$d}]
		    }
		    set deltay1 [expr {$facy1*[$tree get $last_node hdown]+$hup1}]
		    set deltay2 [expr {[$tree get $last_node hup]+$hdown2}]
		} else {
		    if { $last_node ne "" } {
		        set deltay1 [expr {2.0/3.0*[$tree get $last_node hdown]}]
		        set deltay2 [expr {2.5/3.0*[$tree get $last_node hup]}]
		    } else {
		        set deltay1 [expr {2.0/3.0*($fsize-int(.5*$fsize))}]
		        set deltay2 [expr {2.5/3.0*int(.5*$fsize)}]
		    }
		    set curr_w [expr {$curr_w+$wmax}]
		    set x [expr {$x+$wmax}]
		}
		$self _addtotreekey_recursive $nds1 y [expr {$subfac*$deltay1}]
		$self _addtotreekey_recursive $nds1 hup [expr {-1*$deltay1}]
		$self _addtotreekey_recursive $nds1 hdown $deltay1
		$self _set_key $nds1 typename sub

		$self _addtotreekey_recursive $nds2 y [expr {$supfac*$deltay2}]
		$self _addtotreekey_recursive $nds2 hup $deltay2
		$self _addtotreekey_recursive $nds2 hdown [expr {-1*$deltay2}]
		$self _set_key $nds2 typename sup
		
		$tree set $curr_node w [expr {[$tree get $curr_node w]+$wmax}]
		if { $last_node ne "" } {
		    set len [string length [$tree get $curr_node txt]]
		    $self _addtotreekey_norecursive $nds1 fposini $len
		    $self _addtotreekey_norecursive $nds2 fposini $len
		} else {
		    $tree set $curr_node fposini [expr {$fposini+$i}]
		}
		$tree append $curr_node txt $txt
		$tree set $curr_node hdown [expr {$deltay1+$hdown1}]
		$tree set $curr_node hup [expr {$deltay2+$hup2}]
		if { $deltay1+$hdown1 > $curr_hdown } { set curr_hdown [expr {$deltay1+$hdown1}] }
		if { $deltay2+$hup2 > $curr_hup } { set curr_hup [expr {$deltay2+$hup2}] }
		set curr_node_can_append 0
		incr i [expr {[string length $txt]-1}]
	    } elseif { [lindex $suborsub 0] ne "" } {
		foreach "what txt txt_in txt_in_idx" $suborsub break
		switch $what subscript { set what sub } superscript { set what sup }
		set smallsize [expr {$fsize-2}]
		set last_node $curr_node
		if { $last_node eq ""  || [$tree parent $last_node] ne $parentnode } {
		    set curr_node [$self _createnode $parentnode $x $y]
		    lappend curr_nodes $curr_node
		}
		foreach "w hup hdown nds" [$self buildtree $smallsize $txt_in \
		         [lindex $txt_in_idx 0] $curr_node "" $x $y] break

		if { $last_node ne "" && [regexp {(\u222b|\u2211)$} [$tree get $last_node txt]] } {
		    if { [string index [$tree get $last_node txt] end] eq "\u222b" } {
		        set fac -.25 ;# integral
		        set facy1 .7
		    } else { set fac -.5 ; set facy1 1.0 }
		    #set deltax [expr {$fac*[$tree get $last_node w]+$w}]
		    set deltax [expr {$fac*([$tree get $last_node w]+$w)}]
		    $self _addtotreekey_recursive $nds x $deltax

		    set x_min [$self _give_minimum_norecursive $nds x]
		    if { $x_min ne "" && $x_min < [$tree get $last_node x] } {
		        set d [expr {[$tree get $last_node x]-$x_min}]
		        set w [$tree get $last_node w]
		        $tree set $last_node x $x_min
		        $tree set $last_node w [expr {$w+$d}]
		    }

		    if { $what eq "sub" } {
		        set deltay [expr {$facy1*[$tree get $last_node hdown]+$hup}]
		    } else {
		        set deltay [expr {[$tree get $last_node hup]+$hdown}]
		    }
		} else {
		    if { $last_node ne "" } {
		        if { $what eq "sub" } {
		            set deltay [expr {2.0/3.0*[$tree get $last_node hdown]}]
		        } else {
		            set deltay [expr {2.0/3.0*[$tree get $last_node hup]}]
		        }
		    } else {
		        if { $what eq "sub" } {
		            set deltay [expr {2.0/3.0*($fsize-int(.5*$fsize))}]
		        } else {
		            set deltay [expr {2.0/3.0*int(.5*$fsize)}]
		        }
		    }
		    set curr_w [expr {$curr_w+$w}]
		    set x [expr {$x+$w}]
		}
		$self _addtotreekey_recursive $nds y [expr {[set ${what}fac]*$deltay}]
		if { $what eq "sub" } {
		    $self _addtotreekey_recursive $nds hup [expr {-1*$deltay}]
		    $self _addtotreekey_recursive $nds hdown $deltay
		} else {
		    $self _addtotreekey_recursive $nds hup $deltay
		    $self _addtotreekey_recursive $nds hdown [expr {-1*$deltay}]
		}
		$tree set $curr_node w [expr {[$tree get $curr_node w]+$w}]
		if { $last_node ne "" } {
		    set len [string length [$tree get $curr_node txt]]
		    $self _addtotreekey_norecursive $nds fposini $len
		} else {
		    $tree set $curr_node fposini [expr {$fposini+$i}]
		}
		$tree append $curr_node txt $txt
		if { $what eq "sub" } {
		    $tree set $curr_node hdown [expr {$deltay+$hdown}]
		    if { $deltay+$hdown > $curr_hdown } { set curr_hdown [expr {$deltay+$hdown}] }
		} else {
		    $tree set $curr_node hup [expr {$deltay+$hup}]
		    if { $deltay+$hup > $curr_hup } { set curr_hup [expr {$deltay+$hup}] }
		}
		set curr_node_can_append 0
		incr i [expr {[string length $txt]-1}]
	    } elseif { [string range $formula $i [expr {$i+1}]] eq "\u221a(" && \
		           [set len [$self _findmatchingparentheses "("\
		                         $frest]] > 0 } {
		# square root
		set txt [string range $formula $i [expr {$i+$len-1}]]
		set curr_node [$self _createnode $parentnode $x $y]
		lappend curr_nodes $curr_node
		set r [$self buildtree $fsize [string range $txt 2 end-1] 2 $curr_node "" $x $y]
		foreach "w hup hdown nodes" $r break
		foreach "w0 h0up h0down fsize0" [$self drawtextbigger_size $fsize $hup $hdown \
		                                     "\u221a"] break
		set deltay0 [expr {.5*($hup+$hdown)-$hdown}]
		set h0up [expr {$h0up+$deltay0}]
		set h0down [expr {$h0down-$deltay0}]
		$tree lappend $curr_node drawstack [list drawtext $fsize0 "\u221a" 0 \
		                                        [expr {$supfac*$deltay0}]]
		set hl [expr {$supfac*$h0up}]
		set lwidth [expr {.02*$fsize0}]
		$tree lappend $curr_node drawstack [list drawline $lwidth $w0 $hl \
		                                        [expr {$w0+$w}] $hl]
		$tree set $curr_node fposini [expr {$fposini+$i}]
		$tree set $curr_node txt $txt
		set deltax [expr {$w0+$w}]
		if { $h0up > $hup } { set hup $h0up }
		if { $h0down > $hdown } { set hdown $h0down }
		$tree set $curr_node w $deltax
		$tree set $curr_node hup $hup
		$tree set $curr_node hdown $hdown
		$self _addtotreekey_recursive $nodes x $w0

		set curr_w [expr {$curr_w+$deltax}]
		set x [expr {$x+$deltax}]

		if { $hdown > $curr_hdown } { set curr_hdown $hdown }
		if { $hup > $curr_hup } { set curr_hup $hup }
		set curr_node_can_append 0
		incr i [expr {[string length $txt]-1}]
	    } elseif { [regexp {^\((ok|fail)\)} $frest txt signtype] } {
		set curr_node [$self _createnode $parentnode [expr {$x+2}] $y]
		lappend curr_nodes $curr_node
		$tree set $curr_node fposini 0
		set height [expr {1.0*($curr_hup+$curr_hdown)}]
		if { $height > 25 } { set height 25 }
		set w [expr {$height+2}]
		$tree lappend $curr_node drawstack [list drawsign $signtype $height]
		$tree set $curr_node txt $txt
		$tree set $curr_node w $w
		$tree set $curr_node hup [expr {.5*$height}]
		$tree set $curr_node hdown [expr {.5*$height}]

		set curr_w [expr {$curr_w+$w}]
		set x [expr {$x+$w}]
		set curr_node_can_append 0
		incr i [expr {[string length $txt]-1}]
	    } elseif { ([regexp {[\(\[]} [string index $formula $i] par] && \
		[set len [$self _findmatchingparentheses $par $frest]] > 0 && \
		    ![$self _isfunc_argument $formula [expr {$i-1}]]) || \
		([regexp {^matrix\(} [string range $formula $i end]] && \
		    [set len [$self _findmatchingparentheses "(" $frest]] > 0 && \
		    [llength [set args [$self _find_vector_or_matrix [string range \
		                    $formula [expr {$i+7}] [expr {$i+$len-2}]] -1]]] == 4) } {

################################################################################
#    expressions in parentheses and matrixes
################################################################################
		
		set txt [string range $formula $i [expr {$i+$len-1}]]

		set curr_node [$self _createnode $parentnode $x $y]
		lappend curr_nodes $curr_node
		
		lassign "" columns align
		set is_vector_matrix 1
		                
		if { [regexp {^matrix\(} $txt] } {
		    lassign $args par ppar align text_in
		    set par_pos 7
		    set ppar_pos [expr {$len-1}]
		    set ret [$self _find_vector_or_matrix $text_in]
		    if { $ret ne "" } {
		        foreach "columns align_i" $ret break
		        if { [llength $columns] == 1 && [llength [lindex $columns 0]] == 1 } {
		            set columns ""
		        }
		        if { $align eq "" } { set align $align_i }
		    }
		    set err [catch { llength $align }]
		    if { $err } {
		        set columns ""
		    }
		} elseif { [regexp {\w$} [string range $formula 0 [expr {$i-1}]]] } {
		    set text_in [string range $txt 1 end-1]
		    if { $par eq "(" } { set ppar ")" } else { set ppar "\]" }
		    set par_pos 0
		    set ppar_pos [expr {$len-1}]
		} else {
		    if { $par eq "(" } { set ppar ")" } else { set ppar "\]" }
		    set par_pos 0
		    set ppar_pos [expr {$len-1}]
		    set text_in [string range $txt 1 end-1]                    

		    set ret [find_vector_specs $text_in]
		    if { [llength $ret] } {
		        lassign $ret text_in par_i ppar_i align_i is_vector_matrix
		        if { $par_i ne "-" } { set par $par_i }
		        if { $ppar_i ne "-" } { set ppar $ppar_i }
		        set err [catch { llength $align_i }]
		        if { !$err } { set align $align_i  }
		        set has_specs 1
		    } else {
		        set has_specs 0
		    }
		    set ret [$self _find_vector_or_matrix $text_in]
		    if { $ret ne "" } {
		        lassign $ret columns align_i
		        if { [llength $columns] == 1 && [llength [lindex $columns 0]] == 1 } {
		            set columns ""
		        }
		        if { $align eq "" } { set align $align_i }
		        
		        if { $has_specs == 0 } {
		            foreach column $columns {
		                foreach row $column {
		                    set n1 [regexp -all {<\w+>} $row]
		                    set n2 [regexp -all {</\w+>} $row]
		                    if { $n1 != $n2 } {
		                        set columns ""
		                        break
		                    }
		                }
		                if { $columns eq "" } { break }
		            }
		        }
		    }
		}
		if { $columns eq "" || !$is_vector_matrix } {
		    set r [$self buildtree $fsize $text_in 1 $curr_node "" $x $y]
		    foreach "w hup hdown nodes" $r break
		    set is_matrix 0
		} else {
		    set is_matrix 1
		    foreach "w hup hdown nodes rListTot" [list 0 0 0 "" ""] break
		    set nrows [llength [lindex $columns 0]]
		    set hup_row [lrepeat $nrows 0]
		    set hdown_row [lrepeat $nrows 0]
		    set w_col ""

		    set icol 0
		    foreach column $columns {
		        foreach "w_col_i1 w_col_i2 hup_col hdown_col rList" [list 0 0 0 ""] break
		        set align_i [lindex $align $icol]
		        if { $align_i eq "" } { set align_i c }
		        set irow 0
		        foreach row $column {
		            set ret [$self buildtree $fsize $row 1 $curr_node "" $x $y]
		            foreach "w_in hup_in hdown_in nodes_in" $ret break

		            switch -- $align_i {
		                l { set w_in1 0 }
		                c { set w_in1 [expr {0.5*$w_in}] }
		                r { set w_in1 $w_in }
		                default {
		                    if { $nodes_in eq "" } {
		                        set w_in1 $w_in
		                    } else {
		                        set w_in1 [$self give_left_width $nodes_in $align_i]
		                    }
		                }
		            }
		            set w_in2 [expr {$w_in-$w_in1}]
		            lappend rList [list $w_in1 $w_in2 $hup_in $hdown_in $nodes_in]

		            if { $w_in1 > $w_col_i1 } { set w_col_i1 $w_in1 }
		            if { $w_in2 > $w_col_i2 } { set w_col_i2 $w_in2 }
		            
		            if { $hup_in > [lindex $hup_row $irow] } {
		                lset hup_row $irow $hup_in
		            }
		            if { $hdown_in > [lindex $hdown_row $irow] } {
		                lset hdown_row $irow $hdown_in
		            }
		            lappend nodes {*}$nodes_in
		            incr irow
		        }
		        lappend w_col [list $w_col_i1 $w_col_i2]
		        set w [expr {$w+$w_col_i1+$w_col_i2}]
		        if { $icol > 0 } { set w [expr {$w+5}] }
		        lappend rListTot $rList
		        incr icol
		    }
		    set htot 0
		    for { set row 0 } { $row < $nrows } { incr row } {
		        set htot [expr {$htot+[lindex $hup_row $row]+[lindex $hdown_row $row]}]
		    }
		    set hup [expr {.5*$htot}]
		    set hdown [expr {.5*$htot}]

		    set deltax 0
		    
		    set icol 0
		    foreach rList $rListTot {
		        set y_i [expr {$y+$supfac*$hup}]
		        set align_i [lindex $align $icol]
		        if { $align_i eq "" } { set align_i c }
		        set irow 0
		        foreach r $rList {
		            foreach "w_in1 w_in2 hup_in hdown_in nodes_in" $r break
		            set y_i [expr {$y_i-$supfac*[lindex $hup_row $irow]}]
		            foreach "w_col_i1 w_col_i2" [lindex $w_col $icol] break
		            set deltax_i [expr {$deltax+$w_col_i1-$w_in1}]
		            $self _addtotreekey_recursive $nodes_in x $deltax_i
		            $self _addtotreekey_recursive $nodes_in y [expr {$y_i-$y}]
		            set y_i [expr {$y_i-$supfac*[lindex $hdown_row $irow]}]
		            incr irow
		        }
		        set deltax [expr {$deltax+$w_col_i1+$w_col_i2+5}]
		        incr icol
		    }
		}
		lassign [list $hup $hdown] hup_p hdown_p
		foreach what [list 0 2] {
		    switch $what {
		        0 {
		            set p $par
		            set p_pos $par_pos
		            set x_i $x
		            foreach "xi_0 xi_1" [list 7 2] break
		        }
		        2 {
		            set p $ppar
		            set p_pos $ppar_pos
		            set x_i [expr {$x+$w_i(0)+$w}]
		            foreach "xi_0 xi_1" [list 1 6] break
		        }
		    }
		    if { $p in "( ) {}" || ($hup+$hdown < 40 && $columns eq "") } {
		        foreach "w_i($what) hup_i hdown_i fsize_i" [$self drawtextbigger_size $fsize \
		                $hup $hdown $p] break
		        
		        if { $options(-type) eq "pdf"  } {
		            # && $columns ne ""
		            set delta [expr {$hdown-$hdown_i-.5*($hup+$hdown-$hup_i-$hdown_i)}]
		            set y_i [expr {$y-$delta}]
		            set hup_i [expr {$hup_i-$delta}]
		            set hdown_i [expr {$hdown_i+$delta}]
		        } else {
		            set y_i $y
		        }
		        set r [$self buildtree $fsize_i $p $p_pos $curr_node "" $x_i $y_i]
		        foreach "- - - nodes_i" $r break
		        if { $what == 0 && $nodes_i ne "" } {
		            eval $tree move $curr_node 0 $nodes_i
		        }
		        if { $hup_i > $hup_p } { set hup_p $hup_i }
		        if { $hdown_i > $hdown_p } { set hdown_p $hdown_i }
		    } else {
		        set lwidth 0.6
		        
		        set curr_node_i [$self _createnode $parentnode $x_i $y]
		        if { $what == 0 } {
		            $tree insert $curr_node 0 $curr_node_i
		        } else {
		            $tree insert $curr_node end $curr_node_i
		        }
		        $tree set $curr_node_i drawstack [list [list drawline $lwidth \
		                    $xi_0 [expr {$hup+0}] \
		                    $xi_1 $hup \
		                    $xi_1 [expr {-1*$hdown}] \
		                    $xi_0 [expr {-1*($hdown+0)}] \
		                    ]]
		        $tree set $curr_node_i fposini $p_pos
		        $tree set $curr_node_i txt $p
		        $tree set $curr_node_i w 7
		        $tree set $curr_node_i hup [expr {$hup+5}]
		        $tree set $curr_node_i hdown [expr {$hdown+5}]
		        set w_i($what) 7
		        
		        set hup_p [expr {$hup+5}]
		        set hdown_p [expr {$hdown+5}]
		    }
		}
		if { $hup_p > $hup } { set hup $hup_p }
		if { $hdown_p > $hdown } { set hdown $hdown_p }

		$tree set $curr_node fposini [expr {$fposini+$i}]
		$tree set $curr_node txt $txt
		set deltax [expr {$w_i(0)+$w+$w_i(2)}]
		$tree set $curr_node w $deltax
		$tree set $curr_node hup $hup
		$tree set $curr_node hdown $hdown
		$self _addtotreekey_recursive $nodes x $w_i(0)

		set curr_w [expr {$curr_w+$deltax}]
		set x [expr {$x+$deltax}]

		if { $hdown > $curr_hdown } { set curr_hdown $hdown }
		if { $hup > $curr_hup } { set curr_hup $hup }
		set curr_node_can_append 0
		incr i [expr {[string length $txt]-1}]
	    } else {
		set txt [string index $formula $i]
		
		regexp -start $i {\A([[:alpha:]_]+|\"[^\"]+\")} $formula txt

		# \u2248 = ? (semejante) ; \u2208 (pertenece) ; \u2209 (no pertenece)
		# \u2260 (diferente) ; \u2264 (<=) ; \u2265 (>=) ; \u226e (!<=) ; \u226f (!>=)
		# \u00b7 middledot
		set rex {[-+*\u00b7/=\u2248\u2208\u2209\u2260<>\u2264\u2265\u226e\u226f\s,]}
		if { [regexp $rex [string index $formula $i]] && \
		         ![$self _isexponentialnumber $formula $i] } {
		    set isolated 1
		} else {
		    set isolated 0
		}

		if { !$curr_node_can_append || $isolated } {
		    set last_node $curr_node
		    set curr_node [$self _createnode $parentnode $x $y]
		    lappend curr_nodes $curr_node
		    $tree set $curr_node fposini [expr {$fposini+$i}]
		} else { set last_node "" }
		foreach "w hup hdown" [$self drawtext_size $fsize $txt] break
		array unset tmp
		array set tmp [$tree getall $curr_node]
		$tree lappend $curr_node drawstack [list drawtext $fsize $txt [expr {$x-$tmp(x)}]]
		$tree append $curr_node txt $txt

		$tree set $curr_node w [expr {$tmp(w)+$w}]

		if { [regexp {\u00b2|\u00b3|\u2074} $txt] && $last_node ne "" } {
		    # these are: ^2,^3,^4
		    set deltay [expr {2.0/3.0*([$tree get $last_node hup]-$hup-$hdown)}]
		    $self _addtotreekey_recursive $curr_node y [expr {$supfac*$deltay}]
		    set hup [expr {$hup+$deltay}]
		    set hdown [expr {$hdown-$deltay}]
		    set isolated 1
		}
		if { $hup > $tmp(hup) } { $tree set $curr_node hup $hup }
		if { $hdown > $tmp(hdown) } { $tree set $curr_node hdown $hdown }
		
		set curr_w [expr {$curr_w+$w}]
		set x [expr {$x+$w}]

		if { $hdown > $curr_hdown } { set curr_hdown $hdown }
		if { $hup > $curr_hup } { set curr_hup $hup }
		if { $isolated } {
		    set curr_node_can_append 0
		} else { set curr_node_can_append 1 }
		incr i [expr {[string length $txt]-1}]
	    }
	}

################################################################################
#    Correcting for fractions
################################################################################

	set correct_for_fractions 1

	set dx 0
	for { set i 1 } { $correct_for_fractions && $i < [llength $curr_nodes] } { incr i } {
	    set node [lindex $curr_nodes $i]
	    if { $dx != 0 } { $self _addtotreekey_recursive $node x $dx }
	    if { $i < [llength $curr_nodes]-1 && [$tree get $node txt] eq "/" } {
		set prev [lindex $curr_nodes [expr {$i-1}]]
		set next [lindex $curr_nodes [expr {$i+1}]]
		
		if { [$tree get $prev txt] eq "<" } { continue }
		
		set found 0
		foreach nodeP [$tree ancestors $node] {
		    if { [set n [$tree previous $nodeP]] ne "" && [$tree get $n txt] eq "/" } {
		        set found 1
		        break
		    }
		    if { [set n [$tree next $nodeP]] ne "" && [$tree get $n txt] eq "/" } {
		        set found 1
		        break
		    }
		}
		set rex {[-+]}
		if { $found && ![regexp $rex [$tree get $prev txt]] && ![regexp $rex [$tree get $prev txt]] } {
		    continue
		}
		$self _cleanparenthesesdraw $prev
		$self _cleanparenthesesdraw $next
		
		foreach j [list prev next node] { array set ${j}_d [$tree getall [set $j]] }

		set wold [expr {$prev_d(w)+$node_d(w)+$next_d(w)}]
		set wtot [expr {($prev_d(w)>$next_d(w))?$prev_d(w):$next_d(w)}]

		set deltax [expr {($wtot-$prev_d(w))/2.0}]
		if { $options(-type) in "canvas image" } {
		    set delta_l 0
		    set deltay1 [expr {$prev_d(hdown)+2}]
		    set deltay2 [expr {$next_d(hup)+2}]
		} else {
		    set delta_l [expr {.35*$fsize}]
		    set deltay1 [expr {$delta_l+$prev_d(hdown)+2}]
		    set deltay2 [expr {-$delta_l+$next_d(hup)+2}]
		}
		$self _addtotreekey_recursive $prev x $deltax
		$self _addtotreekey_recursive $prev y [expr {$supfac*$deltay1}]
		$self _addtotreekey_recursive $prev hup $deltay1
		$self _addtotreekey_recursive $prev hdown [expr {-1*$deltay1}]

		set deltax [expr {$dx-$prev_d(w)-$node_d(w)+($wtot-$next_d(w))/2.0}]
		$self _addtotreekey_recursive $next x $deltax
		$self _addtotreekey_recursive $next y [expr {$subfac*$deltay2}]
		$self _addtotreekey_recursive $next hup [expr {-1*$deltay2}]
		$self _addtotreekey_recursive $next hdown $deltay2

		$self _addtotreekey_recursive $node x [expr {-$prev_d(w)}]
		$tree set $node w $wtot
		$tree set $node hup [expr {[$tree get $prev hup]}]
		$tree set $node hdown [expr {[$tree get $next hdown]}]

		$tree set $node drawstack [list [list drawline .8 0 $delta_l $wtot $delta_l]]
#                 $self _cleanparenthesesdraw $prev
#                 $self _cleanparenthesesdraw $next
		
		$tree move $node end $prev
		$tree move $node end $next
		$tree set $node fposini [$tree get $prev fposini]
		$tree set $prev fposini 0
		$tree set $next $fposini [expr {[$tree get $next fposini]-\
		                                    [$tree get $node fposini]}]
		$tree set $node txt [$tree get $prev txt][$tree get $node txt][$tree get $next txt]

		set curr_nodes [lreplace $curr_nodes [expr {$i-1}] [expr {$i-1}]]
		set curr_nodes [lreplace $curr_nodes $i $i]

		if { [$tree get $node hup] > $curr_hup } {
		    set curr_hup [$tree get $node hup]
		}
		if { [$tree get $node hdown] > $curr_hdown } {
		    set curr_hdown [$tree get $node hdown]
		}
		set dx [expr {$dx+$wtot-$wold}]
		incr i -1
	    }
	}
	
################################################################################
#    Correcting sub and sup
################################################################################
       
       for { set i 0 } { $i < [llength $curr_nodes] } { incr i } {
	    set node [lindex $curr_nodes $i]
	    foreach node_ch [$tree children $node] {
		if { ![$tree keyexists $node_ch typename] } { continue }
		set next_node [$tree next $node]
		if { $next_node eq "" } { continue }
		set typename [$tree get $node_ch typename]
		set b0 [bounding_box $tree $node_ch]
		
		set next_node $node
		set delta_x_save ""
		while { [set next_node [$tree next $next_node]] ne "" } {
		    set b1 [bounding_box $tree $next_node]
		    set delta_x [expr {[lindex $b0 0]+[lindex $b0 2]-[lindex $b1 0]}]
		    if { $delta_x > 0 && ($delta_x_save eq "" || $delta_x > $delta_x_save) } {
		        if { $typename eq "sub" && [lindex $b0 1] > [lindex $b1 1]+[lindex $b1 3] } {
		            set delta_x_save $delta_x
		        } elseif { $typename eq "sup" && [lindex $b0 1]-[lindex $b0 3] < [lindex $b1 1] } {
		            set delta_x_save $delta_x
		        }
		    }
		}
		if { $delta_x_save ne "" } {
		    set next_node $node
		    while { [set next_node [$tree next $next_node]] ne "" } {
		        $self _addtotreekey_recursive $next_node x $delta_x_save
		    }
		}
	    }
	}
	
	set curr_w [expr {$curr_w+$dx}]
	return [list $curr_w $curr_hup $curr_hdown $curr_nodes]
    }
    proc bounding_box { tree node } {
	set box ""
	set x0 [$tree get $node x]
	set y0 [$tree get $node hup]
	set width [$tree get $node w]
	set height [expr {abs([$tree get $node hup]+[$tree get $node hdown])}]
	return [list $x0 $y0 $width $height]
    }
################################################################################
#    drawing
################################################################################

    method set_bold_or_italic { x y tags what } {
	set bold_or_italic $what
    }
    method image_metrics { font txt } {
	if { [info command font] ne "" } {
	    set dx [font measure $font $txt]
	    set hup [font metrics $font -ascent]
	    set hdown [font metrics $font -descent]
	} else {
	    set ret [gd text $image_gd 0 0 -fill "" -text $txt -font $font -anchor w]
	    regexp {width\s+(\d+)} $ret {} dx
	    regexp {height\s+(\d+)} $ret {} height
	    # 1/3 and 2/3 arbitrary
	    set hup [expr {round(2.0*$height/3.0)}]
	    set hdown [expr {round(1.0*$height/3.0)}]
	}
	return [list $dx $hup $hdown]
    }
    method drawtext_size { fsize txt } {
	if { $txt== "\u222b" } { set fsize [expr {$fsize+4}] }

	if { [info exists cache($bold_or_italic,$fsize,$txt)] } {
	    return $cache($bold_or_italic,$fsize,$txt)
	}
	if { $options(-type) eq "canvas" } {
	    set font [$self give_font $fsize]
	    switch $bold_or_italic {
		"" {
		    if { [string length $txt] == 1 && [string is alpha $txt] } {
		        set font [concat [font actual $font] "-slant italic"]
		    }
		}
		bold { set font [concat [font actual $font] "-weight bold"] }
		italic { set font [concat [font actual $font] "-slant italic"] }
	    }
	    set id [$options(-canvas) create text 0 0 -text $txt -font $font -anchor w]
	    foreach "x1 y1 x2 y2" [$options(-canvas) bbox $id] break
	    $options(-canvas) delete $id
	    set hup [expr {-$y1}]
	    set hdown [expr {$y2}]
	    set ret [list [expr {$x2-$x1-1}] $hup $hdown $fsize]
	} elseif { $options(-type) eq "image" } {
	    if { $::tcl_platform(platform) eq "windows" } {
		set font [list Arial $fsize]
	    } else {
		set font  [list FreeSans $fsize]
	    }
	    switch $bold_or_italic {
		"" {
		    if { [string length $txt] == 1 && [string is alpha $txt] } {
		        lappend font italic
		    }
		}
		bold {lappend font bold }
		italic { lappend font italic }
	    }
	    lassign [$self image_metrics $font $txt] dx hup hdown
	    incr dx 4
	    set ret [list $dx $hup $hdown $fsize]
	} else {
	    set ret [$self drawtext_pdf_size $fsize $txt]
	    lappend ret $fsize
	}
	set cache($bold_or_italic,$fsize,$txt) $ret
	return $ret
    }
    method drawtext { x y tags fsize txt { deltax 0 } { deltay 0 } } {
	if { $txt== "\u222b" } { set fsize [expr {$fsize+4}] }

	set x [expr {$x+$deltax}]
	set y [expr {$y+$deltay}]

	if { $options(-type) eq "canvas" } {
	    set font [$self give_font $fsize]
	    switch $bold_or_italic {
		"" {
		    if { [string length $txt] == 1 && [string is alpha $txt] } {
		        set font [concat [font actual $font] "-slant italic"]
		    }
		}
		bold { set font [concat [font actual $font] "-weight bold"] }
		italic { set font [concat [font actual $font] "-slant italic"] }
	    }
	    set id [$options(-canvas) create text $x $y -text $txt -font $font -anchor w \
		        -fill $options(-color) -tags $tags]
	} elseif { $options(-type) eq "image" } {
	    if { $::tcl_platform(platform) eq "windows" } {
		set font [list Arial $fsize]
	    } else {
		set font  [list FreeSans $fsize]
	    }
	    switch $bold_or_italic {
		"" {
		    if { [string length $txt] == 1 && [string is alpha $txt] } {
		        lappend font italic
		    }
		}
		bold {lappend font bold }
		italic { lappend font italic }
	    }
	    gd text $image_gd $x $y -fill [gd_color $image_gd $options(-color)] \
		        -text $txt -font $font -anchor w
	} else {
	    $self pdftext $fsize $txt $x $y
	}
    }
    method drawline { x y tags width args } {
	
	set coords ""
	foreach "xc yc" $args {
	    lappend coords [expr {$x+$xc}] [expr {$y+$yc}]
	}
	if { $options(-type) eq "canvas" } {
	    $options(-canvas) create line $coords -fill $options(-color) -tags $tags \
		-width $width
	} elseif { $options(-type) eq "image" } {
	    gd line $image_gd {*}$coords \
		-fill [gd_color $image_gd $options(-color)] \
		-width $width
	} else {
	    foreach "x1 y1" [lrange $coords 0 1] break
	    foreach "x y" [lrange $coords 2 end] {
		PDFWriter::CreateLine $x1 $y1 $x $y $width $options(-color)
		foreach "x1 y1" [list $x $y] break
	    }
	}

    }
    method drawtextbigger_size { fsize hup hdown txt } {
	
	if { [info exists cache($bold_or_italic,$hup,$hdown,$txt)] } {
	    return $cache($bold_or_italic,$hup,$hdown,$txt)
	}
	
	if 0 {
	set h [expr {$hup+$hdown}]
	for { set size [expr {$fsize+2}] } { $size <= $fsize+120 } { set size [expr {$size+2}] } {
		if { $options(-type) eq "canvas" } {
		    set font [$self give_font $size]
		    switch $bold_or_italic {
		        "" {
		            if { [string length $txt] == 1 && [string is alpha $txt] } {
		                set font [concat [font actual $font] "-slant italic"]
		            }
		        }
		        bold {
		            set font [concat [font actual $font] "-weight bold"]
		        }
		        italic {
		            set font [concat [font actual $font] "-slant italic"]
		        }
		    }
		    set id [$options(-canvas) create text 0 0 -text $txt -font $font \
		            -anchor w -fill [$options(-canvas) cget -bg]]
		    foreach "x1 y1 x2 y2" [$options(-canvas) bbox $id] break
		    $options(-canvas) delete $id
		    if { $y2-$y1 >= $h } { break }
		} elseif { $options(-type) eq "image" } {
		if { $::tcl_platform(platform) eq "windows" } {
		    set font [list Arial $size]
		} else {
		    set font  [list FreeSans $size]
		}
		switch $bold_or_italic {
		    "" {
		        if { [string length $txt] == 1 && [string is alpha $txt] } {
		            lappend font italic
		        }
		    }
		    bold {lappend font bold }
		    italic { lappend font italic }
		}
		lassign [$self image_metrics $font 0] - hup_i hdown_i
		if { $hup_i+$hdown_i >= $h } { break }
	    } else {
		if { $txt == "\u221a" } { set font Symbol } else {
		    set font $options(-font)
		}
		if { [PDFWriter::LineSpace $font $size] > $h } { break }
	    }
	}
    } else {
	    set size [expr {round(0.7*($hup+$hdown))}]
	    if { $size%2==1 } {
		incr size
	    }
    }
	#set size [expr {$size-2}]

#         if { [info exists cache($bold_or_italic,$size,$txt)] } {
#             return $cache($bold_or_italic,$size,$txt)
#         }

	if { $options(-type) eq "canvas" } {
	    set font [$self give_font $size]
	    switch $bold_or_italic {
		"" {
		    if { [string length $txt] == 1 && [string is alpha $txt] } {
		        set font [concat [font actual $font] "-slant italic"]
		    }
		}
		bold { set font [concat [font actual $font] "-weight bold"] }
		italic { set font [concat [font actual $font] "-slant italic"] }
	    }
	    set id [$options(-canvas) create text 0 0 -text $txt -font $font \
		    -anchor w -fill [$options(-canvas) cget -bg]]
	    foreach "x1 y1 x2 y2" [$options(-canvas) bbox $id] break
	    $options(-canvas) delete $id
	    set hup_i [expr {-$y1}]
	    set hdown_i [expr {$y2}]
	    set ret [list [expr {$x2-$x1-1}] $hup_i $hdown_i $size]
	} elseif { $options(-type) eq "image" } {
	    if { $::tcl_platform(platform) eq "windows" } {
		set font [list Arial $size]
	    } else {
		set font  [list FreeSans $size]
	    }
	    switch $bold_or_italic {
		"" {
		    if { [string length $txt] == 1 && [string is alpha $txt] } {
		        lappend font italic
		    }
		}
		bold {lappend font bold }
		italic { lappend font italic }
	    }
	    lassign [$self image_metrics $font $txt] dx hup_i hdown_i
	    incr dx 4
	    set ret [list $dx $hup_i $hdown_i $size]
	} else {
	    set ret [$self drawtext_pdf_size $size $txt]
	    lappend ret $size
	}
	set cache($bold_or_italic,$hup,$hdown,$txt) $ret
	return $ret
    }
    method drawtext_pdf_size { fsize text } {
	switch $bold_or_italic {
	    "" {
		if { [string length $text] == 1 && [string is alpha $text] } {
		    set font $options(-font)-Oblique
		} else {
		    set font $options(-font)
		}
	    }
	    bold { set font $options(-font)-Bold }
	    italic { set font $options(-font)-Oblique }
	}
	set xlast ""
	if { $text == "\u221a" } {
	    # square root
#             set h [PDFWriter::LineSpace $font $fsize]
#             set hm [expr {.45*$h}]
	    
	    set hup [expr {[PDFWriter::Ascender $font $fsize]*$fsize}]
	    set hdown [expr {-1*[PDFWriter::Descender $font $fsize]*$fsize}]

	    set x [PDFWriter::TextWidth \xd6 Symbol $fsize]
	    return [list $x $hup $hdown]
	}
	if { [regexp {^[-+\u00b7]$} $text] } {
	    set dx [expr {4+[PDFWriter::TextWidth $text $font $fsize]}]
#             set dh [expr {.5*[PDFWriter::LineSpace $font $fsize]}]
	    set hup [expr {[PDFWriter::Ascender $font $fsize]*$fsize}]
	    set hdown [expr {-1*[PDFWriter::Descender $font $fsize]*$fsize}]

	    return [list $dx $hup $hdown]
	}
	if { $text == "\u226e" || $text == "\u226f" } {
	    # not smaller than ; not bigger than
	    set h [expr {.5*$fsize}]
	    return [list $fsize $h $h]
	}
	set x 0
	lassign "0 0" hup hdown
	for { set i 0 } { $i < [string length $text] } { incr i } {
	    set c [string index $text $i]
	    set cidx [scan $c %c]
	    if { $cidx >= 0x391 } {
		set ipos [lsearch $greekletters [format %x $cidx]]
		if { $ipos != -1 && [lindex $greekletters [expr {$ipos-2}]] != "-" } {
		    set csymbol [lindex $greekletters [expr {$ipos-2}]]
		} else { set csymbol "" }
	    }
	    if { $cidx >= 0x391 &&  $csymbol != "" } {
		set c [format %c 0x$csymbol]
		set x [expr {$x+[PDFWriter::TextWidth $c Symbol $fsize]}]
		set hup_l [expr {[PDFWriter::Ascender Symbol $fsize]*$fsize}]
		set hdown_l [expr {-1*[PDFWriter::Descender Symbol $fsize]*$fsize}]
		if { $hup_l > $hup } { set hup $hup_l }
		if { $hdown_l > $hdown } { set hdown $hdown_l }
	    } else {
		set x [expr {$x+[PDFWriter::TextWidth $c $font $fsize]}]
		set hup_l [expr {[PDFWriter::Ascender $font $fsize]*$fsize}]
		set hdown_l [expr {-1*[PDFWriter::Descender $font $fsize]*$fsize}]
		if { $hup_l > $hup } { set hup $hup_l }
		if { $hdown_l > $hdown } { set hdown $hdown_l }
	    }
	}
	return [list $x $hup $hdown]
    }
    method pdftext { fsize text x y } {
	
	set anchor sw
	switch $bold_or_italic {
	    "" {
		if { [string length $text] == 1 && [string is alpha $text] } {
		    set font $options(-font)-Oblique
		} else {
		    set font $options(-font)
		}
	    }
	    bold { set font $options(-font)-Bold }
	    italic { set font $options(-font)-Oblique }
	}
	set xlast ""
	if { $text == "\u221a" } {
	    # square root
	    set ls [PDFWriter::LineSpace Symbol $fsize]
	    set y [expr {$y-$ls*(0.88-[PDFWriter::Ascender Symbol $fsize])}]
	    set x [expr {$x+.05*$fsize}]

	    PDFWriter::WriteText \xd6 $x $y Symbol $fsize $anchor $options(-color)
	    return
#             # square root (drawing method)
#             set cc {
#                 0.443548387097 0.527027027027
#                 0.5 0.472972972973
#                 0.661290322581 1.0
#                 0.758064516129 0.0
#                 0.733870967742 0.0
#                 0.653225806452 0.837837837838
#                 0.516129032258 0.405405405405
#                 0.435483870968 0.5
#             }
#             set coordList ""
#             foreach "xc yc" $cc {
#                 lappend coordList [expr {$x+$fsize*$xc}] [expr {$y+$fsize*(0.5-$yc)}]
#             }
#             PDFWriter::CreatePolygon $coordList $options(-color) 1

#             set coords [list $x $y [expr {$x+$fsize/4.0}] $y [expr {$x+$fsize/2.0}] \
#                             [expr {$y-$fsize/2.0}] [expr {$x+3.0/4.0*$fsize}] \
#                             [expr {$y+$fsize/2.0}] \
#                             [expr {$x+$fsize}] [expr {$y+$fsize/2.0}]]
#             PDFWriter::CreateLineC $coords .8 $options(-color)
#             return
	}
	if { [regexp {^[-+\u00b7]$} $text] } {
	    set x [expr {$x+2}]
	    if { $text eq "\u00b7" } { set y [expr {$y-2}] }
	    PDFWriter::WriteText $text $x $y $font $fsize $anchor $options(-color)
	    return
	}
	if { $text == "\u226e" || $text == "\u226f" } {
	    # not smaller than ; not bigger than
	    if { $text == "\u226e" } { set c < } else { set c > }
	    PDFWriter::WriteText $c $x $y $font $fsize $anchor $options(-color)
	    PDFWriter::CreateLine [expr {$x+2}] [expr {$y-1}]  \
		[expr {$x+$fsize-5}] [expr {$y+$fsize-1}] .8 $options(-color)
	    return
	}
	for { set i 0 } { $i < [string length $text] } { incr i } {
	    set c [string index $text $i]
	    set cidx [scan $c %c]
	    if { $cidx >= 0x391 } {
		set ipos [lsearch $greekletters [format %x $cidx]]
		if { $ipos != -1 && [lindex $greekletters [expr {$ipos-2}]] != "-" } {
		    set csymbol [lindex $greekletters [expr {$ipos-2}]]
		} else { set csymbol "" }
	    }
	    if { $cidx == 0x0307 } {
		# a point accent ligature
		set xp [expr {$x-.75*[PDFWriter::TextWidth u $font $fsize]}]
		foreach "- deltay -" [$self drawtext_pdf_size $fsize u] break
		set yp [expr {$y+$deltay}]
		PDFWriter::WriteText "." $xp $yp $font $fsize $anchor $options(-color)
	    } elseif { $cidx >= 0x391 &&  $csymbol != "" } {
		set c [format %c 0x$csymbol]
		PDFWriter::WriteText $c $x $y Symbol $fsize $anchor $options(-color)
		set x [expr {$x+[PDFWriter::TextWidth $c Symbol $fsize]}]
	    } else {
#                 set w [PDFWriter::TextWidth $c $font $fsize]
#                 PDFWriter::CreateLine $x $y [expr {$x+$w}] $y 1 green
#                 PDFWriter::CreateLine $x $y $x [expr {$y+$fsize}] 1 green
		
		PDFWriter::WriteText $c $x $y $font $fsize $anchor $options(-color)
		set x [expr {$x+[PDFWriter::TextWidth $c $font $fsize]}]
	    }
	}
    }
    method drawsign { x0 y0 tags signtype height } {

	switch $signtype {
	    ok {
		set pts {
		    .12 .44
		    .36 .75
		    .79 0
		    .95 .12
		    .4 1
		    0 .59
		}
		switch $options(-type) {
		    canvas - image { set fac 1 ; set y0 [expr {$y0-.5*$height}] }
		    pdf { set fac -1 ; set y0 [expr {$y0+.5*$height}] }
		}
		set pts1 ""
		foreach "x y" $pts {
		    lappend pts1 [expr {$x0+$height*($x-.02)}] [expr {$y0+$fac*$height*($y+.08)}] 
		}
		$self drawpolygon $pts1 $tags black black .5
		set pts2 ""
		foreach "x y" $pts {
		    lappend pts2 [expr {$x0+$height*$x}] [expr {$y0+$fac*$height*$y}] 
		}
		$self drawpolygon $pts2 $tags black #00b23a .5
	    }
	    fail {
		set pts1 {
		    .25 0
		    .75 0
		    1 .25
		    1 .75
		    .75 1
		    .25 1
		    0 .75
		    0 .25
		}
		set pts2 {
		    .25 .32
		    .32 .25
		    .5 .43
		    .68 .25
		    .75 .32

		    .57 .5

		    .75 .68
		    .68 .75
		    .5 .57
		    .32 .75
		    .25 .68

		    .43 .5

		}
		switch $options(-type) {
		    canvas - image { set fac 1 ; set y0 [expr {$y0-.5*$height}] }
		    pdf { set fac -1 ; set y0 [expr {$y0+.5*$height}] }
		}
		set pts ""
		foreach "x y" $pts1 {
		        lappend pts [expr {$x0+$height*$x}] [expr {$y0+$fac*$height*$y}] 
		}
		$self drawpolygon $pts $tags black red .5
		set pts ""
		foreach "x y" $pts2 {
		    lappend pts [expr {$x0+$height*$x}] [expr {$y0+$fac*$height*$y}] 
		}
		$self drawpolygon $pts $tags "" white .5
	    }
	}
	return $height
    }
    method drawpolygon { coords tags outlinecolor fillcolor lwidth } {

	if { $options(-type) == "canvas" } {
	    $options(-canvas) create polygon $coords -fill $fillcolor -outline $outlinecolor \
		-tags $tags
	} elseif { $options(-type) eq "image" } {
	    gd polygon $image_gd {*}$coords \
		-fill [gd_color $image_gd $fillcolor] \
		-outline [gd_color $image_gd $outlinecolor] \
		-width $lwidth
	} else {
	    if { $fillcolor != "" } {
		PDFWriter::CreatePolygon $coords $fillcolor 1 $lwidth
	    }
	    if { $outlinecolor != "" } {
		PDFWriter::CreatePolygon $coords $outlinecolor 0 $lwidth
	    }
	}
    }

    method drawtree {} {
	$tree walk root -order both "what node" {
	    if { $what eq "enter" } {
		foreach cmd [$tree get $node drawstack] {
		    eval [list $self [lindex $cmd 0] [$tree get $node x] \
		            [$tree get $node y] \
		            $node] [lrange $cmd 1 end]
		}
	    } else {
		foreach cmd [$tree get $node drawafterstack] {
		    eval [list $self [lindex $cmd 0] [$tree get $node x] \
		            [$tree get $node y] \
		            $node] [lrange $cmd 1 end]
		} 
	    }
	}

	if 0 {
	    switch $options(-type) {
		canvas - image { set fac 1 }
		pdf { set fac -1 }
	    }
	    array set t [$tree getall root]
	    $self drawpolygon [list $t(x) [expr {$t(y)-$fac*$t(hup)}] \
		                   [expr {$t(x)+$t(w)}] [expr {$t(y)-$fac*$t(hup)}] \
		                   [expr {$t(x)+$t(w)}] [expr {$t(y)+$fac*$t(hdown)}] \
		                   $t(x)  [expr {$t(y)+$fac*$t(hdown)}]] "" red "" 1
	}

    }
    method cursorblink { tag color } {

	after cancel $cursorblink_after

	if { ![winfo exists $options(-canvas)] } { return }
	set oldwidth [$options(-canvas) itemcget $tag -width]
	if { $oldwidth eq "" } { return }

	if { [$options(-canvas) itemcget $tag -fill] == "" } {
	    $options(-canvas) itemconfigure $tag -fill $color
	    set after 600
	} else {
	    $options(-canvas) itemconfigure $tag -fill ""
	    set after 300
	}
	set cursorblink_after [after $after [mymethod cursorblink $tag $color]]
    }
    method deletecursor {} { $options(-canvas) delete cursor }
    method _givenewcursorpos { current_pos movement } {


    }
    method drawcursor { pos { movement "" } } {

	$options(-canvas) delete cursor

	if { $movement eq "" } {
	    if { $pos < 0 } { set pos 0 }
	    if { $pos > [string length [$tree get root txt]] } {
		set pos [string length [$tree get root txt]]
	    }
	    set pos_in $pos
	    set current root
	    set nodeslist [list [list root $pos_in]]
	    set gravity right
	} else {
	    set pos [lindex $lastcursor_pos 0 1]
	    foreach "node pos_in" [lindex $lastcursor_pos end] break
	    set fposini [$tree get $node fposini]
	    set len [string length [$tree get $node txt]]
	    if { $pos_in == $len-1 && $movement eq "right" } {
		incr pos
		set gravity left
	    } elseif { $pos_in == $len && $movement eq "right" } {
		set gravity right
	    }
	}
	while 1 {
	    set found 0
	    set node ""
	    foreach node [$tree children $current] {
		set fposini [$tree get $node fposini]
		set len [string length [$tree get $node txt]]
		if { $pos_in >= $fposini && $pos_in < $fposini+$len } {
		    set found 1
		    break
		}
	    }
	    if { !$found } { break }
	    set pos_in [expr {$pos_in-$fposini}]
	    lappend nodeslist [list $node $pos_in]
	    set current $node
	}
	foreach "node pos_in" [lindex $nodeslist end] break

	set lastcursor_pos $nodeslist

	array set t [$tree getall $node]
	set y [$tree get root y]
	$options(-canvas) create rectangle $t(x) [expr {$y-$t(hup)}] \
	    [expr {$t(x)+$t(w)}] [expr {$y+$t(hdown)}] \
	    -fill "" -outline black -dash "2 2" -tags cursor

	if { $pos_in == [string length [$tree get $node txt]] } {
	    set width [$tree get $node w]
	} elseif { [$options(-canvas) type $node] eq "text" } {
	    set font [$options(-canvas) itemcget $node -font]
	    set txt [string range $t(txt) 0 [expr {$pos_in-1}]]
	    lassign [$self image_metrics $font $txt] dx hup hdown
	    set width $dx
	} else {
	    set width 0
	}
	$options(-canvas) create line [expr {$t(x)+$width}] [expr {$y-$t(hup)}] \
	    [expr {$t(x)+$width}] [expr {$y+$t(hdown)}] -fill black -width 2 \
	    -tags "cursor cursorline"
	$self cursorblink cursorline black
	$options(-canvas) lower cursor
    }
    method give_left_width { nodes string { void_if_not_found 0 } } {

	if { [llength $nodes] > 1 } {
	    set x0 [$tree get [lindex $nodes 0] x]
	    set wtot 0
	    foreach node $nodes {
		set w [$self give_left_width $node $string 1]
		if { $w ne "" } {
		    return [expr {$w+[$tree get $node x]-$x0}]
		}
		set wtot [expr {$wtot+[$tree get $node w]}]
	    }
	    if { $void_if_not_found } {
		return ""
	    } else {
		return $wtot
	    }
	}
	set node [lindex $nodes 0]
	foreach child_node [$tree children $node] {
	    set w [$self give_left_width $child_node $string 1]
	    if { $w ne "" } {
		return [expr {$w+[$tree get $child_node x]-[$tree get $node x]}]
	    }
	}
	set txt [$tree get $node txt]
	set idx [string first $string $txt]
	if { $idx == -1 } {
	    if { $void_if_not_found } {
		return ""
	    } else {
		return [$tree get $node w]
	    }
	}
	set cmd1 [lindex [$tree get $node drawstack] 0]
	if { [lindex $cmd1 0] ne "drawtext" } {
	    if { $void_if_not_found } {
		return ""
	    } else {
		return [$tree get $node w]
	    }
	}
	set fsize [lindex $cmd1 1]
	return [lindex [$self drawtext_size $fsize [string range $txt 0 $idx-1]] 0]
    }
}

proc lsearch-index { idx args } {
    # WARNING: not valid for inline
    set err [catch { eval lsearch -index $idx $args } ret]
    if { !$err } { return $ret }

    foreach "list pattern" [lrange $args end-1 end] break

    set idxList ""
    foreach i $list {
	lappend idxList [lindex $i $idx]
    }
    set ipos [eval lsearch [lrange $args 0 end-2] [list $idxList $pattern]]
    return $ipos
}

namespace eval drawformula {
    variable dr

    proc Init { _type _canvas _font size { _color black } } {
	variable dr
	
	if { ![info exists dr] } {
	    set dr [drawformulas %AUTO% -type $_type -font $_font -defaultfontsize $size \
		        -color $_color -canvas $_canvas]
	} else {
	    $dr configure -type $_type -font $_font -defaultfontsize $size \
		-color $_color -canvas $_canvas -indent_second_line 1
	}
	return $dr
    }
    proc indent_second_line { boolean } {
	variable dr
	
	$dr configure -indent_second_line $boolean
    }
    proc drawformula { fsize formula { doprint 1 } { x -1 }  { y -1 } { width -1 } { height -1 } \
		           { minheight -1 } { cursor -1 } } {
	variable dr

	return [$dr draw $fsize $formula $doprint $x $y $width $minheight]
    }
    proc draw_anchor { fsize formula x y anchor } {
	if { $anchor eq "center" } { set anchor "" }
	foreach "width hup hdown" [drawformula $fsize $formula 0 $x $y] break
	set height [expr {$hup+$hdown}]
	if { [string first n $anchor] != -1 } {
	    # nothing
	} elseif { [string first s $anchor] != -1 } {
	    set y [expr $y+$height]
	} else {
	    set y [expr $y+$height/2.0]
	}
	if { [string first w $anchor] != -1 } {
	    # nothing
	} elseif { [string first e $anchor] != -1 } {
	    set x [expr $x-$width]
	} else {
	    set x [expr $x-$width/2.0]
	}
	return [drawformula $fsize $formula 1 $x $y]
    }
    proc canbeformula { formula } {
	variable dr

	#return [$dr canbeformula $formula]
	if { [regexp {(\u222b|\u2211|/|<sup>|<sub>).*\(.*\)\s*$} $formula] } { return 1 }
	return 0
    }
    proc is_complex_formula { formula } {
	if { [regexp {(\u222b|\u2211|\u221a|/\(|\)/|<sup|<sub)} $formula] } { return 1 }
	return 0

    }
    proc End { w hup hdown } {
	variable dr

	$dr destroy
	unset dr
    }
}

# repeated in svg.tcl
proc gd_color { image_gd color { dash "" } } {
    set ret ""
    if { $color eq "" } { return "" }
    
    set col_table {
	white "255 255 255"
	black "0 0 0"
    }
    if { [dict exists $col_table $color] } {
	set ret [dict get $col_table $color]
    } else {
	foreach i [winfo rgb . $color] {
	    lappend ret [expr {$i/256}]
	}
    }
    if { [llength $dash] > 1 } {
	lassign $ret r g b
	lassign [list "" ""] dist idx
	foreach i [lindex [gd config $image_gd -colormap] 0 1] {
	    lassign $i idx_i cols
	    lassign $cols r_i g_i b_i
	    set dist_i [expr {($r-$r_i)*($r-$r_i)+($r-$r_i)*($g-$g_i)+($g-$g_i)*($b-$b_i)}]
	    if { $idx eq "" || $dist_i < $dist } {
		lassign [list $dist_i $ids_i] dist idx
	    }
	}
	if { $idx ne "" } {
	    set ret "="
	    set idx_i 0
	    foreach i $dash {
		set num [expr {round($i)}]
		if { $num < 1 } { set num 1 }
		if { $idx_i == 0 } { set idx_i $idx } else { set idx_i 0 }
		lappend ret {*}[lrepeat $num $idx_i]
	    }
	}
    }
    return $ret
}

# set formula "k<sub>h</sub>=1\u00b71\u00b7a<sub>b</sub>/g"
# pack [canvas .c -bd 0 -highlightthickness 0]
# drawformulas dr -type canvas -font Helvetica -defaultfontsize 10 -canvas .c
# dr draw 8 $formula

