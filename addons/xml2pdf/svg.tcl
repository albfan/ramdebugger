
#package require resizer
if { [info command dict] eq "" } {
    package require dict
}
package require snit
package require dialogwin
package require tooltip
namespace eval :: { namespace import -force tooltip::tooltip }

package require compass_utils
package require compass_utils::math

if { 0 } {
    package require compass_utils::img
    package require gd
}

if { [info command ::mylog::debug] eq "" } {
    #mylog::init -view_binding <Control-l> debug
    mylog::init ""
}

# for tclIndex to work
namespace eval svg2draw {}
proc svg2draw::parse_path { args } {}

proc list_intersection { list1 list2 } {
    set nlist ""
    foreach i $list2 {
	if { [lsearch -exact $list1 $i] != -1 } {
	    lappend nlist $i
	}
    }
    return $nlist
}

proc xml { args } {
    set map [list > "&gt;" < "&lt;" & "&amp;" \" "&quot;" ' "&apos;"]
    return [string map $map [join $args ""]]
}

# trick to fill the tclIndex file
proc svg2draw {} {}

snit::type svg2draw {
    option -canvas
    option -destroy_with_canvas 0
    option -auto_redraw 1
    option -parameters ""
    option -svgfile ""
    option -svgnode ""
    option -delete_svgnode_on_end 0
    option -basedir ""
    option -draw_callback ""
    option -add_to_contextual ""
    option -add_zoom_to_contextual 1
    option -delete_dimensions 0
    option -animate 1
    option -draw_animation_time 0
    option -padding ""
    option -time_variable ""
    option -resolve_links_handler ""

    variable transform_stack_main
    variable transform_stack
    variable state ""
    variable svgNode ""
    variable svgNode_needs_cleanup 0
    variable draw_x 0
    variable draw_y 0
    variable draw_width
    variable draw_height

    variable draw_time_start ""
    variable draw_time 0
    variable isAnimated 0
    variable animateList
    variable animate_callback ""

    variable canvas ""
    variable image_gd ""
    variable image_gd_file ""
    variable gid_output 0
    variable gid_buffer ""

    delegate method * to canvas
    delegate option * to canvas

    constructor args {
	$self configurelist $args
    }
    destructor {
	if { [info exists draw_handler_afterid] } {
	    after cancel $draw_handler_afterid
	}
	$self stop_animation

	if { $svgNode_needs_cleanup } {
	    $svgNode delete
	}
	if { $options(-time_variable) ne "" } {
	    trace remove variable $options(-time_variable) write \
		"[mymethod check_time_variable get];#"
	}
    }
    onconfigure -canvas { value } {
	set options(-canvas) $value
	set canvas $value
	if { [winfo class $canvas] ne "Canvas" } {
	    error "error in svg2draw"
	}
	bind $canvas <MouseWheel> "$canvas yview scroll \[expr {%D/-120}\] units"
	bind $canvas <ButtonPress-1> [mymethod manage_sel BP1 %x %y]
	bind $canvas <B1-Motion> [mymethod manage_sel BM1 %x %y]
	bind $canvas <ButtonRelease-1> [mymethod manage_sel BR1 %x %y]
	bind $canvas <ButtonRelease-3> [mymethod contextual_submenu %X %Y]
	
	bind $canvas <Shift-ButtonRelease-3> [mymethod contextual_submenu %X %Y 1]

	if { $options(-auto_redraw) } {
	    bind $canvas <Configure> [mymethod draw_handler]
	}
	if { $options(-destroy_with_canvas) } {
	    bind $canvas <Destroy> [list $self destroy]
	}
    }
    onconfigure -auto_redraw { value } {
	set options(-auto_redraw) $value

	if { [info exists canvas] } {
	    if { $options(-auto_redraw) } {
		bind $canvas <Configure> [mymethod draw_handler]
	    } else {
		bind $canvas <Configure> ""
	    }
	}
    }
    onconfigure -destroy_with_canvas { value } {
	set options(-destroy_with_canvas) $value
	if { $canvas ne "" && $options(-destroy_with_canvas) } {
	    bind $canvas <Destroy> [list $self destroy]
	}
    }
    onconfigure -delete_svgnode_on_end { value } {
	set options(-delete_svgnode_on_end) $value
	set svgNode_needs_cleanup $value
    }
    onconfigure -parameters { value } {
	set options(-parameters) $value
    }
    onconfigure -svgfile { value } {
	set options(-svgfile) $value

	if { $svgNode ne "" && $svgNode_needs_cleanup } { $svgNode delete }
	set doc [dom parse [tDOM::xmlReadFile $options(-svgfile)]]
	set root [$doc documentElement]
	set ns { svg http://www.w3.org/2000/svg }
	set svgNode [$root selectNodes -namespaces $ns /*/svg:svg|/svg:svg]
	set svgNode_needs_cleanup 1
	set options(-basedir) [file dirname $value]
    }
    onconfigure -svgnode { value } {
	if { $svgNode_needs_cleanup } {
	    $svgNode delete
	}
	 set options(-svgnode) $value
	set svgNode $value

	if { $options(-delete_dimensions) } {
	    $self _delete_dimensions
	}
    }
    onconfigure -delete_dimensions { value } {
	set options(-delete_dimensions) $value
	if { $options(-delete_dimensions) } {
	    $self _delete_dimensions
	}
    }
    onconfigure -padding { value } {
	set options(-padding) $value
	
	switch [llength $options(-padding)] {
	    1 { set options(-padding) [lrepeat 4 $options(-padding)] }
	    2 { set options(-padding) [concat $options(-padding) $options(-padding)] }
	}
    }
    onconfigure -time_variable { value } {
	
	if { $options(-time_variable) ne "" } {
	    trace remove variable $options(-time_variable) write \
		"[mymethod check_time_variable get];#"
	}
	set options(-time_variable) $value
	
	trace add variable $value write "[mymethod check_time_variable get];#"
	
    }
    method check_time_variable { what } {
	if { $options(-time_variable) eq "" } { return }
	switch $what {
	    get {
		if { [set $options(-time_variable)] != $draw_time } {
		    set draw_time [set $options(-time_variable)]
		}
	    }
	    set {
		if { [set $options(-time_variable)] != $draw_time } {
		    set $options(-time_variable) $draw_time
		}
	    }
	}
    }
    method is_pdf {} {
	if { $image_gd eq "" && !$gid_output && $canvas eq "" } { return 1 }
	return 0
    }

    proc _length { c } {
	return [scan $c %f]
    }
    #args: ?-scale scalefactor?
    method give_svgNode { args } {

	set scale ""
	while { [string match -* [lindex $args 0]] } {
	    switch -- [lindex $args 0] {
		-scale {
		    set args [lrange $args 1 end]
		    set scale [lindex $args 0]
		}
		default {
		    set drawing 0
		    error "error in give_svgNode"
		}
	    }
	    set args [lrange $args 1 end]
	}
	if { [llength $args] } {
	    error "error in give_svgNode"
	}
	# catch is here for cases where parameters are not valid
	catch { $self update_svgtree }

	if { $scale ne "" } {
	     $svgNode setAttribute width [expr {$scale*[_length [$svgNode @width]]}]
	     $svgNode setAttribute height [expr {$scale*[_length [$svgNode @height]]}]
	}
	if { $options(-padding) ne "" } {
	    $svgNode setAttribute padding $options(-padding)
	}
	return $svgNode
    }
    method tr { coords } {
	foreach i [lreverse $transform_stack] {
	    if { $i eq "-" } { continue }
	    foreach "a b c d e f" $i break
	    set idx -1
	    foreach "x y" $coords {
		lset coords [incr idx] [expr {$a*$x+$c*$y+$e}]
		lset coords [incr idx] [expr {$b*$x+$d*$y+$f}]
	    }
	}
	return $coords
    }
    method tr_inv { coords } {
	foreach i $transform_stack {
	    if { $i eq "-" } { continue }
	    foreach "a b c d e f" $i break
	    set idx -1
	    foreach "x y" $coords {
		lset coords [incr idx] [expr {1.0*($d*$x-$c*$y+$c*$f-$d*$e)/($d*$a-$c*$b)}]
		lset coords [incr idx] [expr {1.0*(-1*$b*$x+$a*$y+$b*$e-$a*$f)/($d*$a-$c*$b)}]
	    }
	}
	return $coords
    }
    method tr_vec { vec } {
	return [m::sub [$self tr $vec] [$self tr "0 0"]]
    }
    method tr_vec_inv { vec } {
	return [m::sub [$self tr_inv $vec] [$self tr_inv "0 0"]]
    }
    method tr_main { coords } {
	foreach i [lreverse [lrange $transform_stack 0 $transform_stack_main]] {
	    if { $i eq "-" } { continue }
	    foreach "a b c d e f" $i break
	    set idx -1
	    foreach "x y" $coords {
		lset coords [incr idx] [expr {$a*$x+$c*$y+$e}]
		lset coords [incr idx] [expr {$b*$x+$d*$y+$f}]
	    }
	}
	return $coords
    }
    method tr_coords { names } {
	set coords ""
	foreach i $names {
	    set d [dict get $state $i]
	    if { ![string is double -strict $d] } {
		error "error: attribute $i is not a valid number state=$state"
	    }
	    lappend coords $d
	}
	return [$self tr $coords]
    }
    method tr_scale {} {
	set v2 [$self tr "1 0"]
	set v1 [$self tr "0 0"]
	return [expr {abs([lindex $v2 0]-[lindex $v1 0])}]
    }
    method tr_scalexy {} {
	set v2 [$self tr "1 0"]
	set v1 [$self tr "0 0"]
	set sx [expr {abs([lindex $v2 0]-[lindex $v1 0])}]
	set v2 [$self tr "0 1"]
	set v1 [$self tr "0 0"]
	set sy [expr {abs([lindex $v2 1]-[lindex $v1 1])}]
	return [list $sx $sy]
    }
    method tr_main_scale {} {
	set v2 [$self tr_main "1 0"]
	set v1 [$self tr_main "0 0"]
	return [expr {abs([lindex $v2 0]-[lindex $v1 0])}]
    }
    method tr_main_scalexy {} {
	set v2 [$self tr_main "1 0"]
	set v1 [$self tr_main "0 0"]
	set sx [expr {abs([lindex $v2 0]-[lindex $v1 0])}]
	set v2 [$self tr_main "0 1"]
	set v1 [$self tr_main "0 0"]
	set sy [expr {abs([lindex $v2 1]-[lindex $v1 1])}]
	return [list $sx $sy]
    }
    method apply_transform_list { list coords } {
	foreach i [lreverse $list] {
	    if { $i eq "-" } { continue }
	    foreach "a b c d e f" $i break
	    set idx -1
	    foreach "x y" $coords {
		lset coords [incr idx] [expr {$a*$x+$c*$y+$e}]
		lset coords [incr idx] [expr {$b*$x+$d*$y+$f}]
	    }
	}
	return $coords
    }
    method apply_transform_list_inv { list coords } {
	foreach i $list {
	    if { $i eq "-" } { continue }
	    foreach "a b c d e f" $i break
	    set idx -1
	    foreach "x y" $coords {
		lset coords [incr idx] [expr {1.0*($d*$x-$c*$y+$c*$f-$d*$e)/($d*$a-$c*$b)}]
		lset coords [incr idx] [expr {1.0*(-1*$b*$x+$a*$y+$b*$e-$a*$f)/($d*$a-$c*$b)}]
	    }
	}
	return $coords
    }
    method transform_mark {} {
	lappend transform_stack -
    }
    method push_transform { a b c d e f } {
	lappend transform_stack [list $a $b $c $d $e $f]
    }
    method push_transform_list { list } {
	foreach i $list {
	    lappend transform_stack $i
	}
    }
    method svg_to_transform { transform } {
	set list ""
	foreach "- name args" [regexp -all -inline {(\w+)\(([^\)]*)\)} $transform] {
	    foreach "a b c d e f" [list 1.0 0.0 0.0 1.0 0.0 0.0] break
	    regsub -all {,} $args { } args
	    switch $name {
		translate {
		    foreach "e f" $args break
		}
		rotate {
		    set degtorad [::math::constants::give_constant degtorad]
		    set angle [expr {$degtorad*$args}]
		    set cosa [expr {cos($angle)}]
		    set sina [expr {sin($angle)}]
		    set a $cosa
		    set b $sina
		    set c [expr {-1*$sina}]
		    set d $cosa
		}
		scale {
		    set d ""
		    foreach "a d" $args break
		    if { $d eq "" } { set d $a }
		}
		matrix {
		    foreach "a b c d e f" $args break
		}
	    }
	    lappend list [list $a $b $c $d $e $f]
	}
	return $list
    }
    # it is necessary to use 'transform_mark' before in order to pop transform
    method push_transform_svg { transform } {
	$self push_transform_list [$self svg_to_transform $transform]
    }
    method pop_transform {} {
	set transform_stack [lrange $transform_stack 0 end-1]
    }
    method pop_transform_list { list } {
	foreach i $list {
	    set transform_stack [lrange $transform_stack 0 end-1]
	}
    }
    method pop_transform_to_mark {} {
	while { [llength $transform_stack] } {
	    set last [lindex $transform_stack end]
	    set transform_stack [lrange $transform_stack 0 end-1]
	    if { $last eq "-" } { break }
	}
    }
    method push_transform_rect { p1 p2 } {

	set ns { svg http://www.w3.org/2000/svg }
	foreach "l1x l1y l2x l2y" [list "" "" "" ""] break
	foreach node [$svgNode selectNodes -namespaces $ns "//svg:rect"] {
	    set params [split [$node @ramdraw:params ""] ,]
	    if { [dict get $params type] ne "hotpoint" } { continue }
	    if { [dict get $params hid] == $p1 } {
		set l1x [dict get $params l1x]
		set l1y [dict get $params l1y]
	    } elseif { [dict get $params hid] == $p2 }  {
		set l2x [dict get $params l1x]
		set l2y [dict get $params l1y]
	    }
	}
	foreach "- - - lines_new" [$self _update_parameters] break
	foreach i [list 1x 1y 2x 2y] {
	    set c$i [dict get $lines_new l[set l$i]]
	}
	set scale_x [expr {1.0*($c2x-$c1x)}]
	set scale_y [expr {1.0*($c2y-$c1y)}]

	set trs "-"
	lappend trs [list 1 0 0 1 $c1x $c1y]
	lappend trs [list $scale_x 0 0 $scale_y 0 0]
	$self push_transform_list $trs
    }
    method pop_transform_rect {} {
	$self pop_transform_to_mark
    }
    method transform_sweep_flag {} {
	# every flip in either x or y axes require to swap the sweep-flag of
	# the triangles

	set sweep_flag 0
	foreach i [lreverse $transform_stack] {
	    if { $i eq "-" } { continue }
	    foreach "a b c d e f" $i break
	    if { $a < 0 } { set sweep_flag [expr {($sweep_flag)?0:1}] }
	    if { $d < 0 } { set sweep_flag [expr {($sweep_flag)?0:1}] }
	}
	return $sweep_flag
    }
    variable draw_handler_afterid
    method draw_handler {} {
	if { [info exists draw_handler_afterid] } {
	    after cancel $draw_handler_afterid
	    unset draw_handler_afterid
	}
	set draw_handler_afterid [after 100 [mymethod draw]]
    }
    method stop_animation {} {
	after cancel $animate_callback
	set animate_callback ""
    }
    method toggle_stop_continue_animation {} {
	if { [$self is_animation_running] } {
	    $self stop_animation
	} else {
	    $self draw -continue_draw_time 1
	}
    }
    method has_animations {} {

	if { $svgNode eq "" } { return 0 }
	set ns { svg http://www.w3.org/2000/svg }
	set xp {svg:animate|svg:animateMotion}
	if { [llength [$svgNode selectNodes -namespaces $ns $xp]] } {
	    return 1
	} else {
	    return 0
	}
    }
    method is_animation_running {} {
	if { $animate_callback ne "" } { return 1 }
	return 0
    }
    variable drawing
    method enable_disable_draw { enable_disable } {
	if { $enable_disable eq "enable" } {
	    unset -nocomplain drawing
	    $self draw
	} else {
	    set drawing 1
	    if { $canvas ne "" } { $canvas delete items }
	}
    }
    method give_animate_end_time {} {
	$self _calc_animate_list
	set end_time 0
	foreach i $animateList {
	    lassign $i id p
	    if { [dict get $p end] ne "indefinite" && [dict get $p end] > $end_time } {
		set end_time [dict get $p end]
	    }
	}
	return $end_time
    }
    method _calc_animate_list {} {
	set animateList ""
	set isAnimated 0
	set ns { svg http://www.w3.org/2000/svg }
	set xp {svg:animate|svg:animateMotion}
	foreach i [$svgNode selectNodes -namespaces $ns $xp] {
	    set id [string trimleft [$i @xlink:href ""] #]
	    set p ""
	    # begin and dur only allowed in seconds
	    dict set p begin [string trim [$i @begin 0] s]
	    dict set p dur [string trim [$i @dur indefinite] s]
	    if { [dict get $p dur] eq "indefinite" } {
		dict set p end indefinite
	    } else {
		dict set p end [expr {[dict get $p begin]+[dict get $p dur]}]
	    }
	    if { [$i nodeName] eq "animateMotion" } {
		dict set p type animateMotion
		dict set p rotate [$i @rotate 0]
		dict set p fill [$i @fill remove]
		set path_id [$i selectNodes -namespaces $ns string(svg:mpath/@xlink:href)]
		foreach "knots pnts" [$self _process_path_for_animateMotion \
		        [string trimleft $path_id #]] break
		dict set p knots $knots
		dict set p pnts $pnts
	    } else {
		dict set p type animate
		dict set p attributeName [$i @attributeName]
		dict set p to [$i @to ""]
		dict set p state none
	    }
	    lappend animateList [list $id $p]
	    set isAnimated 1
	}
	set animateList [lsort -index 0 $animateList]
    }
    proc give_layers_and_groups { nodeP } {
	set ns { svg http://www.w3.org/2000/svg }
	set title [$nodeP selectNodes -namespaces $ns string(svg:title)]
	if { $title eq "" } { set title [_ "Automatic"] }
	regsub -all {[\s_]+} $title { } title
	set title [string totitle $title]
	
	set layersList [list $title]
	set groupsList ""
	set xp {*[@ramdraw:groups or @ramdraw:groups1 or @ramdraw:groups2]}
	foreach node [$nodeP selectNodes -namespaces $ns $xp] {
	    set layer [lindex [split [$node @ramdraw:groups ""] ","] 0]
	    if { $layer ne "" } {
		lappend layersList $layer
	    }
	    lappend groupsList {*}[give_groups $node]
	}
	set layersList [lsort -unique $layersList]
	set groupsList [lsort -unique $groupsList]
	
	return [list $title $layersList $groupsList]
    }
    # args: ?-svgNode svgNode? ?-x x? ?-y y? ?-width width? ?-height height? 
    #       ?-update_view boolean? ?-mirror_y boolean? ?-reset_draw_time boolean?
    method draw { args } {
	catch { unset draw_handler_afterid }
	after cancel $animate_callback
	set animate_callback ""

	if { [info exists drawing] } {
	    return
	}
	set drawing 1

	set err [catch { eval $self draw_do $args } ret]
	if { $err } {
	    if { [info exists ::debug] && $::debug } {
		set debug 1
	    } elseif { [info exists ::lognoter_debug] && $::lognoter_debug } {
		set debug 1
	    } else {
		set debug 0
	    }
	    if { $debug } {
		snit_messageBox -message $::errorInfo
	    } else {
		snit_messageBox -message $ret
	    }
	}
	unset -nocomplain drawing
	return $ret
    }
    method draw_do { args } {

	set save_svgNode $svgNode
	
	set optional {
	    { -svgNode node "" }
	    { -reset_draw_time boolean 0 }
	    { -continue_draw_time boolean 0 }
	    { -draw_one_frame boolean 0 }
	    { -x coord "" }
	    { -y coord "" }
	    { -width coord "" }
	    { -height coord "" }
	    { -mirror_y boolean 0 }
	    { -update_view boolean 1 }
	    { -padding padding "-" }
	    { -image_gd_file file "" }
	    { -gid_output boolean 0 }
	}
	set compulsory ""
	parse_args $optional $compulsory $args
	
	if { $svgNode eq "" } { set svgNode $save_svgNode }
	if { $canvas ne "" && $options(-auto_redraw) && [winfo height $canvas] == 1 } { return }

	if { $svgNode eq "" } { return }

	if { $options(-delete_dimensions) } { $self _delete_dimensions }
	if { $reset_draw_time } { set draw_time_start "" }
	foreach i [list x y width height] {
	    if { [set $i] ne "" } { set draw_$i [set $i] }
	}
	if { $padding ne "-" } { $self configure -padding $padding }

	if { [info exists draw_width] } {
	    set draw_width_now $draw_width
	} elseif { $canvas ne "" } {
	    set draw_width_now [winfo width $canvas]
	    if { $options(-padding) ne "" } {
		incr draw_width_now [expr {-1*([lindex $options(-padding) 0]+
		        [lindex $options(-padding) 2])}]
	    }
	} elseif { [$svgNode @width ""] ne "" } {
	    set draw_width_now [$svgNode @width]
	} else {
	    set draw_width_now 600
	}
	if { [info exists draw_height] } {
	    set draw_height_now $draw_height
	} elseif { $canvas ne "" } {
	    set draw_height_now [winfo height $canvas]
	    if { $options(-padding) ne "" } {
		incr draw_height_now [expr {-1*([lindex $options(-padding) 1]+
		        [lindex $options(-padding) 3])}]
	    }
	} elseif { [$svgNode @height ""] ne "" } {
	    set draw_height_now [$svgNode @height]
	} else {
	    lassign [$svgNode @viewBox] - - width_v height_v
	    set draw_height_now [expr {$draw_width_now*$height_v/double($width_v)}]
	}
	foreach "draw_x_now draw_y_now" [list $draw_x $draw_y] break
	if { $options(-padding) ne "" } {
	    if { $draw_x_now == 0 } {
		set draw_x_now [lindex $options(-padding) 0]
	    }
	    if { $draw_y_now == 0 } {
		set draw_y_now [lindex $options(-padding) 1]
	    }
	}
	if { $image_gd_file ne "" } {
	    package require gd
	    set image_gd [gd open -width $draw_width_now -height $draw_height_now \
		    -background [gd_color "" white]]
	    if { $::tcl_platform(platform) eq "unix" } {
		if { ![info exists ::Freetype_Font_Aliases] } {
		    array set ::Freetype_Font_Aliases [ gd fontalias [ glob -nocomplain /usr/share/fonts/truetype/freefont/*.ttf ] ]
		}
		gd config $image_gd -fonttable ::Freetype_Font_Aliases
	    }
	} elseif { $gid_output } {
	    lassign [give_layers_and_groups $svgNode] layer layersList groupsList
	    set gid_buffer "*****TCL [list param_creator::init_create $layer $layersList $groupsList]\n"
	}
	set err 0
	if { $update_view } {
	    set err [catch { $self update_svgtree } ret]
	    if { !$err } {
		lassign $ret factor trans_x trans_y
	    } else {
		if { $options(-parameters) ne "" } {
		    set errstring $ret
		} else {
		    set err 0
		}
		lassign [list 1.0 0.0 0.0] factor trans_x trans_y
	    }
	    set transform_stack_main 0
	    set transform_stack ""
	    set viewBox [$svgNode @viewBox ""]
	    if { $viewBox eq "" } {
		set viewBox [list 0 0 [_length [$svgNode @width]] \
		        [_length [$svgNode @height]]]
	    }
	    if { !$gid_output } {
		set scale_x [expr {1.0*$draw_width_now/[lindex $viewBox 2]}]
		set scale_y [expr {1.0*$draw_height_now/[lindex $viewBox 3]}]
		
		set preserveAspectRatio [$svgNode @preserveAspectRatio xMidYMid]
		
		if { $preserveAspectRatio ne "none" } {
		    if { $scale_x > $scale_y } {
		        set scale_x $scale_y
		    } else {
		        set scale_y $scale_x
		    }
		}
		set t_x [expr {-1*([lindex $viewBox 0]+.5*[lindex $viewBox 2])*$scale_x}]
		set trans_x [expr {$t_x+$draw_x_now+.5*$draw_width_now}]
		set t_y [expr {-1*([lindex $viewBox 1]+.5*[lindex $viewBox 3])*$scale_y}]
		if { !$mirror_y } {
		    set trans_y [expr {$t_y+$draw_y_now+.5*$draw_height_now}]
		} else {
		    set scale_y [expr {-1*$scale_y}]
		    set trans_y [expr {-$t_y+$draw_y_now-.5*$draw_height_now}]
		}
	    } else {
		set scale_x [expr {1.0/$factor}]
		set scale_y [expr {-1.0*$scale_x}]
	    }
	    lappend transform_stack [list $scale_x 0.0 0.0 $scale_y $trans_x $trans_y]
	}

	set delete_pending_items ""
	if { $draw_time_start eq "" && $canvas ne "" && $image_gd eq "" && !$gid_output } {
	    $canvas delete items

	    $self _calc_animate_list

	    if { $isAnimated } {
		set draw_time_start [clock clicks -milliseconds]
		set draw_time 0
		$self check_time_variable set
	    }
	} elseif { $options(-animate) && $isAnimated && $canvas ne "" && $image_gd eq "" && 
	    !$gid_output } {
	    if { !$continue_draw_time } {
		set draw_time [expr {([clock clicks -milliseconds]-$draw_time_start)/1000.0}]
		$self check_time_variable set
	    } else {
		set draw_time_start [expr {[clock clicks -milliseconds]-1000.0*$draw_time}]
	    }
	    set delete_pending_items [$canvas find withtag animated]
	    #$canvas delete animated
	} elseif { $canvas ne "" && $image_gd eq "" && !$gid_output } {
	    $canvas delete items
	}
	if 0 {
	    # draw lines for testing
	    for { set c 0.1 } { $c <= 1.0 } { set c [expr {$c+.1}] } {
		$canvas create line [$self tr "0 $c 1 $c"] -fill blue
		set txt "[format %.3g $c] - [format %.3g [lindex [$self tr "0.1 $c"] 1]]"
		$canvas create text [$self tr "0.1 $c"] -text $txt \
		    -fill blue -anchor sw
	    }
	}
	set state ""
	foreach i [$svgNode childNodes] {
	    $self do_child $i
	}
	if { $options(-draw_callback) ne "" } {
	    if { $canvas ne "" } {
		uplevel 1 $options(-draw_callback) $canvas
	    } else {
		uplevel 1 $options(-draw_callback)
	    }
	}
	if { $delete_pending_items ne "" } {
	    eval $canvas delete $delete_pending_items
	}
	if { $err } {
	    if { $image_gd ne "" } {
		foreach "x y" [list [expr {$draw_width_now-10}] \
		        [expr {$draw_height_now-10}]] break
		gd text $image_gd $x $y -fill [gd_color $image_gd red] \
		    -text $errstring -font "Arial 10" -anchor se
	    } elseif { $gid_output } {
		# nothing
	    } elseif { $canvas ne "" } {
		$canvas create text [expr {$draw_width_now-10}] \
		    [expr {$draw_height_now-10}] -fill red \
		    -text $errstring -anchor se -tags "items errorstring"
	    } else {
		PDFWriter::WriteText $errstring [expr {$draw_width_now-10}] \
		    [expr {$draw_height_now-10}] Helvetica \
		    12 se red -tags items
	    }
	} else {
	    if { $canvas ne "" } { $canvas delete errorstring }
	    if { $isAnimated && $canvas ne "" } {
		if { $options(-draw_animation_time) } {
		    $canvas delete datime
		    $canvas create text [expr {$draw_width_now-10}] \
		        [expr {$draw_height_now-10}] -fill red \
		        -text [format %.3g $draw_time] -anchor se -tags datime
		}
		set o_animateList $animateList
		set animateList ""
		set AnimationHasFinished 1
		foreach i $o_animateList {
		    foreach "id p" $i break
		    if { [dict get $p end] eq "indefinite" || [dict get $p end] >= $draw_time } {
		        lappend animateList [list $id $p]
		        if { [dict get $p end] ne "indefinite" } {
		            set AnimationHasFinished 0
		        }
		    }
		}
		if { $options(-animate) && !$AnimationHasFinished && !$draw_one_frame } {
		    set animate_callback [after 25 [list catch [mymethod draw_do]]]
		}
	    }
	}
	if { $image_gd_file ne "" } {
	    if { $::tcl_platform(platform) eq "windows" } {
		set file [file attributes $image_gd_file -shortname]
	    } else {
		set file $image_gd_file
	    }
	    gd close $image_gd -save -file $file
	    set image_gd ""
	    set image_gd_file ""
	} elseif { $gid_output } {
	    append gid_buffer "*****TCL param_creator::end_create\n"
	    return $gid_buffer
	} 
    }
    method _process_path_for_animateMotion { path_id } {

	set pathNode [$svgNode selectNodes "//*\[@id='$path_id'\]"]

	regsub -all {[MCz]} [$pathNode @d ""] {} d
	if { $d eq "" } {
	    error "error processing path for animateMotion"
	}
	regsub -all {,} $d { } d
	set total_len 0.0
	set lens ""
	foreach "x0 y0" [lrange $d 0 1] break
	foreach "x1 y1 x2 y2 x3 y3" [lrange $d 2 end] {
	    set len [m::eval_cubic_bezier_length $x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3]
	    lappend lens $len
	    set total_len [expr {$total_len+$len}]
	    foreach "x0 y0" [list $x3 $y3] break
	}
	set knots ""
	set sum 0.0
	foreach len $lens {
	    set sum [expr {$sum+$len}]
	    lappend knots [expr {$sum/$total_len}]
	}
	return [list $knots $d]
    }
    # args: ?-svgNode svgNode? values
    method draw_values { args } {
	
	while { [string match -* [lindex $args 0]] } {
	    switch -- [lindex $args 0] {
		-svgNode {
		    set args [lrange $args 1 end]
		    set svgNode [lindex $args 0]
		}
		default { error "error in draw_values" }
	    }
	    set args [lrange $args 1 end]
	}
	$self configure -parameters [lindex $args 0]
	if { $canvas ne "" } { $self draw }
    }
    method _delete_dimensions {} {

	if { $svgNode eq "" } { return }
	set ns { svg http://www.w3.org/2000/svg }
	foreach node [$svgNode selectNodes -namespaces $ns svg:g] {
	    set params [split [$node @ramdraw:params ""] ,]
	    if { [dict get $params type] eq "line_cota" } {
		$node delete
	    }
	}
    }
    method zoom { what } {
	switch $what {
	    in { $self visualization_rectangle [mymethod _drawzoom in] start }
	    out { $self visualization_rectangle [mymethod _drawzoom out] start }
	    last { $self _drawzoom last - - - - - }
	    frame { $self _drawzoom frame - - - - - }
	}
    }
    method contextual_submenu { x y { full_menu 0 } } {
	destroy $canvas.cmenu
	set menu [menu $canvas.cmenu -tearoff 0]

	if { $options(-add_zoom_to_contextual) || $full_menu } {
	    $menu add command -label [_ "Zoom in"] -command \
		[mymethod visualization_rectangle [mymethod _drawzoom in] start]
	    $menu add command -label [_ "Zoom out"] -command \
		[mymethod visualization_rectangle [mymethod _drawzoom out] start]
	    $menu add command -label [_ "Zoom last"] -command \
		[mymethod _drawzoom last - - - - -]
	    $menu add separator
	    $menu add command -label [_ "Zoom frame"] -command \
		[mymethod _drawzoom frame - - - - -]
	    if { $isAnimated } {
		$menu add separator
		$menu add command -label [_ "Play again"] -command \
		    [mymethod draw -reset_draw_time 1]
		if { [$self is_animation_running] } {
		    $menu add command -label [_ "Stop animation (space)"] -command \
		        [mymethod stop_animation]
		} else {
		    $menu add command -label [_ "Continue animation (space)"] -command \
		        [mymethod draw -continue_draw_time 1]
		}
	    }
	}
	if { $full_menu } {
	    $menu add separator
	    $menu add command -label [_ "Print PDF"] -command \
		[mymethod print_pdf]
	    $menu add command -label [_ "Print image"] -command \
		[mymethod print_image]
	    $menu add command -label [_ "Save SVG file"] -command \
		[mymethod save_svg]
	}

	if { $options(-add_to_contextual) ne "" } {
	    uplevel 1 $options(-add_to_contextual) $menu
	}
	if { [$menu index end] eq "none" } {
	    destroy $menu
	    return
	}
	tk_popup $menu $x $y
    }
    proc _returnminmax { a b } {
	if { $a <= $b } {
	    return [list $a $b]
	}
	return [list $b $a]
    }
    variable startrectanglecoords
    method visualization_rectangle { what action { coords "" } } {
	
	if { [lindex [bindtags $canvas] 0] == "pan_canvas" } {
	    Pan ESC
	}
	
	set w [winfo toplevel $canvas]
	
	switch $action {
	    start_simple {
		if { [llength [$canvas find withtag rectangleL1]] } { return }
		set x [$canvas canvasx [expr {[winfo pointerx $w]-[winfo rootx $canvas]}]]
		set y [$canvas canvasy [expr {[winfo pointery $w]-[winfo rooty $canvas]}]]

		if { [lsearch -exact [bindtags $canvas] rec_canvas] == -1 } {
		    bindtags $canvas [concat rec_canvas [bindtags $canvas]]
		}
		foreach "ev key" [list <KeyPress-Escape> ESC <ButtonPress-1> BP1] {
		    set cmd [mymethod visualization_rectangle $what $key [list %x %y]]
		    bind rec_canvas $ev $cmd
		}
	    }
	    start {
		if { [llength [$canvas find withtag rectangleL1]] } { return }
		set x [$canvas canvasx [expr {[winfo pointerx $w]-[winfo rootx $canvas]}]]
		set y [$canvas canvasy [expr {[winfo pointery $w]-[winfo rooty $canvas]}]]
		
		set status [_ "Select a rectangle in the screen"]
		
		if { [lsearch -exact [bindtags $canvas] rec_canvas] == -1 } {                
		    bindtags $canvas [concat rec_canvas [bindtags $canvas]]
		}

		$canvas conf -cursor crosshair

		foreach "ev key" [list <KeyPress-Escape> ESC <ButtonPress-1> BP1 <Motion> BM] {
		    set cmd [mymethod visualization_rectangle $what $key [list %x %y]]
		    bind rec_canvas $ev "$cmd ; break"
		}
		$canvas create line [$canvas canvasx 0] $y [$canvas canvasx [winfo width $canvas]] $y \
		    -fill grey -tags rectangleL1
		$canvas create line $x [$canvas canvasy 0] $x [$canvas canvasy \
		        [winfo height $canvas]] \
		    -fill grey -tags rectangleL2
	    }
	    end - ESC {
		set status ""
		$canvas delete rectangleL1 rectangleL2 rectangle
		if { [set ipos [lsearch -exact [bindtags $canvas] rec_canvas]] != -1 } {
		    bindtags $canvas [lreplace [bindtags $canvas] $ipos $ipos]
		}
		$canvas conf -cursor ""
		#return -code break
	    }
	    BM {
		foreach "x y" [list [$canvas canvasx [lindex $coords 0]] \
		        [$canvas canvasy [lindex $coords 1]]] break
		$canvas coords rectangleL1 [$canvas canvasx 0] $y [$canvas canvasx \
		        [winfo width $canvas]] $y
		$canvas coords rectangleL2 $x [$canvas canvasy 0] $x [$canvas canvasy \
		        [winfo height $canvas]]
		#return -code break
	    }
	    BP1 {
		set status ""
		set coords [list [$canvas canvasx [lindex $coords 0]] \
		        [$canvas canvasy [lindex $coords 1]]]
		$canvas coords rectangleL1 [$canvas canvasx 0] [lindex $coords 1] \
		    [$canvas canvasx [winfo width $canvas]] [lindex $coords 1]
		$canvas delete rectangleL1 rectangleL2
		
		$canvas create rectangle [concat $coords $coords] -outline red -width 1 \
		    -tags rectangle
		
		if { [lsearch -exact [bindtags $canvas] rec_canvas] == -1 } {
		    bindtags $canvas [concat rec_canvas [bindtags $canvas]]
		}
		foreach "ev key" [list <KeyPress-Escape> ESC <Motion> B1M \
		        <ButtonRelease-1> BR1 <Shift-ButtonRelease-1> SBR1] {
		    set cmd [mymethod visualization_rectangle $what $key [list %x %y]]
		    bind rec_canvas $ev $cmd
		}
		set startrectanglecoords $coords
		#return -code break
	    }
	    B1M {
		set coords [list [$canvas canvasx [lindex $coords 0]] \
		        [$canvas canvasy [lindex $coords 1]]]
		foreach "x1 x2" [_returnminmax [lindex  $startrectanglecoords 0] \
		        [lindex $coords 0]] break
		foreach "y1 y2" [_returnminmax [lindex  $startrectanglecoords 1] \
		        [lindex $coords 1]] break
		$canvas coords rectangle $x1 $y1 $x2 $y2
		#return -code break
	    }
	    BR1 - SBR1 {
		set coords [list [$canvas canvasx [lindex $coords 0]] \
		        [$canvas canvasy [lindex $coords 1]]]
		foreach "x1 x2" [_returnminmax [lindex  $startrectanglecoords 0] \
		        [lindex $coords 0]] break
		foreach "y1 y2" [_returnminmax [lindex  $startrectanglecoords 1] \
		        [lindex $coords 1]] break
		foreach i "x1 x2 y1 y2" { set $i [expr {int([set $i])}] }
		
		$canvas delete rectangle
		
		if { [set ipos [lsearch -exact [bindtags $canvas] rec_canvas]] != -1 } {
		    bindtags $canvas [lreplace [bindtags $canvas] $ipos $ipos]
		}
		bind rec_canvas <Motion> ""
		$canvas conf -cursor ""
		#             if { abs($x1-$x2) < 3 || abs($y1-$y2) < 3 } {
		    #                 tk_messageBox -message \
		        #                     "rectángulo demasiado pequeño (mueva el mouse con el botón presionado)"
		    #                 return
		    #             }
		uplevel $what $x1 $y1 $x2 $y2 $action
		#return -code break
	    }
	}
    }
    method _drawzoom { what x1 y1 x2 y2 action } {
	
	switch $what {
	    in {
		if { $x1 == $x2 || $y2 == $y1 } {
		    set fac 1.2
		} else {
		    set fac1 [expr {[winfo width $canvas]/double($x2-$x1)}]
		    set fac2 [expr {[winfo height $canvas]/double($y2-$y1)}]
		    set fac [expr {($fac1<$fac2)?$fac1:$fac2}]
		}
		set x [expr {($x1+$x2)/2.0}]
		set y [expr {($y1+$y2)/2.0}]
		set xmove [expr {[winfo width $canvas]/2.0-$x}]
		set ymove [expr {[winfo height $canvas]/2.0-$y}]
		set e [expr {$xmove+(1.0-$fac)*$x}]
		set f [expr {$ymove+(1.0-$fac)*$y}]
		incr transform_stack_main
		set transform_stack [linsert $transform_stack 0 [list $fac 0.0 0.0 $fac $e $f]]
		$self draw -update_view 0
	    }
	    out {
		if { $x1 == $x2 || $y2 == $y1 } {
		    set fac 0.8
		} else {
		    set fac1 [expr {double($x2-$x1)/[winfo width $canvas]}]
		    set fac2 [expr {double($y2-$y1)/[winfo height $canvas]}]
		    set fac [expr {($fac1>$fac2)?$fac1:$fac2}]
		}
		set x [expr {($x1+$x2)/2.0}]
		set y [expr {($y1+$y2)/2.0}]
		set e [expr {(1.0-$fac)*$x}]
		set f [expr {(1.0-$fac)*$y}]
		incr transform_stack_main
		set transform_stack [linsert $transform_stack 0 [list $fac 0.0 0.0 $fac $e $f]]
		$self draw -update_view 0
	    }
	    pan {
		# here items are not moved because they were moved dynamically
		set x [expr {$x2-$x1}]
		set y [expr {$y2-$y1}]
		incr transform_stack_main
		set transform_stack [linsert $transform_stack 0 [list 1.0 0.0 0.0 1.0 $x $y]]
		$self draw -update_view 0
	    }
	    last {
		if { $transform_stack_main == 0 } { bell; return } 
		set transform_stack [lrange $transform_stack 1 end]
		incr transform_stack_main -1
		$self draw -update_view 0
	    }
	    frame {
		set transform_stack [lrange $transform_stack $transform_stack_main end]
		set transform_stack_main 0
		$self draw -update_view 0
	    }
	}
    }
    variable x0
    variable y0
    variable save_props
    method manage_sel { what x y } {
	
	switch $what {
	    BP1 {
		foreach c [$canvas find withtag selection] {
		    $canvas itemconfigure $c -fill $save_props($c)
		}
		$canvas delete select_rectangle
		$canvas dtag selection
		array unset save_props
		foreach c [$canvas find closest $x $y] {
		    if { [$canvas type $c] ne "text" } { continue }
		    if { [lsearch -exact [$canvas find overlapping $x $y $x $y] $c] == -1 } {
		        continue
		    }
		    set save_props($c) [$canvas itemcget $c -fill]
		    $canvas itemconfigure $c -fill [$canvas cget -selectforeground]
		    $canvas addtag selection withtag $c
		    set r [$canvas create rectangle [$canvas bbox $c] -fill \
		            [$canvas cget -selectbackground] \
		            -outline "" -tags "select_rectangle"]
		    $canvas lower $r
		}
		foreach "x0 y0" [list $x $y] break
#                 set bbox [$canvas bbox selection]
#                 if { $bbox ne "" } {
#                     $canvas create rectangle [$canvas bbox selection] -fill \
#                         [$canvas cget -selectbackground] \
#                         -outline "" -tags "select_rectangle"
#                     $canvas lower select_rectangle
#                 }
	    }
	    BM1 {
		if { ![info exists x0] } { return }
		foreach c [$canvas find withtag selection] {
		    $canvas itemconfigure $c -fill $save_props($c)
		}
		$canvas delete select_rectangle
		$canvas dtag selection
		array unset save_props
		set ids [lsort -integer [$canvas find overlapping $x0 $y0 $x $y]]
		foreach "min max" [list "" ""] break
		foreach c $ids {
		    if { [$canvas type $c] ne "text" } { continue }
		    if { $min eq "" || $c < $min } { set min $c }
		    if { $max eq "" || $c > $max } { set max $c }
		}
		if { $min eq "" || $max eq "" } { return }
		for { set c $min } { $c <= $max } { incr c } {
		    if { [$canvas type $c] ne "text" } { continue }
		    set save_props($c) [$canvas itemcget $c -fill]
		    $canvas itemconfigure $c -fill [$canvas cget -selectforeground]
		    $canvas addtag selection withtag $c
		    set r [$canvas create rectangle [$canvas bbox $c] -fill \
		            [$canvas cget -selectbackground] \
		            -outline "" -tags "select_rectangle"]
		    $canvas lower $r
		}
#                 set bbox [$canvas bbox selection]
#                 if { $bbox ne "" } {
#                     $canvas coords select_rectangle [$canvas bbox selection]
#                 }
	    }
	    BR1 {
		unset -nocomplain x0 y0
	    }
	}
    }
    variable coordinates_variableData
    method coordinates_variable { variable format } {
	set coordinates_variableData [list $variable $format]
	bind $canvas <Motion> [mymethod _coordinates_variable_do %x %y]
    }
    method _coordinates_variable_do { x y } {
	set x [$canvas canvasx $x]
	set y [$canvas canvasy $y]
	set err [catch { $self tr_inv [list $x $y] } ret]
	if { $err } {
	    uplevel #0 [list set $v "!ERROR"]
	    return
	}
	foreach "x y" $ret break
	foreach "v format" $coordinates_variableData break
	uplevel #0 [list set $v [format $format $x $y]]
    }
    proc parse_formula { formula } {
	regsub -all {\m[a-zA-Z]\w*(?!\(|\w|")} $formula {$&} formula
	regsub -all {\^} $formula {**} formula
	return $formula
    }
    proc e { arg } {
	return [formulae eval [list expr [parse_formula $arg]]]
    }
    method _give_param { n } {
	set v [dict_getd $options(-parameters) $n ""]
	catch { set v [dict get $v v] }
	return $v
    }
    method update_svgtree {} {

	set ns { svg http://www.w3.org/2000/svg }

	lassign [$self _update_parameters] minis maxis lines_old lines_new factor

	set width [_length [$svgNode @width]]
	set height [_length [$svgNode @height]]

	if { ![$svgNode hasAttribute viewBox_save] } {
	    $svgNode setAttribute viewBox_save [$svgNode @viewBox]
	}
	set height_scale [lindex [$svgNode @viewBox_save] 3]
	
	if { $maxis ne "" } {
	    foreach i [list h v] {
		set fac0($i) 0
		set fac1($i) [expr {[dict get $maxis n_$i]/
		        [dict get $lines_old [dict get $maxis v_$i]]}]
		#             set fac [expr {1.0-1.0*[dict get $maxis n_$i]/
		    #                     [dict get $lines_old [dict get $maxis v_$i]]}]
		#             if { $fac > 0 } {
		    #                 set fac0($i) [expr {-.5*$fac}]
		    #                 set fac1($i) [expr {1.0-.5*$fac}]
		    #             } else {
		    #                 set fac0($i) 0
		    #                 set fac1($i) [expr {1.0-$fac}]
		    #             }
	    }
	    set delta_v [expr {$fac1(v)-$fac0(v)}]
	    set delta_h [expr {$fac1(h)-$fac0(h)}]
	    $svgNode setAttribute viewBox [list $fac0(v) $fac0(h) $delta_v \
		    [expr {$delta_h*$height_scale}]]
	}
	foreach node [$svgNode selectNodes {//*[@ramdraw:params]}] {
	    if { $maxis eq "" } { continue }
	    set params [split [$node @ramdraw:params] ,]
	    foreach "pos0 pos1 delta" [list "" "" ""] break
	    foreach i [list l1x l1y l2x l2y] {
		if { [dict exists $params $i] } {
		    set vold [dict get $lines_old l[dict get $params $i]]
		    set v [dict get $lines_new l[dict get $params $i]]
		    lappend pos0 $vold
		    lappend pos1 $v
		    lappend delta [expr {$v-$vold}]
		}
	    }
	    if { ![llength $delta] } { continue }
	    if { ![$node hasAttribute transform_save] } {
		$node setAttribute transform_save [$node @transform ""]
	    }
	    set transform [$node @transform_save]
	    set degtorad [::math::constants::give_constant degtorad]
	    set scale_y $height_scale
	    if { [llength $delta] == 2 } {
		set transform "translate($delta) $transform"
	    } elseif { [llength $delta] == 4 && [dict get $params type] eq "box" } {
		set radtodeg [::math::constants::give_constant radtodeg]
		set pos0A [lrange $pos0 0 1]
		set pos0B [lrange $pos0 2 3]
		set pos1A [lrange $pos1 0 1]
		set pos1B [lrange $pos1 2 3]
		set trs ""
		lappend trs "translate([join [m::scale -1 $pos0A] ,])"
		set V0 [m::sub [list [lindex $pos0B 0] [lindex $pos0A 1]] $pos0A]
		set V1 [m::sub [list [lindex $pos1B 0] [lindex $pos1A 1]] $pos1A]
		set err [catch { expr {1.0*[m::norm $V1]/[m::norm $V0]} } norm_x]
		if { $err } { set norm_x 1 }
		set V0 [m::sub [list [lindex $pos0A 0] [lindex $pos0B 1]] $pos0A]
		set V1 [m::sub [list [lindex $pos1A 0] [lindex $pos1B 1]] $pos1A]
		set err [catch { expr {1.0*[m::norm $V1]/[m::norm $V0]} } norm_y]
		if { $err } { set norm_y 1 }
		lappend trs "scale($norm_x,$norm_y)"
		lappend trs "translate([join $pos1A ,])"
		set transform "[join [lreverse $trs] { }] $transform"
		set scale_y [expr {$scale_y*$norm_y}]
	    } elseif { [llength $delta] == 4 } {
		set radtodeg [::math::constants::give_constant radtodeg]
		set pos0A [lrange $pos0 0 1]
		set pos0B [lrange $pos0 2 3]
		set pos1A [lrange $pos1 0 1]
		set pos1B [lrange $pos1 2 3]
		set trs ""
		lappend trs "translate([join [m::scale -1 $pos0A] ,])"
		set V0 [m::sub $pos0B $pos0A]
		set V1 [m::sub $pos1B $pos1A]
		set angle0 [expr {-1*$radtodeg*atan2([lindex $V0 1],[lindex $V0 0])}]
		lappend trs "rotate($angle0)"
		set err [catch { expr {1.0*[m::norm $V1]/[m::norm $V0]} } norm]
		if { $err } { set norm 1 }
		lappend trs "scale($norm,1.0)"
		set angle1 [expr {$radtodeg*atan2([lindex $V1 1],[lindex $V1 0])}]
		lappend trs "rotate($angle1)"
		lappend trs "translate([join $pos1A ,])"
		set transform "[join [lreverse $trs] { }] $transform"
		#set delta_angle [expr {$angle1+$angle0}]
		# WARNING: not sure if it must be angle0 or angle1
		set delta_angle [expr {$angle1}]
#                 set scale_y [expr {$scale_y*(abs($norm*sin($degtorad*$delta_angle))+ \
#                         abs(cos($degtorad*$delta_angle)))}]
		set f [expr {abs(sin($degtorad*$delta_angle))}]
		set scale_y [expr {$scale_y*($norm*$f+(1.0-$f))}]
	    }
	    $node setAttribute transform [string trim $transform]

	    set ns { svg http://www.w3.org/2000/svg }
	    set xp {descendant-or-self::*[@font-size]}
	    foreach child [$node selectNodes -namespaces $ns $xp] {
		set params [split [$child @ramdraw:params ""] ,]
		if { [dict exists $params redimension] && ![dict get $params redimension] } {
		    continue
		}
		if { [$child hasAttribute font-size_save] } {
		    $child setAttribute font-size [$child @font-size_save]
		}
		$child setAttribute font-size_save [$child @font-size]
		$child setAttribute font-size [expr {[_length [$child @font-size]]/$scale_y}]
		#mylog::debug "changing font-size form [$child @font-size] to [expr {[_length [$child @font-size]]/$scale_y}]"
	    }
	    set xp {descendant-or-self::*[@stroke-width]}
	    foreach child [$node selectNodes -namespaces $ns $xp] {
		if { [dict exists $params redimension] && ![dict get $params redimension] } {
		    continue
		}
		if { [$child hasAttribute stroke-width_save] } {
		    $child setAttribute stroke-width [$child @stroke-width_save]
		}
		$child setAttribute stroke-width_save [$child @stroke-width]
		$child setAttribute stroke-width [expr {[$child @stroke-width]/$scale_y}]
	    }
	}
	set xp "svg:defs/ramdraw:params/ramdraw:prop_param"
	foreach prop_paramNode [$svgNode selectNodes -namespaces $ns $xp] {
	    regexp {#(.*)} [$prop_paramNode @xlink:href] {} cid
	    set paramName [$prop_paramNode @parameter]
	    set param [$self _give_param $paramName]
	    if { $param eq "" } {
		lassign [list "" 0 0] e n last_idx
		foreach idx [regexp -inline -indices -all {[[:alpha:]]\w*} $paramName] {
		    append e [string range $paramName $last_idx [lindex $idx 0]-1]
		    set name [string range $paramName {*}$idx]
		    set param [$self _give_param $name]
		    if { [string is double -strict $param]  }  {
		        append e $param
		        incr n
		    } elseif { $param ne "" } {
		        append e \"$param\"
		        incr n
		    } else {
		        append e $name
		    }
		    set last_idx [expr {[lindex $idx 1]+1}]
		} 
		append e [string range $paramName $last_idx end]
		if { $n } {
		    set err [catch { eval expr $e } param]
		}
		if { !$n || $err } {
		    continue
		}
	    }
	    set att ""
	    set node [$svgNode selectNodes "//*\[@id='$cid'\]"]
	    if { $node eq "" } {
		error "error finding entity with id=$cid"
	    }
	    switch -- [$prop_paramNode @property] {
		state {
		    if { $param eq "hidden" } {
		        set v none
		    } else { set v "inherit" }
		    $node setAttribute display $v
		}
		rx - ry {
		    regsub -all {[\s,]+} [$node @d] { } d
		    regexp {^(.*)(A|a)(.*)$} $d {} moveto Aa arc
		    lassign $arc rx ry x-axis-rotation large-arc-flag sweep-flag c2x c2y
		    set len [string length [string trim [$node @transform_save ""]]]
		    set transform [string range [string trim [$node @transform ""]] \
		            0 end-$len]
		    set c0 [$self apply_transform_list_inv \
		            [$self svg_to_transform $transform] "0 0"]
		    switch -- [$prop_paramNode @property] {
		        rx {
		            set c1 [$self apply_transform_list_inv \
		                    [$self svg_to_transform $transform] "$param 0"]
		            set rx [expr {abs([lindex $c1 0]-[lindex $c0 0])}]
		            set rx [expr {$rx*$factor}]
		        }
		        ry {
		            set c1 [$self apply_transform_list_inv \
		                    [$self svg_to_transform $transform] "0 $param"]
		            set ry [expr {abs([lindex $c1 1]-[lindex $c0 1])}]
		            set ry [expr {$ry*$factor}]
		        }
		    }
		    set v "$moveto$Aa$rx,$ry ${x-axis-rotation} "
		    append v "${large-arc-flag},${sweep-flag} $c2x,$c2y"
		    $node setAttribute d $v
		}
		text {
		    foreach child [$node childNodes] { $child delete }
		    set newnode [[$svgNode ownerDocument] createTextNode $param]
		    $node appendChild $newnode
		}
		width {
		    $node setAttribute stroke-width [expr {$param*$factor}]
		}
		default {
		    error "error in update_svgtree. Unknown property '[$prop_paramNode @property]'"
		}
	    }
	}
	
	set xp {svg:rect[contains(@ramdraw:params,'hotpoint') and
	    contains(@ramdraw:params,'hid,1')]}
	set rNode [$svgNode selectNodes -namespaces $ns $xp]
	
	if { $rNode ne "" } {
	    set l1x [dict get [split [$rNode @ramdraw:params] ","] l1x]
	    set l1y [dict get [split [$rNode @ramdraw:params] ","] l1y]
	    set x [dict get $lines_new l$l1x]
	    set y [dict get $lines_new l$l1y]
	} elseif { [dict exists $minis n_v] } {
	    set x [dict get $minis n_v]
	    set y [dict get $maxis n_h]
	} else {
	    lassign [list 0 0] x y
	    set factor [expr {1.0*[lindex [$svgNode @viewBox] 2]/[$svgNode @width 1.0]}]
	}
	set trans_x [expr {-$x/$factor}]
	set trans_y [expr {$y/$factor}]

	return [list $factor $trans_x $trans_y]
    }
    method _update_parameters {} {
	set ns { svg http://www.w3.org/2000/svg } 
	
	lassign "" max_name max_value
	set xp "svg:defs/ramdraw:params/ramdraw:param"
	foreach node [$svgNode selectNodes -namespaces $ns $xp] {
	    set n [$node @n]
	    if { [string index [$node @value] 0] eq "\[" } { continue }
	    set v [$self _give_param $n]
	    if { $v ne "" } {
		if { [string is double -strict $v] && ($max_value eq "" || $v > $max_value) } {
		    lassign [list $n $v] max_name max_value
		}
	    } elseif { [regexp {^\w+$} $n] } {
		#error [_ "Parameter '%s' does not exist" $n]
	    }
	}
	if { ![interp exists formulae] } { interp create formulae }
	set xp [format_xpath {string(svg:defs/ramdraw:params/ramdraw:param[@n=%s]/@value)} \
		$max_name]
	set old_value [$svgNode selectNodes -namespaces $ns $xp]
	set err [catch { expr {1.0*$old_value/$max_value} } factor]
	if { $err || $max_value == 0.0 } { return "" }
	
	set xp "svg:defs/ramdraw:params/ramdraw:param"
	foreach node [$svgNode selectNodes -namespaces $ns $xp] {
	    set n [$node @n]
	    set v [$self _give_param $n]
	    if { $v ne "" } {
		if { ![string is double -strict $v]} { continue }
		#dict set options(-parameters) $n [expr {$factor*$v}]
		formulae eval set $n [expr {$factor*$v}]
	    }
	}
	foreach node [$svgNode selectNodes -namespaces $ns $xp] {
	    if { [string index [$node @value] 0] eq "\[" } {
		set v [eval [string range [$node @value] 1 end-1]]
		#dict set options(-parameters) [$node @n] $v
		formulae eval set [$node @n] $v
	    }
	}
	lassign "" minis maxis
	set xp "svg:defs/ramdraw:params/ramdraw:base_line"
	foreach node [$svgNode selectNodes -namespaces $ns $xp] {
	    if { [string is double -strict [$node @value]] } {
		set v [$node @value]
	    } else {
		set v [eval [string range [$node @value] 1 end-1]]
	    }
	    formulae eval set [$node @n] $v
	    dict set lines_new [$node @n] $v
	    dict set lines_old [$node @n] [$node @defaultvalue]
	    switch [$node @orient] {
		h {
		    if { ![dict exists $minis n_h] || $v < [dict get $minis n_h] } {
		        dict set minis n_h $v
		        dict set minis v_h [$node @n]
		    }
		    if { ![dict exists $maxis n_h] || $v > [dict get $maxis n_h] } {
		        dict set maxis n_h $v
		        dict set maxis v_h [$node @n]
		    }
		}
		v {
		    if { ![dict exists $minis n_v] || $v < [dict get $minis n_v] } {
		        dict set minis n_v $v
		        dict set minis v_v [$node @n]
		    }
		    if { ![dict exists $maxis n_v] || $v > [dict get $maxis n_v] } {
		        dict set maxis n_v $v
		        dict set maxis v_v [$node @n]
		    }
		}
	    }
	}
	return [list $minis $maxis $lines_old $lines_new $factor]
    }
    method give_base_line_coord { line } {
	set ns { svg http://www.w3.org/2000/svg } 
	set xp "svg:defs/ramdraw:params/ramdraw:base_line\[@n='l$line'\]"
	set node [$svgNode selectNodes -namespaces $ns $xp]
	if { $node eq "" } {
	    error "error in give_base_line_coord. $xp"
	}
	return [$node @defaultvalue]
    }
    proc convert_color { color } {

	set num {\s*(\d+%?)\s*}
	if { [regexp "\\s*rgb\\($num,$num,$num\\)" $color {} r g b] } {
	    foreach i [list r g b] {
		if { [regexp {(\d+)%} [set $i] {} per] } {
		    set $i [expr {round($per*255.0/100.0)}]
		}
	    }
	    set color [format "#%02x%02x%02x" $r $g $b]
	}
	return $color
    }
    method _common_att { node } {
	set atts ""
	foreach att [$node attributes] {
	    if { [llength $att] > 1 } { continue }
	    dict set atts $att [$node @$att]
	}
	$self _common_att_do $atts

	set ns { svg http://www.w3.org/2000/svg }
	if { [set descNode [$node selectNodes {*[name()='desc']}]] ne "" } {
	    dict set state desc [$descNode text]
	}
    }
    # common attributes
    method _common_att_do { atts } {

	$self transform_mark

	if { [dict exists $atts id] } {
	    set id [dict get $atts id]
	    dict set state id $id
	    set idChild 0
	} elseif { [dict exists $state id] } {
	    set id [dict get $state id]
	    set idChild 1
	} else { set id "" }

	if { $isAnimated } {
	    foreach ipos [lsearch -all -sorted -index 0 $animateList $id] {
		foreach "id p" [lindex $animateList $ipos] break
		if { [dict get $p type] eq "animate" } {
		    if { $draw_time < [dict get $p begin] } {
		        if { [dict get $p state] eq "none" } {
		            dict set p state begin
		            lset animateList $ipos 1 $p
		            dict set state visibility visible
		        }
		    } elseif { [dict get $p end] eq "indefinite" || \
		        $draw_time <= [dict get $p end] } {
		        if { [dict get $p state] eq "none" || [dict get $p state] eq "begin" } {
		            # WARNING: this may not work when there are several
		            # entities with the same id (as in blocks, use)
		            $canvas delete $id
		            dict set atts [dict get $p attributeName] \
		                [dict get $p to]
		            dict set p state animate
		            lset animateList $ipos 1 $p
		            dict set state visibility visible
		        }
		    } else {
		        if { [dict get $p state] eq "animate" } {
		            $canvas delete $id
		            dict set p state after_animate
		            lset animateList $ipos 1 $p
		            dict set state visibility visible
		        }
		    }
		} elseif { $draw_time < [dict get $p begin] } {
		    dict set state visibility hidden
		    return
		} elseif { $idChild } {
		    if { [dict get $p end] ne "indefinite" && \
		        $draw_time > [dict get $p end] } {
		        if { [dict get $p fill] eq "remove" } {
		            dict set state visibility hidden
		            return
		        }
		    } else {
		        dict set state isanimated 1
		        dict set state visibility visible
		    }
		} else {
		    set t [expr {($draw_time-[dict get $p begin])/double([dict get $p dur])}]
		    if { $t > 1 } { set t 1 }
		    foreach "x y" [m::eval_cubic_bezier_full [dict get $p knots] \
		            [dict get $p pnts] $t] break
		    $self push_transform_svg "translate($x,$y)"
		    if { [dict get $p rotate] != 0 } {
		        if { [dict get $p rotate] eq "auto" || [dict get $p rotate] eq " auto-reverse" } {
		            set der [m::eval_deriv_cubic_bezier_full [dict get $p knots] \
		                    [dict get $p pnts] $t]
		            set radtodeg [::math::constants::give_constant radtodeg]
		            set angle [expr {$radtodeg*atan2([lindex $der 1],[lindex $der 0])}]
		            if { [dict get $p rotate] eq " auto-reverse" } {
		                set angle [expr {$angle+180}]
		            }
		        } else { set angle [dict get $p rotate] }
		        $self push_transform_svg "rotate($angle)"
		    }
		    if { [dict get $p end] ne "indefinite" && \
		        $draw_time > [dict get $p end] } {
		        if { [dict get $p fill] eq "remove" } {
		            dict set state visibility hidden
		            return
		        }
		        dict set state visibility visible
		    } else {
		        dict set state isanimated 1
		        dict set state visibility visible
		    }
		}
	    } 
	    if { ![dict exists $state visibility] }  {
		if { $draw_time > 0 } {
		    dict set state visibility hidden
		    return
		}
	    } elseif { [dict get $state visibility] eq "hidden" } {
		dict set state visibility hidden
		return
	    }
	}
	if { [dict exists $atts transform] } {
	    $self push_transform_svg [dict get $atts transform]
	}
	if { [dict exists $atts style] } {
	    set atts_in ""
	    foreach i [split [dict get $atts style] ";"] {
		foreach "att val" [split $i :] break
		dict set atts_in [string trim $att] [string trim $val]
	    }
	    $self _common_att_do $atts_in
	    dict for "n v" $atts_in { dict set state $n $v }
	}
#         if { [$node @marker-start ""] eq "url(#Triangle1)" } {
#             if { [$node @marker-end ""] eq "url(#Triangle2)" } {
#                 dict set state arrow both
#             } else {
#                 dict set state arrow first
#             }
#         } else {
#             if { [$node @marker-end ""] eq "url(#Triangle2)" } {
#                 dict set state arrow last
#             } else {
#                 dict set state arrow none
#             }
#         }
#         dict set state arrow none
	if { ![dict exists $state fill] } { dict set state fill black }
	if { ![dict exists $state stroke] } { dict set state stroke none }
	foreach i [list stroke x y x1 y1 x2 y2 font-family font-weight font-style \
		text-anchor baseline-shift display fill] {
	    if { [dict exists $atts $i] } {
		dict set state $i [string trim [dict get $atts $i]]
	    }
	}
	foreach i [list fill stroke] {
	    if { [dict get $state $i] eq "none" } {
		dict set state $i ""
	    } else {
		dict set state $i [convert_color [dict get $state $i]]
	    }
	}
	
	if { [dict exists $atts stroke-dasharray] } {
	    set dash ""
	    foreach len [regexp -inline -all {[\d.]+} [dict get $atts stroke-dasharray]] {
		set v1 [$self tr "0 $len"]
		set v0 [$self tr "0 0"]
		set valueT [expr {round([m::norm [m::sub $v1 $v0]])}]
		if { $valueT < 1 } { set valueT 1 }
		lappend dash $valueT
	    }
	    dict set state dash $dash
	} else {
	    dict set state dash ""
	}

	dict set state lwidth .1
	foreach i [list stroke-width font-size] {
	    if { [dict exists $atts $i] } {
		set value [_length [dict get $atts $i]]
#                 foreach "- value1" [$self tr_main "0 $value"] break
#                 foreach "- value0" [$self tr_main "0 0"] break
		set v1 [$self tr "0 $value"]
		set v0 [$self tr "0 0"]
		set valueT [m::norm [m::sub $v1 $v0]]
		if { $i eq "stroke-width" } { set valueT [expr {$valueT/1.25}] }
		if { $valueT < 0.1 } { set valueT .1 }
		if { $i eq "stroke-width" } { 
		    dict set state lwidth $valueT
		    dict set state stroke-width [dict get $atts $i]
		} else {
		    dict set state $i $valueT
		    #mylog::debug "_common_att_do: changing $i from [dict get $atts $i] to $valueT"
		}
	    }
	}
    }
    method _end_common_att { node } {
	$self pop_transform_to_mark
	set state ""
    }
    proc find_attribute_in_parent { node attribute } {
	if { $node eq "" } { return "" }
	if { [$node hasAttribute $attribute] } {
	    return [$node @$attribute]
	}
	return [find_attribute_in_parent [$node parentNode] $attribute]
    }
    proc points_are_close { p1 p2 { eps 4 } } {
	if { [m::norm [m::sub $p1 $p2]] < $eps } { return 1 }
	return 0
    }
    proc give_groups { node } {
	set ret ""
	foreach i [list 0 1 2] {
	    if { $i == 0 } { set i_p "" } else { set i_p $i }
	    set ret_i ""
	    foreach g [split [$node @ramdraw:groups$i_p ""] ,] {
		lappend ret_i $g
	    }
	    lappend ret $ret_i
	}
	return $ret
    }
    # draw SVG child nodes
    method do_child { node } {
	$self _common_att $node
	
	if { !$gid_output } {
	    foreach "att v" [list visibility hidden display none] {
		if { [dict exists $state $att] && [dict get $state $att] eq $v } {
		    $self _end_common_att $node
		    return
		}
	    }
	}
	set tags items
	set itemsList ""
	if { [dict exists $state id] } { lappend tags [dict get $state id] }
	if { [dict exists $state id_parent] } {
	    lappend tags [dict get $state id_parent]
	}
	lappend tags {*}[dict_getd $state more_tags ""]

	if { [dict exists $state isanimated] } { lappend tags animated }
	switch [$node nodeName] {
	    "line" {
		if { $image_gd ne "" } {
		    gd line $image_gd {*}[$self tr_coords "x1 y1 x2 y2"] \
		        -fill [gd_color $image_gd [dict get $state stroke] [dict get $state dash]] \
		        -width [dict get $state lwidth]

		} elseif { $gid_output } {
		    set p1 [join [$self tr_coords "x1 y1"] ","]
		    set p2 [join [$self tr_coords "x2 y2"] ","]
		    lassign [give_groups $node] gList g1List g2List
		    set layer [lindex $gList 0]
		    set cmd ""
		    lappend cmd *****TCL param_creator::create_stline -layer $layer \
		        -groupsList $gList -groups1List $g1List \
		        -groups2List $g2List -- $p1 $p2
		    append gid_buffer "$cmd\n"
		} elseif { $canvas ne "" } {
		    set coords [$self tr_coords "x1 y1 x2 y2"]
		    set item [$canvas create line $coords -fill [dict get $state stroke]\
		            -dash [dict get $state dash] \
		            -width [dict get $state lwidth] -tags $tags]
		    lappend itemsList $item
		} else {
		    PDFWriter::CreateLine {*}[$self tr_coords "x1 y1 x2 y2"] \
		        [dict get $state lwidth] [dict get $state stroke] \
		        [dict get $state dash]
		}
		set v1 [list [$node @x1] [$node @y1]]
		set v2 [list [$node @x2] [$node @y2]]
		set tangent [m::sub $v2 $v1]
		if { [$node @marker-start ""] ne "" } {
		    $self draw_marker [$node @marker-start] \
		        $v1 $tangent
		}
		if { [$node @marker-end ""] ne "" } {
		    $self draw_marker [$node @marker-end] \
		        $v2 $tangent
		}
	    }
	    "rect" {
		if { ![dict exists $state display] || [dict get $state display] ne "none" } {
		    set x1 [$node @x]
		    set y1 [$node @y]
		    set x2 [expr {$x1+[$node @width]}]
		    set y2 [expr {$y1+[$node @height]}]
		    if { $image_gd ne "" } {
		        eval [list gd rectangle $image_gd] [$self tr "$x1 $y1 $x2 $y2"] \
		            [list -fill [gd_color $image_gd [dict get $state fill]] \
		                -outline [gd_color $image_gd [dict get $state stroke] [dict get $state dash]] \
		                -width [dict get $state lwidth]]

		    } elseif { $gid_output } {
		        set x1 [$node @x]
		        set y1 [$node @y]
		        set x2 [expr {$x1+[$node @width]}]
		        set y2 [expr {$y1+[$node @height]}]
		        lassign [give_groups $node] gList g1List g2List
		        set layer [lindex $gList 0]
		        lassign [list 1 1] i1_old i2_old
		        foreach "i1 i2" [list 1 2 2 2 2 1 1 1] {
		            set p1 [join [$self tr "[set x$i1_old] [set y$i2_old]"] ","]
		            set p2 [join [$self tr "[set x$i1] [set y$i2]"] ","]
		            set cmd ""
		            lappend cmd *****TCL param_creator::create_stline -layer $layer \
		                -groupsList $gList -groups1List $g1List \
		                -groups2List $g2List -- $p1 $p2
		            append gid_buffer "$cmd\n"
		            lassign [list $i1 $i2] i1_old i2_old
		        }
		    } elseif { $canvas ne "" } {
		        set item [$canvas create rectangle [$self tr "$x1 $y1 $x2 $y2"] \
		                -fill [dict get $state fill] \
		                -outline [dict get $state stroke] \
		                -dash [dict get $state dash] \
		                -width [dict get $state lwidth] -tags $tags]
		        lappend itemsList $item
		    } else {
		        PDFWriter::CreatePolygon [$self tr "$x1 $y1 $x1 $y2 $x2 $y2 $x2 $y1"] \
		            [dict get $state fill] 1
		        if { [dict get $state stroke] ne "" } {
		            PDFWriter::CreatePolygon [$self tr "$x1 $y1 $x1 $y2 $x2 $y2 $x2 $y1"] \
		                [dict get $state stroke] 0 \
		                [dict get $state lwidth] [dict get $state dash]
		        }
		    }
		}
	    }
	    "polygon" - "polyline" {
		set points [$node @points]
		regsub -all {,} $points { }  points
		if { $image_gd ne "" } {
		        eval [list gd polygon $image_gd] [$self tr_coords $points] \
		        [list -fill [gd_color $image_gd [dict get $state fill]] \
		            -outline [gd_color $image_gd [dict get $state stroke] [dict get $state dash]] \
		                -width [dict get $state lwidth]]

		} elseif { $gid_output } {
		    # nothing
		} elseif { $canvas ne "" } {
		    set item [$canvas create polygon [$self tr $points] \
		            -fill [dict get $state fill] \
		            -outline [dict get $state stroke] \
		            -dash [dict get $state dash] \
		            -width [dict get $state lwidth] -tags $tags]
		    lappend itemsList $item
		} else {
		    PDFWriter::CreatePolygon [$self tr $points] \
		        [dict get $state fill] 1
		    if { [dict get $state stroke] ne "" } {
		        PDFWriter::CreatePolygon [$self tr $points] \
		            [dict get $state stroke] 0 \
		            [dict get $state lwidth] [dict get $state dash]
		    }
		}
	    }
	    "circle" - "ellipse" {
		if { [$node nodeName] eq "circle" } {
		    set rx [$node @r]
		    set ry [$node @r]
		} else {
		    set rx [$node @rx]
		    set ry [$node @ry]
		}
		set c [list [$node @cx] [$node @cy]]
		set pt1 [m::add $c "-$rx -$ry"]
		set pt2 [m::add $c "$rx $ry"]
		if { $image_gd ne "" } {
		        eval [list gd oval $image_gd] [$self tr [concat $pt1 $pt2]] \
		        [list -fill [gd_color $image_gd [dict get $state fill]] \
		            -outline [gd_color $image_gd [dict get $state stroke] [dict get $state dash]] \
		                -width [dict get $state lwidth]]

		} elseif { $gid_output } {
		    # nothing
		} elseif { $canvas ne "" } {
		    set item [$canvas create oval [$self tr [concat $pt1 $pt2]] \
		            -fill [dict get $state fill] \
		            -outline [dict get $state stroke] \
		            -dash [dict get $state dash] \
		            -width [dict get $state lwidth] -tags $tags]
		    lappend itemsList $item
		} else {
		    set cT [$self tr $c]
		    set rxT [lindex [m::sub [$self tr "$rx 0"] [$self tr "0 0"]] 0]
		    set ryT [lindex [m::sub [$self tr "0 $ry"] [$self tr "0 0"]] 1]
		    set pi [math::constants::give_constant pi]

		    if 1 {
		        # preferring one alternative or the other
		        if { [dict get $state fill] ne "" } {
		            PDFWriter::CreateEllipse [lindex $cT 0] [lindex $cT 1] \
		                $rxT $ryT 0 360 0 [dict get $state fill] 0 1
		        }
		        if { [dict get $state stroke] ne "" } {
		            PDFWriter::CreateEllipse [lindex $cT 0] [lindex $cT 1] \
		                $rxT $ryT 0 360 [dict get $state lwidth] \
		                [dict get $state stroke] 0 0
		        }
		    } else {
		        PDFWriter::PDF::CreateCircle [lindex $cT 0] [lindex $cT 1] $rxT \
		            0 [dict get $state fill] 0 1
		        if { [dict get $state stroke] ne "" } {
		            PDFWriter::PDF::CreateCircle [lindex $cT 0] [lindex $cT 1] \
		                $rxT [dict get $state lwidth] \
		                [dict get $state stroke] [dict get $state dash] 0
		        }
		    }
		}
	    }
	    "path" {
		regsub -all {,} [$node @d ""] { } d
		if { $d eq "" } {
		    error "error processing path"
		}
		if { $image_gd ne "" } {
		    set bezier3_to_lines 1
		} else {
		    # the case where PDF goes directly to print needs to be resolved
		    set bezier3_to_lines 0
		}
		if { $image_gd ne "" || $canvas ne "" } {
		    set quadratic_only_control_pnts 1
		} else {
		    set quadratic_only_control_pnts 0 
		}
		set lines [parse_path -bezier3_to_lines $bezier3_to_lines \
		        -quadratic_only_control_pnts $quadratic_only_control_pnts $d]
		lassign "" pt1 tg1 pts tg2
		set err 0
		foreach i $lines {
		    foreach "type pnts props" $i break
		    switch $type {
		        line - bezier2 - bezier3 {
		            if { [dict get $state fill] ne "" } {
		                if { $image_gd ne "" } {
		                    if { $type ni "bezier2 bezier3" } {
		                        eval [list gd polygon $image_gd] [$self tr $pnts] \
		                            [list -fill [gd_color $image_gd [dict get $state fill]] \
		                                -outline [gd_color $image_gd [dict get $state stroke] [dict get $state dash]] \
		                                -width [dict get $state lwidth]]
		                    } else {
		                        eval [list gd polygon $image_gd] [$self tr $pnts] \
		                            [list -fill [gd_color $image_gd [dict get $state fill]] \
		                                -outline [gd_color $image_gd [dict get $state stroke] [dict get $state dash]] \
		                                -width [dict get $state lwidth] \
		                                -smooth true]

		                    }                                
		                } elseif { $gid_output } {
		                    # nothing
		                } elseif { $canvas ne "" } {
		                    set item [$canvas create polygon [$self tr $pnts] \
		                            -fill [dict get $state fill] \
		                            -outline [dict get $state stroke] \
		                            -dash [dict get $state dash] \
		                            -width [dict get $state lwidth] -tags $tags]
		                    lappend itemsList $item
		                    if { $type in "bezier2 bezier3" } {
		                        if { $type eq "bezier3" } {
		                            set err_smooth [catch {
		                                    $canvas itemconfigure $item -smooth raw
		                                }]
		                        }
		                        if { $type eq "bezier2" || $err_smooth } {
		                            $canvas itemconfigure $item -smooth 1
		                        }          
		                    }
		                } elseif { $type eq "bezier3" } {
		                    PDFWriter::CreateBezierD3 [$self tr $pnts] \
		                        1 [dict get $state fill] 0 1
		                    if { [dict get $state stroke] ne "" } {
		                        PDFWriter::CreateBezierD3 [$self tr $pnts] \
		                            [dict get $state lwidth] \
		                            [dict get $state stroke] 0 0
		                    }
		                } elseif { $type eq "bezier2" } {
		                    PDFWriter::CreateBezierD2 [$self tr $pnts] \
		                        1 [dict get $state fill] 0 1
		                    if { [dict get $state stroke] ne "" } {
		                        PDFWriter::CreateBezierD2 [$self tr $pnts] \
		                            [dict get $state lwidth] \
		                            [dict get $state stroke] 0 0
		                    }
		                } else {
		                    PDFWriter::CreatePolygon [$self tr $pnts] \
		                        [dict get $state fill] 1
		                    if { [dict get $state stroke] ne "" } {
		                        PDFWriter::CreatePolygon [$self tr $pnts] \
		                            [dict get $state stroke] 0 \
		                            [dict get $state lwidth]
		                    }
		                }
		            } else {
		                if { $image_gd ne "" } {
		                    if { [dict get $state stroke] ne "" } {
		                        set lwidth [expr {round([dict get $state lwidth])}]
		                        if { $type ni "bezier2 bezier3" } {
		                            eval [list gd line $image_gd] [$self tr $pnts] \
		                                [list -fill [gd_color $image_gd \
		                                        [dict get $state stroke] [dict get $state dash]] \
		                                    -width $lwidth]
		                        } else {
		                            eval [list gd line $image_gd] [$self tr $pnts] \
		                                [list -fill [gd_color $image_gd \
		                                        [dict get $state stroke] [dict get $state dash]] \
		                                    -width $lwidth \
		                                    -smooth true]
		                            
		                        }
		                    }                         
		                } elseif { $gid_output } {
		                    # nothing
		                } elseif { $canvas ne "" } {
		                    set item [$canvas create line [$self tr $pnts] \
		                            -fill [dict get $state stroke] \
		                            -dash [dict get $state dash] \
		                            -width [dict get $state lwidth] -tags $tags]
		                    lappend itemsList $item
		                    if { $type in "bezier2 bezier3" } {
		                        if { $type eq "bezier3" } {
		                            set err_smooth [catch {
		                                    $canvas itemconfigure $item -smooth raw
		                                }]
		                        }
		                        if { $type eq "bezier2" || $err_smooth } {
		                            $canvas itemconfigure $item -smooth 1
		                        }
		                    }
		                } elseif { $type eq "bezier3" } {
		                   PDFWriter::CreateBezierD3 [$self tr $pnts] \
		                        [dict get $state lwidth] \
		                        [dict get $state stroke] 0 0
		                } elseif { $type eq "bezier2" } {
		                   PDFWriter::CreateBezierD2 [$self tr $pnts] \
		                        [dict get $state lwidth] \
		                        [dict get $state stroke] 0 0
		                } else {
		                    PDFWriter::CreateLineC [$self tr $pnts] \
		                        [dict get $state lwidth] \
		                        [dict get $state stroke]
		                }
		            }
		            if { $pt1 eq "" } {
		                set pt1 [lrange $pnts 0 1]
		                set v2 [lrange $pnts 2 3]
		                set tg1 [m::sub $v2 $pt1]
		            }
		            set v1 [lrange $pnts end-3 end-2]
		            set pt2 [lrange $pnts end-1 end]
		            set tg2 [m::sub $pt2 $v1]
		        }
		        arc {
		            foreach i "rx ry x-axis-rotation large-arc-flag sweep-flag" {
		                set $i [dict get $props $i]
		            }
		            set c1 [lrange $pnts 0 1]
		            set c2 [lrange $pnts 2 3]
		            set c1T [$self tr $c1]
		            set c2T [$self tr $c2]
		            set rxT [m::norm [$self tr_vec "$rx 0"]]
		            set ryT [m::norm [$self tr_vec "0 $ry"]]
		            
		            set degtorad [::math::constants::give_constant degtorad]
		            set xaxis_rad [expr {$degtorad*${x-axis-rotation}}]
		            set v1 [$self tr_vec "[expr {cos($xaxis_rad)}] [expr {sin($xaxis_rad)}]"]
		            set xaxis_rad [m::vec_angle "1 0" $v1]

		            if { [$self transform_sweep_flag] } {
		                set sweep-flag [expr {(${sweep-flag})?0:1}]
		            }
		            if { $canvas eq "" && $image_gd eq "" } {
		                set invert_orient 1
		                #set sweep-flag [expr {(${sweep-flag})?0:1}]
		            } else {
		                set invert_orient 0
		            }
		            set err [catch { $self _calc_ellipse_center_angles \
		                        $c1T $c2T $rxT $ryT $xaxis_rad \
		                        ${large-arc-flag} ${sweep-flag} \
		                        $invert_orient } ret]
		            if { !$err } {
		                lassign $ret coords center start extent tg1LT tg2LT
		                set tg1L [$self tr_vec_inv $tg1LT]
		                set tg2L [$self tr_vec_inv $tg2LT]
		            }
		            if { $image_gd ne "" } {
		                if { $err } {
		                    eval [list gd line $image_gd] [concat $c1T $c2T] \
		                        [list -fill [gd_color $image_gd red] \
		                            -width 3]
		                } else {
		                    if { $extent > 0 } {
		                        set start [expr {-1*($start+$extent)}]
		                    } else {
		                        set start [expr {-1*$start}]
		                        set extent [expr {-1*$extent}]
		                    }
		                    if { [dict get $state stroke] ne "" } {
		                        eval [list gd arc $image_gd] $coords -start $start \
		                            -extent $extent -style arc \
		                            [list -outline [gd_color $image_gd \
		                                    [dict get $state stroke] [dict get $state dash]] \
		                            -width [dict get $state lwidth]]
		                    }                                
		                }                                
		            } elseif { $gid_output } {
		                set p1 [join $c1T ","]
		                set p2 [join $c2T ","]
		                
		                set angle [expr {$degtorad*($start+0.5*$extent)}]
		                set pC [join [m::add $center [list [expr {$rxT*cos($angle)}] \
		                                [expr {-1*$ryT*sin($angle)}]]] ","]
		                lassign [give_groups $node] gList g1List g2List
		                set layer [lindex $gList 0]
		                set cmd ""
		                lappend cmd *****TCL param_creator::create_arcline -layer $layer \
		                    -groupsList $gList -groups1List $g1List \
		                    -groups2List $g2List -- $p1 $pC $p2
		                append gid_buffer "$cmd\n"
		            } elseif { $canvas ne "" } {
		                if { $err } {
		                    set item [$canvas create line [concat $c1T $c2T] \
		                            -fill red -width 3 -tags $tags]
		                    lappend itemsList $item
		                } else {
		                    if 0 {
		                        $canvas delete temp
		                        for { set a 0 } { $a <= 2*3.1416 } { set a [expr {$a+.06}] } {
		                            set a_m [expr {-1*$a}]
		                            lappend coords {*}[m::ellipseF_val $center $rxT $ryT $a_m \
		                                    $xaxis_rad]
		                        }
		                        $canvas create line $coords -fill green -tags temp
		                        $canvas create oval [expr {[lindex $c1T 0]-2}] \
		                            [expr {[lindex $c1T 1]-2}] [expr {[lindex $c1T 0]+2}] \
		                            [expr {[lindex $c1T 1]+2}] -fill red -tags temp
		                        $canvas create oval [expr {[lindex $c2T 0]-2}] \
		                            [expr {[lindex $c2T 1]-2}] [expr {[lindex $c2T 0]+2}] \
		                            [expr {[lindex $c2T 1]+2}] -fill red -tags temp
		                        $canvas create oval [expr {[lindex $center 0]-2}] \
		                            [expr {[lindex $center 1]-2}] [expr {[lindex $center 0]+2}] \
		                            [expr {[lindex $center 1]+2}] -fill red -tags temp
		                        
		                        $canvas create line [lindex $center 0] [lindex $center 1] \
		                            [expr {[lindex $center 0]+$rxT}] [lindex $center 1] \
		                            -fill orange -tags temp
		                        $canvas create line [lindex $center 0] [lindex $center 1] \
		                            [lindex $center 0] [expr {[lindex $center 1]+$ryT}] \
		                            -fill orange -tags temp
		                    }
		                    if { abs($xaxis_rad) > 0.03 } {
		                        set start_rad [expr {$degtorad*$start}]
		                        set extent_rad [expr {$degtorad*$extent}]
		                        set delta [expr {$extent_rad/20}]
		                        set end [expr {$start_rad+$extent_rad+0.5*$delta}]
		                        set coords ""
		                        for { set a $start_rad } { $a < $end } { set a [expr {$a+$delta}] } {
		                            set a_m [expr {-1*$a}]
		                            lappend coords {*}[m::ellipseF_val $center $rxT $ryT $a_m \
		                                    $xaxis_rad]
		                        }
		                        set item [$canvas create line $coords \
		                                -fill [dict get $state stroke]\
		                                -dash [dict get $state dash] \
		                                -width [dict get $state lwidth] -tags $tags]
		                    } else {
		                        set item [$canvas create arc $coords -start $start \
		                                -extent $extent \
		                                -style arc -outline [dict get $state stroke] \
		                                -dash [dict get $state dash] \
		                                -width [dict get $state lwidth] -tags $tags]
		                    }
		                    lappend itemsList $item
		                    if 0 {
		                        foreach "cx cy" [$self tr $center] break
		                        $canvas create oval [expr {$cx-2}] [expr {$cy-2}] \
		                            [expr {$cx+2}] [expr {$cy+2}] -fill blue \
		                            -tags $tags
		                    }
		                }
		            } else {
		                lassign $center x y
		                set endangle [expr {$start+$extent}]
#                                 foreach "value1x value1y" [$self tr "$rx $ry"] break
#                                 foreach "value0x value0y" [$self tr "0 0"] break
#                                 set rx [expr {abs($value1x-$value0x)}]
#                                 set ry [expr {abs($value1y-$value0y)}]
		                
		                set rxT [expr {abs(.5*([lindex $coords 2]-[lindex $coords 0]))}]
		                set ryT [expr {abs(.5*([lindex $coords 3]-[lindex $coords 1]))}]
		                
		                PDFWriter::CreateEllipse $x $y $rxT $ryT $start $endangle \
		                    [dict get $state lwidth] \
		                    [dict get $state stroke] \
		                    [dict get $state dash]
		            }
		            if { !$err } {
		                if { $pt1 eq "" } {
		                    set pt1 $c1
		                    set tg1 $tg1L
		                }
		                set pt2 $c2
		                set tg2 $tg2L
		            }
		        }
		    }
		}
		if { !$err } {
		    if { [$node @marker-start ""] ne "" } {
		        $self draw_marker [$node @marker-start] \
		            $pt1 $tg1
		    }
		    if { [$node @marker-end ""] ne "" } {
		        $self draw_marker [$node @marker-end] \
		            $pt2 $tg2
		    }
		}
	    }
	    "g" {
		if { !$gid_output } {
		    if { ![dict exists $state display] || [dict get $state display] ne "none" } {
		        dict set state id_parent [dict_getd $state id ""]
		        set save_state $state
		        foreach i [$node childNodes] {
		            $self do_child $i
		            set state $save_state
		        }
		    }
		}
	    }
	    "text" {
		if { ![dict exists $state font-family] } {
		    dict set state font-family ""
		}
		set font [list -family [dict get $state font-family]]
		set size [expr {round([_length [dict get $state font-size]])}]
		if { $size < 1 } { set size 1 }
		lappend font -size $size
		if { [dict exists $state font-weight] } {
		    lappend font -weight [dict get $state font-weight]
		}
		if { [dict exists $state font-style] } {
		    set slant [dict get $state font-style]
		    if { $slant eq "normal" } { set slant roman }
		    lappend font -slant $slant
		}
		lassign [$self tr_coords "x y"] x y
		
		set anchor "n"
		set f $font
		if { $canvas ne "" } {
		    dict set f -size [expr {-1*[dict get $font -size]}]
		} else {
		    dict set f -size [expr {1*[dict get $font -size]}]
		}
		set bs [dict_getd $state baseline-shift ""]
		set y [expr {$y-[font metrics $f -ascent]}]
		switch -regexp -- $bs {
		    {^[-+\d.]+%$} {
		        set fac [expr {[scan $bs %d]/100.0}]
		        set y [expr {$y-$fac*[font metrics $f -linespace]}]
		    }
		    {^[-+\d.]+$} {
		        set fac [scan $bs %d]
		        set y [expr {$y-$fac}]
		    }                    
		}
		
#                 if { [dict_getd $state baseline-shift ""] ne "" } {
#                     # nothing
#                 } else {
#                     append anchor n
#                 }
		if { [dict exists $state text-anchor] } {
		    switch [dict get $state text-anchor] {
		        start { append anchor w }
		        middle { append anchor "" }
		        end { append anchor e }
		    }
		} else { append anchor w }
		if { $anchor eq "" } { set anchor center }
		set txt [string trim [$node selectNodes string(.)]]
		#mylog::debug "text: $txt size=[expr {round([_length [dict get $state font-size]])}]"
		if { $image_gd ne "" } {
		    if { $::tcl_platform(platform) eq "windows" } {
		        set f [list [dict_getd $font -family Arial] [dict get $font -size]]
		    } else {
		        set f [list FreeSans [dict get $font -size]]
		    }
		    if { [dict_getd $font -weight ""]  eq "bold" } {
		        lappend f bold
		    }
		    if { [dict_getd $font -slant ""]  eq "italic" } {
		        lappend f italic
		    }
		    gd text $image_gd $x $y \
		        -fill [gd_color $image_gd [dict get $state fill]] \
		        -text $txt -font $f -anchor $anchor
		} elseif { $gid_output } {
		    # nothing
		} elseif { $canvas ne "" } {
		    dict set font -size [expr {-1*[dict get $font -size]}]
		    set item [$canvas create text $x $y -text $txt \
		            -font $font -fill [dict get $state fill] -anchor $anchor -tags $tags]
		    lappend itemsList $item
		} else {
		    dict set font -size [expr {1*[dict get $font -size]}]
		    foreach "f s" [PDFWriter::TclfontToPDF $font 9] break
		    #set size [expr {[font actual $font -size]*1.0/$s}]
		    #set size [expr {[font actual $font -size]*1.0}]
		    set size [expr {abs([font actual $font -size]*1.0/[tk scaling])}]
		    lassign [$self tr_coords "x y"] x y
		    set y [expr {$y+[PDFWriter::Ascender $f $size]*[PDFWriter::LineSpace $f $size]}]
		    switch -regexp -- $bs {
		        {^[-+\d.]+%$} {
		            set fac [expr {[scan $bs %d]/100.0}]
		            set y [expr {$y+$fac*[PDFWriter::LineSpace $f $size]}]
		        }
		        {^[-+\d.]+$} {
		            set fac [scan $bs %d]
		            set y [expr {$y+$fac}]
		        }                    
		    }
		    if [drawformula::is_complex_formula $txt] {
		        drawformula::Init pdf "" $f "" [dict get $state fill]
		        #drawformula::drawformula $size $txt 1 $x $y
		        drawformula::draw_anchor $size $txt $x $y $anchor
		    } else {
		        PDFWriter::WriteText $txt $x $y $f $size $anchor [dict get $state fill]
		    }
		}
	    }
	    "use" {
		if { !$gid_output } {
		    $self push_transform 1.0 0.0 0.0 1.0 [$node @x 0] [$node @y 0]
		    set ns { svg http://www.w3.org/2000/svg }
		    regexp {#(.*)} [$node @xlink:href] {} id
		    set xp ".//svg:defs/svg:symbol\[@id='[xml $id]'\]|"
		    append xp ".//svg:defs/svg:g\[@id='[xml $id]'\]"
		    set node_in [$svgNode selectNodes -namespaces $ns $xp]
		    dict set state id_parent [dict_getd $state id ""]
		    $self _common_att $node_in
		    set save_state $state
		    foreach child [$node_in childNodes] {
		        $self do_child $child
		        set state $save_state
		    }
		    $self _end_common_att $node_in
		    $self pop_transform
		}
	    }
	    "image" {
		# WARNING: Currently, it is displayed as if preserveAspectRatio="none"
		if { ![dict exists $state display] || [dict get $state display] ne "none" } {
		    lassign [$self give_image $node] img isnew
		    lassign [$self tr_scalexy] scx scy
		    set nwidth [expr {$scx*[$node @width]}]
		    set nheight [expr {$scy*[$node @height]}]
		    if { [$self is_pdf] } {
		        set scalex [expr {$nwidth/double([image width $img])}]
		        set scaley [expr {$nheight/double([image height $img])}]
		    } elseif { [image width $img] != $nwidth || [image height $img] != $nheight } {
		        set newimg [image create photo -width [expr {round($nwidth)}] \
		                -height [expr {round($nheight)}]]
		        package require compass_utils::img
		        cu::img::zoom $newimg $img Lanczos3
		        if { $isnew } {
		            image delete $img
		        }
		        set img $newimg
		    }
		    if { $image_gd ne "" } {
		        if { $::tcl_platform(platform) eq "windows" } {
		            set file [file attributes $image_gd_file -shortname]
		        } else {
		            set file $image_gd_file
		        }
		        package require img::png
		        if { 0 } {
		            gd close $image_gd -file $file
		            set img_full [image create photo -file $file]
		            set coords [$self tr_coords "x y"]
		            set x [expr {round([lindex $coords 0])}]
		            set y [expr {round([lindex $coords 1])}]
		            $img_full copy $img -to $x $y
		            $img_full write $image_gd_file -format png
		            set image_gd [gd open -file $file]
		        } else {
		            set file [file join [cu::file::tempdir] image0.png]
		            $img write $file -format png
		            if { $::tcl_platform(platform) eq "windows" } {
		                set file [file attributes $file -shortname]
		            }
		            set gd_img [gd open -file $file -filetype png]
#                             set gd_img [gd open -width $nwidth -height $nheight -file $file \
#                                     -filetype jpeg]
		            lassign [$self tr_coords "x y"] x1 y1
		            set x2 [expr {$x1+$nwidth}]
		            set y2 [expr {$y1+$nheight}]
		            gd rectangle $image_gd $x1 $y1 $x2 $y2 -fill %$gd_img
		            gd close $gd_img
		            file delete $file
		        }
		    } elseif { $gid_output } {
		        # nothing
		    } elseif { $canvas ne "" } {
		        set item [$canvas create image [$self tr_coords "x y"] -image $img \
		                -anchor nw -tags $tags]
		        lappend itemsList $item
		    } else {
		        lassign [$self tr_coords "x y"] x y
		        PDFWriter::InsertImg $img $x $y nw $scalex,$scaley
		        #PDFWriter::InsertImageNoDisk $img $x $y nw 1
		    }
		}
	    }
	    "a" {
		set tag a_[$node @xlink:href]
		dict lappend state more_tags a_[$node @xlink:href]
		$self manage_link_enter_leave set $tag [$node @xlink:href]
		foreach i [$node childNodes] {
		    $self do_child $i
		}
	    }
	}
#         if { $canvas ne "" && [info exists item] } {
#             tooltip $canvas -item $item [$node asXML]
#         }
	if { [dict exists $state desc] && $canvas ne "" } {
	    set txt [dict get $state desc]
	    # space if here to avoid an error when contents begin with "-"
	    if { [string match -* $txt] } { set txt " $txt" }
	    foreach item $itemsList {
		tooltip $canvas -item $item $txt
	    }
	}
	$self _end_common_att $node
    }
    variable manage_link_enter_leave_cursor ""
    variable manage_link_enter_leave_after ""
    variable manage_link_enter_leave_w ""
    method manage_link_enter_leave { enter_leave args } {
	switch $enter_leave {
	    set {
		if { $canvas eq "" } { return }
		lassign $args tagname url
		$canvas bind $tagname <Enter> [mymethod manage_link_enter_leave \
		        enter $tagname $url]
		$canvas bind $tagname <Leave> [mymethod manage_link_enter_leave \
		        leave $tagname]
		$canvas bind $tagname <ButtonRelease-1> [mymethod manage_link_enter_leave \
		        execute $url]
	    }
	    enter {
		lassign $args tagname txt
		set manage_link_enter_leave_cursor [$canvas cget -cursor]
		if { $manage_link_enter_leave_cursor eq "watch" } {
		    set manage_link_enter_leave_cursor ""
		    return
		}
		$canvas configure -cursor hand2
		set manage_link_enter_leave_after [after 600 [list catch [mymethod \
		            manage_link_enter_leave popup $txt]]]
	    }
	    leave {
		$canvas configure -cursor $manage_link_enter_leave_cursor
		set manage_link_enter_leave_cursor ""
		after cancel $manage_link_enter_leave_after
		set manage_link_enter_leave_after ""
		destroy $manage_link_enter_leave_w
		set manage_link_enter_leave_w ""
	    }
	    popup {
		set manage_link_enter_leave_after ""
		destroy $canvas.popup
		set w [toplevel $canvas.popup]
		set manage_link_enter_leave_w $w
		wm overrideredirect $w 1

		pack [label $w.l -text [lindex $args 0] -relief solid -bd 1 -bg white \
		        -anchor w -justify left] \
		    -padx 1 -pady 1
		update idletasks

		set x [winfo pointerx $canvas]
		set y [winfo pointery $canvas]
		if { $x > [winfo screenwidth $canvas]-[winfo reqwidth $w] } {
		    set x [expr {[winfo screenwidth $canvas]-[winfo reqwidth $w]}]
		}
		if { $y > [winfo screenheight $canvas]-[winfo reqheight $w]-20 } {
		    set y [expr {$y-[winfo reqheight $w]-5}]
		} else { set y [expr {$y+20}] }
		wm geometry $w +$x+$y
	    }
	    execute {
		lassign $args url
		if { $options(-resolve_links_handler) eq "" } {
		    snit_messageBox -message [_ "Trying to go to link '%s'" $url]
		} else {
		    uplevel #0 $options(-resolve_links_handler) [list $url]
		}
	    }
	}
    }
    method give_image { node } {
	set href [$node @xlink:href]
	if { [lsearch -exact [image names] $href] != -1 } {
	    return [list $href 0]
	}
	if { [regexp {^\s*data:([^;]+);base64,(.*)} $href {} type data] } {
	    switch -- $type {
		image/png { set format png }
		image/gif { set format gif }
		image/jpeg { set format jpeg }
	    }
	    if { [info exists format] } {
		set img [image create photo -format $format -data [cu::base64 decode $data]]
		return [list $img 1]
	    }
	}
	set file [file join $options(-basedir) $href]
	if { ![file readable $file] } {
	    set dir [find_attribute_in_parent $node xml:base]
	    set file [file join $dir $href]
	}
	set fin [open $file r]
	fconfigure $fin -translation binary
	set img [image create photo -data [read $fin]]
	close $fin
	return [list $img 1]
    }
    proc parse_path { args } {
	
	set optional {
	    { -bezier3_to_lines boolean 0 }
	    { -quadratic_only_control_pnts boolean 1 }
	    { -quadratic_to_cubic boolean 0 }
	}
	set compulsory "d"
	parse_args $optional $compulsory $args

	set d [regexp -inline -all {[a-zA-Z]|[-+]?[0-9.e+-]+} $d]
	lassign [list "" line "" ""] pnt0 linetype pnts lines
	for { set i 0 } { $i < [llength $d] } { incr i } {
	    set type [lindex $d $i]
	    switch -- $type {
		M - m {
		    incr i
		    if { $type eq "m" && [llength $pnts] } {
		        set pntL [lrange $pnts end-1 end]
		        set pnt [m::add $pntL [lrange $d $i [expr {$i+1}]]]
		    } else {
		        set pnt [lrange $d $i [expr {$i+1}]]
		    }
		    if { [llength $pnts] > 2 } {
		        lappend lines [list $linetype $pnts ""]
		    }
		    set pnts ""
		    set linetype line
		    lappend pnts {*}$pnt
		    set pnt0 $pnt
		    incr i
		}
		L - l {
		    if { ![llength $pnts] } {
		        error "error: a path without a moveto"
		    }
		    while { $i < [llength $d]-1 } {
		        incr i
		        if { $type eq "l" } {
		            set pntL [lrange $pnts end-1 end]
		            set pnt [m::add $pntL [lrange $d $i [expr {$i+1}]]]
		        } else {
		            set pnt [lrange $d $i [expr {$i+1}]]
		        }
		        if { $linetype eq "line" } {
		            lappend pnts {*}$pnt
		        } elseif { $linetype eq "bezier3" } {
		            lappend pnts {*}[lrange $pnts end-1 end]
		            lappend pnts {*}$pnt
		            lappend pnts {*}$pnt
		        } elseif { [llength $pnts] > 2 } {
		            lappend lines [list $linetype $pnts ""]
		            set pnts [lrange $pnts end-1 end]
		            lappend pnts {*}$pnt
		            set linetype line
		        }
		        incr i
		        if { [regexp {^[a-zA-Z]$} [lindex $d [expr {$i+1}]]] } { break }
		    }
		}
		H - h {
		    if { ![llength $pnts] } {
		        error "error: a path without a moveto"
		    }
		    while { $i < [llength $d]-1 } {
		        incr i
		        set pntL [lrange $pnts end-1 end]
		        if { $type eq "h" } {
		            set pnt [m::add $pntL [list [lindex $d $i] 0]]
		        } else {
		            set pnt [list [lindex $d $i] [lindex $pntL 1]]
		        }
		        if { $linetype eq "line" } {
		            lappend pnts {*}$pnt
		        } elseif { $linetype eq "bezier3" } {
		            lappend pnts {*}[lrange $pnts end-1 end]
		            lappend pnts {*}$pnt
		            lappend pnts {*}$pnt
		        } elseif { [llength $pnts] > 2 } {
		            lappend lines [list $linetype $pnts ""]
		            set pnts [lrange $pnts end-1 end]
		            lappend pnts {*}$pnt
		            set linetype line
		        }
		        if { [regexp {^[a-zA-Z]$} [lindex $d [expr {$i+1}]]] } { break }
		    }
		}
		V - v {
		    if { ![llength $pnts] } {
		        error "error: a path without a moveto"
		    }
		    while { $i < [llength $d]-1 } {
		        incr i
		        set pntL [lrange $pnts end-1 end]
		        if { $type eq "v" } {
		            set pnt [m::add $pntL [list 0 [lindex $d $i]]]
		        } else {
		            set pnt [list [lindex $pntL 1] [lindex $d $i]]
		        }
		        if { $linetype eq "line" } {
		            lappend pnts {*}$pnt
		        } elseif { $linetype eq "bezier3" } {
		            lappend pnts {*}[lrange $pnts end-1 end]
		            lappend pnts {*}$pnt
		            lappend pnts {*}$pnt
		        } elseif { [llength $pnts] > 2 } {
		            lappend lines [list $linetype $pnts ""]
		            set pnts [lrange $pnts end-1 end]
		            lappend pnts {*}$pnt
		            set linetype line
		        }
		        if { [regexp {^[a-zA-Z]$} [lindex $d [expr {$i+1}]]] } { break }
		    }
		}
		A - a {
		    if { ![llength $pnts] } {
		        error "error: a path without a moveto"
		    } elseif { [llength $pnts] > 2 } {
		        lappend lines [list $linetype $pnts ""]
		        set pnts [lrange $pnts end-1 end]
		    }
		    while { $i < [llength $d]-1 } {
		        incr i
		        set props ""
		        foreach n "rx ry x-axis-rotation large-arc-flag sweep-flag" \
		            v [lrange $d $i [expr {$i+4}]] {
		            dict set props $n $v
		        }
		        incr i 5
		        if { $type eq "a" } {
		            set pntL [lrange $pnts end-1 end]
		            set pnt [m::add $pntL [lrange $d $i [expr {$i+1}]]]
		        } else {
		            set pnt [lrange $d $i [expr {$i+1}]]
		        }
		        eval lappend pnts $pnt
		        lappend lines [list arc $pnts $props]
		        set pnts [lrange $pnts end-1 end]
		        set linetype line
		        incr i
		        if { [regexp {^[a-zA-Z]$} [lindex $d [expr {$i+1}]]] } { break }
		    }
		}
		c - C {
		    if { ![llength $pnts] } {
		        error "error: a path without a moveto"
		    }
		    if { $linetype eq "line" } {
		        set pnts_old $pnts
		        set pnts ""
		        for { set j 2 } { $j < [llength $pnts_old] } { incr j 2 } { 
		            lappend pnts {*}[lrange $pnts_old $j-2 $j-1]
		            lappend pnts {*}[lrange $pnts_old $j-2 $j-1]
		            lappend pnts {*}[lrange $pnts_old $j $j+1]
		            lappend pnts {*}[lrange $pnts_old $j $j+1]
		        }
		    } elseif { $linetype ne "bezier3" && [llength $pnts] > 2 }  {
		        lappend lines [list $linetype $pnts ""]
		        set pnts [lrange $pnts end-1 end]
		    }
		    set linetype bezier3

		    if { $pnts eq "" } { set pnts $pnts_old }
		    while { $i < [llength $d]-1 } {
		        set pntL [lrange $pnts end-1 end]
		        for { set j 0 } { $j < 3 } { incr j } {
		            incr i
		            if { $type eq "c" } {
		                set pnt [m::add $pntL [lrange $d $i [expr {$i+1}]]]
		            } else {
		                set pnt [lrange $d $i [expr {$i+1}]]
		            }
		            lappend pnts {*}$pnt
		            incr i
		        }
		        if { [regexp {^[a-zA-Z]$} [lindex $d [expr {$i+1}]]] } { break }
		    }
		}
		q - Q - t - T {
		    if { $quadratic_to_cubic } { set quadratic_only_control_pnts 0 }

		    if { ![llength $pnts] } {
		        error "error: a path without a moveto"
		    }
		    if { !$quadratic_to_cubic } {
		        set newlinetype bezier2
		    } else {
		        set newlinetype bezier3
		    }
		    if { [llength $pnts] <= 2 } {
		        # nothing
		    } elseif { $linetype eq "line" } {
		        set pnts_old $pnts
		        set pnts ""
		        for { set j 0 } { $j < [llength $pnts_old] } { incr j 2 } {
		            if { j > 0 || $quadratic_to_cubic } {
		                lappend pnts {*}[lrange $pnts_old $j $j+1]
		            }
		            lappend pnts {*}[lrange $pnts_old $j $j+1]
		            if { $quadratic_to_cubic && $j > 0 && $j < [llength $pnts_old]-2 } {
		                lappend pnts {*}[lrange $pnts_old $j $j+1]
		            }
		        }
		    } elseif { $linetype ne $newlinetype }  {
		        lappend lines [list $linetype $pnts ""]
		        set pnts [lrange $pnts end-1 end]
		    }
		    set linetype $newlinetype
		    if { $pnts eq "" } { set pnts $pnts_old }

		    set pntL [lrange $pnts end-1 end]
		    set pntCL [lrange $pnts end-3 end-2]
		    if { $pntCL eq "" } { set pntCL $pntL }

		    if { [llength $pnts] > 2 && $quadratic_only_control_pnts } {
		        set pnts [lrange $pnts 0 end-2]
		    }
		    while { $i < [llength $d]-1 } {
		        incr i
		        if { $type in "t T" } {
		            set pntC [m::add $pntL [m::sub $pntL $pntCL]]
		        } else {
		            if { $type in "q t" } {
		                set pntC [m::add $pntL [lrange $d $i $i+1]]
		            } else {
		                set pntC [lrange $d $i $i+1]
		            }
		            incr i 2
		        }
		        if { $type in "q t" } {
		            set pnt [m::add $pntL [lrange $d $i $i+1]]
		        } else {
		            set pnt [lrange $d $i $i+1]
		        }
		        if { $quadratic_to_cubic } {
		            lappend pnts {*}[m::add [m::scale [expr {1.0/3.0}] $pntL] \
		                    [m::scale [expr {1-1.0/3.0}] $pntC]]
		            lappend pnts {*}[m::add [m::scale [expr {2.0/3.0}] $pntC] \
		                    [m::scale [expr {1-2.0/3.0}] $pnt]]
		        } else {
		            lappend pnts {*}$pntC
		        }
		        if { !$quadratic_only_control_pnts } {
		            lappend pnts {*}$pnt
		        }
		        lassign [list $pnt $pntC] pntL pntCL
		        incr i
		        if { [regexp {^[a-zA-Z]$} [lindex $d $i+1]] } { break }
		    }
		    if { $quadratic_only_control_pnts } {
		        lappend pnts {*}$pnt
		    }
		}
		z {
		    if { ![llength $pnts] } {
		        error "error: a path without a moveto"
		    }
		    if { $linetype eq "line" } {
		        eval lappend pnts $pnt0
		    } else {
		        eval lappend pnts [lrange $pnts end-1 end]
		        eval lappend pnts $pnt0
		        eval lappend pnts $pnt0
		    }
		}
	    }
	}
	if { [llength $pnts] > 2 } {
	    lappend lines [list $linetype $pnts ""]
	}

	if { $bezier3_to_lines } {
	    set idx 0
	    foreach i $lines {
		lassign $i type pnts props
		if { $type ne "bezier3" } { continue }
		set pnts_new ""
		for { set j 0 } { $j < [llength $pnts] } { incr j 6 } {
		    lassign [lrange $pnts $j [expr {$j+7}]] x1 y1 x2 y2 x3 y3 x4 y4
		    if { $x2 eq "" } { break }
		    for { set t 0.0 } { $t <= 1.0 } { set t [expr {$t+0.1}] } {
		        eval lappend pnts_new [m::eval_cubic_bezier $x1 $y1 $x2 $y2 \
		                $x3 $y3 $x4 $y4 $t]
		    }
		}
		lset lines $idx [list line $pnts_new $props]
		incr idx
	    }
	}
	return $lines
    }
    method draw_marker { url pnt tangent } {

	regexp {^url\(#(.*)\)$} $url {} id

	# by now, we disconnect the line endings
	if { [lsearch -exact [list Line1 Line2] $id] != -1 } { return }

	set ns { svg http://www.w3.org/2000/svg }
	set xp "svg:defs/svg:marker\[@id='[xml $id]'\]"
	set markerNode [$svgNode selectNodes -namespaces $ns $xp]
	
	set tr "translate([join $pnt ,]) "

	# this additional scale is not to take into account the additional
	# scales applied to the part of the drawing
	# ramsan: temporaly disconnected
#         foreach "f1x f1y" [$self tr_scalexy] break
#         foreach "f2x f2y" [$self tr_main_scalexy] break
#         set fx [expr {$f2x/$f1x}]
#         set fy [expr {$f2y/$f1y}]
#         append tr "scale($fx,$fy)"

	set radtodeg [::math::constants::give_constant radtodeg]
	if { [$markerNode @orient auto] eq "auto" } {
	    set angle [expr {$radtodeg*atan2([lindex $tangent 1],[lindex $tangent 0])}]
	} else {
	    set angle [$markerNode @orient]
	}
	append tr "rotate($angle) "
	if { [$markerNode @markerUnits strokeWidth] eq "strokeWidth" } {
	    append tr "scale([dict get $state stroke-width]) "
	}
	set viewBox [$markerNode @viewBox ""]
	if { $viewBox eq "" } {
	    set viewBox [list 0 0 [$markerNode @markerWidth 3] \
		    [$markerNode @markerHeight 3]]
	}
	set viewBoxToMarkerUnitsScaleX [expr {1.0*[$markerNode @markerWidth 3]/
		        [lindex $viewBox 2]}]
	set viewBoxToMarkerUnitsScaleY [expr {1.0*[$markerNode @markerHeight 3]/
		        [lindex $viewBox 3]}]
	if { $viewBoxToMarkerUnitsScaleX < $viewBoxToMarkerUnitsScaleY } {
	    set viewBoxToMarkerUnitsScale $viewBoxToMarkerUnitsScaleX
	} else {
	    set viewBoxToMarkerUnitsScale $viewBoxToMarkerUnitsScaleY
	}
	set tr_x [expr {-1*([$markerNode @refX 0]+[lindex $viewBox 0])*$viewBoxToMarkerUnitsScale}]
	set tr_y [expr {-1*([$markerNode @refY 0]+[lindex $viewBox 1])*$viewBoxToMarkerUnitsScale}]
	append tr "translate($tr_x,$tr_y) "
	append tr "scale($viewBoxToMarkerUnitsScale)"

	$self transform_mark
	$self push_transform_svg $tr
	dict set state fill [dict get $state stroke]
	set save_state $state
	foreach child [$markerNode childNodes] {
	    $self do_child $child
	    set state $save_state
	}
	$self pop_transform_to_mark
    }
    method _calc_arc_center_angles { c1 c2 r large-arc-flag sweep-flag } {
	set mid [m::scale .5 [m::add $c1 $c2]]
	set diff1 [m::unitLengthVector [m::sub $c2 $c1]]
	set normal [list [lindex $diff1 1] [expr {-1*[lindex $diff1 0]}]]
	set a [expr {.5*[m::norm [m::sub $c2 $c1]]}]
	if { $r < $a } {
	    error "error in arc path. radius too small r=$r < a=$a"
	    set r $a
	}
	set h [expr {sqrt($r*$r-$a*$a)}]
	set center(1) [m::add $mid [m::scale $h $normal]]
	set center(2) [m::add $mid [m::scale -$h $normal]]
	set radtodeg [::math::constants::give_constant radtodeg]

	foreach i [list 1 2] {
	    foreach "x1 y1" [m::sub $c1 $center($i)] break
	    foreach "x2 y2" [m::sub $c2 $center($i)] break
	    set start [expr {$radtodeg*atan2(-1*$y1,$x1)}]
	    if { $start < 0 } { set start [expr {360+$start}] }
	    set stop [expr {$radtodeg*atan2(-1*$y2,$x2)}]
	    if { $stop < 0 } { set stop [expr {360+$stop}] }
	    if { ${sweep-flag} } {
		foreach "start stop" [list $stop $start] break
	    }
	    if { $stop > $start } {
		set extent [expr {$stop-$start}]
	    } else {
		set extent [expr {360-$start+$stop}]
	    }
	    if { ($extent <= 180 && !${large-arc-flag}) || \
		($extent > 180 && ${large-arc-flag}) } { break }
	}
	set coords ""
	eval lappend coords [m::add $center($i) "-$r -$r"]
	eval lappend coords [m::add $center($i) "$r $r"]
	
	set n1 [m::unitLengthVector "$x1 $y1"]
	set tg1 [m::unitLengthVector [list $y1 [expr {-1*$x1}]]]
	if { ${sweep-flag} == 1 } { set tg1 [m::scale -1 $tg1] }

	set n1 [m::unitLengthVector "$x2 $y2"]
	set tg2 [m::unitLengthVector [list $y2 [expr {-1*$x2}]]]
	if { ${sweep-flag} == 1 } { set tg2 [m::scale -1 $tg2] }

	return [list $coords $center($i) $start $extent $tg1 $tg2]
    }
    method _calc_ellipse_center_angles { c1 c2 rx ry phi large-arc-flag sweep-flag \
	invert_orient } {
	
	set radtodeg [::math::constants::give_constant radtodeg]
	set pi [::math::constants::give_constant pi]

	set sin_phi [expr {sin($phi)}]
	set cos_phi [expr {cos($phi)}]
	set R [m::mkMatrix 2 2 0.0]
	m::setcol R 0 [list $cos_phi [expr {-1*$sin_phi}]]
	m::setcol R 1 [list [expr {$sin_phi}] $cos_phi]
	set cp [m::matmul $R [m::scale .5 [m::sub $c1 $c2]]]

	set rx [expr {abs($rx)}]
	set ry [expr {abs($ry)}]

	set fac [expr {pow([lindex $cp 0],2)/pow($rx,2)+pow([lindex $cp 1],2)/
		pow($ry,2)}]
	if { $fac > 1 } {
	    set eps [::math::constants::give_constant eps]
	    set rx [expr {sqrt($fac)*$rx+$eps}]
	    set ry [expr {sqrt($fac)*$ry+$eps}]
	}
	set r2 [expr {(pow($rx,2)*pow($ry,2)-pow($rx,2)*pow([lindex $cp 1],2)-
		pow($ry,2)*pow([lindex $cp 0],2))/(pow($rx,2)*pow([lindex $cp 1],2)+
		pow($ry,2)*pow([lindex $cp 0],2))}]
	if { $r2 < 0 && $r2 > -1e-7 } { set r2 0.0 }
	set r [expr {sqrt($r2)}]
	if { ${large-arc-flag} == ${sweep-flag} } {
	    set r [expr {-1*$r}]
	}

	set r0x [expr {$rx*[lindex $cp 1]/double($ry)}]
	set r0y [expr {-1*$ry*[lindex $cp 0]/double($rx)}]
	set centerp [m::scale $r [list $r0x $r0y]]

	set center [m::add [m::matmul [m::transpose $R] $centerp] \
		[m::scale .5 [m::add $c1 $c2]]]
	
	foreach "x1 y1" [m::sub $c1 $center] break
	foreach "x2 y2" [m::sub $c2 $center] break
	
	if { $invert_orient } {
	    set sweep-flag [expr {(${sweep-flag})?0:1}]
	    set y1 [expr {-1*$y1}]
	    set y2 [expr {-1*$y2}]
	}
	set start_rad [expr {atan2(-1*$y1/$ry,$x1/$rx)+$phi}]
	if { $start_rad < 0 } { set start_rad [expr {2*$pi+$start_rad}] }
	set stop_rad [expr {atan2(-1*$y2/$ry,$x2/$rx)+$phi}]
	if { $stop_rad < 0 } { set stop_rad [expr {2*$pi+$stop_rad}] }

	set start [expr {$radtodeg*$start_rad}]
	set stop [expr {$radtodeg*$stop_rad}]

	if { ${sweep-flag} } {
	    foreach "start stop" [list $stop $start] break
	}
	if { $stop > $start } {
	    set extent [expr {$stop-$start}]
	} else {
	    set extent [expr {360-$start+$stop}]
	}

	set coords ""
	eval lappend coords [m::add $center [m::matmul $R "-$rx -$ry"]]
	eval lappend coords [m::add $center [m::matmul $R "$rx $ry"]]

	# this is just an approximation to draw better the arrow
	set coords_px [$self tr $coords]
	set delta [expr {16.0/abs([lindex $coords_px 2]-[lindex $coords_px 0])}]
	if { $delta > .4 } { set delta .4 }
	if { ${sweep-flag} == 0 } {
	    set start_c [expr {$start_rad+$delta}]
	    set stop_c [expr {$stop_rad+$delta}]
	} else {
	    set start_c [expr {$start_rad-$delta}]
	    set stop_c [expr {$stop_rad-$delta}]
	}
	set tg1 [m::unitLengthVector [list [expr {-1*$rx*sin($start_c)}] \
		    [expr {-1*$ry*cos($start_c)}]]]

	set tg2 [m::unitLengthVector [list [expr {-1*$rx*sin($stop_c)}] \
		    [expr {-1*$ry*cos($stop_c)}]]]

#         set n1 [m::unitLengthVector "$x1 $y1"]
#         set tg1B [m::unitLengthVector [list $y1 [expr {-1*$x1}]]]
# 
#         set n1 [m::unitLengthVector "$x2 $y2"]
#         set tg2B [m::unitLengthVector [list $y2 [expr {-1*$x2}]]]

	if { ${sweep-flag} == 1 } {
	    set tg1 [m::scale -1 $tg1]
	    set tg2 [m::scale -1 $tg2]
	}
	return [list $coords $center $start $extent $tg1 $tg2]
    }
    # custom draw
    # args: ?-swap_xy boolean? ?-scale "scalex scaley"? ?p1 p2? or ?l1x l1y l2x l2y?
    method start_custom_draw { args } {

	set swap_xy 0
	set scalexy ""
	while { [string match -* [lindex $args 0]] } {
	    switch -- [lindex $args 0] {
		-swap_xy {
		    set swap_xy [lindex $args 1]
		    set args [lrange $args 2 end]
		}
		-scale {
		    set scalexy [lindex $args 1]
		    set args [lrange $args 2 end]
		}
		default {
		    error "error in start_custom_draw '[lindex $args 0]'"
		}
	    }
	}
	if { [llength $args] == 1 } {
	    set args [lindex $args 0]
	}
	if { ![llength $args] } {
	    set s http://www.w3.org/2000/svg
	    set node [[$svgNode ownerDocument] createElementNS $s g]
	    $svgNode appendChild $node
	    return $node
	} elseif { [llength $args] == 2 } {
	    foreach "p1 p2" $args break
	    set ns { svg http://www.w3.org/2000/svg }
	    foreach "c1x c1y c2x c2y" [list "" "" "" ""] break
	    foreach node [$svgNode selectNodes -namespaces $ns "//svg:rect"] {
		set params [split [$node @ramdraw:params ""] ,]
		if { [dict get $params type] ne "hotpoint" } { continue }
		if { [dict get $params hid] == $p1 } {
		    set c1x [dict get $params c1x]
		    set c1y [dict get $params c1y]
		    set l1x [dict get $params l1x]
		    set l1y [dict get $params l1y]
		} elseif { [dict get $params hid] == $p2 }  {
		    set c2x [dict get $params c1x]
		    set c2y [dict get $params c1y]
		    set l2x [dict get $params l1x]
		    set l2y [dict get $params l1y]
		}
	    }
	} elseif { [llength $args] == 4 } {
	    foreach "l1x l1y l2x l2y" $args break
	    foreach i [list 1x 1y 2x 2y] {
		set c$i [$self give_base_line_coord [set l$i]]
	    }
	} else {
	    error "error in start_custom_draw (args=$args)"
	}
	set scale_x [expr {1.0*($c2x-$c1x)}]
	set scale_y [expr {1.0*($c2y-$c1y)}]

	if { !$swap_xy } {
	    set transform "translate($c1x,$c1y) scale($scale_x,$scale_y)"
	} else {
	    set scale_y [expr {-1*$scale_y}]
	    set transform "translate($c1x,$c1y) scale($scale_x,$scale_y) rotate(-90)"
	}
	if { $scalexy ne "" } {
	    append transform " scale([join $scalexy ,])"
	}
	set s http://www.w3.org/2000/svg
	set node [[$svgNode ownerDocument] createElementNS $s g]
	$node setAttribute transform $transform
	set s http://example.org/ramdraw
	set params [dict create type box]
	foreach i [list l1x l1y l2x l2y] {
	    dict set params $i [set $i]
	}
	$node setAttributeNS $s ramdraw:params [join $params ,]
	$svgNode appendChild $node
	return $node
    }
    method add_to_custom_draw { parentNode xml } {
	if { $xml eq "" } { return }
	$parentNode appendXML $xml
    }
    method print_pdf {} {
	set xmlheader "<?xml version='1.0' encoding='utf-8'?><!-- -*- coding: utf-8;-*- -->\n"
	append xmlheader "<!DOCTYPE compass_report>\n"
	set xml "$xmlheader<compass_report>\n"
	append xml "<mediaobject><imageobject>"
	append xml [[$self give_svgNode] asXML]
	append xml "</imageobject></mediaobject>"
	append xml "</compass_report>\n"
	
	set boxList {
	    {mainbox all {11.5% 10.4% 89.3% 92.9%} 1}
	    {colorbox all {10.4% 9.47% 90.5% 93.35%} {} #CEC2C2 3}
	    {colorbox all {10.2% 9.62% 90.3% 93.5%} {} #3A967E 3}
	    {text all {11% 7.3% 65% 8.5%} Helvetica 12 black white sw {<wordwidget><para><emphasis role="strong">$sectiontitle</emphasis></para></wordwidget>} 1}
	    {text all {10% 94.1% 25% 95.8%} Helvetica 10 #000000 white nw {<wordwidget><para>Página $pagenum</para></wordwidget>} 1}
	    {text all {66.6% 94.1% 83.6% 96%} Helvetica 6 #000000 white n {<wordwidget><para>Developed by Compass, Ing. y Sist.</para><para>http://www.compassis.com</para></wordwidget>} 0}
	}
	set opts {
	    TitleBackgroundColor #cee3ea
	    sizedefault 10
	}
	set file [xml2pdf:::PrintXML $xml A4 $boxList $opts]
    }
    method save_svg {} {
	
	set types [list \
		[list [_ "SVG files"] ".svg"] \
		[list [_ "All files"] "*"] \
		]
	set file [tk_getSaveFile -filetypes $types -title \
	[_ "Save SVG file"] -defaultextension .svg]
	if { $file == "" } { return }
	
	set fout [open $file w]
	fconfigure $fout -encoding utf-8
	puts $fout "<?xml version='1.0' encoding='utf-8'?><!-- -*- coding: utf-8;-*- -->\n"
	puts $fout [[$self give_svgNode] asXML]
	close $fout
    }
    method print_image {} {
	set file [cu::file::tempfile image .png]
	$self draw -image_gd_file $file
	package require RamViewer
	set w [RamViewer::Init ""]
	RamViewer::imgopenfile $w $file
    }
    method svg_hide_parts { show_dimensions show_blocks show_images } {
	svg_hide_parts $svgNode $show_dimensions $show_blocks $show_images
    }
    proc svg_hide_parts { svgNode show_dimensions show_blocks show_images } {
	set ns { svg http://www.w3.org/2000/svg }
	foreach node [$svgNode selectNodes -namespaces $ns {*[@display_save]}] {
	    if { [$node @display_save] eq "" } {
		$node removeAttribute display
	    } else {
		$node setAttribute display [$node @display_save]
	    }
	    $node removeAttribute display_save
	}
	set ns { svg http://www.w3.org/2000/svg }
	foreach node [$svgNode selectNodes -namespaces $ns {*[@ramdraw:params]}] {
	    set params [split [$node @ramdraw:params] ,]
	    set type [dict get $params type]
	    set hide 0
	    if { !$show_dimensions && $type eq "line_cota" } { set hide 1 }
	    if { !$show_blocks && $type eq "block" } { set hide 1 }
	    if { !$show_images && $type eq "image" } { set hide 1 }
	    if { $hide && [$node @display ""] ne "none" } {
		$node setAttribute display_save [$node @display ""]
		$node setAttribute display none
	    }
	}
    }
}

# repeated in drawformulas.tcl
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

proc svg2draw::create_svg_menu { args } {
    
    set optional {
	{ -scale factor 1.0 }
	{ -align left|center|right center }
	{ -style line|cascade line }
    }
    set compulsory "menuList"
    parse_args $optional $compulsory $args

    switch -- $style {
	cascade {
	    return [_create_svg_menu_cascade -scale $scale -align $align $menuList]
	}
	default {
	    return [_create_svg_menu_line -scale $scale -align $align $menuList]
	}
    }
}

proc svg2draw::_create_svg_menu_line { args } {
    
    set optional {
	{ -scale factor 1.0 }
	{ -align left|center|right center }
    }
    set compulsory "menuList"
    parse_args $optional $compulsory $args

    set xml "<mediaobject><imageobject>"
    
    set font {{Segoe UI} 11 bold}
    set width 5
    set idx 0
    foreach i $menuList {
	if { $idx > 0 } {
	    set width [expr {$width+19}]
	}
	set width [expr {$width+[font measure $font $i]}]
	incr idx
    }
    set height 22

#     foreach i [list width height] {
#         set $i [expr {$scale*[set $i]}]
#     }
    
    lassign [list $width $height] x y
    append xml [format {<svg xmlns="http://www.w3.org/2000/svg" 
	    xmlns:xlink="http://www.w3.org/1999/xlink" 
	    xmlns:ramdraw="http://example.org/ramdraw" 
	    width="%s" height="%s" version="1.1"
	    viewBox="0 0 %s %s" align="%s">
	} $width $height $x $y $align]

    
    set x 5
    set idx 0
    foreach i $menuList {
	if { $idx > 0 } {
	    set x [expr {$x+4}]
	    set x2 [expr {$x+6}]
	    append xml [format {<path stroke="none" fill="#3399ff"
		d="M%g,7 L %g,11 %g,15 z"/>} $x $x2 $x]
	    set x [expr {$x+9}]
	}
	append xml [format {<text x="%s" y="%s" 
		font-family="Segoe UI" font-weight="bold" font-size="14" 
		fill="#3399ff">%s</text>} $x 16 [xml_map $i]]
	
#         append xml [format {<path stroke="black" fill="black"
#                 d="M%g,16 L %g,16"/>} $x [expr {$x+[font measure $font $i]}]]
	
	set x [expr {$x+[font measure $font $i]}]
	incr idx
    }
    append xml "</svg></imageobject></mediaobject>"
    return $xml
}

proc svg2draw::_create_svg_menu_cascade { args } {
    
    set optional {
	{ -scale factor 1.0 }
	{ -align left|center|right center }
    }
    set compulsory "menuList"
    parse_args $optional $compulsory $args
    
    #set xml {<?xml version="1.0" encoding="utf-8"?><!-- -*- coding: utf-8; mode: SGML -*- -->}
    set xml "<mediaobject><imageobject>"
    
    set svg_menu.png [string trim {
iVBORw0KGgoAAAANSUhEUgAAAK0AAAB9CAMAAADnTxNeAAAAOVBMVEUAAJmPj4+QkJCbm5ugoKCm
pqarq6usrKy0tLS4uLjHx8fKysrU1NTZ2dnj4+Pr6+vw8PD///+Ojo6YbTvWAAAAAXRSTlMAQObY
ZgAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0SU1FB9kFDBAGGszNKgMAAAChSURBVHja7dw5DoNA
FERBlgEPu839D+uAhGAIkG3pW6p3ggo67qqSJEmSJOl+0/CruvRhBe3wjFqqt3/S7v1MS0tLS0tL
S0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLG1rbBOxa+4qY3dLS0tLS0tLS
0tLS0tLS0n5Jm8JW0B4tuW32aLV5KWvXMT/6aOVx9cuic2987L+Pw8GK+gAAAABJRU5ErkJggg==
	}]
    
    set svg_menu_name.png [string trim {
iVBORw0KGgoAAAANSUhEUgAAAK0AAAB9CAMAAADnTxNeAAACBFBMVEUAAABKTFNLTVNLTVRMTlRN
T1VOUFVOUFdQUlZRUlZTVFdbXF1iYmNhY2pkZGVlZWZlZWdmZmdkZ21oaGlpaWpoamxqamtsbG1t
bW5ubm5vb3BwcHFucHhycnJzc3R0dHV1dXZ2dnd3d3h3eX55eXp4en54en96ent5en97e3x6e4B6
fIB9fX18fYB9fYB+fn9+f4F/f4CAgIGCgoN+go6Dg4OEhIWFhYaGhoeIiImJiYqKioqJio+Li4yM
jI2Ojo6Oj5GPj5CPkJOQkJGRkpSSkpKRk5WTk5SUlJSVlZaXl5eYmJiZmZmampubm5ydnZ2dnZ6f
n5+foKKgoKGhoaKeoa2eo66ho6ejo6OkpKWlpaahprKiprCkpqynp6enp6ijqLOkqLOkqbSnqaym
qbCnq7aprbitrrGsr7qtsLu0tLW2t7e2uLu2ucK3ur+2usa5u762u8q5v8+9wMi7wdC9wtDCwsW/
w8y9w9LAxdPCx9bGyM7CyNnHyc/FydfIytDIytHGytjJy9HHy9nFzN3KzNLJzNXIzdnIzdrLzdPH
zd7KzdbLztTIzt/LztfMz9jKz9/J0OHL0ODN0dvM0eHM0uHM0uLN0uLO0+LO0+PP0+PO1OTQ1OTP
1eXQ1eTQ1eXQ1ubR1uXS1ubT2OfU2OjY2dnU2ejV2enV2unW2+rn6e5AQkeBNiBkAAAAAXRSTlMA
QObYZgAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0SU1FB9kFDBAGJ5SlZhIAAAGpSURBVHja7dfr
M1RxGMBxKVaoXEpUKreSQu4KqQiFSikUlfstRCmFjshGWbS27LLOEm1L/2Rs03SsmTNj5vd79/3M
PHOe3zxvvm+Plxcg1s+Op08e1VRX3q+4e+d2eVnprZslxTcKruVfycu5fCk7KzMjLTUl+cK5s7HR
UXq306dORh4/FhF+NOxwaPChg4GBUZ0SYl8HFd6TojCgVXzt1aLG51K8uH5EfO3Fqm5FmNHBYWVc
UUYmN/cpxV98bVLdyy9/GY3Gmc3PtHv9t20/6N08nzPT+8XXJja9mZPET3xtzMPeIUn2SKh93PdR
Egm1cQ0DXyXxFl975tmrz24mzWg37UHv5vk0mfaKr42v75+VZJ+E2ua3393MmtFu2oPezfNpNkuo
TWh5t7xl6dvikm3etmiz2BzL/7dtB73bjqfF4iOhtu3DmigrVlVdsTtU+4Ld8cNqlVB7vn3slyhO
dc09q+qq06mqvuJrc2sn1oVxacd1QHxtV0iPS4r3Jx6Ir/2UbvgthSF9g18dAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAMCu/AHAUGY0UX6KmwAAAABJRU5ErkJggg==
	}]
    
    switch [llength $menuList] {
	2 { lassign [list 200 160 1 0.8] width height x y }
	3 { lassign [list 370 216 1.85 1.08] width height x y }
    }
    
    foreach i [list width height] {
	set $i [expr {$scale*[set $i]}]
    }
    append xml [format {<svg xmlns="http://www.w3.org/2000/svg" 
	    xmlns:xlink="http://www.w3.org/1999/xlink" 
	    xmlns:ramdraw="http://example.org/ramdraw" 
	    width="%s" height="%s" version="1.1"
	    viewBox="0 0 %s %s" align="%s">
	} $width $height $x $y $align]
    
    append xml [format {<image xlink:href="data:image/png;base64,%s" x="0.02" y="0.02" width="0.98" height="0.78" preserveAspectRatio="none"/>} ${svg_menu.png}]
    append xml [format {<image xlink:href="data:image/png;base64,%s" x="0.02" y="0.02" width="0.98" height="0.78" preserveAspectRatio="none"/>} ${svg_menu_name.png}]
    

    append xml {<path stroke="none" fill="#3399ff"
	d="M0.09,0.39 L 0.93,0.39 0.93,0.49 0.09,0.49 z"/>}

    set txts [list \
	    [list 0.6 0.11 black [lindex $menuList 0]] \
	    [list 0.16 0.46 white [lindex $menuList 1]] \
	    [list 0.16 0.25 black ...] \
	    [list 0.16 0.33 black ...] \
	    [list 0.16 0.54 black ...] \
	    [list 0.16 0.70 black ...] \
	    ]
    foreach i $txts {
	lassign $i x y fill txt
	append xml [format {<text x="%s" y="%s" 
		font-family="Segoe UI" font-size="0.06" fill="%s">%s</text>} $x $y $fill $txt]
    }   
    if { [llength $menuList] == 3 } {
	append xml {<path stroke-width="0.012" stroke="none" fill="white"
	    d="M0.88,0.42 L 0.90,0.44 0.88,0.46 z"/>}
	append xml {<g transform="translate(0.85,0.22)">}
	append xml [format {<image xlink:href="data:image/png;base64,%s" x="0.02" y="0.02" width="0.98" height="0.78"/>} ${svg_menu.png}]
	append xml {<path stroke-width="0.012" stroke="none" fill="#3399ff"
	    d="M0.09,0.39 L 0.93,0.39 0.93,0.49 0.09,0.49 z"/>}
	
	lset txts 1 3 [lindex $menuList 2]
	foreach i [lrange $txts 1 end] {
	    lassign $i x y fill txt
	    append xml [format {<text x="%s" y="%s" 
		    font-family="Segoe UI" font-size="0.06" fill="%s">%s</text>} $x $y $fill $txt]
	}
	append xml {</g>}
    }
    append xml "</svg></imageobject></mediaobject>"
    return $xml
}

# snit::macro delegate_existant { snittype name } {
#     set mytype [$snittype %AUTO%]
#     set non_delegation [list info destroy configure configurelist cget]
#     foreach i [$mytype info methods] {
#         if { [lsearch -exact $non_delegation $i] == -1 } {
#             delegate method $i to $name
#         }
#     }
#     $mytype destroy
# }
# 
# snit::widgetadaptor svg2canvas {
# 
#     delegate method * to hull
#     delegate option * to hull
#     delegate_existant ::svg2draw svg2draw
# 
#     component svg2draw
# 
#     constructor args {
#         if { [winfo exists $win] } {
#             if { [winfo class $win] ne "Canvas" } {
#                 error "error in svg2canvas constructor"
#             }
#         } else {
#             canvas $win
#         }
#         installhull $win
#         install svg2draw using svg2draw %AUTO% -canvas $win        
#         $self configurelist $args
#     }
#     destructor {
#         $svg2draw destroy
#     }
# }
# 

namespace eval svg2canvas_window {
    variable window_doc
    variable svg2draw
    variable currentfile
}

proc svg2canvas_window::create_window { wp xml { basedir "" } } {
    variable window_doc
    variable svg2draw

    package require autoscroll

    if { $wp eq "." } { set wp "" }
    destroy $wp._ask
    set w [dialogwin_snit $wp._ask -title [_ "SVG window"] -okname - \
	    -cancelname [_ Close] -callback [namespace code [list create_window_do]] \
	    -grab 0]
    set f [$w giveframe]
	    
    set tcanvas [canvas $f.c -bg white \
	    -xscrollcommand [list $f.sh set] \
	    -yscrollcommand [list $f.sv set] \
	    -bd 0 -highlightthickness 0]
    scrollbar $f.sv -orient vertical -command [list $f.c yview]
    scrollbar $f.sh -orient horizontal -command [list $f.c xview]

    set svg2draw [svg2draw %AUTO% -canvas $tcanvas -destroy_with_canvas 1 \
	    -basedir $basedir -add_to_contextual [list svg2canvas_window::add_to_contextual $w] \
	    -draw_animation_time 1]
    
    if { [info exists window_doc] } {
	$window_doc delete
	unset window_doc
    }

    if { $xml ne "" } {
	dom parse $xml window_doc
	set root [$window_doc documentElement]
	
	set ns { svg http://www.w3.org/2000/svg } 
	set formulaeNode [$root selectNodes formulae]
	set svgNode [$root selectNodes -namespaces $ns /*/svg:svg|/svg:svg]
	set width [svg2draw::_length [$svgNode @width]]
	set height [svg2draw::_length [$svgNode @height]]
    } else {
	foreach "formulaeNode svgNode" [list "" ""] break
	foreach "width height" [list 400 400] break
    }
    if { $formulaeNode ne "" } {
	set key $f
	set wf [formulae::create_windowD -update_callback \
		[list $svg2draw draw_values] \
		-tabstyle notebook $f $key $formulaeNode]
	grid $wf $f.c $f.sv -sticky nsew
	grid columnconfigure $f 1 -weight 1
    } else {
	grid $f.c $f.sv -sticky nsew
	grid columnconfigure $f 0 -weight 1
    }
    grid $f.sh -sticky ew
    grid rowconfigure $f 0 -weight 1

    $svg2draw configure -svgnode $svgNode
    
    if { [$svg2draw has_animations] } {
	set end_time [$svg2draw give_animate_end_time]
	if { $end_time > 0 } {
	    ttk::scale $f.pb -from 0 -to $end_time -command [list svg2canvas_window::animation update] \
		-variable [$w give_uservar animation_time 0]
	    $svg2draw configure -time_variable [$w give_uservar animation_time]
	    ttk::frame $f.f1
	    ttk::button $f.f1.b1 -text [_ "Rewind"] -command [list svg2canvas_window::animation rewind]
	    ttk::button $f.f1.b2 -text [_ "Stop"] -command [list svg2canvas_window::animation \
		    start_stop $f.f1.b2]
	    grid $f.f1.b1 $f.f1.b2 -sticky w -padx 2 -pady 2

	    grid $f.pb - -sticky ew -padx 4 -pady "10 2"
	    grid $f.f1 - -sticky ew
	}
    }

    set twidth [expr {$width-[winfo reqwidth $f.sv]}]
    set theight [expr {$height-[winfo reqheight $f.sh]}]
    if { $theight > 600 } {
	set theight 600
    }
    $tcanvas configure -width $twidth -height $theight -scrollregion \
	[list 0 0 $twidth $theight]

    autoscroll::autoscroll $f.sv
    autoscroll::autoscroll $f.sh

    bind $w <MouseWheel> "$tcanvas yview scroll \[expr {%D/-120}\] units"
    bind $w <space> [list $svg2draw toggle_stop_continue_animation]
    
    if { [info command dnd] ne "" } {
	dnd bindtarget $w text/uri-list <Drop> [list svg2canvas_window::open_files $w %D]
    }
    # trick to make the border disappear
    grid $f -padx 0 -pady 0

    if { $svgNode ne "" } {
	$svg2draw draw -width $width -height $height
	if { $formulaeNode ne "" } { formulae::window_actualize $key }
    }
    focus $tcanvas
    $w createwindow
    return $w
}

proc svg2canvas_window::open_files { w files } {
    return [open_file $w [lindex $files 0]]
}

proc svg2canvas_window::open_file { w { file "" } } {
    variable currentfile
    variable svg2draw
    
    if { $file eq "" } {
	set file [tk_getOpenFile -parent $w -defaultextension .svg \
		-filetypes [list [list [_ "SVG files"] {.svg}] \
		    [list [_ "All files"] *]]]
	if { $file eq "" } { return }
    }
    set file [file normalize $file]
    set err [catch { tDOM::xmlReadFile $file } xml]
    if { $err } {
	snit_messageBox -parent $canvas \
	    -message [_ "Could not open file '%s' (%s')" $file $xml]
	return
    }
    set currentfile($w) $file
    set doc [dom parse $xml]
    set svgNode [$doc documentElement]
    $svg2draw configure -basedir [file dirname $file]
    $svg2draw draw -svgNode $svgNode
}

proc svg2canvas_window::reopen { w } {
    variable currentfile
    
    if { ![info exists currentfile($w)] } { return }
    open_file $w $currentfile($w)
}


proc svg2canvas_window::create_window_do { w } {
    destroy $w
}

proc svg2canvas_window::animation { what args } {
    variable svg2draw
    
    switch $what {
	start_stop {
	    set button [lindex $args 0]
	    if { [$svg2draw is_animation_running] } {
		$button configure -text [_ Start]
		$svg2draw stop_animation
	    } else {
		$button configure -text [_ Stop]
		$svg2draw draw -continue_draw_time 1
	    }
	}
	rewind {
	    $svg2draw draw -reset_draw_time 1
	}
	update {
	    $svg2draw draw -continue_draw_time 1 -draw_one_frame 1
	}
    }
}

proc svg2canvas_window::add_to_contextual { w menu } {
    $menu add separator
    
    $menu add command -label [_ "Open"] -command [list svg2canvas_window::open_file $w]
    $menu add command -label [_ "Repen"] -command [list svg2canvas_window::reopen $w]
    $menu add separator
    $menu add command -label [_ "Print"] -command [list svg2canvas_window::print]
    $menu add command -label [_ "Print PNG"] -command \
	[list svg2canvas_window::print_image]
    $menu add command -label [_ "Print GiD"] -command \
	[list svg2canvas_window::print_gid]

}

proc svg2canvas_window::print {} {
    variable svg2draw

    $svg2draw print_pdf
}

proc svg2canvas_window::print_image {} {
    variable svg2draw
    
    $svg2draw print_image
}

proc svg2canvas_window::print_gid {} {
    variable svg2draw
    
    set file [cu::file::tempfile gid_batch ".bch"]
    set fout [open $file w]
    puts $fout [$svg2draw draw -gid_output 1]
    close $fout
    cu::file::execute gid -p RamSeries6.1/ramseries -b $file
}






























