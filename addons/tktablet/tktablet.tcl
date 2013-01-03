
package require snit
catch { package require img::png } ;# for wince

package provide tktablet 1.1

if { [info command ::_] eq "" } {
    proc ::_ { format args } { return [eval [list format $format] $args] }
}

namespace eval tktablet {
    variable textinputpanel
    variable peninputpanel
    variable last_bottom
    variable drag_mode_state
    variable drag_mode_images_resolution 20
    variable topdir [file dirname [info script]]
    
    namespace eval img {}
}

proc tktablet::is_tablet_pc {} {
    
    if { [info command ::tktablet::IsTabletpc] ne "" } {
	return [tktablet::IsTabletpc]
    }
    return 0
}
proc tktablet::_init_input_panel_lib {} {
    variable topdir
    variable textinputpanel
    variable peninputpanel
    
    if { $::tcl_platform(platform) ne "windows" } { return -1 }
    
    uplevel #0 [list load [file join $topdir tktablet.dll]]
    if { ![tktablet::IsTabletpc] } { return -1 }
    
    if { ![info exists textinputpanel] } {
	
	set err [catch { tktablet::TextInputPanel } textinputpanel]
	if { $err } {
	    unset textinputpanel
	    set err [catch {
		    package require tcom
		    package require twapi
		    tcom::ref createobject "PenInputPanel.PenInputPanel" } \
		    peninputpanel]
	    if { $err } {
		unset peninputpanel
		return -1
	    }
	}
    }
    return 0
}

proc tktablet::init_input_panel {} {

    if { [_init_input_panel_lib] == -1 } { return }
    foreach class [list Entry Spinbox TEntry TCombobox] {
	bind $class <ButtonRelease-1> "+[list tktablet::_show_hover %W]"
    }   
}

proc tktablet::init_input_panel_text {} {
 
    if { [_init_input_panel_lib] == -1 } { return }
    foreach class [list Text] {
	bind $class <ButtonRelease-1> "+[list tktablet::_show_hover %W]"
	bind $class <Motion> "+[list tktablet::_check_hide_panel %W %X %Y]"
    }   
}

proc tktablet::_show_hover { w } {
    variable textinputpanel
    variable peninputpanel
    variable last_bottom
    
    set err [catch { $w cget -state } state]
    if { !$err && $state eq "disabled" } { return }
    set err [catch { $w cget -readonly } readonly]
    if { !$err && $readonly == 1 } { return }
    if { [is_drag_mode_enabled $w] } { return }
   
    if { [info exists peninputpanel] } {
	$peninputpanel AttachedEditWindow [expr [winfo id $w]]
	twapi::set_focus [winfo id $w]
	return
    }

    set height [winfo screenheight $w]
    foreach "top left bottom right" [$textinputpanel InPlaceBoundingRectangle] break
    set bottom0 $bottom
    
    switch -- [winfo class $w] {
	Text {
	    foreach "x0 y0 he" [list 0 0 15] break
	    foreach "x0 y0 - he" [$w bbox insert] break
	    set x [expr {[winfo rootx $w]+$x0}]
	    set y [expr {[winfo rooty $w]+$y0+$he+25}]
	    set yp $y
	    if { $y > .5*$height } {
		set y [expr {[winfo rooty $w]+$y0-75}]
		set yp [expr {$y-$bottom+70}]
	    }
	}
	default {
	    set x [winfo rootx $w]
	    set y [expr {[winfo rooty $w]+[winfo height $w]+25}]
	    set yp $y
	    if { $y > .5*$height } {
		set y [expr {[winfo rooty $w]-65}]
		set yp [expr {[winfo rooty $w]-$bottom-0}]
	    }
	}
    }
    if { $y < .5*$height } {
	set c bottom
    } else {
	set c top
    }
    $textinputpanel AttachedEditWindow $w
    tktablet::SetFocus $w
    #$textinputpanel SetInPlaceVisibility 1

    set err [catch { $textinputpanel SetInPlaceHoverTargetPosition $x $y }]
    if { $err } { return }
    
    set x [expr {$x-50}]
    if { $x < 0 } { set x 0 }
    #$textinputpanel SetInPlacePosition $x $yp $c
    incr right $x
    incr bottom $yp
    set needsupdate 0
    if { $right > [winfo screenwidth $w] } {
	set x [expr {$x-($right-[winfo screenwidth $w])}]
	set needsupdate 1
    }
    if { $bottom > [winfo screenheight $w] } {
	set yp [expr {$yp-($bottom-[winfo screenheight $w])}]
	set needsupdate 1
    }
    $textinputpanel SetInPlacePosition $x $yp $c
    set last_bottom $yp
    if { $c eq "top" } { incr last_bottom $bottom0 }

#     if { $needsupdate } {
#         $textinputpanel SetInPlacePosition $x $yp $c
#     }
}

proc tktablet::_check_hide_panel { w x y } {
    variable textinputpanel
    variable peninputpanel
    variable last_bottom
    
    set err [catch { $w cget -state } state]
    if { !$err && $state eq "disabled" } { return }
    set err [catch { $w cget -readonly } readonly]
    if { !$err && $readonly } { return }
   
    if { [info exists peninputpanel] } {
	return
    }
    if { [info exists last_bottom] && $y > $last_bottom + 200 } {
	set ca [$textinputpanel CurrentInputArea]
	catch { $textinputpanel SetInPlaceVisibility 0 }
	tktablet::SetFocus [winfo toplevel $w]
	unset last_bottom
	$textinputpanel DefaultInputArea $ca
    }
}

proc tktablet::_give_image { imgname } {
    variable topdir

    if { [info command img::$imgname] eq "" } {
	set file [file join $topdir images $imgname]
	image create photo img::$imgname -file $file
    }
    return img::$imgname
}

proc tktablet::_give_no_cursor {} {
    variable topdir
    
    if { [info exists ::starkit::topdir] } {
	set nfile [file join $::env(TEMP) no.cur]
	if { ![file exists $nfile] } {
	    file copy [file join $topdir images no.cur] $nfile
	}
	set no_cursor [list @[file attributes $nfile -shortname]]
    } else {
	set no_cursor [list @[file normalize [file join $topdir images no.cur]]]
    }
    return $no_cursor
}

proc tktablet::is_drag_mode_enabled { text } {
    set b [bindtags $text]
    if { [set ipos [lsearch -exact $b drag_text_enabled]] != -1 } {
	return 1
    }
    return 0
}

proc tktablet::drag_mode { text button variablename res } {
    variable drag_mode_state
    variable drag_mode_images_resolution
    
    set drag_mode_images_resolution $res
 
    bind drag_text_enabled <1> {
	set ::tk::Priv(x) %x
	set ::tk::Priv(y) %y
	set ::tk::Priv(xview) [%W xview]
	set ::tk::Priv(yview) [%W yview]
	set ::tk::Priv(mouseMoved) 0
	break
    }
    bind drag_text_enabled <B1-Motion> {
	if { ![info exists ::tk::Priv(xview)] } {
	    set ::tk::Priv(xview) [%W xview]
	    set ::tk::Priv(yview) [%W yview]
	}

	if {![info exists ::tk::Priv(x)]} { set ::tk::Priv(x) %x }
	if {![info exists ::tk::Priv(y)]} { set ::tk::Priv(y) %y }
	if {(%x != $::tk::Priv(x)) || (%y != $::tk::Priv(y))} {
	    set ::tk::Priv(mouseMoved) 1
	}
	if {[info exists ::tk::Priv(mouseMoved)] && $::tk::Priv(mouseMoved)} {
	    if { %x < 0 || %x > [winfo width %W] ||  %y < 0 || %y > [winfo height %W] } {
		break
	    }
	    foreach "x0 x1" $::tk::Priv(xview) break
	    foreach "y0 y1" $::tk::Priv(yview) break
	    set deltax [expr {1.0*(%x-$::tk::Priv(x))/[winfo width %W]*($x1-$x0)}]
	    
	    %W xview moveto [expr {$x0-$deltax}]
	    set deltay [expr {1.0*(%y-$::tk::Priv(y))/[winfo height %W]*($y1-$y0)}]
	    %W yview moveto [expr {$y0-$deltay}]
	}
	break
    }
    bind drag_text_enabled <B1-Leave> break
    bind drag_text_enabled <ButtonRelease-1> break
    bind drag_text_enabled <Double-1> {
	after 100 [list tktablet::_drag_mode_cmd %W drag_disable]
	tk::TextButton1 %W %x %y
	#break
    }
    bind drag_text_disabled <Double-1> {
	foreach "x0 x y" [list "" %x %y] break
	foreach "x0 y0 wi he" [%W bbox [%W index @$x,$y]] break
	set c [%W get [%W index @$x,$y]]
	if { ![string is space $c] && $x0 ne "" && $x >= $x0 && $x <= $x0+$wi && $y >= $y0 &&
	    $y <= $y0+$he } { return }
	after idle [list tktablet::_drag_mode_cmd %W drag_enable]
	break
    }
    set state drag_off
    if { $variablename ne "" } {
	upvar #0 $variablename v
	if { [info exists v] } {
	    set state $v
	}
    }
    set drag_mode_state($text) [list $button $variablename $state]

    _drag_mode_cmd $text
}
    
proc tktablet::_drag_mode_cmd { text { new_state "" } } {
    variable drag_mode_state
    variable drag_mode_images_resolution

    if { $new_state ne "" } {
	lset drag_mode_state($text) 2 $new_state
    }
    foreach "button variablename state" $drag_mode_state($text) break
    if { $variablename ne "" } {
	upvar #0 $variablename v
	set v $state
    }
    set b [bindtags $text]
    if { [set ipos [lsearch -exact $b drag_text_enabled]] != -1 } {
	set b [lreplace $b $ipos $ipos]
    }
    if { [set ipos [lsearch -exact $b drag_text_disabled]] != -1 } {
	set b [lreplace $b $ipos $ipos]
    }
    switch $state {
	drag_enable {
	    set b [linsert $b 0 drag_text_enabled]
	    $text configure -cursor hand2
	    if { $button ne "" } {
		$button configure -image [_give_image hand-$drag_mode_images_resolution.png] \
		    -command [namespace code [list _drag_mode_cmd $text drag_off]]
	    }
	}
	drag_disable {
	    set b [linsert $b 0 drag_text_disabled]
	    $text configure -cursor [lindex [$text configure -cursor] 3]
	    if { $button ne "" } {
		$button configure -image [_give_image hand-yellow-$drag_mode_images_resolution.png] \
		    -command [namespace code [list _drag_mode_cmd $text drag_off]]
	    }
	}
	drag_off {
	    $text configure -cursor [lindex [$text configure -cursor] 3]
	    if { $button ne "" } {
		$button configure -image [_give_image hand-red-$drag_mode_images_resolution.png] \
		    -command [namespace code [list _drag_mode_cmd $text drag_enable]]
	    }
	}
    }
    bindtags $text $b
}


# this proc is not used by the package but can be useful for debugging
proc tktablet::findApps {arrNamePtr appListPtr} {
    global startupCounter
    
    package require registry
    set localCount 1
    upvar $arrNamePtr progArr $appListPtr appList
    foreach progId [registry keys HKEY_CLASSES_ROOT] {
	if {[catch {set clsid [registry get HKEY_CLASSES_ROOT\\$progId\\CLSID ""]}]} {
	    if {[catch {set clsid [registry get HKEY_CLASSES_ROOT\\$progId\\Clsid ""]}]} {
		continue
	    }
	}
	if {[incr localCount] % 10 == 0} {
	    incr startupCounter
	}
	set keys ""
	catch {set keys [registry keys HKEY_CLASSES_ROOT\\CLSID\\$clsid]}
	if {[lsearch $keys Programmable] >= 0 || \
	    [lsearch $keys LocalServer*] >= 0 } {
	    set l [split $progId .]
	    if {[llength $l] == 2} {
		foreach {prog class} $l break
		if {[string length $prog] && [string length $class]} {
		    lappend progArr($class) $progId
		    lappend appList $progId
		}
	    }
	}
    }
}

snit::widgetadaptor tktablet::notebook {

    delegate method * to hull
    delegate option * to hull
    
    variable overlay
    variable bgimage
    
    constructor args {
	
	set bgimage [image create photo]
	installhull [label $self -image $bgimage -cursor [tktablet::_give_no_cursor]]

	package require tcom
	set overlay [tcom::ref createobject "msinkaut.InkOverlay"]
	
	$overlay hWnd [expr [winfo id $win]]
	$overlay SetEventInterest 0 1 ;# 0: CursorDown (check msinkaut.h)
	$overlay SetEventInterest 4 1 ;# 4: CursorButtonDown
	$overlay SetEventInterest 5 1 ;# 5: CursorButtonUp
	$overlay SetEventInterest 6 1 ;# 6: CursorInRange

	$overlay Enabled 1
	$overlay EditingMode 0
	tcom::bind $overlay [mymethod _events]
	set d [$overlay DefaultDrawingAttributes]
	$d Color [expr {0xff0000}]
	
	bind $win <Configure> [mymethod _configure]
	$self configurelist $args
    }
    method _configure {} {
	set img [tktablet::_give_image note_bg_lines.gif]
	set width [winfo width $win]
	set height [winfo height $win]
	if { $width <= 1 } { return }
	
	if { $width > [image width $bgimage] || $height > [image height $bgimage] } {
	    $bgimage configure -width $width -height $height
	    set h 0
	    while { $h < $height } {
		$bgimage copy $img -to 0 $h
		set w [image width $img]
		while { $w < $width } {
		    $bgimage copy $img -to $w $h -from 40 0
		    incr w [expr {[image width $img]-40}]
		}
		incr h [image height $img]
	    }
	}
    }
    method _events { event args } {
	
	switch $event {
	    Stroke {
		# nothing by now
	    }
	    CursorDown {
		# nothing by now
	    }
	    CursorButtonDown {
		foreach "cursor button" $args break
		if { [$button Name] eq "Barrel Switch" } {
		    $overlay EditingMode 2
		}
	    }
	    CursorButtonUp {
		foreach "cursor button" $args break
		if { [$button Name] eq "Barrel Switch" } {
		    $overlay EditingMode 0
		    
		    set rec [tcom::ref createobject "msinkaut.InkRectangle"]
		    $rec SetRectangle 0 0 1000 1000
		    $overlay Draw $rec
		}
	    }
	    CursorInRange {
		set cursor [lindex $args 0]
		if { [$cursor Inverted] } {
		    $overlay EditingMode 1
		} else {
		    $overlay EditingMode 0
		    
		    set rec [tcom::ref createobject "msinkaut.InkRectangle"]
		    $rec SetRectangle 0 0 1000 1000
		    $overlay Draw $rec                }
	    }
	}        
    }
}



################################################################################
#    gestures
################################################################################

namespace eval gestures {
    variable path
    variable callback
}

proc gestures::init { w _callback } {
    variable callback 

    set callback($w) $_callback

    _prepare_for_gestures $w gestures_$w
    bind gestures_$w <ButtonPress-3> [list gestures::press %X %Y]
    bind gestures_$w <B3-Motion> [list gestures::motion %X %Y]
    bind gestures_$w <ButtonRelease-3> [list gestures::release $w %X %Y]
}

proc gestures::add { w wtree } {
    _prepare_for_gestures $wtree gestures_$w
}

proc gestures::addT { wtree } {
    set w [winfo toplevel $wtree]
    _prepare_for_gestures $wtree gestures_$w
}

proc gestures::_prepare_for_gestures { w bindtag } {
    bindtags $w [linsert [bindtags $w] 0 $bindtag]
    foreach i [winfo children $w] { _prepare_for_gestures $i $bindtag }
}

proc gestures::press { x y } {
    variable path

    set path [list $x $y]
}

proc gestures::motion { x y } {
    variable path

    lappend path $x $y
}

proc gestures::release { w x y } {
    variable path
    variable callback

    lappend path $x $y

    set t 10
    foreach "x y" $path {
	if { ![info exists box(minx)] || $x < $box(minx) } { set box(minx) $x }
	if { ![info exists box(miny)] || $y < $box(miny) } { set box(miny) $y }
	if { ![info exists box(maxx)] || $x > $box(maxx) } { set box(maxx) $x }
	if { ![info exists box(maxy)] || $y > $box(maxy) } { set box(maxy) $y }
    }

    if { $box(maxx)-$box(minx) < $t && $box(maxy)-$box(miny) < $t } { return }

    foreach "x0 y0" [lrange $path 0 1] break
    foreach "xend yend" [lrange $path end-1 end] break

    if { $x0-$xend >= $t } {
	set gesture GestureLeft
    } elseif { $xend-$x0 >= $t } {
	set gesture GestureRight
    } elseif { $x0-$box(miny) >= $t && $xend-$box(miny) >= $t } {
	set gesture GestureUp,GestureDown
    } else { return }
   
    uplevel #0 $callback($w) $gesture
    return -code break
}

################################################################################
#    pocketpc
################################################################################

namespace eval pocketpc {
    variable isinit 0
    variable _buttons_data ""
    variable _buttons_after ""
}

proc pocketpc::init {} {
    variable isinit
    
    set isinit 1
    catch { package require tooltip }
    namespace eval ::tooltip {}
    interp alias "" ::tooltip::tooltip "" ::pocketpc::tooltip
    interp alias "" ::tooltip "" ::pocketpc::tooltip
}

proc pocketpc::add { w } {
    variable isinit
    
    if { !$isinit } { return }
    
    set cmds [list \
	    [list <ButtonPress-1> BP1] \
	    [list <B1-Motion> BM1] \
	    [list <ButtonRelease-1> BR1] \
	    [list <Double-1> D1] \
	    ]
    
    foreach i $cmds {
	foreach "ev cmd" $i break
	set cmd0 [bind $w $ev]
	set cmd [namespace code [list _buttons $w $cmd %x %y %X %Y]]
	bind $w $ev "$cmd ; $cmd0"
    }
    
    bind $w <<Right>>  [list $w xview scroll 1 page]
    bind $w <<Left>>  [list $w xview scroll -1 page]
}

proc pocketpc::release { w } {
    variable isinit
    
    if { !$isinit } { return }
    
    set cmds [list \
	    [list <ButtonPress-1> BP1] \
	    [list <B1-Motion> BM1] \
	    [list <ButtonRelease-1> BR1] \
	    [list <Double-1> D1] \
	    ]
    
    foreach i $cmds {
	foreach "ev cmd" $i break
	bind $w $ev ""
    }
}

proc pocketpc::tooltip { w txt } {
    bind $w <ButtonPress-1> +[namespace code [list _tooltip_do $w $txt BP1]]
    bind $w <ButtonRelease-1> +[namespace code [list _tooltip_do $w $txt BR1]]
}

proc pocketpc::_tooltip_do { w txt what } {
    variable tooltip_do_afterid
    
    switch $what {
	BP1 { set tooltip_do_afterid [after 1000 [namespace code [list _tooltip_do $w $txt after]]] }
	BR1 {
	    if { [info exists tooltip_do_afterid] } {
		after cancel $tooltip_do_afterid
	    } else {
		destroy $w.b_tooltip
		set err [catch { $w cget -command } cmd]
		if { !$err && $cmd ne "" } {
		    $w configure -command ""
		    set cmd1 [bind [winfo class $w] <ButtonRelease-1>]
		    set cmd1 [string map [list %W $w] $cmd1]
		    uplevel #0 $cmd1
		    $w configure -command $cmd
		}
		# dirty trick to avoid the command to be executed
		set ::tk::Priv(repeated) 1
		return -code break
	    }
	    unset -nocomplain tooltip_do_afterid
	}
	after {
	    destroy $w.b_tooltip
	    set b [toplevel $w.b_tooltip -relief solid -bd 1 -bg white]
	    wm overrideredirect $b 1
	    pack [label $b.l -text $txt -bg white -bd 0]
	    update idletasks
	    
	    set x [expr {[winfo rootx $w]+([winfo width $w]-[winfo reqwidth $b])/2}]
	    if { $x < 0 } { set x 0 }
	    if { $x+[winfo reqwidth $b]>[winfo screenwidth $w]} {
		set x [expr {[winfo screenwidth $w]-[winfo reqwidth $b]}]
	    }
	    if { [winfo rooty $w] > .5*[winfo screenheight $w] } {
		set y [expr {[winfo rooty $w]-[winfo reqheight $b]-5}]
	    } else {
		set y [expr {[winfo rooty $w]+[winfo height $w]+5}]
	    }
	    wm geometry $b +$x+$y
	    bind all <ButtonRelease-1> "destroy $b ; bind all <ButtonRelease-1> {}"
	    bind all <B1-Motion> "destroy $b ; bind all <B1-Motion> {}"
	    unset -nocomplain tooltip_do_afterid
	}
    }
    #         bind Text <Key> {if {%k==0 && "%A" eq "\r"} break }
    #         bind Entry <Key> {if {%k==0 && "%A" eq "\r"}  break }
}


proc pocketpc::_buttons { w what args } {
    variable _buttons_data
    variable _buttons_after
    
    foreach "x y X Y" $args break
    switch $what {
	BP1 {
	    _buttons $w cancel
	    set _buttons_data [list $x $y $X $Y -3 [clock clicks -milliseconds]]
	    set _buttons_after [after 100 [namespace code [list _buttons $w after]]]
	    return -code break
	}
	cancel {
	    set ballcount 0
	    foreach  "- - - - ballcount" $_buttons_data break
	    set _buttons_data ""
	    after cancel $_buttons_after
	    set _buttons_after ""
	    for {set n  0} {$n < $ballcount} {incr n} {
		destroy .tapandholdball_$n
	    }
	}
	D1 {
	    _buttons $w cancel
	}
	BM1 - BR1 {
	    if { $_buttons_data eq "BM1" } {
		return
	    } elseif { $_buttons_data eq "" } {
		return -code break
	    }
	    foreach "xold yold - - - told" $_buttons_data break
	    set t [clock clicks -milliseconds]
	    if { $what eq "BM1" } {
		if { abs($x-$xold)<5 || $t-$told < 200 } { return -code break }
	    }
	    _buttons $w cancel
	    
	    if { $what eq "BM1" } {
		set _buttons_data BM1
	    }
	    set r [expr {sqrt(($x-$xold)*($x-$xold)+($y-$yold)*($y-$yold))}]
	    set angle [expr {atan2($yold-$y,$x-$xold)}]

	    if { $what eq "BR1" && $r > 10 } {
		set pi 3.141592653589793
		if { $angle < 0.0 } { set angle [expr {2.0*$pi+$angle}] }
		set zone [expr {round(8*($angle)/(2.0*$pi))}]
		if { $zone > 7 } { set zone 0 }
		
		set events [list \
		        [list <<Left>> [_ "Scroll left"]] \
		        [list <Control-c> [_ Copy]] \
		        [list <Next> [_ Down]] \
		        [list <Delete> [_ Delete]] \
		        [list <<Right>> [_ "Scroll right"]] \
		        [list <Control-z> [_ Undo]] \
		        [list <Prior> [_ Up]] \
		        [list <Control-v> [_ Paste]] \
		        ]
		
		#[list <Alt-Right> [_ Forward]]
		#[list <Alt-Left> [_ Backward]]
		
		event generate $w [lindex $events $zone 0]

		destroy $w.top
		toplevel $w.top
		wm overrideredirect $w.top 1
		catch { wm attributes $w.top -alpha 1 }
		label $w.top.t -text [lindex $events $zone 1] -font "-size 10 -weight bold" \
		    -bd 1 -relief solid -padx 1 -pady 0
		$w.top.t configure -background [$w cget -background]
		pack $w.top.t
		set x [expr {[winfo rootx $w]+[winfo width $w]-[winfo reqwidth $w.top.t]}]
		set y [expr {[winfo rooty $w]}]
		wm geometry $w.top +$x+$y
		#wm geometry $w.top +[expr {$X-20}]+[expr {$Y-20}]

		bind $w.top <1> [list destroy $w.top]
		after 300 [list destroy $w.top]
	    } else {
		set map [list %W $w %x $xold %y $yold]
		set cmd [string map $map [bind [winfo class $w] <ButtonPress-1>]]
		uplevel #0 $cmd
	    }
	}
	after {
	    if { $_buttons_data eq "" } { return }
	    if { $_buttons_data eq "BM1" } { return }
	    foreach "x y X Y ballcount told" $_buttons_data break
	    if { $ballcount < 8 } {
		if {$ballcount > -1} {
		    set angle [expr {2.0*acos(-1)*$ballcount/8} ]
		    set dx [expr {int(16*sin($angle))}]
		    set dy [expr {int(-16*cos($angle))}]
		    destroy .tapandholdball_$ballcount
		    toplevel .tapandholdball_$ballcount -bg lightblue
		    wm overrideredirect .tapandholdball_$ballcount 1
		    wm geometry .tapandholdball_$ballcount \
		        5x5+[expr {$X+$dx}]+[expr {$Y+$dy}]
		}
		set _buttons_after [after 50 [namespace code \
		            [list _buttons $w after $x $y $X $Y]]]
		incr ballcount
		set _buttons_data [list $x $y $X $Y $ballcount $told]
		return
	    }
	    set _buttons_data ""
	    for {set n  0} {$n < $ballcount} {incr n} {
		destroy .tapandholdball_$n
	    }
	    if { [bind $w <<Contextual>>] ne "" } {
		event generate $w <<Contextual>> -x $x -y $y -rootx $X -rooty $Y                
	    } else {
		event generate $w <ButtonRelease-3> -x $x -y $y -rootx $X -rooty $Y
	    }
	}
    }
}







