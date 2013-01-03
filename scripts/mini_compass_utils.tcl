
package require snit

proc info_fullargs { procname } {
    set ret ""
    foreach arg [uplevel 1 [list info args $procname]] {
	if { [uplevel 1 [list info default $procname $arg value]] } {
	    upvar 1 value value
	    lappend ret [list $arg $value]
	} else {
	    lappend ret $arg
	}
    }
    return $ret
}

namespace eval cu {}
namespace eval cu::file {}

# for tclIndex to work 
proc cu::menubutton_button { args } {}

snit::widgetadaptor cu::menubutton_button {
    option -command ""
    option -image ""
    option -text ""

    delegate method * to hull
    delegate option * to hull
    delegate option -_image to hull as -image
    delegate option -_text to hull as -text

    variable xmin
    variable is_button_active 1
    variable press_after ""
    
    constructor args {
	installhull using ttk::menubutton -style Toolbutton
	bind $win <ButtonPress-1> [mymethod BP1 %x %y]
	bind $win <ButtonRelease-1> [mymethod BR1 %x %y]
	bind $win <Down> [list ttk::menubutton::Popdown %W]
	bind $win <Motion> [mymethod check_cursor %x %y]
	bind $win <Configure> [mymethod  _calc_xmin]

	$self configurelist $args
    }
    onconfigure -image {img} {
	set options(-image) $img

	if { $options(-text) ne "" } {
	    $self configure -_image $img
	    return
	} 
	set new_img [cu::add_down_arrow_to_image $img]
	$self configure -_image $new_img
	bind $win <Destroy> +[list image delete $new_img]
    }
    onconfigure -text {value} {
	set options(-text) $value

	if { $options(-text) ne "" } {
	    $self configure -style ""
	    if { $options(-image) ne "" } {
		$self configure -_image $options(-image)
	    }
	}
	$self configure -_text $value
    }
    method _calc_xmin {} {
	if { [winfo width $win] > 1 } {
	    set xmin  [expr {[winfo width $win]-12}]
	} else {
	    set xmin  [expr {[winfo reqwidth $win]-12}]
	}
    }
    method give_is_button_active_var {} {
	return [myvar is_button_active]
    }
    method BP1 { x y } {
	if { !$is_button_active } { return }
	
	if { $x < $xmin && $options(-command) ne "" } {
	    $win instate !disabled {
		catch { tile::clickToFocus $win }
		catch { ttk::clickToFocus $win }
		$win state pressed
	    }
	    set press_after [after 700 [mymethod BP1_after]]
	    return -code break
	}
    }
    method BP1_after {} {
	set press_after ""
	$win instate {pressed !disabled} {
	    ttk::menubutton::Pulldown $self
	}
    }
    method BR1 { x y } {
	if { !$is_button_active } { return }
	
	if { $press_after ne "" } {
	    after cancel $press_after
	}
	if { $press_after ne "" && $x < $xmin && $options(-command) ne "" } {
	    $win instate {pressed !disabled} {
		$win state !pressed
		uplevel #0 $options(-command)
	    }
	    set press_after ""
	    return -code break
	}
	set press_after ""
    }
    method check_cursor { x y } {
	if { $x < $xmin } {
	    $win configure -cursor ""
	} else {
	    $win configure -cursor bottom_side
	}
    }
}

snit::widgetadaptor cu::combobox {
    option -valuesvariable ""
    option -textvariable ""
    option -statevariable ""
    option -values ""
    option -dict ""
    option -dictvariable ""

    variable _translated_textvariable ""

    delegate method * to hull
    delegate option * to hull
    delegate option -_values to hull as -values
    delegate option -_textvariable to hull as -textvariable

    constructor args {
	installhull using ttk::combobox

	cu::add_contextual_menu_to_entry $win init
	bind $win <<ComboboxSelected>> [mymethod combobox_selected]
	$self configurelist $args
    }
    destructor {
	catch {
	    if { $options(-valuesvariable) ne "" } {
		upvar #0 $options(-valuesvariable) v
		trace remove variable v write "[mymethod _changed_values_var];#"
	    }
	    if { $options(-dictvariable) ne "" } {
		upvar #0 $options(-dictvariable) v
		trace remove variable v write "[mymethod _changed_values_var];#"
	    }
	    if { $options(-textvariable) ne "" } {
		upvar #0 $options(-textvariable) v
		trace remove variable v write "[mymethod _written_textvariable];#"
	    }
	    if { $options(-statevariable) ne "" } {
		upvar #0 $options(-statevariable) v
		trace remove variable v write "[mymethod _written_statevariable];#"
		trace remove variable v read "[mymethod _read_statevariable];#"
	    }
	}
    }
    onconfigure -textvariable {value} {
	set options(-textvariable) $value
	$self configure -_textvariable [myvar _translated_textvariable]

	upvar #0 $options(-textvariable) v
	trace add variable v write "[mymethod _written_textvariable];#"
	trace add variable [myvar _translated_textvariable] write \
	    "[mymethod _read_textvariable];#"
	if { [info exists v] } {
	    $self _written_textvariable
	}
    }
    onconfigure -dictvariable {value} {
	set options(-dictvariable) $value
	$self _changed_values_var
	upvar #0 $options(-dictvariable) v
	trace add variable v write "[mymethod _changed_values_var];#"
    }
    onconfigure -statevariable {value} {
	set options(-statevariable) $value

	upvar #0 $options(-statevariable) v
	trace add variable v write "[mymethod _written_statevariable];#"
	trace add variable v read "[mymethod _read_statevariable];#"
	if { [info exists v] } {
	    set v $v
	}
    }
    onconfigure -valuesvariable {value} {
	set options(-valuesvariable) $value

	upvar #0 $options(-valuesvariable) v

	if { $options(-dictvariable) ne "" } {
	    upvar #0 $options(-dictvariable) vd
	    if { [info exists vd] } {
		set dict $vd
	    } else {
		set dict ""
	    }
	} else {
	    set dict $options(-dict)
	}
	if { ![info exists v] } {
	    set v ""
	    foreach value [$self cget -_values] {
		catch { 
		    set value [dict get [dict_inverse $dict] $value]
		}
		lappend v $value
	    }
	} else {
	    set vtrans ""
	    foreach value $v {
		catch { set value [dict get $dict $value] }
		lappend vtrans $value
	    }
	    $self configure -_values $vtrans
	}
	trace add variable v write "[mymethod _changed_values_var];#"
    }
    onconfigure -dict {value} {
	set options(-dict) $value
	$self _changed_values_var
    }
    onconfigure -values {values} {
	if { $options(-valuesvariable) ne "" } {
	    upvar #0 $options(-valuesvariable) v
	    set v $values
	} else {
	    if { $options(-dictvariable) ne "" } {
		upvar #0 $options(-dictvariable) vd
		if { [info exists vd] } {
		    set dict $vd
		} else {
		    set dict ""
		}
	    } else {
		set dict $options(-dict)
	    }
	    set vtrans ""
	    foreach value $values {
		catch { set value [dict get $dict $value] }
		lappend vtrans $value
	    }
	    $self configure -_values $vtrans
	}
    }
    oncget -values {
	set v ""
	foreach value [$self cget -_values] {
#             catch {
#                 set value [dict get [dict_inverse $options(-dict)] $value]
#             }
	    lappend v $value
	}
	return $v
    }
    method _changed_values_var {} {
	if { $options(-valuesvariable) ne "" } {
	    upvar #0 $options(-valuesvariable) v
	} else {
	    set v [$self cget -values]
	}
	if { $options(-dictvariable) ne "" } {
	    upvar #0 $options(-dictvariable) vd
	    if { [info exists vd] } {
		set dict $vd
	    } else {
		set dict ""
	    }
	} else {
	    set dict $options(-dict)
	}
	set vtrans ""
	foreach value $v {
	    catch { set value [dict get $dict $value] }
	    lappend vtrans $value
	}
	$self configure -_values $vtrans
	$self _written_textvariable
    }
    method _written_textvariable { args } {

	set optional {
	    { -force_dict "" 0 }
	}
	set compulsory ""
	parse_args $optional $compulsory $args

	upvar #0 $options(-textvariable) v
	if { ![info exists v] } { return }
	set value $v
	if { $options(-dictvariable) ne "" } {
	    upvar #0 $options(-dictvariable) vd
	    if { [info exists vd] } {
		set dict $vd
	    } else {
		set dict ""
	    }
	} else {
	    set dict $options(-dict)
	}
	if { $force_dict || [$self instate readonly] } {
	    catch { set value [dict get $dict $value] }
	}
	if { $_translated_textvariable ne $value } {
	    set _translated_textvariable $value
	}
    }
    method _read_textvariable {} {
	upvar #0 $options(-textvariable) v
	set value $_translated_textvariable
	if { $options(-dictvariable) ne "" } {
	    upvar #0 $options(-dictvariable) vd
	    if { [info exists vd] } {
		set dict $vd
	    } else {
		set dict ""
	    }
	} else {
	    set dict $options(-dict)
	}
	catch {
	    set value [dict get [dict_inverse $dict] $value]
	}
	if { ![info exists v] || $v ne $value } {
	    set v $value
	}
    }
    method _written_statevariable {} {
	upvar #0 $options(-statevariable) v
	$self state $v
    }
    method _read_statevariable {} {
	upvar #0 $options(-statevariable) v
	set v [$self state]
    }
    method combobox_selected {} {
	if { ![$self instate readonly] } {
	    $self _written_textvariable -force_dict
	}
    }
}

################################################################################
# cu::multiline_entry
################################################################################

snit::widget cu::multiline_entry {
    option -textvariable ""
    option -takefocus 0 ;# option used by the tab standard bindings
    option -values ""
    option -valuesvariable ""

    hulltype frame

    variable text

    delegate method * to text
    delegate option * to text

    constructor args {

	$hull configure -background #a4b97f -bd 0
	install text using text $win.t -wrap word -bd 0 -width 40 -height 3
	
	cu::add_contextual_menu_to_entry $text init

	grid $text -padx 1 -pady 1 -sticky nsew
	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 0 -weight 1

	bind $text <Tab> "[bind all <Tab>] ; break"
	bind $text <<PrevWindow>> "[bind all <<PrevWindow>>] ; break"
	bindtags $text [list $win $text [winfo class $win] [winfo class $text] [winfo toplevel $text] all]
	bind $win <FocusIn> [list focus $text]
	$self configurelist $args
    }
    destructor {
	$self _clean_traces
    }
    onconfigure -textvariable {value} {
	$self _clean_traces
	set options(-textvariable) $value

	set cmd "[mymethod _check_textvariable_read] ;#"
	trace add variable $options(-textvariable) read $cmd
	set cmd "[mymethod _check_textvariable_write] ;#"
	trace add variable $options(-textvariable) write $cmd
    }
    onconfigure -values {value} {
	set options(-values) $value
	
	if { $options(-values) ne "" || $options(-valuesvariable) ne "" } {
	    if { ![winfo exists $win.b] } {
		image create photo cu::multiline_entry::nav1downarrow16 -data {
		    R0lGODlhEAAQAIAAAPwCBAQCBCH5BAEAAAAALAAAAAAQABAAAAIYhI+py+0PUZi0zmTtypflV0Vd
		    RJbm6fgFACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29y
		    IDE5OTcsMTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29t
		    ADs=
		}
		ttk::menubutton $win.b -image cu::multiline_entry::nav1downarrow16 -style Toolbutton -menu $win.b.m
		menu $win.b.m -tearoff 0
		grid $win.b -row 0 -column 1 -padx "0 1" -pady 1 -sticky wns
	    } else {
		$win.b.m delete 0 end
	    }
	    $win.b.m add command -label [_ "(Clear)"] -command [mymethod set_text ""]
	    $win.b.m add separator
	    foreach v $value {
		if { [string length $v] > 60 } {
		    set l [string range $v 0 56]...
		} else {
		    set l $v
		}
		$win.b.m add command -label $l -command [mymethod set_text $v]
	    }
	} elseif { ![winfo exists $win.b] } {
	    destroy $win.b
	}
    }
    onconfigure -valuesvariable {value} {
	set options(-valuesvariable) $value

	upvar #0 $options(-valuesvariable) v

	if { [info exists v] } {
	    $self configure -values $v
	}
	trace add variable v write "[mymethod _changed_values_var];#"
    }
    method set_text { txt } {
	$text delete 1.0 end
	$text insert end $txt
	$text tag add sel 1.0 end-1c
	focus $text
    }
    method _clean_traces {} {
	if { $options(-textvariable) ne "" } {
	    set cmd "[mymethod _check_textvariable_read] ;#"
	    trace remove variable $options(-textvariable) read $cmd
	    set cmd "[mymethod _check_textvariable_write] ;#"
	    trace remove variable $options(-textvariable) write $cmd
	}
	if { $options(-valuesvariable) ne "" } {
	    upvar #0 $options(-valuesvariable) v
	    trace remove variable v write "[mymethod _changed_values_var];#"
	}
    }
    method _check_textvariable_read {} {
	upvar #0 $options(-textvariable) v
	set v [$text get 1.0 end-1c]
    }
    method _check_textvariable_write {} {
	upvar #0 $options(-textvariable) v
	$text delete 1.0 end
	$text insert end $v
    }
    method _changed_values_var {} {
	if { $options(-valuesvariable) ne "" } {
	    upvar #0 $options(-valuesvariable) v
	    $self configure -values $v
	}
    }
}

################################################################################
#    cu::adapt_text_length
################################################################################

# remember to grid the label to fill all space. For example with -sticky ew
proc cu::adapt_text_length { args } {
    foreach w $args {
	bind $w <Configure> [list cu::_adapt_text_length_do $w]
    }
}

proc cu::_adapt_text_length_do { w } {
    if { [winfo width $w] > 1 } {
	$w configure -wraplength [winfo width $w] -justify left
    }
}

################################################################################
#    add_contextual_menu_to_entry
################################################################################

proc cu::add_contextual_menu_to_entry { w what args } {
    switch $what {
	init {
	    bind $w <ButtonRelease-3> [list cu::add_contextual_menu_to_entry $w post %X %Y]
	}
	post {
	    lassign $args x y
	    set menu $w.menu
	    catch { destroy $menu }
	    menu $menu -tearoff 0
	    foreach i [list cut copy paste --- select_all --- clear] \
		txt [list [_ "Cut"] [_ "Copy"] [_ "Paste"] --- [_ "Select all"] --- [_ "Clear"]] {
		if { $i eq "---" } {
		    $menu add separator
		} else {
		    $menu add command -label $txt -command [list cu::add_contextual_menu_to_entry $w $i]
		}
	    }
	    tk_popup $menu $x $y
	}
	clear {
	    if { [winfo class $w] eq "Text" } {
		$w delete 1.0 end
	    } else {
		$w delete 0 end
	    }
	}
	cut {
	    event generate $w <<Cut>>
	}
	copy {
	    event generate $w <<Copy>>
	}
	paste {
	    event generate $w <<Paste>>
	}
	select_all {
	    if { [winfo class $w] eq "Text" } {
		$w tag add sel 1.0 end-1c
	    } else {
		$w selection range 0 end
	    }
	}
    }
}

################################################################################
#     cu::text_entry_bindings
################################################################################

proc cu::text_entry_bindings { w } {

    if { ![info exists ::control] } {
	if { $::tcl_platform(platform) eq "windows" } {
	    set ::control Control
	} elseif { [tk windowingsystem] eq "aqua" } {
	    set ::control Command
	} else {
	    set ::control Control
	}
    }
    # "backslash" and "c" are here to help with a problem in Android VNC
    bind $w <$::control-backslash> "[list cu::text_entry_insert $w];break"
    bind $w <$::control-less> "[list cu::text_entry_insert $w];break"
    foreach "acc1 acc2 c" [list plus "" {[]} c "" {{}} ccedilla "" {{}} 1 "" || 1 1 \\ 3 "" {#}] {
	set cmd "[list cu::text_entry_insert $w $c];break"
	if { $acc2 eq "" } {
	set k2 ""
    } else {
	set k2 <KeyPress-$acc2>
    }
    bind $w <$::control-less><KeyPress-$acc1>$k2 $cmd
    bind $w <$::control-backslash><KeyPress-$acc1>$k2 $cmd
    }
    if { $::tcl_platform(platform) ne "windows" } {
	foreach "ev k" [list braceleft \{ braceright \} bracketleft \[ bracketright \] backslash \\ \
		bar | at @ numbersign # asciitilde ~ EuroSign €] {
	    bind $w <$ev> "[list tk::TextInsert $w $k]"
	}
    }
}

proc cu::text_entry_insert { w { what "" } } {
    variable last_text_enty_bindings
    
    if { ![info exists last_text_enty_bindings] } {
	set last_text_enty_bindings ""
    }
    set list [list "{}" "\[\]" "||" "\\" "#"]
    set t [clock milliseconds]
    lassign [dict_getd $last_text_enty_bindings $w ""] time d
    
    if { $d eq "" } { set d "{}" }
    if { $time ne "" && $t < $time+3000 } {
	if { [winfo class $w] eq "Text" } {
	    set idx [$w search $d insert-1c]
	    if { [$w compare $idx == insert-1c] } {
		if { [string length $d] == 1 } {
		    $w delete insert-1c
		} else {
		    $w delete insert-1c insert+1c
		}
	    }
	} else {
	    set idx [$w index insert]
	    if { $idx > 0 } {
		set idx1 [expr {$idx-1}]
		set idx2 [expr {$idx-1+[string length $d]}]
		set txt [string range [$w get] $idx1 $idx2]
		if { [string equal $d $txt] } {
		    $w delete $idx1 $idx2
		}
	    }
	}
	if { $what eq "" } {
	    set ipos [lsearch -exact $list $d]
	    incr ipos
	    if { $ipos >= [llength $list] } {
		set ipos 0
	    }
	    set d [lindex $list $ipos]
	}
    }
    if { $what ne "" } {
	set d $what
    }
    if { [winfo class $w] eq "Text" } {
	set idx [$w index insert]
	$w insert insert $d
	$w mark set insert "$idx+1c"
    } else {
	set idx [$w index insert]
	$w insert insert $d
	$w icursor [expr {$idx+1}]
    }
    dict set last_text_enty_bindings $w [list $t $d]
}


################################################################################
#    store preferences
################################################################################

proc cu::store_program_preferences { args } {

    set optional {
	{ -valueName name "" }
    }
    set compulsory "program_name data"

    parse_args $optional $compulsory $args

    if { $valueName eq "" } {
	set valueNameF IniData
    } else {
	set valueNameF IniData_$valueName
    }

    if { $::tcl_platform(platform) eq "windows" && $::tcl_platform(os) ne "Windows CE" } {
	set key "HKEY_CURRENT_USER\\Software\\Compass\\$program_name"
	package require registry
	registry set $key $valueNameF $data
    } else {
	package require tdom
	if { $::tcl_platform(os) eq "Windows CE" } {
	    set dir [file join / "Application Data" Compass $program_name]
	    file mkdir $dir
	    set file [file join $dir prefs]
	} elseif { [info exists ::env(HOME)] } {
	    set file [file normalize ~/.compass_${program_name}_prefs]
	} else {
	    set file [file normalize [file join /tmp compass_${program_name}_prefs]]
	}
	set err [catch { tDOM::xmlReadFile $file } xml]
	if { $err } { set xml "<preferences/>" }
	set doc [dom parse $xml]
	set root [$doc documentElement]
	set domNode [$root selectNodes "pref\[@n=[xpath_str $valueNameF]\]"]
	if { $domNode ne "" } { $domNode delete }
	set p [$root appendChildTag pref]
	$p setAttribute n $valueNameF
	$p appendChildText $data

	set fout [open $file w]
	fconfigure $fout -encoding utf-8
	puts $fout [$doc asXML]
	close $fout
    }
}
proc cu::get_program_preferences { args } {

    set optional {
	{ -valueName name "" }
	{ -default default_value "" }
    }
    set compulsory "program_name"

    parse_args $optional $compulsory $args

    if { $valueName eq "" } {
	set valueNameF IniData
    } else {
	set valueNameF IniData_$valueName
    }

    set data $default
    if { $::tcl_platform(platform) eq "windows" && $::tcl_platform(os) ne "Windows CE" } {
	set key "HKEY_CURRENT_USER\\Software\\Compass\\$program_name"
	package require registry
	set err [catch { registry get $key $valueNameF } data]
	if { $err } {
	    set data $default
	}
    } else {
	package require tdom
	if { $::tcl_platform(os) eq "Windows CE" } {
	    set dir [file join / "Application Data" Compass $program_name]
	    file mkdir $dir
	    set file [file join $dir prefs]
	} elseif { [info exists ::env(HOME)] } {
	    set file [file normalize ~/.compass_${program_name}_prefs]
	} else {
	    set file [file normalize [file join /tmp compass_${program_name}_prefs]]
	}
	set err [catch { tDOM::xmlReadFile $file } xml]
	if { !$err } {
	    set doc [dom parse $xml]
	    set root [$doc documentElement]
	    set domNode [$root selectNodes "pref\[@n=[xpath_str $valueNameF]\]"]
	    if { $domNode ne "" } {
		set data [$domNode text]
	    }
	}
    }
    return $data
}

################################################################################
#    cu::set_window_geometry u::give_window_geometry
################################################################################

proc cu::give_window_geometry { w } {

    regexp {(\d+)x(\d+)([-+])([-\d]\d*)([-+])([-\d]+)} [wm geometry $w] {} width height m1 x m2 y
    if { $::tcl_platform(platform) eq "unix" } {
	# note: this work in ubuntu 9.04
	incr x -4
	incr y -24
    }
    return ${width}x$height$m1$x$m2$y
}

proc cu::set_window_geometry { w geometry } {

    if { ![regexp {(\d+)x(\d+)([-+])([-\d]\d*)([-+])([-\d]+)} $geometry {} width height m1 x m2 y] } {
	regexp {(\d+)x(\d+)} $geometry {} width height
	lassign [list 0 0 + +] x y m1 m2
    }
    if { $x < 0 } { set x 0 }
    if { $y < 0 } { set y 0 }
    if { $x > [winfo screenwidth $w]-100 } { set x [expr {[winfo screenwidth $w]-100}] }
    if { $y > [winfo screenheight $w]-100 } { set y [expr {[winfo screenheight $w]-100}] }

    wm geometry $w ${width}x$height$m1$x$m2$y
}

proc cu::create_tooltip_toplevel { b } {

    toplevel $b -class Tooltip
    if {[tk windowingsystem] eq "aqua"} {
	::tk::unsupported::MacWindowStyle style $b help none
    } else {
	wm overrideredirect $b 1
    }
    catch {wm attributes $b -topmost 1}
    # avoid the blink issue with 1 to <1 alpha on Windows
    catch {wm attributes $b -alpha 0.99}
    wm positionfrom $b program
    if { [tk windowingsystem]  eq "x11" } {
	set focus [focus]
	focus -force $b
	raise $b
	if { $focus ne "" } {
	    after 100 [list focus $focus]
	}
    }
}

proc cu::give_widget_background { w } {
 
    set err [catch { $w cget -background } bgcolor]
    if { $err } {
	set err [catch {
		set style [$w cget -style]
		if { $style eq "" } {
		    set style [winfo class $w]
		}
		set bgcolor [ttk::style lookup $style -background]
	    }]
	if { $err } {
	    if { $::tcl_platform(platform) eq "windows" } {
		set bgcolor SystemButtonFace
	    } else {
		set bgcolor grey
	    }
	}
    }
   return $bgcolor
}

################################################################################
#    add_down_arrow_to_image
################################################################################

proc cu::add_down_arrow_to_image { args } {
    variable add_down_arrow_to_image_delta
    
    set optional {
	{ -color color black }
	{ -w widget "" }
    }
    set compulsory "img"
    parse_args $optional $compulsory $args

    if {![info exists add_down_arrow_to_image_delta] } {
	set add_down_arrow_to_image_delta 7
    }
    if { $img ne "" } {
	set width [image width $img]
	set height [image height $img]
    } else {
	lassign [list 0 16] width height
    }
    set new_img [image create photo -width [expr {$width+$add_down_arrow_to_image_delta}] -height $height]
    if { $img ne "" } { $new_img copy $img -to 0 0 }
    set coords {
	-3 -1
	-4 -2 -3 -2 -2 -2
	-5 -3 -4 -3 -3 -3 -2 -3 -1 -3
    }
    foreach "x y" $coords {
	$new_img put $color -to [expr {$width+$add_down_arrow_to_image_delta+$x}] [expr {$height+$y}]
    }
    if { $w ne "" } {
	$w configure -image $new_img
	bind $w <Destroy> +[list image delete $new_img]
    }
    return $new_img
}

################################################################################
#    XML & xpath utilities
################################################################################

proc xpath_str { str } {
    
    foreach "strList type pos" [list "" "" 0] break
    while 1 {
	switch $type {
	    "" {
		set ret [regexp -start $pos -indices {['"]} $str idxs]
		if { !$ret } {
		    lappend strList "\"[string range $str $pos end]\""
		    break
		}
		set idx [lindex $idxs 0]
		switch -- [string index $str $idx] {
		    ' { set type apostrophe }
		    \" { set type quote }
		}
	    }
	    apostrophe {
		set ret [regexp -start $pos -indices {["]} $str idxs]
		if { !$ret } {
		    lappend strList "\"[string range $str $pos end]\""
		    break
		}
		set idx [lindex $idxs 0]
		lappend strList "\"[string range $str $pos [expr {$idx-1}]]\""
		set type quote
		set pos $idx
	    }
	    quote {
		set ret [regexp -start $pos -indices {[']} $str idxs]
		if { !$ret } {
		    lappend strList "'[string range $str $pos end]'"
		    break
		}
		set idx [lindex $idxs 0]
		lappend strList "'[string range $str $pos [expr {$idx-1}]]'"
		set type apostrophe
		set pos $idx
	    }
	}
    }
    if { [llength $strList] > 1 } {
	return "concat([join $strList ,])"
    } else {
	return [lindex $strList 0]
    }
}

proc format_xpath { string args } {
    set cmd [list format $string]
    foreach i $args {
	lappend cmd [xpath_str $i]
    }
    return [eval $cmd]
}

namespace eval ::dom::domNode {}

# args can be one or more tags
proc ::dom::domNode::appendChildTag { node args } {
    if { [::llength $args] == 0 } {
	error "error in appendChildTag. At list one tag"
    }
    ::set doc [$node ownerDocument]
    foreach tag $args {
	if { [string match "text() *" $tag] } {
	    ::set newnode [$doc createTextNode [lindex $tag 1]]
	    $node appendChild $newnode
	    ::set node $newnode
	} elseif { [string match "attributes() *" $tag] } {
	    foreach "n v" [lrange $tag 1 end] {
		$node setAttribute $n $v
	    }
	} else {
	    ::set newnode [$doc createElement $tag]
	    $node appendChild $newnode
	    ::set node $newnode
	}
    }
    return $newnode
}

proc ::dom::domNode::appendChildText { node text } {
    ::set doc [$node ownerDocument]
    foreach child [$node selectNodes text()] { $child delete }
    ::set newnode [$doc createTextNode $text]
    $node appendChild $newnode
    return $newnode
}

proc dict_getd { args } {
    
    set dictionaryValue [lindex $args 0]
    set keys [lrange $args 1 end-1]
    set default [lindex $args end]
    if { [dict exists $dictionaryValue {*}$keys] } {
	return [dict get $dictionaryValue {*}$keys]
    }
    return $default
}

proc linsert0 { args } {
    set optional {
	{ -max_len len "" }
    }
    set compulsory "list element"
    parse_args $optional $compulsory $args

    set ipos [lsearch -exact $list $element]
    if { $ipos != -1 } {
	set list [lreplace $list $ipos $ipos]
    }
    set list [linsert $list 0 $element]
    if { $max_len ne "" } {
	set list [lrange $list 0 $max_len]
    }
    return $list
}

################################################################################
#     cu::file::execute, cu::kill and cu::ps
################################################################################

proc cu::kill { pid } {

    if { $::tcl_platform(platform) eq "windows" } {
	package require compass_utils::c
	return [cu::_kill_win $pid]
    } else {
	exec kill $pid 
    }
}

proc cu::ps { args } {

    if { $::tcl_platform(platform) eq "windows" } {
	package require compass_utils::c
	set ps_args ""
	foreach i $args {
	    if { $i eq "" } { continue }
	    if { ![regexp {^\*} $i] } {
		set i "*$i"
	    }
	    if { ![regexp {\*$} $i] } {
		set i "$i*"
	    }
	    lappend ps_args $i
	}
	set ret [cu::_ps_win {*}$ps_args]
	catch { package require twapi }
	set retret ""
	foreach i $ret {
	    lassign $i cmd pid
	    if { [info command ::twapi::get_process_info] ne "" } {
		set d [twapi::get_process_info $pid -createtime -privilegedtime -workingset]
		set stime [clock format [twapi::large_system_time_to_secs [dict get $d -createtime]] \
		        -format "%H:%M:%S"]
		set cputime [clock format [twapi::large_system_time_to_secs \
		            [dict get $d -privilegedtime]] -format "%H:%M:%S" -timezone :UTC]
		set size [expr {[dict get $d -workingset]/1024}]
		set i [list $cmd $pid $stime $cputime $size]
	    }
	    lappend retret $i
	}
	return $retret
    } else {
	# does not do exactly the same than in Windows
	#set err [catch { exec pgrep -l -f [lindex $args 0] } ret]
	#set retList  [split $ret \n]
	lassign $args pattern
	if { $pattern eq "" } {
	    set err [catch { exec ps -u $::env(USER) --no-headers -o pid,stime,time,pcpu,size,cmd } ret]
	} elseif { [string is integer -strict $pattern] } {
	    set err [catch { exec ps --pid $pattern --no-headers -o pid,stime,time,pcpu,size,cmd } ret]
	} else {
	    set err [catch { exec ps -u $::env(USER) --no-headers -o pid,stime,time,pcpu,size,cmd | grep -i $pattern } ret]
	}        
	if { $err } {
	    return ""
	} else {
	    set retList ""
	    foreach line [split $ret \n] {
		regexp {(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)} $line {} pid stime cputime \
		    pcpu size cmd
		set pcpu [format "%02.0f%%" $pcpu]
		if { $pattern ne "" && $cmd eq "grep -i $pattern" } { continue }
		lappend retList [list $cmd $pid $stime "$cputime ($pcpu)" $size]
	    }
	    return $retList
	}
    }
}

proc cu::file::correct_name { file } {
    if { $::tcl_platform(platform) eq "windows" } {
	regsub -all {[:*?""<>|]} $file {_} file
    }
    return [string trim $file]
}

proc cu::file::execute { args } {
    
    set optional {
	{ -workdir directory "" }
	{ -wait boolean 0 }
	{ -hide_window boolean 0 }
    }
    set compulsory "what file"

    set args [parse_args -raise_compulsory_error 0 $optional $compulsory $args]

    switch -- $what {
	gid {
	    set exe [get_executable_path gid]
	    if { $exe eq "" } { return }
	    if { $wait || $hide_window } {
		set err [catch { package require twapi }]
		if { $err } { set has_twapi 0 } else { set has_twapi 1 }
	    }
	    if { !$wait || $has_twapi } { lappend args & }
	    set pid [exec $exe $file {*}$args]
	   
	    if { !$wait && !$hide_window } { return }
	    if { !$has_twapi } { return }

	    if { $hide_window } {
		foreach hwin [twapi::find_windows -pids $pid -visible true] {
		    twapi::hide_window $hwin
		}
	    }
	    if { $wait } {
		while { [twapi::process_exists $pid] } {
		    after 200
		}
	    }
	}
	emacs {
	    exec runemacs -g 100x72 &
	}
	wish {
	    set pwd [pwd]
	    cd [file dirname $file]
	    eval exec wish [list [file normalize $file]] $args &
	    cd $pwd
	}
	tkdiff {
	    set pwd [pwd]
	    cd [file dirname $file]
	    exec wish ~/myTclTk/tkcvs/bin/tkdiff.tcl -r [file tail $file] &
	    cd $pwd
	}
	start {
	    if { $::tcl_platform(platform) eq "unix" } {
		set programs [list xdg-open gnome-open]
		if { $::tcl_platform(os) eq "Darwin" } {
		    set programs [linsert $programs 0 open]
		}
		foreach i $programs {
		    if { [auto_execok $i] ne "" } {
		        exec $i $file &
		        return
		    }
		}
		error "could not open file '$file'"
	    } elseif { [regexp {[&]} $file] } {
		set bat [file join [file dirname $file] a.bat]
		set fout [open $bat w]
		puts $fout "start \"\" \"$file\""
		close $fout
		exec $bat 
		file delete $bat
	    } else {
		eval exec [auto_execok start] \"\" [list $file] {*}$args &
	    }
	}
	url {
	    if { [regexp {^[-\w.]+$} $file] } {
		set file http://$file
	    }
	    if { ![regexp {(?i)^\w+://} $file] && ![regexp {(?i)^mailto:} $file] } {
		set txt [_ "url does not begin with a known handler like: %s. Proceed?" \
		        "http:// ftp:// mailto:"]
		set retval [tk_messageBox -default ok -icon question -message $txt \
		        -type okcancel]
		if { $retval == "cancel" } { return }
	    }
	    if { $::tcl_platform(platform) eq "windows" } {
		exec rundll32 url.dll,FileProtocolHandler $file &
	    } else {
		set programs [list xdg-open gnome-open]
		if { $::tcl_platform(os) eq "Darwin" } {
		    set programs [linsert $programs 0 open]
		}
		foreach i $programs {
		    if { [auto_execok $i] ne "" } {
		        exec $i $file &
		        return
		    }
		}
		set cmdList ""
		foreach i [list firefox konqueror mozilla opera netscape] {
		    lappend cmdList "$i \"$file\""
		}
		exec sh -c [join $cmdList "||"] & 
	    }
	}
	exec {
	    if { $workdir ne "" } {
		set pwd [pwd]
		cd $workdir
	    }
	    set err [catch { exec $file {*}$args } errstring]
	    if { $workdir ne "" } { cd $pwd }
	    if { $err } {
		error $errstring $::errorInfo
	    }
	}
	execList {
	    foreach i $file {
		if { [auto_execok [lindex $i 0]] ne "" } {
		    exec {*}$i &
		    return
		}
	    }
	  error "Could not execute files"
	}
	default {
	    if { $workdir ne "" } {
		set pwd [pwd]
		cd $workdir
	    }
	    set err [catch { exec $file {*}$args & } errstring]
	    if { $workdir ne "" } { cd $pwd }
	    if { $err } {
		error $errstring $::errorInfo
	    }
	}
    }  
}