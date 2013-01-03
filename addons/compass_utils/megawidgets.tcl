
#package require tile

catch { namespace import -force ttk::style }

package require snit
package require tooltip
namespace import -force tooltip::tooltip

if {![package vsatisfies [package provide Tk] 8.5]} {
    interp alias "" tk::TabToWindow "" keynav::traverseTo
    if { [info command ttk::style] eq "" } {
	interp alias "" ttk::style "" style
    }
}

namespace eval cu {
    variable topdir [file normalize [file dirname [info script]]]
}

# for tclIndex to work 
proc cu::menubutton_button { args } {}
proc cu::menubutton_checkbutton { args } {}
proc cu::menubutton_tree { args } {}
proc cu::combobox { args } {}
proc cu::combobox_tree { args } {}
proc cu::nicelabel { args } {}
proc cu::dater { args } {}
proc cu::dater_entry { args } {}
proc cu::fullscale { args } {}
proc cu::scale { args } {}
proc cu::multiline_entry { args } {}
proc cu::view_csv { args } {}
proc cu::check_listbox { args } {}
proc cu::menubutton_frame { args } {}
proc cu::menubutton_check_listbox { args } {}
proc cu::notebook { args } {}
proc cu::report_makerT { args } {}
proc cu::report_maker { args } {}
proc cu::scrollframe { args } {}
proc cu::handle { args } {}

# deprecated
proc menubutton_button { args } {}

interp alias "" menubutton_button "" cu::menubutton_button
interp alias "" nicelabel "" cu::nicelabel

snit::widgetadaptor cu::menubutton_button {
    option -command ""
    option -image ""
    option -text ""

    delegate method * to hull
    delegate option * to hull
    delegate option -_image to hull as -image
    delegate option -_text to hull as -text

    variable xmin
    variable is_button_active 1 ; # 0, 1, only_button
    variable press_after ""
    
    constructor args {
	installhull using ttk::menubutton -style Toolbutton
	bind $win <ButtonPress-1> [mymethod BP1 %x %y]
	bind $win <ButtonRelease-1> [mymethod BR1 %x %y]
	bind $win <Down> [list ttk::menubutton::Popdown %W]
	bind $win <Motion> [mymethod check_cursor %x %y]
	bind $win <Configure> [mymethod  _calc_xmin]
	
	foreach i [list <ButtonPress-1> <ButtonRelease-1> <B1-Leave>] {
	    regsub {1} $i {3} i3
	    bind $win $i3 [bind TMenubutton $i]
	}
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
    method set_is_button_active { value } {
	set is_button_active $value
    }
    method BP1 { x y } {
	if { $is_button_active == 0 } { return }
	
	if { ($x < $xmin || $is_button_active eq "only_button") && $options(-command) ne "" } {
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
	    $win state !pressed
	    ttk::menubutton::Pulldown $self
	}
    }
    method BR1 { x y } {
	if { $is_button_active == 0 } { return }
	
	if { $press_after ne "" } {
	    after cancel $press_after
	}
	if { $press_after ne "" && ($x < $xmin || $is_button_active eq "only_button") && $options(-command) ne "" } {
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
	if { $x < $xmin || $is_button_active eq "only_button" } {
	    $win configure -cursor ""
	} else {
	    $win configure -cursor bottom_side
	}
    }
}

snit::widgetadaptor cu::menubutton_checkbutton {
    option -menu ""
    option -image ""

    delegate method * to hull
    delegate option * to hull
    delegate option -_image to hull as -image

    variable xmin
    variable press_after ""
    
    constructor args {
	installhull using ttk::checkbutton -style Toolbutton
	bind $win <ButtonPress-1> [mymethod BP1 %x %y]
	bind $win <ButtonRelease-1> [mymethod BR1 %x %y]
	bind $win <ButtonPress-3> [mymethod BP3 %x %y]
	bind $win <Down> [mymethod popup]
	bind $win <Motion> [mymethod check_cursor %x %y]
	bind $win <Configure> [mymethod  _calc_xmin]

	$self configurelist $args
    }
    onconfigure -image {img} {
	set options(-image) $img

	set imgSpec ""
	foreach "state img" [list "" {*}$img] {
	    set new_img [cu::add_down_arrow_to_image $img]
	    bind $win <Destroy> +[list image delete $new_img]
	    if { $state ne "" } {
		lappend imgSpec $state
	    }
	    lappend imgSpec $new_img
	}
	$self configure -_image $imgSpec
    }
    method _calc_xmin {} {
	if { [winfo width $win] > 1 } {
	    set xmin  [expr {[winfo width $win]-12}]
	} else {
	    set xmin  [expr {[winfo reqwidth $win]-12}]
	}
    }
    method popup {} {
	$win instate !disabled {
	    set x [winfo rootx $win]
	    set y [expr {[winfo rooty $win]+[winfo height $win]}]
	    tk_popup $options(-menu) $x $y
	}
    }
    method BP1 { x y } {
	set press_after ""
	if { $x >= $xmin } {
	    $win instate !disabled {
		catch { tile::clickToFocus $win }
		catch { ttk::clickToFocus $win }
		$win state pressed
		$self popup
	    }
	    set press_after [after 700 [mymethod BP1_after]]
	    return -code break
	}
    }
    method BP1_after {} {
	set press_after "pressed"
	$self popup
    }
    method BR1 { x y } {
	if { $press_after ne "" } {
	    after cancel $press_after
	}
	if { $press_after eq "pressed" || $x >= $xmin } {
	    $win instate {pressed !disabled} {
		$win state !pressed
	    }
	    set press_after ""
	    return -code break
	}
	set press_after ""
    }
    method BP3 { x y } {
	$win instate {pressed !disabled} {
	    $win state !pressed
	}
	$self popup
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

snit::widgetadaptor cu::nicelabel {
    option -link_callback ""

    delegate method _insert to hull as insert
    delegate method _delete to hull as delete
    delegate method * to hull
    delegate option * to hull

    variable underlinefont

    constructor args {
	if { [lsearch -exact [font names] TkDefaultFont] != -1 } {
	    set font TkDefaultFont
	} else {
	    set font "-size 10"
	}
	text $self -width 5 -height 1 -bd 0 -insertwidth 0 -spacing3 3 -font $font \
	    -takefocus 0 -wrap word -highlightthickness 0
		
	set bg [cu::give_widget_background [winfo parent $self]]
	$self configure -background $bg
	$self configure -cursor ""
	installhull $self
	
	set font [font actual [$self cget -font]]
	set size [expr {[font actual $font -size]+1}]
	set font [font actual $font]
	set ipos [expr {[lsearch $font -size]+1}]
	set font [lreplace $font $ipos $ipos $size]
	$self configure -font $font

	set font [font actual [$self cget -font]]
	set size [expr {[font actual $font -size]-3}]
	set font [font actual $font]
	set ipos [expr {[lsearch $font -size]+1}]
	set font [lreplace $font $ipos $ipos $size]

	set boldfont [font actual [$self cget -font]]
	set ipos [expr {[lsearch $boldfont -weight]+1}]
	set boldfont [lreplace $boldfont $ipos $ipos bold]
	
	set underlinefont [list {*}[font actual [$self cget -font]] -underline 1]
	
	$self tag configure subscript -font $font -offset -3
	$self tag configure superscript -font $font -offset 3
	$self tag configure bold -font $boldfont
	$self tag configure red -foreground red
	$self tag configure grey -foreground grey
	
	bind $self <1> break
	bind $self <Tab> "[bind all <Tab>] ; break"
	bind $self <<PrevWindow>> "[bind all <<PrevWindow>>] ; break"
	bind $self <Configure> [mymethod _check_configure %w]
	$self configure -state disabled
	$self configurelist $args
    }
    method insert { index txt { tagList "" } } {
	if { $index ne "end" } { set index 1.$index }
	$self configure -state normal
	$self _insert $index $txt $tagList
	$self configure -state disabled
    }
    method insert_link { index txt link title } {
	
	if { $index ne "end" } {
	    set index 1.$index
	} else {
	    set index end-1c
	}
	set index [$self index $index]
	set tag link$index
	$self tag configure $tag -foreground blue -font $underlinefont
	if { $title ne "" } {
	    tooltip::tooltip $self -tag $tag $title
	}
	$self configure -state normal
	$self _insert $index $txt [list $tag]
	$self configure -state disabled
	
	$self tag bind $tag <1> [mymethod _eval_link $link]
	$self tag bind $tag <Enter> +[list $self configure -cursor hand2]
	$self tag bind $tag <Leave> +[list $self configure -cursor ""]
    }
    method delete { index1 index2 } {
	$self configure -state normal
	$self _delete $index1 $index2
	$self configure -state disabled
    }
    method _check_configure { width } {
	if { [winfo width $win] <= 1 } { return }
	set ds [$win count -displaylines 1.0 end]
	set max [expr {([$win cget -height]>4)?[$win cget -height]:4}]
	if { $ds > $max } { set ds $max }
	if { $ds != [$win cget -height] } {
	    $win configure -height $ds
	}
    }
    method _eval_link { cmd } {
	uplevel #0 $options(-link_callback) $cmd
    }
}

################################################################################
# cu::combobox_tree
################################################################################

snit::widgetadaptor cu::_fulltktree {
    option -height 10

    delegate method * to hull
    delegate option * to hull
    delegate option -_height to hull as -height
 
    delegate method _activate to hull as activate
    delegate method _selection to hull as selection
    delegate method _see to hull as see

    #variable marker_resize_xy0

    constructor args {
	package require fulltktree
	installhull using ::fulltktree -height 10 -has_sizegrip 1
	$self configurelist $args
	
#         ttk::sizegrip $win.grip -style TSizegripWhite
#         place $win.grip -relx 1 -rely 1 -anchor se
#         bind $win <<ScrollMap>> [list after 100 [list raise $win.grip]]

#         label $win.l -image [cu::get_image resizer] -cursor sizing
#         grid $win.l -row 1 -column 1
#         grid configure $win.t -rowspan 2
# 
#         bind $win.l <ButtonPress-1> [mymethod marker_resize BP1 %X %Y]
#         bind $win.l <B1-Motion> [mymethod marker_resize BM1 %X %Y]
    }
    onconfigure -height {value} {
	set options(-height) $value
	if { $value ne "" } {
	    set n [$self index last]
	    if { $n < 2 } { set n 2 }
	    if { $n > 10 } { set n 10 }
	    set h1 [$self cget -itemheight]
	    if { $h1 == 0 } {
		set h1 [$self cget -minitemheight]
	    }
	    set h [expr {$h1*$n}]
	    $self configure -_height $h
	}
    }
#     method marker_resize { what x y } {
# 
#         switch $what {
#             BP1 {
#                 set marker_resize_xy0 [list $x $y]
#             }
#             BM1 {
#                 foreach "x0 y0" $marker_resize_xy0 break
#                 set t [winfo toplevel $win]
#                 set width [expr {[winfo width $t]+$x-$x0}]
#                 set height [expr {[winfo height $t]+$y-$y0}]
#                 if { $width < 0 } { set width 0 }
#                 if { $height < 0 } { set height 0 }
#                 wm geometry $t ${width}x$height
#                 set marker_resize_xy0 [list $x $y]
#             }
#         }
#     }
    method curselection {} {
	set ret ""
	foreach i [$self selection get] {
	    lappend ret [expr {$i-1}]
	}
	return $ret
    }
    method activate { args } {
	catch { $self _activate {*}$args }
    }
    method see { args } {
	catch { $self _see {*}$args }
    }
    method selection { what args } {
	if { $what eq "set" } {
	    set err [catch { $self _selection add {*}$args } ret]
	    return $ret
	}
	return [$self _selection $what {*}$args]
    }
}

snit::widgetadaptor cu::combobox_tree {
    option -textvariable ""
    option -popup_width 0
    option -semi_readonly 0
    option -nice_print_separator ""    

    delegate method * to hull
    delegate option * to hull
    delegate method tree_item to toctree as item
    delegate method _insert to hull as insert
    delegate method _delete to hull as delete
    delegate option -_textvariable to hull as -textvariable

    variable popdown
    variable toctree
    variable no_active_items ""
    variable no_active_values ""
    variable cmd_items ""
    variable internal_textvariable
    variable prev_textvariable
    variable batch_insert_handler
    variable save_grab ""
    variable changing_vars 0
    
    constructor args {

	package require fulltktree

	installhull using ttk::combobox
	
	cu::add_contextual_menu_to_entry $win init

	set popdown [toplevel $win.popdown -relief solid -bd 1]
	wm withdraw $popdown
	update idletasks
	wm overrideredirect $popdown 1
	wm transient $popdown [winfo toplevel $win]

	namespace eval ::ttk::combobox [list set Values($win) ""]
	
	if { [package vcompare [package present Tcl] 8.5.9] >= 0 } {
	    set popdown [ttk::frame $popdown.f -style ComboboxPopdownFrame]
	    grid $win.popdown.f -sticky nsew
	    grid columnconfigure $win.popdown 0 -weight 1
	    grid rowconfigure $win.popdown 0 -weight 1
	}
	# tk8.5a6
	ttk::scrollbar $popdown.sb -orient vertical
	bind $win.popdown <Configure> [list grid remove $popdown.sb]
	
	set columns [list [list 10 "" left imagetext 1]]
	
	set toctree [cu::_fulltktree $popdown.l \
		-selecthandler [mymethod press] \
		-columns $columns -expand 1 \
		-selectmode browse -showheader 0 -showlines 0  \
		-showbutton 0 -indent 0 -bd 0 \
		-have_vscrollbar 1 \
		-have_search_button automatic]
		
	grid $toctree -sticky nsew
	grid columnconfigure $popdown 0 -weight 1
	grid rowconfigure $popdown 0 -weight 1

	$self configurelist $args

	bind $win <KeyPress> [mymethod keypress %K %A]
	
	#bind $popdown.l.t <ButtonPress-1>    [mymethod button1 %W %x %y]
	bind $win.popdown <ButtonPress-1>    [mymethod button1 %W %x %y]
	bind $popdown.l.t <KeyPress-Return>  [mymethod select_entry]
	bind $popdown.l.t <KeyPress-Escape>  [mymethod cancel]
#         bind $popdown.l.t <KeyPress-Tab>     { ttk::combobox::LBTab [winfo parent %W] next }
#         bind $popdown.l.t <<PrevWindow>>     { ttk::combobox::LBTab [winfo parent %W] prev }
	bind $popdown.l.t <Destroy>          { catch { ttk::combobox::LBCleanup [winfo parent %W] }}

	switch -- [tk windowingsystem] {
	    win32  {
		bind $win.popdown <FocusOut>  {
		    if { "%W" eq [winfo toplevel %W] } {
		        ttk::combobox::LBCancel [winfo parent %W]
		    }
		}
		#bind $popdown.l.t <FocusOut>  { ttk::combobox::LBCancel [winfo parent %W] }
	    }
	    aqua {
		bind $popdown.l.t <Deactivate> { ttk::combobox::LBCancel [winfo parent %W] }
	    }
	}
	bind $win.popdown <Map> [mymethod begin_post]
	bind $win.popdown <Unmap> [mymethod end_post %W]
	
	set internal_textvariable ""
	$self configure -_textvariable [myvar internal_textvariable]
	#trace add variable [myvar internal_textvariable] write [mymethod changed_textvariable_int]

	$self configurelist $args
	
	$self _check_insert_new
	return $self
    }
    destructor {
	#trace remove variable [myvar internal_textvariable] write [mymethod changed_textvariable_int]

	if { $options(-textvariable) ne "" } {
	    trace remove variable $options(-textvariable) write [mymethod changed_textvariable_ext]
	} 
    }
    onconfigure -textvariable {value} {
	if { $options(-textvariable) ne "" } {
	    trace remove variable $options(-textvariable) write [mymethod changed_textvariable_ext]
	}
	set options(-textvariable) $value
	trace add variable $options(-textvariable) write [mymethod changed_textvariable_ext]
	if { [info exists $value] } {
	    $self changed_textvariable_ext
	}
    }
    onconfigure -semi_readonly {value} {
	set options(-semi_readonly) $value
	
	if { $options(-semi_readonly) } {
	    $self state readonly
	}
    }    
    method insert { args } {
	$self _insert {*}$args
	$self changed_textvariable_int
    }
    method delete { args } {
	$self _delete {*}$args
	$self changed_textvariable_int
    }
    method make_editable { item } {
	$self state !readonly
	$self delete 0 end
	focus $win
    }

    method give_tree {} {
	return $toctree
    }
    method button1 { w x y } {
	
	set top [winfo toplevel $w]
	if { $x < -10 || $x > [winfo width $top]+10 || $y < 0 || $y > [winfo height $top]+10 } {
	    $self action LBCancel
	    return
	}
	return
	focus $popdown.l.t
	return -code continue
    }
    method select_entry {} {   
	set id [$toctree index active]
	if { [dict exists $no_active_items $id] && ![dict exists $cmd_items $id] } {
	    return
	}
	ttk::combobox::LBSelected $popdown.l
	set item [lindex [$popdown.l selection get] 0]
	if { $item eq "" } {
	    set item [$popdown.l index active]
	}
	if { $item eq "" } { return }
		       
	if { [dict exists $cmd_items $item] } {
	    uplevel #0 [dict get $cmd_items $item] $item
	    if { [dict exists $no_active_items $item] } { return }
	    return
	}
	#set internal_textvariable [$popdown.l item text $item 0]
	set internal_textvariable [lindex  [$self cget -values] $item-1]
	
	update idletasks
	$self icursor end
	$self xview [$self index end]
	$self changed_textvariable_int
    }
    method cancel {} {
	ttk::combobox::LBCancel $popdown.l
	$self icursor end
	$self xview [$self index end] 
    }
    method press { t id } {
	if { [dict exists $cmd_items $id] } {
	    if { ![dict exists $no_active_items $id] } { 
		$self action LBCancel
	    }
	    uplevel #0 [dict get $cmd_items $id] $id
	    return
	}
	if { [dict exists $no_active_items $id] } { 
	    $self expand_collapse
	    return 
	}
	wm withdraw [winfo toplevel $popdown]
	$self select_entry
    }
    method expand_collapse {} {   
	set id [$toctree index active]
	set isopen [$toctree item state get $id open]
	if {!$isopen} {
	    $toctree item expand $id
	} else {
	    $toctree item collapse $id
	}
    }
    method action { action } {
	set id [$toctree index active]
	if { $action eq "LBSelected" && [dict exists $no_active_items $id] } { return }
	
	if { $action eq "LBSelected" } {
	    if { [dict exists $no_active_items $id] } { return }
	    $self select_entry
	} elseif { $action eq "LBCancel" } {
	    $self cancel
	} else {
	    ttk::combobox::$action $popdown.l
	    $self icursor end
	    $self xview [$self index end]
	}
    }
    method _check_insert_new {} {
	if { $options(-semi_readonly) && [$toctree item count] == 1 && [$self instate readonly] } {
	    $self tree_insert -image [cu::get_icon filenew-16] -active 1 -command [mymethod make_editable] \
		-check_insert_new 0 end [_ "New#C#Masculino"] [_ "New#C#Masculino"] 0
	}
    }
    method change_item_active { item active_inactive } {
	if { $active_inactive eq "active" } {
	    set fullname [lindex  [$self cget -values] $item-1]
	    dict unset no_active_items $item
	    dict unset no_active_values $fullname
	} else {
	    dict set no_active_items $item ""
	    dict set no_active_values $fullname ""
	}
    }
    method tree_insert { args } {
	set optional {
	    { -image image "" }
	    { -collapse boolean 0 }
	    { -active boolean 1 }
	    { -command cmd "" }
	    { -check_insert_new boolean 1 }
	}
	set compulsory "idx name fullname parent"
	parse_args $optional $compulsory $args
	
	if { $check_insert_new } {
	    $self _check_insert_new
	}
	if { $image eq "" } {
	    set image appbook16
	} elseif { $image eq "-" } {
	    set image ""
	}
	if { [$toctree item style set $parent] eq "imagetext" } {
	    set pimg [$toctree item image $parent 0]
	    if { [lindex $pimg end-1] eq "appbook16" } {
		$toctree item image $parent 0 [list mac-expand open mac-collapse {}]
	    }
	}
	set id [$toctree insert $idx [list [list $image $name]] $parent]
	if { $collapse } {
	    $toctree item collapse $id
	}
	if { !$active } {
	    dict set no_active_items $id ""
	    dict set no_active_values $fullname ""
	}
	if { $command ne "" } {
	    dict set cmd_items $id $command
	}
	if { $fullname eq [$self get] } {
	    $toctree selection clear
	    $toctree selection add $id
	    $toctree activate $id
	    foreach i [lrange [$toctree item ancestors $id] 0 end-1] {
		$toctree item expand $i
	    }
	    $toctree see $id
	}
	set values [$self cget -values]
	lappend values $fullname
	$self configure -values $values

	return $id
    }
    method tree_insert_batch { values_tree { last_item "-1 root" } } {

	set delta 10
	if { [info exists batch_insert_handler] } {
	    after cancel $batch_insert_handler
	    unset batch_insert_handler
	}
	foreach i [lrange $values_tree 0 $delta-1] {
	    lassign $i level name fname image selectable
	    set it [$self tree_insert -collapse 1 -image $image \
		    -active $selectable end \
		    $name $fname [dict get $last_item [expr {$level-1}]]]
	    dict set last_item $level $it
	}
	if { [llength $values_tree] > $delta } {
	    set batch_insert_handler [after 100 [mymethod tree_insert_batch \
		        [lrange $values_tree $delta end] $last_item]]
	}
    }
    method clear {} {
	set no_active_items ""
	set no_active_values ""
	set cmd_items ""
	$toctree item delete all
	$self configure -values ""
	$self _check_insert_new
	wm geometry $win.popdown ""
    }
    method give_first_active {} {
	foreach i [$self cget -values] {
	    if { ![dict exists $no_active_values $i] } {
		return $i
	    }
	}
#         set idx 0
#         foreach id [$toctree item range 1 end] {
#             if { ![dict exists $no_active_items $id] } {
#                 return [lindex [$self cget -values] $idx]
#             }
#             incr idx
#         }
	return ""
    }
    variable _begin_post_handler
    method begin_post {} {
	if { [info exists _begin_post_handler] } { return }
	$self _begin_post_after
	set _begin_post_handler [after 200 [mymethod begin_post_unset]]
    }
    method begin_post_unset {} {
	unset -nocomplain _begin_post_handler
    }
    method _begin_post_after {} {
	
	set pattern [string trimright [$self get] *]*
	foreach id [rangeF 1 [$toctree index last]] {
	    if { [string match -nocase $pattern [$toctree item text $id 0]] } {
		$toctree selection clear
		$toctree selection add $id
		$toctree activate $id
		break
	    }
	}
	set y [winfo rooty $popdown]
	set height [winfo height $popdown]
	
	if { $options(-popup_width) == 0 } {
	    set delta 40
	} else {
	    set delta [expr {$options(-popup_width)-[winfo width $win]}]
	}
	set x [expr {[winfo rootx $win]-$delta}]
	if { $x < 0 } { set x 0 }
	set width [expr {[winfo width $win]+$delta}]

	wm geometry [winfo toplevel $popdown] ${width}x$height+$x+$y
	
	wm attributes [winfo toplevel $popdown] -topmost 1
	raise [winfo toplevel $popdown]

	after 100 [list focus -force [$toctree givetreectrl]]
	set save_grab [grab current $win]
	catch { grab -global [winfo toplevel $popdown] }
    }
    method end_post { w } {
	if { $w ne [winfo toplevel $popdown] } { return }
	
	$toctree close_search_label

	grab release $toctree
	if { $save_grab ne "" } {
	    catch { grab $save_grab }
	}
	focus $self
	
#         if { $options(-textvariable) ne "" } {
#             trace remove variable $options(-textvariable) write [mymethod changed_textvariable]
#         }
	if { [info exists batch_insert_handler] } {
	    after cancel $batch_insert_handler
	    unset batch_insert_handler
	}
    }
    method changed_textvariable_int { args } {
	
	if { $changing_vars } { return }
#         if { $internal_textvariable eq "" } {
#             set prev_textvariable $internal_textvariable
#             return
#         }
#         if { $internal_textvariable eq "" || ![dict exists $no_active_values $internal_textvariable] } {
#             set prev_textvariable $internal_textvariable
#             return
#         }
	set changing_vars 1
	set deltaList [list 1 -1]
	set deltaPos 0
	set vLocal $internal_textvariable
	set idx [lsearch -exact [$self cget -values] $vLocal]
	set idxLocal $idx
	if { [info exists prev_textvariable] } {
	    set idx_prev [lsearch -exact [$self cget -values] $prev_textvariable]
	    if { $idx_prev > $idx } {
		set deltaList [list -1 1]
	    }
	}
	while { [dict exists $no_active_values $vLocal] } {
	    incr idxLocal [lindex $deltaList $deltaPos]
	    set vLocal [lindex [$self cget -values] $idxLocal]
	    if { $vLocal eq "" } {
		incr deltaPos
		if { $deltaPos >= [llength $deltaList] } { break }
		set idxLocal $idx
		set vLocal [lindex [$self cget -values] $idxLocal]
	    }
	}

	#  && [$win instate readonly]
	if { $options(-nice_print_separator) ne "" } {
	    set l ""
	    foreach i $vLocal {
		if { $i ne "" } { lappend l $i }
	    }
	    set internal_textvariable [join $l $options(-nice_print_separator)]
	} else {
	    set internal_textvariable $vLocal
	}
	if { $options(-textvariable) ne "" } {
	    upvar #0 $options(-textvariable) v
	    set v $vLocal
	}
	set prev_textvariable $vLocal
	set changing_vars 0
    }
    method changed_textvariable_ext { args } {
	
	if { $changing_vars } { return }
	set changing_vars 1
	upvar #0 $options(-textvariable) v
	if { $options(-nice_print_separator) ne "" } {
	    set internal_textvariable [join $v $options(-nice_print_separator)]
	} else {
	    set internal_textvariable $v
	}
	set changing_vars 0
    }
    method keypress { key char } {
	if { ![$win instate readonly] } { return }
	if { ![string is print -strict $char] } { return }
	uplevel #0 [string map [list %W [list $win]] [bind TCombobox <KeyPress-Down>]]
	if { ![string is space $char] } {
	    $toctree keypress $key $char
	}
    }
}

################################################################################
# cu::fullscale
################################################################################

snit::widget cu::fullscale {
    option -title ""
    option -left_text ""
    option -right_text ""

    hulltype frame

    variable scale

    delegate method * to scale
    delegate option * to scale

    constructor args {

	$hull configure -background #a4b97f -bd 1 -relief solid
	
	label $win.l1 -textvariable [myvar options(-title)] -background #a4b97f
	label $win.l2 -textvariable [myvar options(-left_text)] -background #a4b97f
	label $win.l3 -textvariable [myvar options(-right_text)] -background #a4b97f
	
	set font [concat [font actual [$win.l1 cget -font]] -weight bold]
	$win.l1 configure -font $font

	install scale using ttk::scale $win.s -length 100

	grid $win.l1 -sticky w
	grid $win.l2 $win.l3 -sticky w
	grid $scale     - -padx 1 -pady 1 -sticky ew

	grid $win.l3 -sticky e
	
	grid columnconfigure $win 1 -weight 1
	grid rowconfigure $win 3 -weight 1

	$self configurelist $args
    }
}

snit::widgetadaptor cu::scale {
    option -title ""
    option -left_text ""
    option -right_text ""
    option -label_style ""
    option -from 0
    option -to 1
    option -variable ""
    option -command ""
    option -resolution 1
    option -length 200
    option -play_handler ""
    option -repeat_variable ""
    option -showvalue 1 ;# compatibility option
    option -orient ;# compatibility option
    
    variable internal_variable
    variable play_widget
    variable scale_left2_full
    variable scale_right2_full
    variable last_value ""
    variable after_handle
    
    delegate method * to hull
    delegate option * to hull

    constructor args {
	installhull using canvas -width 200 -height 24 -background grey -takefocus 1 -highlightthickness 0 -bd 0 \
	    -relief solid
	$self create_images
	set scale_left2_full [image create photo]
	set scale_right2_full [image create photo]

	bind $win <Configure> [mymethod draw -check_window %W -all]
	bind $win <B1-Motion> [mymethod move  -check_window %W -- %x %y]
	bind $win <ButtonRelease-1> [mymethod move  -check_window %W -- %x %y]
	bind $win <Left> [mymethod commands back10]
	bind $win <Right> [mymethod commands forward10]
	bind $win <<Contextual>> [mymethod contextual %X %Y]
	bind $win <FocusIn> [mymethod commands focus]

	$self configurelist $args
	$self draw
    }
    destructor {
	if { $options(-variable) ne "" } {
	    trace remove variable $options(-variable) write "[mymethod draw];#"
	}
    }
    onconfigure -length {value} {
	set options(-length) $value
	$self configure -width $value
    }
    onconfigure -variable {value} {
	
	if { $options(-variable) ne "" } {
	    trace remove variable $options(-variable) write "[mymethod draw];#"
	}
	set options(-variable) $value

	if { $options(-variable) ne "" } {
	    trace add variable $options(-variable) write "[mymethod draw];#"
	}
    }
    method draw { args } {

	set optional {
	    { -all "" 0 }
	    { -check_window w "" }
	}
	set compulsory ""
	parse_args $optional $compulsory $args
	
	if { $check_window ne "" && $check_window ne $win } {
	    return
	}

	set value [$self give_value]
	
	if { $all } {
	    $self delete slider
	} else {
	    if { $value == $last_value } { return }
	    $self delete slider
	}
	set width [winfo width $win]
	set height 24
	
	set value_unitary [$self give_value_unitary]
	set active_width [expr {$width-[image width cu::scale::img::scale_left]-[image width cu::scale::img::scale_right]}]
	if { $active_width < 0 } {
	    set active_width 0
	}
	set left_active_width [expr {round($value_unitary*$active_width)}]
	if { [image width $scale_left2_full] != $left_active_width } {
	    $scale_left2_full configure -width $left_active_width -height 24
	    $scale_left2_full  copy cu::scale::img::scale_left2 -to 0 0 $left_active_width 24
	}
	set right_active_width [expr {$active_width-$left_active_width}]
	if { [image width $scale_right2_full] != $right_active_width } {
	    $scale_right2_full configure -width $right_active_width -height 24
	    $scale_right2_full copy cu::scale::img::scale_right2 -to 0 0 $right_active_width 24
	}
	if { $options(-left_text) ne "" ||  $options(-right_text) ne "" ||  $options(-title) ne "" } {
	    #set font TkSmallCaptionFont 
	    set font SystemTinyFont
	    $self delete labels
	    if { [$self find withtag labels] eq "" } {
		if { $options(-left_text) ne "" } {
		    $self create text 0 0 -anchor nw -text $options(-left_text) -font $font -tags labels
		}
		if { $options(-right_text) ne "" } {
		    $self create text [expr {$width-1}] 0 -anchor ne -text $options(-right_text) -font $font -tags labels
		}
		if { $options(-title) ne "" } {
		    $self create text [expr {0.5*$width}] 0 -anchor n -text $options(-title) -font $font -tags labels
		}
	    }
	    set y [font metrics $font -linespace]
	    incr height $y
	} else {
	    set y 0
	}
	set ymed [expr {$y+12}]
	$self create image 0 $y -anchor nw -image cu::scale::img::scale_left -tags slider
	set x [image width cu::scale::img::scale_left]
	$self create image $x $y -anchor nw -image $scale_left2_full -tags slider
	incr x $left_active_width
	$self create image $x $y -anchor nw -image $scale_right2_full -tags slider
	incr x $right_active_width
	$self create image $x $y -anchor nw -image cu::scale::img::scale_right -tags slider
	incr x -$right_active_width
	$self create image $x $ymed -anchor center -image cu::scale::img::slider -tags slider
	switch $options(-label_style) {
	    time {
		set txt [cu::nice_time -refs [list $options(-from) $options(-to)] $value]
		set tw [expr {[font measure $font $txt]+4}]
		set x [expr {$x+[image width cu::scale::img::slider]}]
		if { $x+$tw > $width-[image width cu::scale::img::scale_right] } {
		    set x [expr {$x-2.5*[image width cu::scale::img::scale_right]-$tw}]
		}
		$self create text $x $ymed -text $txt -font $font
	    }
	}
	if { $options(-play_handler) ne "" } {
	    incr y 24
	    if { ![winfo exists $win.f] } {
		ttk::frame $win.f
		ttk::button $win.f.b1 -image cu::scale::img::back -command [mymethod commands back] \
		    -style Toolbutton
		set play_widget [ttk::button $win.f.b2 -image cu::scale::img::play -command \
		        [mymethod commands toggle_play] -style Toolbutton]
		ttk::button $win.f.b3 -image cu::scale::img::forward -command \
		    [mymethod commands forward] -style Toolbutton
		grid $win.f.b1 $win.f.b2 $win.f.b3 -padx 2 -pady 2
		if { $options(-repeat_variable) ne "" } {
		    ttk::checkbutton $win.f.cb -text [_ "Repeat"] -variable $options(-repeat_variable)
		    grid $win.f.cb -row 0 -column 3 -sticky w -padx "5 0"
		    grid columnconfigure $win.f 3 -weight 1
		    bind $win.f <Configure> [mymethod _configure_buttons_frame $win.f]
		}
		cu::add_bindtag_recursive -pos before_toplevel $win.f $win
	    }
	    if { [$self find withtag controls] eq "" || $all } {
		update idletasks
		grid anchor $win.f n
		grid propagate $win.f 0
		$win.f configure -width [expr {$width-4*[$self cget -bd]}] -height [winfo reqheight $win.f]
		if { [$self find withtag controls] eq "" } {
		    $self create window [expr {0.5*$width}] $y -anchor n -window $win.f -tags controls
		} else {
		    $self coords controls [expr {0.5*$width}] $y
		}
	    }
	    incr height [expr {[winfo reqheight $win.f]}]
	}
	$self configure -height $height
	
	if { $options(-repeat_variable) ne "" && $value >= $options(-to) } {
	    set repeat [set $options(-repeat_variable)]
	    if { $repeat == 0 } {
		$self commands pause_state
	    }
	}
	set last_value $value
	update
    }
    method _configure_buttons_frame { frame } {
	set wi 0
	foreach i [winfo children $frame] { incr wi [winfo reqwidth $i] }
	if { [winfo width $frame] > $wi } {
	    grid columnconfigure $win.f 3 -weight 0
	} else {
	    grid columnconfigure $win.f 3 -weight 1
	}
    }
    method move { args } {
	
	set optional {
	    { -check_window w "" }
	}
	set compulsory "x y"
	parse_args $optional $compulsory $args
	
	if { $check_window ne "" && $check_window ne $win } {
	    return
	}
	lassign [$self bbox slider] x1 y1 x2 y2
	if { $y < $y1 || $y > $y2 } {
	    return
	}
	set width [winfo width $win]
	set active_width [expr {$width-[image width cu::scale::img::scale_left]-[image width cu::scale::img::scale_right]}]
	set value_unitary [expr {($x-[image width cu::scale::img::scale_left])/double($active_width)}]
	$self set_value_unitary $value_unitary
	if { $options(-command) ne "" } {
	    uplevel #0 $options(-command)
	}
	$self commands play1_idle
    }
    method give_value {} {
	if { $options(-variable) ne "" } {
	    set value [set! $options(-variable)]
	} else {
	    set value [set! internal_variable]
	}
	if { $value eq "" } {
	    set value [expr {.5*($options(-from)+$options(-to))}]
	    if { [llength $options(-resolution)] > 1 } {
		lassign [m::linear_interpolate $value  $options(-resolution)] t1 t2 alpha1 alpha2
		set value [expr {($alpha1>=$alpha2)?$t1:$t2}]
	    } elseif { $options(-resolution) > 0 } {
		set value [expr {round($value/double($options(-resolution)))*$options(-resolution)}]
	    }
	}
	return $value
    }
    method give_value_unitary {} {
	set value [$self give_value]
	set value_unitary [expr {($value-$options(-from))/double($options(-to)-$options(-from))}]
	if { $value_unitary < 0.0 } { set value_unitary 0.0 }
	if { $value_unitary > 1.0 } { set value_unitary 1.0 }
	return $value_unitary
    }
    method set_value_window {} {

	package require dialogwin

	destroy $win._ask
	set w [dialogwin_snit $win._ask -title [_ "Enter value"]]
	set f [$w giveframe]
	
	ttk::label $f.l1 -text [_ "value"]:
	spinbox $f.e -textvariable [$w give_uservar value [$self give_value]] -from $options(-from) \
	    -to $options(-to) -increment [expr {0.1*($options(-to)-$options(-from))}]

	grid $f.l1 $f.e -sticky w -padx 2 -pady 3
	grid configure $f.e -sticky ew
	grid columnconfigure $f 1 -weight 1
	
	tk::TabToWindow $f.e
	bind [winfo toplevel $f] <Return> [list $w invokeok]
	set action [$w createwindow]
	set value [$w give_uservar_value value]
	destroy $w
	if { $action < 1 } { return }
	$self set_value $value
	$self commands play1_idle
    }
    method set_value { value } {
	
	if { [llength $options(-resolution)] > 1 } {
	    lassign [m::linear_interpolate $value  $options(-resolution)] t1 t2 alpha1 alpha2
	    set value [expr {($alpha1>=$alpha2)?$t1:$t2}]
	} elseif { $options(-resolution) > 0 } {
	    set value [expr {round($value/double($options(-resolution)))*$options(-resolution)}]
	}
	if { $value < $options(-from) } { set value $options(-from) }
	if { $value > $options(-to) } { set value $options(-to) }

	if { $options(-variable) ne "" } {
	    set $options(-variable) $value
	} else {
	    set internal_variable $value
	    $self draw
	}
    }
    method set_value_unitary  { value_unitary } {
	if { $value_unitary < 0.0 } { set value_unitary 0.0 }
	if { $value_unitary > 1.0 } { set value_unitary 1.0 }
	$self set_value [expr {$options(-from)+$value_unitary*($options(-to)-$options(-from))}]
    }
    method commands { what } {
	switch $what {
	    focus {
		if { [info exists play_widget] && [winfo exists $play_widget] } {
		    focus $play_widget
		}
	    }
	    back {
		$self set_value $options(-from)
		$self commands pause_state
		
		if { $options(-command) ne "" } {
		    uplevel #0 $options(-command)
		}
		$self commands play1_idle
	    }
	    forward {
		$self set_value $options(-to)
		$self commands pause_state
		
		if { $options(-command) ne "" } {
		    uplevel #0 $options(-command)
		}
		$self commands play1_idle
	    }
	    play_state {
		$play_widget configure -image cu::scale::img::pause
	    }
	    pause_state {
		$play_widget configure -image cu::scale::img::play
	    }
	    toggle_play {
		if { [$play_widget cget -image] eq "cu::scale::img::play" } {
		    $self commands play_state
		    set action play
		} else {
		    $self commands pause_state
		    set action pause
		}
		uplevel #0 $options(-play_handler) $action
	    }
	    play1 {
		unset -nocomplain after_handle
		uplevel #0 $options(-play_handler) play1
	    }
	    back10 {
		if { [llength $options(-resolution)] > 1 } {
		    lassign [m::linear_interpolate [$self give_value] $options(-resolution)] t1 t2 alpha1 alpha2 idx1 idx2
		    if { $alpha2 == 1 } {
		        set idx1 $idx2
		    }
		    if { $idx1 > 0 } {
		        $self set_value [lindex $options(-resolution) [expr {$idx1-1}]]
		    }
		} elseif { $options(-resolution) > 0 } {
		    $self set_value [expr {[$self give_value]-$options(-resolution)}]
		} else {
		    $self set_value_unitary [expr {[$self give_value_unitary]-0.1}]
		}
		if { $options(-command) ne "" } {
		    uplevel #0 $options(-command)
		}
		$self commands play1_idle
	    }
	    forward10 {
		if { [llength $options(-resolution)] > 1 } {
		    lassign [m::linear_interpolate [$self give_value] $options(-resolution)] t1 t2 alpha1 alpha2 idx1 idx2
		    if { $idx1 != $idx2 && $alpha2 != 1.0 } {
		        $self set_value [lindex $options(-resolution) $idx2]
		    } elseif { $idx2 < [llength $options(-resolution)]-1 } {
		        $self set_value [lindex $options(-resolution) [expr {$idx2+1}]]
		    }
		} elseif { $options(-resolution) > 0 } {
		    $self set_value [expr {[$self give_value]+$options(-resolution)}]
		} else {
		    $self set_value_unitary [expr {[$self give_value_unitary]+0.1}]
		}
		if { $options(-command) ne "" } {
		    uplevel #0 $options(-command)
		}
		$self commands play1_idle
	    }
	    play1_idle {
		if { $options(-play_handler)  eq "" } { return }
		if { [info exists after_handle] } {
		    after cancel $after_handle
		}
		set after_handle [after 200 [mymethod commands play1]]
	    }
	}
    }
    method contextual { X Y } {

	destroy $win.menu
	menu $win.menu -tearoff 0
	$win.menu add command -label [_ "Set value"] -command [mymethod set_value_window]
	tk_popup $win.menu $X $Y        
    }
    method create_images {} {
	if { [info command cu::scale::img::scale_left] ne "" } { return }
	
	package require img::png
	
	image create photo cu::scale::img::scale_left -data {
	    iVBORw0KGgoAAAANSUhEUgAAAAsAAAAYCAYAAAAs7gcTAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oCCw4cI5w+QK0AAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
	    TVBXgQ4XAAAAyklEQVQ4y5WSQWrEMBAEa5bFHwghLwj+he+55An+dA55RDDZ
	    y24sW5regwlaCJYncxQ9Rat7bBxH5ZyJzHkYBvq+p5RyLJZE13UxMYAkJMXE
	    pZS4jQjZ3TeyuzfFS1r4unxvZHfH3ffFXtDHZ+yDBXi5zdVGiywJW3Ml74nN
	    DCE0p+p5z8bv+5JSzblJlrjMiVOkOQOuOVdya8kRM8FSAH7cg3UbnJ+fYjkb
	    xuv7W4wstEV3lPOfe/6XuFX345weKz0kr+vKNE2hhTsA3NIsHj9zEwAAAABJ
	    RU5ErkJggg==
	}
	image create photo cu::scale::img::scale_left2 -data {
	    iVBORw0KGgoAAAANSUhEUgAAAAEAAAAYCAYAAAA7zJfaAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oCCw4dG60nyXIAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
	    TVBXgQ4XAAAAZUlEQVQI1y3MIRLCMBRF0TtvIIplVLOgbBLVAVyWUhQgQtNS
	    oKI/HxNz5CHG6KSUXCEE5O7IzBq35wMdTld0zAXtlhXZNKP3WNBQZnRfv6hg
	    6MWG9l0H/eXs+i2f9tVaGwDKOfMHLfU3vaubEdUAAAAASUVORK5CYII=
	}
	
	image create photo cu::scale::img::scale_right2 -data {
	    iVBORw0KGgoAAAANSUhEUgAAAAEAAAAYCAYAAAA7zJfaAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oCCw4dO5ZJ6boAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
	    TVBXgQ4XAAAAVElEQVQI12XMKw6AMBRFwdMb0j3ggGV1td0HKJLKOsIn4fVh
	    6jAjh5SSk3N2xRiRuyMz6+yloGHd0HicSO6IEBDwxwxdy4TeZUbPffevtdYB
	    UK2VDykFKfXx26t9AAAAAElFTkSuQmCC
	}
	
	image create photo cu::scale::img::scale_right -data {
	    iVBORw0KGgoAAAANSUhEUgAAAAsAAAAYCAYAAAAs7gcTAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oCCw4eGvENqicAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
	    TVBXgQ4XAAAAzElEQVQ4y5WSvW7DMAwGj7LsLRnapX01P60fxGuDLN4NOeLX
	    IbWTAoksE9Ak4nj8sb7vRUU0TYMNw6CaxHEciV3X7VJDCABEqcoCScScM7UR
	    c85bmRIVIFyuV+Z5RtLb5+53jfRzIZ3PtG2773xaFvRUqqQRDXB33H2fDCAe
	    fq/CzP7IZiCK5PUv8qTxjrxWDRyIuCqUNrmSIxKu8jS2Bv3zA1N5ztvo2u8v
	    8u1W1Agh3JOXlKrOs/qet0PaW/P/Bg8kH1vKNE1V5JQSvzfqry+32QatAAAA
	    AElFTkSuQmCC
	}
	image create photo cu::scale::img::slider -data {
	    iVBORw0KGgoAAAANSUhEUgAAAB0AAAAbCAYAAACAyoQSAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oCCw4GIcId21oAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
	    TVBXgQ4XAAAFuElEQVRIx+2Wy08b6xnGf994Bns8Nr5wdbgkQIAgBUJSRSDS
	    RFlkTSKUs4myy9/SRTfNn5DuHJ1Fom7TgwRpiAQBCalCNKI4pjEuNsaX8WU8
	    jGemi4IP5hBOzqK7vtK7me/yfPO8z3uB/9v/0MSzZ89+2wEhmn5qrus2/bvu
	    WFpa+q6NkiQhSRLHx8fouk6tVsO2bbxeL4FAIBoMBvMAjuP8Krjc1tb2XYCZ
	    TObHRCLxQy6XQwiBpmnIsky1WsWyLLxeLwMDA1y7dk34fL5LgeXLFoUQGIbB
	    5uamm8/nmZyc5PHjxwwODuLxeJr7SqUSW1tbrK6ukkgk3Js3b3LlyhUhSdLF
	    975///6bgEdHR+sbGxu/GxwcZGFhgWg0SrlcJpfLkclksCyLcDhMb28v4XAY
	    RVH4+PEj7969o6+vj9HRUXH2cc0/tW37QjpzuZy7sbHB9PQ0CwsL6LrO4uIi
	    Kysr7O7u4jgOQggcxyEYDHL37l3u3bvH3Nwc0WiUeDyObdvuxMSEOM+m+PPr
	    +C9A3UaD3e1/uCPXr/P06VN0XefNmzesr6/j8Xia1LquixAC13VpNBpEIhHm
	    5+eZm5tje3ubV69eMTExIfzBYEuMZe0vf219BZD2QONqjJmZGRqNBvF4nK2t
	    LU5F5zgOqqqiKArlchkARVEolUq8ffsWVVWZmpri1q1brK+tub/vjQnHAfgv
	    sDxZKLWAHgtI+iSujdwjFouxvLxMMplEVVVc10VRFKLRaPMBXV1d6LpOsVhE
	    URTq9TrLy8v09vby8OFD/ra8DPnCcL/iTTinoHK1foZXqHoEuc5Ornd3N1Wp
	    qmoz1n6/n0ajgWEYTcEpikJPTw/VahUhBOl0mr29PYaGhugfGCCZ/nfiatSL
	    5ZyA2qVyC6ipSNT8fWiqn2QyiSRJaJrWpDWfz3NRKkiSRHt7O67r4vP5+Pr1
	    Kx0dHfT09rKbSCAQcFLF5PI5ekteGV32uK7jUDEMfD4fiqLgOA7JZJJisfhN
	    UK/XS0dHB7ZtY5omtVoNWZYp1OtIohlS5L2S3nK4oLZhNSzqJ/T5/X4cx8Fx
	    HA4PD0kmk1yUe4qi0N3dTXt7O5ZloSgKhmFgmiZWo3Ei0RPQTL3WcrgsGriV
	    GuVKhfZgEE3TmvU0HA5TLBbx+Xy/APV6vXR1deH3+7FtG5/PR71ep1KpoJwr
	    enIJp+VDzbGxckfkjnIEAwE0TWvm2J07d/j06RPZbJa2trZmjlqWxf379xkf
	    H8dxfr7PNE329/f/HlYUTpULIOWdBme9cnyM+a/0SCqdpl6v4/F40DQNTdMY
	    Hh7mxYsX9PT0kM1mOTg4oFgs8ujRI54/f46maQQCAVRVbSr8nzs7k1f9Gq5t
	    w4nL8thwa2wQhBWR2NnZYeBKH+FQiKGhoaZ4bt++zcuXL0mlUui6zsjICKqq
	    NuMshCCTyWAYBtvb2xiGwejsDBUX3NM8vf+nP7RWJEk8ymayP/34+jVfEgnG
	    xsZIpVKMj4+3qDYSiTRL4dkmcXh4SD6fxzRNVlZWmJmdRRkbxbKsn2Naq1TO
	    a+Kn9kCA6elpPnz4QCgUQgjB58+fuXHjBqqqIstyy+TgOA6maZLNZtnb26Ne
	    r7O4uEggEGB2dlaUdf3X+6lt20xNTYlCoeAuLS0xOztLf38/m5ubxGIxOjo6
	    8Hq9CCFoNBpUq1UODg7I5XLous7q6iqGYfDkyZM/nmcDQD6rtvPJ/uDBA6Gq
	    qru2tsbu7i7Dw8PkcjlCoRB+vx8hBJZlUS6XKRQK7O/v8+XLF8LhMPPz8yIa
	    jXJR6xTxePzSyQEgnU67W1tbpFIpjo+PCQQC+P1+JElqVh7TNIlEIoyNjTE+
	    Pi5Oi8qF48pls9EpLX19faKzs5NarUahUCCTybjlchnbtgmFQkQiEWKxmNA0
	    DVVVkSTpm4AA8tHR0W8aQYPBIIFAQJxX7ek8ddp9LrP/AP+Pw0lffpsAAAAA
	    AElFTkSuQmCC
	}
	image create photo cu::scale::img::back -data {
	    iVBORw0KGgoAAAANSUhEUgAAABwAAAAWCAYAAADTlvzyAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oCCw4TJfLF+VcAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
	    TVBXgQ4XAAADu0lEQVRIx71Vy0ojaRT+Tv1lTOWeDrqSJPZICLgRQdypiyCI
	    7tR26UKYV+h+hJlH6HmEzAO4yEQ3gkYEDYpCgooQFa8xWpW6/FX1z2KmCi9J
	    ty5mDtSm6jvnO9+pc6FKpYL/02QhxIcciAi2bYMx9m684zggIhARJNd18Z5H
	    CAEhBJrNJur1+o5t2z/Fu66L6+trNBoN6LoOIcT7FDLGoGkaGo1G8fHxcVGS
	    JD+B5yaEABFBlmU0m02cn58LVVURi8Wa/f39n1zXhey67g/LwRhDrVb77e7u
	    7itjDLIsQ1GUZic/xljBtu2/Dg4OhGEYAIDe3l4Eg8FPnvKuCnt6en69vb39
	    o1qtinA4DE+VaZogouRzhf8mVjg9PT05OzsT0WjUV2uaJizLKrqu+0UI8Vah
	    1xRbW1vfNU37nkwmYVmWH9wL5PkREVRVxe7ubkkIAUVR0G63X8SzLGvRV+g4
	    zgvCRqNRPDw8XEwkEggGg1BV9c1/kiQJjuPAtm2cnZ2JWq2GZDIJIoKu6x0F
	    OI7zD6H3otVq4ejoSDw9PSEWi/ldRUQdCW9ubrC/vy8cx0EikYBhGB3xAMA5
	    9ztXvri4KJyenpaOj48Rj8fBGMPNzU1HR880TUO9Xhd9fX2wbRtXV1dd8UII
	    MMbAOUcgEIC8ublZ2tvbQyqVelOObp3barUQDAbfheecQ9M05PN5hEIhyOPj
	    47/HYrGvOzs74JxDURQQEbp1r/ft4eEB4XAYgUDgh/jXC0Lu6+v7Fo/Hv2Wz
	    WVEul3F5eYlIJNJ1dRER2u02Zmdnsb6+DlVVEQqFIElSV0KP1B8LWZYxMDBA
	    S0tLqFQqYnt725vFjkFM00Qul6PBwUGUy2VRrVahKApkWf454fOxCAQCmJqa
	    omw2K1ZXV3F/f/9GKRH5XacoCubm5mhoaEiUSiVomvYG742PV1LJ2xjesuWc
	    I51O08rKCo2OjkKSJBiGAc45OOewLAu2bft413UxPDxMy8vLv+RyOb8CHp5z
	    /pKw06bnnAMAZmZmaH5+njKZDGzbhqZpPvlzvGmaiEQiJ/Pz8zQ9PY3+/n7o
	    ug5d133yjgpfqzUMA+l0GouLi1QoFPzRsSzrxckSQsBxHFiWhZGREVpYWPhz
	    YmICoVAIqqr6Fem4S1+b67pgjGFsbIzS6bSoVCpYW1vzM35thmEgGo1+mZyc
	    RCaTERsbG2i1Wj4pFYvFd197SZJgmubner1+nM/nqdsoPG8wTdNwe3v7OZVK
	    nYTD4Y8Rvj60H8F7Cby5Fv+1/Q1H6QCdO3ZqMAAAAABJRU5ErkJggg==
	}
	image create photo cu::scale::img::forward -data {
	    iVBORw0KGgoAAAANSUhEUgAAABwAAAAWCAYAAADTlvzyAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oCCw4VAgGV67oAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
	    TVBXgQ4XAAADm0lEQVRIx71WTUsjWRQ9r6qSilViiIrxE0MGvwlRIomLZCHa
	    ItnP/IT5W/6ABnHjxm7BhRvBARURTIGJ4kIjMQRSVurV+5hFz8tEOxkzm75w
	    N8Wte+6575x6Rc7OzvArwxBCQEoJXdchpezrJc45DMPou74zNM/z8Pj4uF2t
	    ViGlhJQSQoieyRiD4ziyXq/3Vf8xDU3T4Lrut6enJ1SrVUxNTZFYLAbGGKSU
	    IIS8m1BKCdd14TiOHBoawvT0NLFtG5zz/hgKIRCJRL6Gw2E0m03c3t7Ki4sL
	    yTmHpmldp7QsC7quo9Fo4OrqSpZKJalpWl9sNSEEKKW/+77fZkApxenpqSyX
	    y6+6rrefq2y1WvB9H1JKaJqGWq2G4+NjWa/X22fbK41/ANsNVJimiXK5HKtU
	    KnJtbY3Ytt1m4Ps+fN9vr5sQgsHBQZyfn0vbtpHJZEgvUWmcczDGEATBu/Q8
	    D5qmgRCCk5MTWSqVJKUUnHNQSt/VUkrRbDYRiUTAOcfR0ZGsVCqSc46PaQgh
	    EAQB1Eo/CoQQgmg0CsdxcHd3J1Op1F8KpJugFNvr62vc39/LpaWl36LR6J1h
	    GBBCgBwcHODy8lLWarWfGnQ20nUdhmHg5eUFw8PD/6lKKSUMwwDnHI1GA8lk
	    EslkEmNjY8TwfR8PDw+o1+sIhUKfyppSiufnZ0Sj0b6M73keHMdBOp1GsVj8
	    sVJl6F5BCGmr13VdmKaJIAh6Aqp6z/PAGEOhUMDy8jIIIT8AGWOglEII0bWB
	    EAJvb2+IRCLY3d3F4eEhQqFQT0DOOVzXxfj4ODY3NzE5OUnC4fC/tmCMwff9
	    rufCGIPneUin09ja2voSCoW+7+/vy1ar1RVMMc9ms9jY2CADAwPtodsfb8Xw
	    IyDnHLZto1gsYnFxkagzVCr9yJBzjlgshmKxiEQiQZQVfrotlA/VFEIImKaJ
	    +fl5bG9v12Ox2HAQBG0FqgE7gSzLQiqVws7ODtF1Haq+6/WkfKhsMTMzg1wu
	    h9XVVcI5f+dRVd9qtcA5h2maSCQSKBQKmJubI77v9wQDAENN3Gw2EY/Hsb6+
	    jkwmg5GREdLJohNQrXNiYgLZbBaZTOaLaZrfPc/7/AJmjP1JKUU6nUY+n8fs
	    7CwhhKCXKBTDfD6PXC73NR6P/6HE0k+Qvb09vL6+ytHRUWJZ1qdmFkLg5uZG
	    LiwstKX+v34xLMuCZVlEmfXTC1TTsLKy0nd9V9H8yvgbJDT9P418Gc4AAAAA
	    SUVORK5CYII=
	}
	image create photo cu::scale::img::play -data {
	    iVBORw0KGgoAAAANSUhEUgAAABwAAAAWCAYAAADTlvzyAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oLDxEiJaGQ744AAAJRSURBVEjHvVY9TyJRFD13GIQhCMlECyAUxDGa
	    IVopGqgoaGzotqPe3+T+hf0HWlHSmkBB1JAYE7QxWMC8j3l3mx0Sd2EAZfcl
	    r3r3zbnn3nPuG+r1evify2bm2ACtNZLJJFbFrQ1ojFl6SES4v7/n/f19cl0X
	    RIS4+K0wDIIADw8P/Pr6imKxSPl8HmEYgplBRNtn6DgOAOD9/R2TyYRzuRwO
	    Dw/JsqxPsbWYGXFbCIHZbDZPYDKZoNvt8svLC2zbxqr7f+6VDIUQEELMy0dE
	    yGazuLu748FggFqtRul0em22dhiGy+lbFqSUUEp96JeUEjs7O0gkEri9vWXP
	    8+B5HkUJxekiVjTMDCklpJQLBcLMyOVyGI1GeHp64mq1+t113R/JZHIp45WA
	    4/E4tkTMjEQiAQC4ubm5LpVK157noVAokGVZm6nUsiz0+33s7e2tZXwhBIbD
	    IZ6fn9Fut5HJZP66FwtojIExBkqppYBRz4IggJQS5+fnOD09hW3bWKSP2JIa
	    Y6C1hpRyKaAxBtPpFPl8Hq1WC5VKhVKp1NwGG/dQa40gCBaeh2EIrTV830ez
	    2TzIZrOPURKfsgURQWsNIcRCMMdx0G634fs+aa0R962NGCqlPgClUikcHx/j
	    6uqK0uk0pJTbmaWRYCIg27ZRLpfRaDTg+z4ppTYCW4uhUgpKKbiui4uLC5yd
	    nf3c3d39tqjMX2bIzJjNZri8vES9XkepVKJo3P2TB1hKiU6ng6OjozfHcdzI
	    l196gFeNrZOTkwMievwq0Fq2+L0et/kT9Qv0dqaa6yjGrAAAAABJRU5ErkJg
	    gg==
	}
	image create photo cu::scale::img::pause -data {
	    iVBORw0KGgoAAAANSUhEUgAAABwAAAAWCAYAAADTlvzyAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oCCxECKb/QciEAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
	    TVBXgQ4XAAADtElEQVRIx22WO24cRxCGv+pZEiChgLIeAJXQEQNRkenIgm4g
	    hTqA4NvJBgwDDmyfwAJsORIPoICEzYCwRHI5/ajfQXfPzJKaBRazg96u+l/V
	    YwDv3v2BmWEGmIEghIEhGJhhAAbtDkkAmAXao3oJNN3UH7lkXAKEBCsAd8cs
	    EMzAhJlx/u8/WAi1kbo7ktjZ2eHevXsAXH7+j5v1TW1yUdUwHj18SFHBJVze
	    OlEvKMwcmWFmbG+v+PW337W+WTcA9dvdOTo64vnz7wzB+/d/68OHD4QQWF47
	    Ozt8/+bNkxzTmbs3RiriVaXIQVb3NSNGePHixcnFxcVTa01IYrVa8fjxY+uU
	    Hh4e2t7ennLOmM10f3X/vsU04i7cfaJTsEQIwQwJBsTV1dXT9XrdtKqbhRAY
	    x/G1XD8AjOP4+urqqm06X5fb23/K+baimzVHWiAkIDU5JFJKxBhZIhyGgZzz
	    W5cbQM75bUqJUsoGwpTSsSNcQvKpoLTQMARHVC1ktwtW07WCyIVqQWKMXyqI
	    3JE77o4LrDm3FpSDAqHrKKN2nulsmBnuXp83XVJKpBTvUBrHkZgzJecpKkIz
	    Qnl7FIQKBBMxRsYxVncxa1hKqd0DpWTGMbZYMSGMKYI0NdKbnmKh/pFPmp2f
	    n9NNMy2XePDgAUWOYXz69JmLi4t5aHRKY9oYEHcLunCEmajDxTg5OZkC3lFe
	    X1+zu7vLs2dHgHF6esrHjx/Z3d2lz5g6EC55+eol7o1Pq4Csx4Jp9DhOdaSk
	    apBpjFmjc6bKXZRSyDnN69r/vZlmRqg5h+rdaRa3lEKMcYPSlBLuZdrAvZBS
	    YhjCxjwtXmrTzTHiNqUNkeOY6gjLOXP7SimRc5kQlpJJKRGCbazLOYP8jnuZ
	    EEo1W6F11LJ0x+4xVvrcwSDnMmV1iaOUQlm4dAG+F6QeIUXTkC6lIllq2DNY
	    3LG2cUe4XNcBLAtqibAvwpgQ5py/iLCUpo+YdLaFD8zqOdllYgO71YLFwUzT
	    mdVNk1K6o2FHzgIht7ZdbW1VJlpe75jG5f0sx9rvm5ubL7o0xnmUxRi5Xq/Z
	    6gZriLa3Cq4am81WGqW5eAt8HbIxJo6Pv5mc2idFKYWDg4OJqq8PDkg5MwzD
	    RsGwGhhjpNRo7LeiT6aj/MeffiaYEYJhzHRN8Zxmk9V1FiA47iK7cHG8mTbt
	    hzD8JdOThWn2FwjLy1UIv0A7ilAVnvom0HPWjeEU5PVly0LA4NTgTLBvcObS
	    cZFetUNpvyE7E+z/D4awVLqI3zBgAAAAAElFTkSuQmCC
	}
    }
}

if 0 {
    package require compass_utils
    wm geometry . 800x600+200+200
    grid [cu::scale .f -left_text 00:00 -right_text 23:12 -title [_ "animation"] -from 0 -to 1392 -label_style time \
	    -play_handler kk] -sticky new
    grid [ttk::button .b -text Exit -command exit]
    grid columnconfigure . 0 -weight 1
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
	focus $text
	$text delete 1.0 end
	$text insert end $txt
	$text tag add sel 1.0 end-1c
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
# cu::dater
################################################################################

snit::widget cu::dater {
    option -font "Verdana 7"
    option -topframecolor "#7b9cc1"
    option -date ""
    option -select_handler ""
    option -events_handler ""
    option -grab 0
    option -detailed_view 0

    hulltype frame

    variable month
    variable year
    variable date_ini ""
    variable date_end ""
    variable last_date ""
    
    constructor args {

	$self configurelist $args
	set days ""
	set d [clock seconds]
	set daynum [clock format $d -format %u]
	foreach i [rangeF [expr {1-$daynum}] [expr {7-$daynum}]] {
	    set day [clock format [clock scan "$i days" \
		        -base $d] -format %A -locale system]
	    if { $options(-detailed_view) } {
		lappend days $day
	    } else {
		lappend days [string index $day 0]
	    }
	}
	$self create_images

	$hull configure -background black

	frame $win.f -background $options(-topframecolor)
	label $win.f.l1 -image fletxa-left-16 -padx 2 -pady 1 -background \
	    $options(-topframecolor)
	frame $win.f.central
	label $win.f.central.l1 -textvariable [myvar month] -background $options(-topframecolor) \
	    -foreground white -font $options(-font)
	label $win.f.central.l2 -textvariable [myvar year] -background $options(-topframecolor) \
	    -foreground white -font $options(-font)
	label $win.f.l3 -image fletxa-right-16 -padx 4 -pady 1 -background \
	    $options(-topframecolor) -font $options(-font)

	grid $win.f.central.l1 -row 0 -column 0
	grid $win.f.central.l2 -row 0 -column 1

	grid $win.f.l1 -sticky w -row 0 -column 0
	grid $win.f.central -sticky ew -row 0 -column 2 
	grid $win.f.l3 -sticky e -row 0 -column 4
	grid columnconfigure $win.f "1 3" -weight 1
	grid $win.f -columnspan 7 -sticky ew

	bind $win.f.l1 <1> [mymethod configure -date "-1 month"]
	bind $win.f.central.l1 <1> [mymethod contextual month %W %X %Y]
	bind $win.f.central.l2 <1> [mymethod contextual year %W %X %Y]
	bind $win.f.l3 <1> [mymethod configure -date "+1 month"]

	set idx 0
	foreach i $days {
	    label $win.l$idx -text $i -background white -padx 0 -pady 0 -anchor w \
		-font $options(-font)
	    grid $win.l$idx -row 1 -column $idx -padx 0 -pady 0 -sticky ew
	    incr idx
	}
	ttk::separator $win.s -orient horizontal
	grid $win.s -columnspan 7 -sticky ew

	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 3 -weight 1

	$self configurelist $args
	if { $options(-date) eq "" } {
	    $self configure -date ""
	}
    }
    onconfigure -date { value } {
	if { $value eq "" } {
	    set options(-date) [clock format [clock seconds] -format %Y-%m-%d]
	} elseif { [regexp {^[-+]} $value] } {
	    set err [catch { clock scan $value -base [clock scan $options(-date)] } date]
	    if { $err } {set date [clock seconds] }
	    set options(-date) [clock format $date -format %Y-%m-%d]
	} else {
	    set err [catch { clock scan $value } date]
	    if { $err } {set date [clock seconds] }
	    set options(-date) [clock format $date -format %Y-%m-%d]
	}
	set err [catch { clock scan $options(-date) } date]
	if { $err } {set date [clock seconds] }

	set err [catch { clock scan $last_date } last_dateS]
	if { $err } { set last_date "" }

	if { $last_date ne "" && abs(($date-$last_dateS)/86400)<25 } {
	    set err [catch { $self _move_to_day } errstring]
	} else {
	    set err 1
	}
	if { $err } {
	    $self fill_days
	}
	set last_date $options(-date)
    }
    onconfigure -grab { value } {
	set options(-grab) $value
	if { $value } {
	    grab $win
	    bind [winfo toplevel $win] <ButtonRelease-1> \
		[mymethod button_release1 "" %X %Y]
	} else {
	    grab release $win
	    bind [winfo toplevel $win] <ButtonRelease-1> ""
	}
    }
    onconfigure -topframecolor { value } {
	set options(-topframecolor) $value
	foreach w [list $win.f $win.f.l1 $win.f.central.l1 $win.f.central.l2 \
		$win.f.l3] {
	    $w configure -background $options(-topframecolor)
	}
    }
    onconfigure -font { value } {
	if { $value eq "" } {
	    set value "Verdana 7"
	}
	set options(-font) $value
   }
    method select_day { curr_date } {
	if { $options(-select_handler) ne "" } {
	    set cmd $options(-select_handler)
	    lappend cmd $curr_date
	    uplevel #0 $cmd
	}
    }
    method set_today {} {
	set date [clock format [clock seconds] -format %Y-%m-%d]
	if { $date eq [$self cget -date] } {
	    after idle [mymethod select_day $date]
	} else {
	    $self configure -date $date
	}
    }
    method button_release1 { curr_date x y } {
	if { $options(-grab) } {
	    if { $x < [winfo rootx $win] || $y < [winfo rooty $win] ||
		$x > [winfo rootx $win]+[winfo width $win] ||
		$y > [winfo rooty $win]+[winfo height $win] } {
		$self select_day ""
		return -code break
	    }
	}
	if { $curr_date ne "" } {
	    $self configure -date $curr_date
	}
    }
    method _move_to_day {} {
	set err [catch { clock scan $options(-date) } date]
	if { $err } {set date [clock seconds] }

	set days [expr {($date-[clock scan $date_ini])/86400}]
	set idx $days
	set w $win.b$idx
	if { ![winfo exists $w] } {
	    error "error: day is not visualized"
	}
	if { [winfo exists $w.l] } { set w $w.l }
	focus $w
    }
    method fill_days {} {
		
	set err [catch { clock scan $options(-date) } date]
	if { $err } {set date [clock seconds] }
	
	set month [clock format $date -format "%B " \
		-locale system]
	set year [clock format $date -format "%Y"]

	set day1 [clock format $date -format %Y-%m-01]
	set weekday1 [clock format [clock scan $day1] -format %u]
	set range ""

	if { $weekday1 == 1 } {
	    set minus 7
	} else {
	    set minus [expr {$weekday1-1}]
	}
	set date_ini [clock format [clock scan "-$minus days" -base [clock scan $day1]] \
		    -format "%Y-%m-%d"]
	set d1 [scan [clock format [clock scan "-$minus days" -base [clock scan $day1]] \
		    -format %d] %d]
	set d2 [scan [clock format [clock scan "-1 days" \
		        -base [clock scan $day1]] -format %d] %d]
	eval lappend range [rangeF $d1 $d2]

	set d3 [scan [clock format [clock scan "+1 month -1 days" \
		        -base [clock scan $day1]] -format %d] %d]
	eval lappend range [rangeF 1 $d3]
	set dayend [clock format $date -format %Y-%m-$d3]
	set weekdayend [clock format [clock scan $dayend] -format %u]
	set date_end [clock format [clock scan "+1 month -1 days" -base [clock scan $day1]] \
		-format "%Y-%m-%d"]

	if { $weekdayend < 7 } {
	    set d4 [scan [clock format [clock scan "+[expr {7-$weekdayend}] days" \
		            -base [clock scan $dayend]] -format %d] %d]
	    eval lappend range [rangeF 1 $d4]
	    set date_end [clock format [clock scan "+[expr {7-$weekdayend}] days" \
		    -base [clock scan $dayend]] -format "%Y-%m-%d"]
	} else { set d4 0 }
	if { [llength $range] <= 35 } {
	    eval lappend range [rangeF [expr {$d4+1}] [expr {$d4+7}]]
	    set date_end [clock format [clock scan "+[expr {7-$weekdayend+7}] days" \
		    -base [clock scan $dayend]] -format "%Y-%m-%d"]
	}
	set date_month [scan [clock format $date -format %m] %d]
	set date_day [scan [clock format $date -format %d] %d]
	set today_month [scan [clock format [clock seconds] -format %m] %d]
	set today_day [scan [clock format [clock seconds] -format %d] %d]

	if { $options(-events_handler) ne "" } {
	    set events_dict [uplevel #0 $options(-events_handler) \
		    [list $date_ini $date_end]]
	} else { set events_dict "" }

	set rel_month -1
	set curr_year [scan [clock format [clock scan $date_ini] -format "%Y"] %d]
	foreach "row col idx" [list 3 0 0] break
	foreach i $range {
	    if { $i == 1 } {
		incr rel_month
		set curr_month [expr {$date_month+$rel_month}]
		if { $curr_month > 12 } { incr curr_month -12 }
		if { $curr_month == 1 } { incr curr_year }
	    } else {
		set curr_month [expr {$date_month+$rel_month}]
	    }
	    destroy $win.b$idx

	    if { $options(-detailed_view) } {
		frame $win.b$idx -background cornsilk2 -bd 0 -takefocus 0 \
		    -width 70 -height 70
		grid propagate $win.b$idx 0
		set label $win.b$idx.l
	    } elseif { $today_month != $curr_month || $today_day != $i } {
		set label $win.b$idx
	    } else {
		frame $win.b$idx -background brown -bd 0 -takefocus 0
		set label $win.b$idx.l
	    }
	    label $label -text $i -background white -relief flat -bd 0 \
		-padx 4 -pady 1 -takefocus 1 -font $options(-font)
	    if { $rel_month != 0 } {
		$label configure -foreground grey
	    }
	    if { $options(-detailed_view) } {
		$label configure -padx 3 -pady 0 -background cornsilk2 -font $options(-font)
		grid $label -padx 1 -pady 1 -sticky nw

	    } elseif { $today_month == $curr_month && $today_day == $i } {
		$label configure -padx 3 -pady 0 -font $options(-font)
		grid $label -padx 1 -pady 1
	    } else {
		grid $label -padx 0 -pady 0
	    }
	    
	    grid $win.b$idx -row $row -column $col -padx 0 -pady 0 -sticky nsew
	    grid columnconfigure $win $col -weight 1
	    grid rowconfigure $win $row -weight 1

	    if { $options(-detailed_view) } {
		grid configure $win.b$idx -padx "0 1" -pady "0 1"
	    }

	    set curr_date [format "%04d-%02d-%02d" $curr_year $curr_month $i]
	    if { [dict exists $events_dict $curr_date] } {
		set txtList ""
		foreach i [dict get $events_dict $curr_date] {
		    set txt ""
		    if { [dict exists $i start] } {
		        append txt "[dict get $i start] "
		        append txt "[dict get $i end]\n"
		    }
		    append txt "[dict get $i text]"
		    lappend txtList $txt
		}
		tooltip $label [join $txtList \n]
		if { $options(-detailed_view) } {
		    set jdx 0
		    foreach j $txtList {
		        label $win.b$idx.l$jdx -anchor w -background cornsilk2 \
		            -font $options(-font)
		        tooltip $win.b$idx.l$jdx $j
		        $win.b$idx.l$jdx configure -text $j
		        grid $win.b$idx.l$jdx -sticky nwe
		        incr jdx
		    }
		    grid columnconfigure $win.b$idx 0 -weight 1
		} else {
		    $label configure -foreground red
		}
	    }

	    bind $label <ButtonRelease-1> [mymethod button_release1 $curr_date %X %Y]
	    bind $label <Double-1> [mymethod select_day $curr_date]
	    bind $label <space> [mymethod select_day $curr_date]
	    bind $label <Return> [mymethod select_day $curr_date]
	    bind $label <Escape> [mymethod select_day ""]
	    bind $label <FocusIn> [list $label configure -background orange]
	    if { $options(-detailed_view) } {
		bind $label <FocusOut> [list $label configure -background cornsilk2]
	    } else {
		bind $label <FocusOut> [list $label configure -background white]
	    }

	    foreach "key d" [list Left "-1day" Right "+1day" Up "-1week" \
		    Down "+1week" Prior "-1month" Next "+1month"] {
		bind $label <Key-$key> [mymethod configure -date $d]
	    }
	    if { $rel_month == 0 && $date_day == $i } { focus $label }
	    
	    incr col
	    if { $col > 6 } {
		set col 0
		incr row
	    }
	    incr idx
	}
	while { [winfo exists $win.b$idx] } {
	    destroy $win.b$idx
	    incr idx
	}
	destroy $win.s2
	ttk::separator $win.s2 -orient horizontal
	grid $win.s2 -columnspan 7 -sticky ew
	
	destroy $win.fb
	frame $win.fb
	ttk::button $win.fb.b -text [_ "Today"] -width 5 -command [mymethod set_today]
	grid $win.fb.b
	grid $win.fb -columnspan 7 -pady 0 -sticky ew
	grid columnconfigure $win.fb 0 -weight 1

	if { [winfo exists $win.f] } {
	    grid forget $win.f
	    grid $win.f -columnspan 7 -sticky ew
	}
    }
    method contextual { month_year label x y } {

	destroy $label.m
	set menu [menu $label.m -tearoff 0]
	if { $month_year eq "month" } {
	    set m0 [scan [clock format [clock scan $options(-date)] \
		        -format %m] %d]
	    set idx 0
	    foreach m [concat [rangeF [expr {1-$m0}] -1] [rangeF 0 [expr {12-$m0}]]] {
		set date [clock format [clock add [clock scan $options(-date)] $m months] \
		        -format %Y-%m-%d]
		set name [clock format [clock scan $date] -format %B -locale system]
		$menu add command -label $name -command [mymethod configure \
		        -date $date]
		if { $m == 0 } { set entry $idx }
		incr idx
	    }
	} else {
	    set year [scan [clock format [clock scan $options(-date)] \
		        -format %Y] %d]
	    set idx 0
	    foreach ye [rangeF [expr {$year-5}] [expr {$year+5}]] {
		set date [clock format [clock scan $options(-date)] \
		        -format $ye-%m-%d]
		$menu add command -label $ye -command [mymethod configure \
		        -date $date]
		if { $ye == $year } { set entry $idx }
		incr idx
	    }
	}
	$self configure -grab 0
	tk_popup $menu $x $y $entry
	if { $::tcl_platform(platform) ne "windows" } {    
	    bind $menu <Unmap> "set ::pp done"
	    vwait ::pp
	}
	after idle [mymethod configure -grab 1]
    }
    method create_images {} {
	if { [lsearch [image names] fletxa-left-16] == -1 } {
	    image create photo fletxa-left-16 -data {
		R0lGODdhEAAQAMQAALW+qI6ZfcPIuJepfZ+whqu7mbbHosvXupWndZutfKCwhqS0iqW3jqKz
		jbbEoZaocZytfKS0i///////////////////////////////////////////////////////
		/ywAAAAAEAAQAAAFryAABuJIkkIwEGAhioYBHqIYBAiSKIqiEAvDNM4RBMgDgYooRgzTOEcQ
		IA80ihLDNM4RBAiSjIoEShLTOEcQIA+kKIokSQzTOEcQIEiiKBIoSQvDNM4RBAiSKJIkEQvD
		NM4RBAiSKKAiSdLCMI1zBAGCJIqiSJLEMI1zBAHygJCiKIokSUzjHEGAPBCoiKLEMI1zBAGC
		JKNCLAzTOEcQDARYiKJhgIcoBgAYiCNJCiEAOw==
	    }

	}
	if { [lsearch [image names] fletxa-right-16] == -1 } {
	    image create photo fletxa-right-16 -data {
		R0lGODdhEAAQAMQAAMPIuI6ZfbW+qMvXurbHoqu7mZ+whpepfbbEoaKzjaW3jqS0iqCwhput
		fJWndaS0i5ytfJaocf//////////////////////////////////////////////////////
		/ywAAAAAEAAQAAAFryAABuJIkkIADqJIEGAhisYRBAOSKMpiMAzDNI4TBAOSKMoDMqIIRU4Q
		DEiiKNIoQpETBAOSKJIkjQzYOE4QDEiiKJIkMQwDRU4QDEiiKIskSSDDNI4TBAOSKMpiSJLE
		NI4TBAOSKMoigZLEMI3jBMGAJIoiSRLDMI3jBMGAJIoEShLDMAwUOUEwIImiSCAjilDkBMGA
		JIqyGCPTOE4QgIMoEgRYiKJxBAAYiCNJCiEAOw==
	    }
	}
    }
}

snit::widget cu::dater_entry {
    option -date ""
    option -font
    option -topframecolor
    option -textvariable ""
    option -editmode 0
    option -takefocus 0 ;# option used by the tab standard bindings
    option -addtodaybutton 0
    option -prefertodayword 0
    option -state
    
    hulltype frame

    variable entry
    variable oldGrab
    variable grabStatus
    variable oldFocus
    variable isCreated
    variable today

    delegate method * to entry
    delegate option * to entry

    constructor args {

	set entry [ttk::entry $win.e]

	set isCreated 0
	$self configurelist $args        
	$self create_images

	ttk::button $win.b -image date-16 -style Toolbutton \
	    -command [mymethod post_dater_toggle]
	
	if { $options(-addtodaybutton) } {
	    ttk::button $win.b2 -text [_ Today] -command [mymethod set_today] -style Toolbutton
	}

	bindtags $entry [concat $win [bindtags $entry]]

	if { $options(-addtodaybutton) } {
	    grid $entry $win.b $win.b2 -sticky w
	} else {
	    grid $entry $win.b -sticky w
	}
	grid configure $entry -sticky ew
	grid columnconfigure $win 0 -weight 1

	cu::add_contextual_menu_to_entry $entry init
	bind $win <FocusIn> [list focus $entry]

	if { $options(-prefertodayword) } {
	    set today [_ "Today"]
	} else {
	    set today [clock format [clock seconds] -format "%Y-%m-%d"]
	}
	
	bind $win <Up> "[mymethod change_date 1 day]; break"
	bind $win <Down> "[mymethod change_date -1 day]; break"
	bind $win <Shift-Up> "[mymethod change_date 1 month]; break"
	bind $win <Shift-Down> "[mymethod change_date -1 month]; break"
	bind $win <Control-Shift-Up> "[mymethod change_date 1 year]; break"
	bind $win <Control-Shift-Down> "[mymethod change_date -1 year]; break"

	set isCreated 1
	$self configurelist $args
    }
    destructor {
	if { $options(-textvariable) ne "" } {
	    trace remove variable $options(-textvariable) write \
		"[mymethod check_value];#"
	}
    }
    onconfigure -prefertodayword { value } {
	set options(-prefertodayword) $value
	
	if { $options(-prefertodayword) } {
	    set today [_ "today"]
	} else {
	    set today [clock format [clock seconds] -format "%Y-%m-%d"]
	}
    }
    onconfigure -date { value } {
	set options(-date) $value
	
	if { !$isCreated } { return }

	if { $options(-textvariable) ne "" } {
	    set $options(-textvariable) $value
	} else {
	    $entry delete 0 end
	    $entry insert end $value
	}
    }
    oncget -date {
	set options(-date) [$entry get]
	return $options(-date)
    }
    onconfigure -textvariable { value } {
	set options(-textvariable) $value
	
	if { !$isCreated } { return }
	
	$entry configure -textvariable $value
	trace add variable $value write "[mymethod check_value];#"
    }
    onconfigure -state { value } {
	set options(-state) $value
	
	$win.e configure -state $value
	$win.b configure -state $value
    }
    method is_today { date } {
	return [expr {$date eq $today}]
    }
    method set_today {} {
	$self configure -date $today
    }
    method change_date { args } {
	set date [$self cget -date]
	set date [clock format [clock add [clock scan $date] {*}$args] -format "%Y-%m-%d"]
	$self configure -date $date
    }
    method check_value {} {
	upvar #0 $options(-textvariable) v
	
	if { !$options(-editmode) } {
	    if { $v in [list "now()" $today] } {
		set v $today
	    } elseif { [regexp {now\(\)\s+(\S.*)} $v {} add] } {
		set err [catch { clock add [clock seconds] {*}$add } c]
		if { !$err } {
		    set v [clock format $c -format "%Y-%m-%d"]
		}
	    }
	}
    }
    method post_dater_toggle {} {
	if { ![winfo exists $win.dt] } {
	    $self post_dater
	} else {
	    $self end_post_dater $win.dt ""
	}
    }
    method post_dater {} {
	destroy $win.dt
	set post_dater [toplevel $win.dt -bd 1 -relief solid]
	wm withdraw $win.dt
	update idletasks
	wm overrideredirect $win.dt 1
	set h [expr {[winfo rooty $win.e]+[winfo height $win.e]}]
	cu::dater $win.dt.d -date [$self cget -date] -select_handler \
	    [mymethod end_post_dater $win.dt] -grab 1
	if { [info exists options(-font)] } {
	    $win.dt.d configure -font $options(-font)
	}
	if { $options(-topframecolor) ne "" } {
	    $win.dt.d configure -topframecolor $options(-topframecolor)
	}
	grid $win.dt.d
	wm geometry $win.dt +[winfo rootx $win.e]+$h
	
	switch -- [tk windowingsystem] {
	    x11 - win32 { wm transient $win.dt [winfo toplevel $win] }
	}
	wm attribute $win.dt -topmost 1
	
	update idletasks
	wm deiconify $win.dt
	
	set oldGrab [grab current $win]
	if { $oldGrab ne "" && [winfo exists $oldGrab] } {
	    set grabStatus [grab status $oldGrab]
	    grab release $oldGrab
	}
	set oldFocus [focus -lastfor $win]
	raise $win.dt
	grab $win.dt
	focus $win.dt
    }
    method end_post_dater { w date } {
	if { $date ne "" } {
	    set t [clock format [clock seconds] -format "%Y-%m-%d"]
	    if { $date eq $t } {
		set date $today
	    }
	    $self configure -date $date
	}
	destroy $w
	
	update
	if {[string compare $oldGrab ""]} {
	    if {[string compare $grabStatus "global"]} {
		if { [winfo exists $oldGrab] && [winfo ismapped $oldGrab] } { grab $oldGrab }
	    } else {
		if { [winfo exists $oldGrab] && [winfo ismapped $oldGrab] } { grab -global $oldGrab }
	    }
	}
	if { $oldFocus ne "" } {
	    focus -force $oldFocus
	}
    }
    method create_images {} {
	if { [lsearch [image names] date-16] == -1 } {
	    image create photo date-16 -data {
		R0lGODlhEAAQAIQAAASC/AQCBPzerPyqXMRaBIQCBISChPz+/KSipMTCxPz+
		BMTCBPwCBPz+xPzCxMQCBISCBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
		AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAVxICCOQGCe
		JjkGwkC8RFEEavkax2G8dB0QuRyhhzoBg8MSYsncJXKJZIDZHCoWP1ogGIwG
		rtnSgUFmHLyNRHhrdpjRamnO/SYkromHdnxwnwkKVxByZW8DgQsQM2JcfwZX
		O0MBCZSVBgMuLzJaRZ0pfiEAIf5oQ3JlYXRlZCBieSBCTVBUb0dJRiBQcm8g
		dmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwgcmlnaHRz
		IHJlc2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==
	    }
	}
    }
}

################################################################################
#    view_csv
################################################################################

snit::widgetadaptor cu::view_csv {
    option -file
    option -encoding ""
    option -csv_sep ";"

    variable update_pending 0
    
    delegate method * to hull
    delegate option * to hull

    constructor args {
	
	package require csv
	package require fulltktree
	fulltktree $win -width 600 -height 600 \
	    -compass_background 0 -showlines 0 \
	    -indent 0 -expand 0 -sensitive_cols all
	installhull $win 
	
	$self configurelist $args
	
	bind $win <Configure> [mymethod update]
    }
    onconfigure -file {value} {
	set options(-file) $value
	set update_pending 1
    }
    onconfigure -encoding {value} {
	set options(-encoding) $value
	set update_pending 1
    }
    onconfigure -csv_sep {value} {
	set options(-csv_sep) $value
	set update_pending 1
    }
    method update {} {
	if { !$update_pending } { return }
	
	set fin [open $options(-file) r]
	if { $options(-encoding) ne "" } {
	    fconfigure $fin -encoding $options(-encoding)
	}
	gets $fin headers
	set cols ""
	foreach h [csv::split $headers $options(-csv_sep)] {
	    lappend cols [list 15 $h left text 1]
	}
	$self item delete all
	$self configure -columns $cols
	
	while { [gets $fin line] != -1 } {
	    $self insert end [csv::split $line $options(-csv_sep)]
	}
	close $fin
	set update_pending 0
    }
}

################################################################################
#    check_listbox
################################################################################

snit::widgetadaptor cu::check_listbox {
    option -values
    option -add_new_button 0
    option -permit_rename 0
    option -permit_delete 0

    delegate method * to hull
    delegate option * to hull

    variable tree
    variable is_built
    variable itemDict ""
    variable exclusiveDict ""
    variable prev_selection ""
    variable add_item ""
    
    constructor args {
	
	package require fulltktree
	
	set is_built 0
	
	set columns [list \
		[list 25 Groups left imagetext 1] \
		]
	
	installhull using ::fulltktree -width 130 -height 150 \
	    -columns $columns -expand 1 \
	    -selectmode extended -showlines 1 -showrootlines 0 -indent 1 -showbutton 1 \
	    -showheader 0 -sensitive_cols all -buttonpress_open_close 0 \
	    -have_vscrollbar 1 \
	    -contextualhandler_menu [mymethod contextual_menu]  \
	    -editbeginhandler [mymethod edit_name_begin] \
	    -editaccepthandler [mymethod edit_name_accept] \
	    -selecthandler [mymethod _selection]
	set tree $self
	
	$tree state define check
	$tree state define on
	
	set is_built 1
	$self configurelist $args
    }
    onconfigure -values { values } {
	set options(-values) $values
	
	if { !$is_built } { return }
	$self _update_from_values
    }
    onconfigure -add_new_button { value } {
	set options(-add_new_button) $value
	
	if { !$is_built } { return }
	$self _update_from_values
    }
    oncget -values {
	foreach i [range 0 [llength $options(-values)]] {
	    set elm [lindex $options(-values) $i]
	    foreach j [range [llength $elm] 4] { lappend elm "" }
	    lassign $elm name state parentName open_close
	    if { $parentName ne "" } {
		set key $parentName
		lappend key $name
	    } else {
		set key $name
	    }
	    set item [dict get $itemDict $key]
	    if { [$tree item state get $item on] } {
		lset elm 1 on
	    } else {
		lset elm 1 off
	    }
	    if { $parentName ne "" } {
		if { [$tree item state get $item open] } {
		    lset elm 3 open
		} else {
		    lset elm 3 close
		}
	    }
	    lset options(-values) $i $elm
	}
	return $options(-values)
    }
    method _update_from_values {} {
	$tree item delete all
	set itemDict ""
	set exclusiveDict ""
	foreach value $options(-values)  {
	    $self _insert end $value
	}
	if { $options(-add_new_button) } {
	    $self _create_add_button
	}
    }
    method contextual_menu { - menu item selection } {
	
	$menu add command -label [_ "Activate"] -command [mymethod set_state $selection on]
	$menu add command -label [_ "Dectivate"] -command [mymethod set_state $selection !on]
	
	set children [$tree item children [$tree item parent $item]]
	set ipos [lsearch -exact $children $add_item]
	if { $ipos != -1 } {
	    set children [lreplace $children $ipos $ipos]
	}
	if { [lsearch -exact $children $item] > 0 } {
	    $menu add command -label [_ "Move up"] -command [mymethod move_item $item up]
	}
	if { [lsearch -exact $children $item] < [llength $children]-1 } {
	    $menu add command -label [_ "Move down"] -command [mymethod move_item $item down]
	}
	$menu add separator
	
	if { $options(-permit_rename) } {
	    $menu add command -label [_ "Rename"] -command [mymethod rename [lindex $selection 0]]
	}
	if { $options(-add_new_button) } {
	    $menu add command -label [_ "Add"] -command [mymethod add_new_item]
	}
	if { $options(-permit_delete) } {
	    $menu add separator
	    $menu add command -label [_ "Delete"] -command [mymethod delete_items $selection]
	}
    }
    method rename { { item "" } } {
	if { $item ne "" } {
	    $tree selection clear
	    $tree selection add $item
	    $tree activate $item
	    $tree see $item
	}
	focus [$tree givetreectrl]
	update
	event generate $tree <F2>
    }
    method _create_add_button {} {
	set b [$tree givetreectrl].message
	destroy $b
	button $b -text [_ "Add"] \
	    -foreground blue -background white -bd 0 -cursor hand2 \
	    -command [mymethod add_new_item]
	set font [concat [font actual [$b cget -font]] -underline 1]
	$b configure -font $font
	
	set add_item [$tree insert end ""]
	$tree item style set $add_item 0 window
	$tree item element configure $add_item 0 e_window -destroy 1 \
	    -window $b
    }
    method add_new_item {} {
	
	foreach item [$tree item children 0] {
	    if { [$tree item style set $item 0] eq "window" } {
		$tree item delete $item
		break
	    }
	}
	set idx 1
	while { [lsearch -index 0 $options(-values) [_ "Unnamed%d" $idx]] != -1 } {
	    incr idx
	}
	set value [list [_ "Unnamed%d" $idx] on "" open]
	lappend options(-values) $value
	set item [$self _insert end $value]
	if { $options(-add_new_button) } {
	    $self _create_add_button
	}
	$tree selection clear
	$tree selection add $item
	$tree activate $item
	$tree see $item
	#focus $tree
	focus [$tree givetreectrl]
	update
	event generate $tree <F2>
    }
    method edit_name_begin { args } {
	if { $options(-permit_rename) == 0 } {
	    return 0
	}
	return 1
    }
    method edit_name_accept { args } {
	lassign $args - item col text
	
	if { ![regexp {[-\w.]{2,}} $text] } {
	    tk_messageBox -message [_ "Name can only contain letters, digits, -_.'"] -parent $win
	    return
	}
	$tree item text $item 0 $text
	set key [dict get [dict_inverse $itemDict] $item]
	set idx 0
	foreach i $options(-values) {
	    lassign $i name state parentName open_close
	    if { $parentName ne "" } {
		set k $parentName
		lappend k $name
	    } else {
		set k $name
	    }
	    if { $k eq $key } {
		lset options(-values) $idx 0 $text
		dict unset itemDict $key
		if { $parentName ne "" } {
		    set k $parentName
		    lappend k $text
		} else {
		    set k $text
		}
		dict set itemDict $k $item
		break
	    }
	    incr idx
	}
    }
    method move_item { item up_down } {
	
	set key [dict get [dict_inverse $itemDict] $item]
	set ipos [$self give_pos_in_values $key]
	
	switch $up_down {
	    up {
		set sibling [$tree item prevsibling $item]
		set key_sibling [dict get [dict_inverse $itemDict] $sibling]
		set ipos_sibling [$self give_pos_in_values $key_sibling]

		set v [lindex $options(-values) $ipos]
		lset options(-values) $ipos [lindex $options(-values) $ipos_sibling]
		lset options(-values) $ipos_sibling $v
		
		$tree item prevsibling $sibling $item
		
	    }
	    down {
		set sibling [$tree item nextsibling $item]
		set key_sibling [dict get [dict_inverse $itemDict] $sibling]
		set ipos_sibling [$self give_pos_in_values $key_sibling]

		set v [lindex $options(-values) $ipos]
		lset options(-values) $ipos [lindex $options(-values) $ipos_sibling]
		lset options(-values) $ipos_sibling $v

		$tree item nextsibling $sibling $item
	    }
	}
    }
    method give_pos_in_values { key } {
	set ipos 0
	foreach i $options(-values) {
	    lassign $i name state parentName open_close
	    if { $parentName ne "" } {
		set k $parentName
		lappend k $name
	    } else {
		set k $name
	    }
	    if { $k eq $key } {
		return $ipos
	    }
	    incr ipos
	}
	return -1
    }
    method delete_items { itemList } {
	for { set il 0 } { $il < [llength $itemList] } { incr il } {
	    set item [lindex $itemList $il]
	    set key [dict_getd [dict_inverse $itemDict] $item ""]
	    if { $key eq "" } { continue }
	    set ipos [$self give_pos_in_values $key]
	    set options(-values) [lreplace $options(-values) $ipos $ipos]
	    dict unset itemDict $key
	    if { [$tree item id $item] ne "" } {
		lappend itemList {*}[$tree item children $item]
		$tree item delete $item
	    }
	}
    }
    method get_selected_comma_list { args } {
	set optional {
	    { -varname name "" }
	}
	set compulsory ""
	parse_args $optional $compulsory $args

	set ret ""
	set map [list "," "&#44;"]
	foreach elm [$self cget -values] {
	    if { [lindex $elm 1] eq "on" } {
		lappend ret [string map $map [lindex $elm 0]]
	    }
	}
	if { $varname ne "" } {
	    uplevel 1 [list set $varname [join $ret ","]]
	} else {
	    return [join $ret ","]
	}
    }
    method set_selected_comma_list { args } {
	set optional {
	    { -varname name "" }
	    { -insert "" 0 }
	}
	set compulsory "data"
	set args [parse_args -raise_compulsory_error 0 \
		$optional $compulsory $args]
	
	if { $varname ne "" } {
	    set data [uplevel 1 [list set $varname]]
	}
	set map [list "&#44;" "," "&comma;" ","]
	set list ""
	foreach i [split $data ","] {
	    lappend list [string map $map $i]
	}
	set list [lsort $list]
	
	foreach i [range 0 [llength $options(-values)]] {
	    set elm [lindex $options(-values) $i]
	    lassign $elm name state parentName open_close
	    if { $parentName ne "" } {
		set key $parentName
		lappend key $name
	    } else {
		set key $name
	    }
	    set item [dict get $itemDict $key]
	    if { [llength [lindex $options(-values) $i]] < 2 } {
		set v [list [lindex $options(-values) $i 0] on]
		lset options(-values) $i $v
	    }
	    if { [lsearch -sorted $list $name] != -1 } {
		$tree item state set $item on
		lset options(-values) $i 1 on
	    } else {
		$tree item state set $item !on
		lset options(-values) $i 1 off
	    }
	}
	if { $insert } {
	    set changes 0
	    set values $options(-values)
	    foreach i $list {
		if { [lsearch -index 0 $values $i] == -1 } {
		    lappend values [list $i on "" open]
		    set changes 1
		}
	    }
	    if { $changes } {
		$self configure -values $values
	    }
	}
    }
    method _insert { index list } {

	set imgState [list \
		[cu::get_image_selected internet-check-on] {selected check on} \
		[cu::get_image_selected internet-check-off] {selected check} \
		[cu::get_image internet-check-on] {check on} \
		[cu::get_image internet-check-off] {check} \
		]
	
	lassign $list name state parentName open_close
	if { [dict exists $itemDict $parentName] } {
	    set parent [dict get $itemDict $parentName]
	} else {
	    set parent 0
	}
	set newlist [list [list ::cu::images::internet-check-off $name]]
	set item [$tree insert $index $newlist $parent]
	if { $parentName ne "" } {
	    set key $parentName
	    lappend key $name
	} else {
	    set key $name
	}
	if { [dict exists $itemDict $key] } {
	    error "error: repeated item name '$name' in cu::check_listbox values"
	}
	dict set itemDict $key $item
	$tree item state set $item check
	if { $state eq "on" } {
	    $tree item state set $item on
	}
	switch $open_close {
	    open { $tree item expand $item }
	    close { $tree item collapse $item }
	    disabled { $tree item enabled $item 0 }
	}
	$tree item element configure $item 0 e_image -image $imgState
	return $item
    }
    method _recursive_change_state { item state } {
	$tree item state set $item $state
	foreach child [$tree item children $item] {
	    $self _recursive_change_state $child $state
	}
    }
    method add_exclusive_item { key } {
	
	set item [dict get $itemDict $key]
	dict set exclusiveDict $item [$tree item state get $item on]
    }
    method set_state { itemList state } {
	set sel [lsort -integer $itemList]
	foreach item $sel {
	    $self _recursive_change_state $item $state
	}
    }
    method _selection { _tree ids } {
	
	set sel [lsort -integer $ids]
	
	foreach item [dict keys $exclusiveDict] {
	    dict set exclusiveDict $item [$tree item state get $item on]
	}
	set num_activated 0
    
	if { $sel eq $prev_selection } {
	    set item active
	    if { [$tree item state get $item on] } {
		$self _recursive_change_state $item !on
	    } else {
		$self _recursive_change_state $item on
		incr num_activated
	    }
	} else {
	    foreach item $sel {
		if { [lsearch -integer -sorted $prev_selection $item] == -1 } {
		    if { [$tree item state get $item on] } {
		        $self _recursive_change_state $item !on
		    } else {
		        $self _recursive_change_state $item on
		        incr num_activated
		    }
		}
	    }
	}
	set prev_selection $sel
	
	foreach item [dict keys $exclusiveDict] {
	    set new_state [$tree item state get $item on]
	    if { $new_state && ![dict get $exclusiveDict $item] } {
		foreach child [$tree item children 0] {
		    $self _recursive_change_state $child !on
		}
		foreach item_in [dict keys $exclusiveDict] {
		    dict set exclusiveDict $item_in 0
		}
		$tree item state set $item on
		dict set exclusiveDict $item 1
		return
	    }
	}
	if { $num_activated } {
	    foreach item [dict keys $exclusiveDict] {
		$tree item state set $item !on
		dict set exclusiveDict $item 0
	    }
	}
    }
}

snit::widget cu::_check_listbox_as_menu {
    option -parent ""

    hulltype toplevel
    
    delegate method * to tree
    delegate option * to tree

    variable tree
    variable marker_resize_xy0
    variable save_grab ""
    
    constructor args {
	wm overrideredirect $win 1

	set tree [cu::check_listbox $win.tree -has_sizegrip 1]
	
	grid $tree -sticky nsew
	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 0 -weight 1
	
	$self configure -bd 1 -relief solid -background white
	$self configurelist $args

#         label $tree.l -image [cu::get_image resizer] -cursor sizing
#         grid $tree.l -row 1 -column 1
	grid configure $tree.t -rowspan 2

	bind $win <ButtonPress-1> [mymethod check_unpost %x %y]
	bind $win <Escape> "[mymethod unpost] ; break"
#         bind $tree.l <ButtonPress-1> [mymethod _marker_resize BP1 %X %Y]
#         bind $tree.l <B1-Motion> [mymethod _marker_resize BM1 %X %Y]
    }
    method deiconify { args } {
	
	set optional {
	    { -force_width "" 0 }
	}
	set compulsory "x y min_width"
	parse_args $optional $compulsory $args

	set n [$tree index last]
	if { $n < 7 } { set n 7 }
	if { $n > 15 } { set n 15 }
	if { [$tree cget -itemheight] != 0 } {
	    set h [expr {[$tree cget -itemheight]*$n}]
	} else {
	    set h [expr {[$tree cget -minitemheight]*$n}]
	}
	$tree configure -height $h
	set wi [winfo width $win]
	if { $wi < $min_width } { set wi $min_width }
	if { !$force_width && $wi+$x > [winfo screenwidth $win] } {
	    set wi [expr {[winfo screenwidth $win]-$x}]
	}
	if { $y+$h+10 > [winfo screenheight $win] } {
	    set h [expr {[winfo screenheight $win]-$y-10}]
	}
	tooltip::tooltip disable
	wm geometry $win ${wi}x$h+$x+$y
	update
	wm deiconify $win
	focus $tree
	set save_grab [grab current $win]
	grab -global $win
	tooltip::tooltip enable
    }
#     method _marker_resize { what x y } {
# 
#         switch $what {
#             BP1 {
#                 set marker_resize_xy0 [list $x $y]
#             }
#             BM1 {
#                 foreach "x0 y0" $marker_resize_xy0 break
#                 set t [winfo toplevel $win]
#                 set width [expr {[winfo width $t]+$x-$x0}]
#                 set height [expr {[winfo height $t]+$y-$y0}]
#                 if { $width < 0 } { set width 0 }
#                 if { $height < 0 } { set height 0 }
#                 wm geometry $t ${width}x$height
#                 set marker_resize_xy0 [list $x $y]
#             }
#         }
#     }
    method check_unpost { x y } {
	if { $x < 0 || $x > [winfo width $win] || 
	    $y < 0 || $y > [winfo height $win] } {
	    $self unpost
	}
    }
    method unpost {} {
	
	$tree close_search_label

	grab release $win
	if { $save_grab ne "" } {
	    grab $save_grab
	}
	wm withdraw $win
	event generate $win <<ComboboxSelected>> 
    }
}

################################################################################
#    cu::menubutton_frame
################################################################################

snit::widgetadaptor cu::menubutton_frame {
    option -postcommand ""
    
    delegate method * to hull
    delegate option * to hull
    
    variable mymenu
    variable save_grab ""
    variable save_focus ""
    
    constructor args {
	installhull using ttk::menubutton
	bindtags $win [linsert [bindtags $win] 1 menubutton_frame]
	
	bind $win <ButtonPress-1> [mymethod BP1 %x %y]
	bind $win <ButtonRelease-1> [mymethod BR1 %x %y]

	set mymenu [toplevel $win.m -relief solid -bd 1]
	$mymenu configure -bg [cu::give_widget_background $win]
	wm withdraw $mymenu
	update idletasks
	wm overrideredirect $mymenu 1
	wm transient $mymenu [winfo toplevel $win]

	bind $win <space> [mymethod post]
	bind $win <Return> [mymethod post]

	bind $mymenu <<ComboboxSelected>> [mymethod endpost]
	$self configurelist $args
    }
    method giveframe {} {
	return $mymenu
    }
    method BP1 { x y } {
       $self post
    }
    method BR1 { x y } {
	$win instate {pressed !disabled} {
	    $win state !pressed
	}
	return -code break
    }
    method post {} {
	if { [$win instate disabled] } { return }
	
	if { $options(-postcommand) ne "" } {
	    uplevel #0 $options(-postcommand)
	}
	catch { tile::clickToFocus $win }
	catch { ttk::clickToFocus $win }
	$win state pressed
	
	set x [winfo rootx $win]
	set y [expr {[winfo rooty $win]+[winfo height $win]}]
	lassign [list 200 220] wi he
	
	if { $x+$wi > [winfo screenwidth $mymenu] } {
	    if { [winfo screenwidth $mymenu]-$x > 150 } {
		set wi [expr {[winfo screenwidth $mymenu]-$x}]
	    } else {
		set x [expr {[winfo screenwidth $mymenu]-$wi}]
		if { $x < 0 } { set x 0 }
	    }
	}
	if { $y+$he > [winfo screenheight $mymenu] } {
	    if { [winfo screenheight $mymenu]-$y > 100 } {
		set he [expr {[winfo screenheight $mymenu]-$y}]
	    } else {
		set y [expr {[winfo screenheight $mymenu]-$he}]
		if { $y < 0 } { set y 0 }
	    }
	}
	wm geometry $mymenu ${wi}x${he}+$x+$y
	wm deiconify $mymenu
	set save_grab [grab current $win]
	set save_focus [focus]
	grab -global $mymenu
	bind $mymenu <1> [mymethod check_unpost %x %y]
	bind $mymenu <Escape> [mymethod unpost]
	focus $mymenu
    }
    method check_unpost { x y } {
	if { $x < 0 || $x > [winfo width $mymenu] || 
	    $y < 0 || $y > [winfo height $mymenu] } {
	    $self unpost
	}
    }
    method unpost {} {
	$win instate {pressed !disabled} {
	    $win state !pressed
	}
	grab release $mymenu

	catch { grab $save_grab }
	catch { focus $save_focus }
    
	wm withdraw $mymenu
	event generate $win <<ComboboxSelected>> 
    }
}

################################################################################
#    menubutton_check_listbox
################################################################################

snit::widgetadaptor cu::menubutton_check_listbox {
    option -label_prefix ""
    option -selected_in_label 0
    option -update_label_command ""
    option -postcommand ""
    
    delegate method * to hull
    delegate option * to hull
    delegate option -values to mymenu
    delegate option -showbutton to mymenu
    delegate option -showline to mymenu
    delegate option -add_new_button to mymenu
    delegate option -permit_rename to mymenu
    delegate option -permit_delete to mymenu
    
    delegate method add_exclusive_item to mymenu
    delegate method get_selected_comma_list to mymenu

    variable mymenu

    constructor args {
	installhull using ttk::menubutton
	bindtags $win [linsert [bindtags $win] 1 menubutton_check_listbox]
	
	bind $win <ButtonPress-1> [mymethod BP1 %x %y]
	bind $win <ButtonRelease-1> [mymethod BR1 %x %y]

	set mymenu $win.m
	cu::_check_listbox_as_menu $mymenu -parent $win -showbutton 0 -showline 0
	wm withdraw $mymenu

	bind $win <space> [mymethod post]
	bind $win <Return> [mymethod post]

	bind $mymenu <<ComboboxSelected>> [mymethod endpost]
	$self configurelist $args
    }
    method BP1 { x y } {
       $self post
    }
    method BR1 { x y } {
	$win instate {pressed !disabled} {
	    $win state !pressed
	}
	return -code break
    }
    method post {} {
	if { [$win instate disabled] } { return }
	
	if { $options(-postcommand) ne "" } {
	    uplevel #0 $options(-postcommand)
	}
	catch { tile::clickToFocus $win }
	catch { ttk::clickToFocus $win }
	$win state pressed
	
	set x [winfo rootx $win]
	set y [expr {[winfo rooty $win]+[winfo height $win]}]
	$mymenu deiconify -force_width $x $y [expr {[winfo width $win]-20}]
    }
    method endpost {} {
	$win instate {pressed !disabled} {
	    $win state !pressed
	}
	$self update_label
	event generate $win <<ComboboxSelected>>
    }
    method set_selected_comma_list { args } {
	
	$mymenu set_selected_comma_list {*}$args
	$self update_label
    }
    method update_label {} {
	if { $options(-update_label_command) ne "" } {
	    lassign [uplevel #0 $options(-update_label_command)] txt txtfull
	    $self configure -text $txt
	    tooltip::tooltip $win $txtfull
	    return
	}
	if { $options(-label_prefix) eq "" && !$options(-selected_in_label) } { return }
	set num 0
	set names ""
	foreach elm [$mymenu cget -values] {
	    if { [lindex $elm 1] eq "on" } {
		incr num
		lappend names [lindex $elm 0]
	    }
	}
	if { !$options(-selected_in_label) } {
	    $self configure -text "$options(-label_prefix): $num"
	} else {
	    if { $options(-label_prefix) ne "" } {
		set txt "$options(-label_prefix): "
		set txtfull "$options(-label_prefix): "
	    } else {
		set txt ""
	    }
	    append txt [join [lrange $names 0 2] ", "]
	    if { [llength $names] > 3 } { append txt "..." }
	    append txtfull [join $names ", "]
	    $self configure -text $txt
	    tooltip::tooltip $win $txtfull
	}
    }
}

################################################################################
#    menubutton_tree
################################################################################

snit::widget cu::_menubutton_tree_helper {
    option -parent ""
    option -columns_list ""
    hulltype toplevel
    
    delegate method * to tree
    delegate option * to tree

    variable tree
    variable marker_resize_xy0

    constructor args {
	wm overrideredirect $win 1

	package require fulltktree
	set columns [list [list 20 "" left imagetext 1]]
	set tree [fulltktree $win.tree -height 50 -has_sizegrip 1 \
		-columns $columns -expand 1]
	
	grid $tree -sticky nsew
	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 0 -weight 1
	
	$self configure -bd 1 -relief solid -background white
	$self configurelist $args

	grid configure $tree.t -rowspan 2

	bind $win <ButtonPress-1> [mymethod check_unpost %x %y]
	bind $win <Escape> "[mymethod unpost] ; break"
    }
    onconfigure -columns_list { value } {
	if { $value eq $options(-columns_list) } { return }
	set options(-columns_list) $value

	set columns ""
	set idx 0
	foreach i $options(-columns_list) {
	    lassign $i name dict
	    if { $idx == 0 } {
		set type imagetext
	    } else {
		set type text
	    }
	    foreach "opt default" [list len 10 justify left is_editable 1 expand ""] {
		set $opt [dict_getd $dict $opt $default]
	    }
	    lappend columns [list $len $name $justify $type $is_editable $expand]
	    incr idx
	}
	$tree configure -expand 0 -columns $columns
    }
    method deiconify { x y min_width } {
		
	set n [$tree index last]
	if { $n < 7 } { set n 7 }
	if { $n > 15 } { set n 15 }
	if { [$tree cget -itemheight] != 0 } {
	    set h [expr {[$tree cget -itemheight]*$n}]
	} else {
	    set h [expr {[$tree cget -minitemheight]*$n}]
	}
	$tree configure -height $h
	set wi [winfo width $win]
	if { $wi < $min_width } { set wi $min_width }
	if { $wi+$x > [winfo screenwidth $win] } {
	    set wi [expr {[winfo screenwidth $win]-$x}]
	}
	if { $y+$h+10 > [winfo screenheight $win] } {
	    set h [expr {[winfo screenheight $win]-$y-10}]
	}        
	wm geometry $win ${wi}x$h+$x+$y
	update
	wm deiconify $win
	focus $tree
	grab -global $win
    }
    method check_unpost { x y } {
	if { $x < 0 || $x > [winfo width $win] || 
	    $y < 0 || $y > [winfo height $win] } {
	    $self unpost
	}
    }
    method unpost {} {
	
	$tree close_search_label

	grab release $win
	wm withdraw $win
	event generate $win <<ComboboxSelected>> 
    }
}

snit::widgetadaptor cu::menubutton_tree {

    option -command ""
    option -postcommand ""
    
    delegate method * to hull
    delegate option * to hull
    delegate method tree_item to toctree as item
    delegate option -columns_list to toctree
    delegate option -tree_width to toctree as -width
    
    variable toctree
    variable fnames ""
    variable no_active_items ""
    variable no_active_values ""
    variable cmd_items ""

    constructor args {
	installhull using ttk::menubutton
	bind $win <ButtonPress-1> [mymethod BP1 %x %y]
	bind $win <ButtonRelease-1> [mymethod BR1 %x %y]

	set toctree $win.m
	cu::_menubutton_tree_helper $toctree -parent $win \
	    -selecthandler [mymethod press]
	wm withdraw $toctree

	bind $win <space> [mymethod post]
	bind $win <Return> [mymethod post]

	bind $toctree <<ComboboxSelected>> [mymethod endpost]
	$self configurelist $args
    }
    method BP1 { x y } {
       $self post
    }
    method BR1 { x y } {
	$win instate {pressed !disabled} {
	    $win state !pressed
	}
	return -code break
    }
    method press { t id } {
	if { [dict exists $cmd_items $id] } {
	    if { ![dict exists $no_active_items $id] } { 
		$self action LBCancel
	    }
	    uplevel #0 [dict get $cmd_items $id] $id
	    return
	}
	if { [dict exists $no_active_items $id] } { return }
	uplevel #0 $options(-command) [list [dict get $fnames $id]]
	$toctree unpost
    }
    method post {} {
	$win instate !disabled {
	    if { $options(-postcommand) ne "" } {
		uplevel #0 $options(-postcommand)
	    }
	    catch { tile::clickToFocus $win }
	    catch { ttk::clickToFocus $win }
	    $win state pressed
	    set x [winfo rootx $win]
	    set y [expr {[winfo rooty $win]+[winfo height $win]}]
	    $toctree deiconify $x $y [expr {[winfo width $win]-20}]
	}
    }
    method endpost {} {
	$win instate {pressed !disabled} {
	    $win state !pressed
	}
	event generate $win <<ComboboxSelected>>
    }
    method tree_insert { args } {
	set optional {
	    { -image image "" }
	    { -collapse boolean 0 }
	    { -active boolean 1 }
	    { -command cmd "" }
	}
	set compulsory "idx name fullname parent"
	parse_args $optional $compulsory $args

	if { $image eq "" } {
	    set image appbook16
	} elseif { $image eq "-" } {
	    set image ""
	}
	if { [$self cget -columns_list] eq "" } {
	    set data [list [list $image $name]]
	} else {
	    set data [list [list $image [lindex $name 0]]]
	    lappend data {*}[lrange $name 1 end]
	}
	set id [$toctree insert $idx $data $parent]
	if { $collapse } {
	    $toctree item collapse $id
	}
	if { !$active } {
	    dict set no_active_items $id ""
	    dict set no_active_values $fullname ""
	}
	if { $command ne "" } {
	    dict set cmd_items $id $command
	}
	dict set fnames $id $fullname
	return $id
    }
}


################################################################################
#    cu::notebook
################################################################################

snit::widgetadaptor cu::notebook {
    option -menu_callback ""
    option -last_menu_callback ""
    
    delegate method * to hull
    delegate option * to hull
    
    delegate method _tab to hull as tab
    delegate method _tabs to hull as tabs
    delegate method _add to hull as add
    delegate method _insert to hull as insert

    constructor args {
	installhull using ttk::notebook
	
	if { [info command img::cu::notebook::plus] eq "" } {
	    image create photo img::cu::notebook::plus -data {
		iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAFo9M/3AAAABGdBTUEAAYagMeiWXwAAAQRJ
		REFUOI2lUjuuhDAMHK+34BYchopr0CBZ4jpINNBSpqShoUSRcgZqTkAR5RX7AoSP2Kc3pTOe
		zNgmZgYAgJmhlAL5CnVdhyiKPk/MDGMMmBkvHLD2eLwAQETcWhARl6YptW27/VcUhfPigYZv
		LcuSfO191MyyjABsRC/lpZVSQe1k8zHHEUFyEXF7PyfCr8EAxxR0jEvMjHEcMc8zrLVYlgXW
		WgzD4E4xjTHBFC/X8+eYj4Qn3FoQEae1htYax9l9JfAt1ggi4vI8J2stiAjW2oDo3GaiaRrn
		T+69J1VVFVhNkoQAoO/72wirwP6IvaM4jjFN0+X7SeAKdV3f/uzx7zX+ANg6cEaakf1xAAAA
		AElFTkSuQmCC
	    }
	    image create photo img::cu::notebook::plus_active -data {
		iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAAHnlligAAAABGdBTUEAAYagMeiWXwAAAiVJ
		REFUKJF9kb1v00AYxh+7dmLnQ2HInIhISBldYZAKqod2AKRKSAhWtgwta6ZKniL1D4Ch/0AG
		F6RK6RKoooJaGCLaOFI6JAxpPmrkcCEYtY2NE8cMJ4IHyjucfnruveeee2/BsixJkhYkSWrz
		i4ymaQC474nbkTAPTdNM0yzXvpqmyQLwPI+uzN7e3ng8BpDNZmGappwrmqbpOM7Ch5/S88d3
		D7+Mb934BcMw5FzRMIzBYIBSqeS6LiFEzhXlXJEQQgixLIuzbfvei9cAABTWVx5tvgPw8dVT
		DsCbzfsAnm190s8uKBNCGF3X0+m0KIoI1NHREVOv15vNZlCNRCKKojCWZU0mEwDUvbz1IJFI
		lMtlVhAEz/M8zyusr9DHjUYj27Y5AGtqhUaac36VYzqdzumQ73wbTz3/wp65U7/8/iS/yjH9
		fn86nQJo/gir2wc0brVa5UKhEMuy80ihUIgCs7+/v7y8HIxr23a322V0XR8Oh7Isx2IxXF+X
		l5fHx8fJZJJttVqKoriuO/pTSxs7lQapNMjSxs5cdF1XUZRWq8UB8H3f9/1/Ggd1yiylNbXS
		JGgM/NOBTwdDJ1QzZifns9r5bE2t0AMc/SsA+ZeVudmTh3cA7L79HLyNtnEAeJ7fVZW/3YXD
		xZtx/ewCQFDneR4AJ4oiz/PxePzq6mq+p24fUAiHwxSi0ajjOKIoMpZltdvtXq/nOM5/xioI
		QiqVymQyvwG9NCv9DrlLmwAAAABJRU5ErkJggg==
	    }
	}
	$self configurelist $args
    }
    onconfigure -menu_callback { value } {
	set options(-menu_callback) $value
	$self _update_icons
    }
    onconfigure -last_menu_callback { value } {
	set options(-last_menu_callback) $value
	$self _update_icons
    }
    method add { args } {
	if { $options(-last_menu_callback) ne "" } {
	    set ret [$self _insert [expr {[llength [$self _tabs]]-1}] {*}$args]
	} else {
	    set ret [$self _add {*}$args]
	}
	$self _update_icons
	return $ret
    }
    method insert { args } {
	set ret [$self _insert {*}$args]
	$self _update_icons
	return $ret
    }
    method tab { args } {
	set ret [$self _tab {*}$args]
	$self _update_icons
	return $ret
    }
    method tabs {} {
	if { $options(-last_menu_callback) ne "" } {
	    return [lrange [$self _tabs] 0 end-1]
	} else {
	    return [$self _tabs]
	}
    }
    method _update_icons {} {
	if { $options(-last_menu_callback) ne "" } {
	    if { ![winfo exists $win.__last_] } {
		ttk::frame $win.__last_
		$self _add $win.__last_
	    }
	    set idx [$self index $win.__last_]
	    if { $idx > 0 && $idx == [$self index current] } {
		$self select [expr {$idx-1}]
	    }
	    bind $win <<NotebookTabChanged>> [mymethod notebook_changed]
	} else {
	    if { [winfo exists $win.__last_] } {
		destroy $win.__last_
	    }
	    bind $win <<NotebookTabChanged>> ""
	}
	if { $options(-menu_callback) ne "" } {
	    foreach t [$self _tabs] {
		$self _tab $t -image img::cu::notebook::plus -compound right
	    }
	    bind $win <ButtonPress-1> [mymethod BP1 %x %y]
	    bind $win <Motion> [mymethod motion %x %y]
	    bind $win <Leave> [mymethod leave]
	    bind $win <ButtonRelease-1> [mymethod BR1 %x %y]
	} else {
	    foreach t [$self _tabs] {
		if { [$self _tab $t -image] eq "img::cu::notebook::plus" } {
		    $self _tab $t -image ""
		}
	    }
	    bind $win <ButtonPress-1> ""
	    bind $win <Motion> ""
	    bind $win <Leave> ""
	    bind $win <ButtonRelease-1> ""
	}
    }
    method notebook_changed {} {
	set idx [$self index current]
	if { $idx > 0 && $options(-last_menu_callback) ne "" && $idx == [llength [$self _tabs]]-1 } {
	    $self select 0
	}
    }
    method leave {} {
	foreach t [$self _tabs] {
	    $self _tab $t -image img::cu::notebook::plus
	}
    }
    method motion { x y } {
	set idx [$self index @$x,$y]
	if { $idx eq "" } { return }
	
	foreach t [$self _tabs] {
	    set i [$self index $t]
	    if { $idx == $i && [$self index @[expr {$x+18}],$y] != $idx } {
		$self _tab $i -image img::cu::notebook::plus_active
	    } else {
		$self _tab $i -image img::cu::notebook::plus
	    }
	}
    }
    method BP1 { x y } {
	if { $options(-last_menu_callback) eq "" } { return }
	if { [$self index @$x,$y] == [llength [$self _tabs]]-1 } {
	    return -code break
	}
    }
    method BR1 { x y } {
	if { [$self index @[expr {$x+18}],$y] != [$self index @$x,$y] } {
	    set idx [$self index @$x,$y]
	    set x [expr {$x+[winfo rootx $win]}]
	    set y [expr {$y+[winfo rooty $win]}]
	    destroy $win.menu
	    menu $win.menu -tearoff 0
	    if { $options(-last_menu_callback) ne "" && $idx == [llength [$self _tabs]]-1 } {
		uplevel #0 $options(-last_menu_callback) $win.menu
	    } else {
		uplevel #0 $options(-menu_callback) $win.menu
	    }
	    tk_popup $win.menu $x $y
	}
    }
}

################################################################################
#    report_maker
################################################################################

snit::widget cu::report_makerT {
    hulltype toplevel
    
    delegate method * to report_maker
    delegate option * to report_maker

    constructor args {
	wm title $win [_ "Report maker"]

	install report_maker as cu::report_maker $win.rm

	ttk::frame $win.buts
	ttk::button $win.buts.b1 -text [_ OK] -command [mymethod accept]
	ttk::button $win.buts.b2 -text [_ Cancel] -command [mymethod cancel]
	ttk::button $win.buts.b3 -text [_ Print] -command [mymethod print]
	
	grid anchor $win.buts center
	grid $win.buts.b1 $win.buts.b2 $win.buts.b3 -padx 2 -pady 2

	grid $win.rm -sticky nsew
	grid $win.buts -sticky ew

	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 0 -weight 1
	$self configurelist $args
    }
    method accept {} {
	
    }
    method cancel {} {
	destroy $win
    }
}

snit::widget cu::report_maker {
    option -list_side left
    option -has_sizegrip 0
    
    delegate method * to wordwidget
    delegate option * to wordwidget
    
    variable current_page ""
    variable current_page_type ""
    variable original_title ""
    variable title ""
    variable pages_list ""
    variable has_changes 0

    variable toctree
    variable wordwidget
    variable title_frame
    
    constructor args {

	panedwindow $win.p1 -orient horizontal -bd 0

	set columns [list [list 10 [_ Pages] left text 1]]
	
	ttk::frame $win.p1.f1 -style WNtoolbar -padding "4 4"
	ttk::label $win.p1.f1.l1 -text [_ "Pages"] -style WNlabel
	tooltip::tooltip $win.p1.f1.l1 [_ "Select the page"]
	
	package require fulltktree
	set toctree [fulltktree $win.p1.f1.tree -width 150 \
		-selecthandler [mymethod select_page_tree] \
		-selecthandler2 "# do nothing" \
		-draghandler [mymethod drop] \
		-columns $columns -expand 1 \
		-selectmode extended -showheader 0 -showlines 0  \
		-indent 0 -font MainFont -showbuttons 0 \
		-font TkDefaultFont \
		]
	set tree $win.p1.f1.tree
	
	cu::menubutton_button $win.p1.f1.b1 -text [_ "Add"] -command [mymethod add_page auto] \
	    -menu $win.p1.f1.b1.m -style WNmenubutton
	set menu [menu $win.p1.f1.b1.m -tearoff 0]
	$menu add command -label [_ "Add title page"] -command [mymethod add_page title]
	$menu add command -label [_ "Add normal page"] -command [mymethod add_page wordwidget]
	
	ttk::button $win.p1.f1.b2 -text [_ "Remove"] -command [mymethod remove_page] -style WNbutton
	
	grid $win.p1.f1.l1 -
	grid $tree - -sticky nsew
	grid $win.p1.f1.b1 $win.p1.f1.b2 -sticky e -padx 2 -pady 2
	grid configure $win.p1.f1.b2 -padx "2 17"
	grid columnconfigure $win.p1.f1 0 -weight 1
	grid rowconfigure $win.p1.f1 0 -minsize 26
	grid rowconfigure $win.p1.f1 1 -weight 1
	
	ttk::sizegrip $win.p1.f1.grip -style TSizegripWN
	place $win.p1.f1.grip -relx 1 -rely 1 -x -1 -y -1 -anchor se
	
	bind $win.p1.f1.grip <ButtonPress-1> "[mymethod _move_sizegrip start];break"
	bind $win.p1.f1.grip <B1-Motion> "[mymethod _move_sizegrip motion %X %Y];break"
	bind $win.p1.f1.grip <ButtonRelease-1> "[mymethod _move_sizegrip end];break"
	
	$win.p1 add $win.p1.f1 -stretch never -sticky nsew -minsize 40 -padx 0 -pady 0
	
	ttk::frame $win.p1.f2 -padding "2 0"

	set title_frame [ttk::frame $win.p1.f2.title]
	ttk::label $win.p1.f2.title.l1 -text [_ "Title"]:
	ttk::entry $win.p1.f2.title.e1 -textvariable [myvar title]
	
	grid $win.p1.f2.title.l1 $win.p1.f2.title.e1 -sticky w -padx 2 -pady 0
	grid configure $win.p1.f2.title.e1 -sticky ew
	grid columnconfigure $win.p1.f2.title 1 -weight 1
		
	package require wordwidget_snit
	set wordwidget [wordwidget_and_toolbox $win.p1.f2.wns1 -w_width 90 \
		-w_height 40 -bd 1 -mode static -scroll 1 -state disabled]

	grid $title_frame -sticky ew -padx "12 4" -pady "20 2"
	grid $wordwidget -sticky nsew -padx 0 -pady 0
	grid columnconfigure $win.p1.f2 0 -weight 1
	grid rowconfigure $win.p1.f2 1 -weight 1
	
	grid remove $title_frame
	
	$win.p1 add $win.p1.f2 -stretch always -sticky nsew -minsize 40 -padx 0 -pady 0

	grid $win.p1 -sticky nsew -padx 0 -pady 0
	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 0 -weight 1
	
	cu::add_bindtag_recursive -pos before_toplevel $win $win
	
	$self configurelist $args
	
	if { $options(-has_sizegrip) == 0 } {
	    place forget $win.p1.f1.grip
	}
    }
    onconfigure -list_side { value } {
	set options(-list_side) $value
	if { $options(-list_side) eq "left" } {
	    $win.p1 paneconfigure $win.p1.f1 -before $win.p1.f2
	} else {
	    $win.p1 paneconfigure $win.p1.f1 -after $win.p1.f2
	}
    }
    method has_changes {} {
	return [expr {$has_changes || $title ne $original_title }]
    }
    method _move_sizegrip { what args } {
	switch $what {
	    start {
		$win.p1 configure -width [winfo width $win.p1] -height [winfo height $win.p1]
	    }
	    motion {
		lassign $args X Y
		set h [expr {$Y-[winfo rooty $win]}]
		if { $h < 80 } { set h 80 }
		$win.p1 configure -height $h
	    }
	    end {
		set w $win.p1
		set y [expr {[winfo rooty $win.p1]+[winfo height $win.p1]}]
		while 1 {
		    set info [grid info $w]
		    set w [dict_getd $info -in ""]
		    if { $w eq "" } { break }
		    if { $y > [winfo rooty $w]+[winfo height $w]+2 } {
		        set y [expr {[winfo rooty $w]+[winfo height $w]-6}]
		    }
		}
		set h [expr {$y-[winfo rooty $win.p1]}]
		if { $h < 80 } { set h 80 }
		
		$win.p1 configure -height $h
	    }
	}
    }
    method _fill_pages_list {} {
	$toctree item delete all
	set page 0
	foreach i $pages_list {
	    $toctree insert end [list [_ "Page %d" [expr {$page+1}]]]
	    incr page
	}
    }
    method set { xml } {
	if { $xml eq "" } { set xml <wordwidget/> }
	dom parse $xml doc
	set title [$doc selectNodes string(/*/title)]
	set pages_list ""
	lappend pages_list [list title $title]
	foreach node [$doc selectNodes /*/section] {
	    set data ""
	    foreach n [$node childNodes] { append data [$n asXML -indent none] }
	    lappend pages_list [list wordwidget $data]
	}
	$self _fill_pages_list
	if { $current_page ne "" && $current_page < [llength $pages_list] } {
	    set page $current_page
	} else {
	    set page 0
	}
	lassign "" "" current_page current_page_type
	$self select_page $page
	set original_title $title
	set has_changes 0
    }
    method get {} {
	$self accept_current_page
	
	foreach i $pages_list {
	    lassign $i page_type data
	    if { $page_type eq "title" } {
		set title $data
		break
	    }
	}
	set xml "<wordwidget><title>[xml_map1 $title]</title>"
	set page 0
	foreach i $pages_list {
	    lassign $i page_type data
	    if { $page_type ne "wordwidget" } { continue }
	    set t [_ "Page %d" [expr {$page+1}]]
	    append xml <section><title>[xml_map1 $t]</title>
	    append xml $data
	    append xml </section>
	    incr page
	}
	append xml "</wordwidget>"
	if { $xml eq "<wordwidget><title></title></wordwidget>" } { set xml "" }
	return $xml
    }
    method accept_current_page {} {
	if { $current_page eq "" } { return }
	switch $current_page_type {
	    title {
		set elm [list title $title]
	    }
	    wordwidget {
		if { [$wordwidget HasChanges] } { set has_changes 1 }
		set data "[$wordwidget exportXML]"
		set elm [list wordwidget $data]
	    }
	}
	lset pages_list $current_page $elm
	lassign "" current_page current_page_type
    }
    method select_page { page } {
	$self accept_current_page
	
	lassign [lindex $pages_list $page] page_type data
	switch $page_type {
	    title {
		grid $title_frame
		grid remove $wordwidget
		set title $data
	    }
	    wordwidget {
		grid remove $title_frame
		grid $wordwidget
		$wordwidget configure -state normal
		$wordwidget ClearText
		$wordwidget ApplyXMLdataAsText <wordwidget>$data</wordwidget>
		$wordwidget reset_changes_counter
	    }
	}
	lassign [list $page $page_type] current_page current_page_type
	$toctree selection clear
	$toctree selection add "rnc $page 0"
    }
    method select_page_tree { args } {
	lassign $args - item
	if { [llength $item] != 1 } { return }
	lassign [$toctree item rnc $item] page
	$self select_page $page
    }
    method add_page { what } {
	
	if { $what eq "auto" } {
	    if { ![llength $pages_list] } {
		set what title
	    } else {
		set what wordwidget
	    }
	}
	switch $what {
	    title {
		set elm [list title ""]
	    }
	    wordwidget {
		set elm [list wordwidget ""]
	    }
	}
	set page [llength $pages_list]
	lappend pages_list $elm
	$toctree insert end [list [_ "Page %d" [expr {$page+1}]]]
	$self select_page $page
	set has_changes 1
    }
    method remove_page {} {
	set old_page $current_page
	$self accept_current_page
	set pages ""
	foreach item [$toctree selection get] {
	    lassign [$toctree item rnc $item] page
	    lappend pages $page
	}
	foreach page [lsort -integer -decreasing $pages] {
	    set pages_list [lreplace $pages_list $page $page]
	}
	$self _fill_pages_list
	
	if { $old_page ne "" && $old_page < [llength $pages_list] } {
	    $self select_page $old_page
	} elseif { [llength $pages_list] } {
	    $self select_page [expr {[llength $pages_list]-1}]
	} else {
	    grid remove $title_frame
	    grid $wordwidget
	    $wordwidget configure -state disabled
	    $wordwidget ClearText
	}
	set has_changes 1
    }
    method drop { args } {
	lassign $args - recieving_item dragged_items prev_next_center
	
	$self accept_current_page
	
	lassign [$toctree item rnc $recieving_item] recieving_idx
	if { $prev_next_center eq "next" } { incr recieving_idx }
	
	set pages ""
	foreach item $dragged_items {
	    lassign [$toctree item rnc $item] page
	    lappend pages $page
	}
	set delta 0
	foreach page [lsort -integer $pages] {
	    set elm [lindex $pages_list $page+$delta]
	    set pages_list [linsert $pages_list $recieving_idx $elm]
	    if { $page >= $recieving_idx } { incr delta }
	    incr recieving_idx
	    set pages_list [lreplace $pages_list $page+$delta $page+$delta]
	    if { $page+$delta < $recieving_idx } {
		incr recieving_idx -1
	    }
	    incr delta -1
	}
	$self _fill_pages_list
	$self select_page [expr {$recieving_idx-1}]
	set has_changes 1
    }
    method print {} {
	
	$self accept_current_page
	
	set mytitle ""
	foreach elm $pages_list {
	    if { [lindex $elm 0] eq "title" } { set mytitle [lindex $elm 1] }
	}
	
	set xml "<wordwidget><title>[xml_map1 $mytitle]</title>"
	append xml "<company>[xml_map1 $mytitle]</company>"
	append xml "<date>[xml_map1 2008-01-01]</date>"
	foreach elm $pages_list {
	    if { [lindex $elm 0] eq "wordwidget" } {
		append xml [lindex $elm 1]<beginpage/>
	    }
	}
	append xml </wordwidget>
	package require xml2pdf
	xml2pdf::addTemplatesDir [file join $cu::topdir xml2pdf_templates]
	set opts ""
	lappend opts printtype PDF
	lappend opts template report_maker
	set file [xml2pdf:::PrintXML $xml A4L "" $opts]
    }
}

################################################################################
#   cu::scrollframe    
################################################################################

snit::widget cu::scrollframe {
    option -text ""
	
    hulltype frame
    
    delegate method * to frame
    delegate option * to hull
    delegate method scroll_set to scroll as set

    variable frame
    variable canvas
    variable scroll
    variable act_bind
    
    
    constructor args {
	$self configure -bd 0   
	set canvas [canvas $win.c -bd 0 -highlightthickness 0 -yscrollcommand [list $win.scroll set]]
	set scroll [scrollbar $win.scroll -orient vertical -command [list $win.c yview]]
	grid $canvas $scroll -sticky nsew
	grid $canvas -sticky nsew
	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 0 -weight 1
	grid remove $scroll
	$self configurelist $args
	
	if { $options(-text) ne "" } {
	    set frame [ttk::labelframe $win.f -text $options(-text)]
	} else {
	    set frame [ttk::frame $win.f]
	}
	$canvas create window 0 0 -anchor nw -window $frame
	bind $canvas <Configure> [mymethod check_scroll]
	bind $frame <Configure> [mymethod check_scroll]

	$self configurelist $args
	return $self
    }
    method giveframe {} { return $frame }
    method givescroll {} { return $scroll }
    method givecanvas {} { return $canvas }
    
    method check_scroll {} {      
	$canvas configure -height [winfo reqheight $frame] -width [winfo reqwidth $frame]
	if { [winfo width $canvas] == 1 } { return }
	#update idletasks
	
	set d 0
	set reqheight [expr {[winfo reqheight $frame]+$d}]
	if { [winfo height $canvas] > $reqheight } {
	    set height [winfo height $canvas]
	} else {
	    set height $reqheight
	}
	$canvas itemconfigure 1 -width [expr {[winfo width $canvas]-$d}] -height [expr {$height-$d}]
	$canvas configure -scrollregion [list 0 0 [winfo width $canvas] $height]
	$canvas configure -height $height
	if { [winfo height $canvas] < $reqheight - 10 } {
	    grid $scroll     
	} else {
	    grid remove $scroll
	}
   
	
    }   
    
    method yview { args } {
	$self check_scroll
	$canvas yview {*}$args
	update
	$canvas yview {*}$args
    }
}

if 0 {
    package require compass_utils
    cu::init_tile_styles
    wm withdraw .
    cu::report_makerT .w1
    bind .w1 <Destroy> exit
}

if 0 {
    package require msgcat
    package require compass_utils
    #pack [cu::dater_entry .d -topframecolor red]
    
    proc give_events { start_date end_date } {
	return [dict create \
		2006-09-05 [list [dict create text "Hola maco" start 8:30 end 9:30] \
		    [dict create text "Hola maco2" start 8:30 end 9:30]] \
		2006-09-12 [list [dict create text "cita 2" start 12:30 end 3:30]]]
    }
    
    pack [cu::dater .d -events_handler give_events -detailed_view 0] -fill both \
	-expand 1
}

if 0 {
    package require compass_utils
    pack [cu::combobox_tree .t -nice_print_separator " --- " -width 30 -textvariable var1]
    set n [.t tree_insert end "pepet 1" [list "pepet 1"] 0]
    set n [.t tree_insert end "cacat  asf asf asdf sadfsa fsad" [list "pepet 1" {cacat  asf asf asdf sadfsa fsad}] $n]
    .t tree_insert end cucut cucut 0
    pack [button .b -text value -command {puts $var1}]
}

if 0 {
    package require compass_utils
    pack [cu::menubutton_tree .t -command pp]
    set n [.t tree_insert end pepet pepet 0]
    set n [.t tree_insert end "cacat  asf asf asdf sadfsa fsad" [list pepet {cacat  asf asf asdf sadfsa fsad}] $n]
    .t tree_insert end cucut cucut 0
    
    proc pp { args } {
	tk_messageBox -message "args=$args"
    }
}

if 0 {
    set values [list "parent1 off" "value1 off parent1 open" "value2 off parent1 close" "value3 off parent1" \
	    "value1 off" "value2 off" \
	    "value3 off" "value4 off" "value5 off" "value6 off" "value7 off"]
    pack [cu::menubutton_check_listbox .t -text pepet -values $values]
    pack [ttk::entry .e1]
    pack [ttk::entry .e2]
    
    wm geometry . 600x600
    bind .t <<ComboboxSelected>> [list update_menubutton_check_listbox_text .t]
    
    proc update_menubutton_check_listbox_text { w } {
	array set parents ""
	foreach v [$w cget -values] {
	    foreach "name state parentName open_close" $v break
	    if { $state eq "on" } {
		if { $parentName ne "" } {
		    incr parents($parentName)
		} else {
		    incr parents($name)
		}
	    }
	}
	$w configure -text "pepet ([llength [array names parents]])"
    }
}

################################################################################
#   cu::handle    
################################################################################


snit::widgetadaptor cu::handle {
    option -orient "horizontal"
    option -length 24

    delegate method * to hull
    delegate option * to hull
    
    constructor args {
	installhull using canvas -background grey -bd 0 -highlightthickness 0 -cursor hand2
	bind $win <Configure> [mymethod draw]
	
	if { [info command ::cu::img::handle_h] eq "" } {
	    image create photo ::cu::img::handle_h -data {
		iVBORw0KGgoAAAANSUhEUgAAAAMAAAAjCAYAAABRhLCJAAAAAXNSR0IArs4c
		6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAAOnAAADsQBdfaEgQAAAAd0
		SU1FB9oKEwk4HQwlnpQAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
		TVBXgQ4XAAAATUlEQVQY092RuQ2AMAADzxmSIfOw4lFAJNKAaHFn+SnsAKoB
		KABJBMilNIAyLcAWlYnCDQ8kifvoZ3D0popaZ1sFWKv/lvkw1aK8X3IA3ZV/
		xIFkIEUAAAAASUVORK5CYII=
	    }
	    image create photo ::cu::img::handle_v -data {
		iVBORw0KGgoAAAANSUhEUgAAACMAAAADCAYAAAAZdbZvAAAAAXNSR0IArs4c
		6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
		SU1FB9oLEQo7EMyW7ogAAABESURBVBjTrY8xDsAgDMT8/6eE8EZ3oRUgygKD
		B+uiXIIK4IvKTM0yZKfe03ejhhp/wy2nZvGSz7u/7qFwc0xc9OWzKg/ylC4d
		5NdFHAAAAABJRU5ErkJggg==
	    }
	}
	$self configurelist $args
	$self draw
    }
    onconfigure -orient {value} {
	set options(-orient) $value
	$self draw
    }
    onconfigure -length {value} {
	set options(-length) $value
	$self draw
    }
    method draw {} {
	
	switch $options(-orient) {
	    horizontal {
		$win configure -width 7 -height $options(-length)
		set img ::cu::img::handle_h
	    }
	    vertical {
		$win configure -width $options(-length) -height 7
		set img ::cu::img::handle_v
	    }
	}
	set w [expr {[winfo width $win]-1}]
	set h [expr {[winfo height $win]-1}]
	set wm [expr {$w/2.0}]
	set hm [expr {$h/2.0}]
	
	$win delete all
	$win create polygon 0 10 0 0 $wm 0 $wm 0 $w 0 $w 10 $w $hm $w [expr {$h-10}] $w $h \
	    $wm $h 0 $h 0 [expr {$h-10}] 0 $hm \
	    -outline grey40 -smooth 1 -fill grey
	$win create image 2 [expr {$h/2}] -image $img -anchor w
    }
}

if 0 {
    pack [cu::notebook .t -menu_callback mc -last_menu_callback mc_last]
    ttk::label .t.l1 -text label1
    ttk::label .t.l2 -text label2
    .t add .t.l1 -text label1
    .t add .t.l2 -text label2
    
    proc mc { menu } {
	$menu add command -label pepet
    }
    proc mc_last { menu } {
	$menu add command -label last
    }
    
    cu::combobox_tree .t -state readonly
    .t tree_insert end s1 l1 0
    .t tree_insert end s2 l2 0
    .t tree_insert end s3 l3 0
    .t tree_insert -command "puts kk" end command command 0
    pack .t
}

if 0 {
    wm geometry . 600x300+600+600
    set img [list [cu::get_image internet-check-off] selected [cu::get_image internet-check-on]]
    pack [cu::menubutton_checkbutton .cb -image $img -variable ::var -menu .cb.m]
    menu .cb.m -tearoff 0
    .cb.m add command -label test1
}








