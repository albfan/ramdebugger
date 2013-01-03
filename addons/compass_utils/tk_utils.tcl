
# lognoter as cgi webserver tries to load cu::init_icons_lib
#package require Tk

namespace eval cu {
    variable topdir [file normalize [file dirname [info script]]]
    variable init_tile_styles_done
    variable add_down_arrow_to_image_delta 7
    variable inside_gid 0
}

################################################################################
#    init_tile_styles
################################################################################

proc cu::init_tile_styles { args } {
    variable topdir
    variable init_tile_styles_done
    variable inside_gid
    if { [ info exists ::GIDDEFAULT]} {
	set inside_gid 1
    }
    
    set optional {
	{ -theme blue|none "none" }
    }
    set compulsory ""
    parse_args $optional $compulsory $args

    package require Tk
    if { [info exists init_tile_styles_done] } { return }
    set init_tile_styles_done 1
    
    if { [info exists ::ispocket] } {
	set ispocket $::ispocket
    } elseif { $::tcl_platform(os) eq "Windows CE" } {
	set ispocket 1
    } else {
	set ispocket 0
    }
    if { !$inside_gid } {
	if { [ tk windowingsystem] eq "x11" || $ispocket } {
	    set err [catch {
		ttk::style theme use clam
		#                 package require ttk::theme::tilegtk
		#                 ttk::style theme use tilegtk
	    }]
	    if { $err } {
		ttk::style theme use clam
	    }
	}
	if { "clam" in [ttk::style theme names] } {
	    ttk::style theme settings clam {
		ttk::style configure TButton -padding 1
		ttk::style configure TMenubutton -padding 1
		ttk::style map Toolbutton -background "focus grey [ttk::style map Toolbutton -background]"
		ttk::style map TCombobox -foreground "disabled #999999 [ttk::style map TCombobox -foreground]"
		ttk::style map TCombobox -fieldbackground "disabled #d9d9d9 [ttk::style map TCombobox -fieldbackground]"
	    }
	}
	if { "xpnative" in [ttk::style theme names] } {
	    ttk::style theme settings xpnative {
		ttk::style map TMenubutton -background "focus grey [ttk::style map Toolbutton -background]"
	    }
	}
	
	if { $theme eq "blue" } {
	    # this is the part that keeps blue color for forms
	    
	    set color #e4e8ed            
	    set color_dark #e4e8ed
	    set color_darker #818486
	    
	    foreach i [list TLabel TCheckbutton TRadiobutton \
			   TCombobox TFrame TLabelframe TNotebook \
			   TLabelframe.Label TSizegrip Toolbutton] {
		ttk::style configure $i -background $color
		ttk::style map $i -background [list disabled $color]
	    }
	    foreach i [list TButton TMenubutton TCombobox TScrollbar] {
		ttk::style configure $i -background $color_dark
		ttk::style map $i -background [list disabled $color_dark]
	    }
	    ttk::style map TCombobox -fieldbackground [list readonly $color_dark]
	
	    foreach i [list TLabelframe Frame Label Checkbutton Panedwindow Menu] {
		option add *$i*Background $color
	    }
	    option add *Menu*Background $color
	    option add *Menu*activeForeground $color_darker
	}
    } ;# !inside_gid
    image create photo wntoolbar_toolbar_bg -file [file join $topdir images wntoolbar_toolbar_bg.gif]
    image create photo wntoolbar_button_bg -file [file join $topdir images wntoolbar_button_bg.gif]
    image create photo wntoolbar_button_bg_pressed -file [file join $topdir images wntoolbar_button_bg_pressed.gif]

    set err [catch {
	    ttk::style element create WNtoolbar.background image wntoolbar_toolbar_bg \
		-border "2 30 2 0" -sticky nsew
    } errstring]
    if { $err } {
	return -code error -errorinfo $::errorInfo $errstring
    }
    ttk::style layout WNtoolbar { WNtoolbar.background }
    
    #catch { ttk::style default WNtoolbar -padding "14 1 1 1" }
    catch { ttk::style configure WNtoolbar -padding "14 1 1 1" }
    
    ttk::style element create WNbutton.background image \
	[list wntoolbar_button_bg active wntoolbar_button_bg_pressed pressed wntoolbar_button_bg_pressed] \
	-sticky nsew -width 0 -height 0 -border "2 0 2 0"
    
    # if { [ tk windowingsystem] eq "x11" } {
    #         ttk::style layout WNbutton [ttk::style layout TButton]
    #         ttk::style configure WNbutton -background #bfe4df
    # } else {
    #         ttk::style layout WNbutton {
    #             WNbutton.background -sticky nsew
    #             Toolbutton.border -sticky nswe -children {
    #                 Toolbutton.padding -sticky nswe -children {
    #                     Toolbutton.label -sticky nswe
    #                 }
    #             }
    #         }
    # }
    
    
    ttk::style layout WNbutton {
	WNbutton.background -sticky nsew
	Toolbutton.border -sticky nswe -children {
	    Toolbutton.padding -sticky nswe -children {
		Toolbutton.label -sticky nswe
	    }
	}
    }
    ttk::style layout WNlabel {
	WNbutton.background -sticky nsew
	Label.border -sticky nswe -border 1 -children {
	    Label.padding -sticky nswe -border 1 -children {Label.label -sticky nswe}
	}
    }
    
    ttk::style layout TEntry.warning [ttk::style layout TEntry]
    ttk::style configure TEntry.warning -foreground red
    
    image create photo wnsizegrip_bg -width 1 -height 16
    wnsizegrip_bg copy wntoolbar_toolbar_bg -from 15 10
    ttk::style element create WNSizegrip.background image wnsizegrip_bg \
		-sticky nsew
    
    ttk::style layout WNSizegrip {
	WNSizegrip.background -sticky nsew
	Sizegrip.sizegrip -side bottom -sticky se
    }
    #option add *TSizegrip*Style WNSizegrip
    
    set l "WNbutton.background -sticky nsew\n[ttk::style layout TMenubutton]"
    ttk::style layout WNmenubutton $l
    
    set file [file join $topdir images search.gif]
    image create photo search1 -file $file -format "gif -index 0"
    image create photo search2 -file $file -format "gif -index 1"
    
    ttk::style element create WNsearch.field image \
	[list search1 focus search2] \
	-border {22 7 14} -sticky ew -padding "22 0 0 0"
    
    ttk::style layout WNsearch.entry {
	WNbutton.background -sticky nsew
	WNsearch.field -sticky nswe -border 1 -children {
	    Entry.padding -sticky nswe -children {
		Entry.textarea -sticky nswe
	    }
	}
    }
#     ttk::style configure WNsearch.entry -background #bfe4df
    
    ttk::style layout TSizegripWhite [ttk::style layout TSizegrip]
    ttk::style configure TSizegripWhite -background white

    ttk::style layout TSizegripWN [ttk::style layout TSizegrip]
    ttk::style configure TSizegripWN -background #bfe4df

    ttk::style layout SmallToolbutton [ttk::style layout Toolbutton]
    ttk::style configure SmallToolbutton {*}[ttk::style configure Toolbutton] -padding 0
    
    ttk::style layout TCheckbutton.small [ttk::style layout TCheckbutton]
    ttk::style configure TCheckbutton.small {*}[ttk::style configure Toolbutton] -padding 0 \
	-font TkSmallCaptionFont
    
    if { !$inside_gid } {
	if { "aqua" in [ttk::style theme names] } {
	    ttk::style theme settings aqua {    
		ttk::style configure Toolbutton -padding 0
	    }
	}
	
	option add *Text*Background white
    } ;# !inside_gid
    option add *Multiline_entry*Background white
    option add *Wordwidget_and_toolbox*Background white

    setup_generic_bindings
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
#    generic bindings
################################################################################

proc cu::setup_generic_bindings {} {
    if { [tk windowingsystem] eq "x11" } {
	bind Text <4> ""
	bind Text <5> ""
	bind Canvas <4> ""
	bind Canvas <5> ""

	bind all <4> {
	    set w %W
	    for { set i 0 } { $i < 10 } { incr i } {
		if { [winfo class $w] eq "Toplevel" || $w eq "." } {
		    break
		}
		if { [winfo class $w] in "Text Canvas" && [$w yview] ne "0.0 1.0" } {
		    $w yview scroll -1 units
		    break
		}
		set w [cu::give_manager_parent $w]
	    }
	}
	bind all <5> {
	    set w %W
	    for { set i 0 } { $i < 10 } { incr i } {
		if { [winfo class $w] eq "Toplevel" || $w eq "." } {
		    break
		}
		if { [winfo class $w] in "Text Canvas"  && [$w yview] ne "0.0 1.0" } {
		    $w yview scroll 1 units
		    break 
		}
		set w [cu::give_manager_parent $w]
	    }
	}
    }
    
    bind Text <MouseWheel> ""
    bind Canvas <MouseWheel> ""
    
    bind all <MouseWheel> {
	set w %W
	 for { set i 0 } { $i < 10 } { incr i } {
	    if { [winfo class $w] eq "Toplevel" || $w eq "." } {
		break
	    }
	    if { [winfo class $w] in "Text Canvas"  && [$w yview] ne "0.0 1.0" } {
		cu::_do_scroll $w %D
		break
	    }
	    set w [cu::give_manager_parent $w]
	}
    }
    
    if { $::tcl_platform(platform) eq "windows" } {
	event add <<ContextualPress>> <ButtonPress-3>
	event add <<Contextual>> <ButtonRelease-3>
	event add <<Contextual>> <App>
	set ::control Control
	set ::control_txt Ctrl
    } elseif { [tk windowingsystem] eq "aqua" } {
	event add <<ContextualPress>> <ButtonPress-2>
	event add <<Contextual>> <ButtonRelease-2>
	set ::control Command
	set ::control_txt Command
	
	foreach ev [bind Text] {
	    if { [regsub {Control} $ev {Command} evC] } {
		bind Text $evC [bind Text $ev]
	    }
	}    
    } else {
	event add <<ContextualPress>> <ButtonPress-3>
	event add <<Contextual>> <ButtonRelease-3>
	set ::control Control
	set ::control_txt Ctrl
    }
}

proc cu::_do_scroll { w d } {
    if { [tk windowingsystem] eq "aqua" } {
	$w yview scroll [expr {-1*$d}] units
    } elseif { ![catch { package vcompare [package provide Tk] 8.5 } ret] && $ret < 0} {
	$w yview scroll [expr {- ($d / 120) * 4}] units
    } elseif { [winfo class $w] eq "Canvas" } {
	$w yview scroll [expr {- ($d / 120) * 4}] units
    } else {
	$w yview scroll [expr {-$d/5}] pixels
    }
}

proc cu::give_manager_parent { w } {
    switch [winfo manager $w] {
	"grid" {
	    set w [dict get [grid info $w] -in]
	}
	"pack" {
	    set w [dict get [pack info $w] -in]
	}
	"canvas" {
	    if { [winfo class [winfo parent $w]] eq "Canvas" } {
		set w [winfo parent $w]
	    } else {
		set found 0
		foreach i [winfo children [winfo parent $w]] {
		    if { $i eq $w } { continue }
		    if { [winfo class $i] eq "Canvas" } {
		        set w $i
		        set found 1
		        break
		    }
		}
		if { !$found } {
		    set w [winfo parent $w]
		}
	    }
	}
	default {
	    set w [winfo parent $w]
	}
    }
    return $w
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
#    add_bindtag_recursive
################################################################################

proc cu::add_bindtag_recursive { args } {
    
    set optional {
	{ -pos start|end|before_toplevel|integer start }
    }
    set compulsory "w tag"
    parse_args $optional $compulsory $args

    if { [winfo class $w] eq "Menu" } {
	return
    }
    set b [bindtags $w]
    set ipos [lsearch -exact $b $tag]
    if { $ipos == -1 } {
	switch -- $pos {
	    start { set ipos 0 }
	    end { set ipos [llength $b] }
	    before_toplevel {
		set t [winfo toplevel $w]
		set ipos [lsearch -exact $b $t]
		if { $ipos == -1 } { set ipos [llength $b] }
	    }
	    default {
		set ipos $pos
	    }
	}
	bindtags $w [linsert $b $ipos $tag]
    }
    foreach i [winfo children $w] {
	add_bindtag_recursive -pos $pos $i $tag
    }
}

################################################################################
#    add_dnd_recursive
################################################################################

proc cu::add_dnd_recursive { w } {
    
    if { [info command dnd] eq "" } { return }
    foreach wi [winfo children $w] {
	foreach a  [dnd bindtarget $w] {
	    dnd bindtarget $wi $a <Drop> [dnd bindtarget $w $a]
	}
	cu::add_dnd_recursive $wi
    }
}

################################################################################
#    enable & disable recursive
################################################################################

# state can be: normal or disabled
proc cu::enable_disable_recursive { w state } {
    variable enable_disable_state_save
    
   if { $state eq "disabled" } {
	if { [string match "T*" [winfo class $w]] && [winfo class $w] ne "Toplevel" } {
	    catch {
		set stateS [$w state]
		$w state [concat $stateS disabled]
		dict set enable_disable_state_save $w $stateS
	    }
	} else {
	    catch {
		set stateS [$w cget -state]
		$w configure -state disabled
		dict set enable_disable_state_save $w $stateS   
	    }
	}
    } elseif { [dict exists $enable_disable_state_save $w] } {
	set stateS [dict get $enable_disable_state_save $w]
	if { [string match "T*" [winfo class $w]] && [winfo class $w] ne "Toplevel" } {
	    if { "disabled" ni $stateS } {
		lappend stateS "!disabled"
	    }
	    $w state $stateS
	} else {
	    $w configure -state $stateS
	}
	dict unset enable_disable_state_save $w
    }    
    foreach wi [winfo children $w] {
	enable_disable_recursive $wi $state
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
    variable contextual_menu_to_entry
    
    switch $what {
	init {
	    if { [llength $args] } {
		dict set contextual_menu_to_entry $w $args
	    }
	    bind $w <<Contextual>> [list cu::add_contextual_menu_to_entry $w post %X %Y]
	}
	post {
	    lassign $args x y
	    set menu $w.menu
	    catch { destroy $menu }
	    menu $menu -tearoff 0
	    foreach i [list cut copy paste --- select_all --- clear] readonly [list 0 1 0 0 1 0 0] \
		txt [list [_ "Cut"] [_ "Copy"] [_ "Paste"] --- [_ "Select all"] --- [_ "Clear"]] {
		if { [winfo class $w] eq "TEntry" } {
		    if { ([$w instate disabled] || [$w instate readonly]) && !$readonly } { continue }
		} else {
		    if { [$w cget -state] eq "disabled" && !$readonly } { continue }
		}
		if { $i eq "---" } {
		    $menu add separator
		} else {
		    $menu add command -label $txt -command [list cu::add_contextual_menu_to_entry $w $i]
		}
	    }
	    if { ![info exists contextual_menu_to_entry] } {
		set contextual_menu_to_entry ""
	    }
	    if { [dict exists $contextual_menu_to_entry $w] } {
		$menu add separator
		foreach "n img cmd" [dict get $contextual_menu_to_entry $w] {
		    if { $img ne "" } {
		        set img_cmd [list -image $img -compound left]
		    } else {
		        set img_cmd ""
		    }
		    $menu add command -label $n -command $cmd {*}$img_cmd
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
#     cu::set_focus_recursive
################################################################################

proc cu::set_focus_recursive { w } {
    
    set accepted_classes [list Entry TEntry TCombobox Text]
 
    set err [catch { $w cget -takefocus } ret]
    if { [winfo class $w] in $accepted_classes && !$err } {
	if { $ret ni [list "" 0 1] } {
	    #set err [catch { uplevel #0 $ret $w } ret]
	    set ret 1
	}
	if { !$err && $ret == 1 } {
	    tk::TabToWindow $w
	    return $w
	}
    }
    foreach i [winfo children $w] {
	set ret [cu::set_focus_recursive $i]
	if { $ret ne "" } {
	    return $ret   
	}
    }
    return ""
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
	    # they are class bindings so as search in text widgets can continue working
	    bind Text <$ev> "[list tk::TextInsert %W $k]; break"
	    bind TEntry <$ev> "[list tk::TextInsert %W $k]; break"
	    bind Entry <$ev> "[list tk::TextInsert %W $k]; break"
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

if 0 {
    package require compass_utils
    pack [ttk::entry .e]
    cu::text_entry_bindings .e
}

################################################################################
#     cu::grid_configure_sticky
################################################################################

proc cu::_grid_configure_sticky_check { widget } {

    set width [winfo width $widget]
    set reqwidth [winfo reqwidth $widget]
    set sticky [dict get [grid info $widget] -sticky]
    if { $width >= $reqwidth } {
	set new_sticky w
    } else {
	set new_sticky ew
    }
    if { $new_sticky ne $sticky } {
	grid  configure $widget -sticky $new_sticky
    }
}

proc cu::grid_configure_sticky { widget } {
    grid  configure $widget -sticky ew
    bind $widget <Configure> +[list cu::_grid_configure_sticky_check $widget]
}

################################################################################
#     cu::tooltip
################################################################################

proc cu::tooltip { what w txt } {
    global register_popup_idle
    
    switch $what {
	register {
	    foreach w_i $w {
		cu::add_bindtag_recursive $w_i cu::tooltip
	    }
	    bind cu::tooltip <1> [list cu::tooltip B1 %W $txt]
	    bind cu::tooltip <Enter> [list cu::tooltip enter %W $txt]
	    bind cu::tooltip <Motion> [list cu::tooltip motion %W $txt]
	    bind cu::tooltip <Leave>  [list cu::tooltip leave %W $txt]
	}
	B1 {
	    destroy $w.t
	    catch { grab release $w }
	    update
	}
	enter - motion {
	    destroy $w.t
	    catch { grab release $w }
	    if { ![info exists register_popup_idle] } { set register_popup_idle "" }
	    if { [dict exists $register_popup_idle $w after] } {
		after cancel [dict get $register_popup_idle $w after] 
	    }
	    set xy [winfo pointerxy $w]
	    dict set register_popup_idle $w coords $xy
	    dict set register_popup_idle $w after [after 500 [list cu::tooltip do $w $txt]]
	}
	leave {
	    if { [dict exists $register_popup_idle $w after] } {
		after cancel [dict get $register_popup_idle $w after] 
	    }
	    dict unset register_popup_idle $w
	}
	do {
	    if { ![winfo exists $w] } {
		dict unset register_popup_idle $w
		return
	    }
	    set xy [winfo pointerxy $w]
	    if { $xy ne [dict get $register_popup_idle $w coords] } {
		dict unset register_popup_idle $w
		return
	    }
	    destroy $w.t
	    toplevel $w.t
	    wm overrideredirect $w.t 1
	    wm geometry $w.t +[expr {[winfo rootx $w]+[winfo width $w]}]+[expr {[winfo rooty $w]+[winfo height $w]}]
	    
	    label $w.t.l -text $txt -background #f6f6b8 -highlightthickness 1 -highlightcolor #baba45 -justify left
	    pack $w.t.l
	    catch {
		tkwait visibility $w.t
		grab $w
	    }
	}
    }
}

################################################################################
#    cu::set_window_geometry cu::give_window_geometry
################################################################################

proc cu::give_window_geometry { w } {

    regexp {(\d+)x(\d+)([-+])([-\d]\d*)([-+])([-\d]+)} [wm geometry $w] {} width height m1 x m2 y
    if { $::tcl_platform(platform) eq "unix" } {
	# note: this work in ubuntu 9.04 (disconnected to make it work in kubuntu and others)
	#incr x -4
	#incr y -24
    }
    return ${width}x$height$m1$x$m2$y
}

proc cu::set_window_geometry { w geometry } {

    if { ![regexp {(\d+)x(\d+)([-+])([-\d]\d*)([-+])([-\d]+)} $geometry {} width height m1 x m2 y] } {
	if { [regexp {(\d+)x(\d+)} $geometry {} width height] } {
	    lassign [list 0 0 + +] x y m1 m2
	} else {
	    regexp {([-+])([-\d]\d*)([-+])([-\d]+)} $geometry {} m1 x m2 y
	    lassign "" width height
	}
    }
    if { $m1 eq "-" && $width ne "" } {
	set x [expr {[winfo screenwidth $w]-$x-$width}]
    }
    if { $m2 eq "-" && $height ne "" } {
	set y [expr {[winfo screenheight $w]-$y-$height}]
    }
    if { $x < 0 } { set x 0 }
    if { $y < 0 } { set y 0 }
    if { $x > [winfo screenwidth $w]-100 } { set x [expr {[winfo screenwidth $w]-100}] }
    if { $y > [winfo screenheight $w]-100 } { set y [expr {[winfo screenheight $w]-100}] }

    if { $width ne "" } {
	if { $x+$width > [winfo screenwidth $w] } {
	    set width [expr {[winfo screenwidth $w]-$x}]
	}
	if { $y+$height > [winfo screenheight $w] } {
	    set height [expr {[winfo screenheight $w]-$y}]
	}
	wm geometry $w ${width}x$height+$x+$y
    } else {
	wm geometry $w +$x+$y
    }
    bind $w <Configure> [list cu::_set_window_geometry_correct $w]
}

proc cu::_set_window_geometry_correct { w } {
    regexp {(\d+)x(\d+)([-+])([-\d]\d*)([-+])([-\d]+)} [wm geometry $w] {} - height_wm - - - y_wm
    regexp {(\d+)x(\d+)([-+])([-\d]\d*)([-+])([-\d]+)} [winfo geometry $w] {} width height m1 x m2 y
    if { $height <= 1 } {
	return
    }
    if { $y+$height > [winfo screenheight $w] } {
	set height [expr {[winfo screenheight $w]-$y}]
	wm geometry $w ${width}x$height
    }
    bind $w <Configure> ""
}

proc cu::create_tooltip_toplevel { args } {

    set optional {
	{ -withdraw "" 0 }
    }
    set compulsory "b"
    parse_args $optional $compulsory $args

    toplevel $b -class Tooltip
    if { $withdraw } {
	wm withdraw $b
    }
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
	    after 100 [list focus -force $focus]
	}
    }
    return $b
}

################################################################################
#    cu::add_write_trace cu::enable_disable_on_variable and change_variable_on_variable
################################################################################

proc cu::add_write_trace { w variable_name cmd } {
    
    trace add variable $variable_name write "$cmd;#"
    bind $w <Destroy> +[list trace remove variable $variable_name write "$cmd;#"]
}

# dict contains values and active widgets for these values
# example: if dict contains:
#    value1 "w1 w2" value2 "w3 w4"
# when value of variable is changed to 'value1', widgets w1 w2 will be enabled
# and widgets w3 w4 will be disabled

proc cu::enable_disable_on_variable { args } {
    set optional {
	{ -clear "" 0 }
    }
    set compulsory "w variable_name dict"
    parse_args $optional $compulsory $args
    
    set cmd "[list cu::_enable_disable_on_variable_cmd $variable_name $dict]; #"
    if { $clear } {
	trace remove variable $variable_name write $cmd
    }
    trace add variable $variable_name write $cmd
    bind $w <Destroy> +[list trace remove variable $variable_name write $cmd]
    catch $cmd
}
proc cu::_enable_disable_on_variable_cmd { variable_name dict } {
    
    upvar #0 $variable_name var
    dict for "n v" $dict {
	if { $n ne $var } {
	    foreach w $v {
		set i_action disable
		if { [regexp {^([-+])(.*)} $w {} sign w] } {
		    if { $sign eq "-" } {
		        set i_action enable
		    } else {
		        continue
		    }
		}
		cu::_enable_disable_widget $w $i_action
	    }
	}
    }
    if { [dict exists $dict $var] } {
	set v [dict get $dict $var]
    } elseif { [dict exists $dict ""] } {
	set v [dict get $dict ""]
    } else {
	set v ""
    }
    foreach w $v {
	set i_action enable
	if { [regexp {^([-+])(.*)} $w {} sign w] } {
	    if { $sign eq "-" } { set i_action disable }
	}
	cu::_enable_disable_widget $w $i_action
    }
}
    
proc cu::_enable_disable_widget { w enable_disable } {
    switch [winfo class $w] {
	Canvas {
	    switch $enable_disable {
		enable { $w itemconfigure all -fill black }
		disable { $w itemconfigure all -fill grey }
	    }
	}
	default {
	    switch $enable_disable {
		enable {
		    set err [catch { $w state !disabled }]
		    if { $err } { catch { $w configure -state normal } }
		}
		disable {
		    set err [catch { $w state disabled }]
		    if { $err } { catch { $w configure -state disabled } }
		}
	    }
	}
    }
    foreach i [winfo children $w] {
	cu::_enable_disable_widget $i $enable_disable
    }
}

# dict contains values and "variable2 newvalue" pairs for these values
# example: if dict contains:
#    value1 "variable2 1" value2 "variable2 0 variable3 v" default "variable4 1"
# when value of variable is changed to 'value1', the value of variable "variable2" is
# changed to "1". when value of variable is changed to 'value2', the value
# of variable "variable2" is changed to "0" and the value of "variable3" is changed to "v"
# for any other value, variable4 is changed to 1
# there can be a "default" value that is applied if none of the other values apply
# if a value for a variable is not given, it is just updated to raise traces

proc cu::change_variable_on_variable { args } {
    set optional {
	{ -clear "" 0 }
    }
    set compulsory "w variable_name dict"
    parse_args $optional $compulsory $args
	
    set cmd "[list cu::_change_variable_on_variable_cmd $variable_name $dict]; #"
    if { $clear } {
	trace remove variable $variable_name write $cmd
    }
    trace add variable $variable_name write $cmd
    bind $w <Destroy> +[list trace remove variable $variable_name write $cmd]
    catch $cmd
}

proc cu::_change_variable_on_variable_cmd { variable_name dict } {
    
    upvar #0 $variable_name var
    if { [dict exists $dict $var] } {
	set v [dict get $dict $var]
	if { [llength $v]%2 == 1 } {
	    upvar #0 [lindex $v end] var2
	    lappend v $var2
	}
	foreach "k v" $v {
	    upvar #0 $k var2
	    set var2 $v
	}
    } elseif { [dict exists $dict default] } {
	set v [dict get $dict default]
	if { [llength $v]%2 == 1 } {
	    upvar #0 [lindex $v end] var2
	    lappend v $var2
	}
	foreach "k v" $v {
	    upvar #0 $k var2
	    set var2 $v
	}
    }
}

################################################################################
#    rounded rectangle
################################################################################

#----------------------------------------------------------------------
 #
 # cu::round_rect --
 #
 #       Draw a rounded rectangle in the canvas.
 #
 # Parameters:
 #       w - Path name of the canvas
 #       x0, y0 - Co-ordinates of the upper left corner, in pixels
 #       x3, y3 - Co-ordinates of the lower right corner, in pixels
 #       radius - Radius of the bend at the corners, in any form
 #                acceptable to Tk_GetPixels
 #       args - Other args suitable to a 'polygon' item on the canvas
 #
 # Results:
 #       Returns the canvas item number of the rounded rectangle.
 #
 # Side effects:
 #       Creates a rounded rectangle as a smooth polygon in the canvas.
 #
 #----------------------------------------------------------------------

proc cu::round_rect { w x0 y0 x3 y3 radius args } {

    set r [winfo pixels $w $radius]
    set d [expr { 2 * $r }]

    # Make sure that the radius of the curve is less than 3/8
    # size of the box!

    set maxr 0.75

    if { $d > $maxr * ( $x3 - $x0 ) } {
	set d [expr { $maxr * ( $x3 - $x0 ) }]
    }
    if { $d > $maxr * ( $y3 - $y0 ) } {
	set d [expr { $maxr * ( $y3 - $y0 ) }]
    }

    set x1 [expr { $x0 + $d }]
    set x2 [expr { $x3 - $d }]
    set y1 [expr { $y0 + $d }]
    set y2 [expr { $y3 - $d }]

    set cmd [list $w create polygon]
    lappend cmd $x0 $y0
    lappend cmd $x1 $y0
    lappend cmd $x2 $y0
    lappend cmd $x3 $y0
    lappend cmd $x3 $y1
    lappend cmd $x3 $y2
    lappend cmd $x3 $y3
    lappend cmd $x2 $y3
    lappend cmd $x1 $y3
    lappend cmd $x0 $y3
    lappend cmd $x0 $y2
    lappend cmd $x0 $y1
    lappend cmd -smooth 1
    return [eval $cmd $args]
 }

################################################################################
#    icons
################################################################################

# program_name is to store a copy of the database (if necessary) in a program
# dependent dir

namespace eval cu {
    variable icons_lib_db
}
namespace eval cu::icons {}
namespace eval cu::icons_contents {}

proc cu::init_icons_lib { dbfile program_name } {
    variable icons_lib_db

    set dbfile [file normalize $dbfile]
    if { [lindex [file system $dbfile] 0] ne "native" } {
	set nfile [file join [cu::file::appdatadir $program_name] \
		[file tail $dbfile]]
	if { ![file exists $nfile] || [file mtime $nfile] < [file mtime $dbfile] } {
	    file copy -force $dbfile $nfile
	}
	set dbfile $nfile
    }
    package require sqlite3

    set idx 1
    while { [info command ::cu::icons_lib_db$idx] ne "" } { incr idx }
    set icons_lib_db ::cu::icons_lib_db$idx
    sqlite3 $icons_lib_db $dbfile
}

proc cu::is_icons_lib_init {} {
    variable icons_lib_db
    return [info exists icons_lib_db]
}

proc cu::get_icon { args } {
    variable icons_lib_db
    
    set optional {
	{ -contents boolean 0 }
    }
    set compulsory "icon_name"
    parse_args $optional $compulsory $args
    
    if { ![info exists icons_lib_db] } {
	return "icons lib library has not been initialized"
    }

    if { $icon_name eq "" } {
	error "icon with void name not valid"
    }
    if { !$contents } {
	if { [info command ::cu::icons::$icon_name] ne "" } {
	    return ::cu::icons::$icon_name
	}
    } else {
	if { [info exists ::cu::icons_contents::$icon_name] } {
	    return [set ::cu::icons_contents::$icon_name]
	}
    }
    set data [$icons_lib_db onecolumn { select data from icons
	    where name=$icon_name }]
    if { $data eq "" } {
	error "error in cu::get_icon. $icon_name not found"
    }
    if { !$contents } {
	image create photo ::cu::icons::$icon_name -data $data
	return ::cu::icons::$icon_name
    } else {
	set ::cu::icons_contents::$icon_name $data
	return [set ::cu::icons_contents::$icon_name]
    }
}

proc cu::get_all_icon_names {} {
    variable icons_lib_db
    
    return [$icons_lib_db eval { select name from icons order by name }]
}

namespace eval cu::images {}
namespace eval cu::images_contents {}

proc cu::get_images_add_dir { dir } {
    variable get_images_dirs
    
    if { ![info exists get_images_dirs] } {
	set get_images_dirs ""
    }
    
    set dir [file normalize $dir]
    if { [lsearch -exact $get_images_dirs $dir] == -1 } {
	lappend get_images_dirs $dir
    }
    return $get_images_dirs
}

proc cu::get_image { args } {
    variable get_images_dirs
    
    set optional {
	{ -contents boolean 0 }
    }
    set compulsory "name"
    parse_args $optional $compulsory $args
    
    if { $name eq "" } {
	error "image with void name not valid"
    }
    if { !$contents } {
	if { [info command ::cu::images::$name] ne "" } {
	    return ::cu::images::$name
	}
    } else {
	if { [info exists ::cu::images_contents::$name] } {
	    return [set ::cu::images_contents::$name]
	}
    }
    if { ![info exists get_images_dirs] } {
	set get_images_dirs ""
    }
    if { [file pathtype $name] eq "absolute" } {
	set file $name
    } else {
	set found 0
	foreach dir $get_images_dirs {
	    set ret [glob -nocomplain -dir $dir $name]
	    if { [llength $ret] > 0 } {
		set found 1
		break
	    }
	    set ret [glob -nocomplain -dir $dir $name.*]
	    if { [llength $ret] > 0 } {
		set found 1
		break
	    }
	}
	if { !$found } {
	    error "image '$name' not found in '$get_images_dirs'"
	}
	set file [lindex $ret 0]
    }
    if { !$contents } {
	image create photo ::cu::images::$name -file $file
	return ::cu::images::$name
    } else {
	set fin [open $file rb]
	set ::cu::images_contents::$name [read $fin]
	close $fin
	return [set ::cu::images_contents::$name]
    }
}

proc cu::get_image_name_from_fullpath { file } {
    variable get_images_dirs
    
    set fdir [file normalize [file dirname $file]]
    foreach dir $get_images_dirs {
	if { [file normalize $dir] ne $fdir } { continue }
	if { [file exists $file] } {
	    return [file tail $file]
	}
    }
    return ""
}


proc cu::get_image_or_icon { args } {
    
    set optional {
	{ -contents boolean 0 }
    }
    set compulsory "name"
    parse_args $optional $compulsory $args
    
    set err [catch { get_image -contents $contents $name } img]
    if { !$err } { return $img }
    return [get_icon -contents $contents $name]
}

proc cu::get_image_selected { name } {

    package require fulltktree
    
    get_image $name
    set name_sel $name-selected
    
    if { [info command ::cu::images::$name_sel] ne "" } {
	return ::cu::images::$name_sel
    }
    image create photo ::cu::images::$name_sel
    ::cu::images::$name_sel copy ::cu::images::$name
    imagetint ::cu::images::$name_sel $fulltktree::SystemHighlight 128
    return ::cu::images::$name_sel
}

proc cu::reset_images_cache {} {
    
    foreach i [info vars ::cu::images::*] {
	catch { image delete $i }
	unset -nocomplain $i
    }
}

proc cu::get_cursors_init { program_name } {
    variable get_cursors_dirs
    variable get_cursors_appname_dir
    
    if { ![info exists get_cursors_dirs] } {
	set get_cursors_dirs ""
    }
    set get_cursors_appname_dir [file normalize [file join \
	    [cu::file::appdatadir $program_name] cursors]]
    set get_cursors_dirs [linsert0 $get_cursors_dirs $get_cursors_appname_dir]
    return $get_cursors_appname_dir

}

proc cu::get_cursors_add_dir { dir } {
    variable get_cursors_dirs
    
    if { ![info exists get_cursors_dirs] } {
	set get_cursors_dirs ""
    }
    
    set dir [file normalize $dir]
    if { [lsearch -exact $get_cursors_dirs $dir] == -1 } {
	lappend get_cursors_dirs $dir
    }
    return $get_cursors_dirs
}

namespace eval cu::cursors {}

proc cu::get_cursor { name } {
    variable get_cursors_dirs
    variable get_cursors_appname_dir
    
    if { $name eq "" } {
	error "cursor with void name not valid"
    }
    if { [info exists ::cu::cursors::$name] } {
	return [set ::cu::cursors::$name]
    }

    if  { [tk windowingsystem] eq "aqua" } {
	set ::cu::cursors::$name ""
	return [set ::cu::cursors::$name]
    }

    if { ![info exists get_cursors_dirs] } {
	set get_cursors_dirs ""
    }
    if { [file pathtype $name] eq "absolute" } {
	if { $::tcl_platform(platform) eq "windows" } {
	    set ::cu::cursors::$name "[list @$name.cur]"
	} else {
	    set ::cu::cursors::$name "[list @$name.x11cursor black]"
	}
	return [set ::cu::cursors::$name]
    }
    set found 0
    foreach dir $get_cursors_dirs {
	if { $::tcl_platform(platform) eq "windows" } {
	    set file [file join $dir $name.cur]
	} else {
	    set file [file join $dir $name.x11cursor]
	}
	if { [file exists $file] } {
	    if { [lindex [file system $file] 0] ne "native" } {
		file mkdir $get_cursors_appname_dir
		set nfile [file join $get_cursors_appname_dir [file tail $file]]
		if { ![file exists $nfile] || [file mtime $nfile] < [file mtime $file] } {
		    file copy -force $file $nfile
		}
		set file $nfile
	    }
	    set found 1
	    break
	}
    }
    if { !$found } {
	error "cursor '$name' not found in '$get_cursors_dirs'"
    }
    if { $::tcl_platform(platform) eq "windows" } {
	set ::cu::cursors::$name "[list @$file]"
    } else {
	set ::cu::cursors::$name "[list @$file black]"
    }
    return [set ::cu::cursors::$name]
}

################################################################################
#    shape window functions
################################################################################

namespace eval cu::img {}

proc cu::img::shape_window {args } {
    
    set optional {
	{ -geometry geom "" }
    }
    set compulsory " w image"
    parse_args $optional $compulsory $args
    
    destroy $w
    toplevel $w -bd 0 -highlightthickness 0 -background white
    wm withdraw $w
    wm overrideredirect $w 1
    if { $::tcl_platform(platform) eq "windows" } {
	wm attributes $w -transparentcolor white
    }
    #grid [label $w.l -image $image -background white -bd 0]
    grid [canvas $w.l -background white -bd 0 -width [image width $image] \
	    -height [image height $image] -highlightthickness 0]
    $w.l create image 0 0 -image $image -anchor nw
    if { $geometry ne "" } {
	wm geometry $w $geometry
    }
    update idletasks
    wm deiconify $w
 
    if { $::tcl_platform(platform) ne "windows" } {
	cu::img::shape set $w photo $image
    }
    return $w.l
}

if 0 {
    load ./compass_utils_tk_64.so compass_utils_tk
    set img [image create photo -file {~/mytcltk/gid_groups_conds_dir/gid_groups_conds/images/gear-22.gif}]
    wm withdraw .
    cu::img::shape_window .t $img
    wm geometry .t +600+600
    bind . <KeyPress> exit
}

namespace eval cu {
    get_images_add_dir [file join $topdir images]
}

################################################################################
#    clipboard
################################################################################

namespace eval cu::img {
    variable registered_clipboard_types ""
}

proc cu::img::clipboard_unix { what args } {
    variable registered_clipboard_types
    
    switch -- $what {
	clear  {
	    ::clipboard clear
	}
	append {
	    set optional {
		{ -type type "" }
		{ -format format "" }
		{ -displayof window . }
	    }
	    set compulsory "data"
	    parse_args $optional $compulsory $args

	    switch $type {
		tclimage {
		    set data [base64::decode [$data data -format png]]
		    ::clipboard append  -displayof $displayof -type image/png $data
		}
		default {
		    ::clipboard append {*}$args
		}
	    }
	}
	get {
	    set optional {
		{ -type type "" }
		{ -displayof window . }
	    }
	    set compulsory ""
	    parse_args $optional $compulsory $args
	    
	    switch -- $type {
		"HTML Format" {
		    if { "text/html" ni [::clipboard get -type TARGETS] } {
		        error "error in clipboard. Type text/html not available"
		    }
		    set txt [binary format c* [::clipboard get -displayof $displayof -type text/html]]
		    #return [encoding convertfrom unicode $txt]
		    #return $txt
		    return [encoding convertfrom utf-8 $txt]
		}
		tclimage {
		    if { "image/png" ni [::clipboard get -type TARGETS] } {
		        error "error in clipboard. Type image/png not available"
		    }
		    package require img::png
		    set data [::clipboard get -displayof $displayof -type image/png]
		    set err [catch { image create photo -data $data } img]
		    if { $err } {
		        set err [catch { image create photo -data [binary format c* $data] } img]
		    }
		    if { $err } {
		        error "error in clipboard. Contents is not a valid image"
		    }
		    return $img
		}
		tcltext - UTF8_STRING - STRING - "" {
		    if { $type eq "tcltext" } {
		        set type ""
		    }
		    # note: without the type, the string contained utf-8 bytes incorrectly in ubuntu 10.10
		    return [::clipboard get {*}$args -type UTF8_STRING]
		}
		default {
		    if { $type ni [::clipboard get -type TARGETS] } {
		        error "error in clipboard. Type $type not available"
		    }
		    if { [dict exists $registered_clipboard_types $type] } {
		        return [::clipboard get {*}$args]
		    } else {
		        return [binary format c* [::clipboard get {*}$args]]
		    }
		}
	    }
	}
	register {
	    set optional {
		{ -encoding encoding "" }
	    }
	    set compulsory "name"
	    parse_args $optional $compulsory $args
	    dict set registered_clipboard_types $name $encoding
	}
	isimage {
	    set err [catch { ::clipboard get -type TARGETS } targets]
	    if { $err } { return 0 }
	    return [expr {  "image/png" in $targets }]
	}
	types - alltypes {
	    set types [::clipboard get -type TARGETS]
	    if { "STRING" in $types } {
		set types [linsert $types 0 tcltext]
	    }
	    if { "image/png" in $types } {
		set types [linsert $types 0 tclimage]
	    }
	    return $types
	}
	default {
	    error "error. argument should be clear, append, get, register, isimage, types, alltypes"
	}
    }
}

if { $::tcl_platform(platform) eq "unix" } {
    interp alias "" cu::img::clipboard "" cu::img::clipboard_unix
}












