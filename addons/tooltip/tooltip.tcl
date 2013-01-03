# tooltip.tcl --
#
#       Balloon help
#
# Copyright (c) 1996-2007 Jeffrey Hobbs
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
# RCS: @(#) $Id: tooltip.tcl,v 1.8 2007/05/18 19:28:21 hobbs Exp $
#
# Initiated: 28 October 1996


package require Tk 8.4
package provide tooltip 1.3
package require msgcat

#------------------------------------------------------------------------
# PROCEDURE
#        tooltip::tooltip
#
# DESCRIPTION
#        Implements a tooltip (balloon help) system
#
# ARGUMENTS
#        tooltip <option> ?arg?
#
# clear ?pattern?
#        Stops the specified widgets (defaults to all) from showing tooltips
#
# delay ?millisecs?
#        Query or set the delay.  The delay is in milliseconds and must
#        be at least 50.  Returns the delay.
#
# disable OR off
#        Disables all tooltips.
#
# enable OR on
#        Enables tooltips for defined widgets.
#
# <widget> ?-index index? ?-item id? ?message?
#        If -index is specified, then <widget> is assumed to be a menu
#        and the index represents what index into the menu (either the
#        numerical index or the label) to associate the tooltip message with.
#        Tooltips do not appear for disabled menu items.
#        If message is {}, then the tooltip for that widget is removed.
#        The widget must exist prior to calling tooltip.  The current
#        tooltip message for <widget> is returned, if any.
#
# RETURNS: varies (see methods above)
#
# NAMESPACE & STATE
#        The namespace tooltip is used.
#        Control toplevel name via ::tooltip::wname.
#
# EXAMPLE USAGE:
#        tooltip .button "A Button"
#        tooltip .menu -index "Load" "Loads a file"
#
#------------------------------------------------------------------------

namespace eval ::tooltip {
    namespace export -clear tooltip
    variable tooltip
    variable G

    array set G {
	enabled                1
	DELAY                500
	AFTERID                {}
	LAST                -1
	TOPLEVEL        .__tooltip__
    }

    # The extra ::hide call in <Enter> is necessary to catch moving to
    # child widgets where the <Leave> event won't be generated
    bind Tooltip <Enter> [namespace code {
	#tooltip::hide
	variable tooltip
	variable G
	set G(LAST) -1
	if {$G(enabled) && [info exists tooltip(%W)]} {
	    set G(AFTERID) \
		[after $G(DELAY) [namespace code [list show %W $tooltip(%W) cursor]]]
	}
    }]

    bind Menu <<MenuSelect>>        [namespace code { menuMotion %W }]
    bind Tooltip <Leave>        [namespace code hide]
    bind Tooltip <Any-KeyPress>        [namespace code hide]
    bind Tooltip <Any-Button>        [namespace code hide]
}

proc ::tooltip::tooltip {w args} {
    variable tooltip
    variable G
    switch -- $w {
	clear        {
	    if {[llength $args]==0} { set args .* }
	    clear $args
	}
	delay        {
	    if {[llength $args]} {
		if {![string is integer -strict $args] || $args<50} {
		    return -code error "tooltip delay must be an\
		            integer greater than 50 (delay is in millisecs)"
		}
		return [set G(DELAY) $args]
	    } else {
		return $G(DELAY)
	    }
	}
	off - disable        {
	    set G(enabled) 0
	    hide
	}
	on - enable        {
	    set G(enabled) 1
	}
	default {
	    set i $w
	    if {[llength $args]} {
		set i [uplevel 1 [namespace code "register [list $w] $args"]]
	    }
	    set b $G(TOPLEVEL)
	    if {![winfo exists $b]} {
		toplevel $b -class Tooltip
		if {[tk windowingsystem] eq "aqua"} {
		    ::tk::unsupported::MacWindowStyle style $b help none
		} else {
		    wm overrideredirect $b 1
		}
		catch {wm attributes $b -topmost 1}
		wm positionfrom $b program
		wm withdraw $b
		label $b.label -highlightthickness 0 -relief solid -bd 1 \
		        -background lightyellow -fg black
		pack $b.label -ipadx 1
	    }
	    if {[info exists tooltip($i)]} { return $tooltip($i) }
	}
    }
}

proc ::tooltip::register {w args} {
    variable tooltip
    set key [lindex $args 0]
    while {[string match -* $key]} {
	switch -- $key {
	    -index        {
		if {[catch {$w entrycget 1 -label}]} {
		    return -code error "widget \"$w\" does not seem to be a\
		            menu, which is required for the -index switch"
		}
		set index [lindex $args 1]
		set args [lreplace $args 0 1]
	    }
	    -item        {
		set namedItem [lindex $args 1]
		if {[catch {$w find withtag $namedItem} item]} {
		    return -code error "widget \"$w\" is not a canvas, or item\
		            \"$namedItem\" does not exist in the canvas"
		}
		if {[llength $item] > 1} {
		    return -code error "item \"$namedItem\" specifies more\
		            than one item on the canvas"
		}
		set args [lreplace $args 0 1]
	    }
	    -tag {
		set tag [lindex $args 1]
		set r [catch {lsearch -exact [$w tag names] $tag} ndx]
		if {$r || $ndx == -1} {
		    return -code error "widget \"$w\" is not a text widget or\
		        \"$tag\" is not a text tag"
		}
		set args [lreplace $args 0 1]
	    }
	    default        {
		return -code error "unknown option \"$key\":\
		        should be -index or -item"
	    }
	}
	set key [lindex $args 0]
    }
    if {[llength $args] != 1} {
	return -code error "wrong # args: should be \"tooltip widget\
		?-index index? ?-item item? ?-tag tag? message\""
    }
    if {$key eq ""} {
	clear $w
    } else {
	if {![winfo exists $w]} {
	    return -code error "bad window path name \"$w\""
	}
	if {[info exists index]} {
	    set tooltip($w,$index) $key
	    return $w,$index
	} elseif {[info exists item]} {
	    set tooltip($w,$item) $key
	    enableCanvas $w $item
	    return $w,$item
	} elseif {[info exists tag]} {
	    set tooltip($w,t_$tag) $key
	    enableTag $w $tag
	    return $w,$tag
	} else {
	    set tooltip($w) $key
	    bindtags $w [linsert [bindtags $w] end "Tooltip"]
	    return $w
	}
    }
}

proc ::tooltip::clear {{pattern .*}} {
    variable tooltip
    foreach w [array names tooltip $pattern] {
	unset tooltip($w)
	if {[winfo exists $w]} {
	    set tags [bindtags $w]
	    if {[set i [lsearch -exact $tags "Tooltip"]] != -1} {
		bindtags $w [lreplace $tags $i $i]
	    }
	    ## We don't remove TooltipMenu because there
	    ## might be other indices that use it
	}
    }
}

proc ::tooltip::show {w msg {i {}}} {
    # Use string match to allow that the help will be shown when
    # the pointer is in any child of the desired widget
    set we [winfo exists $w]
    if { !$we } { return }
    set wc [string match $w* [eval [list winfo containing] \
		                  [winfo pointerxy $w]]]
    set wm [string equal [winfo class $w] "Menu"]
    if {!$we || (!$wm && !$wc)} {
	return
    }

    variable G

    set b $G(TOPLEVEL)
    # Use late-binding msgcat (lazy translation) to support programs
    # that allow on-the-fly l10n changes
    $b.label configure -text [::msgcat::mc $msg]
    update idletasks
    set screenw [winfo screenwidth $w]
    set screenh [winfo screenheight $w]
    set reqw [winfo reqwidth $b]
    set reqh [winfo reqheight $b]
    # When adjusting for being on the screen boundary, check that we are
    # near the "edge" already, as Tk handles multiple monitors oddly
    if {$i eq "cursor"} {
	set y [expr {[winfo pointery $w]+20}]
	if {($y < $screenh) && ($y+$reqh) > $screenh} {
	    set y [expr {[winfo pointery $w]-$reqh-5}]
	}
    } elseif {$i ne ""} {
	set y [expr {[winfo rooty $w]+[winfo vrooty $w]+[$w yposition $i]+25}]
	if {($y < $screenh) && ($y+$reqh) > $screenh} {
	    # show above if we would be offscreen
	    set y [expr {[winfo rooty $w]+[$w yposition $i]-$reqh-5}]
	}
    } else {
	set y [expr {[winfo rooty $w]+[winfo vrooty $w]+[winfo height $w]+5}]
	if {($y < $screenh) && ($y+$reqh) > $screenh} {
	    # show above if we would be offscreen
	    set y [expr {[winfo rooty $w]-$reqh-5}]
	}
    }
    if {$i eq "cursor"} {
	set x [winfo pointerx $w]
    } else {
	set x [expr {[winfo rootx $w]+[winfo vrootx $w]+
		     ([winfo width $w]-$reqw)/2}]
    }
    # only readjust when we would appear right on the screen edge
    if {$x<0 && ($x+$reqw)>0} {
	set x 0
    } elseif {($x < $screenw) && ($x+$reqw) > $screenw} {
	set x [expr {$screenw-$reqw}]
    }
    if {[tk windowingsystem] eq "aqua"} {
	set focus [focus]
    }
    wm geometry $b +$x+$y
    wm deiconify $b
    raise $b
    if {[tk windowingsystem] eq "aqua" && $focus ne ""} {
	# Aqua's help window steals focus on display
	after idle [list focus -force $focus]
    }
}

proc ::tooltip::menuMotion {w} {
    variable G

    if {$G(enabled)} {
	variable tooltip

	# Menu events come from a funny path, map to the real path.
	set m [string map {"#" "."} [winfo name $w]]
	set cur [$w index active]

	# The next two lines (all uses of LAST) are necessary until the
	# <<MenuSelect>> event is properly coded for Unix/(Windows)?
	if {$cur == $G(LAST)} return
	set G(LAST) $cur
	# a little inlining - this is :hide
	after cancel $G(AFTERID)
	catch {wm withdraw $G(TOPLEVEL)}
	if {[info exists tooltip($m,$cur)] || \
		(![catch {$w entrycget $cur -label} cur] && \
		[info exists tooltip($m,$cur)])} {
	    set G(AFTERID) [after $G(DELAY) \
		    [namespace code [list show $w $tooltip($m,$cur) cursor]]]
	}
    }
}

proc ::tooltip::hide {args} {
    variable G

    after cancel $G(AFTERID)
    catch {wm withdraw $G(TOPLEVEL)}
}

proc ::tooltip::wname {{w {}}} {
    variable G
    if {[llength [info level 0]] > 1} {
	# $w specified
	if {$w ne $G(TOPLEVEL)} {
	    hide
	    destroy $G(TOPLEVEL)
	    set G(TOPLEVEL) $w
	}
    }
    return $G(TOPLEVEL)
}

proc ::tooltip::itemTip {w args} {
    variable tooltip
    variable G

    set G(LAST) -1
    set item [$w find withtag current]
    if {$G(enabled) && [info exists tooltip($w,$item)]} {
	set G(AFTERID) [after $G(DELAY) \
		[namespace code [list show $w $tooltip($w,$item) cursor]]]
    }
}

proc ::tooltip::enableCanvas {w args} {
    $w bind all <Enter> +[namespace code [list itemTip $w]]
    $w bind all <Leave>        +[namespace code hide]
    $w bind all <Any-KeyPress> +[namespace code hide]
    $w bind all <Any-Button> +[namespace code hide]
}

proc ::tooltip::tagTip {w tag} {
    variable tooltip
    variable G
    set G(LAST) -1
    if {$G(enabled) && [info exists tooltip($w,t_$tag)]} {
	set G(AFTERID) [after $G(DELAY) \
	    [namespace code [list show $w $tooltip($w,t_$tag) cursor]]]
    }
}

proc ::tooltip::enableTag {w tag} {
    $w tag bind $tag <Enter> +[namespace code [list tagTip $w $tag]]
    $w tag bind $tag <Leave> +[namespace code hide]
    $w tag bind $tag <Any-KeyPress> +[namespace code hide]
    $w tag bind $tag <Any-Button> +[namespace code hide]
}
