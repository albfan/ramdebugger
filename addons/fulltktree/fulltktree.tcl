
package require treectrl
package require tile
package require snit
package require autoscroll
#package require compass_utils

package provide fulltktree 1.0

if { [info command ttk::style] eq "" } {
    interp alias "" ttk::style "" style
}

namespace eval ::fulltktree_top {
    variable topdir [file dirname [info script]]
}
source [file join $::fulltktree_top::topdir fulltktree_bindings.tcl]

# for tclIndex
proc fulltktree { args } {}

#  -editbeginhandler can return:
# 0: no edition; 1,entry: edit with entry; combo "editable values dict": edit with combo;
# combotree  "editable values_tree"

snit::widget fulltktree {
    option -selecthandler "" ;# button-1 or return (if not selecthandler2)
    option -selecthandler2 "" ;# double button or return
    option -returnhandler "" ;# if not null, previous options do not react on return
    option -contextualhandler ""
    option -editbeginhandler "" ;# if returns 0, the edition does not begin (optional)
    option -editaccepthandler ""
    option -deletehandler ""
    option -draghandler ""
    option -columns ""
    option -expand 0
    option -compass_background 0
    option -selectmode extended
    option -sensitive_cols "" ;# can be a list of column indexes or all
    option -item_image ""
    option -folder_image ""
    option -buttonpress_open_close 1
    option -has_sizegrip 0

    variable tree
    variable vscrollbar
    variable hscrollbar

    typevariable SystemHighlight
    typevariable SystemHighlightText
    typevariable SystemFont

    variable searchstring
    variable searchstring_reached_end

    variable sortcolumn
    variable itemStyle

    variable editcolumns ""

    variable uservar
    typevariable type_uservar

    delegate method * to tree
    delegate method _selection to tree as selection
    delegate option * to tree
    delegate option -borderwidth to hull
    delegate option -bd to hull
    delegate option -relief to hull

    constructor { args } {

	$self createimages_colors
	
	set height [font metrics $fulltktree::SystemFont -linespace]
	if {$height < 18} { set height 18 }

	install tree as treectrl $win.t -highlightthickness 0 -borderwidth 0 \
	    -xscrollincrement 20 -showheader 1 -indent 19 \
	    -font $fulltktree::SystemFont -itemheight $height -selectmode extended -showroot no \
	    -showrootbutton no -showbuttons yes -showlines yes \
	    -scrollmargin 16 -xscrolldelay "500 50" -yscrolldelay "500 50" \
	    -yscrollincrement $height

	catch { $win.t configure -usetheme 1 }
	    
	install vscrollbar as scrollbar $win.sv -orient vertical -command [list $win.t yview]
	install hscrollbar as scrollbar $win.sh -orient horizontal -command [list $win.t xview]

	set err [catch {
		$tree configure -openbuttonimage mac-collapse \
		    -closedbuttonimage mac-expand
	    }]
	if { $err } {
	    $tree configure -buttonimage [list mac-collapse open mac-expand !open ]
	}
	autoscroll::autoscroll $win.sv
	
	$self configure -background white
	
	$tree state define disabled
	$tree state define emphasis

	bind $win <FocusIn> [list after idle [list focus $tree]]
	grid $win.t $win.sv -sticky ns
	grid $win.sh -sticky ew
	grid configure $win.t -sticky nsew
	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 0 -weight 1

	$tree element create e_folder_image image -image \
	    {folder-open {open} folder-closed {}}

	$tree element create e_item_image image -image appbook16
	$tree element create e_image image
	
	set bfont [concat [font actual [$tree cget -font]] [list -weight bold]]
	$tree element create e_text_sel text -lines 1 \
	    -fill [list grey disabled \
		$fulltktree::SystemHighlightText {selected focus} \
		] -font [list $bfont emphasis]
	$tree element create e_rect rect -fill \
	    [list $fulltktree::SystemHighlight {selected focus} gray {selected !focus}] \
	    -showfocus yes -open we
	$tree element create e_hidden text -lines 1 -format " " -datatype string
	$tree element create e_selmarker_up rect 
	$tree element create e_selmarker_down rect
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree element create e_window window
	}
	$tree element create e_rect_disabled rect -fill grey \
	    -showfocus no -width 20 -height 20
	set S [$tree style create folder -orient horizontal]
	$tree style elements $S [list e_rect e_folder_image e_text_sel \
		e_selmarker_up e_selmarker_down]
	$tree style layout $S e_folder_image -expand ns
	$tree style layout $S e_text_sel -padx {4 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky w
	}
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2
	$tree style layout $S e_selmarker_up -detach 1
	    
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -width 100 -height 2 -sticky nw
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	     -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -width 100 -height 2 -sticky sw
	}
	set S [$tree style create item -orient horizontal]
	$tree style elements $S [list e_rect e_item_image e_text_sel \
		e_selmarker_up e_selmarker_down]
	$tree style layout $S e_item_image -expand ns
	$tree style layout $S e_text_sel -padx {4 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky w
	}
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2
	$tree style layout $S e_selmarker_up -detach 1
	    
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -width 100 -height 2 -sticky nw
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	    -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -width 100 -height 2 -sticky sw 
	}

	set S [$tree style create imagetext -orient horizontal]
	$tree style elements $S [list e_rect e_image e_text_sel]
	$tree style layout $S e_image -expand ns
	$tree style layout $S e_text_sel -padx {4 0} -squeeze x -expand ns
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2

	set S [$tree style create text -orient horizontal]
	$tree style elements $S [list e_rect e_text_sel]
	$tree style layout $S e_text_sel -padx {2 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky w
	}
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2
  
	set S [$tree style create text_r -orient horizontal]
	$tree style elements $S [list e_rect e_text_sel]
	$tree style layout $S e_text_sel -padx {2 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky e
	}
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2
      
	set S [$tree style create image -orient horizontal]
	$tree style elements $S [list e_rect e_image e_text_sel e_hidden]
	$tree style layout $S e_image -expand ns
	$tree style layout $S e_text_sel -padx 0 -squeeze x -expand ns
	$tree style layout $S e_rect -union [list e_image] -iexpand nswe -ipadx 2
	$tree style layout $S e_hidden -padx 0 -squeeze x -expand nswe

	set S [$tree style create window -orient horizontal]
	$tree style elements $S [list e_rect]
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style elements $S [list e_rect e_window]
	    $tree style layout $S e_window -iexpand xynsew -sticky nsew -squeeze xy -padx 0 -pady 0
	    $tree style layout $S e_rect -union [list e_window] -iexpand nswe -ipadx 2
	} else {
	    $tree style layout $S e_rect -iexpand nswe -ipadx 2
	}
	
	set S [$tree style create full_disabled -orient horizontal]
	$tree style elements $S [list e_rect_disabled]
	$tree style layout $S e_rect_disabled -iexpand xy -ipadx 2 -ipady 2
	
	ttk::style layout TSizegripWhite [ttk::style layout TSizegrip]
	ttk::style configure TSizegripWhite -background white

	$self createbindings
	$self configurelist $args

	return $win
    }
    method hasfocus {} {
	if { [focus -lastfor $tree] eq $tree } {
	    return 1
	} else {
	    return 0
	}
    }
    method focus {} {
	focus $tree
    }
    method givetreectrl {} {
	return $tree
    }
    method createbindings {} {
	bind TreeCtrl <KeyPress-Left> {
	    #TreeCtrl::SetActiveItem %W [TreeCtrl::LeftRight %W -1]
	    %W collapse [%W index active]
	    if { [%W index "active parent"] != 0 } {
		%W activate "active parent"
		%W selection clear all
		%W selection add active
	    }
	}
	bind TreeCtrl <KeyPress-Right> {
	    #TreeCtrl::SetActiveItem %W [TreeCtrl::LeftRight %W 1]
	    if { [%W item numchildren [%W index active]] } {
		%W expand [%W index active]
	    }
	}
#         bind TreeCtrl <Double-ButtonPress-1> {+
#             set id [%W identify %x %y]
#             if {[lindex $id 0] eq "item"} {
#                 %W toggle [%W index active]
#             }
#         }

	$tree notify bind $vscrollbar <Scroll-y> { %W set %l %u }
	bind $vscrollbar <ButtonPress-1> [list focus $tree]

	$tree notify bind $hscrollbar <Scroll-x> { %W set %l %u }
	bind $hscrollbar <ButtonPress-1> [list focus $tree]

	$tree notify install event Header
	$tree notify install detail Header invoke

	$tree notify install event Drag
	$tree notify install detail Drag begin
	$tree notify install detail Drag end
	$tree notify install detail Drag receive

	$tree notify install event Edit
	$tree notify install detail Edit accept

	#$tree notify install event ActiveItem
   

#         $tree notify bind DontDelete <Selection> [mymethod select]
#         $tree notify bind DontDelete <Selection> {
#             if {%c == 1} {
#                 set selection [%T selection get]
#                 #xmlwidget::DisplayNode %T [lindex $selection 0]
#             }
#         }

	bind $tree <KeyPress> [mymethod search_text %A]
	bind $tree <Return> "[mymethod execute_select_return]; break"
	bind $tree <space> "[mymethod execute_select_return]; break"
	bind $tree <ButtonRelease-1> "[bind FullTreeCtrl <ButtonRelease-1>];
		[mymethod execute_select %x %y] ; break"
	bind $tree <ButtonRelease-3> {
	    foreach "type id" [list "" ""] break
	    foreach "type id" [%W identify %x %y] break
	    set fulltktree [winfo parent %W]
	    focus %W
	    if { $type eq "item" && $id ne "" } { 
		if { ![%W selection includes $id] } {
		    %W selection clear all
		    %W selection add $id
		}
		%W activate $id
	    } else { %W selection clear all }
	    set ch [$fulltktree cget -contextualhandler]
	    if { $ch ne "" } {
		uplevel #0 $ch [list $fulltktree [%W selection get] %X %Y]
	    }
	}
	bind $tree <Double-1> [mymethod execute_select_double1 %x %y]

	bind $tree <F2> {
	    if { [[winfo parent %W] is_special_folder active] } { return }
	    set self [winfo parent %W]
	    $self edit_item active
	}
	bind $tree <Delete> {
	     if { [[winfo parent %W] is_special_folder active] } { return }
	    set ch [[winfo parent %W] cget -deletehandler]
	    if { $ch ne "" } {
		set ids [%W selection get]
		uplevel #0 $ch [list [winfo parent %W] $ids]
	    }
	}
	bind $tree <FocusIn> {
	    if { [llength [%W selection get]] == 0 && [%W index last] != 0 } {
		catch {
		    %W selection add "root firstchild visible"
		    %W activate "root firstchild visible"
		}
	    }
	}
	bind $tree <Control-c> [mymethod clipboard_copy]

	$tree notify bind $tree <Edit-accept> { 
	    set self [winfo parent %W]
	    $self end_edit_item [%T index %I] %C %t
	}
	$tree notify bind $tree <Drag-receive> {
	    set self [winfo parent %W]
	    $self end_drop %I %l %x %y
	}
	$tree notify bind $tree <ActiveItem> {
	    set self [winfo parent %W]
	    after idle [list $self active_item_changed %c]
	}

	set sortcolumn ""
	$tree notify bind $tree <Header-invoke> [mymethod header_invoke %C]


#         bind $tree <Delete> {
#             set id [%W selection get]
#             if { $id == "" } { return }
#             #xmlwidget::AddxmlElement %W $id "" delete
#         }
	#bind $tree <<Cut>> [list xmlwidget::CutOrCopy $tree cut]
	#bind $tree <<Copy>> [list xmlwidget::CutOrCopy $tree copy]

    }
    onconfigure -compass_background {value} {
	set options(-compass_background) $value

	if { [package vcompare [package present treectrl] 2.1] < 0 } { return }

	if { $value } {
	    if { [lsearch -exact [image names] fulltktree::compass_background] == -1 } {
		image create photo fulltktree::compass_background -file \
		    [file join $::fulltktree_top::topdir compas_logo_small.gif]
	    }
	    $tree configure -backgroundimage fulltktree::compass_background
	} else {
	    $tree configure -backgroundimage ""
	}
    }
    onconfigure -selectmode {value} {
	set options(-selectmode) $value
	$tree configure -selectmode $value

#         if { $value eq "single" || $value eq "browse" } {
#             if { [package vcompare [package present treectrl] 2.1] >= 0 } {
#                 $tree style layout item e_text_sel -iexpand nsx
#             } else {
#                 $tree style layout item e_text_sel -iexpand ns
#             }
#             if { [package vcompare [package present treectrl] 2.1] >= 0 } {
#                 $tree style layout folder e_text_sel -iexpand nsx
#             } else {
#                 $tree style layout folder e_text_sel -iexpand ns
#             }
#             if { [package vcompare [package present treectrl] 2.1] >= 0 } {
#                 $tree style layout text e_text_sel -iexpand nsx
#             } else {
#                 $tree style layout text e_text_sel -iexpand ns
#             }
#         } else {
#             $tree style layout item e_text_sel -iexpand ns
#             $tree style layout folder e_text_sel -iexpand ns
#             $tree style layout text e_text_sel -iexpand ns
#         }
	$self _apply_columns_values
    }
    onconfigure -expand {value} {
	set options(-expand) $value

	if { $options(-expand) } {
	    grid remove $win.sh
	} else {
	    grid $win.sh
	}
	
	set w0 [font measure [$self cget -font] 0]
	set idx 0
	foreach col $options(-columns) {
	    foreach "width name justify type is_editable" $col break
	    if { $options(-expand) } {
		$tree column configure c$idx -minwidth [expr {$w0*$width}] \
		    -width "" -expand 1 -squeeze 1
	    } else {
		$tree column configure c$idx -width [expr {$w0*$width}] \
		    -expand 0 -squeeze 0
	    }
	    incr idx
	}
    }
    onconfigure -columns {value} {
	set options(-columns) $value

	set err [catch { $tree column delete all }]
	if { $err } {
	    # old version
	    set n [$tree numcolumns]
	    for { set i 0 } { $i < $n } { incr i } {
		$tree column delete 0
	    }
	}
	set w0 [font measure [$self cget -font] 0]
	set itemStyle ""
	set idx 0
	foreach col $options(-columns) {
	    foreach "width name justify type" $col break
	    $tree column create -text $name -width [expr {$w0*$width}] \
		-tag c$idx -justify $justify
	    if { $options(-expand) } {
		$tree column configure c$idx -minwidth [expr {$w0*$width}] \
		    -width "" -expand 1 -squeeze 1
	    }
	    if { $type eq "text" && $justify eq "right" } {
		set type text_r
	    }
	    lappend itemStyle $idx $type
	    incr idx
	}
	bindtags $tree [list $tree FullTreeCtrl TreeCtrl [winfo toplevel $tree] all]
	$tree configure -treecolumn 0

	$self _apply_columns_values
    }
    onconfigure -editaccepthandler {value} {
	set options(-editaccepthandler) $value
	$self _apply_columns_values
    }
    onconfigure -draghandler {value} {
	set options(-draghandler) $value
	$self _apply_columns_values
    }
    onconfigure -sensitive_cols {value} {
	set options(-sensitive_cols) $value
	$self _apply_columns_values
    }
    onconfigure -item_image {value} {
	set options(-item_image) $value
	$tree element configure e_item_image -image $value
    }
    onconfigure -folder_image {value} {
	set options(-folder_image) $value
	$tree element configure e_folder_image -image $value
    }
    onconfigure -has_sizegrip {value} {
	set options(-has_sizegrip) $value
	
	ttk::sizegrip $win.grip -style TSizegripWhite
	place $win.grip -relx 1 -rely 1 -anchor se
	
	bind $vscrollbar <Map> +[list after 100 [list raise $win.grip]]
	bind $win.grip <ButtonPress-1> +[mymethod _move_sizegrip start]
	bind $win.grip <ButtonRelease-1> +[mymethod _move_sizegrip end]
    }
    method _move_sizegrip { what } {
	switch $what {
	    start {
		$tree notify bind $vscrollbar <Scroll-y> ""
	    }
	    end {
		$tree notify bind $vscrollbar <Scroll-y> { %W set %l %u }
		if { [lindex [$tree yview] 0] != 0 || [lindex [$tree yview] 1] != 1 } {
		    eval [list $vscrollbar set] [$tree yview]
		}
	    }
	}
    }
    method selection { what args } {
	if { $what eq "set" } { set what add }
	return [eval [list $self _selection $what] $args]
    }
    method _apply_columns_values {} {

	set idx 0
	foreach "dragimage editable sensitive" [list "" "" ""] break
	set sensitive_off 0
	foreach col $options(-columns) {
	    if { $options(-sensitive_cols) ne "" } {
		if { $options(-sensitive_cols) eq "all" || \
		    [lsearch -integer $options(-sensitive_cols) $idx] != -1 } {
		    set sensitive_off 0
		} else {
		    set sensitive_off 1
		}
	    }
	    foreach "width name justify type is_editable" $col break
	    if { $is_editable } { lappend editcolumns $idx }
	    switch $type {
		item {
		    if { $is_editable } {
		        lappend editable [list $idx $type e_text_sel]
		        lappend editable [list $idx folder e_text_sel]
		    }
		    if { !$sensitive_off } {
		        lappend sensitive [list $idx $type e_item_image e_text_sel \
		                e_selmarker_up e_selmarker_down]
		        lappend sensitive [list $idx folder e_folder_image e_text_sel \
		                e_selmarker_up e_selmarker_down]
		        if { $options(-selectmode) ne "single" && $options(-selectmode) ne "browse" } {
		            set sensitive_off 1
		        }
		    }
		    if { $options(-draghandler) ne "" } {
		        lappend dragimage [list $idx $type e_item_image e_text_sel]
		        lappend dragimage [list $idx folder e_folder_image e_text_sel]
		    }
		}
		imagetext {
		    if { $is_editable } {
		        lappend editable [list $idx $type e_text_sel]
		    }
		    if { !$sensitive_off } {
		        lappend sensitive [list $idx $type e_image e_text_sel]
		        if { $options(-selectmode) ne "single" && $options(-selectmode) ne "browse" } {
		            set sensitive_off 1
		        }
		    }
		    if { $options(-draghandler) ne "" } {
		        lappend dragimage [list $idx $type e_image e_text_sel]
		    }
		}
		text {
		    if { $is_editable } {
		        lappend editable [list $idx $type e_text_sel]
		    }
		    if { !$sensitive_off } {
		        lappend sensitive [list $idx $type e_text_sel]
		        if { $options(-selectmode) ne "single" && $options(-selectmode) ne "browse" } {
		            set sensitive_off 1
		        }
		    }
		    if { $options(-draghandler) ne "" } {
		        lappend dragimage [list $idx $type e_text_sel]
		    }
		}
		image {
		    if { !$sensitive_off } {
		        lappend sensitive [list $idx $type e_text_sel]
		    }
		    if { $options(-draghandler) ne "" } {
		        lappend dragimage [list $idx $type e_image]
		    }
		}
	    }
	    incr idx
	}
	TreeCtrl::SetEditable $tree $editable
	TreeCtrl::SetDragImage $tree $dragimage
	TreeCtrl::SetSensitive $tree $sensitive
    }
   method insert { index list { parent_or_sibling root } } {

	set item [$tree item create]
	eval [list $tree item style set $item] $itemStyle

	set desc ""
	set idx 0
	foreach col $options(-columns) {
	    foreach "- - justify type -" $col break

	    switch $type {
		imagetext {
		    foreach "img txt" [lindex $list $idx] break
		    # dirty trick to avoid a crash in tktreectrl
		    if { $txt eq "" } { set txt " " }
		    set selimg ${img}::selected
		    if { $img ne "" } {
		        if { [lsearch [image names] $selimg] == -1 } {
		            image create photo $selimg
		            $selimg copy $img
		            imagetint $selimg $fulltktree::SystemHighlight 128
		        }
		    } else { set selimg "" }

		    lappend desc [list [list e_image -image \
		                [list $selimg {selected} $img {}]] \
		            [list e_text_sel -text $txt -justify $justify]]
		}
		text -  item {
		    set txt [lindex $list $idx]
		    if { $txt eq "" } { set txt " " }
		    lappend desc [list [list e_text_sel -text $txt -justify $justify]]
		}
		image {
		    foreach "img txt" [lindex $list $idx] break
		    if { $txt eq "" } { set txt " " }
		    lappend desc [list [list e_image -image $img] \
		            [list e_hidden -data $txt]]
		}
	    }
	    incr idx
	}        
	eval [list $tree item complex $item] $desc
	switch $index {
	    end - child {
		$tree item lastchild $parent_or_sibling $item
	    }
	    prev {
		$tree item prevsibling $parent_or_sibling $item
	    }
	    next {
		$tree item nextsibling $parent_or_sibling $item
	    }
	    default {
		error "error: index can be: end (or child), prev or next"
	    }
	}
	if { ($index eq "end" || $index eq "child") \
	    && [$tree index $parent_or_sibling] != 0 } {
	    if { [set ipos [lsearch -exact $itemStyle item]] != -1 } {
		set col [lindex $itemStyle [expr {$ipos-1}]]
		$tree item style map $parent_or_sibling $col folder \
		    [list e_text_sel e_text_sel]
	    }
	    $tree item configure $parent_or_sibling -button 1
	}
	#set ::TreeCtrl::Priv(DirCnt,$tree) $item
	set ::TreeCtrl::Priv(DirCnt,$tree) end

	return $item
    }
    method convert_to_folder { item } {
	set idx 0
	foreach col $options(-columns) {
	    set type [lindex $col 3]
	    if { $type eq "item" } { break }
	    incr idx
	}        
	$tree item style map $item $idx folder [list e_text_sel e_text_sel]
	$tree item collapse $item
	if { [llength [$tree item children $item]] } {
	    $tree item configure $item -button 1
	}
    }
    method is_folder { item } {
	set idx 0
	foreach col $options(-columns) {
	    set type [lindex $col 3]
	    if { $type eq "item" } { break }
	    incr idx
	}        
	if { $idx < [llength $options(-columns)] } {
	    if { [$tree item style set $item $idx] eq "folder" } {
		return 1
	    }
	}
	return 0
    }
    method sort_column { args } {
	set optional {}
	parse_args -compulsory_min 0 "" "column order" $args
	if { $column eq "" } {
	    if { $sortcolumn eq "" } {
		set order ""
	    } else {
		if {[$tree column cget $sortcolumn -arrow] eq "down"} {
		    set order decreasing
		} else {
		    set order increasing
		}
	    }
	    return [list $sortcolumn $order]
	} else {
	    if { $sortcolumn ne "" } {
		$tree column configure $sortcolumn -arrow none -itembackground {}
	    }
	    set sortcolumn $column
	    switch $order {
		increasing { set arrow up }
		decreasing { set arrow down }
	    }
	    $tree column configure $sortcolumn -arrow $arrow -itembackground #F7F7F7
	    
	    set type [lindex $options(-columns) [expr {$sortcolumn*4+3}]]
	    if { $type eq "image" } {
		$self item sort root -$order -column $sortcolumn \
		    -element e_hidden -dictionary
	    } else {
		$self item sort root -$order -column $sortcolumn -dictionary
	    }
	}
    }
    method header_invoke { col } {

	if { $sortcolumn eq "" } {
	    set are_equal 0
	} elseif { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    set are_equal [$tree column compare $col == $sortcolumn]
	} else {
	    set are_equal [expr {$col == $sortcolumn}]
	}
	if { $sortcolumn eq "" } {
	    set order -increasing
	    set arrow up
	    set sortcolumn $col
	} elseif { $are_equal } {
	    if {[$tree column cget $sortcolumn -arrow] eq "down"} {
		set order -increasing
		set arrow up
	    } else {
		set order -decreasing
		set arrow down
	    }
	} else {
	    if {[$tree column cget $sortcolumn -arrow] eq "down"} {
		set order -decreasing
		set arrow down
	    } else {
		set order -increasing
		set arrow up
	    }
	    $tree column configure $sortcolumn -arrow none -itembackground {}
	    set sortcolumn $col
	}
	$tree column configure $col -arrow $arrow -itembackground #F7F7F7

	set type [lindex $options(-columns) [expr {$col*4+3}]]
	 if { $type eq "image" } {
	    $self item sort root $order -column $col -element e_hidden -dictionary
	} else {
	    $self item sort root $order -column $col -dictionary
	}
    }
    method search_text { char } {

	if { [$tree index last] == 0 } { return }
	if { $char eq "\t" } { return }
	if { [string is wordchar -strict $char] || [string is punct -strict $char] \
		 || [string is space -strict $char] } {
	    if { ![info exists searchstring] || [string index $searchstring end] != $char } {
		append searchstring $char
	    }
	    if { [info exists searchstring_reached_end] && $searchstring_reached_end ne "" \
		&& $searchstring_reached_end eq $searchstring } {
		set id "first visible"
	    } elseif { [$tree compare active == "active bottom"] } {
		set id "first visible"
	    } else { set id "active below" }
	    set err [catch { $tree index $id } id]
	    if { $err } { return }
	    set found 0
	    while { $id != "" } {
		set txt [$tree item text $id 0]
		if { [string match -nocase $searchstring* $txt] } {
		    set found 1
		    break
		}
		set id [$tree index "$id below"]
	    }
	    if { !$found } {
		bell
		set searchstring_reached_end $searchstring
		set searchstring ""
		after 300 [list set [varname searchstring_reached_end] ""]
	    } else {
		$tree activate $id
		$tree see $id
		$tree selection clear all
		$tree selection add active
		after 300 [list set [varname searchstring] ""]
	    }
	}
    }
    method activate_select_item { id } {
	$tree activate $id
	$tree selection clear all
	$tree selection add $id
	foreach i [$tree item ancestors $id] {
	    $tree expand $i
	}
	$tree see $id
    }
    method activate_select { name_path { col "" } } {

	if { $col eq "" } {
	    if { [package vcompare [package present treectrl] 2.1] >= 0 } {
		set range [range 0 [$tree column count]]
	    } else {
		set range [range 0 [$tree numcolumns]]
	    }
	} else { set range $col }
	foreach col $range {
	    foreach el [$tree item style elements 1 $col] {
		if { $el eq "e_text_sel" } { break }
	    }
	}
	set parent 0
	foreach i $name_path {
	    set found 0
	    foreach id [$tree item children $parent] {
		if { [$tree item text $id $col] eq $i } {
		    if { $i ne [lindex $name_path end] } {
		        $tree expand $id
		    }
		    set parent $id
		    set found 1
		    break
		}
	    }
	    if { !$found } {
		error "error: '$name_path' not found in tree"
	    }
	}
	$tree activate $id
	$tree selection clear all
	$tree selection add $id
    }
    method _correct_xview { item col } {
	$tree see $item
	foreach "x0 - x1 -" [$tree item bbox $item $col] break
	if { $x0 < 0 || $x1 > [winfo width $tree] } {
	    set x0c [$tree canvasx $x0]
	    set tw [expr {[winfo width $tree]/([lindex [$tree xview] 1]-[lindex [$tree xview] 0])}]
	    $tree xview moveto [expr {$x0c/$tw}]
	    update
	}
    }
    method edit_item { item { col "" } } {
	if { $options(-editaccepthandler) eq "" } { return 0 }

	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    set nc [$tree column count]
	} else {
	    set nc [$tree numcolumns]
	}
	if { $col eq "" } {
	    set range [range 0 $nc]
	} elseif { [string match next* $col] } {
	    if { [lindex $col 1] ne "" } {
		set range [range [expr {[lindex $col 1]+1}] $nc]
	    } else {
		set range [range 0 $nc]
	    }
	} elseif { [string match prev* $col] } {
	    if { [lindex $col 1] ne "" } {
		set range [rangeF [expr {[lindex $col 1]-1}] 0 -1]
	    } else {
		set range [rangeF [expr {$nc-1}] 0 -1]
	    }
	} else { set range $col }

	foreach icol $range {
	    if { [lsearch -integer $editcolumns $icol] == -1 } { continue }
	    foreach el [$tree item style elements $item $icol] {
		if { $el eq "e_text_sel" } {
		    if { $options(-editbeginhandler) ne "" } {
		        set ret [uplevel #0 $options(-editbeginhandler) \
		                [list $tree $item $icol]]
		        if { $ret == 0 } { break }
		    } else { set ret 1 }
		    set w ""
		    switch [lindex $ret 0] {
		        1 - entry {
		            $self _correct_xview $item $icol
		            set w [::TreeCtrl::EntryOpen $tree $item $icol e_text_sel]
		        }
		        combo {
		            $self _correct_xview $item $icol
		            foreach "editable values dict" [list "" "" ""] break
		            foreach "editable values dict" [lrange $ret 1 end] break
		            set w [::TreeCtrl::ComboOpen $tree $item $icol e_text_sel \
		                    $editable $values -NONE- $dict]
		        }
		        combotree {
		            $self _correct_xview $item $icol
		            foreach "editable values_tree" [lrange $ret 1 end] break
		            set w [::TreeCtrl::ComboTreeOpen $tree $item $icol e_text_sel \
		                    $editable $values_tree]
		        }
		    }
		    if { $w ne "" } {
		        bind $w <KeyPress-Tab> "[mymethod edit_item_tab $item $w $icol next];break"
		        bind $w <<PrevWindow>> "[mymethod edit_item_tab $item $w $icol prev];break"
		    }
		    return 1
		}
	    }
	}
	if { [string match next* $col] } {
	    set item [$tree item id "$item below"]
	    if { $item ne "" } {
		$self activate $item
		return [$self edit_item $item "next"]
	    }
	}
	if { [string match prev* $col] } {
	    set item [$tree item id "$item above"]
	    if { $item ne "" } {
		return [$self edit_item $item "prev"]
	    }
	}
	return 0
    }
    method edit_item_tab { item w icol where } {
	if { [winfo class $w] eq "TEntry" || [winfo class $w] eq "Entry" } {
	    TreeCtrl::EntryClose $tree 1
	} else {
	    TreeCtrl::ComboClose $tree 1
	}
	#catch { focus $TreeCtrl::Priv(entry,$tree,focus) }
	$self edit_item $item [list $where $icol]
    }
    method end_edit_item { item col text } {
	if { $options(-editaccepthandler) eq "" } { return }
	if { [lsearch -integer $editcolumns $col] == -1 } { return }
	uplevel #0 $options(-editaccepthandler) [list $tree $item $col $text]
    }
    method end_drop { recieving_item dragged_items x y } {
	if { $options(-draghandler) ne "" } {
	    foreach "- y1 - y2" [$tree item bbox $recieving_item] break
	    if { $y < [expr {.667*$y1+.333*$y2}] } {
		set where prev
	    } elseif { $y < [expr {.333*$y1+.667*$y2}] } {
		set where center
	    } else { set where next }
	    uplevel #0 $options(-draghandler) [list $tree \
		    $recieving_item $dragged_items $where]
	}
    }
    variable execute_select_pressed ""
    method execute_select { { x "" } { y "" } } {
	set ids [$tree selection get]
	set id [lindex $ids 0]

	if { $id eq "" } { return }

	set identify ""
	if { [llength $ids] == 1 } {
	    if { $x ne "" } {
		set identify [$tree identify $x $y]
		if { [lindex $identify 0] ne "item" || [lindex $identify 2] ne "column" } {
		    return
		}
	    }
	    if { $identify ne "" && $execute_select_pressed == $id } {
		set execute_select_pressed ""
		set ch [$self cget -editaccepthandler]
		if { $ch ne "" } {
		    set ret [$self edit_item active [dict get $identify column]]
		    if { $ret } { return }
		}
	    } elseif { ![$self is_special_folder $id] } {
		set execute_select_pressed $id
		after 900 [list catch [list set [varname execute_select_pressed] ""]]
	    }
	    
	    if { $options(-buttonpress_open_close) } {
		set isopen [$tree item state get $id open]
		if { $isopen } {
		    $tree collapse -recurse $id
		} elseif { [$tree item numchildren $id] } {
		    $tree collapse all
		    foreach i [$tree item ancestors $id] { $tree expand $i }
		    if { [$tree item numchildren $id] } {
		        $tree expand -recurse $id
		    }
		}
	    }
	}
	if { 0&& $options(-selecthandler2) ne "" } {
	    if { $identify eq "" } {
		uplevel #0 $options(-selecthandler2) [list $tree $ids]
	    }
	} elseif { $options(-selecthandler) ne "" && $options(-selecthandler2) eq "" } {
	    uplevel #0 $options(-selecthandler) [list $tree $ids]
	}
    }
    method active_item_changed { item } {
	set ids [$tree selection get]
	if { $ids eq "" } {
	    set ids $item
	}
	if { $options(-selecthandler) ne "" && $options(-selecthandler2) ne "" } {
	    uplevel #0 $options(-selecthandler) [list $tree $ids]
	}
    }
    method execute_select_return {} {
	set ids [$tree selection get]

	if { $options(-returnhandler) ne "" } {
	    uplevel #0 $options(-returnhandler) [list $tree $ids]
	} elseif { $options(-selecthandler2) ne "" } {
	    uplevel #0 $options(-selecthandler2) [list $tree $ids]
	} elseif { $options(-selecthandler) ne "" } {
	    uplevel #0 $options(-selecthandler) [list $tree $ids]
	}
    }
    method execute_select_double1 { x y } {
	set id [$tree identify $x $y]
	if { [lindex $id 0] ne "item" || [lindex $id 2] ne "column"} { return }
	set ids [$tree selection get]
	set id0 [lindex $ids 0]
	if { $id0 eq "" } { return }
	
	set ch [$self cget -editaccepthandler]
	
	if { $options(-selecthandler2) ne "" } {
	    set execute_select_pressed ""
	    after idle $options(-selecthandler2) [list $tree $ids]
	} elseif { $ch ne "" } {
	    set ret [$self edit_item $id0 [dict get $id column]]
	    if { $ret } { return }
	}
    }
    method give_item_path_text { name_path { col "" } } {

	if { $col eq "" } {
	    if { [package vcompare [package present treectrl] 2.1] >= 0 } {
		set range [range 0 [$tree column count]]
	    } else {
		set range [range 0 [$tree numcolumns]]
	    }
	} else { set range $col }
	foreach col $range {
	    foreach el [$tree item style elements 1 $col] {
		if { $el eq "e_text_sel" } { break }
	    }
	}
	set parent 0
	foreach i $name_path {
	    set found 0
	    foreach id [$tree item children $parent] {
		if { [$tree item text $id $col] eq $i } {
		    set parent $id
		    set found 1
		    break
		}
	    }
	    if { !$found } { return "" }
	}
	return $id
    }
    method item_path_text { id { col "" } } {

	if { $col eq "" } {
	    if { [package vcompare [package present treectrl] 2.1] >= 0 } {
		set range [range 0 [$tree column count]]
	    } else {
		set range [range 0 [$tree numcolumns]]
	    }
	} else { set range $col }
	foreach col $range {
	    foreach el [$tree item style elements $id $col] {
		if { $el eq "e_text_sel" } { break }
	    }
	}

	set path ""
	while { $id != 0 } {
	    set txt [$tree item text $id $col]
	    set path [linsert $path 0 $txt]
	    set id [$tree item parent $id]
	}
	return $path
    }
#     method set_special_folder { id } {
#         set name [$tree item text $id 0]
#         set description [$tree item text $id 1]
#         $tree item configure $id -button yes
#         $tree item style set $id 0 s1-magenta 1 s3
#         $tree item complex $id \
#             [list [list e3 -text $name]] \
#             [list [list e6 -text $description]]
#     }
#     method get_special_folder {} {
#         foreach id [$tree item children 0] {
#             if {[$tree item style set $id 0] eq "s1-magenta" } {
#                 return $id
#             }
#         }
#         return ""
#     }
     method is_special_folder { id } {
	return 0
#         if {[$tree item style set $id 0] eq "s1-magenta" } {
#             return 1
#         }
#         return 0
    }
    method exists_uservar { key } {
	return [info exists uservar($key)]
    }
    method give_uservar { args } {
	switch -- [llength $args] {
	    1 {
		#nothing
	    }
	    2 {
		set  uservar([lindex $args 0]) [lindex $args 1]
	    }
	    default {
		error "error in give_uservar"
	    }
	}
	return [varname uservar([lindex $args 0])]
    }
    method set_uservar_value { key newvalue } {
	set uservar($key) $newvalue
    }
    method give_uservar_value { args } {
	set key [lindex $args 0]
	switch -- [llength $args] {
	    1 {
		return $uservar($key)
	    }
	    2 {
		if { [info exists uservar($key)] } {
		    return $uservar($key)
		} else {
		    return [lindex $args 1]
		}
	    }
	    default {
		error "error in give_uservar_value"
	    }
	}
    }
    typemethod exists_uservar { key } {
	return [info exists type_uservar($key)]
    }
    typemethod give_uservar { args } {
	switch -- [llength $args] {
	    1 {
		#nothing
	    }
	    2 {
		set  type_uservar([lindex $args 0]) [lindex $args 1]
	    }
	    default {
		error "error in give_uservar"
	    }
	}
	return [typevarname type_uservar([lindex $args 0])]
    }
    typemethod set_uservar_value { key newvalue } {
	set type_uservar($key) $newvalue
    }
    typemethod give_uservar_value { args } {
	set key [lindex $args 0]
	switch -- [llength $args] {
	    1 {
		return $type_uservar($key)
	    }
	    2 {
		if { [info exists type_uservar($key)] } {
		    return $type_uservar($key)
		} else {
		    return [lindex $args 1]
		}
	    }
	    default {
		error "error in give_uservar_value"
	    }
	}
    }
    method createimages_colors {} {

	set w [listbox .listbox]
	set fulltktree::SystemHighlight [$w cget -selectbackground]
	set fulltktree::SystemHighlightText [$w cget -selectforeground]
	set fulltktree::SystemFont [$w cget -font]
	destroy $w

	if { [lsearch -exact [image names] mac-collapse] != -1 } { return }

	image create photo mac-collapse  -data {
	    R0lGODlhEAAQALIAAAAAAAAAMwAAZgAAmQAAzAAA/wAzAAAzMyH5BAUAAAYA
	    LAAAAAAQABAAggAAAGZmzIiIiLu7u5mZ/8zM/////wAAAAMlaLrc/jDKSRm4
	    OAMHiv8EIAwcYRKBSD6AmY4S8K4xXNFVru9SAgAh/oBUaGlzIGFuaW1hdGVk
	    IEdJRiBmaWxlIHdhcyBjb25zdHJ1Y3RlZCB1c2luZyBVbGVhZCBHSUYgQW5p
	    bWF0b3IgTGl0ZSwgdmlzaXQgdXMgYXQgaHR0cDovL3d3dy51bGVhZC5jb20g
	    dG8gZmluZCBvdXQgbW9yZS4BVVNTUENNVAAh/wtQSUFOWUdJRjIuMAdJbWFn
	    ZQEBADs=
	}
	image create photo mac-expand -data {
	    R0lGODlhEAAQALIAAAAAAAAAMwAAZgAAmQAAzAAA/wAzAAAzMyH5BAUAAAYA
	    LAAAAAAQABAAggAAAGZmzIiIiLu7u5mZ/8zM/////wAAAAMnaLrc/lCB6MCk
	    C5SLNeGR93UFQQRgVaLCEBasG35tB9Qdjhny7vsJACH+gFRoaXMgYW5pbWF0
	    ZWQgR0lGIGZpbGUgd2FzIGNvbnN0cnVjdGVkIHVzaW5nIFVsZWFkIEdJRiBB
	    bmltYXRvciBMaXRlLCB2aXNpdCB1cyBhdCBodHRwOi8vd3d3LnVsZWFkLmNv
	    bSB0byBmaW5kIG91dCBtb3JlLgFVU1NQQ01UACH/C1BJQU5ZR0lGMi4wB0lt
	    YWdlAQEAOw==
	}
	image create photo appbook16 -data {
	    R0lGODlhEAAQAIQAAPwCBAQCBDyKhDSChGSinFSWlEySjCx+fHSqrGSipESO
	    jCR6dKTGxISytIy6vFSalBxydAQeHHyurAxubARmZCR+fBx2dDyKjPz+/MzK
	    zLTS1IyOjAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAVkICCOZGmK
	    QXCWqTCoa0oUxnDAZIrsSaEMCxwgwGggHI3E47eA4AKRogQxcy0mFFhgEW3M
	    CoOKBZsdUrhFxSUMyT7P3bAlhcnk4BoHvb4RBuABGHwpJn+BGX1CLAGJKzmK
	    jpF+IQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0K
	    qSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpo
	    dHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
	}

	image create photo folder-open -data {
	    R0lGODlhEAANANIAAAAAAISEhMbGxv//AP////8AAAAAAAAAACH5BAkZAAUA
	    LAAAAAAQAA0AAAM8WBrM+rAEQmmIb4qxBWnNQnCkV32ARHRlGQBgDA7vdN6v
	    UK8tC78qlrCWmvRKsJTquHkpZTKAsiCtWq0JAAA7
	}
	image create photo folder-closed -data {
	    R0lGODlhDwANANIAAAAAAISEhMbGxv//AP////8AAAAAAAAAACH5BAkZAAUA
	    LAAAAAAPAA0AAAMzWBXM+jCIMUWAT9JtmwtEKI5hAIBcOplgpVIs8bqxa8On
	    fNP5zsWzDctD9AAKgKRyuUwAAAA7
	} -format GIF
	image create photo folder-magenta-open -data {
	    R0lGODlhEAANAMQAANnZ2YSEhP77/vno/P33/sSkytt/esSSzcOAz9l2gtp6ftyDdcSjyt6I
	    cYWChYp5jYaAh49vlf///8W1yPz2/vLS+P//////////////////////////////////////
	    /yH5BAEAAAAALAAAAAAQAA0AAAWNICCCgSiCgDiSgTAQgxCQZCAUxmEUAhiIIwAEg4EkiFIQ
	    AzEIAQAEQmEcxrEwYMMshQMEQeA80AM90ANFywMEkjAQAzGAxEA8wOQAgTQth3EYh3EsQBAA
	    QEAdCQImSIIkBwQEABAMBpIgCZIgSgEEAAA4FWgcxmEcxrE8AAAAgfNAD/RAD/Q4ICCOZDmG
	    ADs=
	}
	image create photo folder-magenta-closed -data {
	    R0lGODlhDwANAMQAANnZ2YWChYp5jYaAh8Skytt/esSSzYSEhMW1yNyDdfz2/vLR+Prt/Pvy
	    /fno/P33/tl2gsOAz///////////////////////////////////////////////////////
	    /yH5BAEAAAAALAAAAAAPAA0AAAWKIAAEwiAEoiiKQEAURkEEoigCB5IYhZGACHiII3AoC7Mw
	    S+M8zuMEwOEkRmEURmEUoJEIwKEYUARFUARFkDEAh1NEUARFUASBUSEAh2JAERRBERRBxgAc
	    ThFBERRBEQRGhQAcigFFUARFUAQZA3A4iVEYhVEYBWgkAnAEwiAMwiAMwiAEICCOZAmEADs=
	}
    }
    method clipboard_copy {} {
	set cols [$self cget -columns]
	set dataList ""
	set lineList ""
	foreach i $cols {
	    lappend lineList [lindex $i 1]
	}
	lappend dataList [join $lineList \t]
	set ncols [llength [$self cget -columns]]

	set items [$tree selection get]
	if { ![llength $items] } {
	    set items [$tree item children 0]
	}
	foreach item $items {
	    set lineList ""
	    for { set col 0 } { $col < $ncols } { incr col } {
		lappend lineList [$tree item text $item $col]
	    }
	    lappend dataList [join $lineList \t]
	}
	clipboard clear
	clipboard append [join $dataList \n]
    }
}

proc range {from to {step 1}} {
    set res ""
    if { $step > 0 } {
	for { set i $from } { $i < $to } { set i [expr {$i+$step}] } {
	    lappend res $i
	}
    } else {
	for { set i $from } { $i > $to } { set i [expr {$i+$step}] } {
	    lappend res $i
	}
    }
    return $res
}

proc rangeF {from to {step 1}} {
    set res ""
    if { $step > 0 } {
	for { set i $from } { $i <= $to } { set i [expr {$i+$step}] } {
	    lappend res $i
	}
    } else {
	for { set i $from } { $i >= $to } { set i [expr {$i+$step}] } {
	    lappend res $i
	}
    }
    return $res
}






