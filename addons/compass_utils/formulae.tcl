
if { [info command dict] eq "" } {
    package require dict
}
package require tdom
package require msgcat
package require snit
package require math::constants
package require compass_utils

if { 0 } {
    package require xml2pdf
    package require tooltip
}

################################################################################
#
#  INTERNAL PROCS
#
# process_window_container, query_manager, ask_for_period
#
################################################################################

################################################################################
#
# Entry points:
#
# formulae::create_windowD ?...? parent window root
#       returns window
#
#  formulae::mark_window_as_finished $key
#
# set ndoc [formulae::give_updated_tree $key]
#
# set ndoc [formulae::create_reportD -lognoter_db db -page page $key root]
#
# set dict [formulae::give_params_dict $key]
#
# formulae::calc_and_update_tree -lognoter_db db -page page doc values_dict
#
# formulae::reset_has_changes $key
#
# set has_changes [formulae::has_changes_in_values $key]
#
# formulae::fill_database_values_from_internal_do $db_default $root $lognoter_db
#
# formulae::add_database_values_to_xml $root $lognoter_db
#
# formulae::give_last_modification_date $formulae_table $lognoter_db
#
# set need_page_update [formulae::copy_to_another_lognoter $root $page_to $wdb_from $wdb_to
#
# set ret [formulae::select_one_row $wp $txt $table $lognoter_db
#
# set table [formulae::give_database_name $root $page $lognoter_db]
#
# formulae::update_database_name -keep_old_database 0|1 $db_default $root $lognoter_db
#
# set ret [formulae::delete_database -parent $w -require_confirm_for_default 1 -page $page \
#       $root $lognoter_db]
#
# formulae::check_connect_ramdebugger $key
#
# formulae::search_in_database $key $searchstring
#
#  formulae::give_interp $key
#

proc isdouble { txt } {
    # this is to avoid an error in TCL 8.5a4 with 'na'
    if { [string index $txt 0] eq "n" } { return }
    return [string is double -strict $txt]
}

proc isdouble_or_void { txt } {
    # this is to avoid an error in TCL 8.5a4 with 'na'
    if { [string index $txt 0] eq "n" } { return }
    return [string is double $txt]
}

proc setAttributes { domNode xpath attList } {
    set inum 0
    foreach node [$domNode selectNodes $xpath] {
	foreach "name value" $attList {
	    $node setAttribute $name $value
	}
	incr inum
    }
    return $inum    
}

namespace eval formulae {
    variable values ""
    variable values_default ""
    variable uvalues
    variable current_set
    variable unique_id 0
    variable images ""  
}

proc formulae::unique_key {} {
    variable unique_id
    
    return "formulae::unique_key[incr unique_id]"
}
################################################################################
#    General utilities
################################################################################

proc formulae::xml { args } {
    set map [list > "&gt;" < "&lt;" & "&amp;" \" "&quot;" ' "&apos;"]
    return [string map $map [join $args ""]]
}

proc formulae::comma_field_to_list { field } {

    set map [list "&comma;" "," "&#44;" "," "&amp;" "&"]
   return [string map $map [split $field ","]]
}

proc formulae::list_to_comma_field { list } {
    
    set map [list "," "&comma;" "&" "&amp;"]
   return [join [string map $map $list] ","]
}

proc formulae::subst_units { doc x } {

    regexp {\$\{([^\}]*)\}\[(.*)\]} $x {} var unit_to
    regsub -all {\$} $unit_to {} unit_to
    set root [$doc documentElement]
    set node [$root selectNodes //param\[@n='$var'\]]
    set unit_from [$node @units]

    set degtorad [::math::constants::give_constant degtorad]
    set pi [::math::constants::give_constant pi]

    set units [list \
	    m 1.0 cm 1e-2 mm 1e-3 \
	    rad 1.0 deg $degtorad \
	    N 1.0 kN 1.0e3 \
	    N/m\u0b2 1.0 N/mm\u0b2 1.0e6 kN/m\u0b2 1.0e+3 \
	    W 1.0 MW 1.0e6 \
	    rad/s 1.0 rpm [expr {2*$pi/60.0}] \
	    ]

    if { ![dict exists $units $unit_from] } {
	error "error: unit $unit_from is not known"
    }
    if { ![dict exists $units $unit_to] } {
	error "error: unit $unit_to is not known"
    }
    set ex "(\$\{$var\}*[dict get $units $unit_from]/[dict get $units $unit_to])"
    return $ex
}

proc formulae::node_pn { node } {
    set pn [_ [$node @pn ""]]
    if { $pn eq "" } { set pn [$node @n] }
    regsub -all "</*?emphasis.*?>" $pn {} pn
    if { [regexp {<} $pn] } { set pn [$node @n] }
    return $pn
}

proc formulae::node_pn_xml { node } {
    set pn [_ [$node @pn ""]]
    if { $pn eq "" } { set pn [$node @n] }
    return $pn
}

proc formulae::parse_formula { doc formula } {
    
    #set rex {(\m[[:alpha:]][\w!]*(?!\(|\w|[\s\w.]*"|\]))}
    set rex {(\m[[:alpha:]](?:\w*|!\w+)(?!\(|\w|"|\]))}
    append rex {|(\"[^\"]+\")|(\[[^]]+\])}
    
    lassign [list "" 0] nformula last_idx
    foreach "idxs idxs_var idxs_quotes idxs_br" [regexp -indices -inline -all \
	    $rex $formula] {
	append nformula [string range $formula $last_idx [lindex $idxs 0]-1]
	if { [lindex $idxs_var 0] != -1 } {
	    lassign $idxs_var i1 i2
	    if { [string range $formula $i1-1 $i2] in "!ERROR #REF!" } {
		append nformula [string range $formula {*}$idxs_var]
	    } else {
		append nformula "\$\{" [string range $formula {*}$idxs_var] "\}"
	    }
	} else {
	    append nformula [string range $formula {*}$idxs]
	}
	set last_idx [expr {[lindex $idxs 1]+1}]
    }
    append nformula [string range $formula $last_idx end]
    
    set cmd "formulae::subst_units $doc \$x"
    set formula [regmap {\$\{[[:alpha:]]\w*?\}\[.*?\]} $nformula x $cmd]
    regsub -all {\^} $formula {**} formula
    regsub -all {/} $formula {*1.0/} formula
    return $formula
}

proc formulae::nicer_number { num } {

    if { ![string is double -strict $num] } { return $num }
    set num [format %.4g $num]
    if { [regexp {(.*)e([+-])(\d+)} $num {} p1 s exp] } {
	if { $s eq "+" } { set s "" }
	if { $exp != 0 } { set exp [string trimleft $exp 0] }
	set num "$p1·10<sup>$s$exp</sup>"
    }
    return $num
}

proc formulae::nicer_name { key name } {
    variable values
    
    return [dict_getd $values $key nicer_name $name $name]
}

proc formulae::nicer_value { key name } {
    variable values
    
    set interp [dict get $values $key interp]
    set err [catch { $interp eval [list set ::$name] } value]
    if { !$err } {
	return [nicer_number $value]
    } else {
	return [nicer_name $key $name]
    }
}

proc formulae::_parse_args_list { txt } {
    lassign [list "" 0 0] list last_idx open_par
    for { set i 0 } { $i < [string length $txt] } { incr i } {
	switch -- [string index $txt $i] {
	    "(" { incr open_par }
	    ")" { incr open_par -1 }
	    "," {
		if { !$open_par } {
		    lappend list [string range $txt $last_idx $i-1]
		    set last_idx [expr {$i+1}]
		}
	    }
	}
    }
    lappend list [string range $txt $last_idx end]
    return $list
}

proc formulae::nicer_formula { args } {

    set optional {
	{ -subst_numbers "" 0 }
    }
    set compulsory "key formula"
    parse_args $optional $compulsory $args
    
    catch {
	if { [llength $formula] == 1 } {
	    set formula [lindex $formula 0]
	}
    }
    set map {
	== =
	!= \u2260 
	<= \u2264
	>= \u2265
	* \u0b7
    }
    set formula [string map $map $formula]

    if { [regexp {^\s*case\((.*)\)\s*$} $formula {} args] } {
	set formula [nicer_case_func {*}[_parse_args_list $args]]
    } elseif { [regexp {^\s*if\((.*)\)\s*$} $formula {} args] } {
	set formula [nicer_if_func {*}[_parse_args_list $args]]
    }
    if { !$subst_numbers } {
	set cmd "formulae::nicer_name [list $key] \$x"
    } else {
	set cmd "formulae::nicer_value [list $key] \$x"
    }
    regsub -all {sqrt\(} $formula "\u221a(" formula
    set formula [regmap {\m[[:alpha:]](?:\w*|!\w+)(?!\(|\w|"|\])} $formula x $cmd]
    regsub -all {\^(\w+)} $formula {<sup>\1</sup>} formula

    regsub -all {\*\*([\w$]+)} $formula {<sup>\1</sup>} formula
    regsub -all {\*\*\(([^)]+)\)} $formula {<sup>\1</sup>} formula
    regsub -all {\m(\d+)\.0\M} $formula {\1} formula
    regsub -all {\mpi\(\)} $formula "\u3c0" formula
    regsub -all {exp\((.*?)\)} $formula {e<sup>\1</sup>} formula
	
    return $formula
}

proc formulae::nicer_units { units } {
    regsub {(.*)/} $units {(\1)/} units
    return $units
}

proc formulae::nicer_case_func { args } {
    set ret ""
    foreach "cond val" $args {
	if { $val eq "" } {
	    set val $cond
	    set cond "[_ default]"
	}
	append ret "$val ($cond)\n"
    }
    return $ret
}

proc formulae::nicer_if_func { args } {
    lassign $args cnd val1 val2
    
    # \u2260 (diferente) \u2264 (<=) ; \u2265 (>=) ; \u226e (!<=) ; \u226f (!>=)

    set cmp {[<>=\u2260\u2264\u2265\u226e\u226f]{1,2}}
    set op {[^<>=\u2260\u2264\u2265\u226e\u226f]+}
    
    if { [regexp "^($op)($cmp)($op)\$" $cnd {} op1 sign op2] } {
	switch $sign \
	    < { set sign_op \u2265 } \
	    <= - \u2264 { set sign_op > } \
	    > { set sign_op \u2264 } \
	    >= - \u2265 { set sign_op < } \
	    = - == { set sign_op \u2260 } \
	    != - \u2260 { set sign_op = } \
	    \u226e { set sign_op \u2265 } \
	    \u226f { set sign_op \u2264 }
    }
    if { [info exists sign_op] } {
	set cnd2 "$op1$sign_op$op2"
    } else {
	set cnd2 "not($cnd)"
    }
    set ret "$val1 ($cnd)"
    if { [string length $ret] > 40 } {
	append ret "\n"
    } else {
	append ret " \u22ee "
    }
    append ret "$val2 ($cnd2)"
    return $ret
}


# proc formulae::nicer_formula_max { key args } {
#     set valList ""
#     foreach i $args {
#         lappend valList [nicer_formula $key $i]
#     }
#     return "max([join $valList ,])"
# }
# 
# proc formulae::nicer_formula_min { key args } {
#     set valList ""
#     foreach i $args {
#         lappend valList [nicer_formula $key $i]
#     }
#     return "min([join $valList ,])"
# }
# 
# proc formulae::max { doc args } {
#     variable window_doc
#     variable f_interp
#     set val ""
#     foreach i $args {
#         set iv [$f_interp eval [list expr [parse_formula $window_doc $i]]]
#         if { $val eq "" || $iv > $val } {
#             set val $iv
#         }
#     }
#     return $val
# }

# proc formulae::min { doc args } {
#     variable window_doc
#     variable f_interp
#     
#     set val ""
#     foreach i $args {
#         set iv [$f_interp eval [list expr [parse_formula $window_doc $i]]]
#         if { $val eq "" || $iv < $val } {
#             set val $iv
#         }
#     }
#     return $val
# }

proc formulae::e { key arg } {
    variable values
    
    set doc [dict get $values $key doc]
    set interp [dict get $values $key interp]

    return [$interp eval [list expr [parse_formula $doc $arg]]]
}

proc formulae::create_images {} {
    # nothing
}

proc formulae::create_give_image_color { color } {
    variable images
    if { ![dict exists $images color_$color] } {
	set img [image create photo -width 16 -height 8]
	$img put $color -to 0 0 15 7
	$img put black -to 0 0 15 1
	$img put black -to 0 6 15 7
	$img put black -to 0 0 1 7
	$img put black -to 14 0 15 7
	dict set images color_$color $img
    }
    return [dict get $images color_$color]
}

proc formulae::header_contextual_menu { key wp x y } {
    variable values

    if { [dict get $values $key state] eq "normal" } {
	header_contextual_menu_editable $key $wp $x $y
    } else {
	header_contextual_menu_non_editable $key $wp $x $y
    }
}
  
proc formulae::header_contextual_menu_editable { key wp x y } {
    variable values
    
    if { $wp eq "." } { set wp "" }
    catch { destroy $wp._cmenu }
    set menu [menu $wp._cmenu -tearoff 0]

    $menu add command -label [_ "Form properties"] -command [namespace code \
	    [list form_properties $key $wp]]
    $menu add command -label [_ "Wizard"] -command [namespace code \
	    [list wizard_start $key $wp]]
    $menu add separator
    $menu add command -label [_ "Delete form"] -command [namespace code \
	    [list delete_full_formulae $key $wp]]
    $menu add separator
    $menu add command -label [_ "Create/edit fields"] -command [namespace code \
	    [list edit_properties $key $wp]]
    $menu add command -label [_ "Import from other forms"] -command [namespace code \
	    [list import_other_forms $key $wp]]

    tk_popup $menu $x $y
}

proc formulae::header_contextual_menu_non_editable { key wp x y } {
    variable values
    
    if { $wp eq "." } { set wp "" }
    catch { destroy $wp._cmenu }
    set menu [menu $wp._cmenu -tearoff 0]

    $menu add command -label [_ "Import from other forms"] -command [namespace code \
	    [list import_other_forms $key $wp]]

    tk_popup $menu $x $y
}

proc formulae::entry_contextual_menu { key wp field_name container_param x y } {
    variable values

    if { [dict get $values $key state] eq "normal" } {
	entry_contextual_menu_editable $key $wp $field_name $container_param $x $y
    } else {
	entry_contextual_menu_non_editable $key $wp $field_name $container_param $x $y
    }
}

proc formulae::entry_contextual_menu_editable { key wp field_name container_param x y } {
    variable values
    
    if { $wp eq "." } { set wp "" }
    catch { destroy $wp._cmenu }
    set menu [menu $wp._cmenu -tearoff 0]
    
    $menu add command -label [_ "Edit field"] -command [namespace code [list edit_properties \
		-field_name $field_name $key $wp]]
	
    set doc [dict get $values $key doc]
    set xp [format_xpath {(//param[@n=%s and not(ancestor::setname)]} $field_name]
    append xp [format_xpath {|//container[@n=%s and not(ancestor::setname)]} $field_name]
    append xp [format_xpath {|//condition[@n=%s and not(ancestor::setname)])[1]} $field_name]
    set node [$doc selectNodes $xp]
    
    if { $node ne "" } {
	$menu add cascade -label RamDebugger -menu $menu.ram
	menu $menu.ram -tearoff 0
	$menu.ram add command -label [_ "Edit field '%s'" $field_name] -command \
	    [list formulae::edit_in_ramdebugger $key edit $node]
	if { [$node nodeName] ne "container" } {
	    set node [$node selectNodes ancestor::container]
	    $menu.ram add command -label [_ "Edit container '%s'" [$node @n]] -command \
		[list formulae::edit_in_ramdebugger $key edit $node]
	}
    }
    $menu add separator
    $menu add command -label [_ "Copy field"] -command [namespace code [list edit_properties \
		-field_name $field_name -location [list after $field_name] -copy_field 1 $key $wp]]

    if { $container_param eq "container" } {
	$menu add command -label [_ "Insert field inside"] -command [namespace code \
		[list edit_properties -location [list inside $field_name] $key $wp]]
    }
    $menu add command -label [_ "Insert field before"] -command [namespace code \
	    [list edit_properties -location [list before $field_name] $key $wp]]
    $menu add command -label [_ "Insert field after"] -command [namespace code \
	    [list edit_properties -location [list after $field_name] $key $wp]]
    $menu add separator
    $menu add command -label [_ "Delete field"] -command [namespace code [list \
	    edit_properties_delete -field_name $field_name $key $wp]]

    $menu add separator
    $menu add command -label [_ "Create drawing"] -command \
	[namespace code [list addmodify_SVG_drawing $key $wp $field_name ""]]

    $menu add separator
    
    $menu add command -label [_ "Import from other forms"] -command [namespace code \
	    [list import_other_forms $key $wp]]
    
    $menu add separator

    $menu add command -label [_ "Form properties"] -command [namespace code \
	    [list form_properties $key $wp]]
    $menu add command -label [_ "Wizard"] -command [namespace code \
	    [list wizard_start $key $wp]]

    $menu add separator
    $menu add command -label [_ "Delete form"] -command [namespace code \
	    [list delete_full_formulae $key $wp]]
    
    tk_popup $menu $x $y
}

proc formulae::entry_contextual_menu_non_editable { key wp field_name container_param x y } {
    variable values
    
    if { $wp eq "." } { set wp "" }
    catch { destroy $wp._cmenu }
    set menu [menu $wp._cmenu -tearoff 0]
    
    $menu add command -label [_ "Import from other forms"] -command [namespace code \
	    [list import_other_forms $key $wp]]
    
    set doc [dict get $values $key doc]
    set xp [format_xpath {(//param[@n=%s and not(ancestor::setname)]} $field_name]
    append xp [format_xpath {|//container[@n=%s and not(ancestor::setname)]} $field_name]
    append xp [format_xpath {|//condition[@n=%s and not(ancestor::setname)])[1]} $field_name]
    set node [$doc selectNodes $xp] 
    set field_type [$node @field_type ""]
    
    if {$field_type == "table"} {
	$menu add separator
	$menu add command -label [_ "Add row"] -command [namespace code \
		[list contextual_menu_table $key $field_name $node $wp add_item]] 
	$menu add command -label [_ "Edit row"] -command [namespace code \
		[list contextual_menu_table $key $field_name $node $wp edit_item]] 
	$menu add command -label [_ "Delete"] -command [namespace code \
		[list contextual_menu_table $key $field_name $node $wp delete_item]]
	$menu add command -label [_ "Select all"] -command [namespace code \
		[list contextual_menu_table $key $field_name $node $wp select_all]]                
	$menu add command -label [_ "Copy"] -command [namespace code \
		[list contextual_menu_table $key $field_name $node $wp copy]] -acc "Ctrl-c"  
	$menu add separator
	$menu add command -label [_ "Import from CSV file"] -command [namespace code \
		[list contextual_menu_table $key $field_name $node $wp importfile]]
	$menu add command -label [_ "Export to CSV file"] -command [namespace code \
		[list contextual_menu_table $key $field_name $node $wp filesave]]                
    }

    tk_popup $menu $x $y   
}

proc formulae::add_drawing_contextual_menu { key container_name canvas menu } {
    
    $menu add separator
    $menu add command -label [_ "Edit drawing"] -command \
	[namespace code [list addmodify_SVG_drawing $key $menu $container_name $canvas]]
    $menu add separator
    $menu add command -label [_ "Delete"] -command \
	[namespace code [list delete_SVG_drawing $key $menu $container_name]]
}

proc formulae::delete_SVG_drawing { key wp field_name } {
    variable values

    set doc [dict get $values $key doc]
    
    set xp {
	//param[@n='N'] |
	//condition[@n='N'] |
	//container[@n='N']
    }
    set xp [string map [list N $field_name] $xp]
    set childNode [$doc selectNodes $xp]
    if { [$childNode nodeName] ne "container" } {
	set parentNode [$childNode parentNode]
    } else {
	set parentNode $childNode
    }

    set ns { svg http://www.w3.org/2000/svg }
    set svgNode [$parentNode selectNodes -namespaces $ns svg:svg]
    if { $svgNode ne "" } { $svgNode delete }
    create_windowD_do $key
}

proc formulae::addmodify_SVG_drawing { key wp field_name canvas } {
    variable values
    
    set doc [dict get $values $key doc]
    
    set xp {
	//param[@n='N'] |
	//condition[@n='N'] |
	//container[@n='N']
    }
    set xp [string map [list N $field_name] $xp]
    set childNode [$doc selectNodes $xp]
    if { [$childNode nodeName] ne "container" } {
	set parentNode [$childNode parentNode]
    } else {
	set parentNode $childNode
    }

    set ns { svg http://www.w3.org/2000/svg }
    set svgNode [$parentNode selectNodes -namespaces $ns svg:svg]

    if { [interp exists ramdrawer] } { 
	interp delete ramdrawer
	package forget ramdrawer
    }
    interp create ramdrawer
    set argv [list -read_default_file 0 -save_callback \
	    [list save_formulae $key $parentNode]]
    
    if { $svgNode ne "" } { set xml [$svgNode asXML] } else { set xml "" }
    if { $canvas ne "" && [winfo width $canvas] > 1 } {
	set geometry [winfo width $canvas]x[winfo height $canvas]
    } else {
	set geometry ""
    }
    lappend argv -geometry $geometry -xmldata [list $xml 1 *f*]   
    
    package require ramdrawer
    ramdrawer eval [list proc save_formulae { key parentNode xml } {
	    master [list formulae::addmodify_SVG_drawing_do $key $parentNode $xml]
	}]
    ramdrawer eval [list destroy .r]
    ramdrawer eval [list ramdraw .r {*}$argv]
}

proc formulae::addmodify_SVG_drawing_do { key parentNode xml } {

    dom parse $xml doc
    set svgNode [$doc documentElement]
    set ns { svg http://www.w3.org/2000/svg }

    set err [catch {
	    set svgNode_old [$parentNode selectNodes -namespaces $ns svg:svg]
	    if { $svgNode_old ne "" } { $svgNode_old delete }
	    $parentNode appendChild $svgNode
	} errstring]
    if { $err } {
	snit_messageBox -message [_ "Ramdrawer out of sync (%s)" $errstring]
    }
    create_windowD_do $key
}

proc formulae::_find_entry_combo_with_var { w varname } {

    if { [winfo class $w] in "TEntry TCombobox" && [$w cget -textvariable] eq $varname } {
	return $w
    }
    foreach i [winfo children $w] {
	set ret [_find_entry_combo_with_var $i $varname]
	if { $ret ne "" } { return $ret }
    }
    return ""
}

proc formulae::_find_radiobuttons_with_var { w varname } {

    set rets ""
    if { [winfo class $w] eq "TRadiobutton" && [$w cget -variable] eq $varname } {
	lappend rets $w
    }
    foreach i [winfo children $w] {
	set rets_in [_find_radiobuttons_with_var $i $varname]
	if { [llength $rets_in] } { lappend rets {*}$rets_in }
    }
    return $rets
}

proc formulae::_find_neighbor_labels_buttons { widget { class "" } } {

    set master [dict get [grid info $widget] -in]
    set row [dict get [grid info $widget] -row]
    set ret ""
    foreach child [grid slaves $master -row $row] {
	if { $child eq $widget } { continue }
	if { $class ne "" } {
	    if { [winfo class $child] eq $class } {
		lappend ret $child
	    }
	} elseif { [winfo class $child] in "TLabel TButton TMenubutton Canvas" } {
	    lappend ret $child
	}
    }
    return $ret
}

# update edit properties window
# args: ?-step prop_type|container? ?-field_name field_name? 
#       ?-original_field_name? ?-copy boolean? ?-location "before|after field_name"? w
proc formulae::edit_properties_update { args } {
    variable values
    
    set optional {
	{ -step prop_type|container "" }
	{ -field_name name "" }
	{ -original_field_name "" 0 }
	{ -location "before|after field_name" "" }
	{ -copy_field boolean 0 }
    }
    set compulsory "key w"
    parse_args $optional $compulsory $args

    set steps [list "" prop_type container]
    set step_pos [lsearch -exact $steps $step]
    
    if { $original_field_name } {
	set field_name [$w give_uservar_value original_field_name]
	if { $location eq "" } {
	    set location [$w give_uservar_value original_location]
	}
    }
    set doc [dict get $values $key doc]
    
    foreach i [list param_fields goto_fields fields sfields] {
	set $i ""
    }
    set containers ""
    foreach domNode [$doc selectNodes //param|//condition|//container] {
	if { [$domNode hasAttribute n] } {
	    if { [$domNode hasAttribute n] } {
		if { [$domNode nodeName] eq "container" && [llength $goto_fields] } {
		    lappend goto_fields ---
		}
		lappend goto_fields [$domNode @n]
	    }
	    if { [$domNode hasAttribute n] } {
		lappend fields [$domNode @n]
	    }
	    if { [$domNode nodeName] eq "param" && [$domNode hasAttribute n] } {
		lappend param_fields [$domNode @n]
	    }
	    if { [$domNode hasAttribute pns] } {
		lappend sfields [$domNode @pns [$domNode @n]]
	    }
	    if { [$domNode nodeName] eq "container" } {
		lappend containers [$domNode @n]
	    }
	}
    }

    if { !$copy_field } {
	$w set_uservar_value original_field_name $field_name
    } else {
	$w set_uservar_value original_field_name ""
	$w set_uservar_value copyfrom_field_name $field_name
    }
    $w set_uservar_value original_location $location

    set xp [format_xpath {(//param[@n=%s and not(ancestor::setname)]} $field_name]
    append xp [format_xpath {|//container[@n=%s and not(ancestor::setname)]} $field_name]
    append xp [format_xpath {|//condition[@n=%s and not(ancestor::setname)])[1]} $field_name]
    set node [$doc selectNodes $xp]

################################################################################
#    property type
################################################################################

    if { $step_pos == 0 || (![llength $containers] && \
	[$w give_uservar_value prop_type] ne "container") } {
	if { ![llength $containers] } {
	    $w set_uservar_value prop_type container
	} elseif { $field_name eq "" } {
	    $w set_uservar_value prop_type param
	} else {
	    $w set_uservar_value prop_type [$node nodeName]
	}
    }
    foreach r [_find_radiobuttons_with_var $w [$w give_uservar prop_type]] {
	if { $field_name ne "" } {
	    $r state disabled
	} elseif { [llength $containers] || [$r cget -value] eq "container" } {
	    $r state !disabled
	} else {
	    $r state disabled
	}
    }

################################################################################
#    navigate
################################################################################

    set b [$w give_uservar_value previous_button]
    $b configure -command [namespace code [list edit_properties_navigate $key $w \
		$field_name prev]]

    set b [$w give_uservar_value next_button]
    $b configure -command [namespace code [list edit_properties_navigate $key $w \
		$field_name next]]

    set goto_menu [$w give_uservar_value goto_menu]
    $goto_menu delete 0 end
    foreach i $goto_fields {
	if { $i eq "---" } {
	    $goto_menu add separator
	} else {
	    $goto_menu add command -label $i -command \
		[namespace code [list edit_properties_navigate $key $w $i this]]
	}
    }

################################################################################
#    location
################################################################################

    if { [llength $location] && [lindex $location 1] ne "" } {
	set xp {
	    //param[@n='N'] |
	    //condition[@n='N'] |
	    //container[@n='N']
	}
	set xp1 [string map [list N [lindex $location 1]] $xp]
	set location_node [$doc selectNodes $xp1]
	set container [$location_node selectNodes {string(../@n)}]
	if { [$w give_uservar_value prop_type] eq "container" } {
	    $w set_uservar_value container ""
	} elseif { $step_pos == 2 } {
	    #nothing
	} elseif { $container ne "" } {
	    $w set_uservar_value container $container
	} elseif { [lindex $location 0] ne "inside" } {
	    if { $step_pos == 1 && $location_node ne "" } {
		$w set_uservar_value container [$location_node @n]
	    } else {
		$w set_uservar_value container ""
		$w set_uservar_value prop_type container
	    }
	} else {
	    $w set_uservar_value container [$location_node @n]
#[lindex $containers 0]
	}
    } elseif { $step_pos <= 1 } {
	if { $field_name eq "" } {
	    if { [$w give_uservar_value prop_type] eq "container" } {
		$w set_uservar_value container ""
	    } else {
		$w set_uservar_value container [lindex $containers 0]
	    }
	} else {
	    $w set_uservar_value container [$node selectNodes {string(../@n)}]
	}
    }

    set c [_find_entry_combo_with_var $w [$w give_uservar container]]
    if { [$w give_uservar_value prop_type] eq "container" } {
	$c configure -values ""
	$c state disabled
    } else {
	$c state !disabled
	$c configure -values $containers
    }

    set container [$w give_uservar_value container]
    set containerNode [$doc selectNodes //container\[@n='$container'\]]
    if { $containerNode eq "" } {
	set containerNode [$doc selectNodes formulae]
    }
    set nodes [$containerNode selectNodes container|param|condition]
    set ipos [lsearch -exact $nodes $node]

    if { [llength $location] && [lindex $location 1] ne ""  } {
	if { [lsearch -exact $nodes $location_node] != -1 } {
	    switch [lindex $location 0] {
		before {
		    $w set_uservar_value before_after [_ Before]
		}
		after {
		    $w set_uservar_value before_after [_ After]
		}
	    }
	    set nei_node $location_node
	} else {
	    $w set_uservar_value before_after [_ After]
	    set nei_node [lindex $nodes end]
	}
    } elseif { [$w give_uservar_value before_after] eq [_ After] && $ipos > 0 } {
	set nei_node [lindex $nodes [expr {$ipos-1}]]
    } elseif { $ipos>= 0 && $ipos < [llength $nodes]-1 } {
	$w set_uservar_value before_after [_ Before]
	set nei_node [lindex $nodes [expr {$ipos+1}]]
    } elseif { [llength $nodes] && $ipos == -1 } {
	$w set_uservar_value before_after [_ After]
	set nei_node [lindex $nodes end]
    } else { set nei_node "" }

    if { $nei_node ne "" } {
	$w set_uservar_value before_after_name [$nei_node @n ""]
    } else {
	$w set_uservar_value before_after_name ""
    }

    set c [_find_entry_combo_with_var $w [$w give_uservar before_after_name]]
    set valuesList ""
    foreach i $nodes {
	if { [$i hasAttribute n] } {
	    lappend valuesList [$i @n]
	}
    }
    $c configure -values $valuesList

    set mb [_find_neighbor_labels_buttons $c TMenubutton]
    set menu [$mb cget -menu]
    if { [$w give_uservar_value prop_type] eq "container" } {
	$menu entryconfigure [_ "Same page"] -state normal
	$menu entryconfigure [_ "Same line"] -state disabled
	$menu entryconfigure [_ "Same menu"] -state disabled
	#$mb state !disabled
	$w set_uservar_value same_line 0
	$w set_uservar_value same_menu 0
    } else {
	$menu entryconfigure [_ "Same page"] -state disabled
	$menu entryconfigure [_ "Same line"] -state normal
	$menu entryconfigure [_ "Same menu"] -state normal
	$w set_uservar_value same_page 0
	#$mb state disabled
    }
    if { [lindex $steps $step_pos] eq "container" } {
	# a shortcut to avoid changing user modified values
	return
    }

    foreach i [list same_page same_line same_menu] { $w set_uservar_value $i 0 }
    
    if { !$copy_field && $field_name ne "" && [$node @location ""] eq "same_page" } {
	$w set_uservar_value same_page 1
    } elseif { !$copy_field && $field_name ne "" && [$node @location ""] eq "same_line" } {
	$w set_uservar_value same_line 1
    } elseif { !$copy_field && $field_name ne "" && [$node @location ""] eq "same_menu" } {
	$w set_uservar_value same_menu 1
    }

################################################################################
#    Name
################################################################################

    if { !$copy_field && $field_name ne "" } {
	$w set_uservar_value name [$node @n ""]
	set state disabled
    } else {
	$w set_uservar_value name ""
	set state !disabled
    }
    set c [_find_entry_combo_with_var $w [$w give_uservar name]]
    $c configure -values $fields
    $c state $state

################################################################################
#    short name
################################################################################

    set c [_find_entry_combo_with_var $w [$w give_uservar short_name]]
    if { [$w give_uservar_value prop_type] eq "container" } {
	$c state !disabled
	foreach label [_find_neighbor_labels_buttons $c] {
	    $label state !disabled
	}
	if { $field_name ne "" } {
	    $w set_uservar_value short_name [$node @pns [$node @n]]
	}
	$c configure -values $sfields
    } else {
	$c state disabled
	foreach label [_find_neighbor_labels_buttons $c] {
	    $label state disabled
	}
    }

################################################################################
#    print name
################################################################################

    set c [$w give_uservar_value print_name_widget]
    $c ClearText
    if { [$w give_uservar_value prop_type] eq "condition" } {
	$c configure -state disabled -bg grey90
	foreach label [_find_neighbor_labels_buttons $c] {
	    $label state disabled
	}
    } else {
	$c configure -state normal -bg white
	if { $field_name ne "" } {
	    $c ApplyXMLdataAsText <lognoter>[node_pn_xml $node]</lognoter>
	}
	foreach label [_find_neighbor_labels_buttons $c] {
	    $label state !disabled
	}
    }

################################################################################
#    field type & expression
################################################################################

#     set field_types [list numeric text "long text" expression "text expression" \
#             "options editable" "options non-editable" \
#             "options multiple editable" "options multiple non-editable" \
#              formatted "formatted expression" button date file report_maker]

    set value ""
    if { $field_name ne "" } { set value [$node @value ""] }

    set c [_find_entry_combo_with_var $w [$w give_uservar field_type]]

    set field_type [$w give_uservar_value field_type numeric]

    if { [$w give_uservar_value prop_type] eq "param" } {
	$c state !disabled
	foreach label [_find_neighbor_labels_buttons $c] {
	    $label state !disabled
	}
	if { $field_name ne "" } {
	    if { [$node @field_type ""] eq "" } {
		if { [$node @values ""] ne "" } {
		    if { [$node @editable 0] } {
		        set field_type "options editable"
		    } else {
		        set field_type "options non-editable"
		    }
		} elseif { [regexp {\[e\s+(.*)\]} $value] } {
		    set field_type expression
		} elseif { [string is double -strict $value] && $value > 1e-100 && $value < 1e100 } {
		    set field_type numeric
		} else {
		    set field_type text
		}
	    } else {
		set field_type [$node @field_type]
	    }
	}
    } else {
	$c state disabled
	foreach label [_find_neighbor_labels_buttons $c] {
	    $label state disabled
	}
	set field_type expression
    }
    $w set_uservar_value field_type $field_type
    
    set c [_find_entry_combo_with_var $w [$w give_uservar value]]
    if { [$w give_uservar_value prop_type] eq "container" } {
	$c state disabled
	foreach c_in [_find_neighbor_labels_buttons $c] {
	    $c_in state disabled
	}
    } else {
	$c state !disabled
	foreach c_in [_find_neighbor_labels_buttons $c] {
	    $c_in state !disabled
	}
	switch -glob $field_type {
	    "expression" {
		set cmd_in [string trim $value {[]}]
		set err [catch { lindex $cmd_in 0 } c]
		if { !$err && $c eq "e" && [llength $cmd_in] == 2 } {
		    set value [lindex $cmd_in 1]
		} else {
		    regexp {\[e\s+(.*)\]} $value {} value
		    set value [string trim $value "{}"]
		}
	    }
	    "text expression" {
		regexp {\[(.*)\]} $value {} value
	    }        
	    "formatted expression" {
		regexp {\[(.*)\]} $value {} value
	    }
	    "options*" {
		if { $node ne "" } {
		    $w set_uservar_value valuesField [$node @values ""]
		} else {
		    $w set_uservar_value valuesField ""
		}
	    }
	    file {
		set value ""
	    }
	    button {
		set value [$node @command ""]
	    }
	}
	set value_widget [$w give_uservar_value value_widget]
	set value_fields_widget [$w give_uservar_value value_fields_widget]
	$value_fields_widget delete 0 end
	foreach i $param_fields {
	    $value_fields_widget add command -label $i -command \
		[namespace code [list _insert_in_entry $value_widget $i]]
	}
    }
    $w set_uservar_value value $value

################################################################################
#    units
################################################################################

    if { $field_name ne "" } {
	$w set_uservar_value units [$node @units ""]
    } else { $w set_uservar_value units "" }
    set c [_find_entry_combo_with_var $w [$w give_uservar units]]
    if { [$w give_uservar_value prop_type] ne "container" } {
	$c state !disabled
	foreach label [_find_neighbor_labels_buttons $c] {
	    $label state !disabled
	}
    } else {
	$c state disabled
	foreach label [_find_neighbor_labels_buttons $c] {
	    $label state disabled
	}
    }
    
################################################################################
#    condition
################################################################################
    
    set c [_find_entry_combo_with_var $w [$w give_uservar condition]]
    
    if { [$w give_uservar_value prop_type] eq "condition" } {
	$c state disabled
	foreach c_in [_find_neighbor_labels_buttons $c] {
	    $c_in state disabled
	}
	set condition ""
    } else {
	$c state !disabled
	foreach c_in [_find_neighbor_labels_buttons $c] {
	    $c_in state !disabled
	}
	set condition ""
	if { $field_name ne "" } { set condition [$node @condition ""] }
	set cmd_in [string trim $condition {[]}]
	set err [catch { lindex $cmd_in 0 } c]
	if { !$err && $c eq "e" && [llength $cmd_in] == 2 } {
	    set condition [lindex $cmd_in 1]
	} else {
	    regexp {\[e\s+(.*)\]} $condition {} condition
	    set condition [string trim $condition "{}"]
	}
	set condition_widget [$w give_uservar_value condition_widget]
	set condition_fields_widget [$w give_uservar_value condition_fields_widget]
	$condition_fields_widget delete 0 end
	foreach i $param_fields {
	    $condition_fields_widget add command -label $i -command \
		[namespace code [list _insert_in_entry $condition_widget $i]]
	}
    }
    $w set_uservar_value condition $condition


################################################################################
#    true & false
################################################################################

    if { $field_name ne "" } {
	$w set_uservar_value true [$node @true ""]
	$w set_uservar_value false [$node @false ""]
    } else {
	$w set_uservar_value true ""
	$w set_uservar_value false ""
    }
    set c1 [_find_entry_combo_with_var $w [$w give_uservar true]]
    set c2 [_find_entry_combo_with_var $w [$w give_uservar false]]
    if { [$w give_uservar_value prop_type] eq "condition" } {
	$c1 state !disabled
	$c2 state !disabled
	foreach label [_find_neighbor_labels_buttons $c1] {
	    $label state !disabled
	}
	foreach label [_find_neighbor_labels_buttons $c2] {
	    $label state !disabled
	}
    } else {
	$c1 state disabled
	$c2 state disabled
	foreach label [_find_neighbor_labels_buttons $c1] {
	    $label state disabled
	}
	foreach label [_find_neighbor_labels_buttons $c2] {
	    $label state disabled
	}
    }

################################################################################
#    help
################################################################################

    set widget [$w give_uservar_value help_widget]
    $widget delete 1.0 end
    if { $field_name ne "" } {
	$widget insert end [$node @help ""]
	$w set_uservar_value help_lognoter [$node @help_lognoter ""]
    } else {
	$w set_uservar_value help_lognoter ""
    }
}

# accept values in edit properties
# args: ?-refresh boolean? ?-delete_original? ?-copy_children boolean? w
proc formulae::edit_properties_accept { args } {
    variable values
    
    set optional {
	{ -refresh boolean 1 }
    }
    set compulsory "key w"
    parse_args $optional $compulsory $args

    set delete_original 1

    set doc [dict get $values $key doc]
    
    set prop_type [$w give_uservar_value prop_type]
    set newnode [[$doc ownerDocument] createElement $prop_type]

    set original_field_name [$w give_uservar_value original_field_name]

    set xp [format_xpath {//param[@n=%s]|//condition[@n=%s]|//container[@n=%s]} \
	    $original_field_name $original_field_name $original_field_name]
    set original_fieldNode [lindex [$doc selectNodes $xp] 0]
    
################################################################################
#    name
################################################################################

    set name [string trim [$w give_uservar_value name]]
    if { $name eq "" && $prop_type eq "condition" } {
	set name [string trim [$w give_uservar_value value]]
    }
    if { $name eq "" } {
	$newnode delete
	error [_ "error: name cannot be void"]
    }
    if { [regexp {__} $name] } {
	$newnode delete
	error [_ "error: name cannot contain 2 underlines (__)"]
    }

    if { ![regexp {^[[:alpha:]][\w\s!]*$} $name] } {
	error [_ "error: name must begin with a letter and contain letters, numbers spaces or _ or !"]
    }
    if { [lsearch -exact -nocase [list id id_name] $name] != -1 } {
	$newnode delete
	error [_ "Identifier '%s' is not allowed for a field name" $name]
    }
    $newnode setAttribute n $name

    if { $prop_type eq "container" } {
	$newnode setAttribute pns [string trim [$w give_uservar_value short_name]]
    }

################################################################################
#    print name
################################################################################

    set c [$w give_uservar_value print_name_widget]
    if { $prop_type ne "condition" } {
	dom parse <lognoter>[$c exportXML]</lognoter> doc_pn
	$doc_pn documentElement root_pn
	set paraNode [$root_pn selectNodes {para[1]}]
	set pn ""
	foreach node [$paraNode childNodes] {
	    append pn [$node asXML -indent none]
	}
	set pn [string trim $pn]
	if { $pn ne "" } {
	    $newnode setAttribute pn $pn
	} elseif { [$newnode hasAttribute pn] } {
	    $newnode removeAttribute pn
	}
    }

################################################################################
#    expression
################################################################################

#     set field_types [list numeric text "long text" expression "text expression" \
#             "options editable" "options non-editable" \
#             "options multiple editable" "options multiple non-editable" \
#              formatted "formatted expression" button date file report_maker]

    foreach att [list field_type values editable command] {
	if { [$newnode hasAttribute $att] } {
	    $newnode removeAttribute $att
	}
    }
    if { $prop_type eq "param" } {
	$newnode setAttribute field_type [$w give_uservar_value field_type]
    } else {
	set field_type expression
    }

    set value [string trim [$w give_uservar_value value]]

    switch -glob [$w give_uservar_value field_type] {
	"expression" {
	    if { $value eq "" } {
		if { $prop_type ne "container" } {
		    $newnode delete
		    error [_ "error: expression cannot be void"]
		}
	    } else {
		set value "\[[list e $value]\]"
	    }
	}
	"text expression" - "formatted expression" {
	    if { $value eq "" } {
		$newnode delete
		error [_ "error: expression cannot be void"]
	    } 
	    set value "\[$value\]"
	}
	"options*" {
	    set valuesList [comma_field_to_list [string trim [$w give_uservar_value valuesField]]]
	    $newnode setAttribute values [list_to_comma_field $valuesList]
	    set field_type [$w give_uservar_value field_type]
	    if { $field_type eq "options multiple non-editable" } {
		set fail 0
		foreach i [comma_field_to_list $value] {
		    if { $i ni $valuesList } {
		        set fail 1
		        break
		    }
		}
		if { $fail } {
		    $newnode delete
		    error [_ "error: default values for option must be elements of the list"]
		}
		$newnode setAttribute editable 0
	    } elseif { $field_type eq "options non-editable" } {
		if { [lsearch -exact $valuesList $value] == -1 } {
		    $newnode delete
		    error [_ "error: default value for option must be one element of the list"]
		}
		$newnode setAttribute editable 0
	    } elseif { [string match "*editable" $field_type] } {
		$newnode setAttribute editable 1
	    }
	}
	"numeric" {
	    if { $value eq "" } {
		set value 0.0
	    } elseif { ![string is double -strict $value] } {
		$newnode delete
		error [_ "error: value must be a number"]
	    }
	}
	"button" {
	    $newnode setAttribute command $value
	    set value ""
	}
	"file" {
	    set value ""
	}
    }
    if { $prop_type ne "container" && [$w give_uservar_value field_type] ni "button" } {
	$newnode setAttribute value $value
    }

################################################################################
#   condition
################################################################################

    if { $prop_type ne "condition" } {
	set condition [string trim [$w give_uservar_value condition]]
	if { $condition ne "" } {
	    $newnode setAttribute condition "\[[list e $condition]\]"
	} 
    }

################################################################################
#    units & true/false & help
################################################################################

    if { $prop_type ne "container" } {
	set units [string trim [$w give_uservar_value units]]
	if { $units ne "" } {
	    $newnode setAttribute units $units
	}
    }
    if { $prop_type eq "condition" } {
	set true [string trim [$w give_uservar_value true]]
	if { $true ne "" } {
	    $newnode setAttribute true $true
	}
	set false [$w give_uservar_value false]
	if { $false ne "" } {
	    $newnode setAttribute false $false
	}
    }
    set widget [$w give_uservar_value help_widget]
    set help [string trim [$widget get 1.0 end-1c]]
    if { $help ne "" } {
	$newnode setAttribute help $help
    }
    set help_lognoter [$w give_uservar_value help_lognoter]
    if { $help_lognoter ne "" } {
	$newnode setAttribute help_lognoter $help_lognoter
    }
 
################################################################################
#    location
################################################################################

    if { $prop_type eq "container" && [$w give_uservar_value same_page] } {
	$newnode setAttribute location same_page
    } elseif { $prop_type ne "container" && [$w give_uservar_value same_menu] } {
	$newnode setAttribute location same_menu
    } elseif { $prop_type ne "container" && [$w give_uservar_value same_line] } {
	$newnode setAttribute location same_line
    } 
   
################################################################################
#    Checking that new name is different than existing ones
################################################################################
    
    set fieldsList ""
    foreach node [$doc selectNodes {//param|//condition//container}] {
	lappend fieldsList [$node @n]
    }

    foreach n [lsort -unique [lsearch -exact -inline -all -nocase $fieldsList $name]] {
	if { $delete_original && $n eq $original_field_name } {
	    continue
	}
	set xp [format_xpath {//param[@n=%s]|//condition[@n=%s]|//container[@n=%s]} \
		$n $n $n]
	foreach domNode [$doc selectNodes $xp] {
	    set text [_ "There is already a field with name '%s'. Overwrite?" $n]
	    set retval [snit_messageBox -default ok -icon question -message $text \
		    -type okcancel -parent $w]
	    if { $retval == "cancel" } {
		$newnode delete
		error ""
	    }
	}
    }
    
################################################################################
#    maintaining atributtes not modified in window
################################################################################

    if { $original_fieldNode ne "" } {
	foreach att [list onchange values_tree columns dict global_interp global_interp_v] {
	    if { [$original_fieldNode hasAttribute $att] } {
		$newnode setAttribute $att [$original_fieldNode @$att]
	    }
	}
    }
    
################################################################################
#    Saving a copy, for emergencies
################################################################################

    set formulaeNode [$doc selectNodes formulae]
    if { [$formulaeNode @database ""] ne "" } {
	set formulaeNode_save [$formulaeNode cloneNode -deep]
    } else {
	set formulaeNode_save ""
    }

################################################################################
#    deleting old nodes & inserting new one
################################################################################

    if { $delete_original } {
	if { $original_fieldNode ne "" } {
	    foreach childNode [$original_fieldNode childNodes] {
		$newnode appendChild [$original_fieldNode removeChild $childNode]
	    }
	    $original_fieldNode delete
	}
    }
    foreach n [lsort -unique [lsearch -exact -inline -all -nocase $fieldsList $name]] {
	set xp [format_xpath {//param[@n=%s]|//condition[@n=%s]|//container[@n=%s]} \
		$n $n $n]
	foreach domNode [$doc selectNodes $xp] {
	    foreach childNode [$domNode childNodes] {
		$newnode appendChild [$domNode removeChild $childNode]
	    }
	    $domNode delete
	}
    }
    set container [$w give_uservar_value container]
    set containerNode [$doc selectNodes [format_xpath {//container[@n=%s]} $container]]
    if { $containerNode eq "" } {
	set containerNode [$doc selectNodes formulae]
    }

    set nei_node_name [$w give_uservar_value before_after_name]
    set xp [format_xpath {//param[@n=%s]|//condition[@n=%s]|//container[@n=%s]} \
	    $nei_node_name $nei_node_name $nei_node_name]
    set nei_node [$doc selectNodes $xp]
    if { $nei_node ne "" && [$w give_uservar_value before_after] eq [_ After] } {
	set nodes [$containerNode childNodes]
	set ipos [lsearch -exact $nodes $nei_node]
	if { $ipos == -1 || $ipos == [llength $nodes]-1 } {
	    set nei_node ""
	} else {
	    set nei_node [lindex $nodes [incr ipos]]
	}
    }
    if { $nei_node eq "" } {
	$containerNode appendChild $newnode
    } else {
	$containerNode insertBefore $newnode $nei_node
    }

    set err [catch { create_update_database_table $key } errstring]
    
################################################################################
#    restoring the copy, if error
################################################################################

    if { $err } {
	if { $formulaeNode_save ne "" } {
	    foreach node [$formulaeNode childNodes] {
		$node delete
	    }
	    foreach node [$formulaeNode_save childNodes] {
		$formulaeNode appendChild $node
	    }
	    $formulaeNode_save delete
	}
	error $errstring
    }
    if { $formulaeNode_save ne "" } { $formulaeNode_save delete }
    
    if { $refresh } {
	create_windowD_do $key
    } else {
	$w set_uservar_value refresh_pending 1
    }
}

# args: ?-refresh boolean? ?-field_name field_name? w
proc formulae::edit_properties_delete { args } {
    variable values

    set refresh 1
    set field_name ""
    while { [string match -* [lindex $args 0]] } {
	switch -- [lindex $args 0] {
	    -field_name {
		set args [lrange $args 1 end]
		set field_name [lindex $args 0]
	    }
	    -refresh {
		set args [lrange $args 1 end]
		set refresh [lindex $args 0]
	    }
	    default { error "error in formulae::edit_properties_delete" }
	}
	set args [lrange $args 1 end]
    }
    foreach "key w" $args break

    set doc [dict get $values $key doc]
    
    if { $field_name eq "" } {
	set field_name [string trim [$w give_uservar_value name]]
    }
    if { $field_name eq "" } {
	error [_ "error: name cannot be void"]
    }
    
    set formulaeNode [$doc selectNodes formulae]
    if { [$formulaeNode @database ""] ne "" } {
	set formulaeNode_save [$formulaeNode cloneNode -deep]
    } else {
	set formulaeNode_save ""
    }
    set xp {
	//param[@n='N'] |
	//condition[@n='N'] |
	//container[@n='N']
    }
    set xp1 [string map [list N $field_name] $xp]
    set idx 0
    foreach domNode [$doc selectNodes $xp1] {
	$domNode delete
	incr idx
    }
    if { !$idx } {
	error [_ "error: name is not correct"]
    }
    
    set err [catch { create_update_database_table $key } errstring]
    
    if { $err } {
	if { $formulaeNode_save ne "" } {
	    foreach node [$formulaeNode childNodes] {
		$node delete
	    }
	    foreach node [$formulaeNode_save childNodes] {
		$formulaeNode appendChild $node
	    }
	    $formulaeNode_save delete
	}
	error $errstring
    }
    if { $formulaeNode_save ne "" } { $formulaeNode_save delete }

    if { $refresh } {
	create_windowD_do $key
    } else {
	$w set_uservar_value refresh_pending 1
    }
}

# args: ?-update_callback update_callback? wp
proc formulae::form_properties { args } {
    variable values

    set optional {
	{ -update_callback cmd "" }
    }
    set compulsory "key wp"
    
    parse_args $optional $compulsory $args

    set doc [dict get $values $key doc]
    set database_default_table [dict get $values $key database_default_table]

    destroy $wp._props
    set w [dialogwin_snit $wp._props -title [_ "Form properties"] \
	    -callback [namespace code [list form_properties_do $key $update_callback]] -grab 1 \
	    -transient 1]

    set f [$w giveframe]

    set f1 [ttk::labelframe $f.f1 -text [_ title]]
    ttk::label $f1.l1 -text [_ "Title"]:
    ttk::entry $f1.e1 -textvariable [$w give_uservar title] -width 40

    ttk::label $f1.l2 -text [_ "Description"]:
    text $f1.t1 -wrap word -width 50 -height 2 -bd 1 -font [$f1.e1 cget -font]
    $w set_uservar_value desc_widget $f1.t1

    bind $f1.t1 <Return> "[bind Text <Return>];break"
    bind $f1.t1 <Tab> "[bind all <Tab>];break"
    bind $f1.t1 <<PrevWindow>> "[bind all <<PrevWindow>>];break"
    
    ttk::label $f1.l3 -text [_ "Lognoter help:"]
    cu::combobox_tree $f1.cb -width 20 -textvariable [$w give_uservar help_lognoter] \
	-state readonly -postcommand [list formulae::_fill_lognoter_pages_menu $key $f1.cb]
    
    set help [_ "Use a page of current lognoter database for help"]
    tooltip $f1.l3 $help
    tooltip $f1.cb $help

    grid $f1.l1 $f1.e1 - -sticky w -padx 2 -pady 2
    grid $f1.l2 - - -sticky w -padx 2 -pady 2
    grid $f1.t1 - - -sticky ewns -padx "2 40" -pady "0 4"
    grid $f1.l3 - $f1.cb -sticky w -pady 2
    grid configure $f1.e1 $f1.cb -sticky ew -padx "0 40"
    grid columnconfigure $f1 2 -weight 1
    grid rowconfigure $f1 2 -weight 1
    
    set f2 [ttk::labelframe $f.f2 -text [_ "form type"]]

    ttk::radiobutton $f2.r1 -text [_ "No database"] -variable \
	[$w give_uservar form_type] -value no_database
    
    ttk::label $f2.l1 -text [_ "The form has no associated database. Formulas can be evaluated but results sets cannot be stored"] \
	-wrap 400 -justify left

    ttk::radiobutton $f2.r2 -text [_ "Has database"] -variable \
	[$w give_uservar form_type] -value has_database
    
    ttk::label $f2.l2 -text [_ "The form has one associated database. Sets of values can be stored by the form user in view mode"] \
	-wrap 400 -justify left

    ttk::radiobutton $f2.r3 -text [_ "Has database with named entries"] -variable \
	[$w give_uservar form_type] -value has_database_entries
    
    ttk::label $f2.l3 -text [_ "The form has one associated database. Every stored set of values will have a name"] \
	-wrap 400 -justify left
    
    grid $f2.r1 -sticky w -padx 2 -pady "5 0"
    grid $f2.l1 -sticky w -padx "40 2"
    grid $f2.r2 -sticky w -padx 2 -pady "5 0"
    grid $f2.l2 -sticky w -padx "40 2"
    grid $f2.r3 -sticky w -padx 2 -pady "5 0"
    grid $f2.l3 -sticky w -padx "40 2"

    set f3 [ttk::labelframe $f.f3 -text [_ "database details"]]
    
    ttk::checkbutton $f3.cb1 -text [_ "Database name"]: -variable \
	[$w give_uservar has_database_name]
    cu::combobox $f3.cb2 -textvariable [$w give_uservar database_name] \
	-valuesvariable [$w give_uservar database_nameValues]
    
    set d [dict create no_database "-$f3.cb2" has_database "$f3.cb1" \
	    has_database_entries "$f3.cb1"]
    $w enable_disable_on_key form_type $d
    set d [dict create has_database has_database_name has_database_entries \
	    has_database_name]
    $w change_key_on_key form_type $d

    set d [dict create 0 "" 1 "$f3.cb2"]
    $w enable_disable_on_key has_database_name $d
    set d [dict create 1 [list database_name $database_default_table] \
	    0 [list database_name [_ "Automatic"]]]
    $w change_key_on_key has_database_name $d

    ttk::checkbutton $f3.cb3 -text [_ "Hide notes in view mode"] -variable \
	[$w give_uservar only_form_view_mode]

    tooltip $f3.cb3 [_ "If this option is selected, the notes part of the page is not visualized in view mode"]

    grid $f3.cb1 $f3.cb2 -sticky w -padx 2 -pady 2
    grid $f3.cb3 - -sticky w -padx 2 -pady 2
    grid configure $f3.cb2 -sticky ew -padx "0 40"
    grid columnconfigure $f3 1 -weight 1

    set f4 [ttk::labelframe $f.f4 -text [_ "display style"]]
    
    ttk::frame $f4.f1
    ttk::radiobutton $f4.f1.r1 -text [_ "List#C#Noun"] -variable [$w give_uservar tabstyle] \
	-value tree
    
    ttk::radiobutton $f4.f1.r2 -text [_ "Buttons"] -variable [$w give_uservar tabstyle] \
	-value arrows

    ttk::radiobutton $f4.f1.r3 -text [_ "Small buttons"] -variable [$w give_uservar tabstyle] \
	-value arrows_small

    ttk::radiobutton $f4.f1.r4 -text [_ "Notebook"] -variable [$w give_uservar tabstyle] \
	-value notebook
    
    grid $f4.f1.r1 $f4.f1.r2 $f4.f1.r3 $f4.f1.r4 -sticky w -padx 2
    
    ttk::label $f4.l1 -text [_ "Print formulas"]:
    
    set d [dict create \
	    no [_ "No"] \
	    yes [_ "Yes"] \
	    detailed_edit [_ "Detailed when edit"] \
	    detailed [_ "Detailed"] \
	    ]
    
    cu::combobox $f4.cb1 -textvariable [$w give_uservar print_formulas] \
	-dict $d -values [dict keys $d] -state readonly
    
    ttk::frame $f4.f2
    ttk::label $f4.f2.l1 -text [_ "Form use scrollbars"]:
    ttk::checkbutton $f4.f2.cb1 -text [_ "Horizontal"] -variable [$w give_uservar use_scrollbarsH]
    ttk::checkbutton $f4.f2.cb2 -text [_ "Vertical"] -variable [$w give_uservar use_scrollbarsV]
    
    grid $f4.f2.l1 $f4.f2.cb1 $f4.f2.cb2 -sticky w -padx 2  -pady 2

    grid $f4.f1 - -sticky w
    grid $f4.l1 $f4.cb1 -sticky w -pady 2
    grid $f4.f2 - -sticky w
	
    set f5 [ttk::labelframe $f.f5 -text [_ "print options"]]
    
    ttk::label $f5.l1 -text [_ "For first data set"]:
    ttk::checkbutton $f5.r1 -text [_ "Print detailed form"] \
	-variable [$w give_uservar print_first]
    
    grid $f5.l1 $f5.r1 - - -sticky w -padx 2 -pady "5 2"
    grid configure $f5.l1 -sticky e
    grid configure $f5.r1 -padx "20 2"
    
    ttk::label $f5.l2 -text [_ "For all data sets"]:
    ttk::radiobutton $f5.r2 -text [_ "Print detailed form"] \
	-variable [$w give_uservar print_all] -value form
    ttk::radiobutton $f5.r3 -text [_ "Print table"] \
	-variable [$w give_uservar print_all] -value table
    ttk::radiobutton $f5.r4 -text [_ "Do not print"] \
	-variable [$w give_uservar print_all] -value none
    
    set d [dict create form [list print_first 1]]
    $w change_key_on_key print_all $d
    set d [dict create none "$f5.l1 $f5.r1" table "$f5.l1 $f5.r1"]
    $w enable_disable_on_key print_all $d
    
    grid $f5.l2 $f5.r2 $f5.r3 $f5.r4 -sticky w -padx 2 -pady "5 2"
    #grid configure $f5.l2 -sticky e
    grid configure $f5.r2 -padx "20 2"
    
    grid $f1 -sticky nsew
    grid $f2 -sticky nsew
    grid $f3 -sticky nsew
    grid $f4 -sticky nsew
    grid $f5 -sticky nsew
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 0 -weight 1 -minsize 100

    set database_nameValues [list $database_default_table]
    
    if { $doc ne "" } {
	set title [string trim [$doc selectNodes string(formulae/title)]]
	$w set_uservar_value title $title
	set desc [string trim [$doc selectNodes string(formulae/description)]]
	$f1.t1 insert end $desc
	
	$w set_uservar_value help_lognoter [$doc selectNodes string(formulae/help_lognoter)]

	set formulaeNode [$doc selectNodes formulae]
	$w set_uservar_value only_form_view_mode [$formulaeNode @only_form_view_mode 0]

	set tabstyle [$formulaeNode @tabstyle ""]
	if { $tabstyle in "tree left_tree right_tree" } { set tabstyle tree }
	$w set_uservar_value tabstyle $tabstyle
	
	$w set_uservar_value print_formulas [$formulaeNode @print_formulas yes]
	
	$w set_uservar_value use_scrollbarsH [$formulaeNode @use_scrollbarsH 1]
	$w set_uservar_value use_scrollbarsV [$formulaeNode @use_scrollbarsV 1]
	
	$w set_uservar_value has_database_name 0

	if { [$formulaeNode @default_database 0] == 1 } {
	    if { [$formulaeNode @has_unique_name 0] != 1 } {
		$w set_uservar_value form_type has_database
	    } else {
		$w set_uservar_value form_type has_database_entries
	    }
	    $w set_uservar_value has_database_name 0
	} elseif { [$formulaeNode @database ""] eq "" } {
	    $w set_uservar_value form_type no_database
	    $w set_uservar_value has_database_name 0
	} else {
	    if { [$formulaeNode @has_unique_name 0] != 1 } {
		$w set_uservar_value form_type has_database
	    } else {
		$w set_uservar_value form_type has_database_entries
	    }
	    $w set_uservar_value has_database_name 1
	    $w set_uservar_value databasename [$formulaeNode @database]
	    lappend database_nameValues [$formulaeNode @database]
	}
	$w set_uservar_value print_first [$formulaeNode @print_first 1]
	$w set_uservar_value print_all [$formulaeNode @print_all form]
    } else {
	$w set_uservar_value title ""
	$w set_uservar_value help_lognoter ""
	$w set_uservar_value only_form_view_mode 0
	$w set_uservar_value tabstyle arrows
	$w set_uservar_value print_formulas yes
	$w set_uservar_value use_scrollbarsH 1
	$w set_uservar_value use_scrollbarsV 1
	$w set_uservar_value form_type no_database
	$w set_uservar_value has_database_name 0
	$w set_uservar_value print_first 1
	$w set_uservar_value print_all form
    }
    $w set_uservar_value database_nameValues $database_nameValues

    tk::TabToWindow $f1.e1
    bind $w <Return> [list $w invokeok]
    
    $w createwindow

}

proc formulae::form_properties_do { key update_callback w } {
    variable values
    
    set doc [dict get $values $key doc]
    set lognoter_db [dict get $values $key lognoter_db]
    set database_default_table [dict get $values $key database_default_table]
    
    set action [$w giveaction]

    switch -- $action {
	1 {
	    if { $doc eq "" } {
		set doc [dom createDocument formulae]
		dict set values $key doc $doc
	    }
	    set formulaeNode [$doc selectNodes formulae]

	    switch [$w give_uservar_value form_type] {
		no_database {
		    lassign [list 0 0 "" 0] hasdatabase hasdatabasename \
		        databasename has_unique_name
		}
		has_database -
		has_database_entries {
		    set hasdatabase 1
		    set hasdatabasename [$w give_uservar_value has_database_name]
		    if { !$hasdatabasename } {
		        set databasename ""
		    } else {
		        set databasename [$w give_uservar_value databasename]
		    }
		    switch [$w give_uservar_value form_type] {
		        has_database { set has_unique_name 0 }
		        has_database_entries { set has_unique_name 1 }
		    }
		}
	    } 
	    set err [catch { create_update_database -databasename $databasename \
		        -force_update 0 $key $hasdatabase $hasdatabasename \
		        $has_unique_name } ret]
	    if { $err } {
		if { $ret ne "" } {
		    snit_messageBox -icon error -message $ret
		}
		return
	    }
	    
	    set title [string trim [$w give_uservar_value title]]
	    if { $title eq "" } {
		snit_messageBox -icon error -message \
		    [_ "It is necessary to enter a title"]
		return
	    }
	    set titleNode [$formulaeNode selectNodes title]
	    if { $titleNode ne "" } {
		foreach i [$titleNode childNodes] { $i delete }
	    } else {
		set titleNode [$doc createElement title]
		set firstChild [$formulaeNode selectNodes {./*[1]}]
		if { $firstChild ne "" } {
		    $formulaeNode insertBefore $titleNode $firstChild
		} else {
		    $formulaeNode appendChild $titleNode
		}
	    }
	    $titleNode appendChildText $title
	    
	    set help_lognoter [$w give_uservar_value help_lognoter]
	    if { $help_lognoter ne "" } {
		$formulaeNode setAttribute help_lognoter $help_lognoter
	    } elseif { [$formulaeNode hasAttribute help_lognoter] } {
		$formulaeNode removeAttribute help_lognoter
	    }
	    $formulaeNode setAttribute usableviewmode 1
	    $formulaeNode setAttribute only_form_view_mode [$w give_uservar_value \
		    only_form_view_mode]
	    
	    set tabstyle [$w give_uservar_value tabstyle]
	    $formulaeNode setAttribute tabstyle $tabstyle
	    $formulaeNode setAttribute print_formulas [$w give_uservar_value print_formulas]
	    
	    $formulaeNode setAttribute use_scrollbarsH [$w give_uservar_value use_scrollbarsH]
	    $formulaeNode setAttribute use_scrollbarsV [$w give_uservar_value use_scrollbarsV]
	    
	    set desc_widget [$w give_uservar_value desc_widget]
	    set description [string trim [$desc_widget get 1.0 end-1c]]
	    set descNode [$formulaeNode selectNodes description]
	    if { $descNode ne "" } {
		foreach i [$descNode childNodes] { $i delete }
	    } else {
		set descNode [$doc createElement description]
		set secondChild [$formulaeNode selectNodes {./*[2]}]
		if { $secondChild ne "" } {
		    $formulaeNode insertBefore $descNode $secondChild
		} else {
		    $formulaeNode appendChild $descNode
		}
	    }
	    $descNode appendChildText $description
	    
	    $formulaeNode setAttribute print_first [$w give_uservar_value print_first]
	    $formulaeNode setAttribute print_all [$w give_uservar_value print_all]

	    set setnameNode [$formulaeNode selectNodes setname]
	    if { $setnameNode ne "" } {
		$setnameNode delete
	    }

	    if { $update_callback ne "" } {
		uplevel #0 $update_callback
	    } else {
		create_windowD_do $key
	    }
	}
    }
    destroy $w
}

proc formulae::get_table_columns { node } {
    if { [$node @columns ""] ne "" } {
	return [$node @columns]
    }
    set columns ""
    foreach colNode [$node selectNodes {columns/column}] {
	if {[$colNode @pn ""] ne ""} {
	    set name [$colNode @pn]
	} else {
	    set name [$colNode @n]
	}
	set d ""
	foreach n [$colNode attributes] {
	    if { $n eq "n" } { continue }
	    dict set d $n [$colNode @$n]
	}
	lappend columns [list $name $d]
    }
    return $columns
}

proc formulae::contextual_menu_table { key field_name node wp what } {
    variable values
    variable uvalues
    
    set wtable [dict get $values $key wtables $node]
 
    switch $what {
	copy {
	    clipboard clear  
	    set dataList ""
	    set columns ""
	    
	    foreach i [get_table_columns $node] {  
		lassign $i name -                       
		lappend columns $name
	    }                          
	    lappend dataList [join $columns "\t"]           
	    foreach item [$wtable item children 0] {
		set colList ""
		foreach col [$wtable item text $item] {
		    lappend colList [string trim $col]
		}
		lappend dataList [join $colList "\t"]
	    }
	    clipboard append [join $dataList "\n"]   
	}        
	filesave {
	    set lognoter_db [dict get $values $key lognoter_db]
	    set path [dict_getd $values $key uvalues_moredata [$node @n] path ""]
	    if { $path ne "" && [file isdirectory [file dirname $path]] } {
		set lastdir [file dirname $path]
	    } else {
		set lastdir [$lognoter_db getpreference lastdir]
	    }          
	    set file [tk_getSaveFile -initialdir $lastdir -filetypes \
		    [list [list [_ "CSV files"] {.csv}] \
		        [list [_ "All files"] *]] -defaultextension .csv \
		    -title [_ "Export to CSV"] -parent $wp]            
	    if { $file eq "" } { return }                                            
	    set columns ""
	    foreach i [get_table_columns $node] {
		lassign $i name dict
		lappend columns $name                          
	    } 
	    set fout [open $file w]
	    puts $fout [cu::string::csv_join $columns ";"]  
	    foreach item [$wtable item children 0] {
		set colList ""
		foreach col [$wtable item text $item] {
		    lappend colList [string trim $col]
		}
		puts $fout [cu::string::csv_join $colList ";"]
	    }
	    close $fout                                            
	}
	importfile {
	    set lognoter_db [dict get $values $key lognoter_db]
	    set path [dict_getd $values $key uvalues_moredata [$node @n] path ""]
	    if { $path ne "" && [file isdirectory [file dirname $path]] } {
		set lastdir [file dirname $path]
	    } else {
		set lastdir [$lognoter_db getpreference lastdir]
	    }
	    set file [tk_getOpenFile -initialdir $lastdir -filetypes \
		    [list [list [_ "CSV files"] {.csv}] \
		        [list [_ "All files"] *]] -defaultextension .csv \
		    -initialdir $lastdir -title [_ "Import from CSV file"] -parent $wp]       
	    if { $file eq "" } { return }  
	    
	    set fin [open $file r]                       
	    package require csv      
	    formulae::widget_table_ops $key $node $wtable import - $fin 
	    close $fin              
	}        
	select_all {          
	    $wtable selection add all
	}       
	add_item {
	    add_edit_row add $wp $key $node $wtable ""
	}
	edit_item {
	    if { [llength [$wtable selection get]] != 1 } {
		tk_messageBox -message [_ "It is necessary to select one entry in order to edit it"] -parent $wp 
		return
	    }
	    add_edit_row edit $wp $key $node $wtable [lindex [$wtable selection get] 0]
	}
	delete_item {
	    set items [$wtable selection get]          
	    if {[llength $items] == 0 } {                            
		tk_messageBox -message \
		    [_ "It is necessary to select one or more entries in order to delete them"] -parent $wp 
		return
	    } 
	    foreach item $items {
		widget_table_ops $key $node $wtable delete - $item
	    }
	}       
    }        
}

proc formulae::add_edit_row { what wp key node wtable item } {
    variable values
    
    if { $wp eq "." } {
	set w .addeditrow
    } else {
	set w $wp.addeditrow
    }
    destroy $w
    package require dialogwin
    
    switch $what {
	add {
	    set title [_ "Add row"]
	}
	edit {
	    set title [_ "Edit row"]
	    set vList [widget_table_ops $key $node $wtable get - $item]
	}
    }

    set lognoter_db [dict get $values $key lognoter_db]
    set lognoter [dict_getd $values $key lognoter ""]

    dialogwin_snit $w -title $title \
	-morebuttons [list [_ "Apply"]] -grab 0 -transient 1 -callback \
	[list formulae::add_edit_row_do $what $key $node $wtable $item]
    
    set xml [format_xml {
	    <lognoter>
	    <formulae usableviewmode='1' tabstyle='' updatebutton='0'>
	    <container n="row_values" pn="%s">
	} [_ "Row values"]]

    set ipos 0
    foreach i [get_table_columns $node] {
	lassign $i name dict
	append xml [format_xml {<param n="%s"} $name]
	dict for "n v" $dict {
	    if { $n in "len justify expand value" } { continue }
	    if { $n eq "readonly" } {
		set n "condition"
		if { $v } {
		    set v "disabled"
		} else {
		    set v ""
		}
	    }
	    append xml [format_xml { %s="%s"} $n $v]
	}
	if { [info exists vList] } {
	    set value [lindex $vList $ipos]
	} else {
	    set value [dict_getd $dict value ""]
	}
	append xml [format_xml { value="%s"/>} $value]
	incr ipos
    }
    append xml "</container>"
    append xml "</formulae>"
    append xml [format_xml {<tcl type="substitute">%s</tcl>} [dict_getd $values $key page_tcl_code ""]]
    append xml "</lognoter>"
    
    set doc [dom parse $xml]
    set formulaeNode [$doc selectNodes {/*/formulae}]
    
    set f [$w giveframe]
    set key_in $w
    set f_in [formulae::create_windowD -state disabled \
	    -lognoter_db $lognoter_db \
	    -lognoter $lognoter \
	    $f $key_in $formulaeNode]
    grid $f_in -sticky nsew
    grid rowconfigure $f 0 -weight 1
    grid columnconfigure $f 0 -weight 1
    
    bind $w <Return> [list $w invokeok]
    set action [$w createwindow]
    $doc delete
}

proc formulae::add_edit_row_do { what key node wtable item w } {
    
    switch -- [$w giveaction] {
	-1 - 0 { destroy $w }
	1 - 2 {
	    set key_in $w
	    set d [formulae::give_params_dict $key_in]
	    
	    set list ""
	    foreach i [get_table_columns $node] {
		lassign $i name dict
		lappend list [dict get $d $name]
	    }
	    switch $what {
		add { widget_table_ops $key $node $wtable add - $list }
		edit { widget_table_ops $key $node $wtable edit - $item $list }
	    }
	    destroy $w
	}
    }
}

# proc formulae::add_edit_item { args } {
#     variable values
# 
#     set optional {
#         { -tool tool add}
#     }
#     set compulsory "key node wtable wp"
#     
#     parse_args $optional $compulsory $args   
#     catch { destroy $wp._cmenu }
#     
#     set columns_names ""
#     set rowList ""
#     foreach icol [$node @columns] {  
#         lappend columns_names [lindex $icol 0]
#         lappend rowList ""
#     }   
#     if {$tool eq "edit"} {    
#         set sel [$wtable selection get]  
#         if {$sel == ""} {
#             snit_messageBox -message \
#                 [_ "It is necessary to select one entry in order to delete it"] \
#                 -parent $wp
#             return
#         }               
#         set icount 0
#         foreach wcol [$wtable item text $sel] icol $columns_names {
#             lassign $icol col type val  
#             switch -- $type {               
#                 entry - file - checkbutton {                                        
#                     lset columns_names $icount 2 [string trim $wcol]
#                 }
#             }
#             incr icount
#         }    
#     }     
# 
#     formulae::widget_table_ops $key $node $wtable $tool - $rowList     
# }

proc formulae::GetOpenFile { w icount } {  
    if {[$w give_uservar_value f$icount] != ""} {
	set dir [file dirname [$w give_uservar_value f$icount]]
    } else {            
	set dir [pwd] 
    }
  
    set exts [list [_ "All files"] *] 
    set exts [list [list [_ "All files"] [list "*"]]]   
    set file [tk_getOpenFile -defaultextension .txt -filetypes $exts \
	    -initialdir $dir -title [_ "Load file"] -parent $w]
    if { $file eq "" } { return }
    $w set_uservar_value f$icount $file      
}

proc formulae::import_other_forms { args } {
    variable values

    set optional {
	{ -page page "" }
    }
    set compulsory "key wp"
    
    parse_args $optional $compulsory $args

    set doc [dict get $values $key doc]
    set database_default_table [dict get $values $key database_default_table]

    destroy $wp._import
    set w [dialogwin_snit $wp._import -title [_ "Import from other forms"] \
	    -callback [namespace code [list import_other_forms_do $key]] -grab 1 \
	    -transient 1]

    set f [$w giveframe]

    set f1 [ttk::labelframe $f.f1 -text [_ Import]]
    ttk::label $f1.l1 -text [_ "Select form"]:
    cu::combobox_tree $f1.cb1 -width 50 -textvariable [$w give_uservar page] \
	-state readonly
    bind $f1.cb1 <<ComboboxSelected>> [list formulae::import_other_forms_update \
	    $w $key post_page]
    $w set_uservar_value w_page $f1.cb1
    
    ttk::label $f1.l2 -text [_ "Entry"]:
    cu::combobox $f1.cb2 -width 50 -textvariable [$w give_uservar entry] \
	-state readonly -valuesvariable [$w give_uservar entry_list]
    $w set_uservar_value w_entry $f1.cb2
    
    grid $f1.l1 $f1.cb1 -sticky w -padx 2 -pady 2
    grid $f1.l2 $f1.cb2 -sticky w -padx 2 -pady 2
    grid configure $f1.cb1 $f1.cb2 -sticky ew
    grid columnconfigure $f1 1 -weight 1
    
    grid $f1 -sticky nsew
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 0 -weight 1

    bind $w <Return> [list $w invokeok]
    
    if { $page ne "" } {
	$w set_uservar_value page $page
	tk::TabToWindow $f1.cb2
    } else {
	$w set_uservar_value page ""
	tk::TabToWindow $f1.cb1
    }
    import_other_forms_update $w $key init
    $w createwindow
}

proc formulae::import_other_forms_do { key w } {
    variable values
    variable uvalues
    
    set action [$w giveaction]

    switch -- $action {
	-1 - 0 {
	    destroy $w
	}
	1 {
	    set page [$w give_uservar_value page]
	    if { $page eq "" } {
		snit_messageBox -message [_ "Page is not a correct form"] \
		    -parent $w
		return
	    }
	    set entry [$w give_uservar_value entry]
	    if { $entry eq "" } {
		snit_messageBox -message [_ "Entry is not correct"] \
		    -parent $w
		return
	    }
	    set lognoter_db [dict get $values $key lognoter_db]
	    set table [$w give_uservar_value table]
	    set column_names [$lognoter_db sql column_names $table]
	    set id_name [$lognoter_db sql escape $entry]
	    set vs [$lognoter_db sql sel "select * from \"$table\" where id_name='$id_name'"]
	    set d ""
	    foreach c $column_names v $vs {
		dict set d $c $v
	    }
	    foreach n [array names uvalues $key,*] {
		set v $uvalues($n)
		regexp {,(.*)} $n {} np
		if { [dict exists $d $np] } {
		    set err [catch { format %.3g [dict get $d $np] } value]
		    if { $err } { set value [dict get $d $np] }
		    set uvalues($n) $value
		}
	    }
	    set notebook_widget [dict get $values $key notebook_widget]
	    set id_name_in [$notebook_widget id_name]
	    set id_values [$notebook_widget id_values]
	    if { $id_name_in eq "" } {
		$notebook_widget id_name $id_name
	    }
	    $notebook_widget id_values [linsert0 $id_values $id_name]
	    
	    set doc [dict get $values $key doc]
	    set formulaeNode [$doc selectNodes formulae]
	    set pages ""
	    foreach n [$formulaeNode selectNodes import_pages] {
		lappend pages [$n text]
		$n delete
	    }
	    set ipos [lsearch -exact $pages $page]
	    if { $ipos != -1 } {
		set pages [lreplace $pages $ipos $ipos]
	    }
	    set pages [linsert $pages 0 $page]
	    foreach p $pages {
		$formulaeNode appendChildTag import_pages [list text() $p]
	    }
	    window_actualize $key
	    destroy $w
	}
    }
}

proc formulae::_import_other_forms_fill_pages_list { combo key id } {
    variable values
    
    set lognoter [dict_getd $values $key lognoter ""]
    if { $lognoter eq "" } {
	return
    }
    set len [llength [$combo tree_item children $id]]
    if { !$len } {
	$lognoter giveinfo linklist_combobox_tree $combo local $id
	$combo tree_item collapse all
	$combo tree_item expand $id
    }
}

proc formulae::_import_other_forms_clear_from_list { w key page id } {
    variable values
    
    set doc [dict get $values $key doc]
    set formulaeNode [$doc selectNodes formulae]
    foreach n [$formulaeNode selectNodes import_pages] {
	if { [$n text] eq $page } {
	    $n delete
	    break
	}
    }
    import_other_forms_update $w $key updateall
}

proc formulae::import_other_forms_update { w key what } {
    variable values
    
    set combo [$w give_uservar_value w_page]
    set combo_entry [$w give_uservar_value w_entry]

    if { $what in "init updateall" } {
	$combo clear
	set doc [dict get $values $key doc]
	set formulaeNode [$doc selectNodes formulae]
	
	set pages ""
	foreach n [$formulaeNode selectNodes import_pages] {
	    lappend pages [$n text]
	}
	foreach page $pages {
	    $combo tree_insert end $page $page 0
	    if { [$w give_uservar_value page] eq "" } {
		$w set_uservar_value page $page
	    }
	}
	set cmd [list formulae::_import_other_forms_fill_pages_list $combo $key]
	set id [$combo tree_insert -command $cmd -active 0 end [_ "Select page"]... mu 0]
	$combo tree_item state set $id emphasis
	if { [dict get $values $key state] eq "normal" && [llength $pages] } {
	    set id [$combo tree_insert -active 0 end [_ "Clear from list"]... mu 0]
	    $combo tree_item state set $id emphasis
	    foreach page $pages {
		set cmd [list formulae::_import_other_forms_clear_from_list $w $key $page]
		$combo tree_insert -command $cmd -active 0 end $page $page $id
	    }
	}
	$combo tree_item collapse all
	
	$w set_uservar_value entry ""
	$combo_entry state disabled
    }
    set page [$w give_uservar_value page]
    if { $page ne "" } {
	set err [catch { _get_database_from_page $key $page } table]
	if { $err } {
	    if { $what ne "init" } {
		snit_messageBox -message [_ "Page '%s' is not a correct form" $page] \
		    -parent $w
	    }
	    $w set_uservar_value page ""
	    set page ""
	}
    }
    if { $page ne "" } {
	$combo_entry state !disabled
	set lognoter_db [dict get $values $key lognoter_db]
	set entry_list [$lognoter_db sql sel "select id_name from \"$table\" order by id"]
	$w set_uservar_value entry_list $entry_list
	$w set_uservar_value entry [lindex $entry_list 0]
	$w set_uservar_value table $table
    }
}

proc formulae::_get_database_from_page { key page } {
    variable values
    
    set lognoter_db [dict get $values $key lognoter_db]
    lassign [$lognoter_db givepage $page] - - - data
    dom parse $data doc
    set root [$doc documentElement]
    set formulaeNode [$doc selectNodes //formulae]
    if { [$formulaeNode @has_unique_name] == 0 } {
	error "error database has not an unique name"
    }
    return [$formulaeNode @database]
}

proc formulae::create_update_database { args } {
    variable values
    
    set optional {
	{ -databasename name "" }
	{ -force_update boolean 1 }
    }
    set compulsory "key hasdatabase hasdatabasename has_unique_name"
    
    parse_args $optional $compulsory $args

    set doc [dict get $values $key doc]
    set formulaeNode [$doc selectNodes formulae]
    set lognoter_db [dict get $values $key lognoter_db]
    set database_default_table [dict get $values $key database_default_table]
    
    set needs_update $force_update
    
    if { $hasdatabase } {
	if { !$hasdatabasename } {
	    set databasename $database_default_table
	    if { [$formulaeNode @default_database 0] == 0 } {
		set needs_update 1
	    }
	    if { [$formulaeNode @database ""] ne $databasename } {
		set needs_update 1
	    }
	} else {
	    set databasename [string trim $databasename]
	    if { $databasename eq "" } {
		error [_ "It is necessary to enter a database name"]
	    }
	    if { ![regexp {^[\w-]+$} $databasename] } {
		error [_ "Database name must only contain letters, numbers -_"]
	    }
	    if { [$formulaeNode @default_database 0] == 1 } {
		set needs_update 1
	    }
	    if { [$formulaeNode @database ""] ne $databasename } {
		set needs_update 1
	    }
	}
	if { [$formulaeNode @has_unique_name ""] ne $has_unique_name } {
	    set needs_update 1
	}
	if { 1 } {
	    if { $hasdatabasename && ![string equal -nocase [$formulaeNode @database ""] \
		$databasename] } {
		if { [$lognoter_db sql table_exists $databasename] } {
		    set ret [snit_messageBox -type okcancel -message \
		            [_ "Do you want to delete existing table '%s'?" \
		                $databasename]]
		    if { $ret eq "cancel" } { error "" }
		}
	    }
	    set database_save [$formulaeNode @database ""]
	    set default_database_save [$formulaeNode @default_database ""]
	    set has_unique_name_save [$formulaeNode @has_unique_name ""]
	    if { $hasdatabasename } {
		$formulaeNode setAttribute default_database 0
	    } else {
		$formulaeNode setAttribute default_database 1
	    }
	    set old_database [$formulaeNode @database ""]
	    $formulaeNode setAttribute database $databasename
	    $formulaeNode setAttribute has_unique_name $has_unique_name
	    
	    set err [catch { create_update_database_table -old_database $old_database $key } \
		    errstring]
	    if { $err } {
		if { $default_database_save ne "" } {
		    $formulaeNode setAttribute default_database $default_database_save
		} else {
		    $formulaeNode removeAttribute default_database
		}
		if { $has_unique_name_save ne "" } {
		    $formulaeNode setAttribute has_unique_name $has_unique_name_save
		} else {
		    $formulaeNode removeAttribute has_unique_name
		}
		$formulaeNode setAttribute database $database_save
		
		error $errstring
	    }
	}
    }  elseif { [$formulaeNode @database ""] ne "" } {
	set ret [snit_messageBox -type yesnocancel -message \
		[_ "Do you want to delete database '%s'?" \
		    [$formulaeNode @database]]]
	if { $ret eq "cancel" } { error "" }
	if { $ret eq "yes" } {
	    set table_fts [give_table_fts_name [$formulaeNode @database]]
	    $lognoter_db sql exec "drop table if exists [$lognoter_db sql fs0 $table_fts]"
	    $lognoter_db sql exec "drop table [$lognoter_db sql fs0 [$formulaeNode @database]]"
	}
	catch { $formulaeNode removeAttribute default_database }
	catch { $formulaeNode removeAttribute has_unique_name }
	$formulaeNode removeAttribute database
    }
}

proc formulae::give_table_fts_name { table } {
    if { ![regsub {_page$} $table {_fts} table_fts] } {
	set table_fts ${table}_fts
    }
    return $table_fts
}

proc formulae::fill_database_values_from_internal { db_default key } {
    variable values
    
    set doc [dict get $values $key doc]
    set formulaeNode [$doc selectNodes formulae]
    set lognoter_db [dict get $values $key lognoter_db]
    fill_database_values_from_internal_do $db_default $formulaeNode $lognoter_db
}

proc formulae::fill_database_values_from_internal_do { db_default root lognoter_db } {
    
    set formulaeNode [$root selectNodes ".//formulae"]
    if { [llength $formulaeNode] != 1 } { return }
    
    if { [$formulaeNode @default_database 0] == 1 && $db_default ne "" } {
	set table $db_default
	$formulaeNode setAttribute database $table
    } else {
	set table [$formulaeNode @database ""]
    }
    if { $table eq "" } { return }
    
    set databaseNodes [$formulaeNode selectNodes "database"]
  
    if { ![llength $databaseNodes] } {
	if { ![$lognoter_db sql table_exists $table] } {
	    lassign [give_create_statement $formulaeNode $lognoter_db] - table_schema
	    $lognoter_db sql exec $table_schema
	}
    }
    foreach node $databaseNodes {
	if { [$lognoter_db sql table_exists $table] } {
	    set table_fts [give_table_fts_name $table]
	    $lognoter_db sql exec "drop table if exists [$lognoter_db sql fs0 $table_fts]"
	    $lognoter_db sql exec "drop table [$lognoter_db sql fs0 $table]"
	}
#         set table_schema [$node selectNodes string(schema)]
#         if { $table_schema eq "" } {
#             lassign [give_create_statement $formulaeNode $lognoter_db] - table_schema
#         }
	lassign [give_create_statement $formulaeNode $lognoter_db] - table_schema
	$lognoter_db sql exec $table_schema
	set column_types [$lognoter_db sql column_types $table]
	foreach rowNode [$node selectNodes row] {
	    set valuesList ""
	    set idx 0
	    foreach entryNode [$rowNode selectNodes entry] {
		if { [string match *blob* [lindex $column_types $idx]] } {
		    set data [base64::decode [$entryNode text]]
		} else {
		    set data [$entryNode text]
		}
		lappend valuesList [$lognoter_db sql escape $data]
		incr idx
	    }
	    set cmd "insert into[$lognoter_db sql fs0 $table] values("
	    append cmd "'[join $valuesList "','"]')"
	    $lognoter_db sql exec $cmd
	}
	$node delete
	break
    }
}

proc formulae::add_database_values_to_xml { root lognoter_db } {
    
    foreach formulaeNode [$root selectNodes {.//formulae}] {
	set table [$formulaeNode @database ""]
	if { $table ne "" } {
	    set table_schema [$lognoter_db sql table_schema $table]
	    set _ "<database><schema>"
	    append _ [xml $table_schema] "</schema>"
	    set __column_names [$lognoter_db sql column_names $table]
	    set __column_types [$lognoter_db sql column_types $table]
	    $lognoter_db sql maplist "select [$lognoter_db sql fs $__column_names] from [$lognoter_db sql fs0 $table]" {
		append _ "<row>"
		set __idx 0
		foreach __i $__column_names {
		    if { [string match *blob* [lindex $__column_types $__idx]] } {
		        set __data [base64::encode [set $__i]]
		    } else {
		        set __data [set $__i]
		    }
		    append _ "<entry>[xml $__data]</entry>"
		    incr __idx
		}
		append _ "</row>"
	    }
	    append _ "</database>"
	    $formulaeNode appendXML $_
	}
    }
}

proc formulae::search_in_database { key searchstring } {
    variable values
    
    set notebook_widget [dict get $values $key notebook_widget]
    $notebook_widget manage_database update -searchstring_any $searchstring
}

proc formulae::give_last_modification_date { formulae_table wdb } {
    set cmd "select max(modification_date) from [$wdb sql fs0 $formulae_table]"
    set err [catch { $wdb sql onecolumn $cmd } ret]
    if { $err } { set ret "" }
    return $ret
}

proc formulae::copy_to_another_lognoter { args } {
    
    set optional {
	{ -language language - }
    }
    set compulsory "root page_to wdb_from wdb_to"
    parse_args $optional $compulsory $args

    set need_page_update 1
    foreach formulaeNode [$root selectNodes {.//formulae}] {
	if { [$formulaeNode @database ""] ne "" } {
	    set table_from [$formulaeNode @database]
	    if { ![$wdb_from sql table_exists $table_from] } {
		$formulaeNode setAttribute database ""
		set need_page_update 1
		continue
	    }
	    set table_to [$formulaeNode @database]
	    if { [$formulaeNode @default_database 0] == 1 } {
		set default_db [$wdb_to default_table_for_page \
		        -language $language $page_to]
		if { $default_db ne $table_to } {
		    $formulaeNode setAttribute database $default_db
		    set table_to $default_db
		    set need_page_update 1
		}
	    }
	    lassign [formulae::give_create_statement $formulaeNode $wdb_to] names cmd
	    if { [$wdb_to sql table_exists $table_to] } {
		set table_fts [give_table_fts_name $table_to]
		$wdb_to sql exec "drop table if exists [$wdb_to sql fs0 $table_fts]"
		$wdb_to sql exec "drop table [$wdb_to sql fs0 $table_to]"
	    }
	    $wdb_to sql exec $cmd
	    
	    $wdb_to begin_transaction
	    set __column_names [$wdb_from sql column_names $table_from]
	    set __cmd "insert into [$wdb_to sql fs0 $table_to] ([$wdb_to sql fs $__column_names]) values ("
	    set idx 0
	    foreach n $__column_names {
		if { $idx > 0 } { append __cmd "," }
		append __cmd "'\[$wdb_to sql escape \[set [list $n]\]\]'"
		incr idx
	    }
	    append __cmd ")"
	    set icounter 0
	    $wdb_from sql maplist "select [$wdb_from sql fs $__column_names] from [$wdb_from sql fs0 $table_from]" {
		if { "id_name" in $__column_names && $id_name eq "" } {
		    set id_name [_ "Name%d"  [incr icounter]]
		    set cmd "select 1 from [$wdb_to sql fs0 $table_to] where id_name=[$wdb_to sql fs0 $id_name]"
		    while { [$wdb_to sql onecolumn $cmd] ne "" } {
		        set id_name [_ "Name%d"  [incr icounter]]
		    }
		}
		$wdb_to sql exec [subst $__cmd]
	    }
	    $wdb_to commit_transaction
	}
    }
    return $need_page_update
}

proc formulae::select_one_row { wp txt table lognoter_db } {
	
    set ret [$lognoter_db sql sel "select id from [$lognoter_db sql fs0 $table]"]
    if { [llength $ret] < 2 } {
	return [list $table $ret]
    }
    set w $wp._ask
    destroy $w
    dialogwin_snit $w -title $txt -entrytext $txt: 
    set f [$w giveframe]

    set columns ""
    lappend columns [list 6 id left text 1]
    set cols [$lognoter_db sql column_names $table]
    foreach col [lrange $cols 3 end] {
	if { $col eq "id_name" } { set col [_ "Name id"] }
	if { [regexp {__(contents|cdate|mdate|type|size|path)$} $col] } {
	    continue
	}
	lappend columns [list 14 $col left text 1]
    }
    package require fulltktree
    fulltktree $f.list -columns $columns -showbuttons 0 -showlines 0 \
	-width 400 -bd 2 -relief raised -sensitive_cols all \
	-selecthandler2 "[list $w invokeok];#" \
	-selectmode browse
    
    set cols [$lognoter_db sql column_names $table]
    set __selcols [lrange $cols 0 2]
    foreach col [lrange $cols 3 end] {
	if { ![regexp {(.*)__(.*)} $col {} name subname] } {
	    lappend __selcols $col
	}
    }
    set selcols_p "\"[join $__selcols \",\"]\""
    
    
    set cmd "select $selcols_p from \"$table\""
    $lognoter_db sql maplist $cmd {
	set __v [list $id]
	foreach __i [lrange $__selcols 3 end] {
	    lappend __v [set $__i]
	}
	$f.list insert end $__v
    }
    catch { $f.list selection add 1 }
    
    grid $f.list -sticky nsew
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 1

    tk::TabToWindow $f.list
    bind $w <Return> [list $w invokeok]

    set action [$w createwindow]
    set item [$f.list selection get]
    if { [llength $item] == 1 } {
	set ret [list $table [$f.list item text $item 0]]
    } else {
	error ""
    }
    destroy $w
    if { $action <= 0 } { error "" }
    return $ret
}

proc formulae::give_database_name { args } {
    
    set optional {
	{ -language language - }
    }
    set compulsory "root page lognoter_db"
    parse_args $optional $compulsory $args

    
    set formulaeNode [$root selectNodes {.//formulae[1]}]
    if { $formulaeNode eq "" } { return "" }
    
    if { [$formulaeNode @default_database 0] == 1 } {
	return [$lognoter_db default_table_for_page  -language $language $page]
    }
    return [$formulaeNode @database ""]
}

proc formulae::update_database_name { args } {

    set optional {
	{ -keep_old_database boolean 0 }
    }
    set compulsory "db_default root lognoter_db"
    parse_args $optional $compulsory $args

    
    set formulaeNode [$root selectNodes {.//formulae[1]}]
    if { $formulaeNode eq "" } { return 0 }
    
    if { [$formulaeNode @default_database 0] != 1 } { return 0 }
    if { [$formulaeNode @database] eq $db_default } { return 0 }
    
    set old_database [$formulaeNode @database ""]
    $formulaeNode setAttribute database $db_default
    create_update_database_table_do -old_database $old_database \
	-keep_old_database $keep_old_database $lognoter_db $formulaeNode
    return 1
}

proc formulae::delete_database { args } {
    
    set optional {
	{ -parent w "" }
	{ -require_confirm_for_default boolean 1 }
	{ -page name "" }
    }
    set compulsory "root lognoter_db"
    parse_args $optional $compulsory $args
    
    set formulaeNode [$root selectNodes {.//formulae[1]}]
    if { $formulaeNode eq "" } { return 0 }
    if { [$formulaeNode @database ""] eq "" } { return 0 }
    
    if { ![$lognoter_db sql table_exists  [$formulaeNode @database]] } {
	return 0
    }
    set deleted_internal 0
    if { [$formulaeNode @default_database 0] == 1 } {
	if { $require_confirm_for_default } {
	    if { $page ne "" } {
		set text [_ "Are you sure to delete internal database for page '%s'?" $page]
	    } else {
		set text [_ "Are you sure to delete internal database for current page?"] 
	    }
	    set retval [snit_messageBox -default ok -icon question -message $text \
		    -type okcancel -parent $parent]
	    if { $retval eq "cancel" } {
		return -1
	    }
	}
	set deleted_internal 1
    } else {
	if { $page ne "" } {
	    set text [_ "Do you want to delete database '%s' associated to page '%s'?" \
		    [$formulaeNode @database] $page]
	} else {
	    set text [_ "Do you want to delete database '%s' associated to current page?" \
		    [$formulaeNode @database]]
	}
	set retval [snit_messageBox -default no -icon question -message $text \
		-type yesnocancel -parent $parent]
	if { $retval eq "cancel" } {
	    return -1
	} elseif { $retval eq "no" } {
	    return 0
	}
    }
    set table_fts [give_table_fts_name [$formulaeNode @database]]
    $lognoter_db sql exec "drop table if exists [$lognoter_db sql fs0 $table_fts]"
    $lognoter_db sql exec "drop table [$lognoter_db sql fs0 [$formulaeNode @database]]"
    
    if { $deleted_internal } {
	return 2
    } else {
	return 1
    }
}

proc formulae::give_create_statement { args } {

    set optional {
	{ -temporal_table_name name "" }
    }
    set compulsory "formulaeNode lognoter_db"
    parse_args $optional $compulsory $args

    set names ""
    set types ""
    set defaults ""
    
    if { [$formulaeNode @has_unique_name 0] == 1 } {
	lappend names id_name
	lappend types "varchar(255) unique"
	lappend defaults ""
    }
    foreach paramNode [$formulaeNode selectNodes //param] {
	if { [$paramNode @in_database 1] == 0 } { continue }
	set field_type [$paramNode @field_type ""]
	if { $field_type eq "button" } { continue }

	lappend names [$paramNode @n]

	if { $field_type eq "file" || $field_type eq "directory" } {
	    lappend names [$paramNode @n]__contents [$paramNode @n]__cdate \
		[$paramNode @n]__mdate [$paramNode @n]__type [$paramNode @n]__size \
		[$paramNode @n]__path
	    lappend types text longblob datetime datetime text numeric text
	    lappend defaults "" "" "" "" "" ""
	} elseif { [lsearch "numeric expression" $field_type] != -1 || \
	    [$paramNode @units ""] ne "" || [string is double -strict [$paramNode @value]] ||
	    [regexp {^\[e} [$paramNode @value]] } {
	    # type numeric gives problems
	    lappend types text
	} else {
	    lappend types text
	}
	if { [regexp {^\[} [$paramNode @value]] } {
	    lappend defaults 0.0
	} else {
	    lappend defaults ""
	}
    }
    set cmdList ""
    foreach name $names type $types def $defaults {
	if { [string is double -strict $def]  && $def > 1e-100 && $def < 1e100 } {
	    lappend cmdList "[$lognoter_db sql fs0 $name] $type default $def"
	} else {
	    lappend cmdList "[$lognoter_db sql fs0 $name] $type"
	}
    }
    if { [$lognoter_db cget -dbtype] eq "mysql" } {
	set id_type "integer unsigned auto_increment primary key"
    } else {
	set id_type "integer primary key"
    }
    if { $temporal_table_name ne "" } {
	set cmd "create temporary table [$lognoter_db sql fs0 $temporal_table_name]("
    } else {
	set cmd "create table [$lognoter_db sql fs0 [$formulaeNode @database]]("
    }
    append cmd "id $id_type,creation_date datetime,modification_date datetime"

    if { [llength $cmdList] } {
	append cmd ",[join $cmdList ,]"
    }
    append cmd ")"
    if { [$lognoter_db cget -dbtype] eq "mysql" } {
	append cmd " engine = InnoDB"
    }
    return [list $names $cmd]
}

proc formulae::create_update_database_table { args } {
    variable values
    
    set optional {
	{ -old_database db "" }
    }
    set compulsory "key"
    parse_args $optional $compulsory $args

    set doc [dict get $values $key doc]
    set lognoter_db [dict get $values $key lognoter_db]

    set formulaeNode [$doc selectNodes formulae]
    
    create_update_database_table_do -old_database $old_database $lognoter_db $formulaeNode
	
}

proc formulae::create_update_database_table_xml { lognoter_db xml } {
    
    dom parse $xml doc
    set formulaeNode [$doc selectNodes //formulae]
    create_update_database_table_do $lognoter_db $formulaeNode
    return [$formulaeNode @database ""]
}

proc formulae::create_update_database_table_do { args } {
    
    set optional {
	{ -old_database db "" }
	{ -keep_old_database boolean 0 }
    }
    set compulsory "lognoter_db formulaeNode"
    parse_args $optional $compulsory $args

    if { [$formulaeNode nodeName] ne "formulae" } {
	set formulaeNode [$formulaeNode selectNodes //formulae]
    }
    if { $formulaeNode eq "" || [$formulaeNode @database ""] eq "" } { return }

    lassign [give_create_statement $formulaeNode $lognoter_db] names cmd

    if { $old_database eq "" } {
	set old_database [$formulaeNode @database]
    }
    set table_to_delete ""
    
    if { $old_database ne "" && [$lognoter_db sql table_exists $old_database] } {
	set cols [$lognoter_db sql column_names $old_database]
	set old_names [lrange $cols 3 end]
	
	if { $names eq $old_names && $old_database eq [$formulaeNode @database] } {
	    set table_fts [give_table_fts_name $old_database]
	    $lognoter_db sql exec "drop table if exists [$lognoter_db sql fs0 $table_fts]"
	    return
	}
	if { $old_database eq [$formulaeNode @database] } {
	    set table_fts [give_table_fts_name $old_database]
	    $lognoter_db sql exec "drop table if exists [$lognoter_db sql fs0 $table_fts]"
	    $lognoter_db sql exec "drop table if exists [$lognoter_db sql fs0 ${old_database}___old]"
	    $lognoter_db sql exec "alter table [$lognoter_db sql fs0 $old_database]
		rename to [$lognoter_db sql fs0 ${old_database}___old]"
	    set from_table [$formulaeNode @database]___old
	} else {
	    set from_table $old_database
	}
	if { !$keep_old_database } {
	    set table_to_delete $from_table
	}
    } else {
	set from_table ""
    }
    set table_fts [give_table_fts_name [$formulaeNode @database]]
    $lognoter_db sql exec "drop table if exists [$lognoter_db sql fs0 $table_fts]"
    $lognoter_db sql exec "drop table if exists [$lognoter_db sql fs0 [$formulaeNode @database]]"

    set err [catch { $lognoter_db sql exec $cmd } errstring]
    if { $err } {
	if { $table_to_delete ne "" && $table_to_delete ne $old_database } {
	    catch {
		$lognoter_db sql exec "alter table [$lognoter_db sql fs0 ${table_to_delete}]
		    rename to [$lognoter_db sql fs0 ${old_database}]"
	    }
	}
	error $errstring
    }
    if { $from_table ne "" } {
	if { $names eq $old_names } {
	    $lognoter_db sql exec "insert into [$lognoter_db sql fs0 [$formulaeNode @database]] select
		* from [$lognoter_db sql fs0 $from_table]"
	} elseif { "id_name" in $names && "id_name" ni $old_names } {
	    set common_fields [list_intersection $names $old_names]
	    if { [llength $common_fields] } {
		$lognoter_db sql exec "insert into [$lognoter_db sql fs0 [$formulaeNode @database]]
		    (id_name,[$lognoter_db sql fs $common_fields]) select 
		    concat('name',id),[$lognoter_db sql fs $common_fields] from
		    [$lognoter_db sql fs0 $from_table]"
	    }
	} else {
	    set common_fields [list_intersection $names $old_names]
	    if { [llength $common_fields] } {
		$lognoter_db sql exec "insert into [$lognoter_db sql fs0 [$formulaeNode @database]]
		    ([$lognoter_db sql fs $common_fields]) select 
		    [$lognoter_db sql fs $common_fields] from
		    [$lognoter_db sql fs0 $from_table]"
	    }
	}
	if { $table_to_delete ne "" } {
	    set table_fts [give_table_fts_name $table_to_delete]
	    $lognoter_db sql exec "drop table if exists [$lognoter_db sql fs0 $table_fts]"
	    $lognoter_db sql exec "drop table [$lognoter_db sql fs0 $table_to_delete]"
	}
    }
}

proc formulae::delete_full_formulae { key w } {
    variable values

    set doc [dict get $values $key doc]
    set root [$doc documentElement]
    set lognoter_db [dict get $values $key lognoter_db]

    if { $doc eq "" } {
	return -1
    }
    set text [_ "Are you sure to delete all forms information in this page?"]
    set retval [snit_messageBox -default cancel -icon question -message $text \
	    -type okcancel -parent $w]
    if { $retval eq "cancel" } {
	return -1
    }
    set ret [delete_database -parent $w $root $lognoter_db]
    if { $ret == -1 } { return -1 }

    set wp [dict get $values $key window_parent]
    destroy $wp.n
    return 0
}

proc formulae::_create_functions_button { button entry interp } {

    ttk::menubutton $button -image [cu::get_icon sum-16] -style Toolbutton \
	-menu $button.m
    set menu [menu $button.m -tearoff 0]
    set columnbreak 0
    
    set funcs [list sum(,) product(,) average(,) max(,) min(,) interpolate(,) \
	    dictionary(,) pi() if(,,) --- + - * / ^ ---- \
	    sin() cos() tan() asin() acos() atan() ---- \
	    sinh() cosh() tanh() atan2(,) ---- \
	    abs() sqrt() exp() log() log10() pow(,) hypot(,)]
    
    set columnbreak 0
    foreach ff1 [interp eval $interp info commands ::tcl::mathfunc::*] {
	set f1 [namespace tail $ff1]
	if { [lsearch -glob $funcs "${f1}(*"] == -1 } {
	    if { !$columnbreak } {
		lappend funcs ----
		set columnbreak 1
	    }
	    lappend funcs "${f1}()"
	}
    }
    
    foreach i $funcs {
	if { $i eq "---" } {
	    $menu add separator
	} elseif { $i eq "----" } {
	    set columnbreak 1
	} else {
	    $menu add command -label $i -command \
		[namespace code [list _insert_in_entry $entry $i]] \
		-columnbreak $columnbreak
	    set columnbreak 0
	}
    }
}

# edit properties window
proc formulae::edit_properties { args } {
    variable values
    
    set optional {
	{ -field_name name "" }
	{ -location "before|after field_name" "" }
	{ -copy_field boolean 0 }
    }
    set compulsory "key wp"
    parse_args $optional $compulsory $args
    
    set interp [dict get $values $key interp]

    destroy $wp._props
    set w [dialogwin_snit $wp._props -title [_ "Change properties"] \
	    -callback [namespace code [list edit_properties_do $key]] -grab 1 \
	    -transient 1 -morebuttons [list [_ Apply]]]


    $w set_uservar_value refresh_pending 0

    set f [$w giveframe]

################################################################################
#    property type
################################################################################

    set l1 [ttk::labelframe $f.l1 -text [_ "property type"]]
    
    ttk::radiobutton $l1.r1 -text [_ Parameter] -variable [$w give_uservar prop_type] \
	-value param -command [namespace code [list edit_properties_update -step prop_type \
		-original_field_name $key $w]]
    ttk::radiobutton $l1.r2 -text [_ Condition] -variable [$w give_uservar prop_type] \
	-value condition -command [namespace code [list edit_properties_update -step prop_type \
		-original_field_name $key $w]]
    ttk::radiobutton $l1.r3 -text [_ Container] -variable [$w give_uservar prop_type] \
	-value container -command [namespace code [list edit_properties_update -step prop_type \
		-original_field_name $key $w]]

    grid $l1.r1 -sticky nw
    grid $l1.r2 -sticky nw
    grid $l1.r3 -sticky nw

################################################################################
#    navigate
################################################################################

    set l12 [ttk::labelframe $f.l12 -text [_ "navigate"]]

    ttk::button $l12.b1 -text [_ "Previous"] -image [cu::get_icon back-16] -compound left
    $w set_uservar_value previous_button $l12.b1

    ttk::button $l12.b2 -text [_ "Next"] -image [cu::get_icon forward-16] -compound right
    $w set_uservar_value next_button $l12.b2

    ttk::menubutton $l12.mb1 -text [_ "Go to"] -menu $l12.mb1.m
    $w set_uservar_value goto_menu [menu $l12.mb1.m -tearoff 0]

    ttk::menubutton $l12.mb2 -text [_ Action] -menu $l12.mb2.m
    menu $l12.mb2.m -tearoff 0
    $l12.mb2.m add command -label [_ New] -command [namespace code \
	    [list edit_properties_do $key $w 3]]
    $l12.mb2.m add command -label [_ Copy] -command [namespace code \
	    [list edit_properties_do $key $w 4]]
    $l12.mb2.m add command -label [_ Delete] -command [namespace code \
	    [list edit_properties_do $key $w 5]]

    ttk::checkbutton $l12.cb1 -text [_ "Update current"] -variable \
	[$w give_uservar update_current 1]

    ttk::checkbutton $l12.cb2 -text [_ "Refresh"] -variable \
	[$w give_uservar refresh 0]

    grid $l12.b1 $l12.b2 -sticky w -padx 2
    grid $l12.mb1 $l12.mb2 -sticky w
    grid $l12.cb1 $l12.cb2 -sticky w -padx 2

    set l2 [ttk::labelframe $f.l2 -text [_ "properties"]]

################################################################################
#    location
################################################################################

    ttk::label $l2.l0 -text [_ Location:]
    ttk::frame $l2.f0
    ttk::combobox $l2.f0.cb1 -textvariable [$w give_uservar container ""] \
	-state readonly
    bind $l2.f0.cb1 <<ComboboxSelected>> [namespace code [list edit_properties_update \
		-step container -original_field_name $key $w]]

    ttk::combobox $l2.f0.cb2 -textvariable [$w give_uservar before_after [_ After]] \
	-width 7 -state readonly -values [list [_ Before] [_ After]]
    ttk::combobox $l2.f0.cb3 -textvariable [$w give_uservar before_after_name] \
	-state readonly

    ttk::menubutton $l2.f0.m1 -image [cu::get_icon keditbookmarks-16] -style Toolbutton -menu $l2.f0.m1.m
    set m [menu $l2.f0.m1.m -tearoff 0]

    $m add checkbutton -label [_ "Same page"] -variable [$w give_uservar same_page 0]
    $m add checkbutton -label [_ "Same line"] -variable [$w give_uservar same_line 0]
    $m add checkbutton -label [_ "Same menu"] -variable [$w give_uservar same_menu 0]
    
    grid $l2.f0.cb1 $l2.f0.cb2 $l2.f0.cb3 $l2.f0.m1 -sticky w
    grid configure $l2.f0.cb1 $l2.f0.cb3 -sticky ew
    grid columnconfigure $l2.f0 "0 2" -weight 1

################################################################################
#    name & Short name & Print name
################################################################################

    ttk::label $l2.l1 -text [_ Name:]
    ttk::combobox $l2.cb1 -textvariable [$w give_uservar name ""]

    foreach i [list $l2.l1 $l2.cb1] {
	bind $i <1> {
	    %W instate disabled {
		set ret [snit_messageBox -message [_ "Do you want to rename field?"] \
		        -type okcancel -parent %W]
		if { $ret eq "cancel" } { return }
		%W state !disabled
	    }
	}
    }
    ttk::label $l2.l2 -text [_ "Short name:"]
    ttk::combobox $l2.cb2 -textvariable [$w give_uservar short_name ""]
   
    ttk::labelframe $l2.l3
    ttk::label $l2.l3.l3 -text [_ "Print name:"]
    set wt [wordwidget_and_toolbox $l2.l3.wns1 -height 2 -bd 1 -mode static -scroll 0 \
	    -lastdircmd [list formulae::manage_lastdir $key] -exportimagesinline 1]
    $w set_uservar_value print_name_widget $wt
    grid $l2.l3.l3 $l2.l3.wns1 -sticky ew -padx 2 -pady 2
    grid columnconfigure $l2.l3 1 -weight 1

################################################################################
#    field type
################################################################################
   
    set field_types [list numeric text "long text" expression "text expression" \
	    "options editable" "options non-editable" \
	    "options multiple editable" "options multiple non-editable" \
	    formatted "formatted expression" button date file report_maker]

    set dict [dict create numeric [_ numeric] text [_ text] expression \
	    [_ expression] "text expression" [_ "text expression"] \
	    "options editable" [_ "options editable"] \
	    "options non-editable" [_ "options non-editable"] \
	    "options multiple editable" [_ "options multiple editable"] \
	    "options multiple non-editable" [_ "options multiple non-editable"] \
	    formatted [_ formatted] "formatted expression" [_ "formatted expression"] \
	    button [_ button] date [_ date] report_maker [_ "Report maker"]]
    
    ttk::frame $l2.f35
    ttk::label $l2.f35.l -text [_ "Field type:"]
    cu::combobox $l2.f35.cb -textvariable [$w give_uservar field_type numeric] \
	-values $field_types -dict $dict \
	-state readonly -width 18
    ttk::frame $l2.f35.f
    ttk::label $l2.f35.f.l -text [_ "Values:"]
    ttk::entry $l2.f35.f.e -textvariable [$w give_uservar valuesField ""]

    set t [_ "Enter a comma separator list of values here"]
    tooltip $l2.f35.f.l $t
    tooltip $l2.f35.f.e $t

    set a "$l2.f35.f.l $l2.f35.f.e" ;# @values
    set e "$l2.f1.m1 $l2.f1.m2" ;# expressions helpers
    set u "$l2.l5 $l2.cb3 $l2.m3" ;# units
    set d [dict create numeric $u text "" "long text" "" \
	    expression "$e $u" "options editable" \
	    $a "options non-editable" $a \
	    "options multiple editable" $a "options multiple non-editable" $a \
	    formatted "" "formatted expression" $e \
	    "text expression" $e]
    $w enable_disable_on_key field_type $d

    set a [list value_label [_ Value:]]
    set b [list value_label [_ Expression:]]
    set c [list value_label [_ Command:]]
    set d [dict create numeric $a text $a "long text" $a \
	    expression $b "options editable" \
	    $a "options non-editable" $a \
	    "options multiple editable" $a "options multiple non-editable" $a \
	    formatted $a "formatted expression" $b \
	    "text expression" $b button $c]
    $w change_key_on_key field_type $d

    grid $l2.f35.f.l $l2.f35.f.e -sticky w
    grid configure $l2.f35.f.e -sticky ew
    grid columnconfigure $l2.f35.f 1 -weight 1
    grid $l2.f35.l $l2.f35.cb $l2.f35.f -sticky w
    grid configure $l2.f35.f -sticky ew
    grid columnconfigure $l2.f35 2 -weight 1

################################################################################
#    expression
################################################################################

    ttk::label $l2.l4 -textvariable [$w give_uservar value_label]

    set examples {
	123.45
	A+2*F+sin(N2)*sqrt(V3)
	B>=C
    }
    regsub -all {\s*\n\s*} $examples "\n\t" examples
    set examples [string trim $examples]

    tooltip $l2.l4 [_ "Examples:\n\t%s" $examples]
    ttk::frame $l2.f1
    $w set_uservar_value value_widget [ttk::entry $l2.f1.e1 -textvariable \
	    [$w give_uservar value] -width 20]

    ttk::menubutton $l2.f1.m1 -image [cu::get_icon keditbookmarks-16] \
	-style Toolbutton -menu $l2.f1.m1.m
    $w set_uservar_value value_fields_widget [menu $l2.f1.m1.m -tearoff 0]

    _create_functions_button $l2.f1.m2 $l2.f1.e1 $interp
    
    grid $l2.f1.e1 $l2.f1.m1 $l2.f1.m2 -sticky w
    grid configure $l2.f1.e1 -sticky we
    grid columnconfigure $l2.f1 0 -weight 1

################################################################################
#    units
################################################################################

    ttk::label $l2.l5 -text [_ Units:]
    ttk::combobox $l2.cb3 -textvariable [$w give_uservar units] -values \
	[list m cm mm N kN Pa MPa m\u0b2 m\u0b3 m\u2074 \u20ac]
    ttk::menubutton $l2.m3 -image [cu::get_icon keditbookmarks-16] \
	-style Toolbutton -menu $l2.m3.m
    menu $l2.m3.m -tearoff 0
    foreach i [list \u0b2 \u0b3 \u2074] {
	$l2.m3.m add command -label $i -command [list $l2.cb3 insert insert $i]
    }
    
################################################################################
#    condition
################################################################################

    ttk::label $l2.l53 -text [_ "Condition"]:

    set examples {
	true
	A+2*F+sin(N2)*sqrt(V3) < 0
	B>=C
	disabled (this word disables the field)
	hidden (this word hides the field)
    }
    regsub -all {\s*\n\s*} $examples "\n\t" examples
    set examples [string trim $examples]

    tooltip $l2.l53 [_ "If condition evaluate to false or 0, field or container will be disabled.\nExamples:\n\t%s" $examples]
    ttk::frame $l2.f15
    $w set_uservar_value condition_widget [ttk::entry $l2.f15.e1 -textvariable \
	    [$w give_uservar condition] -width 20]

    ttk::menubutton $l2.f15.m1 -image [cu::get_icon keditbookmarks-16] \
	-style Toolbutton -menu $l2.f15.m1.m
    $w set_uservar_value condition_fields_widget [menu $l2.f15.m1.m -tearoff 0]

    _create_functions_button $l2.f15.m2 $l2.f15.e1 $interp
    
    grid $l2.f15.e1 $l2.f15.m1 $l2.f15.m2 -sticky w
    grid configure $l2.f15.e1 -sticky we
    grid columnconfigure $l2.f15 0 -weight 1


################################################################################
#    true & false
################################################################################

    ttk::label $l2.l54 -text [_ "True text:"]
    ttk::combobox $l2.cb31 -textvariable [$w give_uservar true]
    ttk::label $l2.l56 -text [_ "False text:"]
    ttk::combobox $l2.cb32 -textvariable [$w give_uservar false]

################################################################################
#    help
################################################################################

    ttk::label $l2.l6 -text [_ Help]
    $w set_uservar_value help_widget [text $l2.t -wrap word -height 4 -width 20]
    
    bind $l2.t  <Return> "[bind Text <Return>]; break"
    
    ttk::label $l2.l7 -text [_ "Lognoter help:"]
    cu::combobox_tree $l2.cb -width 20 -textvariable [$w give_uservar help_lognoter] \
	-state readonly -postcommand [list formulae::_fill_lognoter_pages_menu $key $l2.cb]
    
    set help [_ "Use a page of current lognoter database for help"]
    tooltip $l2.l7 $help
    tooltip $l2.cb $help
    
################################################################################
#    grid
################################################################################

    grid $l2.l0 $l2.f0 - -sticky w -pady 1
    grid $l2.l1 $l2.cb1 - -sticky w -pady 1
    grid $l2.l2 $l2.cb2 - -sticky w -pady 1
    grid $l2.l3 - - -sticky ew -pady 1 -padx 2
    grid $l2.f35 - - -sticky ew -pady 1
    grid $l2.l4 $l2.f1 - -sticky w -pady 1
    grid $l2.l5 $l2.cb3 $l2.m3 -sticky w -pady 1
    grid $l2.l53 $l2.f15 - -sticky w -pady 0
    grid $l2.l54 $l2.cb31 -sticky w -pady 1
    grid $l2.l56 $l2.cb32 -sticky w -pady 1
    grid $l2.l6 - - -sticky w  -padx "20 0" -pady 1
    grid $l2.t  - - -sticky wens -padx "20 4" -pady "1 4"
    grid $l2.l7 $l2.cb -sticky w -pady 1

    grid configure $l2.f0 $l2.cb1 $l2.cb2 $l2.f1 $l2.cb3 $l2.f15 $l2.cb31 $l2.cb32 \
	$l2.cb -sticky ew -padx "0 2"
    grid columnconfigure $l2 1 -weight 1
    grid rowconfigure $l2 11 -weight 1

    grid $l1 $l12 -sticky nsew
    grid $l2  - -sticky nsew

    grid configure $l12 -padx "2 0"
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 1

    tk::TabToWindow $l2.cb1
    bind $w <Return> [list $w invokeok]
    
    edit_properties_update -field_name $field_name -copy_field $copy_field \
	-location $location $key $w
    $w createwindow
}

proc formulae::_insert_in_entry { entry text } {
    focus $entry
    catch {
	set insert [$entry index insert]
	if {([$entry index sel.first] <= $insert)
	    && ([$entry index sel.last] >= $insert)} {
	    $entry delete sel.first sel.last
	}
    }
    set i [$entry index insert]
#     if { $i == 0 } {
#         $entry icursor end
#         set i [$entry index insert]
#     }
    $entry insert insert $text
    set j [string first "(" $text]
    if { $j != -1 } {
	$entry icursor [expr {$i+$j+1}]
    }
}

proc formulae::edit_properties_do { key w { action "" } } {

    if { $action eq "" } {
	set action [$w giveaction]
    }

    switch -- $action {
	-1 - 0 {
	    if { [$w give_uservar_value refresh_pending] } {
		create_windowD_do $key
	    }
	}
	1 {
	    set err [catch { edit_properties_accept $key $w } errstring]
	    if { $err } {
		if { $errstring ne "" } {
		    snit_messageBox -icon error -parent $w -message $errstring
		}
		#snit_messageBox -icon error -message $::errorInfo
		return
	    }
	}
	2 {
	    set err [catch { edit_properties_accept $key $w } errstring]
	    if { $err } {
		if { $errstring ne "" } {
		    snit_messageBox -icon error -parent $w -message $errstring
		}
		return
	    }
	    return
	}
	3 {
	    if { [$w give_uservar_value update_current] && [$w give_uservar_value name] ne "" } {
		set refresh [$w give_uservar_value refresh]
		set err [catch { edit_properties_accept -refresh $refresh $key $w } errstring]
		if { $err } {
		    if { $errstring ne "" } {
		        snit_messageBox -icon error -parent $w -message $errstring
		    }
		    return
		}
	    }
	    set field_name [$w give_uservar_value name]
	    edit_properties_update -location [list after $field_name] $key $w
	    return
	}
	4 {
	    set field_name [$w give_uservar_value name]
	    edit_properties_update -original_field_name -copy_field 1 \
		-location [list after $field_name] $key $w
	    return
	}
	5 {
	    set refresh [$w give_uservar_value refresh]
	    set err [catch { edit_properties_delete -refresh $refresh $key $w } errstring]
	    if { $err } {
		snit_messageBox -icon error -message $errstring
		return
	    }
	    edit_properties_update $key $w
	    return
	}
    }
    destroy $w
}

# what can be: prev, next this
proc formulae::edit_properties_navigate { key w ref_field_name what } {
    variable values
    
    set doc [dict get $values $key doc]

    if { [$w give_uservar_value update_current] && [$w give_uservar_value name] ne "" } {
	set refresh [$w give_uservar_value refresh]
	set err [catch { edit_properties_accept -refresh $refresh $key $w } errstring]
	if { $err } {
	    if { $errstring ne "" } {
		snit_messageBox -icon error -parent $w -message $errstring
	    }
	    return
	}
    }
    if { $ref_field_name eq "" } {
	set node ""
	if { [$w give_uservar_value name] ne "" } {
	    set name [$w give_uservar_value name]
	    set xp {
		//param[@n='N'] |
		//condition[@n='N'] |
		//container[@n='N']
	    }
	    set xp [string map [list N [xml [$w give_uservar_value name]]] $xp]
	    set node [$doc selectNodes $xp]
	    if { $node ne "" } {
		set ref_field_name [$node @n]
	    }
	}
	if { $node eq "" } {
	    set nodes [$doc selectNodes "//param|//condition|//container"]
	    set node [lindex $nodes 0]
	    if { $node ne "" } {
		set ref_field_name [$node @n]
		#             if { [llength $nodes] == 1 } {
		    #                 set what this
		    #             } elseif { $what eq "prev" } { set what next }
	    } else {
		set what new
	    }
	}
    }
    set xp {
	//param[@n='N'] |
	//condition[@n='N'] |
	//container[@n='N']
    }
    set xp [string map [list N $ref_field_name] $xp]

    set ref_node [$doc selectNodes $xp]
    switch $what {
	prev {
	    set nodes [$doc selectNodes //param|//condition|//container]
	    set ipos [lsearch -exact $nodes $ref_node]
	    set node [lindex $nodes [incr ipos -1]]
	}
	next {
	    set nodes [$doc selectNodes //param|//condition|//container]
	    set ipos [lsearch -exact $nodes $ref_node]
	    set node [lindex $nodes [incr ipos 1]]
	}
	this {
	    set node $ref_node
	}
    }
    if { $node ne "" } {
	set name [$node @n ""]
	if { $name eq "" } { set name [$node @value] }
	set location ""
    } else {
	set name ""
	switch $what prev { set location before }  next { set location after }
	lappend location $ref_field_name
    }
    edit_properties_update -field_name $name -location $location $key $w
}

proc formulae::defaultvalue { root name } {
    return [$root selectNodes "string(//param\[@n='[xml $name]'\]/@defaultvalue)"]
}

proc formulae::mathfunc_sum { args } {
    set res 0.0
    foreach i $args {
	set res [expr {$res+$i}]
    }
    return $res
}

proc formulae::mathfunc_product { args } {
    set res 1.0
    foreach i $args {
	set res [expr {$res*$i}]
    }
    return $res
}

proc formulae::mathfunc_average { args } {
    set res 0.0
    set num 0
    foreach i $args {
	set res [expr {$res+$i}]
	incr num
    }
    if { $num } { set res [expr {$res/double($num)}] }
    return $res
}

proc formulae::mathfunc_max { args } {
    set res ""
    foreach i $args {
	if { $res eq "" || $i > $res } {
	    set res $i
	}
    }
    return $res
}

proc formulae::mathfunc_min { args } {
    set res ""
    foreach i $args {
	if { $res eq "" || $i < $res } {
	    set res $i
	}
    }
    return $res
}

proc formulae::mathfunc_case { args } {
    set res ""
    foreach "cond value" $args {
	if { $value eq "" } {
	    set value $cond
	    set cnd 1
	} else {
	    set cnd [uplevel 1 expr $cond]
	}
	if { $cnd } { return [uplevel 1 expr $value] }
    }
    return 0.0
}

proc formulae::mathfunc_if { cond true_value false_value } {
    
    set cnd [uplevel 1 expr $cond]
    ::if { $cnd } {
	return $true_value
	#return [uplevel 1 expr $true_value]
    } else {
	return $false_value
	#return [uplevel 1 expr $false_value] 
    }
}


proc formulae::mathfunc_pi { args } {
    ::math::constants::constants pi pi
    return $pi
}

proc formulae::mathfunc_interpolate { val args } {
    if { $val < [lindex $args 0] } {
	return [lindex $args 1]
    }
    set lenm2 [expr {[llength $args]-2}]
    for { set i 0 } { $i < $lenm2 } { incr i 2 } {
	set x [lindex $args $i]
	set xnext [lindex $args [expr {$i+2}]]
	if { $val >= $x && $val <= $xnext } {
	    set deltax [expr {($val-$x)/double($xnext-$x)}]
	    return [expr {(1.0-$deltax)*[lindex $args [expr {$i+1}]]+\
		    $deltax*[lindex $args [expr {$i+3}]]}]
	}
    }
    return [lindex $args end]
}

proc formulae::mathfunc_dictionary { val args } {
    return [dict get $args $val]
}

proc formulae::mathfunc_t_set { table args } {
    
    if { [llength $args] == 0 } {
	set rows [llength $table]
	set columns [llength [lindex $table 0]]
	return [list $rows $columns]
    } elseif { [llength $args] == 2 } {
	lassign $args row column
	return [lindex $table $row $column]
    } else {
	set args [lassign $args row column]
	if { $row eq "first_void" } {
	    set row 0
	    foreach rowList $table {
		set is_void 1
		foreach col $rowList {
		    if { $col ne "" } {
		        set is_void 0
		        break
		    }
		}
		if { $is_void } { break }
		incr row
	    }
	    if { $row == [llength $table] } {
		lappend table [lrepeat [llength [lindex $table 0]] ""]
	    }
	}
	foreach v $args {
	    lset table $row $column $v
	    incr column
	}
	return $table
    }
}

proc formulae::mathfunc_function { min max expr } {
    if { $min > $max } { lassign [list $min $max] max min }

    set expr [parse_formula "" $expr]
    regexp {\$\{(.*?)\}} $expr {} varname
    upvar 1 $varname var
    if { $var < $min } {
	set $varname $min
    } elseif { $var > $max } {
	set $varname $max
    } else {
	set $varname $var
    }
    return [eval $expr]
}

proc formulae::mathfunc_eval_expr { varname value expr } {
    set $varname $value
    eval $expr
}

proc formulae::reset_has_changes { key } {
    variable values
    
    dict set values $key reset_has_changes 1
}

proc formulae::has_changes_in_values { key } {
    variable values
    
    if { [dict_getd $values $key reset_has_changes 0] == 1 } {
	return 0
    }
    if { ![dict exists $values $key notebook_widget] } { return 0 }
    set nb [dict get $values $key notebook_widget]

    dict for "n file" [dict_getd $values $key reload_file ""] {
	if { $file eq "" || [file isdirectory $file] } { continue }
	set mtime_last [dict get $values $key reload_mtime $n]
	set mtime_file [cu::file::mtime_recursive $file]
	if { $mtime_file ne "" && $mtime_file > $mtime_last } {
	    set message0 "Do you want to reload file '%s'?"
	    set message [_ "Do you want to reload file '%s'?" $file]
	    set retval [snit_messageBox -default ok -icon question -message $message \
		    -type yesnocancel -parent $nb -do_not_ask_again 1 -do_not_ask_again_key $message0]
	    if { $retval eq "cancel" } { error "" }
	    if { $retval eq "yes" } {
		lassign [dict get $values $key reload_menu_pos $n] menu idx
		$menu invoke $idx
	    }
	}
    }
    if { [info exists message0] } {
	dialogwin_snit clear_do_not_ask_again $message0
    }    
    return [$nb manage_database check_has_changes_only]
}

proc formulae::update_sub_tree { key node_tree } {
    variable values
    variable uvalues
    
    window_actualize -exec_callback 0 $key

    foreach n [array names uvalues $key,*] {
	set v $uvalues($n)
	regexp {,(.*)} $n {} np
	set xp [format_xpath {//param[@n=%s]} $np]
	set node [$node_tree selectNodes $xp]
	if { $node eq "" } { continue }
	set value $v
	$node setAttribute value $value
    }
}

proc formulae::give_updated_tree { key } {
    variable values
    variable current_set
    variable uvalues
    
    window_actualize -exec_callback 0 $key
    
    foreach i [list doc] {
	set $i [dict get $values $key $i]
    }

    if { $doc eq "" } { return "" }
    
    set root [$doc documentElement]
    if { $root eq "" } { return "" }

    set current_set(name,$key) [string trim $current_set(name,$key)]
    set secnameNode [$root selectNodes .//setname]
    if { $secnameNode ne "" && $current_set(name,$key) ne "" } {
	set cnode [$secnameNode selectNodes "name\[@n='[xml $current_set(name,$key)]'\]"]
	if { $cnode ne "" } { $cnode delete }
	set cnode [$secnameNode selectNodes {name[1]}]
	set newnode [[$secnameNode ownerDocument] createElement name]
	$newnode setAttribute n $current_set(name,$key) print $current_set(print,$key)
	if { $cnode ne "" } {
	    $secnameNode insertBefore $newnode $cnode
	} else {
	    $secnameNode appendChild $newnode
	}
	$secnameNode setAttribute n $current_set(name,$key)
    }
    foreach n [array names uvalues $key,*] {
	set v $uvalues($n)
	regexp {,(.*)} $n {} np
	set xp [format_xpath {//param[@n=%s]} $np]
	set node [$root selectNodes $xp]
	set value $v
#         if { [$node @dict ""] ne "" } {
#             set dict [dict_inverse [comma_field_to_list [$node @dict]]]
#             set value [dict_getd $dict $v $v]
#         } else {
#             set value $v
#         }
	$node setAttribute value $value
	if { [info exists newnode] } {
	    set npa [$newnode appendChildTag name_param]
	    $npa setAttribute n $np value $value
	}
    }
    return $doc
}

proc formulae::give_params_dict { args } {
    variable values
    variable uvalues
    
    set optional {
	    { -trim "" 0 }
	}
    set compulsory "key"
    parse_args $optional $compulsory $args
    
    set doc [dict get $values $key doc]

    if { $doc eq "" } { return "" }
    set root [$doc documentElement]
    if { $root eq "" } { return "" }

    set d ""
    foreach n [array names uvalues $key,*] {
	set v $uvalues($n)
	regexp {,(.*)} $n {} np
#         set xp [format_xpath {//param[@n=%s]} $np]
#         set node [$root selectNodes $xp]
#         if { [$node @dict ""] ne "" } {
#             set dict [dict_inverse [comma_field_to_list [$node @dict]]]
#             set value [dict_getd $dict $v $v]
#         } else {
#             set value $v
#         }
	if { !$trim } {
	    dict set d $np $v
	} else {
	    dict set d $np [string trim $v]
	}
    }
    return $d
}

proc formulae::manage_names { what key combo node } {
    variable values
    variable current_set
    variable uvalues
    
#     foreach n [array names uvalues $key,*] {
#         set $n $uvalues($key,$n)
#     }
    set current_set(name,$key) [string trim $current_set(name,$key)]

    set xp [format_xpath {name[@n=%s]} $current_set(name,$key)]
    set cnode [$node selectNodes $xp]

    switch $what {
	"accept" {
	    if { $cnode ne "" } { $cnode delete }
	    set cnode [$node selectNodes {name[1]}]
	    set newnode [[$node ownerDocument] createElement name]
	    $newnode setAttribute n $current_set(name,$key) print $current_set(print,$key)
	    if { $cnode ne "" } {
		$node insertBefore $newnode $cnode
	    } else {
		$node appendChild $newnode
	    }            
	    foreach n [array names uvalues $key,*] {
		set v $uvalues($n)
		regexp {,(.*)} $n {} np
		set npa [$newnode appendChildTag name_param]
		$npa setAttribute n $np value $v
	    }
	    set valuesList ""
	    foreach cnode [$node selectNodes name] {
		lappend valuesList [$cnode @n]
	    }
	    $combo configure -values $valuesList
	}
	"select" {
	    if { $cnode eq "" } { return }
	    foreach n [array names uvalues $key,*] {
		set v $uvalues($n)
		regexp {,(.*)} $n {} np
		set xp [format_xpath {name_param[@n=%s]} $np]
		set pnode [$cnode selectNodes $xp]
		if { $pnode ne "" } {
		    set uvalues($n) [$pnode @value]
		}
	    }
	    set current_set(print,$key) [$cnode @print 1]
	    window_actualize $key
	}
	"delete" {
	    if { $cnode eq "" } { return }
	    $cnode delete
	    set valuesList ""
	    foreach cnode [$node selectNodes name] {
		lappend valuesList [$cnode @n]
	    }
	    $combo configure -values $valuesList
	    set current_set(name,$key) ""
	    set current_set(print,$key) 1
	}
	"print" {
	    if { $cnode eq "" } { return }
	    $cnode setAttribute print $current_set(print,$key)

	    foreach "numprinted numtotal" [list 0 0] break
	    foreach cnode [$node selectNodes name] {
		incr numprinted [$cnode @print 1]
		incr numtotal
	    }
	    if { $numprinted == $numtotal } {
		set current_set(print_all_none,$key) all
	    } elseif { $numprinted > 0 } {
		set current_set(print_all_none,$key) some
	    } else {
		set current_set(print_all_none,$key) none
	    }
	}
	"printall" {
	    foreach cnode [$node selectNodes name] {
		$cnode setAttribute print 1
	    }
	    set current_set(print,$key) 1
	    set current_set(print_all_none,$key) all
	}
	"printnone" {
	    foreach cnode [$node selectNodes name] {
		$cnode setAttribute print 0
	    }
	    set current_set(print,$key) 0
	    set current_set(print_all_none,$key) none
	}
    }
}

snit::widgetadaptor snit_notebook {
    option -lognoter_db ""
    option -database ""
    option -rootnode ""
    option -notes_widgets ""
    option -main_state normal
    option -dbstate normal ;# normal or disabled
    option -actualize_callback
    option -fill_form_callback
    option -eval_command
    option -contextual_add_callback ""
    option -type_name notebook
    option -preferred_tree_position left
    option -db_view_dates 0
    option -create_edit_delete_cmds ;# a dict, with keys: create,edit,edit_multiple,delete

    variable tabs ""
    variable tab_options
    variable current_tab ""
    variable grid_pos ""
    variable grid_opts ""
    variable search_state 0
    variable is_created 0
    variable tree
    variable db_list
    variable current_id_name
    variable current_id_values
    variable save_focus ""
    variable last_fill_entries_item ""
    variable fill_database_entries ""
    variable fill_database_entries_limit 200
    variable searchstring ""
    
    delegate method * to hull
    delegate method paned_add to hull as add
    delegate option * to hull

    constructor args {

	#installhull [ttk::panedwindow $win -orient vertical]
	installhull [panedwindow $win -orient vertical -bd 0]
	ttk::frame $win.f -borderwidth 0
	#$win paned_add $win.f -weight 0
	$win paned_add $win.f -stretch last -sticky nsew -minsize 40 
	
	$self configurelist $args
	
	if { $options(-type_name) in "left_tree right_tree tree" } {
	    ttk::panedwindow $win.f.paned -orient horizontal
	    package require fulltktree
	    set columns [list [list 10 [_ Pages] left imagetext 1]]

	    ttk::frame $win.f.paned.f1 -style WNtoolbar -padding "2 0"
	    ttk::label $win.f.paned.f1.l1 -text [_ "Form pages"] -style WNlabel
	    tooltip::tooltip $win.f.paned.f1.l1 [_ "Select the form page"]

	    set toctree [fulltktree $win.f.paned.f1.tree -width 120 \
		    -selecthandler [mymethod _pn_select tree] \
		    -selecthandler2 "# do nothing" \
		    -columns $columns -expand 1 \
		    -selectmode browse -showheader 0 -showlines 0  \
		    -indent 0 -font MainFont -showbuttons 0 \
		    -folder_image [list [cu::get_icon toggle_log-16] \
		        open appbook16 {}]]
	    set tree $win.f.paned.f1.tree
	    
	    grid $win.f.paned.f1.l1
	    grid $win.f.paned.f1.tree -sticky nsew
	    grid columnconfigure $win.f.paned.f1 0 -weight 1
	    grid rowconfigure $win.f.paned.f1 0 -minsize 26
	    grid rowconfigure $win.f.paned.f1 1 -weight 1

	    ttk::frame $win.f.paned.f -borderwidth 0
	    grid columnconfigure $win.f.paned.f 0 -weight 1
	    grid rowconfigure $win.f.paned.f 0 -weight 1

	} elseif { $options(-type_name) in "arrows_small arrows_small_h arrows_small_v" } {
	    ttk::button $win.f.b1 -image [cu::get_icon start-16] -style Toolbutton \
		-command [mymethod _pn_select start]
	    tooltip $win.f.b1 [_ "Start page"]
	    ttk::button $win.f.b2 -image [cu::get_icon back-16] -style Toolbutton \
	       -command [mymethod _pn_select previous]
	    tooltip $win.f.b2 [_ "Previous page"]
	    ttk::button $win.f.b3 -image [cu::get_icon forward-16] -style Toolbutton \
		-command [mymethod _pn_select next]
	    tooltip $win.f.b3 [_ "Next page"]
	    ttk::button $win.f.b4 -image [cu::get_icon finish-16] -style Toolbutton \
		-command [mymethod _pn_select last]
	    tooltip $win.f.b4 [_ "Last page"]
	    ttk::menubutton $win.f.b5 -text [_ "Go"] -style Toolbutton \
		-menu $win.f.b5.m
	    menu $win.f.b5.m -tearoff 0 -postcommand [mymethod _fill_go_menu $win.f.b5.m]

	    ttk::separator $win.f.sep
	} elseif { $options(-type_name) eq "arrows" } {
	    set grid_pos [list -row 2 -column 0]
	    set grid_opts [list -row 2 -column 0 -columnspan 5 -rowspan 1]

	    ttk::button $win.f.b1 -text [_ "Start"] -image [cu::get_icon start-16] \
		-compound left -width 8 -command [mymethod _pn_select start]
	    ttk::button $win.f.b2 -text [_ "Previous"] -image [cu::get_icon back-16] \
		-compound left -width 8 -command [mymethod _pn_select previous]
	    ttk::button $win.f.b3 -text [_ "Next"] -image [cu::get_icon forward-16] \
		-compound right -width 8 -command [mymethod _pn_select next]
	    ttk::button $win.f.b4 -text [_ "Last"] -image [cu::get_icon finish-16] \
		-compound right -width 8 -command [mymethod _pn_select last]
	    ttk::menubutton $win.f.b5 -text [_ "Go"] \
		-menu $win.f.b5.m
	    menu $win.f.b5.m -tearoff 0 -postcommand [mymethod _fill_go_menu $win.f.b5.m]

	    ttk::separator $win.f.sep
	} else {
	    ttk::notebook $win.f.nb
	    ttk::separator $win.f.sep
	}
	$self configurelist $args
	bind $win <Configure> [mymethod _compute_size]

	set is_created 1
	$self _configure_database
    }
    method id_name { args } {
	switch [llength $args] {
	    0 {
		return $current_id_name
	    }
	    1 {
		set current_id_name [lindex $args 0]
	    }
	    default {
		error "error in id_name"
	    }
	}
    }
    method id_values { args } {
	switch [llength $args] {
	    0 {
		return $current_id_values
	    }
	    1 {
		set current_id_values [lindex $args 0]
	    }
	    default {
		error "error in id_values"
	    }
	}
    }
    method _grid_buttons {} {
	
	if { $options(-type_name) in "left_tree right_tree tree" } {
	    set grid_pos [list -row 0 -column 0]
	    set grid_opts [list -row 0 -column 0 -columnspan 1 -rowspan 1 -in $win.f.paned.f]
	    
	    if { $options(-database) eq "" } {
		if { [winfo exists $win.f.db] } {
		    grid remove $win.f.db
		    grid rowconfigure $win.f.paned.f 0 -weight 1
		    grid rowconfigure $win.f.paned.f 1 -weight 0
		}
	    } else {
		grid $win.f.db -row 0 -column 0 -columnspan 1 -rowspan 1 \
		    -in $win.f.paned.f -sticky w -pady "0 5"
		grid rowconfigure $win.f.paned.f 0 -weight 0
		grid rowconfigure $win.f.paned.f 1 -weight 1
		set grid_pos [list -row 1 -column 0]
		set grid_opts [list -row 1 -column 0 -columnspan 1 -rowspan 1 \
		        -in $win.f.paned.f]
	    }
	    catch {
		$win.f.paned forget $win.f.paned.f1
		$win.f.paned forget $win.f.paned.f
	    }
	    switch $options(-type_name) {
		left_tree { set tn left }
		right_tree { set tn right }
		default { 
		    set tn $options(-preferred_tree_position)
		}
	    }
	    switch $tn {
		left {
		    $win.f.paned insert end $win.f.paned.f1 -weight 1
		    $win.f.paned insert end $win.f.paned.f -weight 3
		}
		right {
		    $win.f.paned insert end $win.f.paned.f -weight 3
		    $win.f.paned insert end $win.f.paned.f1 -weight 1
		}
	    }
	    grid $win.f.paned -row 0 -row 0 -column 0 -sticky nsew -padx "1 2"
	    grid columnconfigure $win.f 0 -weight 1
	    grid rowconfigure $win.f 0 -weight 1
	} elseif { $options(-type_name) eq "arrows_small_v" } {
	    set grid_pos [list -row 0 -column 1]
	    set grid_opts [list -row 0 -column 1 -columnspan 1 -rowspan 6]

	    if { [llength $tabs] > 1 } {
		if { $options(-database) eq "" } {
		    set idx 0
		    foreach i [list $win.f.b1 $win.f.b2 $win.f.b3 $win.f.b4 $win.f.sep $win.f.b5] {
		        grid $i -sticky n -row $idx -column 0
		        grid rowconfigure $win.f $idx -weight 0
		        incr idx
		    }
		    grid configure $win.f.b1 -pady "40 0"
		    grid configure $win.f.sep -sticky new -padx 2
		    grid columnconfigure $win.f 1 -weight 1
		    grid rowconfigure $win.f 5 -weight 1
		    if { [winfo exists $win.f.db] } {
		        grid remove $win.f.db
		    }
		    dict set grid_opts -rowspan 6
		} else {
		    set idx 0
		    foreach i [list $win.f.b1 $win.f.b2 $win.f.b3 $win.f.sep $win.f.b5 \
		            $win.f.db] {
		        grid $i -sticky n -row $idx -column 0
		        grid rowconfigure $win.f $idx -weight 0
		        incr idx
		    }
		    grid configure $win.f.b1 -pady "40 0"
		    grid configure $win.f.sep -sticky new -padx 2
		    grid columnconfigure $win.f 1 -weight 1
		    grid rowconfigure $win.f 5 -weight 1
		    grid remove $win.f.b4
		    dict set grid_opts -rowspan 6
		}
	    } else {
		grid remove $win.f.b1 $win.f.b2 $win.f.b3 $win.f.b4 \
		    $win.f.b5 $win.f.sep
		if { $options(-database) eq "" } {
		    if { [winfo exists $win.f.db] } {
		        grid remove $win.f.db
		    }

		} else {
		    set idx 0
		    foreach i [list $win.f.db] {
		        grid $i -sticky n -row $idx -column 0
		        incr idx
		    }
		    grid columnconfigure $win.f 1 -weight 1
		    grid rowconfigure $win.f 0 -weight 1
		    dict set grid_opts -rowspan 1
		}
	    }
	} elseif { $options(-type_name) in "arrows arrows_small arrows_small_h" } {
	    set grid_pos [list -row 2 -column 0]
	    set grid_opts [list -row 2 -column 0 -columnspan 5 -rowspan 1]
	    if { [llength $tabs] > 1 } {
		if { $options(-database) eq "" } {
		    set idx 0
		    foreach i [list $win.f.b1 $win.f.b2 $win.f.b3 $win.f.b4 $win.f.b5] {
		        grid $i -sticky w -row 0 -column $idx
		        grid columnconfigure $win.f $idx -weight 0
		        incr idx
		    }
		    grid $win.f.sep -padx "0 50" -sticky ew -pady 2 -row 1 -column 0 \
		        -columnspan 5
		    grid columnconfigure $win.f 4 -weight 1
		    if { [winfo exists $win.f.db] } {
		        grid remove $win.f.db
		    }
		    dict set grid_opts -columnspan 5
		} else {
		    set idx 0
		    if { $options(-type_name) eq "arrows" } {
		        set list [list $win.f.b1 $win.f.b2 $win.f.b3 $win.f.b5 \
		                $win.f.db]
		    } else {
		        set list [list $win.f.b1 $win.f.b2 $win.f.b3 $win.f.b4 \
		                $win.f.b5 $win.f.db]
		    }
		    foreach i $list {
		        grid $i -sticky nw -row 0 -column $idx
		        grid columnconfigure $win.f $idx -weight 0
		        incr idx
		    }
		    grid configure $win.f.db -padx "20 0"
		    grid $win.f.sep -padx "0 50" -sticky ew -pady 2 -row 1 -column 0 \
		        -columnspan [llength $list]
		    grid columnconfigure $win.f [expr {[llength $list]-1}] -weight 1
		    dict set grid_opts -columnspan [llength $list]
		}
		grid rowconfigure $win.f 2 -weight 1
	    } else {
		grid remove $win.f.b1 $win.f.b2 $win.f.b3 $win.f.b4 \
		    $win.f.b5 $win.f.sep
		if { $options(-database) eq "" } {
		    if { [winfo exists $win.f.db] } {
		        grid remove $win.f.db
		    }
		} elseif { [winfo exists $win.f.db] } {
		    set idx 0
		    foreach i [list $win.f.db] {
		        grid $i -sticky w -row 0 -column $idx
		        grid columnconfigure $win.f $idx -weight 0
		        incr idx
		    }
		    grid $win.f.sep -padx "0 50" -sticky ew -pady 2 -row 1 -column 0 \
		        -columnspan 1
		    grid columnconfigure $win.f 0 -weight 1
		    grid rowconfigure $win.f 2 -weight 1
		    dict set grid_opts -columnspan 1
		}
		grid rowconfigure $win.f 2 -weight 1
	    }
	} else {
	    if { $options(-database) eq "" } {
		grid $win.f.nb -sticky nsew -row 0 -column 0
		grid columnconfigure $win.f 0 -weight 1
		grid rowconfigure $win.f 0 -weight 1
		if { [winfo exists $win.f.db] } {
		    grid remove $win.f.db
		}
	    } else {
		set idx 0
		foreach i [list $win.f.db] {
		    grid $i -sticky w -row 0 -column $idx
		    grid columnconfigure $win.f $idx -weight 0
		    incr idx
		}
		grid $win.f.sep -padx "0 50" -sticky ew -pady 2 -row 1 -column 0 \
		    -columnspan 1
		grid $win.f.nb -row 1 -column 0 -sticky nsew -pady 2 -row 2 -column 0 \
		    -columnspan 1
		grid columnconfigure $win.f 0 -weight 1
		grid rowconfigure $win.f 2 -weight 1
		dict set grid_opts -columnspan 1
		dict set grid_opts -row 1
		dict set grid_pos -row 1
	    }
	}
    }
    method _configure_database { args } {
	if { !$is_created } { return }
	
	set optional {
	    { -force "" 0 }
	}
	set compulsory ""
	parse_args $optional $compulsory $args

	unset -nocomplain current_id_name
	
	if { $options(-lognoter_db) ne "" && $options(-database) ne "" && \
	     $options(-rootnode) ne "" } {
	    if { $force } {
		destroy $win.list $win.listupdown
	    }
	    if { ![winfo exists $win.list] } {
		set db $options(-lognoter_db)
		set table $options(-database)
		set columns ""
		set sort_type_cols ""
		lappend columns [list 6 id left text 1]
		lappend sort_type_cols [list integer]
		if { $options(-db_view_dates) } {
		    lappend columns [list 12 [_ "Creation date"] left text 1]
		    lappend columns [list 12 [_ "Modification date"] left text 1]
		    lappend sort_type_cols "" ""
		}
		set cols [$db sql column_names $table]
		set xp0 {//param[@n=%s]|//condition[@n=%s]}
		foreach col [lrange $cols 3 end] {
		    if { $col eq "id_name" } { set col [_ "Name id"] }
		    if { [regexp {__(contents|cdate|mdate|type|size|path)$} $col] } {
		        continue
		    }
		    lassign [list 14 left ""] len justify sort
		    set xp [format_xpath $xp0 $col $col]
		    set node [$options(-rootnode) selectNodes $xp]
		    if { $node ne "" } {
		        set col [formulae::node_pn $node]
		        if { [$node @field_type ""] eq "numeric" } {
		            lassign [list 10 right real] len justify sort
		        }
		    }
		    lappend columns [list $len $col $justify text 1]
		    lappend sort_type_cols $sort
		}
		package require fulltktree
		frame $win.list
		ttk::frame $win.list.f -style WNtoolbar -padding "2 2"
		fulltktree $win.list.f.tree -columns $columns -sort_type_cols $sort_type_cols \
		    -showbuttons 0 -showlines 0 \
		    -bd 1 -relief raised -sensitive_cols all \
		    -contextualhandler [mymethod manage_database contextual] \
		    -selecthandler [mymethod manage_database selected_row] \
		    -selecthandler2 [mymethod manage_database fill_entries] \
		    -deletehandler [mymethod manage_database delete in_tree] \
		    -height 40
		
		set db_list $win.list.f.tree
		
		set idx 1
		foreach col [lrange $columns 1 end] {
		    $db_list column configure c$idx -minwidth 20 -width "" \
		        -expand 0 -maxwidth 200
		    incr idx
		}
		grid $win.list.f.tree -sticky nsew
		grid columnconfigure $win.list.f 0 -weight 1
		grid rowconfigure $win.list.f 0 -weight 1
		
		grid $win.list.f -sticky nsew -padx "2 2"
		grid columnconfigure $win.list 0 -weight 1
		grid rowconfigure $win.list 0 -weight 1
		
		frame $win.listupdown -width 48 -height 16
		button $win.listupdown.up -image [cu::get_icon nav1uparrow16] \
		    -command [mymethod view_database_list show_more] \
		    -relief flat -overrelief solid -background "mint cream"
		button $win.listupdown.down -image [cu::get_icon nav1downarrow16] \
		    -command [mymethod view_database_list show_less] \
		    -relief flat -overrelief solid -background "mint cream"
		button $win.listupdown.right -image [cu::get_icon navforward16] \
		    -command [mymethod view_full_columns] \
		    -relief flat -overrelief solid -background "mint cream"

		tooltip $win.listupdown.up [_ "Show database list"]
		tooltip $win.listupdown.down [_ "Hide database list"]
		tooltip $win.listupdown.right [_ "View columns at maximum width"]
		
		grid $win.listupdown.up $win.listupdown.down $win.listupdown.right -sticky nsew
		grid columnconfigure $win.listupdown "0 1" -weight 1
		grid rowconfigure $win.listupdown 0 -weight 1
		grid propagate $win.listupdown 0
		
		place $win.listupdown -in $win.list -relx 1 -x -22 -y 3 -anchor ne
	    }
	    #$win paned_add $win.list -weight 1
	    $win paned_add $win.list -stretch always -sticky nsew -minsize 60
	    $self manage_database update

	    if { ![winfo exists $win.f.db] } {
		if { $options(-rootnode) ne "" && [$options(-rootnode) @has_unique_name 0] == 1 } {
		    set has_unique_name 1
		} else {
		    set has_unique_name 0
		}
		ttk::frame $win.f.db -style WNtoolbar
		ttk::button $win.f.db.create -image [cu::get_icon filenew-16] \
		    -style WNbutton \
		    -command [mymethod manage_database create] \
		    -text [_ "New entry"] -compound left
		cu::menubutton_button $win.f.db.edit -image [cu::get_icon edit-16] \
		    -command [mymethod manage_database edit] \
		    -text [_ "Modify entry"] -compound left \
		    -menu $win.f.db.edit.m -style WNmenubutton
		
		set m [menu $win.f.db.edit.m -tearoff 0]
		$m add command -image [cu::get_icon edit-16] -label [_ "Modify entry"] -compound left \
		    -command [mymethod manage_database edit]
		$m add command -image [cu::get_icon edit-16] -label [_ "Modify some fields"]... -compound left \
		    -command [mymethod manage_database edit_multiple]
		
		if { [dict_getd $options(-create_edit_delete_cmds) edit_multiple ""] eq "-" } {
		    $m entryconfigure end -state disabled
		}
		cu::menubutton_button $win.f.db.find -image [cu::get_icon kfind-16] \
		    -menu $win.f.db.find.m \
		    -command [mymethod manage_database search_any] \
		    -style WNbutton

		menu $win.f.db.find.m -tearoff 0
		$win.f.db.find.m add command -label [_ "Search in any field"] \
		    -command [mymethod manage_database search_any]
		if { $has_unique_name } {
		    $win.f.db.find.m add command -label [_ "Search in name"] \
		        -command [mymethod manage_database search_name]
		}
		$win.f.db.find.m add command -image [cu::get_icon kfind-16] \
		    -label [_ "Search in field"] -compound left \
		    -command [mymethod manage_database search_in_fields]

		if {  ![$self manage_database can_have_search_fields] } {
		    $win.f.db.find.m entryconfigure end -state disabled
		}
		cu::menubutton_button $win.f.db.more -image [cu::get_icon list-add-16] \
		    -menu $win.f.db.more.m -style WNbutton

		menu $win.f.db.more.m -tearoff 0
		
		$win.f.db.more.m add command -label [_ "View form and database"] \
		    -command [mymethod view_database_list show_both]
		$win.f.db.more.m add command -label [_ "View database list"] \
		    -command [mymethod view_database_list show_list]
		$win.f.db.more.m add command -label [_ "View form"] \
		    -command [mymethod view_database_list hide_list]

		$win.f.db.more.m add separator
		$win.f.db.more.m add command -label [_ "Delete entries"] \
		    -image [cu::get_icon remove-16] -compound left \
		    -command [mymethod manage_database delete in_menu]
		
		if { $has_unique_name } {
		    frame $win.f.db.name -bd 0
		    ttk::label $win.f.db.name.l -text [_ "Name"]: -style WNlabel
		    cu::combobox $win.f.db.name.cb -textvariable [myvar current_id_name] \
		        -valuesvariable [myvar current_id_values]
		    
		    bind $win.f.db.name.cb <Control-s> [mymethod manage_database search_any]
		    bind $win.f.db.name.cb <Return> [mymethod manage_database create]
		    
		    foreach i [list $win.f.db.name.l $win.f.db.name.cb] {
		        tooltip $i [_ "Enter the name of the entry to create or modify"]
		    }
		    grid $win.f.db.name.l $win.f.db.name.cb -sticky w
		    grid $win.f.db.name.l -sticky wns
		    grid configure $win.f.db.name.cb
		    set current_id_name ""
		}

		grid $win.f.db.create $win.f.db.edit $win.f.db.find $win.f.db.more \
		    -sticky w -padx 2
		grid configure $win.f.db.create -padx "14 2"
		if { [winfo exists $win.f.db.name] } {
		    grid $win.f.db.name -columnspan 4 -sticky w -padx "14 2" -pady "0 2"
		}
		grid columnconfigure $win.f.db 3 -weight 1
		
		tooltip $win.f.db.create [_ "Create new entry in the database"]
		tooltip $win.f.db.edit [_ "Modify selected entry in the database"]
		tooltip $win.f.db.find [_ "Search database"]
		tooltip $win.f.db.more [_ "Additional commands"]
	    }
	    if { $options(-dbstate) eq "disabled" } {
		foreach i [list $win.f.db.create $win.f.db.edit $win.f.db.find \
		       $win.f.db.more] {
		    $i state disabled
		} 
	    }
	    #$self _grid_buttons
	} elseif { $options(-lognoter_db) eq "" && $options(-database) eq "" } {
	    unset -nocomplain db_list
	    if { [winfo exists $win.list] } {
		destroy $win.list
	    }
	    #$self _grid_buttons
	}
	if { $options(-notes_widgets) ne "" } {
	    ttk::frame $win.notes
	    [lindex $options(-notes_widgets) 0] configure -height 4
	    grid [lindex $options(-notes_widgets) 0] -in $win.notes -row 0 -column 0 -sticky nsew
	    grid [lindex $options(-notes_widgets) 1] -in $win.notes -row 0 -column 1 -sticky ns
	    grid columnconfigure $win.notes 0 -weight 1
	    grid rowconfigure $win.notes 0 -weight 1
	    foreach i $options(-notes_widgets) { raise $i }
	    #$win paned_add $win.notes -weight 0
	    $win paned_add $win.notes -stretch always -sticky nsew -minsize 50
	}
	$self _grid_buttons
    }
    method try_add_actualize_button { image text cmd state tooltip } {

	if { [winfo exists $win.f.db] } {
	    set cmd [string map [list %W $win.f.db.actualize] $cmd]
	    set b [ttk::button $win.f.db.actualize -style WNbutton -image $image \
		    -text $text -compound left -command $cmd -state $state]
	    tooltip $b $tooltip

	    set ws [grid slaves $win.f.db -row 0]
	    set col [llength $ws]
	    
	    for { set i [expr {$col-1}] } { $i >= 0 } { incr i -1 } {
		set w [grid slaves $win.f.db -row 0 -column $i]
		grid forget $w
		grid $w -row 0 -column [expr {$i+1}] -sticky w -padx 2
	    }
	    grid $b -in $win.f.db -row 0 -column 0 -padx "14 2" -pady 2
	    grid columnconfigure $win.f.db [expr {$col-1}] -weight 0
	    grid columnconfigure $win.f.db $col -weight 1
	    return 1
	}
	return 0
    }
    onconfigure -database {value} {
	set options(-database) $value
	$self _configure_database
    }
    onconfigure -lognoter_db {value} {
	set options(-lognoter_db) $value
	$self _configure_database
    }
    onconfigure -rootnode {value} {
	set options(-rootnode) $value
	$self _configure_database
    }
    onconfigure -notes_widgets {value} {
	set options(-notes_widgets) $value
	$self _configure_database
    }
    onconfigure -dbstate {value} {
	set options(-dbstate) $value
	$self _configure_database
    }
    onconfigure -db_view_dates {value} {
	set options(-db_view_dates) $value
	$self _configure_database -force
    }
    method add { w args } {
	set pos [llength $tabs]
	set tab_options($pos) $args
	if { ![dict exists $tab_options($pos) -state] } {
	    dict set tab_options($pos) -state normal
	}
	lappend tabs $w
	if { $options(-type_name) in "left_tree right_tree tree" } {
	    set col [list [dict_getd $tab_options($pos) -image ""] \
		    [dict_getd $tab_options($pos) -text [_ "Page"]]]
	    $tree insert end [list $col]
	    
	    if { [dict_getd $tab_options($pos) -state ""] in "disabled hidden" } {
		$tree item state set end disabled
	    }
	} elseif { $options(-type_name) eq "notebook" } {
	    $win.f.nb add $w {*}$args
	} else {
	    if { [llength $tabs] == 1 } {
		$self select 0
	    } else {
		$win.f.b3 state !disabled
		$win.f.b4 state !disabled
	    }
	}
	$self _grid_buttons
    }
    method tab { index args } {
	if { $options(-type_name) eq "notebook" } {
	    $win.f.nb tab $index {*}$args
	} else {
	    set index [$self index $index]
	    if { ![llength $args] } {
		return $tab_options($index)
	    } elseif { [llength $args] == 1 } {
		if { [dict exists $tab_options($index) [lindex $args 0]] } {
		    return [dict get $tab_options($index) [lindex $args 0]]
		} else { return "" }
	    } else {
		set item [expr {$index+1}]
		foreach "n v" $args {
		    if { $options(-type_name) in "left_tree right_tree tree" } {
		        switch -- $n {
		            -state {
		                switch $v {
		                    disabled - hidden { $tree item state set $item disabled }
		                    default { $tree item state set $item !disabled }
		                }
		            }
		            -text {
		                $tree item text $item 0 $v
		            }
		            -image {
		                $tree item element configure $item 0 e_image -image $v
		            }
		        }
		    }
		    dict set tab_options($index) $n $v
		}
	    }
	}
    }
    method index { index } {
	if { $options(-type_name) eq "notebook" } {
	    return [$win.f.nb index $index]
	} else {
	    if { [set ipos [lsearch -exact $tabs $index]] != -1 } {
		return $ipos
	    }
	    # warning: end could be llength-1
	    set index [string map [list current $current_tab \
		        end [llength $tabs]] $index]
	   return [expr $index]
	}
    }
    proc _add_to_bindtag_recursive { w tag } {
	if { [lsearch [bindtags $w] $tag] == -1 } {
	    bindtags $w [concat $tag [bindtags $w]]
	}
	foreach wc [winfo children $w] {
	    _add_to_bindtag_recursive $wc $tag
	}
    }
    method select { index } {

	set index [$self index $index]
	set w [lindex $tabs $index]
	_add_to_bindtag_recursive $w notebook_children

	if { $options(-type_name) in "left_tree right_tree tree" } {
	    set wp [dict get $grid_opts -in]
	    set slaves [grid slaves $wp -row [dict get $grid_pos -row] \
		    -column [dict get $grid_pos -column]]
	    if { [llength $slaves] } {
		grid forget {*}$slaves
	    }
	    grid $w -in $wp -sticky nsew {*}$grid_opts
	    set current_tab $index
	    
	    set item [$tree selection get]
	    set dosel 1
	    if { [llength $item] == 1 } {
		set text [$tree item text $item 0]
		if { $text eq [dict get $tab_options($index) -text] } {
		    set dosel 0
		}
	    }
	    if { $dosel } { TreeCtrl::SetActiveItem $tree [expr {$index+1}] }
	} elseif { $options(-type_name) eq "notebook" } {
	    $win.f.nb select $index
	} else {
	    set old_w [grid slaves $win.f {*}$grid_pos]
	    if { $old_w ne "" } { grid remove $old_w }
	    grid $w -in $win.f -sticky nsew {*}$grid_opts
	    grid columnconfigure $win.f 0 -weight 1
	    set current_tab $index

	    for { set i 1 } { $i <= 4 } { incr i } {
		$win.f.b$i state !disabled
	    }
	    if { $index == 0 } {
		$win.f.b1 state disabled
		$win.f.b2 state disabled
	    }
	    if { $index == [llength $tabs]-1 } {
		$win.f.b3 state disabled
		$win.f.b4 state disabled
	    }
	}
	if { [winfo exists $win.f.db] } {
	    set height [winfo height $win.f.db]
	    if { [llength [$win panes]] >= 2 } {
		#set y [$win sashpos 0]
		set y [lindex [$win sash coord 0] 1]
		if { $y < 2*$height } {
		    set hmax 0
		    foreach i $tabs {
		        if { [winfo reqheight $i] > $hmax } { set hmax [winfo reqheight $i] }
		    }
		    set height [expr {$height+$hmax}]
		    #$win sashpos 0 $height
		    $win sash place 0 0 $height
		}
	    }
	}
    }
    method view_full_columns {} {
	set idx 1
	foreach col [lrange [$db_list cget -columns] 1 end] {
	    $db_list column configure c$idx -maxwidth "" -width ""
	    incr idx
	}
    }
    method view_database_list_check {} {
	set vdl [$options(-rootnode) @view_database_list show_both]
	after 100 [list catch [mymethod view_database_list $vdl]]
    }
    method view_database_list { what } {
	
	if { [winfo height $win] == 1 } { return }
	
	if { ![info exists db_list] } {
	    if { [llength [$win panes]] > 1 } {
		update idletasks
		set height [winfo reqheight [lindex [$win panes] 0]]
		if { $height > [winfo height $win]-40 } {
		    set height [expr {[winfo height $win]-40}]
		}
		update idletasks
		$win sash place 0 0 $height
	    }
	    return
	}
	
	if { $options(-main_state) eq "normal" } {
	    set root $options(-rootnode)
	    if { $root ne "" } {
		$root setAttribute view_database_list $what
	    }
	}
	switch $what {
	    show_list {
		set height [expr {[winfo height $win.f.db]+2}]
		if { "$win.list" ni [$win panes] } {
		    place forget $win.listupdown
		    raise $win.listupdown
		    if { "$win.notes" in [$win panes] } {
		        #$win insert 1 $win.list -weight 1
		        $win paned_add $win.list -stretch always -sticky nsew -minsize 40
		        $win paneconfigure $win.list -before $win.notes
		    } else {
		        #$win paned_add $win.list -weight 1
		        $win paned_add $win.list -stretch always -sticky nsew -minsize 40
		    }
		    place $win.listupdown -in $win.list -relx 1 -x -6 -y 3 -anchor ne
		}
		$win.listupdown.up configure -state disabled
		$win.listupdown.down configure -state normal
	    }
	    hide_list {
		if { "$win.list" in [$win panes] } {
		    $win forget $win.list
		    place forget $win.listupdown
		    raise $win.listupdown
		    if { "$win.notes" in [$win panes] } {
		        set w [lindex [grid slaves $win.notes] end]
		        place $win.listupdown -in $win.f -relx 1 -x -6 -rely 1 -y -0 -anchor se 
		    } else {
		        place $win.listupdown -in $win -relx 1 -x -6 -rely 1 -y 0 -anchor se
		    }
		    $win.listupdown.up configure -state normal
		    $win.listupdown.down configure -state disabled
		}
		return
	    }
	    show_both {
		if { $options(-type_name) eq "notebook" } {
		    set hmax [expr {[winfo reqheight $win.f.nb]+10}]
		    if { $hmax < 50 } { return }
		} else {
		    set hmax 0
		    foreach i $tabs {
		        if { [winfo reqheight $i] > $hmax } { set hmax [winfo reqheight $i] }
		    }
		}
		set height [expr {[winfo height $win.f.db]+$hmax}]
		if { $height > [winfo height $win] - 60 } {
		    set height [expr {[winfo height $win] - 60}]
		}
		if { "$win.list" ni [$win panes] } {
		    place forget $win.listupdown
		    raise $win.listupdown
		    if { "$win.notes" in [$win panes] } {
		        #$win insert 1 $win.list -weight 1
		        $win paned_add $win.list -stretch always -sticky nsew -minsize 40
		        $win paneconfigure $win.list -before $win.notes

		    } else {
		        #$win paned_add $win.list -weight 1
		        $win paned_add $win.list -stretch always -sticky nsew -minsize 40
		    }
		    place $win.listupdown -in $win.list -relx 1 -x -6 -y 3 -anchor ne
		    $win.listupdown.up configure -state normal
		    $win.listupdown.down configure -state normal
		}
	    }
	    show_more {
		if { "$win.list" ni [$win panes] } {
		    $self view_database_list show_both
		} else {
		    $self view_database_list show_list
		}
		return
	    }
	    show_less {
		if { "$win.list" ni [$win panes] } { return }
		set height [expr {[winfo height $win.f.db]+6}]
		#set y [$win sashpos 0]
		set y [lindex [$win sash coord 0] 1]
		if { $y <= $height } {
		    $self view_database_list show_both
		} else {
		    $self view_database_list hide_list
		}
		return
	    }
	}
	#$win sashpos 0 $height
	update idletasks
	$win sash place 0 0 $height
    }
    method _compute_size {} {
	if { $options(-type_name) eq "notebook" } {
	    $self view_database_list_check
	    return
	}
	update idletasks

	if { $options(-type_name) eq "arrows_small" } {
	    foreach "- - base_width base_height" [grid bbox $win.f 0 0 1 0] break
	    #set base_height 0
	} else {
	    set base_width 0
	    foreach "- - - base_height" [grid bbox $win.f 0 0 0 1] break
	}
	if { [winfo exists $win.f.db] } {
	    incr base_height [winfo reqheight $win.f.db]
	}
	foreach i $tabs {
	    set index [$self index $i]
	    if { [dict get $tab_options($index) -state] eq "hidden" } { continue }
	    if { [winfo reqwidth $i]+$base_width > [$win.f cget -width] } {
		$win.f configure -width [expr {[winfo reqwidth $i]+$base_width}]
	    }
	    if { [winfo reqheight $i]+$base_height > [$win.f cget -height] } {
		$win.f configure -height [expr {[winfo reqheight $i]+$base_height}]
	    }
	}
	if { [winfo reqheight $win] < [winfo reqheight $win.f] } {
	    $win configure -height [winfo reqheight $win.f]
	} elseif { [llength [$win panes]] == 1 && [winfo reqheight $win] > [winfo reqheight $win.f] } {
	    $win configure -height [winfo reqheight $win.f]
	}
	$self view_database_list_check
    }
    method _pn_select { what args } {
	switch $what {
	    previous {
		set tab [expr {$current_tab-1}]
		while { $tab >= 0 && [$self tab $tab -state] ne "normal" } {
		    incr tab -1
		}
		if { $tab < 0 } { bell ; return }
		$self select $tab
	    }
	    next {
		set len [llength $tabs]
		set tab [expr {$current_tab+1}]
		while { $tab < $len && [$self tab $tab -state] ne "normal" } {
		    incr tab 1
		}
		if { $tab >= $len } { bell ; return }
		$self select $tab
	    }
	    start {
		set len [llength $tabs]
		set tab 0
		while { $tab < $len && [$self tab $tab -state] ne "normal" } {
		    incr tab 1
		}
		if { $tab >= $len } { bell ; return }
		$self select $tab
	    }
	    last {
		set tab [expr {[llength $tabs]-1}]
		while { $tab >= 0 && [$self tab $tab -state] ne "normal" } {
		    incr tab -1
		}
		if { $tab < 0 } { bell ; return }
		$self select $tab
	    }
	    tree {
		set item [lindex $args 1]
		set text [$tree item text $item 0]
		for { set tab 0 } { $tab < [llength $tabs] } { incr tab } {
		    if { $text eq [dict_getd $tab_options($tab) -text ""] } {
		        break
		    }
		}
		if { $tab >= [llength $tabs] } { set tab 0 }
		if { $tab != [$self index current] } {
		    $self select $tab
		}
	    }
	}
	$self _compute_size
    }
    method _fill_go_menu { menu } {
	$menu delete 0 end
	for { set i 0 } { $i < [llength $tabs] } { incr i } {
	    if { [dict get $tab_options($i) -state] eq "hidden" } { continue }
	    if { $i == $current_tab } {
		$menu add checkbutton -command [mymethod select $i] \
		    -variable kk
		set ::kk 1
	    } else {
		$menu add command -command [mymethod select $i]
	    }
	    foreach "v l" [list -text -label -image -image -compound -compound \
		    -state -state] {
		if { [dict exists $tab_options($i) $v] } {
		    $menu entryconfigure end $l [dict get $tab_options($i) $v]
		}
	    }
	}
	if { [info exists db_list] } {
	    $menu add separator
	    $menu add command -label [_ "Database List"] -command \
		[mymethod view_database_list show_list]
	}

    }
    variable _clear_save_entries_recursive_dict
    method _count_entries_recursive { w } {
	set num 0
	set err [catch { $w get } content]
	if { $err } {
	    set err [catch { $w get 1.0 end-1c } content]
	}
	if { !$err } {
	    incr num
	}
	foreach wc [winfo children $w] {
	    incr num [$self _count_entries_recursive $wc]
	}
	return $num
    }
    method _clear_save_entries_recursive { w } {

	set err [catch { $w get } content]
	if { $err } {
	    set err [catch { $w get 1.0 end-1c } content]
	}
	if { !$err } {
	    dict set _clear_save_entries_recursive_dict $w $content
	    catch { $w delete 0 end }
	    catch { $w ClearText }
	}
	foreach wc [winfo children $w] {
	    $self _clear_save_entries_recursive $wc
	}
    }
    method _restore_entries_recursive { w } {

	if { [dict exists $_clear_save_entries_recursive_dict $w] } {
	    set v [dict get $_clear_save_entries_recursive_dict $w]
	    catch {
		$w delete 0 end
		$w insert end $v
	    }
	    catch {
		$w ClearText
		$w ApplyXMLdataAsText $v
	    }
	}
	foreach wc [winfo children $w] {
	    $self _restore_entries_recursive $wc
	}
    }
    variable db_update_idle
    # manage database
    method manage_database { args } {
	set db $options(-lognoter_db)
	set table $options(-database)

	set optional {
	}
	set compulsory "what"
	set args [parse_args -raise_compulsory_error 0 $optional \
		$compulsory $args]
	
	switch $what {
	    create - edit - edit_multiple {
		if { [dict exists $options(-create_edit_delete_cmds) $what] } {
		    set cmd [dict get $options(-create_edit_delete_cmds) $what]
		    if { $cmd eq "-" } { return }
		    uplevel #0 $options(-eval_command) $cmd
		    return
		}
		set cols [$db sql column_names $table]
		set idList ""
		if { $what ne "edit_multiple" && [lindex $cols 3] eq "id_name" } {
		    set current_id_name [string trim $current_id_name]
		    if { $current_id_name eq "" } {
		        if { $what eq "create" } {
		            set t [_ "It is necessary to enter a name for the new entry"]
		        } else {
		            set t [_ "It is necessary to enter the name of the entry to be modified"]
		        }
		        snit_messageBox -message $t -parent $win
		        return
		    }
		    set cmd "select id_name from \"$table\" where id_name=
		        '[$db sql escape $current_id_name]'"
		    set existing_id_name [$db sql onecolumn $cmd]
		    
		    if { $existing_id_name ne "" && $what eq "create" } {
		        set t [_ "There is already one entry with name '%s'. Modify it?" \
		                $existing_id_name]
		        set retval [snit_messageBox -default ok -icon question -message $t \
		                -type okcancel -parent $win]
		        if { $retval eq "cancel" } { return }
		    } elseif { $existing_id_name eq "" && $what eq "edit" } {
		        set t [_ "There is no entry with name '%s'. It cannot be modified" \
		                $current_id_name]
		        snit_messageBox -message $t -parent $win
		        return
		    }
		} elseif { $what in "edit edit_multiple" } {
		    set idList ""
		    foreach item [$db_list selection get] {
		        if { [$db_list item text $item 0] eq "" } { continue }
		        lappend idList [$db_list item text $item 0]
		    }
		    if { [llength $idList] == 0 } {
		        set cmd "select id from [$db sql fs0 $table] limit 2"
		        set id [$db sql sel $cmd]
		        if { [llength $id] == 1 } {
		            set idList [list $id]
		        } else {
		            snit_messageBox -message \
		                [_ "It is necessary to select one entry in order to modify it"] \
		                -parent $win
		            return
		        }
		    }
		}
		if { [llength $idList] > 1 } { set what edit_multiple }
		
		if { $what ne "edit_multiple" && [lindex $cols 3] eq "id_name" } {
		    set cols_r [lrange $cols 4 end]
		    set valuesList [lrepeat [expr {[llength $cols]-4}] null]                    
		} else {
		    set cols_r [lrange $cols 3 end]
		    set valuesList [lrepeat [expr {[llength $cols]-3}] null]
		}
		if { $what eq "edit_multiple" } {
		    set colsT ""
		    set colsTdict ""
		    set numContainers [llength [$options(-rootnode) selectNodes {.//container}]]
		    set usedContainers ""
		    foreach node [$options(-rootnode) selectNodes {.//param}] {
		        if { [$node @n] ni $cols_r } { continue }
		        set containerNode [$node selectNodes {ancestor::container}]
		        if { $numContainers > 1 } {
		            if { ![dict exists $usedContainers [$containerNode @n]] } {
		                set pn [formulae::node_pn $containerNode]
		                lappend colsT [list $pn off "" open]
		                dict set usedContainers [$containerNode @n] $pn
		            }
		            set parent [dict get $usedContainers [$containerNode @n]]
		        } else {
		            set parent ""
		        }
		        set col_pn [formulae::node_pn $node]
		        dict set colsTdict [list $col_pn $parent] [$node @n]
		        lappend colsT [list $col_pn off $parent open]
		    }
#                     set xp0 {//param[@n=%s]|//condition[@n=%s]|//container[@n=%s]}
#                     foreach col $cols_r {
#                         if { $col eq "id_name" } { continue }
#                         if { [regexp {__(contents|cdate|mdate|type|size|path)$} $col] } {
#                             continue
#                         }
#                         set xp [format_xpath $xp0 $col $col $col]
#                         set node [$options(-rootnode) selectNodes $xp]
#                         if { $node ne "" } {
#                             set col_pn [formulae::node_pn $node]
#                         } else {
#                             set col_pn $col
#                         }
#                         dict set colsTdict $col_pn $col
#                         lappend colsT [list $col_pn off "" open]
#                     }
		    destroy $win._ask
		    set w [dialogwin_snit $win._ask -title [_ "Select fields to update"] -entrytext \
		            [_ "Select fields to update (Modifying %d entries)" [llength $idList]]:]
		    set f [$w giveframe]
		    
		    cu::check_listbox $f.cb1 -values $colsT -width 200

		    grid $f.cb1 -sticky nsew -pady 2
		    grid columnconfigure $f 0 -weight 1
		    grid rowconfigure $f 1 -weight 1
		    tk::TabToWindow $f.cb1
		    bind [winfo toplevel $f] <Return> [list $w invokeok]
		    set action [$w createwindow]
		    set colsT [$f.cb1 cget -values] 
		    destroy $w
		    if { $action < 1 } { return }
		    
		    set colsM ""
		    foreach c $colsT {
		        if { [lindex $c 1] eq "on" } {
		            set key [list [lindex $c 0] [lindex $c 2]]
		            lappend colsM [dict get $colsTdict $key]
		        }
		    }
		}
		set err [catch $options(-actualize_callback) ret]
		if { $err } { return }
		lassign $ret values values_moredata

		dict for "n v" $values {
		    set ipos [lsearch -exact $cols_r $n]
		    if { $ipos == -1 } { continue }
		    if { $what eq "edit_multiple" && $n ni $colsM } { continue }

		    set v [string trim $v]

		    set xp [format_xpath {string(//param[@n=%s]/@field_type)} $n]
		    set field_type [$options(-rootnode) selectNodes $xp]
		    if { $field_type eq "file" || $field_type eq "directory" } {                        
		        set err [catch { dict get $values_moredata $n contents } c]                                               
		        set xp_store_int [format_xpath {string(//param[@n=%s]/@store_internally)} $n]                                             
		        set store_internally [$options(-rootnode) selectNodes $xp_store_int]                      
		        if { $store_internally == 1 || $store_internally == "" } {
		            if { $err || $c eq "" } {
		                set v ""
		            } elseif { $v eq "" } {
		                dict set values_moredata $n ""
		            }                      
		        } 
		    }
		    if { $field_type in [list "" numeric expression] &&
		        [string is double -strict $v] && $v > 1e-100 && $v < 1e100 } {
		        lset valuesList $ipos [$db sql escape [format %.6g $v]]
		    } else {
		        lset valuesList $ipos '[$db sql escape $v]'
		    }
		}
		dict for "n v" $values_moredata {                                       
		    dict for "n2 v2" $v {
		        set nfull "${n}__$n2"
		        set ipos [lsearch -exact $cols_r $nfull]
		        if { $ipos == -1 } { continue }
		        if { $what eq "edit_multiple" && $nfull ni $colsM } { continue }
		        lset valuesList $ipos '[$db sql escape $v2]'
		    }
		}
		$db sql begin
		if { $what ne "edit_multiple" && [lindex $cols 3] eq "id_name" } {
		    set cmd "select id from \"$table\" where id_name=
		        '[$db sql escape $current_id_name]'"
		    set id [$db sql onecolumn $cmd]
		    if { $id eq "" } {
		        set cmd "select coalesce(max(id),0)+1 from [$db sql fs0 $table]"
		        set newid [$db sql onecolumn $cmd]
		        set cmd "insert into [$db sql fs0 $table] (\"[join $cols \",\"]\") values(
		            $newid,now(),now(),'[$db sql escape $current_id_name]',"
		        append cmd [join $valuesList ,] ")"
		        $db sql exec $cmd
		        set t [_ "Created new entry with name '%s'" $current_id_name]
		    } else {
		        set cmd "update [$db sql fs0 $table] set [lindex $cols 2]=now()"
		        append cmd ",id_name='[$db sql escape $current_id_name]'"
		        foreach n [lrange $cols 4 end] v $valuesList {
		            if { $v ne "null" } {
		                append cmd ",[$db sql fs0 $n]=$v"
		            }
		        }
		        append cmd " where id=$id"
		        $db sql exec $cmd
		        set t [_ "Modified entry with name '%s'" $current_id_name]
		    }
		} elseif { $idList eq "" } {
		    set cmd "select coalesce(max(id),0)+1 from [$db sql fs0 $table]"
		    set newid [$db sql onecolumn $cmd]
		    set cmd "insert into [$db sql fs0 $table] (\"[join $cols \",\"]\") values(
		        $newid,now(),now(),"
		    append cmd [join $valuesList ,] ")"
		    $db sql exec $cmd
		    set t [_ "Created new entry"]
		} else {
		    foreach id $idList {
		        set cmd "update [$db sql fs0 $table] set [lindex $cols 2]=now()"
		        foreach n [lrange $cols 3 end] v $valuesList {
		            if { $v ne "null" } {
		                append cmd ",[$db sql fs0 $n]=$v"
		            }
		        }
		        append cmd " where id=$id"
		        $db sql exec $cmd
		    }
		    if { [llength $idList] == 1 } {
		        set t [_ "Updated selected entry"]
		    } else {
		        set t [_ "Updated %d selected entries" [llength $idList]]
		    }
		}
		$db sql commit
		$self manage_database display_warning long $t
		$self manage_database update
	    }
	    selected_row {
		if { [info exists current_id_name] } {
		    set current_id_name [$db_list item text [lindex $args 1 0] 1]
		}
	    }
	    display_warning {
		lassign [lrange $args 0 1] long_short txt
		set focus [focus]
		ttk::frame $win.warning -style WNtoolbar -padding 2
		set width [expr {[winfo width $win.f.db]-10}]
		ttk::label $win.warning.l -text $txt \
		        -style WNbutton -wraplength $width -justify left
		
		ttk::button $win.warning.b2 -image [cu::get_icon remove-16] \
		    -style WNbutton \
		    -command [mymethod manage_database display_warning_end $focus]
		
		foreach i [list Escape Return] {
		    bind $win.warning <$i> [mymethod manage_database display_warning_end \
		            $focus]
		}

		grid $win.warning.l $win.warning.b2 -sticky w -pady 0
		grid configure $win.warning.l -padx 2
		
		place $win.warning -in $win.f.db -x 0 -rely 1 -y 0 -anchor sw
		tkwait visibility $win.warning
		grab $win.warning
		focus $win.warning
		switch $long_short {
		    short { set time 1000 }
		    long { set time 2000 }
		}
		after $time [mymethod manage_database display_warning_end $focus]
	    }
	    display_warning_end {
		if { ![winfo exists $win.warning] } { return }
		destroy $win.warning
		set focus [lindex $args 0]
		if { [winfo exists $focus] } {
		    focus $focus
		}
	    }
	    fill_entries_default {
		if { ![info exists db_list] } { return }
		if { [llength [$db_list item children 0]] == 0 } { return }
		TreeCtrl::SetActiveItem $db_list 1
		$self manage_database fill_entries_do
	    }
	    check_has_changes_only {
		if { ![info exists db_list] } { return 0 }
		if { [llength [$db_list item children 0]] == 0 } { return 0 }
		if { [llength [$db_list selection get]] != 1 } { return 0 }
		return [$self manage_database fill_entries_do -check_has_changes_only]
	    }
	    fill_entries {
		# DISCONNECTED TEMPORALLY
#                set ret [$self manage_database fill_entries_do -check_has_changes_only]
#                 if { $ret } {
#                     set text [_ "The form has changes in its values. Do you want to proceed and loose any modifications?"]
#                     set retval [snit_messageBox -default ok -icon question \
#                             -message $text -type okcancel \
#                             -parent $win]
#                     if { $retval eq "cancel" } {
#                         return
#                    }
#                }
		return [$self manage_database fill_entries_do]
	    }            
	    fill_entries_do {
		set check_has_changes_only 0
		set mod [lindex $args 0]
		if { $mod eq "-searching" } {
		    set items [$db_list item children 0]
		} elseif { $mod eq "-check_has_changes_only" } {
		    set check_has_changes_only 1
		    if { $last_fill_entries_item ne "" } {
		        set items [list $last_fill_entries_item]
		    } else {
		        return 0
		        #set items [$db_list selection get]
		    }
		} else {
		    set items [$db_list selection get]
		}
		if { [llength $items] == 0 && [$db_list item count] == 2 } {
		    $db_list selection add "first visible"
		    set items [$db_list selection get]
		}
		if { [llength $items] == 0 } {
		    if { $mod eq "-searching" } {
		        set t [_ "There are no entries that fullfill this search criteria"]
		        $self manage_database display_warning short $t
		    } else {
		        set t [_ "It is necessary to select one entry in order to get its values"]
		        snit_messageBox -message $t -parent $win
		    }
		    return
		}
		if { [llength $items] > 1 } {
		    if { $mod eq "-searching" } {
		        set t [_ "There are %d entries that fullfill this search criteria" \
		                [llength $items]]
		        $self manage_database display_warning short $t
		    } else {
		        set t [_ "It is necessary to select only one entry in order to get its values"]
		        snit_messageBox -message $t -parent $win
		    }
		    return
		}
		set item [lindex $items 0]
		set id [$db_list item text $item 0]
		if { [string trim $id] eq "" } { set id 1 }

		set cols [$db sql column_names $table]
		set valuesList [$db sql sel "select * from [$db sql fs0 $table] where id=$id"]
		if { $valuesList eq "" } {
		    return 0
		}
		if { $mod eq "" } {
		    set last_fill_entries_item $item
		}
		lassign "" input_values input_values_more
		
		if { [lindex $cols 3] eq "id_name" } {
		    set current_id_name [lindex $valuesList 3]
		    set idx 4
		} else {
		    set idx 3
		}
		foreach col [lrange $cols $idx end] {
		    if { [regexp {(.*)__(.*)} $col {} name subname] } {
		        dict set input_values_more $name $subname [lindex $valuesList $idx]
		    } else {
		        dict set input_values $col [lindex $valuesList $idx]
		    }
		    incr idx
		}

		if { $search_state in "any name" } {
		    $self manage_database search_$search_state
		}
		if { $search_state == 1 } {
		    $self manage_database search_in_fields
		}
		return [uplevel #0 $options(-fill_form_callback) [list \
		            $check_has_changes_only $input_values $input_values_more]]
	    }
	    delete {
		if { [dict exists $options(-create_edit_delete_cmds) $what] } {
		    set cmd [dict get $options(-create_edit_delete_cmds) $what]
		    if { $cmd eq "-" } { return }
		    uplevel #0 $options(-eval_command) $cmd
		    return
		}
		set where [lindex $args 0]
		set items [$db_list selection get]
		if { [llength $items] == 0 } {
		    snit_messageBox -message \
		        [_ "It is necessary to select one or more entries in order to delete them"] \
		        -parent $win
		    return
		}
		set existing_id_name ""
		if { $where eq "in_menu" && [info exists current_id_name] } {
		    set current_id_name [string trim $current_id_name]
		    if { $current_id_name eq "" } {
		        set t [_ "It is necessary to enter a name for deleting its entry"]
		        snit_messageBox -message $t -parent $win
		        return
		    }
		    set cmd "select id_name from \"$table\" where id_name=
		        '[$db sql escape $current_id_name]'"
		    set existing_id_name [$db sql onecolumn $cmd]
		    if { $existing_id_name eq "" } {
		        set t [_ "Entry with name '%s' does not exist in the database"]
		        snit_messageBox -message $t -parent $win
		        return
		    }
		    set text [_ "Are you sure to delete entry '%s'?" $existing_id_name]
		} elseif { [llength $items] == 1 } {
		    set text [_ "Are you sure to delete one entry?"]
		} else {
		    set text [_ "Are you sure to delete %d entries?" [llength $items]]
		}
		set retval [snit_messageBox -default cancel -icon question -message $text \
		        -type okcancel -parent $win]
		if { $retval == "cancel" } { return }
		
		$db sql exec "begin"
		if { $existing_id_name ne "" } {
		    $db sql exec "delete from \"$table\" where id_name=
		        '[$db sql escape $current_id_name]'"
		} else {
		    foreach item $items {
		        set id [$db_list item text $item 0]
		        $db sql exec "delete from \"$table\" where id=$id"
		    }
		}
		$db sql exec "commit"
		$self manage_database update
	    }
	    search_any - search_name {
		if { $search_state == 1 } {
		    $self manage_database search_in_fields
		}
		if { $search_state in "any name" } {
		    set height0 [winfo reqheight $win.f.db]
		    destroy $win.f.db.searchframe
		    update idletasks
		    set height [winfo reqheight $win.f.db]
		    #$win sashpos 0 [expr {[$win sashpos 0]+$height-$height0}]
		    $win sash place 0 0 [expr {[lindex [$win sash coord 0] 1]+$height-$height0}]
		    
		    set search_state 0
		    $self manage_database update
		    
		    if { [winfo exists $save_focus] } {
		        focus $save_focus
		    }
		    set save_focus ""
		    return
		}
		switch $what {
		    search_any { set search_state any }
		    search_name { set search_state name }
		}
		set save_focus [focus]
		
		ttk::frame $win.f.db.searchframe  \
		    -style WNtoolbar -padding 0
		ttk::label $win.f.db.searchframe.l -text [_ "Search"]: \
		    -style WNbutton
		ttk::entry $win.f.db.searchframe.e -textvariable [myvar searchstring]
		ttk::button $win.f.db.searchframe.b1 -image [cu::get_icon kfind-16] \
		    -style WNbutton \
		    -command [mymethod manage_database update_idle]
		ttk::button $win.f.db.searchframe.b2 -image [cu::get_icon remove-16] \
		    -style WNbutton \
		    -command [mymethod manage_database $what]

		tooltip $win.f.db.searchframe.b1 [_ "Search text"]
		tooltip $win.f.db.searchframe.b2 [_ "Close search window"]

		bind $win.f.db.searchframe.e <KeyPress> [mymethod manage_database update_idle]
		bind $win.f.db.searchframe.e <Control-s> [mymethod manage_database $what]
		bind $win.f.db.searchframe.e <Escape> [mymethod manage_database $what]
		bind $win.f.db.searchframe.e <Return> [mymethod manage_database fill_entries_do -searching]

		set height0 [winfo reqheight $win.f.db]
		
		grid $win.f.db.searchframe.l $win.f.db.searchframe.e \
		    $win.f.db.searchframe.b1 $win.f.db.searchframe.b2 -sticky w -pady 1
		grid configure $win.f.db.searchframe.l -padx 2
		grid $win.f.db.searchframe -row 1 -column 0 -columnspan 4 \
		    -padx 2 -pady "0 2" -sticky w
		focus $win.f.db.searchframe.e
		update idletasks

		set height [winfo reqheight $win.f.db]
		#$win sashpos 0 [expr {[$win sashpos 0]+$height-$height0}]
		$win sash place 0 0 [expr {[lindex [$win sash coord 0] 1]+$height-$height0}]
	    }
	    can_have_search_fields {
		set num 0
		foreach w $tabs {
		    incr num [_count_entries_recursive $w]
		}
		return [expr {$num > 0}]
	    }
	    search_in_fields {
		if { $search_state in "any name" } {
		    $self manage_database search_$search_state
		}
		if { $search_state == 0 } {
		    set _clear_save_entries_recursive_dict ""
		    foreach w $tabs {
		        $self _clear_save_entries_recursive $w
		    }
		    
		    ttk::frame $win.f.db.searchframe  \
		        -style WNtoolbar -padding 0
		    ttk::label $win.f.db.searchframe.l -text [_ "Enter the search text in any of the fields"] \
		        -style WNbutton
		    
		    ttk::button $win.f.db.searchframe.b2 -image [cu::get_icon remove-16] \
		        -style WNbutton \
		        -command [mymethod manage_database search_in_fields]

		    tooltip $win.f.db.searchframe.b2 [_ "Close search window"]
		    
		    set height0 [winfo reqheight $win.f.db]

		    grid $win.f.db.searchframe.l $win.f.db.searchframe.b2 -sticky w -pady 0
		    grid configure $win.f.db.searchframe.l -padx 2

		    grid $win.f.db.searchframe -row 1 -column 0 -columnspan 4 \
		        -padx 2 -pady "0 2"
		    update idletasks
		    
		    set height [winfo reqheight $win.f.db]
		    #$win sashpos 0 [expr {[$win sashpos 0]+$height-$height0}]
		    $win sash place 0 0 [expr {[lindex [$win sash coord 0] 1]+$height-$height0}]
		    
		    bind notebook_children <KeyPress> "[mymethod manage_database update_idle]"
		    bind notebook_children <Escape> "[mymethod manage_database search_in_fields] ;break"
		    bind notebook_children <Return> "[mymethod manage_database fill_entries_do -searching] ;break"
		    set search_state 1
		} else {
		    foreach w $tabs {
		        $self _restore_entries_recursive $w
		    }
		    set height0 [winfo reqheight $win.f.db]
		    destroy $win.f.db.searchframe
		    update idletasks
		    set height [winfo reqheight $win.f.db]
		    #$win sashpos 0 [expr {[$win sashpos 0]+$height-$height0}]
		    $win sash place 0 0 [expr {[lindex [$win sash coord 0] 1]+$height-$height0}]
		    
		    bind notebook_children <KeyPress> ""
		    bind notebook_children <Escape> ""
		    bind notebook_children <Return> ""
		    set search_state 0
		    $self manage_database update
		}
	    }
	    update_idle {
		if { [info exist db_update_idle] } { after cancel $db_update_idle }
		set db_update_idle [after 100 [list catch [mymethod manage_database update]]]
	    }
	    update {
		set optional {
		    { -see item "" }
		    { -see_entries boolean "" }
		    { -see_more_entries boolean 0 }
		    { -if_table_is table_name "" }
		    { -searchstring_any string "" }
		}
		set compulsory ""
		parse_args $optional $compulsory $args
		
		if { $if_table_is ne "" && $if_table_is ne $table } { return }
		
		if { $searchstring_any ne "" } {
		    set search_state "any"
		    set searchstring $searchstring_any
		}
		if { $see_more_entries } {
		    set fill_database_entries_limit [expr {$fill_database_entries_limit*2}]
		}
		if { $see_entries ne "" } {
		    set fill_database_entries $see_entries
		}
		if { [info exist db_update_idle] } {
		    after cancel $db_update_idle
		    unset db_update_idle
		}
		if { [llength [$db_list item children 0]] && [$db_list item style set 1 0] eq "window" } {
		    set is_button 1
		} else {
		    set is_button 0
		}
		set sort_column [$db_list sort_column]
		set yview [$db_list yview]
		set sel_ids ""
		if { !$is_button } {
		    foreach item [$db_list selection get] {
		        set i [$db_list item text $item 0]
		        if { ![string is integer -strict $i] } { continue }
		        lappend sel_ids $i
		    }
		    set sel_ids [lsort -integer $sel_ids]
		}
		
		$db_list item delete all

		if { ![$db sql table_exists $table] } {
		    return
		}
		set cols [$db sql column_names $table]
		set selcols [lrange $cols 0 2]
		if { $options(-db_view_dates) } {
		    set sumcols [list "" "" ""]
		} else {
		    set sumcols [list ""]
		}
		set xp0 {//param[@n=%s]|//condition[@n=%s]}
		foreach col [lrange $cols 3 end] {
		    if { ![regexp {(.*)__(.*)} $col {} name subname] } {
		        lappend selcols $col
		        set xp [format_xpath $xp0 $col $col]
		        set node [$options(-rootnode) selectNodes $xp]
		        set sumcol ""
		        if { $node ne "" } {
		            if { [$node @field_type ""] eq "numeric" } {
		                set sumcol $col
		            }
		        }
		        lappend sumcols $sumcol
		    }
		}
		lassign "" cndList where
		if { $search_state eq "name" } {
		    set v [string trim $searchstring]
		    if { $v ne "" } {
		        foreach pat [cu::sql_patterns_list $v] {
		            lappend cndList "id_name like [$db sql vs0 $pat]"
		        }
		    }
		    if { [llength $cndList] } {
		        set where [join $cndList " or "]
		    }
		} elseif { $search_state eq "any" } {
		    set v [string trim $searchstring]
		    if { $v ne "" } {
		        foreach col [concat id [lrange $cols 3 end]] {
		            if { [regexp {__} $col] } { continue }
		            foreach pat [cu::sql_patterns_list $v] {
		                lappend cndList "[$db sql fs0 $col] like [$db sql vs0 $pat]"
		            }
		        }
		    }
		    if { [llength $cndList] } {
		        set where [join $cndList " or "]
		    }
		} elseif { $search_state == 1 } {
		    foreach i [dict keys $_clear_save_entries_recursive_dict] {
		        set err [catch { $i cget -textvariable } var]
		        if { !$err && [regexp {uvalues\(.*,(.*)\)} $var {} fieldname] } {
		            set ipos [lsearch -exact $cols $fieldname]
		            if { $ipos != -1 } {
		                set v [string trim [$i get]]
		            } else { set err 1 }
		        } else { set err 1 }
		        if { $err } {
		            set err [catch { $i cget -clientdata } fieldname]
		            if { !$err } {
		                set ipos [lsearch -exact $cols $fieldname]
		                if { $ipos != -1 } {
		                    set v [$i get 1.0 end-1c]
		                } else { set err 1 }
		            }
		        }
		        if { $err } { continue }
		        set v [string trim $v]
		        if { $v eq "" } { continue }
		        foreach pat [cu::sql_patterns_list $v] {
		            lappend cndList "[$db sql fs0 $fieldname] like [$db sql vs0 $pat]"
		        }
		    }
		    if { [llength $cndList] } {
		        set where [join $cndList " and "]
		    }
		} else {
		    set cmd "select [$db sql fs $selcols] from [$db sql fs0 $table]"
		}
		set cmd "select [$db sql fs $selcols] from [$db sql fs0 $table]"
		if { $where ne "" } {
		    append cmd " where $where"
		}
		append cmd " order by id"
		
		if { $fill_database_entries != 1 } {
		    append cmd " limit $fill_database_entries_limit"
		} elseif { [info exists ::ispocket] && $::ispocket } {
		    append cmd " limit 20"
		}
		if { [lindex $cols 3] eq "id_name" } {
		    set __has_id_name 1
		} else {
		    set __has_id_name 0
		}
		set __num 0
		set __values ""
		set __selcols $selcols
		set __db_list $db_list
		set __sel_ids $sel_ids
		$db sql maplist $cmd {
		    set __v [list $id]
		    if { $options(-db_view_dates) } {
		        lappend __v $creation_date $modification_date
		    }
		    foreach __i [lrange $__selcols 3 end] {
		        if { [string length $__i] > 200 } {
		            set __i "[string range $__i 0 170] ([string length $__i] [_ letters])..."
		        }
		        lappend __v [set $__i]
		    }
		    $__db_list insert end $__v
		    if { [lsearch -sorted -integer $__sel_ids $id] != -1 } {
		        $__db_list selection add end
		    }
		    if { $__has_id_name } { lappend __values [lindex $__v 1] }
		    incr __num
		}
		if { $fill_database_entries != 1 && $__num == $fill_database_entries_limit } {
		    button $db_list.message1 -text [_ "See more entries"] \
		        -foreground blue -background white -bd 0 -cursor hand2 \
		        -command [mymethod manage_database update -see_more_entries 1]
		    set font [concat [font actual [$db_list.message1 cget -font]] -underline 1]
		    $db_list.message1 configure -font $font
		    button $db_list.message2 -text [_ "See all entries"] \
		        -foreground blue -background white -bd 0 -cursor hand2 -font $font \
		        -command [mymethod manage_database update -see_entries 1]
		        
		    set item [$db_list insert end ""]
		    $db_list item style set $item 0 window
		    set span [expr {[llength $selcols]-2}]
		    if { $span > 2 } { set span 2 }
		    $db_list item span $item 0 $span
		    $db_list item element configure $item 0 e_window -destroy 1 \
		        -window $db_list.message1
		    set col $span
		    if { $col > [llength $selcols]-2 } {
		        set col [expr {[llength $selcols]-2}]
		    }
		    $db_list item style set $item $col window
		    $db_list item span $item $col 2
		    $db_list item element configure $item $col e_window -destroy 1 \
		        -window $db_list.message2
		}
		set current_id_values $__values
		
		if { [$options(-rootnode) @sum_numeric_columns ""] ne "" } {
		    set format [$options(-rootnode) @sum_numeric_columns ""]
		    lassign "" sumcols_sql sumcols_tot_sqlList
		    foreach col $sumcols {
		        if { $col eq "" } {
		            lappend sumcols_sqlList "''"
		        } else {
		            lappend sumcols_sqlList "sum([$db sql fs0 $col])"
		            lappend sumcols_tot_sqlList [$db sql fs0 $col]
		        }
		    }
		    set cmd "select [join $sumcols_sqlList ,]"
		    if { [llength $sumcols_tot_sqlList] == 2 } {
		        append cmd ",sum([join $sumcols_tot_sqlList -])"
		    }
		   append cmd " from [$db sql fs0 $table]"
		    if { $where ne "" } {
		        append cmd " where $where"
		    }
		    lassign "" list sum
		    set vs [$db sql sel $cmd]
		    if { [llength $vs] > [llength $sumcols] } {
		        set sum [lindex $vs end]
		        set vs [lrange $vs 0 end-1]
		    }
		    for { set i 0 } { $i < [llength $vs ] } { incr i } {
		        set v  [lindex $vs $i]
		        if { $v ne "" } {
		            lappend list [format $format $v]
		        } elseif { [lindex $vs $i+1] ne "" } {
		            lappend list [_ "Sum"]:
		        } elseif { [lindex $vs $i-1] ne "" && $sum ne "" } {
		            lappend list [format $format $sum]
		            set sum ""
		        } else {
		            lappend list ""
		        }
		    }
		    if { [llength $list] } {
		        set item [$db_list insert end $list]
		        $db_list item state set $item emphasis
		        if { $see eq "" } {
		            set see $item
		        }
		    }
		}
		$db_list yview moveto [lindex $yview 0]
		if { $see ne "" } {
		    update idletasks
		    $db_list see $see
		}
		if { [llength [$db_list selection get]] == 0 } {
		    catch { $db_list selection add "first visible" }
		}
	    }
	    contextual {
		lassign $args - - x y
		destroy $win._cmenu
		set menu [menu $win._cmenu -tearoff 0]
		
		$menu add command -label [_ "Fill form"] -command [mymethod \
		        manage_database fill_entries]
		$menu add command -label [_ "Search"] -command [mymethod \
		        manage_database search_any]
		$menu add checkbutton -label [_ "View dates"] -variable \
		    [myvar options(-db_view_dates)] -command \
		    [mymethod _configure_database -force]
		
		if { $options(-main_state) eq "normal" } {
		    $menu add command -label [_ "SQL table schema"] -command \
		        [mymethod manage_database output_schema]
		}
		$menu add separator
		
		if { $options(-contextual_add_callback) ne "" } {
		    uplevel #0 $options(-contextual_add_callback) [list $menu]
		}
		
		$menu add command -label [_ "Select all"] -command [mymethod \
		        manage_database select_all]
		$menu add command -label [_ "Copy"] -command [mymethod \
		        manage_database copy]
		$menu add command -label [_ "Paste"] -command [mymethod \
		        manage_database paste]
		$menu add separator
		$menu add cascade -label [_ "Move entry to"] -menu $menu.m1
		menu $menu.m1 -tearoff 0
		foreach i [list first prev next last] j [list [_ "First"] [_ "Previous"] \
		        [_ "Next"] [_ "Last"]] {
		    $menu.m1 add command -label $j -command \
		        [mymethod manage_database move_rows $i]
		}
		$menu add command -label [_ "Reorder ids"] -command [mymethod \
		        manage_database move_rows reorder_ids]
		$menu add separator
		$menu add command -label [_ "Delete"] -command \
		    [mymethod manage_database delete in_tree]
		
		tk_popup $menu $x $y
	    }
	    move_rows {
		set where [lindex $args 0]
		set formulaeNode $options(-rootnode)
		set sql_table [$db sql fs0 [$formulaeNode @database]]
		set sel_ids ""
		foreach item [$db_list selection get] {
		    lappend sel_ids [$db_list item text $item 0]
		}
		if { [llength $sel_ids] == 0 && $where ne "reorder_ids" } { return }
		set sel_ids [lsort -integer $sel_ids]
		
		if { $where eq "reorder_ids" } {
		    set sel_ids [list ""]
		}
		$db sql exec begin
		while { [llength $sel_ids] } {
		    set id0_from [lindex $sel_ids 0]
		    for { set i 1 } { $i < [llength $sel_ids] } { incr i } {
		        if { [lindex $sel_ids $i] > [lindex $sel_ids $i-1]+1 } { break }
		    }
		    set id1_from [lindex $sel_ids $i-1]
		    if { $id0_from eq "" } {
		        set num 0
		    } else {
		        set num [expr {$id1_from-$id0_from+1}]
		    }
		    set sel_ids [lrange $sel_ids $i end]
		    set cmd "select max(id) from $sql_table"
		    set id_last [$db sql onecolumn $cmd]
		    switch $where {
		        first { set id0_to 1 }
		        last { set id0_to [expr {$id_last-$num+1}] }
		        prev {
		            set id0_to [$db sql onecolumn "select count(*) from $sql_table
		                    where id < $id0_from"]
		            if { $id0_to == 0 } { set id0_to 1 }
		        }
		        next {
		            set id0_to [$db sql onecolumn "select count(*)+1 from $sql_table
		                    where id <= $id0_from"]
		            set tot [$db sql onecolumn "select count(*) from $sql_table"]
		            if { $id0_to > $tot } { set id0_to $tot }
		        }
		        reorder_ids {
		            lassign [list 0 0 0] id0_from id1_from id0_to
		            
		        }
		    }
		    set id1_to [expr {$id0_to+$num-1}]
		    
		    set cmd "update $sql_table
		        set id=id+$id_last-$id0_from+1
		        where id >= $id0_from and id <= $id1_from"
		    $db sql exec $cmd
		    
		    set cmd "select id from $sql_table where id <= $id_last order by id"
		    set id_to 1
		    set id ""
		    $db sql maplist $cmd {
		        if { $id_to == $id0_to } {
		            set id_to [expr {$id0_to+$num}]
		        }
		        if { $id_to < $id } {
		            $db sql exec "update $sql_table set id=$id_to where id=$id"
		        } elseif { $id_to > $id } {
		            break
		        }
		        if { [set ipos [lsearch -integer -sorted $sel_ids $id]] != -1 } {
		            lset sel_ids $ipos $id_to
		        }
		        incr id_to
		        set id ""
		    }
		    if { $id ne "" } {
		        set tot [$db sql onecolumn "select count(*) from $sql_table
		                where id>=$id and id<=$id_last"]
		        set id_to [expr {$id_to+$tot-1}]
		        set cmd "select id from $sql_table where id >= $id and id <= $id_last order by id desc"
		        $db sql maplist $cmd {
		            if { $id_to > $id } {
		                $db sql exec "update $sql_table set id=$id_to where id=$id"
		            }
		            if { [set ipos [lsearch -integer -sorted $sel_ids $id]] != -1 } {
		                lset sel_ids $ipos $id_to
		            }
		            incr id_to -1
		        }
		    }
		    set cmd "update $sql_table set id=id+$id0_to-$id_last-1 where id > $id_last"
		    $db sql exec $cmd
		}
		$db sql exec commit
		$self manage_database update
	    }
	    output_schema {
		set table_schema [$db sql table_schema $table]

		destroy $win._output_schema
		set w [dialogwin_snit $win._output_schema -title \
		        [_ "SQL table schema"] -okname -]
		set f [$w giveframe]
		package require wordwidget_snit
		wordwidget_snit $f.t -width 45 -height 15  -readonly 1
		catch { $f.t configure -bg [$f cget -bg] }
		pack $f.t -fill both -expand 1
		
		set xml "<lognoter>"
		foreach line [split $table_schema \n] {
		    append xml "<para>[xml_map $line]</para>\n"
		}
		append xml "</lognoter>"
		#$f.t ApplyXMLdataAsText $xml
		after 100 [list $f.t ApplyXMLdataAsText $xml]
		set action [$w createwindow]
		destroy $w
	    }
	    select_all {
		$db_list selection add all
	    }
	    give_selection {
		if { [llength [$db_list item children 0]] && [$db_list item style set 1 0] eq "window" } {
		    return ""
		}
		set ids ""
		foreach item [$db_list selection get] {
		    lappend ids [$db_list item text $item 0]
		}
		return $ids
	    }
	    copy {
		if { [llength [$db_list item children 0]] && [$db_list item style set 1 0] eq "window" } {
		    return
		}
		clipboard clear
		
		set ids ""
		foreach item [$db_list selection get] {
		    lappend ids [$db_list item text $item 0]
		}
		set data ""
		foreach id [lsort -integer $ids] {
		    set cmd "select * from [$db sql fs0 $table] "
		    append cmd "where id=$id"
		    set list ""
		    foreach i [$db sql sel $cmd] {
		        lappend list [string trim [string map [list \\ \\\\ \n \\n \r "" \t \\t] $i]]
		    }
		    append data [join $list "\t"]\n
		}
		clipboard append $data
	    }
	    paste {
		set cols [$db sql column_names $table]
		set len_cols [llength $cols]
		
		set colsT [lrange $cols 0 2]
		set xp0 {//param[@n=%s]|//condition[@n=%s]|//container[@n=%s]}
		foreach col [lrange $cols 3 end] {
		    if { $col eq "id_name" } { set col [_ "Name id"] }
		    if { [regexp {__(contents|cdate|mdate|type|size|path)$} $col] } {
		        continue
		    }
		    set xp [format_xpath $xp0 $col $col $col]
		    set node [$options(-rootnode) selectNodes $xp]
		    if { $node ne "" } {
		        set col [formulae::node_pn $node]
		    }
		    lappend colsT $col
		}
		set err [catch { clipboard get } data]
		if { $err } { return }
		set rows [split $data "\n"]
		set col0 [split [lindex $rows 0] \t]
		set len [llength $col0]
		
		set cols_pos ""
		foreach i $col0 {
		    set ipos [lsearch -exact $cols $i]
		    if { $ipos == -1 } {
		        set ipos [lsearch -exact $colsT $i]
		    }
		    if { $ipos != -1 } { lappend cols_pos $ipos }
		}                
		if { [llength $cols_pos] != $len_cols && $len != $len_cols-2 && $len != $len_cols-3 } {
		    set t [_ "Pasted data is not correct (# columns=%d). In needs to have %d or %d columns or have a header with some column names" \
		            $len [expr {$len_cols-2}]  [expr {$len_cols-3}]]
		    snit_messageBox -message $t -parent $win
		    return
		}
		if { [llength $cols_pos] < $len_cols } {
		    set cols_pos ""
		} else {
		    set rows [lrange $rows 1 end]
		}
		set text [_ "Are you sure to add %d entries to current database?" \
		        [llength $rows]]
		set retval [snit_messageBox -default cancel -icon question -message $text \
		        -type okcancel -parent $win]
		if { $retval eq "cancel" } { return }
		$db sql begin
		set err [catch { $self manage_database paste_rows $cols_pos $rows } errstring]
		if { $err } {
		    $db sql rollback
		    set t [_ "Error pasting data (%s)" $errstring]
		    snit_messageBox -message $t -parent $win
		} else {
		    $db sql commit
		    $self manage_database update
		}
	    }
	    paste_rows {
		lassign $args cols_pos rows
		set cols [$db sql column_names $table]
		set colsR [lrange $cols 0 2]
		foreach i $cols_pos {
		    if { $i < 3 } { continue }
		    lappend colsR [lindex $cols $i]
		}
		set now [$db sql onecolumn "select now()"]
		
		set map [list \\n \n \\t \t \\\\ \\]
		
		foreach row $rows {
		    set l [split $row "\t"]
		    set lv ""
		    if { [llength $cols_pos] } {
		        set ipos [lsearch -integer $cols_pos 0]
		        if { $ipos != -1 } {
		            lappend lv [string map $map [lindex $l $ipos]]
		        } else {
		            set cmd "select coalesce(max(id),0)+1 from [$db sql fs0 $table]"
		            set newid [$db sql onecolumn $cmd]
		            lappend lv [$db sql onecolumn $cmd]
		        }
		        set ipos [lsearch -integer $cols_pos 1]
		        if { $ipos != -1 } {
		            lappend lv [string map $map [lindex $l $ipos]]
		        } else {
		            lappend lv $now
		        }
		        set ipos [lsearch -integer $cols_pos 2]
		        if { $ipos != -1 } {
		            lappend lv [string map $map [lindex $l $ipos]]
		        } else {
		            lappend lv $now
		        }
		        foreach i $cols_pos {
		            if { $i < 3 } { continue }
		            lappend lv [string map $map [lindex $l $i]]
		        }
		    } elseif { [llength $l] == [llength $cols]-3 } {
		        set cmd "select coalesce(max(id),0)+1 from [$db sql fs0 $table]"
		        set newid [$db sql onecolumn $cmd]
		        lappend lv [$db sql onecolumn $cmd] $now $now
		        foreach i $l { lappend lv [string map $map $i] }
		    } elseif { [llength $l] == [llength $cols]-2 } {
		        lappend lv [lindex $l 0] $now $now
		        foreach i [lrange $l 1 end] { lappend lv [string map $map $i] }
		    } else {
		        continue
		    }
		    set cmd "insert into [$db sql fs0 $table] ([$db sql fs $cols]) values "
		    append cmd "([$db sql vs $lv])"
		    $db sql exec $cmd
		}
	    }
	}
    }
}

################################################################################
#    create_windowD
################################################################################

proc formulae::identity { arg } { return $arg }

proc formulae::_create_interp { key lognoter lognoter_db snit_notebook database page } {
    set idx 0
    while { [interp exists formulae$idx] } { incr idx }
    set f_interp [interp create formulae$idx]
    
    $f_interp eval [list set auto_path $::auto_path]
    $f_interp eval [list set appdir ""]

    #interp alias $f_interp ::tcl::mathfunc::defaultvalue "" $f_interp::defaultvalue $root
    
    foreach i [list sum product average max min case if pi interpolate dictionary \
	    function eval_expr t_set] {
	interp alias $f_interp ::tcl::mathfunc::$i "" formulae::mathfunc_$i
    }
    interp alias $f_interp xml "" formulae::xml
    interp alias $f_interp map "" xml_map
    interp alias $f_interp format_xpath "" format_xpath
    interp alias $f_interp messageBox "" snit_messageBox
    interp alias $f_interp edit_txt_in_ramdebugger "" formulae::edit_txt_in_ramdebugger $key
    if { $snit_notebook ne "" } {
	interp alias $f_interp actualize_field "" formulae::actualize_field $key
    } else {
	$f_interp eval [list proc actualize_field { args } {}]
    }
    interp hide $f_interp package
    interp alias $f_interp package "" formulae::_interp_package $f_interp
    interp alias $f_interp _ "" _
    if { [info exists ::lognoter_debug] && $::lognoter_debug == 1 } {
	interp alias $f_interp mylog::debug "" mylog::debug
    }
    if { $lognoter_db ne "" } {
	interp alias $f_interp lognoter_db "" $lognoter_db
	interp alias $f_interp executefile "" $lognoter_db executefile
	interp alias $f_interp sql "" $lognoter_db sql
	interp alias $f_interp connect_to_mysql "" formulae::connect_to_mysql
	interp alias $f_interp sql_mysql "" formulae::sql_mysql
	interp alias $f_interp ask_for_period "" formulae::ask_for_period $key -interp $f_interp
	interp alias $f_interp query_manager "" formulae::query_manager $key
	interp alias $f_interp source_page "" formulae::source_page $key
	interp alias $f_interp edit_page_xml "" formulae::edit_page_xml $key
	interp alias $f_interp current "" formulae::identity $page
	interp alias $f_interp give_tablename "" formulae::give_tablename $lognoter_db $snit_notebook $database $page
	interp alias $f_interp update_database_table_xml "" formulae::create_update_database_table_xml $lognoter_db      
	
	if { $snit_notebook ne "" } {
	    interp alias $f_interp refresh "" $snit_notebook manage_database update
	    interp alias $f_interp give_selection "" $snit_notebook manage_database give_selection
	} else {
	    $f_interp eval [list proc refresh { args } {}]
	    $f_interp eval [list proc give_selection {} {}]
	}
    }
    if { $lognoter ne "" } {
	interp alias $f_interp showpage "" $lognoter process gotopage
    } else {
	$f_interp eval [list proc showpage { args } {}]
    }
    $f_interp eval [list namespace eval form_control {}]
    $f_interp eval [list set form_control::edit_choose_name ""]
    $f_interp eval [list set form_control::edit_choose_value ""]

    return $f_interp
}

proc formulae::give_tablename { lognoter_db snit_notebook database page args } {

    if { $snit_notebook ne "" } {
	set table [$snit_notebook cget -database]
    } else {
	set table $database
    }
    if { [llength $args] == 0 } {
	return $table
    }
    set optional {
	{ -same_level "" 0 }
    }
    set compulsory "other_page"
    parse_args $optional $compulsory $args

    if { $same_level } {
	set other_page [join [concat [lrange [split $page >] 0 end-1] [list $other_page]] >]
    }
    return [$lognoter_db default_table_for_page $other_page]
}

proc formulae::_interp_package { f_interp args } {
    
    set ret [interp invokehidden $f_interp package {*}$args]
    if { [lindex $args 0] eq "require" } {
	switch -- [lindex $args 1] {
	    "xml2pdf" {
		foreach dir [xml2pdf::GiveTemplateDirList] {
		    interp eval $f_interp [list xml2pdf::addTemplatesDir $dir]
		}
	    }
	}
    }
    return $ret
}

proc formulae::_eval_in_ramdebugger { args } {
    ramdebugger eval $args
}

proc formulae::check_connect_ramdebugger { key } {
    variable values
    
    set interp [dict_getd $values $key interp ""]
    if { $interp eq "" } { return }
    if { ![interp exists ramdebugger] } { return }

    interp alias ramdebugger master $interp eval
    interp alias $interp ramdebugger "" formulae::_eval_in_ramdebugger
}

proc formulae::check_unconnect_ramdebugger { key } {
    variable values
    
    set interp [dict_getd $values $key interp ""]
    if { $interp eq "" } { return }
    if { ![interp exists ramdebugger] } { return }
    
    interp alias ramdebugger master "" eval
}

proc formulae::eval_in_interp { key args } {
    variable values
    
    set interp [dict get $values $key interp]
    return [$interp eval $args]
}

proc formulae::edit_txt_in_ramdebugger { key title data callback } {
    variable values
    
    set session_id [dict_getd $values $key session_id ""]
    set wp [dict_getd $values $key notebook_widget ""]
    
    set callbackG [list formulae::edit_txt_in_ramdebugger_end $key $session_id $callback]
    set err [catch { cu::file::edit_in_ramdebugger $wp $title $data $callbackG } errstring]
    if { $err } {
	snit_messageBox -message $errstring
	return
    }
}

proc formulae::edit_txt_in_ramdebugger_end { key session_id callback title data } {
    variable values
    
    set interp [dict get $values $key interp]
    if { $session_id ne [dict_getd $values $key session_id ""] } {
	snit_messageBox -message [_ "Data cannot be saved. It is out of sync"]
	return
    }
    set err [catch { $interp eval $callback [list $data] } errstring]
    if { $err } {
	snit_messageBox -message $errstring
	return
    }
}

proc formulae::edit_in_ramdebugger { key what args } {
    variable values
    
    switch $what {
	edit {
	    lassign $args domNode
	    set session_id [dict_getd $values $key session_id ""]
	    set wp [dict_getd $values $key notebook_widget ""]
	    set title "[_ {Edit subtree '%s'} [$domNode @n]] (xml)"
	    update_sub_tree $key $domNode
	    set data [$domNode asXML -indent 2]
	    set callback [list formulae::edit_in_ramdebugger $key endedit $session_id [$domNode @n]]
	    set err [catch { cu::file::edit_in_ramdebugger $wp $title $data $callback } errstring]
	    if { $err } {
		snit_messageBox -message $errstring
		return
	    }
	}
	endedit {
	    lassign $args session_id field_name title data
	    if { $session_id ne [dict_getd $values $key session_id ""] } {
		snit_messageBox -message [_ "Data cannot be saved. It is out of sync"]
		return
	    }
	    set doc [dict get $values $key doc]
	    set xp [format_xpath {(//param[@n=%s and not(ancestor::setname)]} $field_name]
	    append xp [format_xpath {|//container[@n=%s and not(ancestor::setname)]} $field_name]
	    append xp [format_xpath {|//condition[@n=%s and not(ancestor::setname)])[1]} $field_name]
	    set node [$doc selectNodes $xp]
	    if { $node eq "" } {
		snit_messageBox -message [_ "Data cannot be saved. It is not correct"]
		return
	    }
	    set err [catch { [$node parentNode] appendBeforeXML $data $node } newNode]
	    if { $err } {
		snit_messageBox -message [_ "Data cannot be saved. It is not valid XML (%s)" $newNode]
		return 
	    }
	    [$node parentNode] removeChild $node
	    
	    set err [catch { create_update_database_table $key } errstring]
	    
	    if { $err } {
		[$newNode parentNode] insertBefore $node $newNode
		$newNode delete
		snit_messageBox -message [_ "Data cannot be saved. It is not OK"]
		return
	    }
	    create_windowD_do $key
	}
	default {
	    error "error in formulae::edit_in_ramdebugger what=$what"
	}
    }
}

proc formulae::edit_page_xml { key what xpath args } {
    variable values

    set doc [dict get $values $key doc]
    if { $doc eq "" } { return "" }
    set formulaeNode [$doc selectNodes //formulae]
    if { $formulaeNode eq "" } { return "" }

    set domNode [$formulaeNode selectNodes $xpath]
    if { $domNode eq "" } { return "" }
    
    switch $what {
	get {
	    update_sub_tree $key $domNode
	    return [$domNode asXML -indent 2]
	}
	set {
	    set data [lindex $args 0]
	    set err [catch { [$domNode parentNode] appendBeforeXML $data $domNode } newNode]
	    if { $err } {
		error [_ "Data cannot be saved. It is not valid XML (%s)" $newNode]
	    }
	    [$domNode parentNode] removeChild $domNode

	    set err [catch { create_update_database_table $key } errstring]
	    
	    if { $err } {
		[$newNode parentNode] insertBefore $domNode $newNode
		$newNode delete
		error [_ "Data cannot be saved. It is not OK (%s)" $errstring]
	    }
	    $domNode delete
	    create_windowD_do $key
	}
    }
}

proc formulae::source_page { key page } {
    variable values
    
    set lognoter_db [dict get $values $key lognoter_db]
    set interp [dict get $values $key interp]
    
    set err [catch { source_pageIL $interp $lognoter_db $page } ret]

    if { $err } {
	snit_messageBox -message [_ "Failed to source page '%s'. Page does not exist" $page]
	return
    }
}

proc formulae::source_pageIL { interp lognoter_db page } {
    
    lassign [$lognoter_db givepage -askpassword 0 -raise_error 1 $page] - - - data - -

    set doc [dom parse $data]
    foreach tclNode [$doc selectNodes {//tcl}] {
	$interp eval [$tclNode text]
    }
    $doc delete
}

proc formulae::_ask_for_period_update { w what } {

    if { [$w exists_uservar updating_period] } {
	return
    }
    set year [$w give_uservar_value year]
    set period_type [$w give_uservar_value period_type]
    set start_date [$w give_uservar_value start_date]
    set end_date [$w give_uservar_value end_date]
    
    if { $what eq "dates" } {
	set err [catch { clock format [clock scan $start_date] -format %Y } start_year]
	if { $err } { return }
	set err [catch { clock format [clock scan $end_date] -format %Y } year]
	if { $err } { return }
    }
    if { $year eq "" } {
	if { $period_type eq "full_range" } {
	    ask_for_period_set_full_years_range $w
	    return
	} elseif { $period_type eq "user_defined" } {
	    return
	} else {
	    set err [catch { clock format [clock scan $end_date] -format %Y } year]
	    if { $err } { set year 2011 }
	    $w set_uservar_value year $year
	}
    }
    set idx 1
    foreach i [list 0 3 6 9] {
	set start($idx) [clock format [clock add [clock scan $year-01-01] $i month] -format %Y-%m-%d]
	set end($idx) [clock format [clock add [clock scan $year-01-01] [expr {$i+3}] month \
		    -1 day] -format %Y-%m-%d]
	incr idx
    }
    set start($idx) $start(1)
    set end($idx) $end(4)
    
    switch $what {
	year - period_type {
	    switch $period_type {
		"1 quarter" { set idx 1 }
		"2 quarter" { set idx 2 }
		"3 quarter" { set idx 3 } 
		"4 quarter" { set idx 4 }
		"year" { set idx 5 }
		"user_defined" { set idx 0 }
		"full_range" { set idx -1 }
	    }
	    if { $idx == -1 } {
		ask_for_period_set_full_years_range $w
		return
	    } elseif { $idx == 0 } {
		set err [catch { clock format [clock scan $start_date] -format $year-%m-%d } start_date]
		if { $err } {
		    set start_date $year-01-01
		}
		set err [catch { clock format [clock scan $end_date] -format $year-%m-%d } end_date]
		if { $err } {
		    set end_date $year-12-31
		}
		$w set_uservar_value year ""
	    } else {
		set start_date $start($idx)
		set end_date $end($idx)
	    }
	    if { $start_date ne [$w give_uservar_value start_date] } {
		$w set_uservar_value start_date $start_date
	    }
	    if { $end_date ne [$w give_uservar_value end_date] } {
		$w set_uservar_value end_date $end_date
	    }
	}
	dates {
	    set found 0
	    if { $start_year == $year } {
		foreach idx [list 1 2 3 4 5] {
		    if { $start_date eq $start($idx) && $end_date eq $end($idx) } {
		        set found 1
		        break
		    }
		}
	    }
	    if { $found } {
		switch $idx {
		    1 { set period_type "1 quarter" }
		    2 { set period_type "2 quarter" }
		    3 { set period_type "3 quarter" } 
		    4 { set period_type "4 quarter" }
		    5 { set period_type "year" }
		}
	    } else {
		set period_type "user_defined"
	    }
	    if { $period_type ne [$w give_uservar_value period_type] } {
		$w set_uservar_value period_type $period_type
	    }
	    if { $start_year == $year && $year ne [$w give_uservar_value year] } {
		$w set_uservar_value year $year
	    }
	}
    }
}

proc formulae::ask_for_period_sql_where_accept { w combo wtable } {
    
    set sql_where [$w give_uservar_value sql_where]
    set values [linsert0 -max_len 15 [$combo cget -values] $sql_where]
    $combo configure -values $values
    
    ask_for_period_create_table_update_idle $w $wtable
}

proc formulae::ask_for_period_create_table_update_idle { w wtable } {
    variable ask_for_period_create_table_update_idle
    
    after cancel [set! ask_for_period_create_table_update_idle]
    set ask_for_period_create_table_update_idle [after 100 [list \
		formulae::ask_for_period_create_table_update $w $wtable]]
}

proc formulae::ask_for_period_create_table_update { w wtable } {
    variable ask_for_period_create_table_update_idle
    variable ask_for_period_create_table_update_prev
    
    after cancel [set! ask_for_period_create_table_update_idle]
    unset -nocomplain ask_for_period_create_table_update_idle
    
    if { [$w give_uservar_value disable_draw_table] } {
	return
    }
    
    lassign [$w give_uservar_value table_definition] name fields datename table sel1 sel2 orderby
    set lognoter_db [$w give_uservar_value lognoter_db]
    
    foreach i [list start_date end_date sql_where] {
     set $i [$w give_uservar_value $i]   
    }
    
    lassign [set! ask_for_period_create_table_update_prev] prev prev_start_date prev_end_date
    set ask_for_period_create_table_update_prev [list $sql_where $start_date $end_date]
    set len [string length $sql_where]
    set len_prev [string length $prev]
    if { [list $start_date $end_date] eq [list $prev_start_date $prev_end_date] &&
	[$wtable item count] > 1 && $len < 3 && $len >= $len_prev } {
	return
    }
    
    $wtable item delete all
    if { [regexp {^\s*(\w+)(?:\s+(\w+))*$} $sql_where] } {
	set searchstring $sql_where
	set orList ""
	foreach field $fields {
	    set andList ""
	    foreach p [cu::sql_patterns_list $searchstring] {
		lappend andList "[$lognoter_db sql fs0 $field] like [$lognoter_db sql vs0 $p]"
	    }
	    lappend orList [join $andList { and }]
	}
	set sql_where [join $orList { or }]
    }
    
    set sql_whereG ""
    
    if { $datename ne "" } {
	append sql_whereG " where $datename >= [$lognoter_db sql vs0 $start_date] and 
	    $datename <= [$lognoter_db sql vs0 $end_date]"
    }
    if { $sql_where ne "" } {
	if { $sql_whereG ne "" } {
	    append sql_whereG " and"
	    if { [regexp {\mor\M} $sql_where] && ![regexp {^\s*[(].*[)]\s*$} $sql_where] } {
		set sql_where "($sql_where)"
	    }
	} else {
	    append sql_whereG " where"
	}
	append sql_whereG " $sql_where"
    }
    set idx 0
    foreach sel [list $sel1 $sel2] {
	set num($idx) 0
	if { $sel ne "" } {
	    set err [catch { lindex $sel 0 } cmd0]
	    if { !$err && [string tolower $cmd0] in [list "union" "union all"] } {
		set cmdList ""
		foreach i [lrange $sel 1 end] {
		    lappend cmdList "$i $sql_whereG"
		}
		set cmd [join $cmdList " $cmd0 "]
	    } else {
		set cmd "$sel $sql_whereG"
	    }
	    if { $orderby ne "" } {
		append cmd " order by $orderby"
	    }
	    set err [catch { $lognoter_db sql sel $cmd } ret]
	    if { $err } {
		$w set_uservar_value table_status [_ "error: %s" $ret]
		return
	    }
	} else {
	    set ret ""
	}
	foreach $fields $ret {
	    set vs ""
	    set isnull 1
	    foreach i $fields {
		lappend vs [set $i]
		if { [set $i] ne "" } {
		    set isnull 0
		}
	    }
	    if { !$isnull } {
		set item [$wtable insert end $vs]
		if { $idx } {
		    $wtable item state set $item emphasis
		}
		incr num($idx)
	    }
	}
	incr idx
    }
    $wtable see end
    $w set_uservar_value table_status [_ "Num entries=%d" $num(0)]
}

proc formulae::ask_for_period_add_set_condition { what w tree item column } {
    
    set lognoter_db [$w give_uservar_value lognoter_db]
    lassign [$w give_uservar_value table_definition] - fields - - - - -
    
    set sql_where [string trim [$w give_uservar_value sql_where]]
    if { $what in "set set_new" } {
	set sql_where ""
    } else {
	set sql_where [string trim [$w give_uservar_value sql_where]]
    }
    if { [regexp {^\w+(\s+\w+)?$} $sql_where] } {
	set sql_where ""
    }
    if { $sql_where ne "" } {
	append sql_where " and "
    }
    set txt [$tree item text $item $column]
    foreach pat [cu::sql_patterns_list -one_pattern $txt] {
	lappend cndList "[$lognoter_db sql fs0 [lindex $fields $column]] like [$lognoter_db sql vs0 $pat]"
    }
    append sql_where "[join $cndList { and }]"
    
    if { $what eq "set_new" } {
	set key [$w give_uservar_value key]
	set args [lrange [$w give_uservar_value args] 0 end-1]
	lappend args -unique_window 0 -sql_where $sql_where \
	    -table_definition [$w give_uservar_value table_definition] \
	    [lindex [$w give_uservar_value args] end]
	ask_for_period $key {*}$args
    } else {
	$w set_uservar_value sql_where $sql_where
    }
}

proc formulae::ask_for_period_copy_one { tree item column } {
    
    set txt [$tree item text $item $column]
    clipboard clear  
    clipboard append $txt
}

proc formulae::ask_for_period_copy { tree itemList } {

    set dataList ""
    foreach item [lsort -integer $itemList] {
	lappend dataList [join [$tree item text $item] \t]
    }
    clipboard clear  
    clipboard append [join $dataList "\n"]    
}

proc formulae::ask_for_period_selectall { tree } {
    $tree selection clear
    $tree selection add all
}

proc formulae::ask_for_period_contextual { w tree - menu item itemList x y } {

    lassign [$tree identify $x $y] what item col_name column
    if { $what ne "item" || $col_name ne "column" } { return }
    
    $menu add command -label [_ "Set condition"] -command [list formulae::ask_for_period_add_set_condition \
	    set $w $tree $item $column]
    $menu add command -label [_ "Set condition new window"] -command [list formulae::ask_for_period_add_set_condition \
	    set_new $w $tree $item $column]
    $menu add command -label [_ "Add condition"] -command [list formulae::ask_for_period_add_set_condition \
	    add $w $tree $item $column]
    $menu add separator
    $menu add command -label [_ "Select all"] -command [list formulae::ask_for_period_selectall $tree]
    $menu add command -label [_ "Copy"] -command [list formulae::ask_for_period_copy $tree $itemList]
    $menu add command -label [_ "Copy one"] -command [list formulae::ask_for_period_copy_one $tree $item $column]
}

proc formulae::ask_for_period_set_full_years_range { w } {
    
    set ldb [$w give_uservar_value lognoter_db]
    
    lassign [$w give_uservar_value table_definition] - - datename table sel1 - -
    if { $datename eq "" } { return }

    set err [catch { lindex $sel1 0 } cmd0]
    if { !$err && [string tolower $cmd0] in [list "union" "union all"] } {
	lassign "" d1 d2
	foreach t $table {
	    set cmd "select min($datename),max($datename)
		from [$ldb sql fs0 $t]"
	    lassign [$ldb sql sel $cmd] d1L d2L
	    if { $d1 eq "" || $d1L < $d1 } {
		set d1 $d1L
	    }
	    if { $d2 eq "" || $d2L > $d2 } {
		set d2 $d2L
	    }
	}
    } else {
	set cmd "select min($datename),max($datename)
	    from [$ldb sql fs0 $table]"
	lassign [$ldb sql sel $cmd] d1 d2
    }
    $w set_uservar_value updating_period 1
    $w set_uservar_value period_type "full_range"
    $w set_uservar_value year ""
    $w set_uservar_value start_date $d1
    $w set_uservar_value end_date $d2
    set y1 [clock format [clock scan $d1] -format %Y]
    set y2 [clock format [clock scan $d2] -format %Y]
    $w set_uservar_value years [rangeF $y1 $y2]
    $w unset_uservar updating_period
}

proc formulae::ask_for_period_update_columns { w wtable } {
    
    lassign [$w give_uservar_value table_definition] - fields - - - - -
    set columns ""
    foreach i $fields {
	if { $i in "id Id Entry" } {
	    set width 6
	} elseif { $i in [list Company Concept Name concept notes name Comments References] } {
	    set width 25
	} else {
	    set width 10
	}
	lappend columns [list $width $i left text 1]
    }
    $wtable configure -columns $columns
    $wtable item delete all
}

proc formulae::ask_for_period_change_database { w wtable } {
    
    $w set_uservar_value disable_draw_table 1
    
    set current [$w give_uservar_value current_table_definition]
    set ipos [lsearch -index 0 [$w give_uservar_value table_definitionList] $current]
    $w set_uservar_value table_definition [lindex [$w give_uservar_value table_definitionList] $ipos]
    ask_for_period_set_full_years_range $w
    ask_for_period_update_columns $w $wtable
    $w set_uservar_value disable_draw_table 0
    $wtable item delete all
}

proc formulae::ask_for_period_create_table { w frame combo } {
    
    if { [winfo exists $frame.table] } {
	destroy $frame.table $frame.lctd $frame.cbctd
	update
	wm geometry $w ""
	return
    }
    
    if { [llength [$w give_uservar_value table_definitionList]] } {
	
	ttk::label $frame.lctd -text [_ "Database"]:
	cu::combobox_tree $frame.cbctd -textvariable [$w give_uservar current_table_definition] \
	    -state readonly
	
	set parentDict ""
	foreach i [$w give_uservar_value table_definitionList] {
	    set name [lindex $i 0]
	    set parent 0
	    for { set j 0 } { $j < [llength [split $name ">"]]-1 } { incr j } {
		set nameP [join [lrange [split $name ">"] 0 $j] >]
		if { ![dict exists $parentDict $nameP] } {
		    set item [$frame.cbctd tree_insert -active 0 -collapse 1 end [lindex [split $name ">"] $j] \
		            $nameP $parent]
		    dict set parentDict $nameP $item
		    set parent $item
		} else {
		    set parent [dict get $parentDict $nameP]
		}
	    }
	    $frame.cbctd tree_insert end [lindex [split $name ">"] end] \
		$name $parent
	}
	
	$w set_uservar_value current_table_definition [lindex [$w give_uservar_value table_definition] 0]
	grid $frame.lctd $frame.cbctd -padx 2 -pady "2 10" -sticky w
	
	bind $frame.cbctd <<ComboboxSelected>> [list \
	    formulae::ask_for_period_change_database $w $frame.table]
    }
    set wtable [fulltktree $frame.table -showbuttons 0 -showlines 0 \
	    -width 800 -height 300 -bd 0 -relief raised \
	    -contextualhandler_fmenu \
	    [list formulae::ask_for_period_contextual $w $frame.table]]
    
    ask_for_period_update_columns $w $wtable
    
    destroy $frame.l
    ttk::label $frame.l -textvariable [$w give_uservar table_status]
    
    lassign [grid size $frame] cols rows
    grid $wtable -columnspan $cols -sticky nsew  -padx 2
    grid rowconfigure $frame [dict get [grid info $wtable] -row] -weight 1
    grid $frame.l -columnspan $cols -sticky w
    foreach i [list start_date end_date sql_where] {
	$w add_trace_to_uservar $i [list formulae::ask_for_period_create_table_update_idle $w $wtable]
    }
    bind $combo <Return> "[list formulae::ask_for_period_sql_where_accept $w $combo $wtable]; break"
    
    #ask_for_period_create_table_update $w $wtable
    update
    wm geometry $w ""
}

proc formulae::query_manager { key } {
    variable values
    
    set db [dict get $values $key lognoter_db]

    set table_definitionList ""
    
    foreach name [list invoices expenses credit_note currency] {
	set table ${name}_register
	switch $name {
	    invoices - credit_note {
		set fields [list Company invoice_number invoice_date base VAT total state comments \
		        payment_date payment_type]
	    }
	    expenses {
		set fields [list Company invoice_number invoice_date base VAT total acc_references]
	    }
	    currency {
		set fields [list concept payment_date value total notes invoice_number \
		acc_references]
	    }
	}
	set sel1 "select [$db sql fs $fields] from [$db sql fs0 $table]"
	if { $name eq "currency" } {
	    set sel2 "select '','',round(sum(value),2),round(sum(total)),'','','' from
		[$db sql fs0 $table]"
	    set datename payment_date
	} else {
	    set sel2 "select '','','',round(sum(base),2),'',round(sum(total)),'','' from
		[$db sql fs0 $table]"
	    set datename "str_to_date(invoice_date,'%d/%m/%Y')"
	}
	set orderby [$db sql fs0 id]
	lappend table_definitionList [list Compasser>$name $fields $datename $table $sel1 $sel2 $orderby]
    }
    set tables [$db sql table_names]
#     foreach year [rangeF 2001 2007] {
#         set tables_year [lsearch -inline -all $tables accounting_operations_register_${year}_gestidret*]
#         if { [llength $tables_year] == 0 } {
#             set tables_year [lsearch -inline -all $tables accounting_operations_register_${year}]
#         }
#         set table [lindex [lsort -dictionary $tables_year] end]
#         if { $table ni $tables } {
#             continue
#             #error "error finding table op for $year"
#         }
#         regsub -all {operations} $table {accounts} table_acc
#         if { $table_acc ni $tables } {
#             set table_acc "accounting_accounts_register"
#         }
#         set table_gen accounting_general_register_$year
#         if { $table_gen ni $tables } {
#             set table_gen accounting_general_register
#         }
#         set name "$year>Accounts general ($table_gen)"
#         set fields [list number name]
#         set sel1 "select [$db sql fs $fields] from [$db sql fs0 $table_gen]"
#         set sel2 ""
#         set datename ""
#         set orderby [$db sql fs0 id]
#         lappend table_definitionList [list $name $fields $datename $table_gen $sel1 $sel2 $orderby]
# 
#         set name "$year>Accounts ($table_acc)"
#         set fields [list number name]
#         set sel1 "select [$db sql fs $fields] from [$db sql fs0 $table_acc]"
#         set sel2 ""
#         set datename ""
#         set orderby [$db sql fs0 id]
#         lappend table_definitionList [list $name $fields $datename $table_acc $sel1 $sel2 $orderby]
# 
#         set name "$year>Accounting ($table)"
#         set fields1 [list session_id session_date account_1 amount_euros_1 amount_euros_2 concept]
#         set fields_name1 [list Entry Date "Account number" Debit Credit Concept]
#         set fields2 [list name]
#         set fields_name2 [list Name]
#         set fields [concat $fields_name1 $fields_name2]
#         set sel1 "select [$db sql fs_prefix op $fields1 $fields_name1],[$db sql fs_prefix acc $fields2 $fields_name2] from
#             [$db sql fs0 $table] as op left join [$db sql fs0 $table_acc] as acc 
#             on [$db sql fs0_prefix op account_1]=[$db sql fs0_prefix acc "number"] "
#         set sel2 "select '','','',round(sum(op.amount_euros_1),2),round(sum(op.amount_euros_2),2),
#             round(sum(op.amount_euros_1-op.amount_euros_2),2),'' from
#             [$db sql fs0 $table] as op left join [$db sql fs0 $table_acc] as acc 
#             on [$db sql fs0_prefix op "account_1"]=[$db sql fs0_prefix acc "number"] "
#         set datename session_date
#         set orderby [$db sql fs0_prefix op id]
#         lappend table_definitionList [list $name $fields $datename $table $sel1 $sel2 $orderby]
#     }
    lassign "" acc_selList tableList

    foreach year [rangeF 2001 2011] {
	set name "$year>Accounts general"
	if { $year <= 2007 } {
	    set page0 "Accounting prev>$year"
	} else {
	    set page0 "Accounting $year"
	}
	set table [$db default_table_for_page "$page0>accounts general"]
	set fields [list Number Name]
	set sel1 "select [$db sql fs $fields] from [$db sql fs0 $table]"
	set sel2 ""
	set datename ""
	set orderby [$db sql fs0 id]
	lappend table_definitionList [list $name $fields $datename $table $sel1 $sel2 $orderby]

	set name "$year>Accounts"
	set table [$db default_table_for_page "$page0>accounts"]
	set fields [list Number Name]
	set sel1 "select [$db sql fs $fields] from [$db sql fs0 $table]"
	set sel2 ""
	set datename ""
	set orderby [$db sql  fs0 id]
	lappend table_definitionList [list $name $fields $datename $table $sel1 $sel2 $orderby]

	set name "$year>Accounting"
	set table [$db default_table_for_page "$page0>accounting data"]
	set table_acc [$db default_table_for_page "$page0>accounts"]
	set fields1 [list Entry Date "Account number" Debit Credit Concept]
	set fields2 [list Name]
	set fields [concat $fields1 $fields2]
	set sel1 "select [$db sql fs_prefix op $fields1],[$db sql fs_prefix acc $fields2] from
	    [$db sql fs0 $table] as op left join [$db sql fs0 $table_acc] as acc 
	    on [$db sql fs0_prefix op "Account number"]=[$db sql fs0_prefix acc "Number"] "
	set sel2 "select '','','',round(sum(op.Debit),2),round(sum(op.Credit),2),
	    round(sum(op.Debit-op.Credit),2),'' from
	    [$db sql fs0 $table] as op left join [$db sql fs0 $table_acc] as acc 
	    on [$db sql fs0_prefix op "Account number"]=[$db sql fs0_prefix acc "Number"] "
	set datename Date
	set orderby [$db sql fs0_prefix op id]
	lappend table_definitionList [list $name $fields $datename $table $sel1 $sel2 $orderby]
	lappend acc_selList $sel1
	lappend tableList $table
    }
    
    set sel1 [list "union" {*}$acc_selList]
    set sel2 ""
    lappend table_definitionList [list [_ "Accounting all"] $fields $datename $tableList $sel1 $sel2 \
	    $datename]
    
    set name "Banks>Deutsche_bank"
    set table [$db default_table_for_page "Banks>Deutsche_bank"]
    set fields [list id Date Value Total Comments References]
    set sel1 "select [$db sql fs $fields] from [$db sql fs0 $table]"
    set sel2 "select '',round(sum(Value),2)  from [$db sql fs0 $table]"
    set datename "Date"
    set orderby [$db sql fs0 id]
    lappend table_definitionList [list $name $fields $datename $table $sel1 $sel2 $orderby]
    
    ask_for_period $key -callback - -default_template "" \
	-has_where_clause 1 -unique_window 0 \
	-table_definitionList $table_definitionList -title [_ "Query manager"] "query_manager"
}

proc formulae::ask_for_period { key args } {
    variable values
    
    set optional {
	{ -only_date boolean 0 }
	{ -default_is_detailed boolean "" }
	{ -default_template template "" }
	{ -has_where_clause boolean 0 }
	{ -user_list dict "" }
	{ -table_definition "name fields datename table sel1 sel2 orderby" "" }
	{ -table_definitionList list "" }
	{ -callback proc "" }
	{ -interp interp "" }
	{ -unique_window boolean 1 }
	{ -title title "" }
	{ -sql_where txt "" }
    }
    set compulsory "modetype"
    parse_args $optional $compulsory $args
    
    set ldb [dict get $values $key lognoter_db]
    set window_parent [dict get $values $key window_parent]
    set lognoter_db [dict get $values $key lognoter_db]
    
    if { $table_definition eq "" } {
	set name [$ldb getpreference ${modetype}_table_definition_name ""]
	set ipos [lsearch -index 0 $table_definitionList $name]
	if { $ipos != -1 } {
	    set table_definition [lindex $table_definitionList $ipos]
	} else {
	    set table_definition [lindex $table_definitionList end]
	}
    }
    
    set period_types_dict [list \
	    "1 quarter" [_ "First quarter"] \
	    "2 quarter" [_ "Second quarter"] \
	    "3 quarter" [_ "Third quarter"] \
	    "4 quarter" [_ "Forth quarter"] \
	    "year" [_ "Full year"] \
	    "user_defined" [_ "User defined"] \
	    "full_range" [_ "Full range"] \
	    ]
    
    xml2pdf::addTemplatesDir [file join $::topdir_external export_templates \
		pdf_templates]
	
    xml2pdf::addTemplatesDir [file join [lognoter_db give_appdata_dir] \
	    export_templates pdf_templates]

    set templates [xml2pdf::GiveTemplates]
    
    if { $unique_window } {
	set w $window_parent._time_period
	destroy $w
    } else {
	set idx 1
	set w $window_parent._time_period$idx
	while { [winfo exists $w] } {
	    set w $window_parent._time_period[incr idx]
	}
    }
    if { $callback eq "-" } {
	lassign [list "-" "" 0] okname callbackL grab
    } elseif { $callback ne "" } {
	set callbackL [list formulae::ask_for_period_accept]
	set grab 0
	set okname ""
    } else {
	lassign [list "" "" 1] okname callbackL grab
    }
    if { $title eq "" } {
	set title [_ "Enter time period"]
	set class [_ "Time period"]
    } else {
	set class $title
	regsub -all {[^\w]} $class {_} class
    }
    dialogwin_snit $w -title $title -callback $callbackL -grab $grab -okname $okname -class $class
    
    set f [$w giveframe]
    
    if { !$only_date } {
	set f1 [ttk::labelframe $f.f1 -text [_ "time period"]]
    } else {
	set f1 [ttk::labelframe $f.f1 -text [_ "time"]]
    }
    ttk::label $f1.l1 -text [_ "Period"]:
    cu::combobox $f1.cb1 -textvariable [$w give_uservar period_type "user_defined"] \
	-values [dict keys $period_types_dict] -dict $period_types_dict \
	-state readonly -width 12

    cu::combobox $f1.cb2 -textvariable [$w give_uservar year ""] \
	-valuesvariable [$w give_uservar years] -state readonly -width 4
    
    if { !$only_date } {
	ttk::label $f1.l2 -text [_ "Dates"]:
    } else {
	ttk::label $f1.l2 -text [_ "Date"]:  
    }
    ttk::frame $f1.f
    ttk::label $f1.f.l1 -text [_ "From"]:
    cu::dater_entry $f1.f.e1 -textvariable [$w give_uservar start_date ""] \
	-width 10
    ttk::label $f1.f.l2 -text [_ "To"]:

    cu::dater_entry $f1.f.e2 -textvariable [$w give_uservar end_date ""] \
	-width 10
    
    if { !$only_date } {
	grid $f1.f.l1 $f1.f.e1 $f1.f.l2 $f1.f.e2 -sticky w -padx 2
	grid configure $f1.f.e1 $f1.f.e2 -sticky ew
	grid configure $f1.f.e2 -padx "2 20"
	grid columnconfigure $f1.f "1 3" -weight 1
    } else {
	grid $f1.f.e2 -sticky ew -padx "2 20"
	grid columnconfigure $f1.f 0 -weight 1
    }
    if { !$only_date } {
	grid $f1.l1 $f1.cb1 $f1.cb2 -sticky w -padx 2 -pady 2
	grid configure $f1.cb2 -padx "2 20" -sticky ew
    }
    grid $f1.l2 $f1.f      -    -sticky w -padx 2 -pady 2

    grid configure $f1.f -sticky ew -padx 0
    grid columnconfigure $f1 "2" -weight 1
    
    set f2 [ttk::labelframe $f.f2 -text [_ "print template"]]

    ttk::label $f2.l1 -text [_ "Template"]: 
    cu::combobox $f2.cb1 -textvariable [$w give_uservar template] \
	-values $templates -state readonly

    grid $f2.l1 $f2.cb1 -sticky w -padx 2 -pady 2
    grid configure $f2.cb1 -padx "2 20" -sticky ew
    grid columnconfigure $f1 1 -weight 1
    grid columnconfigure $f2 1 -weight 1
    
    set f3 [ttk::labelframe $f.f3 -text [_ "print detail"]]

    ttk::checkbutton $f3.l1 -text [_ "Print detailed data"] -variable \
	[$w give_uservar is_detailed]
    
    grid $f3.l1 -sticky w -padx 2 -pady 2
    
    if { $has_where_clause } {
	set f4 [ttk::labelframe $f.f4 -text [_ "SQL condition"]]
	
	set examples [list \
		"Concept like '%Nómina%'" \
		"Concept like '%Nómina%' and Debit < 3000" \
		"\"Account number\" like '620%'" \
		"\"Account number\" like '620%' and Concept != 'Asiento de regularización'" \
		]
	ttk::label $f4.l1 -text [_ "SQL condition"]: 
	cu::combobox $f4.cb1 -textvariable [$w give_uservar sql_where] \
	    -values $examples -width 50
	
	if { $table_definition ne "" } {
	    ttk::button $f4.b1 -image [cu::get_icon nav1downarrow16] -style Toolbutton -command \
		[list formulae::ask_for_period_create_table $w $f4 $f4.cb1]
	    tooltip::tooltip $f4.b1 [_ "Open a report table"]
	    
	    if { !$unique_window } {
		ttk::button $f4.b2 -image [cu::get_icon list-add-16] -style Toolbutton -command \
		    [list formulae::ask_for_period $key {*}$args]
		tooltip::tooltip $f4.b2 [_ "Open another window"]
		grid $f4.l1 $f4.cb1 $f4.b1 $f4.b2  -sticky w -padx 2 -pady 2
	    } else {
		grid $f4.l1 $f4.cb1 $f4.b1 -sticky w -padx 2 -pady 2
	    }
	    grid configure $f4.cb1 -padx "2 2" -sticky ew
	} else {
	    grid $f4.l1 $f4.cb1 -sticky w -padx 2 -pady 2
	    grid configure $f4.cb1 -padx "2 20" -sticky ew
	}
	grid columnconfigure $f4 1 -weight 1
    }
    if { $user_list ne "" } {
	set f5 [ttk::labelframe $f.f5 -text [dict_getd $user_list frame_name [_ "User defined"]]]
	ttk::label $f5.l1 -text [dict_getd $user_list label [_ "User defined"]]:
	ttk::combobox $f5.cb1 -textvariable [$w give_uservar user_defined] -values \
	    [dict_getd $user_list values ""]
	if { [dict_getd $user_list readonly 0] } {
	    $f5.cb1 state readonly
	}
	grid $f5.l1 $f5.cb1 -sticky w -padx 2 -pady 2
	grid configure $f5.cb1 -padx "2 20" -sticky ew
	grid columnconfigure $f5 1 -weight 1
    }
    
    grid $f1 -sticky nsew
    if { $default_template ne "" } {
	grid $f2 -sticky nsew
    }
    if { $default_is_detailed ne "" } {
	grid $f3 -sticky nsew
    }
    if { $has_where_clause } {
	grid $f4 -sticky nsew
	grid rowconfigure $f [dict get [grid info $f4] -row] -weight 1
    }
    if { $user_list ne "" } {
	grid $f5 -sticky nsew
    }
    grid columnconfigure $f 0 -weight 1

    set d [dict create user_defined "-$f1.cb2"]
    $w enable_disable_on_key period_type $d

    $w add_trace_to_uservar period_type [list formulae::_ask_for_period_update $w period_type]
    $w add_trace_to_uservar year [list formulae::_ask_for_period_update $w year]
    $w add_trace_to_uservar start_date [list formulae::_ask_for_period_update $w dates]
    $w add_trace_to_uservar end_date [list formulae::_ask_for_period_update $w dates]

    $w set_uservar_value key $key
    $w set_uservar_value args $args
    $w set_uservar_value lognoter_db [dict get $values $key lognoter_db]
    $w set_uservar_value template [$ldb getpreference ${modetype}_template $default_template]
    $w set_uservar_value is_detailed [$ldb getpreference ${modetype}_is_detailed $default_is_detailed]
    $w set_uservar_value start_date [$ldb getpreference ${modetype}_start_date 2008-01-01]
    $w set_uservar_value end_date [$ldb getpreference ${modetype}_end_date 2008-12-31]
    if { $sql_where ne "" } {
	$w set_uservar_value sql_where $sql_where
    } else {
	$w set_uservar_value sql_where [$ldb getpreference ${modetype}_sql_where ""]
    }
    $w set_uservar_value table_definition $table_definition
    $w set_uservar_value table_definitionList $table_definitionList
    $w set_uservar_value user_defined [dict_getd $user_list value \
		[lindex [dict_getd $user_list values ""] 0]]
    $w set_uservar_value interp $interp
    
    foreach i [list only_date default_template callback default_is_detailed has_where_clause modetype] {
	$w set_uservar_value $i [set $i]
    }
    
    $w set_uservar_value years [rangeF 2001 2020]
    
    if { $table_definition ne "" } {
	ask_for_period_set_full_years_range $w
    }
    
    if { $has_where_clause && $table_definition ne "" } {
	ask_for_period_create_table $w $f4 $f4.cb1
    }
    $w set_uservar_value disable_draw_table 0
    
    tk::TabToWindow $f1.cb1
    bind [winfo toplevel $f] <Return> [list $w invokeok]
    $w createwindow
    if { $callback eq "-" } {
	ask_for_period_save_preferences $w
	destroy $w
	return
    }
    if { $callback ne "" } {
	return
    }
    while 1 {
	lassign [ask_for_period_accept $w] err ret
	if { !$err } { return $ret }
	$w waitforwindow
    }
}

proc formulae::ask_for_period_save_preferences { w } {

    set lognoter_db [$w give_uservar_value lognoter_db]
    
    foreach i [list only_date default_template callback default_is_detailed has_where_clause modetype] {
	set $i [$w give_uservar_value $i]
    }
    foreach i [list template is_detailed start_date end_date sql_where] {
	if { $default_template eq "" && $i eq "template" } { continue }
	if { $default_is_detailed eq "" && $i eq "is_detailed" } { continue }
	if { $only_date && $i eq "start_date" } { continue }
	if { !$has_where_clause && $i eq "sql_where" } { continue }
	$lognoter_db addpreference ${modetype}_$i [$w give_uservar_value $i]
    }
    if { [$w exists_uservar table_definition] } {
	set table_definition [$w give_uservar_value table_definition]
	if { $table_definition ne "" } {
	    $lognoter_db addpreference ${modetype}_table_definition_name [lindex $table_definition 0]
	}
    }
}

proc formulae::ask_for_period_accept { w } {
   
    set action [$w giveaction]
    set interp [$w give_uservar_value interp]
    set lognoter_db [$w give_uservar_value lognoter_db]

    foreach i [list only_date default_template callback default_is_detailed has_where_clause modetype] {
	set $i [$w give_uservar_value $i]
    }
    if { $action <= 0 } {
	ask_for_period_save_preferences $w
	destroy $w
	if { $callback ne "" } {
	    if { $interp ne "" } {
		$interp eval $callback
	    } else {
		uplevel #0 $callback
	    }
	}
	return [list 0 ""]
    }
    if { !$only_date } {
	set err [catch { clock scan [$w give_uservar_value start_date] }]
    } else {
	set err 0
    }
    if { !$err } {
	set err [catch { clock scan [$w give_uservar_value end_date] }]
    }
    if { $err } {
	snit_messageBox -message [_ "Dates are not OK"] -parent $w
	return [list 1 ""]
    } else {
	ask_for_period_save_preferences $w
	
	set ret [list [$w give_uservar_value start_date] \
		[$w give_uservar_value end_date] [$w give_uservar_value template] \
		[$w give_uservar_value is_detailed] \
		[$w give_uservar_value sql_where] \
		[$w give_uservar_value user_defined]]
	destroy $w
	if { $callback ne "" } {
	    if { $interp ne "" } {
		$interp eval $callback $ret
	    } else {
		uplevel #0 $callback $ret
	    }
	}
	return [list 0 $ret]
    }
}

# proc formulae::give_bindtag { key } {
#     variable values
#     return [dict get $values $key bindtag]
# }

proc formulae::give_interp { key } {
    variable values
    
    return [dict get $values $key interp]
}

proc formulae::destroy_window { key } {
    variable values
    
    set doc [dict get $values $key doc]
    set interp [dict get $values $key interp]

    if { $doc ne "" } { $doc delete }
    
    check_unconnect_ramdebugger $key

    interp delete $interp
    
    set destroy_callback [dict_getd $values $key destroy_callback ""]
    
    dict unset values $key
    
    if { $destroy_callback ne "" } {
	uplevel #0 $destroy_callback
    }
}

proc formulae::mark_window_as_finished { key } {
    variable values 
    
    set doc [dict_getd $values $key doc ""]
    if { $doc eq "" } { return }
    set root [$doc documentElement]
    set wp [dict get $values $key window_parent]

    if { [$root @user_defined_frame ""] ne "" && [dict get $values $key state] eq "disabled" } {
	set err [catch { uplevel #0 [$root @user_defined_frame] $wp end } ret]
	if { $err } {
	    if { [set! ::lognoter_debug] } {
		set ret $::errorInfo
	    }
	    snit_messageBox -message $ret
	} else {
	    return $ret
	}
    }
}

proc formulae::create_window { wp xmldata } {

    set doc [dom parse $xmldata]
    return [create_windowD $wp [$doc documentElement]]
}

proc formulae::create_windowD { args } {
    variable values
    variable current_set
    variable uvalues
    
    set optional {
	{ -tabstyle arrows|arrows_small|notebook|left_tree|right_tree|tree arrows }
	{ -update_callback callback "" }
	{ -destroy_callback callback "" }
	{ -state normal|disabled normal }
	{ -lognoter_db db "" }
	{ -lognoter db "" }
	{ -database_default_table table "" }
	{ -maintain_grid boolean 1 }
	{ -showhidden boolean 0 }
	{ -default_title title_name "" }
	{ -notes_widgets widget "" }
	{ -bindings_list list "" }
	{ -page pageName "" }
    }
    set compulsory "wp key root"
    parse_args $optional $compulsory $args
    
    if { $root eq "" } {
	set xml {
	    <formulae usableviewmode="1" only_form_view_mode="0" tabstyle="">
	    <title></title>
	    <description/>
	    </formulae>
	}
	set new_doc [dom parse $xml]
	set new_root [$new_doc documentElement]
	dict set values $key page_tcl_code ""
    } else {
	set new_doc [dom createDocument [$root nodeName]]
	set new_root [$new_doc documentElement]
	foreach att [$root attributes] {
	    $new_root setAttribute $att [$root @$att]
	}
	set page_tcl_code ""
	foreach node [$root selectNodes //tcl] {
	    append page_tcl_code "[$node text]\n"
	}
	dict set values $key page_tcl_code $page_tcl_code

	foreach node [$root childNodes] {
	    $new_root appendChild $node
	}
    }
    if { [$new_root selectNodes {string(title)}] eq "" && $default_title ne "" } {
	set titleNode [$new_root selectNodes title]
	$titleNode appendChildText $default_title
    }
    if { [$new_root @tabstyle ""] ne "" } {
	set tabstyle [$new_root @tabstyle]
    }
    
    dict set values $key session_id [unique_key]

    set current_set(name,$key) ""
    array unset uvalues $key,*
    
    foreach "n v" [list doc $new_doc window_parent $wp update_stack ""  \
	    svg_stack "" uvalues_moredata "" nicer_name "" tabstyle $tabstyle \
	    update_callback $update_callback destroy_callback $destroy_callback \
	    lognoter_db $lognoter_db lognoter $lognoter database_default_table \
	    $database_default_table state $state maintain_grid $maintain_grid \
	    showhidden $showhidden notes_widgets $notes_widgets \
	    bindings_list $bindings_list page $page] {
	dict set values $key $n $v
    }
    
    #package require tile
    package require wordwidget_snit
    package require tooltip
    namespace eval :: { namespace import -force tooltip::tooltip }

    create_images
    
    return [create_windowD_do $key]

#     if { $root eq "" } {
#         #set update_callback ""
#         form_properties -update_callback $update_callback $key $wp
#         set ret ""
#     } else {
#         set ret [create_windowD_do -maintain_grid $maintain_grid \
#                 -tabstyle $tabstyle]
#     }
#     return $ret
}

proc formulae::set_preferred_tree_position { key what } {
    variable values
    variable values_default
    
    dict set values_default $key preferred_tree_position $what
    if { [dict exists $values $key window_parent] } {
	set wp [dict get $values $key window_parent]
	if { ![winfo exists $wp.n] } { return }
	$wp.n configure -preferred_tree_position $what
	catch { $wp.n _grid_buttons }
    }
}

# create the formulae window
# args can be: ?-tabstyle arrows|arrows_small|notebook? ?-showhidden boolean?
proc formulae::create_windowD_do { args } {
    variable values
    variable values_default
    variable uvalues
    variable current_set

    set optional {
	{ -tabstyle tabstyle - }
	{ -showhidden boolean - }
	{ -maintain_grid boolean 1 }
    }
    set compulsory "key"
    parse_args $optional $compulsory $args
    
    
    set doc [dict get $values $key doc]
    set root [$doc documentElement]
    set wp [dict get $values $key window_parent]

    set current_set(name,$key) ""
    array unset uvalues $key,*
    
    foreach n [list update_stack svg_stack uvalues_moredata nicer_name validate_cmd] {
	dict set values $key $n ""
    }
    foreach n [list use_scrollbarsH use_scrollbarsV] {
	dict set values $key $n [$root @$n 1]
    }
    if { [$root @tabstyle ""] ne "" } {
	dict set values $key tabstyle [$root @tabstyle]
    }
    dict set values $key print_formulas [$root @print_formulas yes]

    foreach i [list tabstyle showhidden maintain_grid] {
	if { [set $i] ne "-" } {
	    dict set values $key $i [set $i]
	}
    }
    set active_tab 0
    if { [winfo exists $wp.n] } {
	catch { $wp.n index current } active_tab
	bind $wp.n <Destroy> ""
	if { [dict get $values $key maintain_grid] } {
	    set grid_info [grid info $wp.n]
	    if { $grid_info eq "" } {
		set grid_info [winfo manager $wp.n]
	    }
	}
	destroy $wp.n
    } else {
	set active_tab 0
    }

    dict set values $key usableviewmode [$root @usableviewmode 1]
    set updatebutton [$root @updatebutton ""]
    if { $updatebutton eq "" } {
	set xp {//param[@field_type="expression" or 
	    @field_type="text expression" or
	    @field_type="formatted expression" or
	    starts-with(@value,'[')]}
	if { [$root selectNodes $xp] ne "" } {
	    set updatebutton 1
	} else {
	    set updatebutton 0 
	}
    }

    if { [dict get $values $key usableviewmode] } {
	set db_state normal
    } else {
	set db_state [dict get $values $key state]
    }
    
    set lognoter [dict get $values $key lognoter]
    set lognoter_db [dict get $values $key lognoter_db]
    
    if { [$root @user_defined_frame ""] ne "" && [dict get $values $key state] eq "disabled" } {
	set table [$root @database ""]
	set page [dict get $values $key page]
	set err [catch { uplevel #0 [$root @user_defined_frame] [list $wp init $lognoter \
	     $lognoter_db $table $page] } ret]
	if { $err } {
	    if { [set! ::lognoter_debug] } {
		set ret $::errorInfo
	    }
	    snit_messageBox -message $ret
	} else {
	    return $ret
	}
    }
    
    if {  [$root @database ""] ne "" } {
	if { ![$lognoter_db sql table_exists  [$root @database]] } {
	    error [_ "Error: table '%s' does not exist" [$root @database]]
	}
    }
    set create_edit_delete_cmds ""
    foreach i [list create edit edit_multiple delete] {
	if { [$root @${i}_command ""] ne "" } {
	    dict set create_edit_delete_cmds $i [$root @${i}_command ""]
	}
    }
    snit_notebook $wp.n -type_name [dict get $values $key tabstyle] \
	-lognoter_db [dict get $values $key lognoter_db] \
	-database [$root @database ""] -rootnode $root \
	-dbstate $db_state -main_state [dict get $values $key state] \
	-actualize_callback [namespace code [list window_actualize -raise_error \
		-exec_callback 0 $key]] \
	-preferred_tree_position [dict_getd $values_default $key preferred_tree_position left] \
	-fill_form_callback [namespace code [list fill_form $key]] \
	-eval_command [namespace code [list eval_in_interp $key]] \
	-contextual_add_callback [namespace code [list add_to_contextual $key]] \
	-notes_widgets [dict_getd $values $key notes_widgets ""] \
	-create_edit_delete_cmds $create_edit_delete_cmds
    
    dict set values $key notebook_widget $wp.n

    if { [info exists grid_info] } {
	if { [string match paned* $grid_info] } {
	    set p [lindex [grid slaves $wp] 0]
	    $p insert 0 $wp.n -weight 1
	} elseif { $grid_info ne "" } {
	    grid $wp.n {*}$grid_info
	}
    }
    set lognoter [dict_getd $values $key lognoter ""]
    if { $lognoter ne "" } {
	set page [[dict get $values $key lognoter] cget -page]
    } else {
	set page ""
    }
    set interp [_create_interp $key $lognoter $lognoter_db $wp.n [$root @database ""] $page]
    dict set values $key interp $interp
    dict set values $key bindtag $interp
    
    check_connect_ramdebugger $key
    
    if { [dict get $values $key page_tcl_code] ne "" } {
	set err [catch { $interp eval [dict get $values $key page_tcl_code] } errstring]
	if { $err } {
	    snit_messageBox -message $errstring
	} 
    }

    foreach bind_key [bind $interp] {
	bind $interp $bind_key ""
    }
    foreach i [dict get $values $key bindings_list] {
	lassign $i bind_key cmd
	bind $interp $bind_key $cmd
    }
    bind $wp.n <Destroy> [namespace code [list destroy_window $key]]
    bind [winfo toplevel $wp] <Control-h> [namespace code [list create_windowD_do \
		-showhidden [expr {([dict get $values $key showhidden])?0:1}] $key]]

    #if { $root eq "" } { return $wp.n }
    
    incr idx
    set ff [frame $wp.n.f$idx -bd 0]
    set c [canvas $ff.c -bd 0 -yscrollcommand [list $ff.sb set] \
	    -xscrollcommand [list $ff.sh set]]    
    bind $c <4> { %W yview scroll -50 pixels }
    bind $c <5> { %W yview scroll 50 pixels }

    scrollbar $ff.sb -orient vertical -command [list $ff.c yview]
    scrollbar $ff.sh -orient horizontal -command [list $ff.c xview]
    grid $c $ff.sb -sticky ns
    grid $ff.sh -sticky ew
    grid configure $c -sticky nsew
    grid remove $ff.sb
    grid remove $ff.sh
    grid columnconfigure $ff 0 -weight 1
    grid rowconfigure $ff 0 -weight 1
    
    set f [frame $ff.f]
    $c create window 0 0 -anchor nw -window $f
    bind $c <Configure> [list formulae::adjust_canvas_frame $key $c $f $ff.sb $ff.sh]
    bind $f <Configure> [list formulae::adjust_canvas_frame $key $c $f $ff.sb $ff.sh]
    grid columnconfigure $f 0 -weight 1
    $wp.n add $ff -sticky nsew -text [_ "Intro"] -image [cu::get_icon iconthemes-16] \
	-compound left
    
    lassign [list 0 0] idxw numlf

    bind $f <ButtonRelease-3> [namespace code [list header_contextual_menu $key $wp \
		%X %Y]]
    
    dict set values $key creating_window 1
    
    set numContainers 0
    foreach node [$root selectNodes title|description|container|setname] {
	switch [$node nodeName] {
	    title {
		set l [cu::nicelabel $f.l[incr idxw]]
		set txt [$node text]
		if { [string index $txt 0] == "\[" } {
		    set cmd1 [string range $txt 1 end-1]
		    set cmd [namespace code [list update_title_description $key $l $cmd1]]
		    set update_stack [dict get $values $key update_stack]
		    lappend update_stack $cmd
		    dict set values $key update_stack $update_stack
		}
		$l insert end $txt bold
		ttk::frame $f.lbuts
		set gridList ""
		if { $updatebutton } {
		    set cmd [namespace code [list window_actualize \
		                -conditions_window %W $key]]
		    set tooltip [_ "Actualize formulae values (Return)"]
		    if { [dict get $values $key state] eq "disabled" && 
		        ![dict get $values $key usableviewmode] } {
		        set state disabled
		    } else {
		        set state normal
		    }
		    if { ![$wp.n try_add_actualize_button [cu::get_icon iconthemes-16] \
		        [_ "Actualize"] $cmd $state $tooltip] } {
		        destroy $wp.actualize_butt
		        set cmd [string map [list %W $f.lbuts.actualize_butt] $cmd]
		        set b [ttk::button $f.lbuts.actualize_butt -image [cu::get_icon iconthemes-16] \
		                -text [_ "Actualize"] -compound left -command $cmd -state $state]               
		        lappend gridList $b
		        tooltip $b $tooltip
		    }
		}
		if { [$root @help_html ""] ne "" || [$root @help_lognoter ""] ne "" } {
		    if { [$root @help_lognoter ""] ne "" } {
		        set cmd [list formulae::_open_lognoter_help $key $wp [$root @help_lognoter]]
		    } else {
		        set cmd [list formulae::_open_html_help $key $wp [$root @help_html]]
		    }
		    destroy $wp.help_butt
		    set hh [ttk::button $f.lbuts.help_butt -image [cu::get_icon help-16] \
		            -style Toolbutton -command $cmd]
		    lappend gridList $hh
		}
		if { [$root selectNodes import_pages] ne "" } {
		    set ip [cu::menubutton_button $f.lbuts.import_pages \
		            -image [cu::get_icon dialog-information-16] \
		            -style Toolbutton -command [list formulae::import_other_forms $key $wp] \
		            -menu $f.lbuts.import_pages.m]
		    set m [menu $f.lbuts.import_pages.m -tearoff 0]
		    foreach n [$root selectNodes import_pages] {
		        $m add command -label [lindex [split [$n text] >] end] -command \
		            [list formulae::import_other_forms -page [$n text] $key $wp]
		    }
		    $m add separator
		    $m add command -label [_ "Import"]... -command \
		        [list formulae::import_other_forms $key $wp]
		    tooltip $ip [_ "Import data from other forms"]
		    lappend gridList $ip
		}
		if { [llength $gridList] } {
		    grid {*}$gridList -sticky w
		}
		grid $l $f.lbuts -in $f -sticky w
		grid configure $l -sticky we -padx 2
		grid columnconfigure $f 0 -weight 1

		bind $l <ButtonRelease-3> [namespace code [list header_contextual_menu $key $wp \
		            %X %Y]]
		
	    }
	    description {
		set txt [string trim [$node text]]
		if { $txt ne "" } {
		    set l [cu::nicelabel $f.l[incr idxw]]
		}
		if { [string index $txt 0] == "\[" } {
		    set cmd1 [string range $txt 1 end-1]
		    set cmd [namespace code [list update_title_description $key $l $cmd1]]
		    set update_stack [dict get $values $key update_stack]
		    lappend update_stack $cmd
		    dict set values $key update_stack $update_stack
		}
		if { $txt ne "" } {
		    $l insert end $txt
		    grid $l -sticky nwe -columnspan 2 -pady 2 -padx 2
		    bind $l <ButtonRelease-3> [namespace code [list header_contextual_menu \
		                $key $wp %X %Y]]

		}
	    }
	    setname {
		# this is obsolete
		set valuesList ""
		foreach "numprinted numtotal" [list 0 0] break
		foreach cnode [$node selectNodes name] {
		    lappend valuesList [$cnode @n]
		    incr numprinted [$cnode @print 1]
		    incr numtotal
		}
		set current_set(name,$key) [$node @n ""]
		set current_set(print,$key) [$node @print 1]
		if { $numprinted == $numtotal } {
		    set current_set(print_all_none,$key) all
		} elseif { $numprinted > 0 } {
		    set current_set(print_all_none,$key) some
		} else {
		    set current_set(print_all_none,$key) none
		}

		set f1 [frame $f.f[incr idxw]]
		set l [ttk::label $f1.l[incr idxw] -text [_ "Set name:"]]
		set cb [ttk::combobox $f1.cb[incr idxw] -textvariable \
		        [namespace current]::current_set(name,$key) -values $valuesList]
		if { [dict get $values $key state] eq "disabled" } {
		    if { [dict get $values $key usableviewmode] } {
		        $cb state readonly
		    } else {
		        $cb state disabled
		    }
		}
		
		tooltip $l [_ "A set name is a way to store a group of values and results for later tabular printing"]
		
		#focus $cb
		bindtags $cb [concat $interp [bindtags $cb]]
		bind $cb <<ComboboxSelected>> [namespace code [list manage_names select \
		            $key $f1.cb$idxw $node]]
		bind $cb <Return> [namespace code [list manage_names accept $key $cb $node]]
		set mb [cu::menubutton_button $f1.mb[incr idxw] -image [cu::get_icon bookmark_add-16] \
		        -menu $f1.mb$idxw.m -state [dict get $values $key state] -command \
		        [namespace code [list manage_names accept $key $cb $node]]]
		menu $f1.mb$idxw.m -tearoff 0
		$f1.mb$idxw.m add command -label [_ "Create new set"] -command \
		    [namespace code [list manage_names accept $key $cb $node]] \
		    -acc "Return"
		$f1.mb$idxw.m add separator
		$f1.mb$idxw.m add checkbutton -label [_ "Print"] -variable \
		    [namespace current]::current_set(print,$key) -command \
		    [namespace code [list manage_names print $key $cb $node]]
		$f1.mb$idxw.m add separator
		$f1.mb$idxw.m add radiobutton -label [_ "Print all"] -command \
		    [namespace code [list manage_names printall $key $cb $node]] -variable \
		    [namespace current]::current_set(print_all_none,$key) -value all
		$f1.mb$idxw.m add radiobutton -label [_ "Print none"] -command \
		    [namespace code [list manage_names printnone $key $cb $node]] -variable \
		    [namespace current]::current_set(print_all_none,$key) -value none
		$f1.mb$idxw.m add separator
		$f1.mb$idxw.m add command -label [_ "Delete"] -command \
		    [namespace code [list manage_names delete $key $cb $node]]
	    
		grid $l $cb $mb -sticky we
		grid columnconfigure $f 1 -weight 1
		grid $f1 -sticky nw -columnspan 2 -pady 2 -padx 2
	     
		if { [dict get $values $key state] eq "normal" } {
		    foreach i [list $l $cb $mb] {
		        bind $i <ButtonRelease-3> [namespace code [list header_contextual_menu \
		                    $key $wp %X %Y]]
		    }
		} else {
		    foreach i [list $l $cb $mb] {
		        tooltip $i [_ "Sets can only be modified in edit page mode"]
		    }
		}  
	    }
	    container {
		if { [$node @location ""] ne "same_page" } {
		    if { $numlf != 0 } { incr idx }
		    set txt ""
		    foreach i [list pns pn n] {
		        if { [$node @$i ""] ne "" } {
		            set txt [$node @$i]
		            break
		        }
		    }
		    if { $txt eq "" } {
		        set txt [_ "Step %d" $idx]
		    }
		    if { $numlf != 0 } {
		        set ff [frame $wp.n.f$idx -bd 0]
		        set c [canvas $ff.c -bd 0 -yscrollcommand [list $ff.sb set] \
		                -xscrollcommand [list $ff.sh set]]
		        
		        bind $c <4> { %W yview scroll -50 pixels }
		        bind $c <5> { %W yview scroll 50 pixels }

		        scrollbar $ff.sb -orient vertical -command [list $ff.c yview]
		        scrollbar $ff.sh -orient horizontal -command [list $ff.c xview]
		        grid $c $ff.sb -sticky ns
		        grid $ff.sh -sticky ew
		        grid $c -sticky nsew
		        grid remove $ff.sb
		        grid remove $ff.sh
		        grid columnconfigure $ff 0 -weight 1
		        grid rowconfigure $ff 0 -weight 1
		        
		        set f [frame $ff.f]
		        $c create window 0 0 -anchor nw -window $f
		        bind $c <Configure> [list formulae::adjust_canvas_frame $key $c $f $ff.sb $ff.sh]
		        bind $f <Configure> [list formulae::adjust_canvas_frame $key $c $f $ff.sb $ff.sh]
		        grid columnconfigure $f 0 -weight 1
		        grid rowconfigure $f 1 -weight 1
		        $wp.n add $ff -sticky nsew -text $txt -image \
		            [cu::get_icon contents2-16] -compound left
		        foreach "idxw numlf" [list 1 0 0] break                       
		    } else {
		        $wp.n tab 0 -text $txt -image [cu::get_icon contents2-16]
		    }
		}
		set cstate [$node @state normal]
		if { $cstate eq "hidden" && [dict get $values $key showhidden] } {
		    set cstate normal
		}
		if { $cstate eq "hidden" } {
		    $wp.n tab $f -state hidden
		} elseif { [$node @condition ""] ne "" } {                    
		    set cmd [namespace code [list check_container_condition \
		                [$node @n ""] [$node @condition ""] $wp.n $ff $key]]
		    set update_stack [dict get $values $key update_stack]
		    lappend update_stack $cmd
		    dict set values $key update_stack $update_stack
		}
		if { [$node @help ""] ne "" || [$node @help_html ""] ne "" || [$node @help_lognoter ""] ne "" } {
		    set l [cu::nicelabel $f.l[incr idxw]]
		    $l insert end [string trim [$node @help]]
		    set fb [frame $f.fb[incr idxw]]
		    set b [ttk::button $fb.b1 -image \
		            [cu::get_icon iconthemes-16] \
		            -command [namespace code [list window_actualize $key]]]
		    
		    if { [$node @help_html ""] ne "" || [$node @help_lognoter ""] ne "" } {
		        if { [$node @help_lognoter ""] ne "" } {
		            set cmd [list formulae::_open_lognoter_help $key $wp [$node @help_lognoter]]
		        } else {
		            set cmd [list formulae::_open_html_help $key $wp [$node @help_html]]
		        }
		        ttk::button $fb.b2 -image [cu::get_icon help-16] \
		            -style Toolbutton -command $cmd
		        grid $fb.b1 $fb.b2 -sticky w
		    } else {
		        grid $fb.b1 -sticky w
		    }
		    grid $l $fb -sticky nw -padx 2 -pady 2
		    grid configure $l -sticky nwe
		    grid columnconfigure $f 0 -weight 1
		    tooltip $b [_ "Update values (Return)"]
		    bind $l <ButtonRelease-3> [namespace code [list entry_contextual_menu \
		                $key $wp [$node @n] container %X %Y]]
		}
		set l [ttk::labelframe $f.l[incr idxw] -text [node_pn $node] \
		        -padding "0 0 0 2"]
		grid $l -sticky nsew -columnspan 2 -padx 5 -pady "2 5"
		set ncols [lindex [grid size $f] 1]
		foreach i [range 0 $ncols] { grid rowconfigure $f $i -weight 0 }
		grid rowconfigure $f $ncols -weight 1

		set ns { svg http://www.w3.org/2000/svg }
		set svgNode [$node selectNodes -namespaces $ns svg:svg]
		if { $svgNode ne "" } {
		    package require xml2pdf
		    ttk::panedwindow $l.p -orient horizontal
		    $l.p add [frame $l.p.f1]
		    canvas $l.p.c -bg white
		    set svg2draw [svg2draw %AUTO% -destroy_with_canvas 1 -canvas $l.p.c]
		    set svg_stack [dict get $values $key svg_stack]
		    lappend svg_stack $svg2draw
		    dict set values $key svg_stack $svg_stack
		    if { [dict get $values $key state] eq "normal" } {
		        $svg2draw configure -add_to_contextual \
		            [namespace code [list add_drawing_contextual_menu $key \
		                    [$node @n] $l.p.c]]
		    }
		    catch { $svg2draw draw -svgNode $svgNode }
		    $l.p add $l.p.c
		    grid $l.p -sticky nwes
		    grid columnconfigure $l 0 -weight 1
		    grid rowconfigure $l 0 -weight 1
		    set l $l.p.f1
		}
		if { [$node @window_create_func ""] ne "" } {
		    set cmd [$node @window_create_func]
		    lappend cmd $key $l $node formulae::uvalues
		    uplevel #0 $cmd
		} else {
		    process_window_container $key $l $node $idx
		}
		bind $l <ButtonRelease-3> [namespace code [list entry_contextual_menu \
		            $key $wp [$node @n] container %X %Y]]

		if { ![llength [winfo children $l]] } {
		    $l configure -height 50
		}
		
		bind $l <ButtonRelease-3> [namespace code [list entry_contextual_menu \
		            $key $wp [$node @n] container %X %Y]]
		incr numlf
		incr numContainers
	    }
	}
    }
    if { !$numContainers && [dict get $values $key state] ne "disabled" } {
	cu::nicelabel $f.lwarning -width 500
	set t [_ "Use contextual menu (right button mouse) over the field elements in order to modify this form"]
	$f.lwarning insert end $t "bold red"
	
	grid $f.lwarning - -sticky ew -padx 2 -pady 40
	grid columnconfigure $f 0 -weight 1
	
	bind $f.lwarning <ButtonRelease-3> [namespace code [list header_contextual_menu $key $wp \
		    %X %Y]]
    }
    dict unset values $key creating_window
    window_actualize $key
    catch { $wp.n select $active_tab }
    $wp.n _compute_size

    cu::set_focus_recursive $f
    
    #after idle [namespace code [list _check_paned_size $wp.n]]
    
    if { [dict get $values $key state] eq "disabled" } {    
	$wp.n manage_database fill_entries_default
    }
    return $wp.n
}

# proc formulae::_check_paned_size { w } {
#     
#     if { [string match paned* [winfo manager $w]] } {
#         set p [lindex [grid slaves [winfo parent $w]] 0]
#         set h [winfo reqheight $w]
#         #$p sashpos 0 $h
#         $win sash place 0 0 $h
#     }
# }

proc formulae::adjust_canvas_frame { key c f sb sh } {
    variable values
    
    $c configure -height [winfo reqheight $f] -width [winfo reqwidth $f]
    if { [winfo width $c] == 1 } { return }
    #update idletasks
    
    set height [winfo reqheight $f]
    if { [winfo height $c] > $height } {
	set height [winfo height $c]
    }
    set width [winfo reqwidth $f]
    if { [winfo width $c] > $width } {
	set width [winfo width $c]
    }
    
    if { ![dict get $values $key use_scrollbarsV] } {
	set height [winfo height $c]
    }
    if { ![dict get $values $key use_scrollbarsH] } {
	set width [winfo width $c]
    }
    
    $c itemconfigure 1 -width $width -height $height
    $c configure -scrollregion [list 0 0 [winfo reqwidth $f] [winfo reqheight $f]]
    $c configure -height [winfo reqheight $f] -width [winfo reqwidth $f]          
       
    if { [winfo height $c] < $height - 10 } {
	grid $sb
	raise $sb
    } else {
	grid remove $sb
    }
    if { [winfo width $c] < $width - 10 } {
	grid $sh
	raise $sb
    } else {
	grid remove $sh
    }
}

proc formulae::recursively_change_state { w state } {
    catch { $w state $state }
    foreach i [winfo children $w] {
	recursively_change_state $i $state
    }
}

proc formulae::_check_param_active_inactive { args } {
    variable values
    variable uvalues

   set optional {
	{ -changed_checkbutton "" 0 }
    }
    set compulsory "cb key n vs"
    parse_args $optional $compulsory $args

    if { $changed_checkbutton } {
	if { [$cb instate selected] } {
	    if { [dict exists $values $key save_value $n] } {
		set v [dict get $values $key save_value $n]
	    } else {
		set v [lindex $vs 1]
	    }
	    set state !disabled
	} else {
	    set v [lindex $vs 0]
	    set state disabled
	}
	if { $uvalues($key,$n) ne $v } {
	    dict set values $key save_value $n $uvalues($key,$n)
	    set uvalues($key,$n) $v
	}
    } else {
	if { $uvalues($key,$n) eq [lindex $vs 0] } {
	    $cb state "!selected !alternate"
	    set state disabled
	} else {
	    $cb state "selected !alternate"
	    set state !disabled
	}
    }
    set inf [grid info $cb]
    if { $inf ne "" } {
	set parent [dict get [grid info $cb] -in]
	set row [dict get [grid info $cb] -row]
	foreach w [grid slaves $parent -row $row] {
	    if { $w ne $cb } {
		recursively_change_state $w $state
	    }
	}
    }
}

proc formulae::_sizegrip_motion { win args } {
    lassign $args X Y
    set h [expr {$Y-[winfo rooty $win]}]
    if { $h < 60 } { set h 60 }
    grid propagate $win 0
    $win configure -height $h
}

proc formulae::set_font { fp } {
    
    while 1 {
	set err [catch { $fp cget -font } font]
	if { $err } {
	    if { $fp eq "." } {
		if { "MainFont" in [font names] } {
		    set font MainFont
		} else {
		    set font TkDefaultFont
		}
		break
	    }
	} else { break }
	set fp [winfo parent $fp]
    }   
    return $font
}

# create window for container
proc formulae::process_window_container { key w containerNode numContainer } {
    variable values
    variable uvalues

    set interp [dict get $values $key interp]
    
    set wp $w
    while { $wp ne "." && [winfo width $wp] < 2 } { set wp [winfo parent $wp] }
    set len [expr {[winfo width $wp]-5}]
    
    set max_grid_column ""
    set idxw 0
    set font [set_font $w]
    # Radiobuttons & checkbuttons form style
    ttk::style configure RBFormStyle {*}[ttk::style configure TRadiobutton] -font $font
    ttk::style layout RBFormStyle [ttk::style layout TRadiobutton]
    
    ttk::style configure CBFormStyle {*}[ttk::style configure TCheckbutton] -font $font
    ttk::style layout CBFormStyle [ttk::style layout TCheckbutton]
       
    set current_row 0
    foreach node [$containerNode selectNodes param|condition] {
	if { [$node nodeName] eq "param" } {
	    dict set values $key nicer_name [$node @n ""] [node_pn_xml $node]
	}
	set node_state [$node @state normal]
	if { [dict get $values $key showhidden] } { set node_state normal }
	
	set field_type [$node @field_type ""]
	set dict [comma_field_to_list [$node @dict ""]]
	set editable [$node @editable 0]
	if { [regexp {\seditable$} $field_type] } {
	    set editable 1
	}
	if { $field_type eq "" } {
	    if { [$node @values ""] ne "" } {
		if { $editable } {
		    set field_type "options editable"
		} else {
		    set field_type "options non-editable"
		}
	    } elseif { [$node @command ""] ne "" } {
		set field_type "button"
	    }
	}
	if { $field_type in [list "options editable" "options non-editable"] } {
	    set vs [$node @values ""]
	    if { [string index $vs 0] eq "\[" } {
		# to be calculated in postcommand
		set valuesList ""
	    } else {
		set valuesList [comma_field_to_list $vs]
		if { [llength $valuesList] == 2 &&  \
		    [string is boolean -strict [lindex $valuesList 0]] && \
		    [string is boolean -strict [lindex $valuesList 1]] } {
		    set field_type boolean
		} elseif { [llength $valuesList] == 2 && $editable == 0 } {
		    set field_type radiobutton
		}
	    }
	}
	if { [string index [$node @value ""] 0] == "\[" } {
	    set is_formula 1
	    if { $field_type in [list "formatted expression" "text expression"] } {
		set is_formula 0
	    }
	} else {
#             set err [catch { expr {[$node @value]*1.0} } v]
#             if { $err } { set v "!ERROR" }
	    set v [$node @value ""]
	    $interp eval [list set ::[$node @n] $v]
	    set is_formula 0
	}
	set ws ""
	if { !$is_formula && $node_state eq "hidden" } {
	    set wList ""
	} elseif { $is_formula && $node_state eq "hidden" } {
	    set cmd [list formulae::calculate_and_update_formula $key [$node nodeName]]
	    foreach i [list n pn value units true false] {
		lappend cmd [$node @$i ""]
	    }
	    lappend cmd ""
	    uplevel #0 $cmd
	    set update_stack [dict get $values $key update_stack]
	    lappend update_stack $cmd
	    dict set values $key update_stack $update_stack
	} elseif { $field_type eq "report_maker" } {
	    set f [frame $w.f[incr idxw] -bd 1 -relief raised]
	    ttk::label $f.l1 -text [node_pn $node]
	    $f.l1 configure -font $font
	    set w_rm [cu::report_maker $f.rm -list_side right -has_sizegrip 1 \
		    -lastdircmd [list formulae::manage_lastdir $key] \
		    -exportimagesinline 1]
	    
	    grid $f.l1 -sticky w -padx 2 -pady 2
	    grid $w_rm -sticky nsew -padx 2 -pady 2
	    grid columnconfigure $f 0 -weight 1
	    grid rowconfigure $f 1 -weight 1
	    grid $f -sticky nwe -columnspan 3 -padx 4 -pady 2
	    lappend ws [list $f.l1 $w_rm]
	    set gridList $f
	    set sticky ew
	    
	    set mgc [expr {[llength $gridList]-1}]
	    if { $max_grid_column eq "" || $mgc > $max_grid_column } {
		set max_grid_column $mgc
	    }
	    
	    set uvalues($key,[$node @n]) [$node @value ""]           
	    
	    set cmd0 [list formulae::report_maker_ops $key $node $w_rm]
	    uplevel #0 [concat $cmd0 update_from_interp]
	    set update_stack [dict get $values $key update_stack]
	    lappend update_stack [concat $cmd0 update_from_interp]
	    dict set values $key update_stack $update_stack
	    
	    set cmd [concat $cmd0 actualize_uvalues]
	    trace add variable formulae::uvalues($key,[$node @n]) read "$cmd;#"
	    bind $w_rm <Destroy> [list trace remove variable \
		    formulae::uvalues($key,[$node @n]) read "$cmd;#"]
	    
	    set cmd [concat $cmd0 actualize_from_uvalues]
	    trace add variable formulae::uvalues($key,[$node @n]) write "$cmd;#"
	    bind $w_rm <Destroy> +[list trace remove variable \
		    formulae::uvalues($key,[$node @n]) write "$cmd;#"]

	    set wList [list $f.l1 $w_rm]
	} elseif { $field_type eq "table" } {
	    set f [frame $w.f[incr idxw] -bd 1 -relief raised]
	    
	    lassign "" wList
	    if { [$node @pn ""] ni [list "" "-"] } {
		ttk::label $f.l -text [node_pn $node]
		lappend wList $f.l
		grid $f.l -sticky w
	    }
	    set columns ""
	    set cols [get_table_columns $node]
	    if { [llength $cols] == 0 } {
		error [_ "Field type 'table' needs a 'columns' attribute or columns/column children"]
	    }
	    foreach i $cols {
		lassign $i name dict
		foreach "opt default" [list len 10 justify left expand ""] {
		    set $opt [dict_getd $dict $opt $default]
		}
		lappend columns [list $len $name $justify text 1 $expand]
	    }
	    set cmd0 [list formulae::widget_table_ops $key $node $f.wf1]
	    set wtable [fulltktree $f.wf1 -columns $columns -showbuttons 0 -showlines 0 \
		    -width 400 -height 100 -bd 0 -relief raised -sensitive_cols all \
		    -spreadsheet_mode 1 \
		    -selectmode browse -expand 1 \
		    -button1handler [concat $cmd0 button1handler] \
		    -returnhandler [concat $cmd0 return] \
		    -editbeginhandler [concat $cmd0 edit_begin] \
		    -editaccepthandler [concat $cmd0 edit_accept] \
		    -deletehandler [concat $cmd0 delete] \
		    -contextualhandler [concat $cmd0 contextual] \
		    ]
	    set height [expr {[$wtable cget -minitemheight]*([$node @height 4]+1)}]
	    $wtable configure -height $height
	    
	    dict set values $key wtables $node $wtable
	    
	    grid $wtable -sticky ewns -padx 2 -pady 2
	    grid remove $wtable.sv 
	    grid $wtable.sh -sticky ew   
	    grid columnconfigure $f 0 -weight 1

	    if { [winfo exists $f.l] } {
		grid rowconfigure $f 1 -weight 1
	    } else {
		grid rowconfigure $f 0 -weight 1
	    }
	    ttk::sizegrip $f.grip
	    #-style TSizegripWhite
	    place $f.grip -relx 1 -rely 1 -anchor se

	    bind $f.grip <ButtonPress-1> "break"
	    bind $f.grip <B1-Motion> "[list formulae::_sizegrip_motion $f %X %Y];break"
	    bind $f.grip <ButtonRelease-1> "break"
	    
	    grid $f -sticky nwe -columnspan 3 -padx 4 -pady 2
	    lappend ws $wtable
	    set gridList $f
	    set sticky ew
	    
	    if { [$node @default_buttons ""] == 1 } {                        
		grid configure $wtable -columnspan 3
		set addb [ttk::button $f.b[incr idxw] -text [_ "Add row"] -style Toolbutton \
		        -image [$node @img ""] -command [namespace code \
		            [list contextual_menu_table $key [$node @n] $node $wp add_item]]]                    
		set editb [ttk::button $f.b[incr idxw] -text [_ "Edit row"] -style Toolbutton  \
		        -image [$node @img ""] -command [namespace code \
		            [list contextual_menu_table $key [$node @n] $node $wp edit_item]]]
		set delb [ttk::button $f.b[incr idxw] -text [_ "Delete"] -style Toolbutton \
		        -image [$node @img ""] -command [namespace code \
		            [list contextual_menu_table $key [$node @n] $node $wp delete_item]]]                                                                                                                  
		grid $addb $editb $delb -sticky e -pady 1 -padx 2
		grid configure $addb -padx "2 0"                           
		grid configure $delb -padx "2 20"
		lappend $wList $addb $editb $delb
	    }                                 
	    
	    set mgc [expr {[llength $gridList]-1}]
	    if { $max_grid_column eq "" || $mgc > $max_grid_column } {
		set max_grid_column $mgc
	    }
	    set uvalues($key,[$node @n]) [$node @value ""]            
	    
	    set cmd [concat $cmd0 update_from_interp]
	    uplevel #0 $cmd
	    set update_stack [dict get $values $key update_stack]
	    lappend update_stack $cmd
	    dict set values $key update_stack $update_stack
	    
	    set cmd [concat $cmd0 actualize_uvalues]
	    trace add variable formulae::uvalues($key,[$node @n]) read "$cmd;#"
	    bind $wtable <Destroy> [list trace remove variable \
		    formulae::uvalues($key,[$node @n]) read "$cmd;#"]                       
	    
	    set cmd [concat $cmd0 actualize_from_uvalues]
	    trace add variable formulae::uvalues($key,[$node @n]) write "$cmd;#"
	    bind $wtable <Destroy> [list trace remove variable \
		    formulae::uvalues($key,[$node @n]) write "$cmd;#"]

	    lappend wList $wtable
	} elseif { $is_formula } {
	    set f [frame $w.f[incr idxw]]
	    #set fp [winfo parent $f]
	    #set font [set_font $fp]
	    set fontsize [font actual $font -size]
	    set wimg [wordwidgetformula $f.wf1 -font $font -normalize_spaces 0 \
		    -fontsize $fontsize -maxwidth [expr {int(1.0*$len)}]]
	    grid $wimg -sticky w
	    grid $f -sticky nw -columnspan 3 -padx "2 0" -pady 2
	    lappend ws $wimg
	    set gridList $f
	    
	    set cmd [list formulae::calculate_and_update_formula $key [$node nodeName]]
	    foreach i [list n pn value units true false] {
		lappend cmd [$node @$i ""]
	    }
	    lappend cmd $wimg
	    uplevel #0 $cmd
	    set update_stack [dict get $values $key update_stack]
	    lappend update_stack $cmd
	    dict set values $key update_stack $update_stack
	    set wList $wimg
	} else {
	    lassign "" gridList wList

	    if { $field_type in "button boolean" } {
		set l ""
	    } elseif { [regexp {<(sup|sub|emphasis)} [node_pn_xml $node]] } {
		set l [wordwidgetformula $w.wf[incr idxw] \
		        -maxwidth 100 -formula [node_pn_xml $node]:]
		$l configure -font $font
		lappend gridList $l
		lappend wList $l
	    } elseif { [$node @disabled_value ""] ne "" }  {
		set vs [$node @disabled_value]
		set l $w.l[incr idxw]
		set l [ttk::checkbutton $l -text [node_pn $node]: \
		        -command [list formulae::_check_param_active_inactive \
		            -changed_checkbutton $l $key [$node @n] $vs]]
		$l configure -style CBFormStyle
		lappend gridList $l
		lappend wList $l
		
		set cmd [list formulae::_check_param_active_inactive $l $key [$node @n] \
		        $vs]
		set vn [namespace current]::uvalues($key,[$node @n])
		trace add variable $vn write "$cmd;#"
		bind $l <Destroy> [list trace remove variable $vn write "$cmd;#"]
	    } elseif { [node_pn $node] eq "-" } {
		set l ""
	    } else {
		set l [ttk::label $w.l[incr idxw] -text [node_pn $node]: \
		        -takefocus 0]
		$l configure -font $font
		lappend gridList $l
		lappend wList $l
	    }
	    switch -- [$node @units ""] "" { set isnumber 0 } default { set isnumber 1 }

	    set sticky w
	    if { $field_type eq "date" } {
		set e [cu::dater_entry $w.e[incr idxw] -textvariable \
		        [namespace current]::uvalues($key,[$node @n])]
		$e configure -font $font
		if { [dict get $values $key state] eq "normal" } {
		    $e configure -editmode 1
		}
		lappend gridList $e
		lappend wList $e
	    } elseif { $field_type eq "color" } {
		set f [ttk::frame $w.f[incr idxw]]
		set e [ttk::entry $f.e -textvariable \
		        [namespace current]::uvalues($key,[$node @n]) -width 8]
		set l [label $f.l -text "  " -relief solid -bd 1]
		set cmd [namespace code [list fieldtype_color_ops change $w $e $l $f.b $key $node]]
		set b [cu::menubutton_button $f.b -command $cmd \
		        -image [cu::get_icon colors-16] -menu $f.b.m]
		menu $f.b.m -tearoff 0
		$f.b.m configure -font $font
		$f.b.m add command -label [_ "Select color"]... \
		    -image [cu::get_icon colors-16] -command $cmd -compound left
		$f.b.m add separator

		if { [$node @value] ni [list "" auto] } {
		    set cmd [namespace code [list fieldtype_color_ops initial_color $w $e $l $f.b \
		                $key $node]]
		    $f.b.m add command -label [_ "Initial value"] -command $cmd -image \
		        [formulae::create_give_image_color [$node @value]] -compound left
		}
		if { [$node @default_value ""] ni [list "" auto] } {
		    set cmd [namespace code [list fieldtype_color_ops default_color $w $e $l $f.b \
		                $key $node]]
		    $f.b.m add command -label [_ "Default value"] -command $cmd  -image \
		        [formulae::create_give_image_color [$node @default_value [$node @value]]] \
		        -compound left
		}
		set cmd [namespace code [list fieldtype_color_ops \
		            modvar $w $e $l $f.b $key $node]]
		trace add variable [namespace current]::uvalues($key,[$node @n]) write \
		    "$cmd;#"
		bind $l <Destroy> [list trace remove variable \
		        [namespace current]::uvalues($key,[$node @n]) \
		        write "$cmd;#"]
		grid $e $l $b -sticky w
		lappend gridList $f
		lappend wList $e $l $b
	    } elseif { $field_type eq "file" } {
		set f [ttk::frame $w.f[incr idxw]]
		set e [ttk::entry $f.e -textvariable \
		        [namespace current]::uvalues($key,[$node @n])]
		set b [cu::menubutton_button $f.b \
		        -image [cu::get_icon filenew-16] -style Toolbutton]
		grid $e $b -sticky w
		$b configure -menu $b.m -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" open_browser $key $node]]
		menu $b.m -tearoff 0
		$b.m configure -font $font
		$b.m add command -image [cu::get_icon filenew-16] -label [_ "Select file"] \
		    -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" open_browser $key $node]] -compound left
		
		if { [$node @examples_dir ""] ne "" } {
		    set dir [$node @examples_dir]
		    $b.m add command -image [cu::get_icon filenew-16] -label [_ "Select file (examples directory)"] \
		        -command [namespace code [list fieldtype_file_ops \
		                $w "$e $b" open_browser $key $node -importdir $dir]] -compound left
		}
		$b.m add command -label [_ "View/execute"] \
		    -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" execute $key $node]]
		$b.m add separator
		
		$b.m add command -image [cu::get_icon remove-16] -label [_ "Clear"] \
		    -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" clear $key $node]] -compound left

		$b.m add separator
		
		$b.m add command -label [_ "Reload"] \
		    -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" reload $key $node ""]] -state disabled
		dict set values $key reload_file [$node @n] ""
		dict set values $key reload_menu_pos [$node @n] [list $b.m [$b.m index end]]
		
		set dirs [cu::file::give_standard_dirs]
		set tmpdir [cu::file::tempdir]

		foreach dir $dirs {
		    regsub {([\\/]).*([\\/])} $dir {\1...\2} dir_nice
		    $b.m add command -label [_ "Extract in '%s'" $dir_nice] \
		        -command [namespace code [list fieldtype_file_ops \
		                $w "$e $b" extract $key $node $dir]]
		}
		$b.m add command -label [_ "Extract in ..."] \
		    -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" extract $key $node ""]]
		# Ramviewer option has been removed temporarily
#                 $b.m add separator
#                 $b.m add command -label [_ "Ramviewer"] \
#                     -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" ramviewer $key $node]]
		if { [info command dnd] ne "" } {
		    foreach i [list $e $b] {
		        dnd bindtarget $i text/uri-list <Drop> [namespace code [list fieldtype_file_ops \
		                    $w "$e $b" open_files $key $node %D]]
		    }
		}
		bind $e <<Paste>> [namespace code [list fieldtype_file_ops \
		            $w "$e $b" paste $key $node $e]]
		lappend gridList $f
		lappend wList $e $b

		set cmd [namespace code [list fieldtype_file_ops $w $wList update_tooltip \
		            $key $node]]
		trace add variable [namespace current]::uvalues($key,[$node @n]) write "$cmd;#"
		bind $e <Destroy> [list trace remove variable \
		        [namespace current]::uvalues($key,[$node @n]) write "$cmd;#"]
	    } elseif {$field_type eq "directory"} {
		set f [ttk::frame $w.f[incr idxw]]
		set e [ttk::entry $f.e -textvariable \
		        [namespace current]::uvalues($key,[$node @n])]
		set b [cu::menubutton_button $f.b \
		        -image [cu::get_icon filenew-16] -style Toolbutton]
		grid $e $b -sticky w
		$b configure -menu $b.m -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" open_browser $key $node -isdir 1]]
		menu $b.m -tearoff 0
		$b.m configure -font $font
		              
		$b.m add command -image [cu::get_icon filenew-16] -label [_ "Select directory"] \
		    -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" open_browser $key $node -isdir 1]] -compound left
		                            
		$b.m add separator
		
		$b.m add command -image [cu::get_icon remove-16] -label [_ "Clear"] \
		    -command [namespace code [list fieldtype_file_ops \
		            $w "$e $b" clear $key $node]] -compound left 
		
		dict set values $key reload_file [$node @n] ""
		dict set values $key reload_menu_pos [$node @n] ""
		    
		if { [info command dnd] ne "" } {
		    foreach i [list $e $b] {
		        dnd bindtarget $i text/uri-list <Drop> [namespace code [list fieldtype_file_ops \
		                    $w "$e $b" open_files $key $node %D]]
		    }
		}
		bind $e <<Paste>> [namespace code [list fieldtype_file_ops \
		            $w "$e $b" paste $key $node $e]]
		lappend gridList $f
		lappend wList $e $b

		set cmd [namespace code [list fieldtype_file_ops $w $wList update_tooltip \
		            $key $node]]
		trace add variable [namespace current]::uvalues($key,[$node @n]) write "$cmd;#"
		bind $e <Destroy> [list trace remove variable \
		        [namespace current]::uvalues($key,[$node @n]) write "$cmd;#"]
	    } elseif { $field_type in [list "long text" "formatted" "formatted expression" \
		"text expression"] } {
		if { [$node @min_max_height ""] ne "" } {
		    set min_max_height [$node @min_max_height]
		    set mode dynamic
		} elseif { $field_type in [list "long text" "text expression"] } {
		    set min_max_height "1 3"
		    set mode off
		} else {
		    set min_max_height "3 6"
		    set mode dynamic
		}
		if { [lindex $min_max_height 0] > 3 } {
		    set mode on
		}
		set e [wordwidget_and_toolbox $w.e[incr idxw] \
		        -highlightthickness 1 -bd 0 -clientdata [$node @n] \
		        -min_max_height $min_max_height -mode $mode -speed slow \
		        -lastdircmd [list formulae::manage_lastdir $key] \
		        -exportimagesinline 1 \
		        -width 100 \
		        -has_sizegrip 1]
		
		if { [lindex $min_max_height 0] > 3 } {
		    set fe [ttk::frame $w.f[incr idxw]]
		    if { $l ne "" } {
		        grid $l -in $fe -sticky w -padx 2 -pady 2
		        raise $l
		        set l ""
		    }
		    grid $e -in $fe -padx 2 -pady 2 -sticky nsew
		    grid columnconfigure $fe 0 -weight 1
		    grid rowconfigure $fe 1 -weight 1
		    raise $e
		    lappend wList $e
		    set gridList $fe
		} else {
		    lappend gridList $e
		    lappend wList $e
		}
		set sticky we
		if { [lsearch [list "formatted expression" "text expression"] \
		    $field_type] != -1 } {
		    set cmd [namespace code [list calculate_and_update_formatted $e \
		                value [$node @field_type] $key [$node @n] [$node @value]]]
		    set update_stack [dict get $values $key update_stack]
		    lappend update_stack $cmd
		    dict set values $key update_stack $update_stack
		    $e configure -readonly 1 -bd 0
		    uplevel #0 $cmd
		} else {
		    set cmd [namespace code [list update_variable_formatted $e \
		                $field_type $key [$node @n]]]
		    trace add variable [namespace current]::uvalues($key,[$node @n]) read \
		        "$cmd;#"
		    bind $e <Destroy> [list trace remove variable \
		            [namespace current]::uvalues($key,[$node @n]) read "$cmd;#"]
		}
		set cmd [namespace code [list calculate_and_update_formatted $e \
		            variable [$node @field_type] $key [$node @n] [$node @value]]]
		trace add variable [namespace current]::uvalues($key,[$node @n]) write \
		    "$cmd;#"
		bind $e <Destroy> [list trace remove variable \
		        [namespace current]::uvalues($key,[$node @n]) write "$cmd;#"]
	    } elseif { $field_type eq "boolean" } {
		set e [ttk::checkbutton $w.e[incr idxw] -text [node_pn $node] \
		        -variable [namespace current]::uvalues($key,[$node @n]) \
		        -command [namespace code [list entry_validate -window_actualize \
		                %W [$node @n] $key $isnumber]]]
		$e configure -style CBFormStyle
		dict set values $key validate [$node @n] [list formulae::entry_validate \
		        -show_error $numContainer $e [$node @n] $key $isnumber]
		lappend gridList $e
		lappend wList $e
	    } elseif { $field_type eq "button" } {
		if { [$node @global_interp 0] == 1 } {
		    set cmd [$node @command ""]
		} elseif { [$node @global_interp_v 0] == 1 } {
		    set cmd [$node @command ""]
		    lappend cmd [dict get $values $key]
		} else {
		    set cmd [list formulae::eval_in_interp $key {*}[$node @command ""]]
		}
		set image [$node @img ""]
		if { $image ne "" && [info command $image] eq "" } {
		    set err [catch { cu::get_image_or_icon $image } ret]
		    if { !$err } {
		        set image $ret
		    } else {
		        set err [catch { image create photo -data [formulae::eval_in_interp $key $image data \
		                        -format png] } ret]
		        if { !$err } {
		            set image $ret
		        }
		    }
		}
		if { [$node @only_in_edit_mode 0] && [dict get $values $key state] ne "normal" } {
		    continue
		} elseif { [$node @values_tree ""] eq "" } {
		    set e [ttk::button $w.e[incr idxw] -text [node_pn $node] \
		            -image $image -command $cmd]
		} else {
		    set cols [get_table_columns $node]
		    set e [cu::menubutton_tree $w.e[incr idxw] -text [node_pn $node] \
		            -image $image -command $cmd -columns_list $cols]
		    $e configure -postcommand [namespace code [list update_combox_tree $e $key \
		                $node]]
		}
		if { [$node @pn ""] ne "" && [$node @img ""] ne "" } {
		    $w.e$idxw configure -compound left
		} elseif { [$node @img ""] ne "" } {
		    $w.e$idxw configure -style Toolbutton
		}
		lappend gridList $e
		lappend wList $e
	    } elseif { [string match "*multiple*" $field_type] } {
		if { $editable == 0 } {
		    set e [cu::menubutton_check_listbox $w.e[incr idxw] -width 17 -selected_in_label 1 \
		            -permit_rename 0 -add_new_button 0]
		} else {
		    set e [cu::menubutton_check_listbox $w.e[incr idxw] -width 17 -selected_in_label 1 \
		            -permit_rename 1 -permit_delete 1 -add_new_button 1]                
		}
		$e configure -postcommand [list formulae::options_multiple_ops $key $node $e post]
		
		set uvalues($key,[$node @n]) [$node @value ""]           
	    
		set cmd0 [list formulae::options_multiple_ops $key $node $e]
		uplevel #0 [concat $cmd0 update_from_interp]
		set update_stack [dict get $values $key update_stack]
		lappend update_stack [concat $cmd0 update_from_interp]
		dict set values $key update_stack $update_stack
	    
		set cmd [concat $cmd0 actualize_uvalues]
		trace add variable formulae::uvalues($key,[$node @n]) read "$cmd;#"
		bind $e <Destroy> [list trace remove variable \
		        formulae::uvalues($key,[$node @n]) read "$cmd;#"]
	    
		set cmd [concat $cmd0 actualize_from_uvalues]
		trace add variable formulae::uvalues($key,[$node @n]) write "$cmd;#"
		bind $e <Destroy> +[list trace remove variable \
		        formulae::uvalues($key,[$node @n]) write "$cmd;#"]

		lappend gridList $e
		lappend wList $e                
	    } elseif { $field_type eq "radiobutton" } {
		set ef [ttk::frame $w.e[incr idxw]]
		lassign [list 0 ""] idx e
		foreach v $valuesList {
		    set tl [dict_getd $dict $v $v]
		    set r [ttk::radiobutton $w.e$idxw.r$idx -text $tl -value $v -style RBFormStyle \
		            -variable [namespace current]::uvalues($key,[$node @n]) \
		            -command [list formulae::entry_validate %W [$node @n] $key $isnumber]]
		    grid $r -row 0 -column $idx -sticky w -padx 2
		    lappend e $r
		    lappend wList $r
		    incr idx
		}
		dict set values $key validate [$node @n] [list formulae::entry_validate \
		        -show_error $numContainer $w.e$idxw.r0 [$node @n] $key $isnumber]
		set cmd [namespace code [list update_non_editable_field \
		            $w.e$idxw.r0 $key $node $valuesList]]
		trace add variable [namespace current]::uvalues($key,[$node @n]) write \
		    "$cmd;#"
		bind $ef <Destroy> [list trace remove variable \
		        [namespace current]::uvalues($key,[$node @n]) write "$cmd;#"]
		lappend gridList $w.e$idxw
	    } elseif { [$node @values_tree ""] ne "" } {
		set e [cu::combobox_tree $w.e[incr idxw] -width 20 -textvariable \
		        [namespace current]::uvalues($key,[$node @n]) \
		        -validate focusout -validatecommand \
		        [namespace code [list entry_validate %W [$node @n] $key $isnumber]] \
		        -invalidcommand [namespace code [list entry_invalid %W]]]
		$e configure -postcommand [namespace code [list update_combox_tree $e $key \
		            $node]]
		if { $editable != 1 } {
		    $e state readonly
		    if { $editable eq "semi" } {
		        $e configure -semi_readonly 1
		    }
		}
		dict set values $key validate [$node @n] [list formulae::entry_validate \
		        -show_error $numContainer $e [$node @n] $key $isnumber]
		lappend gridList $e
		lappend wList $e
	    } elseif { $field_type in [list "options editable" "options non-editable"] } {
		set e [cu::combobox $w.e[incr idxw] -width 17 -textvariable \
		        [namespace current]::uvalues($key,[$node @n]) \
		        -validate focusout -validatecommand \
		        [namespace code [list entry_validate \
		                %W [$node @n] $key $isnumber]] \
		        -invalidcommand [namespace code [list entry_invalid %W]] \
		        -values $valuesList -dict $dict]
		dict set values $key validate [$node @n] [list formulae::entry_validate \
		        -show_error $numContainer $e [$node @n] $key $isnumber]
		if { [string index [$node @values ""] 0] eq "\[" } {
		    $e configure -postcommand [namespace code [list update_combox $e $key \
		                $node]]
		}
		if { $editable == 0 } {
		    $e state readonly
		    set cmd [namespace code [list update_non_editable_field \
		                $e $key $node $valuesList]]
		    trace add variable [namespace current]::uvalues($key,[$node @n]) write \
		        "$cmd;#"
		    bind $e <Destroy> [list trace remove variable \
		            [namespace current]::uvalues($key,[$node @n]) write "$cmd;#"]
		}
		lappend gridList $e
		lappend wList $e
	    } else {
		set e [ttk::entry $w.e[incr idxw] -width 20 -textvariable \
		        [namespace current]::uvalues($key,[$node @n]) \
		        -validate focusout -validatecommand \
		        [namespace code [list entry_validate %W [$node @n] $key $isnumber]] \
		        -invalidcommand [namespace code [list entry_invalid %W]]]
		dict set values $key validate [$node @n] [list formulae::entry_validate \
		        -show_error $numContainer $e [$node @n] $key $isnumber]
		lappend gridList $e
		lappend wList $e
	    }
	    if { [dict get $values $key state] eq "disabled" && 
		![dict get $values $key usableviewmode] } {
		foreach e_in $e { $e_in state disabled }
	    }
	    foreach e_in $e {
		if { [winfo class $e_in] eq "Wordwidget_and_toolbox" } {
		    #set e_in [$e_in give_worwidget]
		    continue
		}
		bindtags $e_in [concat $interp [bindtags $e_in]]
		bind $e_in <Return> [namespace code [list window_actualize_entry %W \
		            [$node @n] $key $isnumber]]
	    }
	    set sticky_col [lindex $gridList end]
	    
	    if { [$node @units ""] ne "" } {
		set u [ttk::label $w.l[incr idxw] -text [$node @units ""]]
		$u configure -font $font
		lappend gridList $u
		lappend wList $u
	    } else { set u "" }

	    if { [$node @help_html ""] ne "" || [$node @help_lognoter ""] ne "" } {
		if { [$node @help_lognoter ""] ne "" } {
		    set cmd [list formulae::_open_lognoter_help $key $w [$node @help_lognoter]]
		} else {
		    set cmd [list formulae::_open_html_help $key $w [$node @help_html]]
		}
		set hh [ttk::button $w.hh[incr idxw] -image [cu::get_icon help-16] \
		        -style Toolbutton -command $cmd]
		lappend gridList $hh
		lappend wList $hh
	    } else {
		set hh ""
	    }
	    grid {*}$gridList -sticky $sticky -pady 1 -row $current_row
	    
	    if { [llength $gridList] == 1 } {
		grid configure $gridList -columnspan 2
	    }
	    if { $l ne "" } {
		grid configure $l -padx "2 0"
	    } else {
		grid configure [lindex $gridList 0] -padx "2 0"
	    }
	    if { [llength $e] == 1 && [winfo class $e] eq "Wordwidget_and_toolbox" } {
		grid configure $e -columnspan 2 -padx "0 4"
		set sticky ""
	    }
	    if { $u ne "" } { grid configure $u -padx 2 -sticky w }
	    if { [regexp {e} $sticky] && [regexp {w} $sticky] } {
		grid configure [lindex $gridList end] -padx "0 2"
		grid columnconfigure $w $sticky_col -weight 1
	    } else {
		set mgc [expr {[llength $gridList]-1}]
		if { $max_grid_column eq "" || $mgc > $max_grid_column } {
		    set max_grid_column $mgc
		}
#                 in this moment, when a column is too short, one scrollbar appears
#                 set grid_widget [lindex $gridList $max_grid_column]
#                 if { $grid_widget ne "" } {
#                     cu::grid_configure_sticky $grid_widget
#                 } 
	    }
	    # values are applied after grid as some traces on value
	    # require the widgets to be grided in order to change state
	    if { $field_type ne "formatted expression" && \
		$field_type ne "text expression"} {
		set uvalues($key,[$node @n]) $v
#                 if { $dict eq "" || ![dict exists $dict $v] } {
#                     set uvalues($key,[$node @n]) $v
#                 } else {
#                     set uvalues($key,[$node @n]) [dict get $dict $v]
#                 }
	    } else {
	       #set uvalues($key,[$node @n]) ""
	    }
	    lappend ws {*}$wList
	}
	foreach i $wList {
	    if { [winfo class $i] eq "Wordwidget_and_toolbox" } {
		set i [$i give_worwidget]
	    }
	    bind $i <ButtonRelease-3> [namespace code [list entry_contextual_menu \
		        $key $wp [$node @n] param %X %Y]]
	}
	if { [$node @condition ""] ne "" } {                    
	    set cmd [namespace code [list check_field_condition \
		        [$node @n ""] [$node @field_type ""] [$node @condition ""] $wList $gridList $key]]
	    set update_stack [dict get $values $key update_stack]
	    lappend update_stack $cmd
	    dict set values $key update_stack $update_stack
	}
	if { [$node @onchange ""] ne "" } {
	    set cmd [list formulae::eval_onchange $key $node]
	    trace add variable [namespace current]::uvalues($key,[$node @n]) write \
		"$cmd;#"
	    bind [lindex $wList 0] <Destroy> +[list trace remove variable \
		    [namespace current]::uvalues($key,[$node @n]) write "$cmd;#"]
	}
	if { [$node hasAttribute help] } {
	    foreach i $ws {
		tooltip $i [format_help_field [$node @help]]
	    }
	}
	if { [$node @pady ""] ne "" } {
	    foreach w_grid $gridList {
		grid configure $w_grid -pady [$node @pady]
	    }
	}
	if { [$node @location ""] in "same_line same_menu" } {
	    lassign [grid size $w] ncols nrows
	    set wi1 ""
	    foreach i [range 0 $ncols] {
		set wi [grid slaves $w -row [expr {$current_row-1}] -column $i]
		if { $wi ne "" && $wi ne [lindex $wi1 end] } {
		    lappend wi1 $wi
		}
	    }
	    set wi2 ""
	    foreach i [range 0 $ncols] {
		set wi [grid slaves $w -row [expr {$current_row-0}] -column $i]
		if { [llength $wi] > 1 } { continue }
		if { $wi ne "" && $wi ne [lindex $wi2 end] } {
		    lappend wi2 $wi
		}
	    }
	    set done 0
	    set wi1_end [lindex $wi1 end]
	    if { $wi1_end ne "" && [winfo class $wi1_end] in "Frame TFrame" } {
		lassign "" max_row max_w
		foreach i [grid slaves $wi1_end] {
		    set row [dict get [grid info $i] -row]
		    if { $max_row eq "" || $row > $max_row } {
		        lassign [list $row $i] max_row max_w
		    }
		}
		set wi1_end $max_w
	    }
	    if { [$node @location] eq "same_menu" && [llength $wi2] == 1 && [llength $wi1] &&
		[winfo class $wi1_end] in "TButton TMenubutton" && [winfo class $wi2] eq "TButton" } {
		lassign "" conf1 conf1b conf2
		if { [winfo class $wi1_end] eq "TButton" } {
		    set grid_info [grid info $wi1_end]
		    
		    set xp {preceding-sibling::param[@field_type='button' and not(@values_tree)][1]}
		    set node_prev [$node selectNodes $xp]
		    if { $node_prev ne "" && [$node_prev @pns ""] ne "" } {
		        set txt_menu [$node_prev @pns]
		        dict set conf1 -text [$node_prev @pns]
		    } else {
		        set txt_menu ""
		    }
		    foreach i [$wi1_end configure] {
		        if { [lindex $i 0] ni "-text -image -compound -command" } { continue }
		        dict set conf1b [lindex $i 0] [lindex $i 4]
		        if { $txt_menu ne "" && [lindex $i 0] in "-text -command" } { continue }
		        dict set conf1 [lindex $i 0] [lindex $i 4]
		    }
		    set BR3 [bind $wi1_end <ButtonRelease-3>]
		    set tooltip [tooltip $wi1_end]
		    destroy $wi1_end
		    cu::menubutton_button $wi1_end {*}$conf1 -menu $wi1_end.m -style ""
		    grid $wi1_end {*}$grid_info
		    menu $wi1_end.m -tearoff 0
		    $wi1_end.m add command -label [dict get $conf1b -text] -image [dict get $conf1b -image] \
		        -compound [dict get $conf1b -compound] -command [dict get $conf1b -command]
		    
		    bind $wi1_end <ButtonRelease-3> $BR3
		    tooltip $wi1_end $tooltip
		}
		foreach i [$wi2 configure] {
		    if { [lindex $i 0] ni "-text -image -compound -command" } { continue }
		    dict set conf2 [lindex $i 0] [lindex $i 4]
		}
		destroy $wi2
		$wi1_end.m add command -label [dict get $conf2 -text] -image [dict get $conf2 -image] \
		    -compound [dict get $conf2 -compound] -command [dict get $conf2 -command]
		set done 1
		incr current_row -1
	    }
	    if { !$done && [llength $wi1] } {
		set two_rows 0
		set padx_end ""
		set wi1_end [lindex $wi1 end]
		if { [winfo class $wi1_end] in "Frame TFrame" } {
		    set f1 $wi1_end
		    set icol [llength [grid slaves $f1 -row 0]]
		    if { $icol == 1 && [winfo class [lindex [grid slaves $f1 -row 0] 0]] eq "Fulltktree" } {
		        set two_rows 1
		        set icol [llength [grid slaves $f1 -row 1]]
		        set padx_end "3 20"
		    }
		} else {
		    set f1 [ttk::frame $w.f[incr idxw] -borderwidth 0]
		    set f_row [dict get [grid info [lindex $wi1 0]] -row]
		    set f_col 0
		    set icol 0
		    foreach i $wi1 {
		        raise $i $f1
		        if { $icol == 0 && $f_col == 0 && [winfo class $i] in "TLabel Label" } {
		            grid $i -sticky w -row $f_row -column $f_col -columnspan 1 -rowspan 1
		            incr f_col
		        } else {
		            grid $i -in $f1 -sticky w -row 0 -column $icol -columnspan 1 -rowspan 1
		            incr icol
		        }
		    }
		    grid $f1 -columnspan [expr {$ncols-$f_col}] -sticky ew -column $f_col -row $f_row
		}
		foreach i $wi2 {
		    raise $i
		    if { !$two_rows } {
		        grid $i -in $f1 -sticky w -row 0 -column $icol -columnspan 1 -rowspan 1
		    } else {
		        grid configure [grid slaves $f1 -row 0 -column 0] -columnspan [expr {$icol+1}]
		        grid $i -in $f1 -sticky e -row 1 -column $icol -columnspan 1 -rowspan 1
		    }
		    incr icol
		}
		if { $padx_end ne "" } {
		    grid [lindex $wi2 end] -padx $padx_end
		}
		set padx [dict get [grid info [lindex $wi2 0]] -padx]
		set padx [lreplace $padx 0 0 2]
		grid configure [lindex $wi2 0] -padx $padx
		incr current_row -1
	    }
	}
	incr current_row
    }
    if { $max_grid_column ne "" } {
	grid columnconfigure $w $max_grid_column -weight 1
    }
}

proc formulae::format_help_field { help } {
    set ret ""
    while { [string length $help] > 80 } {
	set ipos [string last " " $help 79]
	if { $ipos == -1 } {
	    append ret $help
	    break
	}
	append ret [string trimright [string range $help 0 $ipos]]\n
	set help [string trimleft [string range $help $ipos end]]
    }
    append ret $help
    return $ret
}

proc formulae::_fill_lognoter_pages_menu { key combo { parent0 "" } } {
    variable values
    
    set lognoter [dict_getd $values $key lognoter ""]
    if { $lognoter eq "" } {
	return
    }
    if { $parent0 ne "" } {
	set len [llength [$combo tree_item children $parent0]]
    } else {
	set len [llength [$combo cget -values]]
    }
    if { !$len } {
	$lognoter giveinfo linklist_combobox_tree $combo local $parent0
	$combo tree_item collapse all
    }
}

proc formulae::_open_lognoter_help { key w page } {
    variable values
    
    set lognoter [dict_getd $values $key lognoter ""]
    if { $lognoter eq "" } {
	snit_messageBox -message [_ "It is not possible to visualize help for page '%s'" $page] \
	    -parent $w
	return
    }
    $lognoter process help -open_self $page
}

proc formulae::_open_html_help { key w file } {
    GiDHelpViewer::Show $file \
	-title [= Help] \
	-report 1 \
	-base $w.hh
    
    #HelpWindow CUSTOM_HELP_FILE $file    
}

proc formulae::check_container_condition { name condition nb tab key } {
    set err 0
    if { $condition ne "" } {
	if { [string index $condition 0] == "\[" } {
	    set value [string range $condition 1 end-1]
	    
	    set valueE [string map [list \[ \\\[] $value]
    
	    if { [lindex $valueE 0] ne "e" } {
		error "error in formulae::check_container_condition"
	    }
	    set err [catch { e $key [lindex $valueE 1] } condition]
	    if { $err } {
		set errstring $condition
		set  condition 1
	    }
	} elseif { [string is boolean -strict $condition] } {
	    # nothing
	} else {
	    error [_ "error in container '%s'. Condition not ok" $name]
	}
    } else {
	set condition 1
    }
    if { $tab ne "" } {
	if { !$condition } {
	    $nb tab $tab -state disabled
	} else {
	    $nb tab $tab -state normal
	}
    }
    if { $err } {
	regsub {\s*condition error:.*$} [tooltip::tooltip $nb] {} txt
	if { $txt ne "" } { append txt "\n" }
	append txt "condition error: $name --- $errstring"
	tooltip::tooltip $nb $txt
    } elseif { [regexp {\s*condition error:.*$} [tooltip::tooltip $nb]] } {
	regsub {\s*condition error:.*$} [tooltip::tooltip $nb] {} txt
	tooltip::tooltip $nb $txt
    }
    return $condition
}

proc formulae::eval_onchange { key node } {
    # after idle is necessary so as to be called after the write trace
    after idle [list formulae::eval_onchange_do $key $node]
}

proc formulae::eval_onchange_do { key node } {
    variable values
    variable uvalues
    
    if { [dict_getd $values $key disable_onchange 0] == 1 } {
	return
    }
    
    set interp [dict get $values $key interp]
    
    foreach i [array names uvalues] {
	regexp {(.*),(.*)} $i {} key_in name
	if { $key ne $key_in } { continue }
	
	set cmd [dict_getd $values $key validate $name ""]
	if { $cmd ne "" } {
	    set ret [uplevel #0 $cmd]
	    if { $ret == 0 } {
		if { $raise_error } { error "incorrect parameter name '$name'" }
		return
	    }
	}
	$interp eval [list set ::$name $uvalues($i)]
    }
    #catch { $interp eval [$node @onchange] }
    $interp eval [$node @onchange]
}

proc formulae::check_field_condition { name field_type condition wList gridList key } {
    variable values
    
    set err 0
    if { $condition ne "" } {
	if { [string index $condition 0] == "\[" } {
	    set value [string range $condition 1 end-1]
	    
	    #set valueE [string map [list \[ \\\[] $value]
	    set valueE $value
    
	    if { [lindex $valueE 0] ne "e" } {
		error "error in formulae::check_field_condition"
	    }
	    set err [catch { e $key [lindex $valueE 1] } condition]
	    if { $err } {
		set errstring $condition
		set  condition 1
	    }
	} elseif { [string is boolean -strict $condition] } {
	    # nothing
	} elseif { $condition ni "disabled hidden" } {
	    set errstring [_ "error in field '%s'. Condition not ok" $name]
	    set  condition 1
	}
    } else {
	set condition 1
    }
    if { $condition eq "hidden" } {
	set state hidden
    } elseif { $condition eq "disabled" || !$condition } {
	set state disabled
    } else {
	set state !disabled
    }
    if { $state eq "disabled" && [dict get $values $key state] eq "disabled" } {
	if { $field_type in "expression {text expression} {formatted expression}" } {
	    set state hidden
	}
    }
    if { $state eq "hidden" } {
	foreach e $gridList {
	    grid remove $e
	}
    } else {
	foreach e $gridList {
	    grid $e
	}
	foreach e $wList {
	    catch { $e state $state }
	}
    }
    foreach e $wList {
	if { $err } {
	    regsub {\s*condition error:.*$} [tooltip::tooltip $e] {} txt
	    if { $txt ne "" } { append txt "\n" }
	    append txt "condition error: $errstring"
	    tooltip::tooltip $e $txt
	} elseif { [regexp {\s*condition error:.*$} [tooltip::tooltip $e]] } {
	    regsub {\s*condition error:.*$} [tooltip::tooltip $e] {} txt
	    tooltip::tooltip $e $txt
	}
    }
}

proc formulae::update_title_description { key wp cmd } {
    variable values
    
    set interp [dict get $values $key interp]
    
    set err [catch { $interp eval $cmd } v]
    $wp delete 1.0 end
    $wp insert end $v
}

proc formulae::update_variable_formatted { wp field_type key name } {
    variable uvalues

    switch $field_type {
	"long text" {
	    set uvalues($key,$name) [string trim [$wp get 1.0 end-1c]]
	}
	default {
	    if { ![$wp HasChanges] } { return }
	    set ret [$wp exportXML]
	    if { $ret eq "" || $ret eq "<para></para>" } {
		set uvalues($key,$name) ""
	    } else {
		set uvalues($key,$name) "<wordwidget>$ret</wordwidget>" 
	    }
	}
    }
}

proc formulae::calculate_and_update_formatted { wordwidget type field_type key name value } {
    variable values
    variable uvalues

    set interp [dict get $values $key interp]

    if { $type eq "variable" } {
	set v $uvalues($key,$name)
	if { [regexp {^<wordwidget>|<lognoter>} $v] } {
	    set vp $v
	} else {
	    set vp "<wordwidget>"
	    foreach line [split $v \n] {
		append vp "<para>[xml $line]</para>"
	    }
	    append vp "</wordwidget>"
	}
    } elseif { [string index $value 0] eq "\[" } {
	set value [string range $value 1 end-1]
	set err [catch { $interp eval $value } v]
	if { $err } {
	    set vp "<wordwidget>"
	    set numlines 0
	    foreach line [split $v \n] {
		append vp "<para>[xml "error: $line"]</para>"
		incr numlines
	    }
	    append vp "</wordwidget>"
	    set v "error: $v"
	} elseif { $field_type eq "text expression" } {
	    set _ "<wordwidget>"
	    set numlines 0
	    foreach line [split $v \n] {
		append _ "<para>[xml $line]</para>"
		incr numlines
	    }
	    append _ "</wordwidget>"
	    append vp $_
	    if { $numlines > 4 } { set numlines 4 }
	    $wordwidget configure -height $numlines
	} else {
	    set vp $v
	    if { [string trim $vp] eq "" } { set vp "<wordwidget/>" }
	}
    } else {
	set v $value
	if { [regexp {^<wordwidget>|<lognoter>} $v] } {
	    set vp $v
	} else {
	    set vp "<wordwidget>"
	    foreach line [split $v \n] {
		append vp "<para>[xml $line]</para>"
	    }
	    append vp "</wordwidget>"
	}
    }
    $wordwidget ClearText
    set err [catch { $wordwidget ApplyXMLdataAsText $vp } errstring]
    if { $err } {
	set vp "<wordwidget><para>[xml "error: $errstring"]</para></wordwidget>"
	$wordwidget ApplyXMLdataAsText $vp
    }
    $wordwidget reset_changes_counter
    #$wordwidget check_scroll 1
    $interp eval [list set ::$name $v]
}

proc formulae::_options_multiple_ops_calc_values { att_value att_values } {
 
    set vList [comma_field_to_list $att_value]
    set valuesList [comma_field_to_list $att_values]
    set vs ""
    foreach v $valuesList {
	if { $v ni $vList } {
	    lappend vs [list $v off "" open]
	} else {
	    lappend vs [list $v on "" open]
	}
    }
    foreach v $vList {
	if { $v ni $valuesList } {
	    lappend vs [list $v on "" open]
	}
    }
    return $vs
}

proc formulae::options_multiple_ops { key node w_cb what } {
    variable values
    variable uvalues

    set interp [dict get $values $key interp]
    set n [$node @n]
    
    set vs [$node @values ""]
    if { [set has_cmd [regexp {^\[(.*)\]$} $vs {} cmd]] } {
	if { [$node @global_interp 0] == 1 } {
	    #nothing
	} elseif { [$node @global_interp_v 0] == 1 } {
	    lappend cmd [dict get $values $key]
	} else {
	    set cmd [list formulae::eval_in_interp $key {*}$cmd]
	}
	set err [catch $cmd vs]
	if { $err } { set vs "" }
    }
    switch $what {
	"update_from_interp" {
	    set v [$interp eval [list set ::$n]]
	    $w_cb configure -values [_options_multiple_ops_calc_values $v $vs]
	    $w_cb update_label
	}
	"actualize_uvalues" {
	    set vList ""
	    foreach i [$w_cb cget -values] {
		if { [lindex $i 1] eq "on" } {
		    lappend vList [lindex $i 0]
		}
	    }
	    set v [list_to_comma_field $vList]
	    if { $v ne $uvalues($key,$n) } {
		set uvalues($key,$n) $v
	    }
	}
	"actualize_from_uvalues" {
	    $interp eval [list set ::$n $uvalues($key,$n)]
	    $w_cb configure -values [_options_multiple_ops_calc_values $uvalues($key,$n) $vs]
	    $w_cb update_label
	}
	"post" {
	    if { !$has_cmd } { return }
	    $w_cb configure -values [_options_multiple_ops_calc_values $uvalues($key,$n) $vs]
	    $w_cb update_label
	}
    }
}

proc formulae::report_maker_ops { key node w_rm what } {
    variable values
    variable uvalues
    
    set interp [dict get $values $key interp]
    set n [$node @n]

    switch $what {
	"update_from_interp" {
	    $w_rm set [$interp eval [list set ::$n]]
	}
	"actualize_uvalues" {
	    if { [$w_rm has_changes] } {
		set uvalues($key,$n) [$w_rm get]
	    }
	}
	"actualize_from_uvalues" {
	    $w_rm set $uvalues($key,$n)
	    $interp eval [list set ::$n $uvalues($key,$n)]
	}
    }
}

proc formulae::widget_table_ops { key node wtable what args } {
    variable values
    variable uvalues
    variable widget_table_disable_actualize
    variable widget_table_delete_actualize 
    
    set interp [dict get $values $key interp]
    set n [$node @n]
    
    switch $what {
	"update_from_interp" {
	    set v [$interp eval [list set ::$n]]
	    if { $v eq "" } {
		set cols [get_table_columns $node]
		set v [list [lrepeat [llength $cols] ""]]
	    }
	    set sel [$wtable selection get]
	    set active [$wtable index active]            
	    $wtable item delete all
	    foreach row $v {
		set item [$wtable insert end $row]
		if { $item in $sel } {
		    $wtable selection add $item
		    $wtable activate $item                   
		}
	    }      
	  
	    $interp eval [list set ::$n $v]
	    set widget_table_disable_actualize 1
	    set uvalues($key,$n) $v
	    unset widget_table_disable_actualize
	}
	actualize_uvalues {
	    if { [$wtable give_uservar_value changes 0] } {
		set v ""
		foreach item [$wtable item children 0] {
		    set colList ""
		    foreach col [$wtable item text $item] {
		        lappend colList [string trim $col]
		    }
		    lappend v $colList
		}
		set uvalues($key,$n) $v
	    }
	}
	actualize_from_uvalues {
	    if { [info exists widget_table_disable_actualize] } { return }
	    set v $uvalues($key,$n)
	    if { $v eq "" } {
		set cols [get_table_columns $node]
		set v [list [lrepeat [llength $cols] ""]]
	    }
	    set sel [$wtable selection get]
	    $wtable item delete all
	    set active [$wtable index active]
	    foreach row $v {
		set item [$wtable insert end $row]
		if {$item in $sel} {
		    $wtable selection add $item
		    $wtable activate $item                    
		}
	    }         
	    $interp eval [list set ::$n $v]
	    $wtable set_uservar_value changes 0
	}
	"recalculate_expressions" {
	    set v [$interp eval [list set ::$n]]
	    set column 0
	    foreach i [get_table_columns $node] {
		lassign $i name dict
		if { [dict_getd $dict expr ""] ne "" } {
		    $interp eval [list set column $column]
		    set row 0
		    foreach rowList $v {
		        $interp eval [list set row $row]
		        set err [catch { e $key [dict_getd $dict expr ""] } v_i]
		        if { $err } { set v_i "" }
		        lset v $row $column $v_i
		        $wtable item text [expr {$row+1}] $column $v_i
		        incr row
		    }
		}
		incr column
	    }
	    $interp eval [list set ::$n $v]
	    set widget_table_disable_actualize 1
	    set uvalues($key,$n) $v
	    unset widget_table_disable_actualize
	}
	"button1handler" {
	    lassign $args - - identify x y
	    set item [lindex $identify 1]
	    set column [lindex $identify 3]
	    set element [lindex $identify 5]
	    lassign [$wtable item bbox $item $column $element] x1 y1 x2 y2
	    set txt [string trim [$wtable item text $item $column]]
	    set isok 1
	    if { [string length $txt] } {
		set dx [font measure [$wtable cget -font] $txt]
		if { $x > $x1+$dx } {
		    set isok 0
		}
	    }
	    if { $isok } {
		$wtable edit_item $item $column
	    }
	}
	"return" {
	    set v [$interp eval [list set ::$n]]
	    set ncols [llength [$wtable cget -columns]]
	    if { $v eq "" } { set v [list [lrepeat $ncols ""]] }
	    
	    set is_void 1
	    foreach e [lindex $v end] {
		if { $e ne "" } {
		    set is_void 0
		    break
		}
	    }
	    if { !$is_void } {
		$wtable insert end ""
		lappend v [lrepeat $ncols ""]
	    }
	    $interp eval [list set ::$n $v]
	    set widget_table_disable_actualize 1
	    set uvalues($key,$n) $v
	    unset widget_table_disable_actualize
	    $wtable activate_select_item end
	    $wtable edit_item active
	    $wtable set_uservar_value changes 1
	}
	"edit_begin" {
	    lassign $args - item column
	    set cols [get_table_columns $node]
	    set i [lindex $cols $column]
	    lassign $i name dict
	    if { [dict_getd $dict expr ""] ne "" && [dict_getd $dict readonly ""] != 0 } {
		return 0
	    }
	    if { [dict_getd $dict maxlines ""] ne "" } {
		$wtable item element configure $item $column e_text_sel -lines \
		    [dict_getd $dict maxlines ""]
		return "text"
	    } else {
		return "entry"
	    }
	}
	"edit_accept" {
	    lassign $args - item col text
	    lassign [$wtable item rnc $item] row                        
	    set v [$interp eval [list set ::$n]]
	    set ncols [llength [$wtable cget -columns]]
	    if { $v eq "" } { set v [list [lrepeat $ncols ""]] }
	    lset v $row $col $text
	    $wtable item text $item $col $text
	    $interp eval [list set ::$n $v]
	    set widget_table_disable_actualize 1
	    set uvalues($key,$n) $v
	    unset widget_table_disable_actualize
	    widget_table_ops $key $node $wtable recalculate_expressions
	    $wtable set_uservar_value changes 1
	}
	"contextual" {
	    lassign $args - - X Y
	    entry_contextual_menu $key $wtable $n param $X $Y
	}
	"delete" {            
	    lassign $args - item
	    if {$item == ""} { return }
	    lassign [$wtable item rnc $item] row
	    set v [$interp eval [list set ::$n]]
	    set v [lreplace $v $row $row]
	    set ncols [llength [$wtable cget -columns]]
	    if { $v eq "" } { set v [list [lrepeat $ncols ""]] }
	    $interp eval [list set ::$n $v]
	    set widget_table_delete_actualize 1
	    set uvalues($key,$n) $v
	    widget_table_ops $key $node $wtable update_from_interp
	    $wtable set_uservar_value changes 1
	    unset widget_table_delete_actualize
	}
	"import" {
	    lassign $args - fin                   
	    set v [$interp eval [list set ::$n]]
	    set cols [get_table_columns $node]
	    if { $v == [list [lrepeat [llength $cols] ""]] } {
		set v ""
		$wtable item delete all 
	    }     
	    gets $fin headers      
	    while { [gets $fin line] != -1 } {              
		lappend v [csv::split $line ";"]                 
	    }
	    foreach row $v {
		set item [$wtable insert end $row]
	    }           
	    $interp eval [list set ::$n $v]
	    set widget_table_disable_actualize 1
	    set uvalues($key,$n) $v
	    unset widget_table_disable_actualize
	    widget_table_ops $key $node $wtable recalculate_expressions
	    $wtable set_uservar_value changes 1
	}
	"add" {
	    lassign $args - rowList                   
	    set v [$interp eval [list set ::$n]]
	    set cols [get_table_columns $node]
	    set void [list [lrepeat [llength $cols] ""]]
	    if { $v eq $void } {
		set v ""
	    }
	    lappend v $rowList                           
	    set uvalues($key,$n) $v
	    $wtable set_uservar_value changes 1
	}
	"edit" {
	    lassign $args - item rowList                   
	    set v [$interp eval [list set ::$n]]
	    lassign [$wtable item rnc $item] row column
	    set v [lreplace $v $row $row $rowList]
	    set uvalues($key,$n) $v
	    $wtable set_uservar_value changes 1
	}
	"get" {
	    lassign $args - item                   
	    set v [$interp eval [list set ::$n]]
	    lassign [$wtable item rnc $item] row column
	    return [lindex $v $row]
	}
    }
}

# calculate_and_update_formula
proc formulae::calculate_and_update_formula { key ntype name pname value units true false wimg } {
    variable values

    set interp [dict get $values $key interp]
    
    set errorMessage ""
    set value [string range $value 1 end-1]
    set valueE [string map [list \[ \\\[] $value]
    
    if { [lindex $valueE 0] ne "e" } {
	error "error in formulae::calculate_and_update_formula"
    }
    set err [catch { e $key [lindex $valueE 1] } v]
    if { $err } {
	set errorMessage $v
	set v !ERROR
    }
    
    if { $pname ne "" } { set txt "$pname" } else { set txt "" }   
    if { [string match "e *" $value] } {
	set df [dict_getd $values $key print_formulas yes]
	if { $df eq "detailed_edit"} {
	    if { [dict get $values $key state] eq "normal" } {
		set df detailed
	    } else {
		set df yes
	    }
	}
	if { $df eq "no" && [dict get $values $key state] eq "normal" } {
	    set df yes
	}
	set dosimple 1
	if { $ntype eq "condition" && [regexp {[<>]} $value] } {
	    set err [catch { llength $value } len]
	    if { !$err && [lindex $value 0] eq "e" && $len == 2 } {
		set f [lindex $value 1]
	    } else {
		set f [string range $value 2 end]
	    }
	    regexp {(.*)([<>]=?)(.*)} $f {} v1 sign v2
	    set v1_e [string map [list \[ \\\[] $v1]
	    set v2_e [string map [list \[ \\\[] $v2]
	    set err 0
	    if { !$err } { set err [catch { e $key $v1_e } v1_res] }
	    if { !$err } { set err [catch { e $key $v2_e } v2_res] }
	    if { !$err } { set err [catch { nicer_formula $key $v1 } v1_f] }
	    if { !$err } { set err [catch { nicer_formula $key $v2 } v2_f] }
	    if { !$err && [isdouble $v1_res] && [isdouble_or_void $v2_res]} {
		if { $txt ne "" } { append txt "=" }
		append txt "$v1_f=[nicer_number $v1_res] "
		append txt "$sign "
		append txt "$v2_f=[nicer_number $v2_res]"
		set dosimple 0
	    }
	}
	if { $dosimple } {
	    set err [catch { llength $value } len]
	    if { !$err && [lindex $value 0] eq "e" && $len == 2 } {
		set f [lindex $value 1]
	    } else {
		set f [string range $value 2 end]
	    }
	    set err [catch { nicer_formula $key $f } ret]
	    if { !$err } {
		if { $df ne "no" } {
		    if { $txt ne "" } { append txt "=" }
		    append txt $ret
		}
	    } else {
		set errorMessage $ret
		if { $txt ne "" } { append txt "=" }
		append txt [_ "!ERROR"]
	    }
	    if { !$err && $df eq "detailed" } {
		set err [catch { nicer_formula -subst_numbers $key $f } ret]
		if { !$err } {
		    append txt "=$ret"
		}
	    }
	}
    }
    if { $ntype eq "param" } {
	$interp eval [list set ::$name $v]
	append txt "=[nicer_number $v]"
	if { $units ne "" } {
	    append txt " [nicer_units $units]"
	}
    } else {
	if { $units ne "" } {
	    append txt " [nicer_units $units]"
	}
	if { $v eq "!ERROR" } {
	    append txt "     [_ "!ERROR"]"
	} elseif { $v } {
	    if { $true eq "" } { set true "[_ OK#C#Valid]" }
	    append txt "     $true (ok)"
	} else {
	    if { $false eq "" } { set false "[_ {Fail#C#No Valid}]" }
	    append txt "     $false (fail)"
	}
    }
    if { $wimg ne "" } {
	$wimg configure -formula $txt
	if { $errorMessage ne "" } {
	    tooltip $wimg $errorMessage
	}
    }
}

# proc formulae::_sql_fs { args } {
#     return "\"[join $args \",\"]\""
# }
# 
# proc formulae::_sql_fs_prefix { prefix args } {
#     set ret ""
#     foreach i $args { lappend ret "$prefix.\"$i\"" }
#     return [join $ret ","]
# }
# 
# proc formulae::_sql_vs { ldb args } {
#     set ret ""
#     foreach i $args { lappend ret "'[$ldb sql escape $i]'" }
#     return [join $ret ","]
# }
# 
# proc formulae::_sql_vfs { ldb args } {
#     set ret ""
#     foreach i $args {
#         set v [uplevel 1 [list set $i]]
#         lappend ret "'[$ldb sql escape $v]'"
#     }
#     return [join $ret ","]
# }

proc formulae::connect_to_mysql { args } {
    variable lb1_mysql
    
    set optional {
	{ -title title formula }
	{ -dbconnection "host user password userconfirm" ""}
    }
    set compulsory ""
    parse_args $optional $compulsory $args
    
    if { ![info exists lb1_mysql] } { set lb1_mysql "" }
    set lb [dict_getd $lb1_mysql $dbconnection ""]
    if { $lb ne "" && [$lb isconnected] } { return $lb}
    if { $lb ne "" } { destroy $lb }
    set lb [lognoter_db %AUTO% -dbtype mysql -title $title -dbconnection $dbconnection]
    if { $dbconnection eq "" } {
	$lb readpreferences ""
    }
    $lb connect_mysql
    if { ![$lb isconnected] } {
	destroy $lb
	return ""
    }
    if { $dbconnection eq "" } {
	$lb writepreferences -only_in_group 1
    }
    dict set lb1_mysql $dbconnection $lb
    return $lb
}

proc formulae::sql_mysql { args } {
    
    set optional {
	{ -ldb ldb "" }
    }
    set compulsory ""
    set args [parse_args -raise_compulsory_error 0 $optional $compulsory $args]

    if { $ldb eq "" } {
	set ldb [connect_to_mysql]
    }
    if { $ldb eq "" } {
	error "error not connected"
    }
    $ldb sql {*}$args
}

proc formulae::values_tree_from_table { args } {

    set optional {
	{ -fieldsList list "" }
	{ -groupField name "" }
	{ -remote "" 0 }
    }
    set compulsory "table field values"
    parse_args $optional $compulsory $args
    
    if { $remote } {
	set ldb [connect_to_mysql]
	if { $ldb eq "" } { return "" }
    } else {
	set ldb [dict get $values lognoter_db]
    }
    set fields ""
    if { $groupField ne "" } { lappend fields $groupField }
    lappend fields $field
    lappend fields {*}$fieldsList
    set cmd "select [$ldb sql fs $fields] from $table "
    if { $groupField ne "" } {
	append cmd "order by [$ldb sql fs0 $groupField $field] limit 500"
    } else {
	append cmd "order by [$ldb sql fs0 $field] limit 500"
    }
    set ret ""
    set last_groupField "-"
    if { $groupField ne "" } { set level 1 } else { set level 0 }
    $ldb sql maplist $cmd {
	if { $groupField ne "" && ![string equal -nocase [set $groupField] $last_groupField] } {
	    lappend ret [list 0 [set $groupField] [set $groupField] "" 0 "emphasis 1 expand 0"]
	    set last_groupField [set $groupField]
	}
	set name [set $field]
	foreach i $fieldsList { append name " [set $i]" }
	lappend ret [list $level $name [set $field] "" 1]
    }
    return $ret
}

# proc formulae::values_tree_from_cmd { args } {
# 
#     set optional {
#         { -remote "" 0 }
#     }
#     set compulsory "cmd values"
#     parse_args $optional $compulsory $args
#     
#     if { $remote } {
#         set ldb [connect_to_mysql]
#         if { $ldb eq "" } { return "" }
#     } else {
#         set ldb [dict get $values lognoter_db]
#     }
#     set interp [dict get $values interp]
#     $interp alias $ldb $ldb
#     return [$interp eval $cmd [list $ldb]]
# }

proc formulae::update_combox { w key node } {
    variable values
    
    set vs [$node @values]
    if { ![regexp {^\[(.*)\]$} $vs {} cmd] } { return }
    
    if { [$node @global_interp 0] == 1 } {
	#nothing
    } elseif { [$node @global_interp_v 0] == 1 } {
	lappend cmd [dict get $values $key]
    } else {
	set cmd [list formulae::eval_in_interp $key {*}$cmd]
    }
    set vs [eval $cmd]
    $w configure -values [comma_field_to_list $vs]
}

proc formulae::update_combox_tree { w key node } {
    variable values
	
    set values_tree [$node @values_tree]
    
    if { [regexp {^\[(.*)\]$} $values_tree {} cmd] } {
	if { [$node @global_interp 0] == 1 } {
	    #nothing
	} elseif { [$node @global_interp_v 0] == 1 } {
	    lappend cmd [dict get $values $key]
	} else {
	    set cmd [list formulae::eval_in_interp $key {*}$cmd]
	}
	set values_tree [eval $cmd]
    } elseif { [llength [$w tree_item children 0]] } {
	return
    }
    $w clear
    set last_item(-1) root
    foreach i $values_tree {
	lassign $i level name fname icon selectable dict
	if { $selectable eq "" } { set selectable 1 }
	if { $fname eq "" } { set fname $name }
	set parent $last_item([expr {$level-1}])
	set image $icon
	if { $image ni [list "" "-"] && [info command $image] eq "" } {
	    catch {
		set image [image create photo -data [formulae::eval_in_interp $key $image data \
		        -format png]]
	    }
	}
	if { $image eq "" } { set image - }
	set command [dict_getd $dict command ""]
	if { $command ne "" } {
	    set command [list formulae::eval_in_interp $key {*}$command]   
	}
	set item [$w tree_insert -image $image \
		-active $selectable -command $command end \
		$name $fname $parent]
	if { [dict_getd $dict emphasis 0] } {
	    $w tree_item state set $item emphasis
	}
	if { [dict_getd $dict expand 1] == 0 } {
	    $w tree_item collapse $item
	}
	set last_item($level) $item
    }
}

proc formulae::entry_validate { args } {
    variable values
    variable uvalues

    set optional {
	{ -show_error numContainer 0 }
	{ -window_actualize "" 0 }
    }
    set compulsory "entry name key isnumber"
    
    parse_args $optional $compulsory $args
    
    set interp [dict get $values $key interp]
    
    if { $isnumber } {
	set ret [isdouble_or_void $uvalues($key,$name)]
    } else {
	set ret 1
    }
    if { $ret } {
	$interp eval [list set ::$name $uvalues($key,$name)]
	catch { $entry configure -foreground "" }
    } else {
	catch { $entry configure -foreground red }
	if { $show_error } {
	    set nb [dict get $values $key notebook_widget]
	    $nb select [expr {$show_error-1}]
	    $entry selection range 0 end
	    focus $entry
	}
    }
    
    if { $window_actualize } {
	window_actualize -exec_callback 0 $key
    }
    return $ret
}

proc formulae::entry_invalid { entry } {
    $entry selection range 0 end
    #focus $entry
    bell
}

proc formulae::update_non_editable_field { e key node valuesList } {
    variable uvalues

    if { [string index [$node @values ""] 0] eq "\[" } {
	update_combox $e $key $node
	set valuesList [$e cget -values]
    }
    set n [$node @n]
    set v $uvalues($key,$n)
    if { [lsearch -exact $valuesList $v] == -1 } {
	set uvalues($key,$n) [lindex $valuesList 0]
    }
    window_actualize -exec_callback 0 $key
}

proc formulae::fieldtype_color_ops { op w e l b key node } {
    variable uvalues

    set color [string trim $uvalues($key,[$node @n])]
    switch $op {
	change {
	    set err [catch { $l configure -background $color }]
	    if { $err } {
		set color [tk_chooseColor -parent $w -title \
		        [_ "Choose Color"]]
	    } else {
		set color [tk_chooseColor -initialcolor $color -parent $w -title \
		        [_ "Choose Color"]]
	    }
	    if { $color eq "" } { return }
	    set uvalues($key,[$node @n]) $color
	}
	modvar {
	    catch { $l configure -background $color }
	}
	initial_color {
	    set uvalues($key,[$node @n]) [$node @value]
	}
	default_color {
	    set uvalues($key,[$node @n]) [$node @default_value [$node @value]]
	}
    }
}


proc formulae::manage_lastdir { key what args } {
    variable values
    
    set lognoter_db [dict_getd $values $key lognoter_db ""]

    switch $what {
	get {
	    if { $lognoter_db eq "" } { return [pwd] }
	    return [$lognoter_db getpreference lastdir]
	}
	set {
	    if { $lognoter_db eq "" } { return }
	    $lognoter_db addpreference lastdir [lindex $args 0]
	}
    }
}

proc formulae::fieldtype_file_ops { w wList what key node args } {
    variable values
    variable uvalues
    variable uvalues_moredata
    
    set lognoter [dict_getd $values $key lognoter ""]

    switch $what {
	open_browser {
	    set optional {
		{ -importdir name "" }
		{ -isdir boolean 0}
	    }
	    set compulsory ""
	    parse_args $optional $compulsory $args

	    set lognoter_db [dict get $values $key lognoter_db]

	    if { $importdir ne "" } {
		set importdir [file join [$lognoter_db give_appdir] $importdir]
	    } else {
		if { $lognoter_db ne "" } {
		    set importdir [$lognoter_db getpreference lastimportdir]
		} else {
		    set importdir [pwd]
		}
	    }
	    set exts ""
	    foreach "n v" [split [$node @extensions ""] ","] {
		lappend exts [list [_ $n] $v]
	    }
	    lappend exts [list [_ "All files"] [list "*"]]
	    if {!$isdir} {
		set file [tk_getOpenFile -filetypes $exts -initialdir \
		        $importdir \
		        -parent $w -title [_ "Load file"]]
		if { $file == "" } { return }
		$lognoter_db addpreference lastimportdir [file dirname $file]
		return [fieldtype_file_ops $w $wList open $key $node $file]
	    } else {
		set dir [formulae::selectdir -initialdir \
		        $importdir -parent $w]
		if { $dir == "" } { return }
		$lognoter_db addpreference lastimportdir $dir
		return [fieldtype_file_ops $w $wList open $key $node $dir]
	    }
	}
	open_files {
	    set file [lindex $args 0 0]
	    return [fieldtype_file_ops $w $wList open $key $node $file]
	}
	paste {
	    set entry [lindex $args 0]
	    set err [catch { cu::img::clipboard isimage } ret]
	    if { !$err && $ret } {
		set img [cu::img::clipboard get -type tclimage]
		set data [$img data -format png]
		image delete $img
		fieldtype_file_ops $w $wList open $key $node -data $data \
		    jpeg pastedimage.jpeg
		return -code break
	    }
	}
	ramviewer {
	    set c [dict_getd $values $key uvalues_moredata [$node @n] contents ""]
	    if { $c eq "" } {
		snit_messageBox -message [_ "There is no image to view"] -parent $w
		return
	    }
	    set err [catch {
		    set data [cu::inflate $c]]
		    set img [image create photo -data $data]
		} errstring]
	    if { $err } {
		snit_messageBox -message [_ "Could not open RamViewer (%s)" \
		        $errstring] -parent $w
		return
	    }
	    package require RamViewer
	    set w [RamViewer::Init "" $img 1]
	    focus -force [focus -lastfor $w]
	    image delete $img
	}
	open {
	    set file [lindex $args 0]
	    set path $file
	    set filename [file tail $file]
	    set can_delete 0
	    
	    set import_filter [$node @import_filter ""]
	    if { $import_filter ne "" } {
		lappend import_filter $w $file $lognoter
		set err [catch $import_filter ret]
		if { $err } {
		    if { $ret ne "" } {
		        snit_messageBox -message $ret -parent $w
		    }
		    return
		} else {
		    lassign $ret file path filename can_delete
		}
	    }
	    if { $file eq "-data" } {
		foreach "data type file" [lrange $args 1 3] break
		set now [date_tcl2sql [clock seconds]]
		foreach "cdate mdate" [list $now $now] break
		set size [string length $data]
		set contents [cu::deflate -level 9 $data]
		set uvalues($key,[$node @n]) $file
	    } elseif { ![file isdirectory $file] } {
		set fin [open $file r]
		fconfigure $fin -translation binary
		set contents [cu::deflate -level 9 [read $fin]]
		close $fin
		set size [file size $file]
		set uvalues($key,[$node @n]) $filename
		set type [string tolower [string range [file extension $file] 1 end]]
		file stat $file stat
		set stat_dict [array get stat]
		set cdate [date_tcl2sql [dict get $stat_dict ctime]]
		set mdate [date_tcl2sql [dict get $stat_dict mtime]]
	    } else {
		if { [$node @store_internally 1] == 1 } {
		    set zipfile [cu::file::tempfile formulae]
		    set pwd [pwd]
		    cd [file dirname $file]
		    file delete -force $zipfile
		    cu::zipfile -r -9 $zipfile [file tail $file]
		    cd $pwd
		    set fin [open $zipfile r]
		    fconfigure $fin -translation binary
		    set contents [read $fin]
		    close $fin
		    set size [file size $zipfile]
		} else {                   
		    lassign "" contents size
		}
		if { ![file isdirectory $file] } {
		    set uvalues($key,[$node @n]) $filename                   
		} else {
		    set uvalues($key,[$node @n]) $file                   
		}
		
		if { ![string equal -nocase [file extension $file] ".gid"] } {
		    set type directory
		} else {
		    set type gid
		}
		file stat $file stat
		set stat_dict [array get stat]
		set cdate [date_tcl2sql [dict get $stat_dict ctime]]
		set mdate [date_tcl2sql [dict get $stat_dict mtime]]
	    }

	    foreach i [list contents cdate mdate type size path] {
		dict set values $key uvalues_moredata [$node @n] $i [set $i]
	    }
	    if { $can_delete } {
		file delete $file
	    }
	    if { $path ne "-data" && [file exists $path] } {
		dict set values $key reload_file [$node @n] [file normalize $path]
		dict set values $key reload_mtime [$node @n] [file mtime $path]               
		lassign [dict get $values $key reload_menu_pos [$node @n]] menu idx
		if {$idx != ""} { $menu entryconfigure $idx -state normal }               
	    }           
	    fieldtype_file_ops $w $wList update_tooltip $key $node         
	}
	clear {
	    set uvalues($key,[$node @n]) ""
	    dict set values $key uvalues_moredata [$node @n] ""
	    fieldtype_file_ops $w $wList update_tooltip $key $node
	}
	update_tooltip {
	    set file [string trim $uvalues($key,[$node @n])]
	    if { $file ne "" } {
		if {![file isdirectory $file]} {                    
		    set txt [_ "File: %s\n" $file]
		} else {
		    set txt [_ "Directory: %s\n" $file]
		}
	    }
	    set size [dict_getd $values $key uvalues_moredata [$node @n] size ""]
	    if { $size ne "" } {
		set mega [expr {1024.0*1024.0}]
		if { $size < $mega } {
		    set sizep [format "%.4g Kb" [expr {$size/1024.0}]]
		} else {
		    set sizep [format "%.4g Mb" [expr {$size/$mega}]]
		}
		append txt [_ "Size: $sizep\n"]
	    }
	    append txt [$node @help ""]
	    foreach widget $wList {
		tooltip $widget $txt
	    }
	}
	execute - extract {
	    set lognoter_db [dict get $values $key lognoter_db]
	    set size [dict_getd $values $key uvalues_moredata [$node @n] size ""]
	    if { $size eq "" } {
		snit_messageBox -message [_ "There is no file to extract"] \
		    -parent $w
		return
	    }
	    set dir [lindex $args 0]
	    set file $uvalues($key,[$node @n])

	    if { $what eq "execute" } {
		set dir [cu::file::tempdir]
	    } elseif { $dir eq "" } {
		set path [dict_getd $values $key uvalues_moredata [$node @n] path ""]
		if { $path ne "" && [file isdirectory [file dirname $path]] } {
		    set lastdir [file dirname $path]
		} else {
		    set lastdir [$lognoter_db getpreference lastdir]
		}
		set fullfile [tk_getSaveFile -initialdir $lastdir \
		        -initialfile $file -title [_ "Save file"]]
		if { $fullfile eq "" } { return }
		set dir [file dirname $fullfile]
		set file [file tail $fullfile]
		if { ![file exists $dir] } { file mkdir $dir }
		$lognoter_db addpreference lastdir $dir
	    }
	    set fullfile [file join $dir $file]
	    set type [dict get $values $key uvalues_moredata [$node @n] type]

	    if { $type eq "gid" } {                                                
		set zipfile [cu::file::tempfile zip]
		set fout [open $zipfile w]
		fconfigure $fout -translation binary
		puts -nonewline $fout [dict get $values $key uvalues_moredata [$node @n] contents]
		close $fout
		set pwd [pwd]
		cd $dir
		if { $dir ne [cu::file::tempdir] && [file exists $fullfile] } {
		    set txt [_ "File '%s' exists. Overwrite?" $file]
		    set retval [snit_messageBox -default ok -icon question \
		            -message $txt -parent $w \
		            -type okcancel]
		    if { $retval == "cancel" } { return }
		}
		file delete -force $file
		cu::unzipfile -o $zipfile
		cd $pwd
		file delete -force $zipfile                
	    } else {
		if { [$node @store_internally 1] == 1 } { 
		    if { $dir ne [cu::file::tempdir] && [file exists $fullfile] } {
		        set txt [_ "File '%s' exists. Overwrite?" $file]
		        set retval [snit_messageBox -default ok -icon question \
		                -message $txt -parent $w \
		                -type okcancel]
		        if { $retval == "cancel" } { return }
		    }
		    file delete -force $fullfile
		    set fout [open $fullfile w]
		    fconfigure $fout -translation binary
		    set c [dict get $values $key uvalues_moredata [$node @n] contents]
		    puts -nonewline $fout [cu::inflate $c]
		    close $fout
		} 
	    }
	    set export_filter [$node @export_filter ""]
	    if { $export_filter ne "" } {
		lappend export_filter $w $fullfile
		set err [catch $export_filter ret]
		if { $err } {
		    if { $ret ne "" } {
		        snit_messageBox -message $ret -parent $w
		    }
		    return
		} elseif { $ret ne "" } {
		    snit_messageBox -message $ret -parent $w
		}
	    } elseif { $what eq "execute" } {
		# Ramviewer has been removed temporarily
#                 if { $type in [list gif jpg jpeg png avi mpg] } {
#                     fieldtype_file_ops $w $wList ramviewer $key $node 
#                     return
#                 } 
		if {$type ne "gid" } {
		    set etype start
		} else {
		    set etype gid
		}
		set err [catch { $lognoter_db executefile $etype $fullfile } errstring]
		if { $err } {
		    snit_messageBox -message [_ "error: %s" $errstring] -parent $w
		    return
		}
	    } else {
		snit_messageBox -message [_ "Saved file in '%s'" $fullfile] -parent $w
	    }
	    if { $what eq "extract" && [file exists $fullfile] } {
		dict set values $key reload_file [$node @n] [file normalize $fullfile]
		dict set values $key reload_mtime [$node @n] [file mtime $fullfile]
		lassign [dict get $values $key reload_menu_pos [$node @n]] menu idx
		$menu entryconfigure $idx -state normal
	    }
	}
	reload {
	    set file [dict get $values $key reload_file [$node @n]]
	    return [fieldtype_file_ops $w $wList open $key $node $file]
	}
    }
}

proc formulae::_are_fields_equal { f1 f2 } {
    if { $f1 == $f2 } { return 1 }
    if { $f2 eq "" } { lassign [list $f1 $f2] f2 f1 }
    if { $f1 eq "" } {
	set err [catch { llength $f2 }]
	if { !$err } {
	    set is_void 1
	    foreach i $f2 {
		if { $i ne "" } {
		    set err [catch { llength $i }]
		    if { $err } {
		        set is_void 0
		        break
		    } else {
		        foreach j $i {
		            if { [string trim $j] ne "" } {
		                set is_void 0
		                break
		            }
		        }
		    }
		}
	    }
	    if { $is_void } { return 1 }
	}
    }
    return 0
}

proc formulae::fill_form { key check_has_changes input_values input_values_more } {
    variable values
    variable uvalues
	
    set interp [dict get $values $key interp]

    if { !$check_has_changes } {
	dict set values $key disable_onchange 1
    }
    
    dict for "n v" $input_values_more {
	if { $check_has_changes } {
	    if { [dict_getd $values $key uvalues_moredata $n ""] ne $v } {
		return 1
	    }
	} else {
	    dict set values $key uvalues_moredata $n $v
	}
    }
    foreach i [array names uvalues] {
	regexp {(.*),(.*)} $i {} key_in name
	if { $key ne $key_in } { continue }
	if { [dict exists $input_values $name] } {
	    if { $check_has_changes } {
		if { ![_are_fields_equal [dict get $input_values $name] $uvalues($i)] } {
		    return 1
		}
	    } else {
		set uvalues($i) [dict get $input_values $name]
	    }
	}
	if { !$check_has_changes } {
	    $interp eval [list set ::$name $uvalues($i)]
	}
    }
    if { !$check_has_changes } {
	dict set values $key disable_onchange 0
	window_actualize $key
    }
    return 0
}

proc formulae::actualize_field { args } {
    variable values
    variable uvalues
    
    set optional {
	{ -value value "-" }
    }
    set compulsory "key field_name"
    parse_args $optional $compulsory $args
    
    set interp [dict get $values $key interp]

    if { $value ne "-" } {
	$interp eval [list set ::$field_name $value]
    }
    set doc [dict get $values $key doc]
    set xp [format_xpath {(//param[@n=%s and not(ancestor::setname)]} $field_name]
    append xp [format_xpath {|//container[@n=%s and not(ancestor::setname)]} $field_name]
    append xp [format_xpath {|//condition[@n=%s and not(ancestor::setname)])[1]} $field_name]
    set node [$doc selectNodes $xp]
    if { $node eq "" } {
	error [_ "Could not find field %s" $field_name]
    }
    switch [$node @field_type ""] {
	"table" {
	    foreach i [dict get $values $key update_stack] {
		if {[lrange $i 0 2] eq [list formulae::widget_table_ops $key $node] } {
		    eval $i
		    break
		}
	    }
	}
	default {
	    set uvalues($key,[$node @n]) [$interp eval [list set ::[$node @n]]]
	}
    }
}

proc formulae::add_to_contextual { key menu } {
    variable values
    
    set m ""
    set doc [dict get $values $key doc]
    set root [$doc documentElement]
    foreach node [$root selectNodes {container/param[@field_type='button' and not(@values_tree)]}] {
	if { [$node @only_in_edit_mode 0] && [dict get $values $key state] ne "normal" } {
	    continue
	}
	if { [$node @global_interp 0] == 1 } {
	    set cmd [$node @command ""]
	} elseif { [$node @global_interp_v 0] == 1 } {
	    set cmd [$node @command ""]
	    lappend cmd [dict get $values $key]
	} else {
	    set cmd [list formulae::eval_in_interp $key {*}[$node @command ""]]
	}
	set image [$node @img ""]
	if { $image ne "" && [info command $image] eq "" } {
	    set err [catch { cu::get_image_or_icon $image } ret]
	    if { !$err } {
		set image $ret
	    } else {
		set err [catch { image create photo -data [formulae::eval_in_interp $key $image data \
		                -format png] } ret]
		if { !$err } {
		    set image $ret
		}
	    }
	}
	if { [$node @pn ""] eq "" } { continue }
	lappend m [list [node_pn $node] $image $cmd [$node @location ""] [$node @pns ""]]
    }
    if { ![llength $m] } { return }
    
    if { [$menu type end] ni [list "" separator] } {
	$menu add separator
    }
    set submenu ""
    set idx 0
    foreach i $m {
	lassign $i txt img cmd location pns
	
	if { $location eq "same_menu" } {
	    if { $submenu eq "" } {
		lassign $last_i last_txt last_img last_cmd last_location last_pns
		$menu delete end
		if { $last_pns eq "" } { set last_pns $last_txt }
		$menu add cascade -label $last_pns -menu $menu.mm$idx
		set submenu [menu $menu.mm$idx -tearoff 0]
		incr idx
		$submenu add command -label $last_txt -image $last_img -command $last_cmd -compound left
	    }
	    $submenu add command -label $txt -image $img -command $cmd -compound left
	} else {        
	    $menu add command -label $txt -image $img -command $cmd -compound left
	    set submenu ""
	}
	set last_i $i
    }
    $menu add separator
}

proc formulae::window_actualize { args } {
    variable values
    variable uvalues

    set optional {
	{ -exec_callback boolean 1 }
	{ -conditions_window wp "" }
	{ -raise_error "" 0 }
    }
    set compulsory "key"
    parse_args $optional $compulsory $args
    
    if { [dict exists $values $key creating_window] } { return }

    set interp [dict get $values $key interp]

    foreach i [array names uvalues] {
	regexp {(.*),(.*)} $i {} key_in name
	if { $key ne $key_in } { continue }
	
	set cmd [dict_getd $values $key validate $name ""]
	if { $cmd ne "" } {
	    set ret [uplevel #0 $cmd]
	    if { $ret == 0 } {
		if { $raise_error } { error "incorrect parameter name '$name'" }
		return
	    }
	}
	$interp eval [list set ::$name $uvalues($i)]
    }

    if { $conditions_window ne "" } {
	set wp $conditions_window
	destroy $wp.conditions_window
	set w [toplevel $wp.conditions_window -bd 2 -relief raised -bg white]
	wm overrideredirect $w 1
	
	wm withdraw $w
	set cmdList [list [list destroy $w]]
	if { [focus] ne "" } {
	    lappend cmdList [list focus [focus]]
	}
	if { [grab current $wp] ne "" } {
	    lappend cmdList [list grab [grab current $wp]]
	}
	bind $w <1> "[join $cmdList ";"]; break"
	bind $w <KeyPress> "[join $cmdList ";"]; break"
	set idx 0
    }

    set conditions_window_results 0

    foreach i [dict get $values $key update_stack] {
	uplevel #0 $i
	lassign $i cmd key_in ntype name pname value units true false wimg
	if { $conditions_window ne "" && $cmd eq "formulae::calculate_and_update_formula" && 
	    $ntype eq "condition" } {
	    set wimg2 [wordwidgetformula $w.wf[incr idx] -font [$wimg cget -font] \
		    -normalize_spaces 0 -fontsize [$wimg cget -fontsize] \
		    -maxwidth [$wimg cget -maxwidth]]
	    grid $wimg2 -sticky w -padx 2 -pady 2
	    calculate_and_update_formula $key $ntype $name $pname $value $units $true \
		$false $wimg2
	    incr conditions_window_results
	}
    }
    if { $conditions_window ne "" } {
	if { !$conditions_window_results } {
	    destroy $w
	} else {
	    update idletasks
	    set x [expr {[winfo rootx $wp]+20}]
	    set y [expr {[winfo rooty $wp]+[winfo height $wp]+10}]
	    if { $x+[winfo reqwidth $w] > [winfo screenwidth $w] } {
		set x [expr {[winfo screenwidth $w]-[winfo reqwidth $w]}]
		if { $x < 0 } { set x 0 }
	    }
	    if { $y+[winfo reqheight $w] > [winfo screenheight $w] } {
		set y [expr {[winfo screenheight $w]-[winfo reqheight $w]}]
		if { $y < 0 } { set y 0 }
	    }
	    wm geometry $w +$x+$y
	    wm deiconify $w
	    grab $w
	    focus $w
	}
    }
    set valuesList ""
    foreach i [dict keys [dict get $values $key nicer_name]] {
	lappend valuesList $i [$interp eval [list set ::$i]]
    }
    foreach i [dict get $values $key svg_stack] {
	$i configure -parameters $valuesList
	$i draw
    }
    if { $exec_callback && [dict get $values $key update_callback] ne "" } {
	uplevel #0 [dict get $values $key update_callback] [list $valuesList]
    }
    return [list $valuesList [dict get $values $key uvalues_moredata]]
}

proc formulae::window_actualize_entry { entry name key isnumber } {
    variable uvalues

    if { $isnumber } {
	set ret [isdouble_or_void $uvalues($key,$name)]
    } else {
	set ret 1
    }
    if { !$ret } {
	bell
	$entry selection range 0 end
	focus $entry
    } else {
	window_actualize -conditions_window $entry $key
    }
}

################################################################################
#    reports
################################################################################

proc formulae::create_report_xml { xmldata } {
    set ndoc [create_report $xmldata]
    set _ ""
    set root [$ndoc documentElement]
    foreach node [$root childNodes] {
	append _ [$node asXML]
    }
    return $_
}

proc formulae::create_report_from_file { xmlfile } {

    return [create_report [tDOM::xmlReadFile $xmlfile]]
}

proc formulae::create_report { args } {
    set optional {
	{ -lognoter_db db "" }
    }
    set compulsory "key xmldata"
    parse_args $optional $compulsory $args
    
    dom parse $xmldata doc
    $doc documentElement root
    return [create_reportD -lognoter_db $lognoter_db $key $root]
}
proc formulae::create_reportD { args } {
    variable values
    
    set optional {
	{ -lognoter_db db "" }
	{ -page page "" }
    }
    set compulsory "key root"
    parse_args $optional $compulsory $args

    if { ![info exists values] } { set values "" }
    
    dict set values $key doc [$root ownerDocument]
    dict set values $key lognoter_db $lognoter_db
    
    if { ![dict exists $values $key interp] } {
	dict set values $key interp [_create_interp $key "" $lognoter_db "" [$root @database ""] $page]
	set interp_isnew 1
    } else {
	set interp_isnew 0
    }
    set page_tcl_code [dict_getd $values $key page_tcl_code ""]
    set err [catch {
	$interp eval $page_tcl_code
    }]
    if { [$root @user_defined_print ""] ne "" } {
	set err [catch { uplevel #0 [$root @user_defined_print] $key $lognoter_db [$root @database ""] } ret]
	if { $err } {
	    snit_messageBox -message $ret
	} else {
	    set xml "<report version='1.0'>"
	    append xml $ret
	    append xml "</report>"
	    set ndoc [dom parse $xml]
	    return $ndoc
	}
    }
    set ndoc [dom createDocument report]
    $ndoc documentElement nroot
    $nroot setAttribute version 1.0

    set printedLC_title 0
    foreach node [$root selectNodes title|description|container] {
	switch [$node nodeName] {
	    title {
		set em [$nroot appendChildTag para emphasis]
		$em setAttribute role header
		$em appendChildText [$node text]
	    }
	    description {
		set para [$nroot appendChildTag para]
		$para appendChildText [$node text]

	    }
	    container {
#                 set xp {../setname/@n = ../setname/name[@print='0']/@n}
#                 if { [$node selectNodes $xp] } { continue }
# 
#                 if { !$printedLC_title } {
#                     set setNode [$root selectNodes setname]
#                     if { $setNode ne "" && [$setNode @n ""] ne "" } {
#                         set em [$nroot appendChildTag emphasis]
#                         $em setAttribute role header
#                         $em appendChildText [_ "Set %s" [$setNode @n]]
#                         $nroot appendChildTag para
#                     }
#                     set printedLC_title 1
#                 }
#                 if { [$node @condition ""] ne "" } {
#                     set ret [eval [string range [$node @condition] 1 end-1]]
#                     if { !$ret } { continue }
#                 }
#                 set ns { svg http://www.w3.org/2000/svg }
#                 set svgNode [$node selectNodes -namespaces $ns svg:svg]
#                 if { $svgNode ne "" } {
#                     set m [$nroot appendChildTag mediaobject imageobject]
#                     $m appendXML [$svgNode asXML]
#                     set svgNode [$m selectNodes -namespaces $ns svg:svg]
#                     $svgNode setAttribute width 50% height 50%
#                 }
#                 set em [$nroot appendChildTag emphasis]
#                 $em setAttribute role header
#                 $em appendChildText [$node @pn [$node @n]]
#                 set tab [$nroot appendChildTag table]
#                 $tab setAttribute \
#                     relativesizes "0 65 35" tabstyle "" \
#                     cols_justify "left left"
#                 process_container $key $tab $node
	    }
	}
    }
    
    set table [$root @database ""]
    
    lassign [list none none] print_type print_table_type
    if { [$root @print_first 1] } { set print_type first }
    switch [$root @print_all form] {
	form {
	    set print_type all
	}
	table {
	    set print_table_type all
	}
    }    
    if { $print_type ne "none" } {
	report_loadcases $key $root $nroot $table $print_type
    }
    if { $print_table_type ne "none" && $table ne "" } {
	formulae::report_loadcases_table $key $root $nroot $table $print_table_type
    }
    if { $interp_isnew } {
	interp delete [dict get $values $key interp]
	dict set values $key ""
    }
    return $ndoc
}

proc formulae::check_state_condition { key node } {

    set condition [$node @condition ""]
    if { $condition ne "" } {
	if { [string index $condition 0] == "\[" } {
	    set value [string range $condition 1 end-1]
	    
	    set valueE [string map [list \[ \\\[] $value]
	    
	    if { [lindex $valueE 0] ne "e" } {
		error "error in formulae::check_state_condition"
	    }
	    set err [catch { e $key [lindex $valueE 1] } condition]
	    if { $err } { set  condition 1 }
	} elseif { [string is boolean -strict $condition] } {
	    # nothing
	} else {
	    error [_ "error in field '%s'. Condition not ok" [$node @n]]
	}
    } else {
	set condition 1
    }
    return $condition
}

proc formulae::process_container { key nnode containerNode } {
    variable values
    
    set interp [dict get $values $key interp]
    
    foreach node [$containerNode selectNodes param|condition] {
	lassign "" txt txt_xml
	if { [$node nodeName] eq "param" } {
	    set txt "[node_pn $node]"
	    set txt_xml "[node_pn_xml $node]"
	}
	if { [string index [$node @value] 0] == "\[" } {
	    set is_expression 1
	    set value [string range [$node @value] 1 end-1]
	    set valueE [string map [list \[ \\\[] $value]
	    
	    if { [lsearch [list "formatted expression" "text expression"] \
		[$node @field_type ""]] != -1 } {
		set err [catch { $interp eval $valueE } v]
	    } else {
		if { [lindex $valueE 0] ne "e" } {
		    error "error in formulae::process_container"
		}
		set err [catch { e $key [lindex $valueE 1] } v]
	    }
	    if { $err } {
		set v "!ERROR ($v)"
	    }
	    if { [string match "e *" $value] } {
		set df [dict_getd $values $key print_formulas yes]
		if { $df eq "detailed_edit" } {
		    set df yes
		}
		set dosimple 1
		if { [$node nodeName] eq "condition" && [regexp {[<>]} $value] } {
		    set err [catch { llength $value } len]
		    if { !$err && [lindex $value 0] eq "e" && $len == 2 } {
		        set f [lindex $value 1]
		    } else {
		        set f [string range $value 2 end]
		    }
		    regexp {(.*)([<>]=?)(.*)} $f {} v1 sign v2
		    set v1_e [string map [list \[ \\\[] $v1]
		    set v2_e [string map [list \[ \\\[] $v2]
		    set err 0
		    if { !$err } { set err [catch { e $key $v1_e } v1_res] }
		    if { !$err } { set err [catch { e $key $v2_e } v2_res] }
		    if { !$err } { set err [catch { nicer_formula $key $v1 } v1_f] }
		    if { !$err } { set err [catch { nicer_formula $key $v2 } v2_f] }
		    if { !$err && [isdouble $v1_res] && [isdouble_or_void $v2_res]} {
		        if { $txt ne "" } {
		            append txt "="
		            append txt_xml "="
		        }
		        append txt "$v1_f=[nicer_number $v1_res] "
		        append txt "$sign "
		        append txt "$v2_f=[nicer_number $v2_res]"
		        
		        append txt_xml "$v1_f=[nicer_number $v1_res] "
		        append txt_xml "[xml_map1 $sign] "
		        append txt_xml "$v2_f=[nicer_number $v2_res]"

		        set dosimple 0
		    }
		}
		if { $dosimple } {
		    set err [catch { llength $value } len]
		    if { !$err && [lindex $value 0] eq "e" && $len == 2 } {
		        set f [lindex $value 1]
		    } else {
		        set f [string range $value 2 end]
		    }
		    set err [catch { nicer_formula $key $f } ret]
		    if { !$err } {
		        if { $df ne "no" } {
		            if { $txt ne "" } {
		                append txt "="
		                append txt_xml "="
		            }
		            append txt $ret
		            append txt_xml $ret
		        }
		    } else {
		        set errorMessage $ret
		        if { $txt ne "" } {
		            append txt "="
		            append txt_xml "="
		        }
		        append txt [_ "!ERROR"]
		        append txt_xml [_ "!ERROR"]
		    }
		    if { !$err && $df eq "detailed" } {
		        set err [catch { nicer_formula -subst_numbers $key $f } ret]
		        if { !$err } {
		            if { [string index $txt end] ne "=" } {
		                append txt "="
		                append txt_xml "="
		            }
		            append txt "$ret"
		            append txt_xml "$ret"
		        }
		    }
		}
	    }
	} else {
	    set is_expression 0
	    set err [catch { expr {[$node @value]*1.0} } v]
	    if { $err } { set v [$node @value] }
	}
	if { [$node nodeName] eq "param" } {
	    $interp eval [list set ::[$node @n] $v]
	    dict set values $key nicer_name [$node @n] [node_pn_xml $node]
	    
	    if { [string index $txt end] ne "=" } {
		append txt "="
		append txt_xml "="
	    }
	    append txt "[nicer_number $v]"
	    append txt_xml "[nicer_number $v]"
	    if { [$node hasAttribute units] } {
		append txt " [nicer_units [$node @units]]"
		append txt_xml " [nicer_units [$node @units]]"
	    }
	} else {
	    if { [$node hasAttribute units] } {
		append txt " [nicer_units [$node @units]]"
		append txt_xml " [nicer_units [$node @units]]"
	    }
	    set txt_in ""
	    if { ![string is double -strict $v] } {
		# nothing
	    } elseif { $v } {
		if { [$node @true ""] ne "" } {
		    append txt_in "     [$node @true] (ok)"
		} else {
		    append txt_in "     [_ OK#C#Valid] (ok)"
		}
	    } else {
		if { [$node @false ""] ne "" } {
		    append txt_in "     [$node @false] (fail)"
		} else {
		    append txt_in "     [_ {Fail#C#No Valid}] (fail)"
		}
	    }
	    append txt $txt_in
	    append txt_xml [xml_map1 $txt_in]
	}
	if { ![check_state_condition $key $node] } { continue }
   
	set row [$nnode appendChildTag row]
	if { [regexp {^(\w+)=dictionary\((.*)\)=(.*)$} $txt {} v_i args_i res_i] } {
	    set args_i_list [split $args_i ","]
	    if { [llength $args_i_list] >= 3 } {
		set xml "<table tabstyle='plainlines'>"
		append xml "<row><entry>[xml [lindex $args_i_list 0]]</entry>"
		foreach "n v" [lrange $args_i_list 1 end] {
		    set n [string trim $n \"]
		    append xml "<entry>[xml $n]</entry>"
		}
		append xml "</row><row><entry>[xml $v_i]</entry>"
		foreach "n v" [lrange $args_i_list 1 end] {
		    set v [string trim $v \"]
		    append xml "<entry>[xml $v]</entry>"
		}
		append xml "</row></table>"
		set entry [$row appendChildTag entry]
		$entry setAttribute  morerows 1
		$entry appendXML $xml
		set row [$nnode appendChildTag row]
		set txt "${v_i}([lindex $args_i_list 0])=$res_i"
		set txt_xml "${v_i}([lindex $args_i_list 0])=$res_i"
	    }
	}
	set entry [$row appendChildTag entry]
	set em [$entry appendChildTag emphasis]
	if { [string match "formatted*" [$node @field_type ""]] } {
	    $entry setAttribute cols_justify justify
	    if { [node_pn_xml $node] ne "-" } {
		$em appendXML "
		    <lognoter><para><emphasis role='strong'>[node_pn_xml $node]
		    </emphasis></para></lognoter>
		"
	    }
	    if { [string trim $v] eq "" } { set v "<lognoter/>" }
	    set err [catch { $em appendXML $v } errstring]
	    if { $err } {
		$em appendChildText $v
		#error [_ "error: field '%s' not correct value='%s'" [$node @n] $v]
	    }
	    foreach n0 [$em childNodes] {
		foreach n [$n0 childNodes] {
		    $em appendChild $n
		}
		$n0 delete
	    }
	} else {
	    $em setAttribute role formula
	    set done 0
	    if { [regexp {<.*>} $txt_xml] } {
		set err [catch { dom parse <n>$txt_xml</n> } doc1]
		if { !$err } {
		    set root1 [$doc1 documentElement]
		    foreach n [$root1 selectNodes {//emphasis[@role='formula']}] {
		        foreach ni [$n childNodes] {
		            [$n parentNode] insertBefore $ni $n
		        }
		        $n delete
		    }
		    foreach n [$root1 childNodes] {
		        $em appendChild $n
		    }
		    set done 1
		    $doc1 delete
		}
	    }
	    if { !$done } {
		$em appendChildText $txt
	    }
	}
	if { [$node @help ""] ne "" } {
	    set entry [$row appendChildTag entry]
	    $entry appendChildText [$node @help ""]
	} else {
	    $entry setAttribute morerows 1
	}
    }
}

proc formulae::report_loadcases_table { key root nroot table print_table_type } {
    variable values
    
    set lognoter_db [dict get $values $key lognoter_db]
    set interp [dict get $values $key interp]
    
    if { $table ne "" } {
	set cols [$lognoter_db sql column_names $table]
    } else {
	set cols ""
	set print_type normal
    }
    set cmd "select * from $table"
    set rows [$lognoter_db sql sel $cmd]
    if { [llength $rows] == 0 } { return }

    set xml "<table cols_justify='center'><row>"
    foreach node [$root selectNodes //param|//condition] {
	set pn [node_pn_xml $node]
	if { [$node @units ""] ne "" } {
	    append pn " ([$node @units])"
	}
	set err [catch { dom parse <emphasis>$pn</emphasis> }]
	if { !$err } {
	    #append xml "<entry><emphasis role='formula'>$pn"
	    append xml "<entry>$pn"
	} else {
	    append xml "<entry><emphasis role='formula'>[xml $pn]"
	    append xml "<entry>[xml $pn]"
	}
	#append xml "</emphasis>"
	append xml "</entry>"
    }
    append xml </row>
	
    set delta_ipos [expr {[llength $cols]-1}]
    for { set ipos 0 } { $ipos < [llength $rows] } { incr ipos [llength $cols] } {
	set valuesList [lrange $rows $ipos $ipos+$delta_ipos]

	if { [lindex $cols 3] eq "id_name" } {
	    set name [lindex $valuesList 3]
	    set idx0 4
	} else {
	    set name [lindex $valuesList 0]
	    set idx0 3
	}
	for { set idx $idx0 } { $idx < [llength $cols] } { incr idx } {
	    set n [lindex $cols $idx]
	    set v [lindex $valuesList $idx]
	    
	    set xp [format_xpath {(//param[@n=%s])[1]} $n]
	    set paramNode [$root selectNodes $xp]
	    if { $paramNode eq "" } { continue }
	    $paramNode setAttribute value $v
	}
	append xml "<row>"
	foreach node [$root selectNodes //param|//condition] {
	    if { [string index [$node @value] 0] == "\[" } {
		set value [string range [$node @value] 1 end-1]
		set valueE [string map [list \[ \\\[] $value]
		
		if { [lsearch [list "formatted expression" "text expression"] \
		    [$node @field_type ""]] != -1 } {
		    set err [catch { $interp eval $valueE } v]
		} else {
		    if { [lindex $valueE 0] ne "e" } {
		        error "error in formulae::report_loadcases_table"
		    }
		    set err [catch { e $key [lindex $valueE 1] } v]
		}
		if { $err } {
		    #set v "!ERROR ($v)"
		    set v "!ERROR"
		}
	    } else {
		set err [catch { expr {[$node @value]*1.0} } v]
		if { $err } { set v [$node @value] }
	    }
	    append xml "<entry>"
	    if { [$node nodeName] eq "param" } {
		$interp eval [list set ::[$node @n] $v]
		if { $v ne "!ERROR" } {
		    set err [catch { format %.4g $v } num]
		    if { $err } { set num $v }
		} else {
		    set num "!ERROR"
		}
		if { [regexp {(.*)e([+-])(\d+)} $num {} p1 s exp] } {
		    if { $s eq "+" } { set s "" }
		    if { $exp != 0 } { set exp [string trimleft $exp 0] }
		    set num "$p1·10<sup>$s$exp</sup>"
		}
		if { [check_state_condition $key $node] } {
		    append xml "$num"
		}
	    } else {
		if { [check_state_condition $key $node] } {
		    append xml "<emphasis role='formula'>"
		    if { ![string is double -strict $v] } {
		        # nothing
		    } elseif { [string is double -strict $v] && $v } {
		        if { [$node @true ""] ne "" } {
		            append xml [xml " (ok)"]
		        } else {
		            append xml [xml " (ok)"]
		        }
		    } else {
		        if { [$node @false ""] ne "" } {
		            append xml [xml " (fail)"]
		        } else {
		            append xml [xml " (fail)"]
		        }
		    }
		    append xml "</emphasis>"
		}
	    }
	    append xml "</entry>"
	}
	append xml "</row>"
    }
    append xml "</table>"
    
    $nroot appendXML $xml
    
    set tableNode [$nroot selectNodes {child::*[last()]}]
    set numEntries [$tableNode selectNodes {count(row[1]/entry)}]
    
    for { set icol 1 } { $icol <= $numEntries } { incr icol } {
	set isvoid 1
	foreach eNode [$tableNode selectNodes [format {row[position()>1]/entry[%s]} $icol]] {
	    if { [$eNode selectNodes {string(.)}] ne "" } {
		set isvoid 0
		break
	    }
	}
	if { $isvoid } {
	    foreach eNode [$tableNode selectNodes [format {row/entry[%s]} $icol]] {
		$eNode delete
	    }
	    incr numEntries -1
	    incr icol -1
	}
    }
    set num_max 6
    while { $numEntries > $num_max } {
	set newtableNode [$nroot appendChildTag table [list attributes() cols_justify center]]
	foreach rowNode [$tableNode selectNodes row] {
	    set newrowNode [$newtableNode appendChildTag row]
	    set xp [format {entry[position()>%d]} $num_max]
	    foreach entryNode [$rowNode selectNodes $xp] {
		$newrowNode appendChild $entryNode
	    }
	}
	set tableNode $newtableNode
	set numEntries [$tableNode selectNodes {count(row[1]/entry)}]
    }
}

proc formulae::report_loadcases { key root nroot table print_type } {
    variable values
    
    set lognoter_db [dict get $values $key lognoter_db]

    if { $table ne "" } {
	set cols [$lognoter_db sql column_names $table]
    } else {
	set cols ""
	set print_type normal
    }
    switch $print_type {
	all {
	    set cmd "select * from $table"
	}
	first {
	    set id_min [$lognoter_db sql onecolumn "select min(id) from \"$table\""]
	    if { $id_min eq "" } {
		set cmd "select * from $table"
	    } else {
		set cmd "select * from \"$table\" where id=$id_min"
	    }
	}
	normal {
	    set cmd "select \"\""
	}
    }
    set rows [$lognoter_db sql sel $cmd]
    if { ![llength $rows] } {
	set rows [list ""]
    }
    set delta_ipos [expr {[llength $cols]-1}]
    for { set ipos 0 } { $ipos < [llength $rows] } { incr ipos [llength $cols] } {
	set valuesList [lrange $rows $ipos $ipos+$delta_ipos]
	if { [lindex $cols 3] eq "id_name" } {
	    set name [lindex $valuesList 3]
	    set idx0 4
	} else {
	    set name [lindex $valuesList 0]
	    set idx0 3
	}
	if { [llength $valuesList] > 1 } {
	    for { set idx $idx0 } { $idx < [llength $cols] } { incr idx } {
		set n [lindex $cols $idx]
		set v [lindex $valuesList $idx]
		
		set xp [format_xpath {(//param[@n=%s])[1]} $n]
		set paramNode [$root selectNodes $xp]
		if { $paramNode eq "" } { continue }
		
		set field_type [$paramNode @field_type ""]
		if { $field_type eq "" && [regexp {\[e\s+(.*)\]} [$paramNode @value]] } {
		    set field_type expression
		}
		if { $field_type ni [list expression "text expression" "formatted expression"] } {
		    $paramNode setAttribute value $v
		}
	    }
	}
	if { $name ne "" } {
	    set em [$nroot appendChildTag para emphasis]
	    $em setAttribute role header
	    $em appendChildText [_ "Set %s" $name]
	}
	foreach node [$root selectNodes container] {
	    switch [$node nodeName] {
		container {
		    if { [$node @state normal] eq "hidden" } { continue }
		    set cnd [check_container_condition [$node @n] [$node @condition ""] \
		            "" "" $key]
		    if { !$cnd } { continue }
		    
		    set ns { svg http://www.w3.org/2000/svg }
		    set svgNode [$node selectNodes -namespaces $ns svg:svg]
		    if { $svgNode ne "" } {
		        set m [$nroot appendChildTag mediaobject imageobject]
		        $m appendXML [$svgNode asXML]
		        set svgNode [$m selectNodes -namespaces $ns svg:svg]
		        $svgNode setAttribute width 50% height 50%
		    }
		    set em [$nroot appendChildTag para emphasis]
		    $em setAttribute role header
		    $em appendChildText [node_pn_xml $node]
		    
		    if { [$node @help ""] ne "" } {
		        set para [$nroot appendChildTag para]
		        $para appendChildText [$node @help]
		    }
		    set tag [$nroot appendChildTag table]
		    $tag setAttribute \
		        relativesizes "0 65 35" tabstyle "" \
		        cols_justify "left left"
		    process_container $key $tag $node
		}
	    }
	}
	if { ![llength $cols] } { break }
    }
}


proc formulae::print_report { ndoc file format } {

    package require xml2pdf

    switch $format {
	pdf - print {
	    PDFWriter::choosetype $format
	    
	    set papersize A4
	    set boxList {
		{mainbox {225 285 1505 2260} {11.5% 10.4% 89.3% 92.9%} 1}
		{colorbox all {10.4% 9.47% 90.5% 93.35%} {} #CEC2C2 3}
		{colorbox all {10.2% 9.62% 90.3% 93.5%} {} #3A967E 3}
		{text all {11% 7.3% 65% 8.5%} Helvetica 12 black white sw {<wordwidget><para><emphasis role="strong">$sectiontitle</emphasis></para></wordwidget>} 1}
		{text all {10% 94.1% 25% 95.8%} Helvetica 10 #000000 white nw {<wordwidget><para>Página $pagenum</para></wordwidget>} 1}
		{text all {66.6% 94.1% 83.6% 96%} Helvetica 6 #000000 white n {<wordwidget><para>Developed by Compass, Ing. y Sist.</para><para>http://www.compassis.com</para></wordwidget>} 0}
	    }
	    set opts {
		TitleBackgroundColor #dceae6
		sizedefault 10
	    }
	    if { $file == "" } {
		set file [xml2pdf::PrintXML [$ndoc asXML -indent none] $papersize \
		        $boxList $opts]
	    } else {
		lappend opts filename [file root $file].pdf
		xml2pdf::PrintXML [$ndoc asXML -indent none] $papersize $boxList $opts
	    }
	}
	html {
	    set opts [list filename $file multiplesfiles $multiplesfiles]
	    #lappend opts outputrtf 1
	    xml2html:::PrintHTML [$ndoc asXML -indent none] $opts
	}
	rtf {
	    set opts [list filename $file]
	    lappend opts outputrtf 1
	    xml2html:::PrintHTML [$ndoc asXML -indent none] $opts
	}
	xml {
	    if { $file eq "" } {
		foreach "file fout" [xml2html::GiveTempFile htmfile .html] break
	    } else {
		set fout [open $file w]
	    }
	    fconfigure $fout -encoding utf-8
	    puts $fout "<?xml version=\"1.0\" encoding=\"utf-8\"?><!-- -*- coding: utf-8;-*- -->"
	    set root [$ndoc documentElement]
	    puts $fout "<!DOCTYPE [$root nodeName]>"
	    $ndoc asXML -indent none -channel $fout
	    close $fout
	}
    }
}

################################################################################
#    calc_and_update_tree
################################################################################

proc formulae::_add_row_to_database { args } {

    set optional {
    }
    set compulsory "db table values values_moredata root"
    parse_args $optional $compulsory $args

    set cols [$db sql column_names $table]
    set cols_r [lrange $cols 3 end]
    set valuesList [lrepeat [expr {[llength $cols]-3}] null]
    
    foreach "n v" $values {
	set ipos [lsearch -exact $cols_r $n]
	if { $ipos == -1 } { continue }
	set v [string trim $v]
	
	set xp [format_xpath {string(//param[@n=%s]/@field_type)} $n]
	set field_type [$root selectNodes $xp]
	if { $field_type eq "file" || $field_type eq "directory" } {
	    set err [catch { dict get $values_moredata $n contents } c]
	    set xp_store_int [format_xpath {string(//param[@n=%s]/@store_internally)} $n]   
	    set store_internally [$root selectNodes $xp_store_int]           
	    if { $store_internally == 1 || $store_internally == "" } {
		if { $err || $c eq "" } {
		    set v ""
		} elseif { $v eq "" } {
		    dict set values_moredata $n ""
		}
	    }
	} 
	if { $field_type in [list "" numeric expression] &&
	    [string is double -strict $v] && $v > 1e-100 && $v < 1e100 } {
	    lset valuesList $ipos [$db sql escape $v]
	} else {
	    lset valuesList $ipos '[$db sql escape $v]'
	}
    }
    foreach "n v" $values_moredata {
	dict for "n2 v2" $v {
	    set nfull "${n}__$n2"
	    set ipos [lsearch -exact $cols_r $nfull]
	    if { $ipos == -1 } { continue }
	    if { $v2 ne "null" } {
		lset valuesList $ipos '[$db sql escape $v2]'
	    } 
	}
    }


    $db sql check_state 

    set cmd "insert into \"$table\" (\"[join $cols \",\"]\") values(
	null,now(),now(),"
    append cmd [join $valuesList ,] ")"
    $db sql exec $cmd
}

proc formulae::_edit_database_row { args } {

    set optional {
    }
    set compulsory "db table values values_moredata primary_key root"
    parse_args $optional $compulsory $args

    set cols [$db sql column_names $table]
    set cols_r [lrange $cols 3 end]
    set valuesList [lrepeat [expr {[llength $cols]-3}] null]   
    
    foreach "n v" $values {
	set ipos [lsearch -exact $cols_r $n]
	if { $ipos == -1 } { continue }
	set v [string trim $v]
	
	set xp [format_xpath {string(//param[@n=%s]/@field_type)} $n]
	set field_type [$root selectNodes $xp]
	if { $field_type eq "file" || $field_type eq "directory" } {
	    if { $v eq "clear" } {
		lset valuesList $ipos [$db sql escape "clear"]
		continue
	    }
	    set err [catch { dict get $values_moredata $n contents } c]
	    set xp_store_int [format_xpath {string(//param[@n=%s]/@store_internally)} $n]                                             
	    set store_internally [$root selectNodes $xp_store_int]           
	    if { $store_internally == 1 || $store_internally == "" } {                
		if { $err || $c eq "" } {
		    dict set values_moredata $n ""
		    continue
		} elseif { $v eq "" } {
		    dict set values_moredata $n ""
		    continue
		}
	    }
	}        
	
	if { $field_type in [list "" numeric expression] &&
	    [string is double -strict $v] && $v > 1e-100 && $v < 1e100 } {
	    lset valuesList $ipos [$db sql escape $v]
	} else {
	    lset valuesList $ipos '[$db sql escape $v]'
	}
    }
    foreach "n v" $values_moredata {
	set ipos [lsearch -exact $cols_r $n]
	if { $ipos == -1 || [lindex $valuesList $ipos] eq "null" } { continue }

	dict for "n2 v2" $v {
	    set nfull "${n}__$n2"
	    set ipos [lsearch -exact $cols_r $nfull]
	    if { $ipos == -1 } { continue }
	    lset valuesList $ipos '[$db sql escape $v2]'
	}
    }

    $db sql check_state
    
    set cmd "update [$db sql fs0 $table] set [lindex $cols 2]=now()"
    foreach n $cols_r v $valuesList {
	if { $v ne "null" && $v ne "clear" } {
	    append cmd ",[$db sql fs0 $n]=$v"
	} elseif { $v eq "clear" } {
	    append cmd ",[$db sql fs0 $n]=''"
	}
    }
    set ipos [lsearch -exact $cols_r $primary_key]
    append cmd " where [$db sql fs0 $primary_key]=[lindex $valuesList $ipos]"
    $db sql exec $cmd
}

proc formulae::date_timestampsql2tcl { date } {
    if { [regexp {([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})} $date\
	  {} y m d h min s] } {
	return [clock scan "$y-$m-$d $h:$min:$s"]
    } else {
	return [clock scan $date]
    }
}

proc formulae::give_file_from_form { args } {
    variable values
    set optional {
	{ -lognoter_db db "" }
    }
    set compulsory "table edit_choose_name edit_choose_value file_name"
    parse_args $optional $compulsory $args
    
    set valuesList [$lognoter_db sql sel "select * from [$lognoter_db sql fs0 $table] where \
	    [$lognoter_db sql fs0 $edit_choose_name]=[$lognoter_db sql vs0 $edit_choose_value]"]
    
    set pos_contents [lsearch -exact $valuesList $file_name]
    if { $pos_contents != -1 } {
	set contents [cu::inflate [lindex $valuesList $pos_contents+1]]
	set cdate [date_timestampsql2tcl [lindex $valuesList $pos_contents+2]]
	set mdate [date_timestampsql2tcl [lindex $valuesList $pos_contents+3]]
	set extension [lindex $valuesList $pos_contents+4]
	set size [lindex $valuesList $pos_contents+5]
    } else {
	set contents ""
	set cdate ""
	set mdate ""
	set extension ""
	set size "" 
    }

    set map [list txt text-plain doc msword bin octet-stream exe octet-stream so octet-stream \
	    dll octet-stream pdf pdf xls vnd.ms-excel ppt vnd.ms-powerpoint gtar x-gtar \
	    hdf x-hdf js x-javascript tgz x-tar tar x-tar tcl x-tcl zip zip xhtml xhtml+xml \
	    xht xhtml+xml xml xml xsl xml dtd xml-dtd xslt xslt+xml au basic snd basic \
	    mid midi midi midi kar midi mpga mpeg mp2 mpeg mp3 mpeg aif x-aiff aiff x-aiff \
	    aifc x-aiff m3u x-mpegurl ram x-pn-realaudio rm x-pn-realaudio ram x-realaudio \
	    bmp bmp cgm cgm gif gif ief ief jpeg jpeg jpg jpeg jpe jpeg png png \
	    svg svg+xml ico x-icon css css shtml html html html htm html asc plain \
	    rtx richtext rtf rtf sgml sgml sgm sgml tsv tab-separated-values mpeg mpeg mpg mpeg \
	    mpe mpeg qt quicktime mov quicktime avi x-msvideo movie x-sgi-movie wnl wnl]
    
    set subtype [string map -nocase $map $extension]
    
    set content_type "application/$subtype; charset=utf-8"

    return [list $contents $cdate $mdate $content_type $size]
}

proc formulae::check_conditions { args } {
    variable values
    set optional {
	{ -lognoter_db db "" }
	{ -page page "" }
    }
    set compulsory "doc values_dict"
    parse_args $optional $compulsory $args

    set root [$doc selectNodes //formulae]
    
    set key [unique_key]
    set interp [_create_interp $key "" $lognoter_db "" [$root @database ""] $page]
    
    dict set values $key doc $doc
    dict set values $key interp $interp
    if { $lognoter_db ne "" } {
	dict set values $key lognoter_db $lognoter_db
    }    
    
    set valuesP_dict ""
    dict for "n v" $values_dict {
	if { [regexp {^p_(.*)} $n {} n] } {
	    $interp eval [list set ::$n $v]
	    dict set valuesP_dict $n $v
	}
    }

    set condition_values [dict create]
    set field_names [list]
    
    if { [$root hasAttribute print_formulas] } {
       set print_formulas [$root @print_formulas]
    } else {
       set print_formulas "no"
    }
    
    #Keep in mind for condition attributes in param tags: 
    #Condition OK -> v = 1 -> disabled attribute of param nodes = false -> v_map = false
    #Condition NO -> v = 0 -> disabled attribute of param nodes = true -> v_map = true 
    
    foreach node [$doc selectNodes //param] {
       if { [$node hasAttribute condition] } {
	  set condition [$node @condition ""]
	  if { [string index [$node @condition] 0] == "\[" } {
	     set value [string range [$node @condition] 1 end-1]
	     set valueE [string map [list \[ \\\[] $value]
		
	     if { [lsearch [list "formatted expression" "text expression"] \
		    [$node @field_type ""]] != -1 } {
		set err [catch { $interp eval $valueE } v]
	     } else {
		if { [lindex $valueE 0] ne "e" } {
		   error "error in formulae::process_container"
		}
		set err [catch { e $key [lindex $valueE 1] } v]
	     }        
	     if { $err && ![string match "*no such variable*" $v] } {
		set has_errors 1
		$interp eval [list set has_errors 1]
		set v "!ERROR ($v)"
	     } elseif { $err && [string match "*no such variable*" $v] } {
		regexp {"(.*?)"} $v disabled_field
		set disabled_field [string trim $disabled_field \"]
		if { $disabled_field in $field_names } {
		   set v 0
		} else {
		   set has_errors 1
		   $interp eval [list set has_errors 1]
		   set v "!ERROR ($v)"
		}
	     }
	  } else {
	     set v 1
	  }
	  if { ![string match "!ERROR*" $v] } {
	     set v_map [string map { 1 false 0 true } $v]
	     dict set condition_values [$node @n] $v_map [$node @field_type]
	     if { [$node @field_type ""] eq "file" } {
		dict set condition_values combo_[$node @n] $v_map button 
	     } 
	  } else {
	     dict set condition_values [$node @n] true ""
	     if { [$node @field_type ""] eq "file" } {
		dict set condition_values combo_[$node @n] true button 
	     }
	  }
	  if { [$node @n] ni $field_names } {
	     lappend field_names [$node @n]
	  }        
       }
       if { [$node @field_type ""] eq "expression" } {      
	  if { [string index [$node @value] 0] == "\[" } {
	     set value [string range [$node @value] 1 end-1]
	     set valueE [string map [list \[ \\\[] $value]
		
	     if { [lindex $valueE 0] ne "e" } {
		error "error in formulae::process_container"
	     }
	     set err [catch { e $key [lindex $valueE 1] } v]
       
	     if { $err && ![string match "*no such variable*" $v] } {
		set has_errors 1
		$interp eval [list set has_errors 1]
		set v "!ERROR ($v)"
	     } elseif { $err && [string match "*no such variable*" $v] } {
		regexp {"(.*?)"} $v disabled_field
		set disabled_field [string trim $disabled_field \"]
		if { $disabled_field in $field_names } {
		   set v "--"
		} else {
		   set has_errors 1
		   $interp eval [list set has_errors 1]
		   set v "!ERROR ($v)"
		}
	     }
	  }
	  if { ![string match "!ERROR*" $v] } {
	     if { $print_formulas } {      
		lassign [print_formulas $key $node $value $v] txt
	     } else {
		if { [$node hasAttribute pn] && [$node @pn] ne "" } {
		   set txt "[$node @pn] = $v"
		   set txt [string map {subscript sub superscript sup} $txt]
		} else { 
		   set txt "" 
		}
	     }
	     set conditions [list $v $txt] 
	  } else {
	     if { [$node hasAttribute pn] && [$node @pn] ne "" } {
		set txt "[$node @pn] = $v"
		set txt [string map {subscript sub superscript sup} $txt]
	     } else { 
		set txt $v 
	     }
	     set conditions [list ERROR $txt]
	  }
	    
	  dict set condition_values [$node @n] $conditions expression
	  if { [$node @n] ni $field_names } {
	     lappend field_names [$node @n]
	  }       
       }
    }
    
    #Keep in mind for condition tags: 
    #Condition OK -> v = 1 -> condition node accomplishes -> v_map = true (right check image)
    #Condition NO -> v = 0 -> condition node doesn't accomplish -> v_map = false (wrong check image)
    
    foreach node [$doc selectNodes //condition] {
	if { [string index [$node @value] 0] == "\[" } {
	    set value [string range [$node @value] 1 end-1]
	    set valueE [string map [list \[ \\\[] $value]

	    if { [lsearch [list "formatted expression" "text expression"] \
		[$node @field_type ""]] != -1 } {
		set err [catch { $interp eval $valueE } v]
	    } else {
		if { [lindex $valueE 0] ne "e" } {
		    error "error in formulae::process_container"
		}
		set err [catch { e $key [lindex $valueE 1] } v]
	    }
	    if { $err && ![string match "*no such variable*" $v] } {
		set has_errors 1
		$interp eval [list set has_errors 1]
		set v "!ERROR ($v)"
	    } elseif { $err && [string match "*no such variable*" $v] } {
		regexp {"(.*?)"} $v disabled_field
		set disabled_field [string trim $disabled_field \"]
		if { $disabled_field in $field_names } {
		   set v 0
		} else {
		   set has_errors 1
		   $interp eval [list set has_errors 1]
		   set v "!ERROR ($v)"
		}
	    } 
	} else {
	    set v 0 
	}

	if { ![string match "!ERROR*" $v] } {
	    if { $print_formulas } {      
		lassign [print_formulas $key $node $value $v] txt
		set v_map [string map { 1 true 0 false } $v]
		set conditions [list $v_map $txt]   
	    } else {
		set v_map [string map { 1 true 0 false } $v]
		set conditions [list $v_map ""]
	    }
	} else {
	    if { $print_formulas } {      
		set conditions [list false ERROR]   
	    } else {
		set conditions [list false ""]
	    }
	}
	dict set condition_values [$node @n] $conditions condition
	if { [$node @n] ni $field_names } {
	    lappend field_names [$node @n]
	} 
    }    
    
    interp delete $interp
    dict set values $key ""

    #condition_values is a list that has the following format:
    #[ node_name { node_value *optional: node_text_to_print* } node_type ]
    
    return [list $condition_values]
}

proc formulae::print_formulas { key node value v } {
    
    if { [$node nodeName] ne "condition" } {
       if { [$node hasAttribute pn] && [$node @pn] ne "" } { 
	  set txt "[$node @pn]"
	  set txt [string map {subscript sub superscript sup} $txt] 
       } else { 
	  set txt "" 
       }
    } else {
       set txt ""
    }
    set dosimple 1
    set err [catch { llength $value } len]
    if { !$err && [lindex $value 0] eq "e" && $len == 2 } {
       set f [lindex $value 1]
    } else {
       set f [string range $value 2 end]
    }
    if { [regexp {[<>]} $value] } {
       regexp {(.*)([<>]=?)(.*)} $f {} v1 sign v2
       set v1_e [string map [list \[ \\\[] $v1]
       set v2_e [string map [list \[ \\\[] $v2]
       set err 0
       if { !$err } { set err [catch { e $key $v1_e } v1_res] }
       if { !$err } { set err [catch { e $key $v2_e } v2_res] }
       if { !$err } { set err [catch { nicer_formula $key $v1 } v1_f] }
       if { !$err } { set err [catch { nicer_formula $key $v2 } v2_f] }
       if { !$err && [isdouble $v1_res] && [isdouble_or_void $v2_res] } {
	  if { $txt ne "" } { append txt " = " }
	  append txt "$v1_f = [nicer_number $v1_res] "
	  append txt "$sign "
	  append txt "$v2_f = [nicer_number $v2_res]"
	  set dosimple 0
       }
    }
    if { $dosimple } {
       set err [catch { llength $value } len]
       if { !$err && [lindex $value 0] eq "e" && $len == 2 } {
	  set f [lindex $value 1]
       } else {
	  set f [string range $value 2 end]
       }
       set err [catch { nicer_formula $key $f } ret]
       if { !$err } {
	  if { $txt ne "" } { append txt " = " }
	     append txt $ret
	  } else {
	     set errorMessage $ret
	     if { $txt ne "" } { append txt " = " }
		append txt [_ "!ERROR"]
	  }
       }
       if { !$err && [$node nodeName] ne "condition" } {
	  append txt " = [nicer_number $v]"
	  if { [$node hasAttribute units] && [$node @units] ne "" } {
	     append txt " [nicer_units [$node @units]]"
	  }
       }
       
       return [list $txt]  
}

proc formulae::calc_and_update_tree { args } {
    variable values
    set optional {
	{ -lognoter_db db "" }
	{ -page page "" }
    }
    set compulsory "doc values_dict XSLTcmd"
    parse_args $optional $compulsory $args

    set page_tcl_code ""
    foreach node [$doc selectNodes //tcl] {
	append page_tcl_code "[$node text]\n"
    }
    set root [$doc selectNodes //formulae]
    
    set edit_choose_name [dict_getd $values_dict edit_choose_name ""]
    set edit_choose_value [dict_getd $values_dict edit_choose_value ""]
    if { $edit_choose_value eq "" } {
	set edit_choose_value [dict_getd $values_dict p_edit_choose_value ""]
    }
    if { [dict exists $values_dict edit_choose] } {
	if { $edit_choose_name eq "" } {
	    error "when using parameter 'edit_choose' it is necessary to use parameter 'edit_choose_name'"
	}
	if { $edit_choose_value eq "" } {
	    set edit_choose_name_p $edit_choose_name
	    set xp [format_xpath {//param[@n=%s]} $edit_choose_name]
	    set domNode [$root selectNodes $xp]
	    if { $domNode ne "" } {
		set edit_choose_name_p [$domNode @pn [$domNode @n]]
	    }
	    foreach domNode [$root childNodes] {
		$domNode delete
	    }
	    set table [$root @database]
	    set vList ""
	    set cmd "select distinct [$lognoter_db sql fs0 $edit_choose_name] from [$lognoter_db sql fs0 $table] 
		order by [$lognoter_db sql fs0 $edit_choose_name]"
	    foreach v [$lognoter_db sql sel $cmd] {
		if { $v eq "" } { continue }
		lappend vList $v
	    }
	    $root appendXML [format_xml {
		    <title>%s</title>
		} [_ "Select %s" $edit_choose_name_p]]
	    
	    set xml [format_xml {
		    <container n="select_field" pn="%s">
		      <param n="edit_choose_value" pn="%s" field_type="options non-editable" condition="false"
		        values="%s" value="%s"/>
		    </container>
		} [_ "Select field"] [_ "Select"] [join $vList ","] [lindex $vList 0]]
	    $root appendXML $xml
	    return
	} else {
	    set table [$root @database]
	    set cols [$lognoter_db sql column_names $table]
	    set valuesList [$lognoter_db sql sel "select * from [$lognoter_db sql fs0 $table] 
		    where [$lognoter_db sql fs0 $edit_choose_name]=[$lognoter_db sql vs0 $edit_choose_value]"]
	    if { $valuesList eq "" } {
		error "when using parameter 'edit_choose' value 'edit_choose_value' is not correct"
	    }
	    foreach col [lrange $cols 3 end] v [lrange $valuesList 3 end] {
		dict set values_dict p_$col $v
	    }
	}
    }

    set key [unique_key]
    set interp [_create_interp $key "" $lognoter_db "" [$root @database ""] $page]
    
    dict set values $key doc $doc
    dict set values $key interp $interp
    if { $lognoter_db ne "" } {
	dict set values $key lognoter_db $lognoter_db
    }    
    $interp eval [list namespace eval form_control {}]
    $interp eval [list set form_control::edit_choose_name $edit_choose_name]
    $interp eval [list set form_control::edit_choose_value $edit_choose_value]
    
    set valuesP_dict ""
    dict for "n v" $values_dict {
	if { [regexp {^p_(.*)} $n {} n] } {
	    $interp eval [list set ::$n $v]
	    dict set valuesP_dict $n $v
	}
    }
    if { $edit_choose_name ne "" } {
	dict set valuesP_dict $edit_choose_name $edit_choose_value
    }
    
    set err [catch {
	    $interp eval $page_tcl_code
	}]
#     if { $err } {
#         puts $::errorInfo
#     }

    set has_errors 0
    set field_names [list]
    
    if { [$root hasAttribute print_formulas] } {
       set print_formulas [$root @print_formulas]
    } else {
       set print_formulas "no"
    }
    
    foreach node [$doc selectNodes //param|//condition] {
	if { [string index [$node @value] 0] == "\[" } {
	    set value [string range [$node @value] 1 end-1]
	    set valueE [string map [list \[ \\\[] $value]

	    if { [lsearch [list "formatted expression" "text expression"] \
		[$node @field_type ""]] != -1 } {
		set err [catch { $interp eval $valueE } v]
	    } else {
		if { [lindex $valueE 0] ne "e" } {
		    error "error in formulae::process_container"
		}
		set err [catch { e $key [lindex $valueE 1] } v]
	    }
	    if { $err && ![string match "*no such variable*" $v] } {
		set has_errors 1
		$interp eval [list set has_errors 1]
		set v "!ERROR ($v)"
	    } elseif { $err && [string match "*no such variable*" $v] } {
		regexp {"(.*?)"} $v disabled_field
		set disabled_field [string trim $disabled_field \"]
		if { $disabled_field in $field_names } {
		   set v "--"
		} else {
		   set has_errors 1
		   $interp eval [list set has_errors 1]
		   set v "!ERROR ($v)"
		}
	    }
	} else {
	    if { [dict exists $values_dict p_[$node @n]] } {
		set v [dict get $values_dict p_[$node @n]]
		if { $v eq "" && [$node @field_type ""] eq "options non-editable" } {
		   set v [$node @value]
		} 
		if { [$node @field_type ""] eq "file" } {
		   if { [dict exists $values_dict p_[$node @n]__size] } {
		       set v_size [dict get $values_dict p_[$node @n]__size]
		   } 
		} 
	    } else {
		set v [$node @value]
	    }
	    if { [lsearch [list "text"] [$node @field_type ""]] == -1 } {
		set err [catch { expr {$v*1.0} } newvalue]
		if { !$err } { set v $newvalue }
	    }
	}
	
	if { [$node @n] ni $field_names } {
	    lappend field_names [$node @n]
	} 
	
	dict set values_dict p_[$node @n] $v

	if { [$node nodeName] eq "param" } {
	    if { [$node @field_type ""] eq "options non-editable" } {
		set vs [$node @values ""]
		if { [string index $vs 0] eq "\[" } {
		    set vs [string range $vs 1 end-1]
		    set err [catch { $interp eval $vs } vs]
		    if { $err } {
		        set vs [list_to_comma_field [list $vs]]
		    }
		    if { $v eq "" } {
		        set v [lindex [comma_field_to_list $vs] 0]
		        $node setAttribute value $v
		    }
		    $node setAttribute values $vs
		}
		set valuesList [comma_field_to_list $vs]
		if { $valuesList eq "0 1" || $valuesList eq "1 0" } {
		    set v [expr {int($v)}]
		}
		if { !$has_errors && [lsearch -exact $valuesList $v] == -1 } {
		    error [_ "error in field '%s' not valid value '%s'" [$node @n] $v]
		}
	    }
	    if { [$node @field_type ""] eq "file" } {
		set file_attr ""
		set filename ""
		set pos_filename [lsearch -exact $v filename]
		if { $pos_filename != -1 } {
		    set filename [lindex $v $pos_filename+1]
		    dict set valuesP_dict [$node @n] [encoding convertfrom utf-8 $filename]
		    set pos_contents [lsearch -exact $v contents]     
		    set contents [lindex $v $pos_contents+1]    
		    lappend file_attr contents [cu::deflate -level 9 $contents]
		    
		    set now [clock seconds] 
		    set date [clock format $now -format "%Y-%m-%d"]
		    set time [clock format $now -format "%H:%M:%S"]
		    set format_now ""
		    lappend format_now $date $time
		    lappend file_attr cdate $format_now
		    lappend file_attr mdate $format_now
		    
		    if { [file extension $filename] ne "" } {
		        lappend file_attr type [string trimleft [file extension $filename] .]
		    } else {
		        lappend file_attr type null
		    }
		    lappend file_attr size [string length [lindex $v $pos_contents+1]]
		    lappend file_attr path null
		} elseif { [dict exists $valuesP_dict [$node @n]] } {  
		    if { [dict exists $valuesP_dict file_status_[$node @n]] } {
		       set file_status [dict get $valuesP_dict file_status_[$node @n]]
		       if { $file_status eq "clear" } {
		          dict set valuesP_dict [$node @n] "clear"
		          set file_attr [list contents "" cdate "" mdate "" type "" size "" path ""] 
		          set filename ""
		       } else {
		          set filename $file_status
		       }
		    } else {
		       set filename $v
		    }
		} else {    
		    set filename ""
		}
		lappend valuesPfile_dict [$node @n] $file_attr
		if { [dict exists $values_dict p_file_status_[$node @n]] } {
		    set values_dict [dict remove $values_dict p_file_status_[$node @n]]
		    set valuesP_dict [dict remove $valuesP_dict file_status_[$node @n]]
		}
		$interp eval [list set ::[$node @n] $filename]
	    } else {
		$interp eval [list set ::[$node @n] $v]
	    }
	    if { [string match "formatted expression" [$node @field_type ""]] } {
		if { [string trim $v] eq "" } { set v "<lognoter/>" }

		if { [string match "!ERROR*" $v] } {
		    set v "<lognoter>$v</lognoter>"
		}
		set err [catch { $node appendXML $v } errstring]
		if { !$has_errors && $err } {
		    error [_ "error: field '%s' not correct value='%s'" [$node @n] $v]
		}
		set n0 [lindex [$node childNodes] 0]
		foreach n [$n0 childNodes] {
		    $node appendChild $n
		}
		$n0 delete
		$node setAttribute value ""
	    } elseif { [string match "formatted" [$node @field_type ""]] } {
		if { ![string match "<wordwidget>*" $v] && ![string match "</wordwidget>" $v] } {
		   set to_parse 1
		   if { [string trim $v] eq "" } { set to_parse 0 }
		   if { [string match "!ERROR*" $v] } {
		       set v "<lognoter>$v</lognoter>"
		       set to_parse 0
		   }
		   if { $to_parse } {
		       set v [encoding convertfrom utf-8 $v]
		       set v "<html>$v</html>" 
		       set err [catch { set v_parsed [html2tdom::parse $v] } errstring]
		       if { !$has_errors && $err } {
		          error [_ "error: field '%s' not correct value='%s'" [$node @n] $v]
		       }
		       set valuesP_dict [dict replace $valuesP_dict format [$v_parsed asXML -indent none]]
		       $node setAttribute value [$v_parsed asXML -indent none]
		   } else {
		       $node setAttribute value [encoding convertfrom utf-8 $v]
		   }
		} else {
		   set v [dom parse $v]
		   set v_parsed [$XSLTcmd -parameters [list base_path "" \
		          form_type "" accept_name [_ "Accept"] edit_name [_ "Edit"] \
		          reset_name [_ "Reset"] page $page edit_choose_name $edit_choose_name \
		          edit_choose_value $edit_choose_value form_interactivity ""] $v]    
		   $node setAttribute value [$v_parsed asHTML]
		}     
	    } elseif { [$node @field_type ""] eq "expression" } {
		if { ![string match "!ERROR*" $v] } {
		   if { $print_formulas } {      
		      lassign [print_formulas $key $node $value $v] txt
		   } else {
		      if { [$node hasAttribute pn] && [$node @pn] ne "" } {
		        set txt "[$node @pn] = $v"
		        set txt [string map {subscript sub superscript sup} $txt]
		      } else { 
		        set txt ""
		      }
		   }
		} else {
		   if { [$node hasAttribute pn] && [$node @pn] ne "" } {
		      set txt "[$node @pn] = $v"
		      set txt [string map {subscript sub superscript sup} $txt]
		   } else { 
		      set txt $v 
		   }
		}
		$node setAttribute print $txt
		$node setAttribute value $v   
	    } else {
		if { [$node @field_type ""] ne "file" } {
		    catch { set v [format "%.5g" $v] }
		    $node setAttribute value $v
		} else {
		    $node setAttribute value [encoding convertfrom utf-8 $filename]
		    if { [info exists v_size] } { $node setAttribute file_size "$v_size" }
		    $node setAttribute file_table "[$root @database]"
		}
	    }
	    if { [$node hasAttribute condition] } {
		set condition [$node @condition ""]
		if { [string index [$node @condition] 0] == "\[" } {
		   set value [string range [$node @condition] 1 end-1]
		   set valueE [string map [list \[ \\\[] $value]

		   if { [lsearch [list "formatted expression" "text expression"] \
		            [$node @field_type ""]] != -1 } {
		      set err [catch { $interp eval $valueE } v]
		   } else {
		      if { [lindex $valueE 0] ne "e" } {
		         error "error in formulae::process_container"
		      }
		      set err [catch { e $key [lindex $valueE 1] } v]
		   }
		   if { $err && ![string match "*no such variable*" $v] } {
		      set has_errors 1
		      $interp eval [list set has_errors 1]
		      set v "!ERROR ($v)"
		   } elseif { $err && [string match "*no such variable*" $v] } {
		      regexp {"(.*?)"} $v disabled_field
		      if { $disabled_field in $field_names } {
		         set v 0
		      } else {
		         set has_errors 1
		         $interp eval [list set has_errors 1]
		         set v "!ERROR ($v)"
		      }
		   } 
		} else {
		   set v 1
		}
		if { ![string match "!ERROR*" $v] } {
		   set v_map [string map {1 false 0 true} $v]
		   $node setAttribute condition $v_map
		} else {
		   $node setAttribute condition false
		}          
	    } else {
		$node setAttribute condition false
	    }
	} else {
	    #when [$node nodeName] eq "condition"
	    if { ![string match "!ERROR*" $v] } {
	       set v_map [string map { 1 true 0 false } $v]
	       $node setAttribute value $v_map
	       if { $print_formulas } {
		  lassign [print_formulas $key $node $value $v] txt 
		  $node setAttribute print $txt 
	       } else {
		  $node setAttribute print ""
	       }
	    } else {
	       $node setAttribute value false
	       $node setAttribute print $v
	    }
	}
    }
    
    foreach node [$root selectNodes title|description] {
	switch [$node nodeName] {
	    title - description {
		set txt [$node text]
		if { [string index $txt 0] == "\[" } {
		    set cmd [string range $txt 1 end-1]
		    set err [catch { $interp eval $cmd } v]
		    set txtNode [$node selectNodes text()]
		    if { [regexp {<.*>} $v] && ![catch { dom parse "<html>$v</html>" doc_in }] } {
		        $txtNode delete
		        $node appendChild [$doc_in documentElement]
		    } else {
		        [$node selectNodes text()] nodeValue $v
		    }
		}
	    }
	}
    }
    
    if { ![info exists valuesPfile_dict] } {
	set valuesPfile_dict ""
    }
    
    if { [$root @database ""] ne "" && !$has_errors } {
	if { [dict exists $values_dict new] } {
	    _add_row_to_database $lognoter_db [$root @database] $valuesP_dict $valuesPfile_dict $root
	} elseif { [dict exists $values_dict edit] } {
	    if { [dict exists $valuesP_dict $edit_choose_name] } {
		_edit_database_row $lognoter_db [$root @database] $valuesP_dict $valuesPfile_dict \
		    $edit_choose_name $root
	    }
	}
    }
    interp delete $interp
    dict set values $key ""
}

################################################################################
#    wizard
################################################################################

proc formulae::wizard_start { key wp } {
    variable values
	
    set doc [dict get $values $key doc]
    set formulaeNode [$doc documentElement]
    set lognoter_db [dict get $values $key lognoter_db]
    set w $wp.wizard
    
    #if { $::debug == 1 } { destroy $w }
    destroy $w
    if { ![winfo exists $w] } {
	formulae::wizard $w -formulae_node $formulaeNode -key $key \
	    -lognoter_db $lognoter_db
    }
    $w open_page 1
}

snit::widgetadaptor formulae::wizard {
    option -formulae_node
    option -key
    option -lognoter_db ""
    
    delegate method * to hull
    delegate option * to hull
    
    variable y_extend_finish_up
    
    constructor {args} {   
	
	installhull using wizard_snit -image [cu::get_image formulae_wizard.png] \
	    -geometry 600x370 -on_exit_callback [mymethod on_exit]
    
	$self configurelist $args
	
	$self create_page -check_callback [mymethod page1_check] [_ "Form type"] \
	    [mymethod page1]
	
	$self create_page  -check_callback [mymethod page2_check] [_ "Import data"] \
	    [mymethod page2]
 
	$self create_page -check_callback [mymethod page3_check] [_ "Create form fields"] \
	    [mymethod page3]
	
	$self create_page -check_callback [mymethod page4_check] [_ "Form created"] \
	    [mymethod page4]
    }
    method page1 { w f } {
	
	set txt [_ "Welcome to the forms creation wizard. Select what kind of form do you wish to create"]
	
	ttk::label $f.l1 -text $txt -wraplength 300 -justify left
	
	ttk::radiobutton $f.r1 -text [_ "A set of fields where to enter values and formulas"] \
	    -variable [$w give_uservar form_type] -value formulae
	
	ttk::radiobutton $f.r2 -text [_ "A form, to store values in a database"] \
	    -variable [$w give_uservar form_type] -value database       

	ttk::radiobutton $f.r3 -text [_ "I shall create it by myself"] \
	    -variable [$w give_uservar form_type] -value nothing       

	grid $f.l1 -sticky w -padx 2 -pady "10 30"
	grid $f.r1 -sticky w -padx 2 -pady 2
	grid $f.r2 -sticky w -padx 2 -pady 2
	grid $f.r3 -sticky w -padx 2 -pady 2
      
	if { [$w exists_uservar form_type] == 0 } {
	    if { $options(-lognoter_db) ne "" } {
		set form_type [$options(-lognoter_db) getpreference \
		        formulae::wizard::form_type]
	    }
	    if { $form_type ne "" } {
		$w set_uservar_value form_type $form_type
	    } else {
		$w set_uservar_value form_type formulae
	    }
	}
    }
    method page1_check { w f next_finish } {
	if { [$w give_uservar_value form_type] eq "nothing" } {
	    return "finish"
	} else {
	    return ""
	}
    }
    method page2 { w f } {
	
	if { $options(-lognoter_db) ne "" } {
	    $options(-lognoter_db) addpreference formulae::wizard::form_type \
		[$w give_uservar_value form_type]
	}
		
	ttk::radiobutton $f.r1 -text [_ "Enter fields definition manually"] \
	    -variable [$w give_uservar field_definition] -value manually
	
	ttk::radiobutton $f.r2 -text [_ "Import from Spreadsheet (Excel or OpenOffice)"] \
	    -variable [$w give_uservar field_definition] -value excel       

	if { [$w give_uservar_value form_type] eq "formulae" } {
	    
	    ttk::radiobutton $f.r3 -text [_ "Automatic"] \
		-variable [$w give_uservar variable_names_where] -value auto       

	    ttk::radiobutton $f.r4 -text [_ "Variable names on the left"] \
		-variable [$w give_uservar variable_names_where] -value left       
	    
	    ttk::button $f.b1 -image [cu::get_icon help-16] -style Toolbutton \
		-command [mymethod _view_image $w [_ "View Spreadsheet variable names"] \
		    excel_style_formulae.png]
	    
	    ttk::radiobutton $f.r5 -text [_ "Variable names above"] \
		-variable [$w give_uservar variable_names_where] -value above
	    
	    ttk::radiobutton $f.r6 -text [_ "Values by color"] \
		-variable [$w give_uservar variable_names_where] -value color       
	} else {
	    ttk::radiobutton $f.r3 -text [_ "Variable names above the table rows"] \
		-variable [$w give_uservar variable_names_where] -value above       
	    
	    ttk::button $f.b1 -image [cu::get_icon help-16] -style Toolbutton \
		-command [mymethod _view_image $w [_ "View Spreadsheet variable names"] \
		    excel_style_database.png]            
	}
	ttk::label $f.l1 -text [_ "Spreadsheet file"]:
	ttk::entry $f.e1 -textvariable [$w give_uservar excel_file]
	ttk::button $f.b2 -image [cu::get_icon filenew-16] -style Toolbutton \
	    -command [mymethod select_excel_file $w]
	
	set formulaeNode $options(-formulae_node)
	set numParams [$formulaeNode selectNodes count(//param|//condition)]
	
	if { $numParams > 0 } {
	    ttk::checkbutton $f.cb1 -text [_ "Delete existing fields"] -variable \
		[$w give_uservar delete_existing_fields]
	}
	
	grid $f.r1 - - -sticky w -padx 2 -pady 5
	grid $f.r2 - - -sticky w -padx 2 -pady 5

	grid $f.r3 - $f.b1 -sticky w -padx 2 -pady "5 0"
	
	set excel_widgets [list $f.r3 $f.b1 $f.l1 $f.e1 $f.b2]
	foreach i [list 4 5 6] {
	    set wi $f.r$i
	    if { [winfo exists $wi] } {
		grid $wi - - -sticky w -padx "20 2" -pady 0
		lappend excel_widgets $wi
	    }
	}
	grid $f.l1 $f.e1 $f.b2 -sticky w -padx 2 -pady 0
	if { [winfo exists $f.cb1] } {
	    grid $f.cb1 -sticky w -padx "20 2" -pady 2
	    lappend excel_widgets $f.cb1
	}
	grid configure $f.r3 $f.l1 -padx "20 2"
	grid configure $f.e1 -sticky ew
	grid columnconfigure $f 1 -weight 1
	
	set d [dict create excel $excel_widgets]
	$w enable_disable_on_key field_definition $d

	if { [$w exists_uservar field_definition] == 0 } {
	    if { $options(-lognoter_db) ne "" } {
		set field_definition [$options(-lognoter_db) getpreference \
		        formulae::wizard::field_definition]
	    }
	    if { $field_definition ne "" } {
		$w set_uservar_value field_definition $field_definition
	    } else {
		$w set_uservar_value field_definition manually
	    }
	    $w set_uservar_value variable_names_where auto
	    $w set_uservar_value excel_file ""
	    $w set_uservar_value delete_existing_fields 1
	}
	if { [$w give_uservar_value form_type] ne "formulae" } {
	    $w set_uservar_value variable_names_where above
	}
    }
    method page2_check { w f next_finish } {
	
	if { [$w give_uservar_value field_definition] ne "excel" } {
	    $w edit_page 3 -is_hidden 0
	    
	} else {
	    foreach i [list form_type variable_names_where excel_file delete_existing_fields] {
		set $i [$w give_uservar_value $i]
	    }
	    $w configure -cursor watch
	    update
	    set err [catch { $self import_excel_file $excel_file $excel_file.xml \
		        $form_type $variable_names_where $delete_existing_fields } new_doc]
	    file delete $excel_file.xml
	    $w configure -cursor ""
	    if { $err } {
		if { [info exists ::lognoter_debug] && $::lognoter_debug == 1 } {
		    set new_doc $::errorInfo
		}
		error [_ "There has been an error when importing the Spreadsheet file (%s)" \
		        $new_doc] $::errorInfo
	    }
	    if { [$w exists_uservar new_doc] && [$w give_uservar_value new_doc] ne "" } {
		[$w give_uservar_value new_doc] delete
	    }
	    $w set_uservar_value new_doc $new_doc
	    $w edit_page 3 -is_hidden 1
	}
	if { $options(-lognoter_db) ne "" } {
	    $options(-lognoter_db) addpreference formulae::wizard::field_definition \
		[$w give_uservar_value field_definition]
	}
	return ""
    }
    method page3 { w f } {
	
	ttk::label $f.l1 -text [_ "Add fields to the form"]:
		
	set columns [list \
		[list 5 [_ #] left text 0] \
		[list 17 [_ Name] left text 0] \
		[list 8 [_ type] left text 0] \
		[list 25 [_ "Default value"] left text 0] \
		[list 15 [_ Container] left text 0] \
		[list 25 xml left text 0] \
		]
	
	package require fulltktree
	set tree [fulltktree $f.tree -width 280 -height 150 \
		-columns $columns -expand 0 -relief solid -bd 1 \
		-selectmode extended -showlines 0 -showrootlines 0 -indent 0 -showbutton 0 \
		-showheader 1 -sensitive_cols all -buttonpress_open_close 0 \
		-selecthandler [mymethod _add_fields_cmd $w $f.tree select] \
		-selecthandler2 [mymethod _add_fields_cmd $w $f.tree select]]
	$w set_uservar_value tree $tree
	$tree column configure 5 -visible 0
	
	$w set_uservar_value active_container ""
	
	ttk::label $f.l2 -text [_ "Field name"]:
	cu::combobox $f.cb1 -textvariable [$w give_uservar field_name] \
	    -valuesvariable [$w give_uservar field_nameList]
		        
	grid $f.l1 -  -sticky w -padx 2 -pady "5 10"
	grid $f.tree - -sticky nsew -padx 4 -pady 2

	grid $f.l2 $f.cb1 -sticky w -padx 2 -pady 2
	grid configure $f.cb1 -sticky ew -padx 2 -pady 2
	
	if { [$w give_uservar_value form_type] eq "formulae" } {
	    set f1 [ttk::labelframe $f.f1 -text [_ "default value"]]
	    ttk::frame $f1.radios
	    ttk::radiobutton $f1.radios.r1 -text [_ "Numeric"] -variable \
		[$w give_uservar number_expression] -value numeric
	    ttk::radiobutton $f1.radios.r2 -text [_ "Expression"] -variable \
		[$w give_uservar number_expression] -value expression
	    ttk::menubutton $f1.radios.mb1 -text [_ "More"]... -menu \
		$f1.radios.mb1.m
	    
	    set m [menu $f1.radios.mb1.m -tearoff 0]
	    
	    set field_types [list condition text "long text" "text expression" \
		    "options editable" "options non-editable" \
		    "options multiple editable" "options multiple non-editable" \
		formatted "formatted expression" button date file report_maker]

	    foreach ft $field_types {
		$m add radiobutton -label $ft -variable \
		    [$w give_uservar number_expression] \
		    -value $ft
	    }
	    
	    grid $f1.radios.r1 $f1.radios.r2 $f1.radios.mb1 -sticky w -padx 2 -pady 2
	    
	    ttk::label $f1.l3 -textvariable [$w give_uservar label_name]
	    
	    set examples_num { 0.0 123.45 }
	    
	    set examples_expr {
		123.45
		A+2*F+sin(N2)*sqrt(V3)
		B>=C
	    }

	    cu::combobox $f1.cb2 -textvariable [$w give_uservar field_value] \
		-valuesvariable [$w give_uservar def_val_examples]
	    
	    ttk::menubutton $f1.mb1 -image [cu::get_icon keditbookmarks-16] \
		-style Toolbutton -menu $f1.mb1.m
	    menu $f1.mb1.m -tearoff 0
	    
	    set cmd [mymethod _add_fields_cmd $w $f.tree update_fields_menu $f1.mb1.m \
		    $f1.cb2]
	    $w add_trace_to_uservar field_nameList $cmd

	    formulae::_create_functions_button $f1.mb2 $f1.cb2 ""
	    
	    regsub -all {\s*\n\s*} $examples_expr "\n\t" examples_expr
	    set examples_expr [string trim $examples_expr]
	    
	    foreach i [list $f1.l3 $f1.mb1 $f1.mb1 $f1.mb2] {
		tooltip $i [_ "Examples:\n\t%s" $examples_expr]
	    }
	    
	    set d [dict create expression "$f1.mb1 $f1.mb2" condition "$f1.mb1 $f1.mb2"]
	    $w enable_disable_on_key -clear number_expression $d
	    
	    set d [dict create number [list label_name [_ Number]: \
		        def_val_examples $examples_num] \
		    expression [list label_name [_ Expression]: def_val_examples \
		        $examples_expr]]
	    $w change_key_on_key -clear number_expression $d
	    
	    grid $f1.radios - - - -sticky w
	    grid $f1.l3 $f1.cb2 $f1.mb1 $f1.mb2 -sticky w
	    grid configure $f1.l3 -padx 2
	    grid configure $f1.cb2 -sticky ew -padx 2 -pady 2
	    grid columnconfigure $f1 1 -weight 1
	    grid $f1 - -sticky ew -padx 3 -pady 3
	}
	
	set fb [ttk::frame $f.fb]
	ttk::button $fb.bok -image [cu::get_icon ok-16] -text [_ Add] -compound left \
	    -command [mymethod _add_fields_cmd $w $f.tree add]
	
	tooltip $fb.bok [_ "Add a new field to the form"]
  
	ttk::button $fb.edit -image [cu::get_icon edit-16] -text [_ Edit] -compound left \
	    -command [mymethod _add_fields_cmd $w $f.tree edit]
	
	tooltip $fb.edit [_ "Edit the selected field of the form"]

	ttk::button $fb.remove -image [cu::get_icon remove-16] -text [_ Remove] \
	    -compound left  \
	    -command [mymethod _add_fields_cmd $w $f.tree delete]
	
	tooltip $fb.remove [_ "Remove the selected field from the list"]
      
	ttk::menubutton $fb.bmore -image [cu::get_icon list-add-16] -text [_ More]... \
	    -compound left \
	    -menu $fb.bmore.m
	set m [menu $fb.bmore.m -tearoff 0]
	$m add command -label [_ "Add new field in new container"] -command \
	    [mymethod _add_fields_cmd $w $f.tree add_in_new_container]
	$m add command -label [_ "Edit field in new container"] -command \
	    [mymethod _add_fields_cmd $w $f.tree edit_in_new_container]

	$m add separator
	$m add command -label [_ "Delete all fields"] -command \
	    [mymethod _add_fields_cmd $w $f.tree delete_all]

	grid $fb.bok $fb.edit $fb.remove $fb.bmore -padx 3 -pady 4
	
	grid $fb -
	
	grid columnconfigure $f 1 -weight 1
	grid rowconfigure $f 1 -weight 1
	
	bind $f.cb1 <Return> [list $fb.bok invoke]
	
	foreach "n v" [list field_name "" field_nameList "" field_value ""] {
	    $w set_uservar_value $n $v
	}
	if { [$w give_uservar_value form_type] eq "formulae" } {
	    $w set_uservar_value number_expression numeric
	} else {
	    $w set_uservar_value number_expression text
	}
	$self _add_fields_cmd $w $f.tree init
    }
    method page3_check { w f next_finish } {
	set tree [$w give_uservar_value tree]
	$self _add_fields_cmd $w $tree tree_to_xml
    }  
    method page4 { w f } {
	set new_doc [$w give_uservar_value new_doc]
	set numParams [$new_doc selectNodes count(//param|//condition)]
	set numRows [$new_doc selectNodes count(//row)]
	
	if { $numParams == 0 } {
	    if { [$w give_uservar_value field_definition] eq "excel" } {
		set txt [_ "It has not been possible to import any field from the Spreadsheet file"]
	    } else {
		set txt [_ "No field has been created"]
	    }   
	} elseif { !$numRows } {
	    set txt [_ "A new form has been created with %d fields" $numParams]
	} else {
	    set txt [_ "A new form has been created with %d fields. %d rows have been imported" \
		    $numParams $numRows]
	}
	ttk::label $f.l1 -text $txt -wraplength 300 -justify left
	
	grid $f.l1 -sticky w -padx 2 -pady 20

	if { $numParams } {
	    set txt [_ "You can modify any field in the form by right-clicking the mouse over it"]
	} else {
	    if { [$w give_uservar_value field_definition] eq "excel" } {
		set txt [_ "You can try with a new Spreadsheet file by pressing button 'Previous'"] 
	    } else {
		set txt [_ "You can add new fields by pressing button 'Previous'"]
	    }  
	}
	ttk::label $f.l2 -text $txt -wraplength 300 -justify left
	grid $f.l2 -sticky w -padx 2 -pady 20
    }
    method page4_check { w f next_finish } {

	set formulaeNode $options(-formulae_node)
	set new_doc [$w give_uservar_value new_doc]
	set form_type [$w give_uservar_value form_type]
	set key $options(-key)
	
	$formulaeNode setAttribute usableviewmode 1
	foreach node [$formulaeNode selectNodes container|setname|database] {
	    $node delete
	}
	foreach node [$new_doc selectNodes /*/container|/*/database] {
	    $formulaeNode appendChild $node
	}
	
	set hasdatabasename 0
	set databasename ""
	if { $form_type eq "formulae" } {
	    set hasdatabase 1
	    set has_unique_name 1
	    if { [$formulaeNode @database ""] ne "" } {
		set hasdatabasename 1
		set databasename [$formulaeNode @database]
	    }
	} else {
	    set hasdatabase 1
	    set has_unique_name 0
	    if { [$formulaeNode @database ""] ne "" } {
		set hasdatabasename 1
		set databasename [$formulaeNode @database]
	    }
	}

	set err [catch { formulae::create_update_database -databasename $databasename \
		    $key $hasdatabase $hasdatabasename $has_unique_name } ret]
	if { $err } {
	    if { $ret ne "" } {
		snit_messageBox -icon error -message $ret
	    }
	    return ""
	}

	formulae::fill_database_values_from_internal "" $key
	after idle [list formulae::create_windowD_do $key]
	return ""
    }
    method on_exit { w } {
	if { [$w exists_uservar new_doc] && [$w give_uservar_value new_doc] ne "" } {
	    [$w give_uservar_value new_doc] delete
	}
    }
    method _view_image { wp title image_name } {
	set w [dialogwin_snit $wp._ask -title $title -okname -]
	set f [$w giveframe]
	
	ttk::label $f.l1 -image [cu::get_image $image_name]
	
	grid $f.l1 -sticky w -pady 2
	
	bind [winfo toplevel $f] <Return> [list $w invokecancel]
	set action [$w createwindow]
	destroy $w
    }
    method _add_fields_cmd { w tree what args } {
	switch $what {
	    init {
		set formulaeNode $options(-formulae_node)
		set new_doc [$w give_uservar_value new_doc ""]
		if { $new_doc eq "" } {
		    set new_doc [dom parse "<formulae/>"]
		    $w set_uservar_value new_doc $new_doc
		    set new_root [$new_doc documentElement]
		    foreach node [$formulaeNode selectNodes container] {
		        $new_root appendChild [$node cloneNode -deep]
		    }
		} else {
		    set new_root [$new_doc documentElement]
		}
		set field_nameList [$w give_uservar_value field_nameList]  
		$tree item delete all
		set num 1
		foreach cNode [$new_root selectNodes container] {
		    foreach pNode [$cNode selectNodes param|condition] {
		        switch [$pNode nodeName] {
		            param { set field_type [$pNode @field_type ""] }
		            condition { set field_type condition }
		        }
		        set value [$pNode @value]
		        if { $field_type in "{} numeric expression condition" } {
		            if { [string index [$pNode @value] 0] eq "\[" } {
		                set value [string range [$pNode @value] 1 end-1]
		                set err [catch { llength $value } len]
		                if { !$err && $len == 2 } {
		                    set value [lindex $value 1]
		                } else {
		                    set value [string range $value 2 end]
		                }
		                if { $field_type ne "condition" } {
		                    set field_type expression
		                }
		            } else {
		                set field_type numeric
		            }
		        }
		        $tree insert end [list $num [$pNode @n] $field_type \
		                $value [$cNode @n] [$pNode asXML]]
		        lappend field_nameList [$pNode @n]
		        incr num
		    }
		}
		$w set_uservar_value field_nameList $field_nameList
	    }
	    tree_to_xml {
		set new_doc [$w give_uservar_value new_doc]
		set new_root [$new_doc documentElement]
		foreach node [$new_root selectNodes container] {
		        $node delete
		}
		set rows ""
		foreach item [$tree item children 0] {
		    lappend rows [$tree item text $item]
		}
		foreach row [lsort -index 0 $rows] {
		    lassign $row - name field_type value container xml
		    set xp [format_xpath {container[@n=%s]} $container]
		    set cNode [$new_root selectNodes $xp] 
		    if { $cNode eq "" } {
		        set cNode [$new_root appendChildTag container [list attributes() \
		                    n $container]]
		    }
		    if { [string trim $xml] ne "" } {
		        $cNode appendXML $xml
		        set pNode [$cNode selectNodes {(param|condition)[last()]}]
		    } else {
		        switch $type {
		            condition { set nodeName condition }
		            default { set nodeName param }
		        }
		        set pNode [$cNode appendChildTag $nodeName [list attributes() \
		                    n $name]]
		    }
		    if { $field_type in "expression condition" } {
		        set value "\[[list e $value]\]"  
		    }
		    if { [$pNode @n] ne $name } { $pNode setAttribute pn "" }
		    $pNode setAttribute n $name field_type $field_type value $value
		}
	    }   
	    select {
		set item [$tree selection get]
		if { [llength $item] != 1 } { return }
		set vars [list num field_name number_expression field_value active_container]
		foreach n $vars v [$tree item text $item] {
		    $w set_uservar_value $n $v
		}
	    }
	    add_in_new_container - edit_in_new_container {
		set containers ""
		foreach item [$tree item children 0] {
		    lappend containers [$tree item text $item 4]
		}
		set containers [lsort -dictionary -unique $containers]
		
		if { $what eq "edit_in_new_container" } {
		    set item [$tree selection get]
		    if { [llength $item] != 1 } {
		        snit_messageBox -message [_ "It is necessary to select one field in the list"] \
		            -parent $win
		        return
		    }
		    lassign [$tree item text $item] - - - - ag
		} else {
		    set ag [$w give_uservar_value active_container]
		}
		if { $ag eq "" } { set ag [_ "Container %d" 1] }
		destroy $win.gr1
		dialogwin_snit $win.gr1 -title [_ "Select container"] -entrytype entry \
		    -entrytext [_ "Enter the name for the container where the field will be inserted:"] \
		    -entryvalues $containers -entrydefault $ag
		
		set f [$win.gr1 giveframe]
		set font [ttk::style lookup TLabel -font]
		set size [expr {[font actual $font -size]-1}]
		set font [concat [font actual $font] [list -size $size]]
		set bfont [concat [font actual $font] [list -size $size -weight bold]]
		text $f.t -bg [$win.gr1 giveframe_background] -bd 0 -width 20 -height 3 \
		    -wrap word -font $font
		$f.t tag configure bold -font $bfont
		$f.t insert end "[_ Note]: " bold \
		    [_ "All fields need to be contained in one container. There can exist one or more containers"]
		$f.t configure -state disabled
		bind $f.t <1> [list focus $f.t]
		grid $f.t - -sticky nwe
		grid columnconfigure $f 0 -weight 1
		
		set action [$win.gr1 createwindow]
		set active_container [$win.gr1 giveentryvalue]
		destroy $win.gr1
		if { $action <= 0 } { return }
		if { $active_container eq "" } {
		    snit_messageBox -message [_ "It is necessary to enter a container name"] \
		        -parent $win
		    return
		}
		switch $what {
		    add_in_new_container { set cmd add }
		    edit_in_new_container { set cmd edit }
		}
		return [$self _add_fields_cmd $w $tree $cmd -container $active_container]
	    }
	    _give_fields {
		set whatfor [lindex $args 0]
		foreach i [list field_name number_expression field_value] {
		    set $i [string trim [$w give_uservar_value $i]]
		}
		if { $field_name eq "" } {
		    error [_ "It is necessary to enter a field name"]
		}
		if { $whatfor eq "add" } {
		    if { $field_name in [$w give_uservar_value field_nameList] } {
		        error [_ "A field with this name already exists"]
		    }
		}
		if { $number_expression eq "numeric" && 
		    ![string is double -strict $field_value]} {
		    error [_ "The value for a numeric field needs to be a number"]
		}
		if { $number_expression eq "expression" && $field_value eq "" } {
		   error [_ "The value for a expression field cannot be void"]
		}
		return [list $field_name $number_expression $field_value]
	    }
	    add {
		set optional {
		    { -container active_container "" }
		}
		set compulsory ""
		parse_args $optional $compulsory $args

		set num 0
		foreach item [$tree item children 0] {
		    set n [$tree item text $item 0]
		    if { $n > $num } { set num $n }
		}
		incr num
		if { $container eq "" } {
		    set container [$w give_uservar_value active_container]
		}
		if { $container eq "" } {
		    return [$self _add_fields_cmd $w $tree add_in_new_container]
		}
		set err [catch { $self _add_fields_cmd $w $tree _give_fields add } l]
		if { $err } {
		    snit_messageBox -message $l -parent $win
		    return
		}
		set ltot [list $num {*}$l $container ""]
		$tree insert end $ltot
		
		set field_nameList [$w give_uservar_value field_nameList]
		lappend field_nameList [lindex $l 0]
		$w set_uservar_value field_nameList $field_nameList
		$w set_uservar_value field_name ""
		$w set_uservar_value active_container $container
	    }
	    edit {
		set optional {
		    { -container active_container "" }
		}
		set compulsory ""
		parse_args $optional $compulsory $args

		set item [$tree selection get]
		if { [llength $item] != 1 } {
		    snit_messageBox -message [_ "It is necessary to select one field in the list"] \
		        -parent $win
		    return
		}
		lassign [$tree item text $item] num field_name number_expression field_value \
		    container_in xml
		if { $container eq "" } { set container $container_in }
		set err [catch { $self _add_fields_cmd $w $tree _give_fields edit } l]
		if { $err } {
		    snit_messageBox -message $l -parent $win
		    return
		}
		set ltot [list $num {*}$l $container $xml]
		$tree insert next $ltot $item
		$tree item delete $item
		
		set field_name_new [lindex $l 0]
		if { $field_name ne $field_name_new } {
		    set field_nameList [$w give_uservar_value field_nameList]
		    set ipos [lsearch -exact $field_nameList $field_name]
		    set field_nameList [lreplace $field_nameList $ipos $ipos]
		    lappend field_nameList $field_name_new
		    $w set_uservar_value field_nameList $field_nameList
		}
		$w set_uservar_value field_name ""
		$w set_uservar_value active_container $container
	    }
	    delete - delete_all {
		switch $what {
		    delete { set items [$tree selection get]  }
		    delete_all { set items [$tree item children 0]  }
		}
		set names ""
		foreach item [lsort -decreasing -integer $items] {
		    set num [$tree item text $item 0]
		    lappend names [$tree item text $item 1]
		    foreach i [$tree item children 0] {
		        if { [$tree item text $item 0] > $num } {
		            $tree item text $item 0 [expr {[$tree item text $item 0]-1}]
		        }
		    }
		    $tree item delete $item
		}
		if { $what eq "delete" } {
		    set field_nameList [$w give_uservar_value field_nameList]
		    foreach name $names {
		        set ipos [lsearch -exact $field_nameList $name]
		        set field_nameList [lreplace $field_nameList $ipos $ipos]
		    }
		    $w set_uservar_value field_nameList $field_nameList
		} else {
		    $w set_uservar_value field_nameList ""
		}
	    }
	    update_fields_menu {
		lassign $args menu entry
		$menu delete 0 end
		foreach name [$w give_uservar_value field_nameList] {
		    $menu add command -label $name -command \
		        [namespace code [list _insert_in_entry $entry $name]]
		}
	    }
	}
    }
    method select_excel_file { w } {
	
	if { $options(-lognoter_db) ne "" } {
	    set lastimportdir [$options(-lognoter_db) getpreference lastimportdir]
	}
	if { ![info exists lastimportdir] || ![file isdirectory $lastimportdir] } {
	    if { [info exists ::topdir_external] } {
		set lastimportdir [file join $::topdir_external templates examples]
	    }
	    if { ![file isdirectory $lastimportdir] } {
		set lastimportdir [pwd]
	    }
	}
	set file [tk_getOpenFile -defaultextension .xls -filetypes [list \
		    [list [_ "Spreadsheet files"] [list ".xls" ".xlsx" ".xml" ".ods"]] \
		    [list [_ "All files"] [list "*"]]] \
		-initialdir $lastimportdir -parent $w -title [_ "Import Spreadsheet file"]]
	if { $file eq "" } { return }
	if { $options(-lognoter_db) ne "" } {
	    $options(-lognoter_db) addpreference lastimportdir [file dirname $file]
	}
	$w set_uservar_value excel_file $file
    }
    proc _row_col_to_colname { row col } {
	
	set A [scan A %c]
	incr col -1
	set colA ""
	while { $col >= 0 } {
	    set colA [format %c [expr {$A+$col%26}]]$colA
	    set col [expr {$col/26-1}]
	}
       return $colA$row 
    }
    proc _colname_to_row_col { colname } {
	
	set colname [string toupper $colname]
	regexp {([A-Z]+)(\d+)} $colname {} colA row

	set A [scan A %c]
	set col 0
	set fac 1
	foreach i [lreverse [split $colA ""]] {
	    set col [expr {$col+([scan $i %c]-$A+1)*$fac}]
	    set fac [expr {$fac*26}]
	}
	return [list $row $col]
    }
    proc _colname_to_col { colname } {
	
	set colname [string toupper $colname]
	regexp {([A-Z]+)} $colname {} colA

	set A [scan A %c]
	set col 0
	set fac 1
	foreach i [lreverse [split $colA ""]] {
	    set col [expr {$col+([scan $i %c]-$A+1)*$fac}]
	    set fac [expr {$fac*26}]
	}
	return $col
    }
    proc _range_to_row_col_list { ranges } {
	set retList ""
	foreach range [split $ranges ","] {
	    if { [regexp {^R(\d+)C(\d+)$} $range {} row1 col1] } {
		lassign [list $row1 $col1] row2 col2
	    } elseif { [regexp {^R(\d+)C(\d+):R(\d+)C(\d+)$} $range {} row1 col1 \
		row2 col2] } {
	    } elseif { [regexp {^([A-Z]+[\d+]+):([A-Z]+[\d+]+)$} $range {} r1 r2] } {
		lassign [_colname_to_row_col $r1] row1 col1
		lassign [_colname_to_row_col $r2] row2 col2
	    } elseif { [regexp {^[A-Z]+[\d+]+$} $range] } {
		lassign [_colname_to_row_col $range] row1 col1
		lassign [list $row1 $col1] row2 col2
	    } else {
		continue
	    }
	    if { $row1 > $row2 } { lassign [list $row1 $row2] row2 row1 }
	    if { $col1 > $col2 } { lassign [list $col1 $col2] col2 col1 }
	    for { set row $row1 } { $row <= $row2 } { incr row } {
		for { set col $col1 } { $col <= $col2 } { incr col } {
		    lappend retList $row $col
		}
	    }
	    return $retList
	}
    }
    proc _parse_formula_refs { data sname row col reverse_rows_cols } {
	set sheet1 {([^-!',+*/(){}=]+)}
	set sheet2 {'([^']+)'}
	set rel {(\[?[-\d]+\]?)}
	set colname {([A-Z]+)(\d+)(?![(\w])}
	set rex "(?:$sheet1!|$sheet2!|)(?:R(?:$rel)?C(?:$rel)?|$colname)"
	
	set ret ""
	set last_idx 0
	foreach "full s1_idx s2_idx r1_idx c1_idx c2_idx r2_idx" [regexp -inline -indices -all $rex $data] {
	    foreach v [list s1 s2 r1 c1 r2 c2] {
		set $v [string range $data {*}[set ${v}_idx]]
	    }
	    if { $r1 ne "" || $c1 ne "" } {
		lassign [list $r1 $c1] r c
	    } else {
		lassign [list $r2 $c2] r c
	    }
	    if { $r eq "" && $c eq "" } { continue }
	    if { $s1 ne "" } { set s $s1 } elseif { $s2 ne "" } { set s $s2 } else {
		set s $sname
		regsub {_cnd$} $s {} s
	    }
	    if { $r eq "" } {
		set r $row
	    } elseif { [regexp {\[([-\d]+)\]} $r {} rel] } {
		set r [expr {$row+$rel}]
	    }
	    if { $c eq "" } {
		set c $col
	    } elseif { [regexp {\[([-\d]+)\]} $c {} rel] } {
		set c [expr {$col+$rel}]
	    } elseif { [regexp {[A-Z]} $c] } {
		set c [_colname_to_col $c]
	    }
	    if { $reverse_rows_cols } { foreach "r c" [list $c $r] break }
	    foreach "idx0 idx1" $full break
	    
	    if { $idx0 == $last_idx+1 && [string index $data $last_idx] eq ":" } {
		foreach "- last_r last_c" [lindex $ret end] break
		lset ret end end ","
		
		incr idx0 -1
		for { set row_range $last_r } { $row_range <= $r } { incr row_range } {
		    for { set col_range $last_c } { $col_range <= $c } { incr col_range } {
		        if { $row_range == $last_r && $col_range == $last_c } { continue }
		        if { $row_range == $r && $col_range == $c } { continue }
		        lappend ret [list $s $row_range $col_range $idx0 $idx0 ","]
		    }
		}
	    }
	    set last_idx [expr {$idx1+1}]
	    lappend ret [list $s $r $c $idx0 $idx1 ""]
	}
	return $ret
    }
    proc _is_red_color { colorList } {
	foreach color $colorList {
	    regexp {(\w{2})(\w{2})(\w{2})} $color {} r g b
	    foreach i [list r g b] { set $i [scan [set $i] %x] }
	    if { $r == 0 } { continue }
	    if { ($r-$g)/double($r) > 0.15 && ($r-$b)/double($r) > 0.15 } {
	       return 1
	    }
	}
	return 0
    }
    proc _generate_name { name sname bname fieldsList idx } {
	if { $idx ne "" } {
	    set idxOpt "-index $idx"
	} else {
	    set idxOpt ""
	}
	if { $name ne "" } {
	    regsub -all {[:]} $name {} pname
	    regsub -all {[-+*/(){}:.,]} $name {} name
	    regsub -all {\s+} $name {_} name
	    set base $name
	    set num 2
	    while { [lsearch {*}$idxOpt -exact -nocase $fieldsList $name] != -1 } {
		set name "${base}_$num"
		incr num
	    }
	} else {
	    set bname [string trimright $bname ":"]
	    set ipos [lsearch {*}$idxOpt -exact -nocase $fieldsList $bname]
	    if { $ipos != -1 } {
		set name $sname!$bname
	    } else {
		set name $bname
	    }
	    regsub -all {[:]} $name {} pname
	    regsub -all {[-+*/(){}:.,]} $name {_} name
	    regsub -all {\s+} $name {} name
	}
	return [list $name $pname]
    }
    method import_excel_file { file tmp_file form_type variable_names_where delete_existing_fields } {
	
	set file [string trim $file]
	if { $file eq "" } {
	    error [_ "It is necessary to enter a file name"]
	} elseif { ![file exists $file] } {
	    error [_ "File '%s' does not exist"]
	}
	switch -- [file extension $file] {
		    ".xlsx" { set filetype xlsx }
		    ".ods" { set filetype ods }
	    default { set filetype xml2003 }
	}

	set ns ""
	if { $filetype eq "xlsx" } {
	    set h [cu::zip open $file r]
	    
	    lappend ns e http://schemas.openxmlformats.org/spreadsheetml/2006/main ee ""

	    set err [catch { cu::zip set $h xl/sharedStrings.xml }]
	    if { !$err } {
		dom parse [encoding convertfrom utf-8 [cu::zip read $h]] doc_si
		foreach node [$doc_si selectNodes -namespaces $ns /*/e:si] {
		    lappend sharedStrings [$node selectNodes string(.)]
		}
		$doc_si delete
	    }
	    cu::zip set $h xl/workbook.xml
	    dom parse [encoding convertfrom utf-8 [cu::zip read $h]] doc
	    set root [$doc documentElement]
	    foreach n [$root selectNodes -namespaces $ns {e:sheets/e:sheet}] {
		cu::zip set $h xl/worksheets/sheet[$n @sheetId].xml
		$root appendXML [encoding convertfrom utf-8 [cu::zip read $h]]
		set ni [lindex [$root childNodes] end]
		$ni setAttribute name [$n @name]
	    }
	    set err [catch { cu::zip set $h xl/styles.xml }]
	    if { !$err } {
		$root appendXML [encoding convertfrom utf-8 [cu::zip read $h]]
	    }
	    cu::zip close $h
	    
	    set nv {
		sheet_xp /*/e:worksheet
		sheet_name_att name
		row_xp e:sheetData/e:row
		row_index_att r
		col_xp e:c
		col_type_xp string(@t)
		col_formula_xp string(e:f)
		col_value_xp string(e:v)
		col_data_xp string(e:v)
		col_style_xp string(@s)
		CF_xp e:conditionalFormatting
		CF_range_xp string(@sqref)
		CF_condition_xp e:cfRule
		CF_condition_qualifier_xp string(@operator)
		CF_style string(@dxfId)
		CF_value string(e:formula)
		validation_xp e:dataValidations/e:dataValidation
		validation_type_xp {@type != "list"}
		validation_values_xp {string(e:formula1)}
		merge_range_xp {string(e:mergeCells/e:mergeCell/@ref[starts-with(.,%s)])}
	    }

	} elseif { $filetype eq "ods" } {
	    set h [cu::zip open $file r]
	    cu::zip set $h content.xml
	    dom parse [encoding convertfrom utf-8 [cu::zip read $h]] doc
	    set root [$doc documentElement]
	    
	    lappend ns office urn:oasis:names:tc:opendocument:xmlns:office:1.0 \
		table urn:oasis:names:tc:opendocument:xmlns:table:1.0 \
		style urn:oasis:names:tc:opendocument:xmlns:style:1.0 \
		ee "" 
	  
	    set err [catch { cu::zip set $h styles.xml }]
	    if { !$err } {
		$root appendXML [encoding convertfrom utf-8 [cu::zip read $h]]
		set ni [lindex [$root childNodes] end]
		foreach n [$ni childNodes] {
		    $root appendChild $n
		}
		$ni delete
	    }
	    cu::zip close $h
	    
	    set nv {
		sheet_xp /*/office:body/office:spreadsheet/table:table
		sheet_name_att table:name
		row_xp table:table-row
		row_index_att ""
		col_xp table:table-cell|table:covered-table-cell
		col_type_xp string(@office:value-type)
		col_formula_xp string(@table:formula)
		col_value_xp string(@office:value)
		col_data_xp string(.)
		col_style_xp string(@table:style-name)
		CF_xp NONE
		validation_xp NONE
		validation_type_xp NONE
		validation_values_xp NONE
		merge_range_xp NONE
	    }

	} else {
	    if { [file extension $file] ne ".xml" } {
		set err [catch {
		        package require tcom
		        ::tcom::ref createobj Excel.Application
		    } excel]
		if { $err } {
		    error [_ "It is necessary to have Excel installed in the computer. Try to import format XML-2003 or XLSX or ODS"]
		}
		set workbooks [$excel Workbooks]
		set workbook [$workbooks Open $file]
		# 46 xlXMLSpreadsheet
		$workbook SaveAs [file nativename $tmp_file] 46
		$excel Quit
	    } else {
		set tmp_file $file
	    }
	    set xml [tDOM::xmlReadFile $tmp_file]
	    set doc [dom parse $xml]
	    lappend ns e urn:schemas-microsoft-com:office:spreadsheet \
		ee urn:schemas-microsoft-com:office:excel
	    
	    set nv {
		sheet_xp /*/e:Worksheet
		sheet_name_att ss:Name
		row_xp e:Table/e:Row
		row_index_att ss:Index
		col_xp e:Cell
		col_type_xp string(e:Data/@ss:Type)
		col_formula_xp string(@ss:Formula)
		col_value_xp string(e:Data)
		col_data_xp string(e:Data)
		col_style_xp string(@ss:StyleID)
		style_def_xp {/*/e:Styles/e:Style[@ss:ID=%s]}
		CF_xp ee:ConditionalFormatting
		CF_range_xp string(ee:Range)
		CF_condition_xp ee:Condition
		CF_condition_qualifier_xp string(ee:Qualifier)
		CF_style string(ee:Format/@Style)
		CF_value string(ee:Value1)
		validation_xp ee:DataValidation
		validation_type_xp {ee:Type != "List"}
		validation_values_xp {string(ee:Value)}
		merge_range_xp NONE
	    }
	}
	foreach "n v" $nv { set $n $v }
	
#         if { $variable_names_where eq "above" && $form_type eq "formulae" } {
#             set reverse_rows_cols 1
#         } else {
#             set reverse_rows_cols 0
#         }
	set reverse_rows_cols 0
	
################################################################################
#    from XML to a dict
################################################################################

	lassign [list "" "" 1 0 0] vals snameList sheetNum maxRow maxCol
	
	foreach sheetNode [$doc selectNodes -namespaces $ns $sheet_xp] {
	    set sname [$sheetNode @$sheet_name_att N$sheetNum]
	    set row 1
	    foreach rowNode [$sheetNode selectNodes -namespaces $ns $row_xp] {
		if { [$rowNode @$row_index_att ""] ne "" } {
		    set row [$rowNode @$row_index_att]
		}
		set col 1
		foreach colNode [$rowNode selectNodes -namespaces $ns $col_xp] {
		    set newcol ""
		    if { [$colNode @$row_index_att ""] ne "" } {
		        set newcol [$colNode @$row_index_att]
		    }
		    #if { $col eq "" } { continue }
		    
		    if { $newcol ne "" && ![string is digit -strict $newcol] } {
		        set newcol [_colname_to_col $newcol]
		    }
		    if { $newcol ne "" } {
		        if { $newcol < $col } { continue }
		        set col $newcol
		    }
		    set type [$colNode selectNodes -namespaces $ns $col_type_xp]
		    set formula [$colNode selectNodes -namespaces $ns $col_formula_xp]
		    if { $formula ne "" } {
		        set type formula
		        set data [string trimleft $formula =]
		        regsub {^\w+:=} $data {} data
		        regsub -all {\[([^.]+)\.([A-Z]+\d+)\]} $data {\1!\2} data
		        regsub -all {\[\.([A-Z]+\d+)\]} $data {\1} data
		    } else {
		        switch $type {
		            float - currency - percentage - Number - "" {
		                set data [$colNode selectNodes -namespaces $ns $col_value_xp]
		                if { [string is double -strict $data] &&
		                    [string length $data] > 8 } {
		                    set data [format %g $data]
		                }
		                set type Number
		            }
		            DateTime {
		                set data [$colNode selectNodes -namespaces $ns $col_value_xp]
		                regsub -all {T.*$} $data {} data
		                set type Date
		            }
		            String - string {
		                set data [$colNode selectNodes -namespaces $ns $col_data_xp]
		                set type String
		            }
		            s {
		                set num [$colNode selectNodes -namespaces $ns $col_data_xp]
		                set data [lindex $sharedStrings $num]
		                set type String
		            }
		            default {
		                set data ""
		            }
		        }
		    }
		    set color ""
		    set style [$colNode selectNodes -namespaces $ns $col_style_xp]
		    if { [info exists style_def_xp] } {
		        set xp [format_xpath $style_def_xp $style]
		        set node [$colNode selectNodes -namespaces $ns $xp]
		        if { $node ne "" } {
		            set color ""
		            foreach "- c" [regexp -inline -all {#(\w{6})} [$node asXML]] {
		                lappend color $c
		            }
		            if { $color in "000000 FFFFFFFF" } { set color "" }
		        }
		    } elseif { $filetype eq "xlsx" && $style ne "" } {
		        set color ""
		        set xp [format {/*/*/e:cellXfs/e:xf[%d]} [expr {$style+1}]]
		        set xfNode [$colNode selectNodes -namespaces $ns $xp]
		        set xp [format {/*/*/e:cellStyleXfs/e:xf[%d]} [expr {[$xfNode @xfId]+1}]]
		        set xfcellStylNode [$colNode selectNodes -namespaces $ns $xp]
		        
		        lassign "" fontId fillId
		        if { [$xfcellStylNode @applyFont 1] == 1 } {
		            set fontId [$xfcellStylNode @fontId]
		        }
		        if { [$xfcellStylNode @applyFill 1] == 1 } {
		            set fillId [$xfcellStylNode @fillId]
		        }
		        if { [$xfNode @applyFont 1] == 1 } {
		            set fontId [$xfNode @fontId]
		        }
		        if { [$xfNode @applyFill 1] == 1 } {
		            set fillId [$xfNode @fillId]
		        }
		        if { $fontId ne "" } {
		            set xp [format {/*/*/e:fonts/e:font[%d]} [expr {$fontId+1}]]
		            set fontNode [$colNode selectNodes -namespaces $ns $xp]
		            set xp {string(e:color/@rgb)}
		            set c [$fontNode selectNodes -namespaces $ns $xp]
		            if { $c ne "" } { lappend color $c }
		            set xp {string(e:color/@indexed)}
		            set c [$fontNode selectNodes -namespaces $ns $xp]
		            if { $c ne "" && $c > 2 } { lappend color $c }
		        }
		        if { $fillId ne "" } {
		            set xp [format {/*/*/e:fills/e:fill[%d]} [expr {$fillId+1}]]
		            set fillNode [$colNode selectNodes -namespaces $ns $xp]
		            set xp {string(e:patternFill/e:fgColor/@rgb)}
		            set c [$fillNode selectNodes -namespaces $ns $xp]
		            if { $c ne "" } { lappend color $c }
		            set xp {string(e:patternFill/e:fgColor/@indexed)}
		            set c [$fillNode selectNodes -namespaces $ns $xp]
		            if { $c ne "" && $c > 2 } { lappend color $c }
		            set xp {string(e:patternFill/e:bgColor/@rgb)}
		            set c [$fillNode selectNodes -namespaces $ns $xp]
		            if { $c ne "" } { lappend color $c }
		            set xp {string(e:patternFill/e:bgColor/@indexed)}
		            set c [$fillNode selectNodes -namespaces $ns $xp]
		            if { $c ne "" && $c > 2 } { lappend color $c }
		        }
		        if { $color in "000000 FFFFFFFF" } { set color "" }
		    }
		    set merge 0
		    if { $data ne "" || $color ne "" } {
		        if { $row > $maxRow } { set maxRow $row }
		        if { $col > $maxCol } { set maxCol $col }
		        
		        switch $reverse_rows_cols {
		            0 { set key $sname,$row,$col }
		            1 { set key $sname,$col,$row }
		        }
		        dict set vals $key type $type
		        dict set vals $key data $data
		        
		        set merge [$colNode @ss:MergeAcross 0]
		        
		        set n [_row_col_to_colname $row $col]
		        if { $merge_range_xp ne "NONE" } {
		            set xp [format_xpath $merge_range_xp "$n:"]
		            set merge_range [$sheetNode selectNodes -namespaces $ns $xp]
		        } else {
		            set merge_range ""
		        }
		        if { $merge_range ne "" } {
		            regexp {:([\w\d]+)$} $merge_range {} r2
		            lassign [_colname_to_row_col $r2] row2 col2
		            if { $row == $row2 } {
		                set merge [expr {$col2-$col}]
		            }
		        }
		        dict set vals $key merge $merge
		        dict set vals $key style $style
		        if { $color ne "" } {
		            dict set vals $key color $color
		        }
		    }
		    incr col [$colNode @table:number-columns-repeated 1]
		    incr col $merge
		}
		incr row [$rowNode @table:number-rows-repeated 1]
	    }
	    set numCnd 0
	    if { $form_type eq "formulae" } {
		foreach cndNode [$sheetNode selectNodes -namespaces $ns $CF_xp] {
		    set ranges [$cndNode selectNodes -namespaces $ns $CF_range_xp]
		    foreach "row col" [_range_to_row_col_list $ranges] {
		        set found 0
		        foreach c [$cndNode selectNodes -namespaces $ns $CF_condition_xp] {
		            switch -- [$c selectNodes -namespaces $ns $CF_condition_qualifier_xp] {
		                Greater - greaterThan { set sign > }
		                GreaterOrEqual - greaterThanOrEqual { set sign >= }
		                Less - lessThan { set sign < }
		                LessOrEqual - lessThanOrEqual { set sign <= }
		                Equal - equal { set sign == }
		                default { continue }
		            }
		            set is_red_color 0
		            
		            set style [$c selectNodes -namespaces $ns $CF_style]
		            set colorList ""
		            if { [string is digit -strict $style] } {
		                set xp "/*/e:styleSheet/e:dxfs/e:dxf\[[expr {$style+1}]\]"
		                set dxf [$doc selectNodes -namespaces $ns $xp]
		                if { [llength $dxf] == 1 } {
		                    foreach n [$dxf selectNodes -namespaces $ns .//e:color|.//e:bgColor] {
		                        lappend colorList [string range [$n @rgb] 2 7]
		                    }
		                }
		            } else {
		                foreach "- color" [regexp -inline -all {#(\w{6})} $style] {
		                    lappend colorList $color
		                }
		            }
		            if { [_is_red_color $colorList] } {
		                switch -- $sign {
		                    ">" { set sign "<=" }
		                    "<" { set sign ">=" }
		                    ">=" { set sign "<" }
		                    "<=" { set sign ">" }
		                    "==" { set sign "!=" }
		                }
		            }
		            set v [$c selectNodes -namespaces $ns $CF_value]
		            regsub -all {\$([A-Z]+)\$(\d+)(?![(\w])} $v {\1\2} v
		            set formula "R${row}C$col $sign $v"
		            set found 1
		            break
		        }
		        if { $found } {
		            switch $reverse_rows_cols {
		                0 { set key ${sname}_cnd,$row,$col }
		                1 { set key ${sname}_cnd,$col,$row }
		            }
		            dict set vals $key type condition
		            dict set vals $key data $formula
		            
		            incr numCnd
		        }
		    }
		}
		
		foreach vNode [$sheetNode selectNodes -namespaces $ns $validation_xp] {
		    if { [$vNode selectNodes -namespaces $ns $validation_type_xp] } { continue }
		    set vs [$vNode selectNodes -namespaces $ns $validation_values_xp]
		    set values ""
		    foreach i [split [string trim $vs \"] ","] {
		        lappend values [string trim $i]
		    }
		    if { $values eq "" } { continue }
		    
		    set ranges [$vNode selectNodes -namespaces $ns $CF_range_xp]
		    foreach "row col" [_range_to_row_col_list $ranges] {
		        switch $reverse_rows_cols {
		            0 { set key $sname,$row,$col }
		            1 { set key $sname,$col,$row }
		        }
		        if { [dict_getd $vals $key data ""] ne "" } {
		            dict set vals $key values $values
		        }
		    }
		}
	    }
	    incr sheetNum
	    lappend snameList $sname
	    if { $numCnd } {
		lappend snameList ${sname}_cnd
	    }
	}
	
################################################################################
#    conditions for ODS
################################################################################

	if { $form_type eq "formulae" && $filetype eq "ods" } {
	    foreach key [dict keys $vals] {
		set s [dict get $vals $key style]
		if { $s eq "" } { continue }
		regexp {(.*),(.*),(.*)} $key {} sname row col

		set found 0
		set xp [format_xpath {/*/office:automatic-styles/style:style[@style:name=%s]|} $s]
		append xp [format_xpath {/*/office:styles/style:style[@style:name=%s]} $s]
		foreach style [$doc selectNodes -namespaces $ns $xp] {
		    foreach map [$style selectNodes -namespaces $ns style:map] {
		        set formula [$map @style:condition ""]
		        set apply_style [$map @style:apply-style-name ""]
		        if { $formula eq "" || $apply_style eq "" } { continue }

		        regsub -all {\[([^.]+?)\.\$([A-Z]+?)\$(\d+?)\]} $formula {\1!\2\3} formula
		        regsub -all {\[\.([A-Z]+\d+)\]} $formula {\1} formula
		        set curr "$sname![_row_col_to_colname $row $col]"
		        set formula [string map [list cell-content() $curr] $formula]
		        
		        set xpi [format_xpath {/*/office:automatic-styles/style:style[@style:name=%s]|} $apply_style]
		        append xpi [format_xpath {/*/office:styles/style:style[@style:name=%s]} $apply_style]
		        set style_i [$doc selectNodes -namespaces $ns $xpi]
		        if { [llength $style_i] == 1 } {
		            set colorList ""
		            foreach i [$style_i selectNodes {.//*/@fo:background-color|.//*/@fo:color}] {
		                lappend colorList [string range [lindex $i 1] 1 end]
		            }
		            if { [_is_red_color $colorList] } {
		                set map [list > <= < >= >= < <= > == != != ==]
		                set formula [string map $map $formula]
		            }
		        }
		        set found 1
		        break
		    }
		    if { $found } { break }
		}
		if { $found } {
		    set key ${sname}_cnd,$row,$col
		    dict set vals $key type condition
		    dict set vals $key data $formula
		    dict set vals $key style ""
		    if { [lsearch -exact $snameList ${sname}_cnd] == -1 } {
		        lappend snameList ${sname}_cnd
		    }
		}
	    }
	}

################################################################################
#    reversing
################################################################################

	# does not reverse any more
	if { $reverse_rows_cols } {
	    lassign [list $maxCol $maxRow] maxRow maxCol
	}

	    
################################################################################
#    Stadistics to choose best name options
################################################################################
	   
	if { $form_type eq "formulae" && $variable_names_where in "auto color" } {
	    lassign [list 0 0 0 0 ""] numFields numLeftGood numAboveGood numColorGood colorsDict

	    dict for "n v" $vals {
		regexp {(.*),(.*),(.*)} $n {} sname row col
		set type [dict get $v type]
		
		if { [set color [dict_getd $v color ""]] ne "" } {
		    incr numColorGood
		    dict incr colorsDict $color
		    if { $type ni "Number Date formula condition" } { incr numFields }
		}
		if { $type ni "Number Date formula condition" } { continue }
		
		for { set i 1 } { $i <= 3 } { incr i } {
		    set key_m1 $sname,$row,[expr {$col-$i}]
		    #set merge [dict_getd $vals $key_m1 merge 0]
		    #if { $i != $merge+1 } { continue }
		    if { [dict_getd $vals $key_m1 type ""] eq "String" } {
		        set name [string trim [dict get $vals $key_m1 data]]
		        if { $name ne "" } {
		            incr numLeftGood
		            break
		        }
		    }
		}
		set key_m1 $sname,[expr {$row-1}],$col
		if { [dict_getd $vals $key_m1 type ""] eq "String" } {
		    set name [string trim [dict get $vals $key_m1 data]]
		    if { $name ne "" } { incr numAboveGood }
		}
		incr numFields
	    }
	    
	    if { $variable_names_where eq "auto" } {
		if { $numColorGood >= $numLeftGood } {
		    if { $numColorGood > $numAboveGood } {
		        set variable_names_where color
		    } else {
		        set variable_names_where above
		    }
		} else {
		    if { $numLeftGood >= $numAboveGood } {
		        set variable_names_where left
		    } else {
		        set variable_names_where above
		    }
		}
	    }
	    if { $variable_names_where eq "color" } {
		set valuesColorsList ""
		dict for "n v" $colorsDict {
		    if { $v >= 2 } {
		        lappend valuesColorsList $n
		    }
		}
		set valuesColorsList [lsort $valuesColorsList]
	    }
	}
	
################################################################################
#    Creating new XML document
################################################################################

	set new_doc [dom parse "<formulae/>"]
	set new_root [$new_doc documentElement]

	if { !$delete_existing_fields } {
	    set formulaeNode $options(-formulae_node)
	    
	    foreach node [$formulaeNode selectNodes container] {
		$new_root appendChild [$node cloneNode -deep]
	    }
	}
    
################################################################################
#    void values referenced by a formula become 0.0
################################################################################
	 
	if { $form_type eq "formulae" } {            
	   dict for "n v" $vals {
		if { [dict get $v type] in "formula condition" } {
		    regexp {(.*),(.*),(.*)} $n {} sname row col
		    set data [dict get $v data]
		    
		    foreach i [_parse_formula_refs $data $sname $row $col $reverse_rows_cols] {
		        lassign $i s r c
		        if { ![dict exists $vals $s,$r,$c] } {
		            dict set vals $s,$r,$c type Number
		            dict set vals $s,$r,$c data 0.0
		        }
		    }
		}
	    }
################################################################################
#    Creating list of fields
################################################################################

	    set fieldsList ""
	    foreach sname $snameList {
		for { set row 1 } { $row <= $maxRow } { incr row } {
		    for { set col 1 } { $col <= $maxCol } { incr col } {
		        if { ![dict exists $vals $sname,$row,$col] } { continue }
		        set type [dict get $vals $sname,$row,$col type]
		        set to $type
		        if { $variable_names_where eq "color" } {
		            if { [dict_getd $vals $sname,$row,$col color ""] in $valuesColorsList } {
		                set to color
		            }
		        }
		        if { $to ni "Number Date formula condition color" } { continue }
		        
		        set data [dict get $vals $sname,$row,$col data]
		        
		        switch $reverse_rows_cols {
		            0 { set bname [_row_col_to_colname $row $col] }
		            1 { set bname [_row_col_to_colname $col $row] }
		        }
		        lassign "" nameL nameA colorA colorL
		        for { set i 1 } { $i <= 2 } { incr i } {
		            set key_cm1 $sname,$row,[expr {$col-$i}]
		            #set merge [dict_getd $vals $key_cm1 merge 0]
		            set key_rm1 $sname,[expr {$row-$i}],$col

		            if { $i == 1 } {
		                if { [dict_getd $vals $key_rm1 type ""] eq "String" } {
		                    set nameA [string trim [dict get $vals $key_rm1 data]]
		                    set colorA [dict_getd $vals $key_rm1 color ""]
		                }
		            }
		            #if { $i != $merge+1 } { continue }
		            if { [dict_getd $vals $key_cm1 type ""] eq "String" } {
		                set nameL [string trim [dict get $vals $key_cm1 data]]
		                if { $nameL ne "" } {
		                    set colorL [dict_getd $vals $key_cm1 color ""]
		                    break
		                }
		            }
		        }
		        switch $variable_names_where {
		            left {
		                set name $nameL
		                set key_p1 $sname,$row,[expr {$col+1}]
		                set key_p2 $sname,$row,[expr {$col+2}]
		            }
		            above {
		                set name $nameA
		                set key_m1 $sname,[expr {$row-1}],$col
		                lassign "" key_p1 key_p2
		            }
		            color {
		                lassign [list 0 0] scoreL scoreA
		                if { $colorL ni $valuesColorsList } { incr scoreL 2 }
		                if { $colorA ni $valuesColorsList } { incr scoreA 2 }
		                if { $nameL ne "" && ![string is double $nameL] } { incr scoreL 1 }
		                if { $nameA ne "" && ![string is double $nameA] } { incr scoreA 1 }
		                if { $scoreA > $scoreL } {
		                    set name $nameA
		                    lassign "" key_p1 key_p2
		                } else {
		                    set name $nameL
		                    set key_p1 $sname,$row,[expr {$col+1}]
		                    set key_p2 $sname,$row,[expr {$col+2}]
		                }
		            }
		        }
		        lassign "" units help
		        set type_p1 [dict_getd $vals $key_p1 type ""]
		        set type_p2 [dict_getd $vals $key_p2 type ""]
		        
		        if { $type_p1 eq "String" && $type_p2 eq "" } {
		            set txt [dict_getd $vals $key_p1 data ""]
		            set openpar "\[("
		            set closepar "\])"
		            set special "\u0b9\u0b2\u0b3\u2074\ub7/*·^"
		            set rex1 [format {^(.*)([%s][^%s]+[%s])(.*)$} $openpar $closepar $closepar]
		            set rex2 [format {^(\w{1,3}|\S+[%s]\S*)$} $special]
		            if { [regexp $rex1 $txt {} h1 units h2] } {
		                set help "$h1$h2"
		                set units [string trim $units "\[\]()"]
		            } elseif { [regexp $rex2 $txt] } {
		                set units $txt
		            } else {
		                set help $txt
		            }
		            set map [list 2 \u0b2 3 \u0b3 4 \u2074]
		            set units [string map $map $units]
		        }
		        lassign [_generate_name $name $sname $bname $fieldsList 0] name pname
		        lappend fieldsList [list $name $pname $sname $row $col $type $data $units $help]
		        dict set vals $sname,$row,$col name $name
		    }
		}
	    }
################################################################################
#   All formulae need to reference fields positioned before the formula
################################################################################

	    set len [llength $fieldsList]
	    for { set try 0 } { $try < 20 } { incr try } {
		set needs_recalculate 0
		for { set i 0 } { $i < $len } { incr i } {
		    lassign [lindex $fieldsList $i] name pname sname row col type data - -
		    if { $type ni "formula condition" } { continue }
		    
		    foreach j [_parse_formula_refs $data $sname $row $col $reverse_rows_cols] {
		        lassign $j s r c
		        set name [dict_getd $vals $s,$r,$c name ""]
		        set ipos [lsearch -exact -index 0 $fieldsList $name]
		        if { $ipos > $i } {
		            set elm [lindex $fieldsList $ipos]
		            set fieldsList [lreplace $fieldsList $ipos $ipos]
		            set fieldsList [linsert $fieldsList $i $elm]
		            if { [lindex $elm 5] in "formula condition" } {
		                incr needs_recalculate
		            }
		            incr i
		        }
		    }
		}
		if { !$needs_recalculate } { break }
	    }

################################################################################
#    Tranforming formula into variable names
################################################################################

	    for { set i 0 } { $i < $len } { incr i } {
		lassign [lindex $fieldsList $i] name pname sname row col type data - -
		if { $type ni "formula condition" } { continue }
		set new_formula ""
		set last_idx 0
		foreach j [_parse_formula_refs $data $sname $row $col $reverse_rows_cols] {
		    lassign $j s r c full0 full1 text_after
		    set name [dict_getd $vals $s,$r,$c name "#REF!"]
		    set txt [string range $data $last_idx $full0-1]

		    set cmd "string tolower \$x"
		    set txt [regmap {\m[[:alpha:]][\w!]*(?=\()} $txt x $cmd]

		    append new_formula $txt $name $text_after
		    set last_idx [expr {$full1+1}]
		}
		append new_formula [string range $data $last_idx end]
		regsub -all {([^=<>])=([^=])} $new_formula {\1==\2} new_formula
		lset fieldsList $i 6 $new_formula
	    }
		        
################################################################################
#    creating the XML (for formulae)
################################################################################

	    set min_num_fields 3
	    set max_num_fields 8
	    lassign [list "" "" 0 0 0] groupList current_group numGroup numParam numParamTotal
	    for { set i 0 } { $i < $len } { incr i } {
		lassign [lindex $fieldsList $i] name pname sname row col type data units help
		
		if { $current_group ne "" } {
		    if { $numParam >= $max_num_fields } {
		        set current_group ""
		        set numParam 0
		    } elseif { $numParam >= $min_num_fields } {
		        set merge [dict_getd $vals $lastSname,$lastRow,$lastCol merge 0]
		        if { $sname eq $lastSname && $col == $lastCol && $row == $lastRow+1 } {
		            set isgood 1
		        } elseif { $sname eq $lastSname && $row == $lastRow && $col == $lastCol+$merge+1 } {
		            set isgood 1
		        } else {
		            set isgood 0
		        }
		        if { !$isgood } {
		            set current_group ""
		            set numParam 0
		        }
		    }
		}
		if { $current_group eq "" } {
		    lassign [list $row $col] r c
		    while 1 {
		        lassign [list [expr {$r-1}] [expr {$c-1}]] rm1 cm1
		        if { [dict_getd $vals $sname,$r,$cm1 type ""] ne "" } {
		            set c $cm1
		        } elseif { [dict_getd $vals $sname,$rm1,$c type ""] ne "" } {
		            set r $rm1
		        } elseif { [dict_getd $vals $sname,$rm1,$cm1 type ""] ne "" } {
		            lassign [list $rm1 $cm1] r c
		        } elseif { [dict_getd $vals $sname,[expr {$r-2}],$c type ""] eq "String" &&
		            [dict_getd $vals $sname,[expr {$r-2}],$c data ""] ne "" &&
		            [dict_getd $vals $sname,[expr {$r-3}],$c type ""] eq "" } {
		            set current_group [dict get $vals $sname,[expr {$r-2}],$c data]
		            break
		        } elseif { [dict_getd $vals $sname,$r,$c type ""] eq "String" &&
		            [dict_getd $vals $sname,$r,$c data ""] ne "" } {
		            set current_group [dict get $vals $sname,$r,$c data]
		            break
		        } else {
		            break
		        }
		    }                    
		    set current_group [string trimright $current_group ":"]
		    incr numGroup
		    if { $current_group eq "" } {
		        set current_group [_ "Group %d" $numGroup]
		    }
		    while { $current_group in $groupList } {
		        if { ![regexp {^(.*[^\d\s])\s*(\d+)$} $current_group {} base num] } {
		            set num 2
		        } else {
		            incr num
		            set current_group [string trimright $base]
		        }
		        append current_group " $num"
		    }
		    lappend groupList $current_group
		    set containerNode [$new_root appendChildTag container \
		            [list attributes() n ${current_group}_container pn $current_group]]
		}
		switch $type {
		    Number { set field_type numeric }
		    Date { set field_type date }
		    formula { set field_type expression }
		    condition { set field_type "" }
		    default { set field_type "" }
		}
		if { $type in "formula condition" } {
		    set data "\[[list e $data]\]"
		}
		if { $type ne "condition" } {
		    set nodeName param
		} else {
		    set nodeName condition
		}
		set node [$containerNode appendChildTag $nodeName [list attributes() \
		            n $name pn $pname field_type $field_type value $data \
		            units $units help $help]]

		if { [dict_getd $vals $sname,$row,$col values ""] ne "" } {
		    $node setAttribute field_type "options non-editable" values \
		        [join [dict_getd $vals $sname,$row,$col values ""] ","]
		}
		incr numParam
		incr numParamTotal
		lassign [list $sname $row $col] lastSname lastRow lastCol
	    }

################################################################################
#    correcting pages in the XML
################################################################################

	    set min_num_fields_in_page 10
	    set num_fields_in_page 0
	    foreach containerNode [$new_root selectNodes container] {
		set num [$containerNode selectNodes count(param|condition)]
		if { $num_fields_in_page > 0 } {
		    if { $num_fields_in_page + $num <= $min_num_fields_in_page } {
		        $containerNode setAttribute location same_page
		    } else {
		        set num_fields_in_page 0
		    }
		}
		incr num_fields_in_page $num
	    }
	} else {
################################################################################
#    finding biggest block for database
################################################################################

	    # block is "r0 c0 sname delta_r delta_c"
	    set maxblock [list 0 0 "" 0 0]
	    foreach sname $snameList {
		for { set row 1 } { $row <= $maxRow } { incr row } {
		    for { set col 1 } { $col <= $maxCol } { incr col } {
		        if { ![dict exists $vals $sname,$row,$col] } { continue }
		        if { [dict exists $vals $sname,[expr {$row-1}],$col] } { continue }
		        if { [dict exists $vals $sname,$row,[expr {$col-1}]] } { continue }
		        
		        set delta_c ""
		        
		        for { set row_b $row } { $row_b <= $maxRow } { incr row_b } {
		            for { set col_b $col } { $col_b <= $maxCol } { incr col_b } {
		                if { ![dict exists $vals $sname,$row_b,$col_b] } { break }
		            }
		            if { $delta_c ne "" && $col_b-$col != $delta_c } { break }
		            set delta_c [expr {$col_b-$col}]
		        }
		        set delta_r [expr {$row_b-$row}]
		        foreach "- - - dr dc" $maxblock break
		        if { $delta_r*$delta_c > $dr*$dc } {
		            set maxblock [list $row $col $sname $delta_r $delta_c]
		        }
		    }
		}
	    }
	    
################################################################################
#    creating XML for database
################################################################################

	    foreach "r0 c0 sname delta_r delta_c" $maxblock break
	    
	    if { $delta_r } {
		set row_m1 [expr {$r0-1}]
		set col_m1 [expr {$c0-1}]
		set current_group ""
		if { [dict_getd $vals $sname,$row_m1,$c0 type ""] ne "" } {
		    if { [dict_getd $vals $sname,$row_m1,$c0 type ""] eq "String" } {
		        set current_group [dict get $vals $sname,$row_m1,$c0 data]
		    }
		} elseif { [dict_getd $vals $sname,$row_m1,$col_m1 type ""] ne "" } {
		    if { [dict_getd $vals $sname,$row_m1,$col_m1 type ""] eq "String" } {
		        set current_group [dict get $vals $sname,$row_m1,$col_m1 data]
		    }
		}
		if { $current_group eq "" } {
		    set current_group [_ "Group %d" 1]
		}
		set containerNode [$new_root appendChildTag container \
		        [list attributes() n $current_group]]
		
		set namesList ""
		for { set col $c0 } { $col < $c0+$delta_c } { incr col } {
		    set name [dict get $vals $sname,$r0,$col data]
		    
		    switch $reverse_rows_cols {
		        0 { set bname [_row_col_to_colname $r0 $col] }
		        1 { set bname [_row_col_to_colname $col $r0] }
		    }
		    lassign [_generate_name $name $sname $bname $namesList ""] name pname
		    lappend namesList $name
		    
		    $containerNode appendChildTag param [list attributes() \
		            n $name pn $pname field_type text value ""]
		}
		set dNode [$new_root appendChildTag database]
		set now [date_tcl2sql [clock seconds]]
		set id 1
		for { set row [expr {$r0+1}] } { $row < $r0+$delta_r } { incr row } {
		    set rNode [$dNode appendChildTag row]
		    $rNode appendChildTag entry [list text() $id]
		    $rNode appendChildTag entry [list text() $now]
		    $rNode appendChildTag entry [list text() $now]
		    for { set col $c0 } { $col < $c0+$delta_c } { incr col } {
		        set data [dict get $vals $sname,$row,$col data]
		        $rNode appendChildTag entry [list text() $data]
		    }
		    incr id
		}
	    }
	}
	$doc delete
	return $new_doc
    } 
}

proc formulae::selectdir {args} {     

    set optional {
	{ -initialdir initialdir "" }
	{ -parent parent "" }     
    }
    set compulsory ""
    parse_args $optional $compulsory $args
	
    set dir [tk_chooseDirectory -initialdir $initialdir \
	    -parent $parent -mustexist 1]
    
    return $dir      
}

if { 0 } {
    set ndoc [formulae::create_report_from_file ~/../Escritorio/placa_anclaje.xml]
    formulae::print_report $ndoc placa_anclaje.pdf pdf
    exit
}


















