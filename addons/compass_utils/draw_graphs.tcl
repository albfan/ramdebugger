package require snit
package require compass_utils
package require dialogwin
package require xml2pdf
package require tooltip
package require msgcat

namespace import -force tooltip::tooltip 

namespace eval cu {
    variable topdir [file dirname [info script]]
    variable topdir0 [file normalize $topdir/..]

    cu::get_images_add_dir [file join $topdir0 compass_utils images]
}


# for tclIndex to work 
proc cu::draw_graphs { args } {}


snit::widgetadaptor cu::draw_graphs {
    option -xlabel ""
    option -ylabel ""
    option -title ""
    option -xyvalues
    option -font "Helvetica 8"
    option -fonttitle "Helvetica 6"
    option -includeyzero 1
    option -functionnames "Function" 
    option -showlegend 0
    option -showtitle 1
    option -scatterplot 0
    option -multiplegraphs 0   
    option -backgroundcolor white  
    option -foregroundcolor black
    option -xlabelunit ""
    option -ylabelunit ""
    option -colorset [list blueviolet aquamarine3 coral2 darkblue darkred darksalmon \
	    chocolate4 grey DarkGreen salmon RoyalBlue "orange red" coral DarkKhaki \
	    ivory2 gold maroon "sky blue"]     
    option -canvaswidthset ""
    option -canvasheightset ""
    option -xaxismax "-1e10 -1e10"
    option -xaxismin "1e10 1e10"
    option -yaxismaxleft "-1e10 -1e10"
    option -yaxisminleft "1e10 1e10"
    option -yaxismaxright "-1e10 -1e10"
    option -yaxisminright "1e10 1e10"
    option -xyaxisauto 1
    option -xymovement "0.0 0.0" 
    option -svgfile ""  
    option -addgridlines 0
    option -gridlinescolor "grey"
	
    variable xfact ""
    variable xm ""
    variable yfactleft ""
    variable yfactright ""
    variable ym ""
    variable ymax ""          
    
    variable fg  
    variable defaultdir $::env(HOME) 
    variable yaxis 
    variable idleid 
    variable xml
    
    delegate method _insert to hull as insert
    delegate method _delete to hull as delete
    delegate method _mark to hull as mark
    delegate method * to hull
    delegate option * to hull
      
    constructor args {
	canvas $self -background white -width 400 -height 350 \
	    -bd 0 -highlightthickness 0 -relief raised                 
	
	installhull $self      
	
	option add *tearOff 0 widgetDefault
	option add *borderWidth 1 widgetDefault
	option add *Frame.borderWidth 0 widgetDefault
	option add *Toplevel.borderWidth 0 widgetDefault            
	 
	$self configurelist $args        
	
	focus $win         
	bind $win <ButtonPress-3> [mymethod contextual_submenu %X %Y]
	bind $win <Control-c> [mymethod clipboard_copy]
	bind $win <Configure> [mymethod draw]            
    }
    onconfigure -xyvalues { value } {
	set options(-xyvalues) $value
	after idle [mymethod draw]
    }   
    onconfigure -xlabel { value } {
	set options(-xlabel) $value
	after idle [mymethod draw]
    }  
    onconfigure -ylabel { value } {
	set options(-ylabel) $value
	after idle [mymethod draw]
    }  
    onconfigure -canvaswidthset { value } {
	set options(-canvaswidthset) $value
	after idle [mymethod draw]
    } 
    onconfigure -canvasheightset { value } {
	set options(-canvasheightset) $value
	after idle [mymethod draw]
    } 
    onconfigure -functionnames { value } {
	set options(-functionnames) $value       
    }   
    onconfigure -multiplegraphs { value } {
	set options(-multiplegraphs) $value      
    }
    onconfigure -title { value } {
	set options(-title) $value     
    }
    onconfigure -xlabelunit { value } {
	set options(-xlabelunit) $value
    }
    onconfigure -ylabelunit { value } {
	set options(-ylabelunit) $value
    }
    onconfigure -xaxismax { value } {
	set options(-xaxismax) [lreplace $options(-xaxismax) 1 1 $value]
    }
    onconfigure -xaxismin { value } {
	set options(-xaxismin) [lreplace $options(-xaxismin) 1 1 $value]
    }
    onconfigure -yaxismaxleft { value } {
	set options(-yaxismaxleft) [lreplace $options(-yaxismaxleft) 1 1 $value]
    }
    onconfigure -yaxismaxright { value } {
	set options(-yaxismaxright) [lreplace $options(-yaxismaxright) 1 1 $value]
    }
    onconfigure -yaxisminleft { value } {
	set options(-yaxisminleft) [lreplace $options(-yaxisminleft) 1 1 $value]
    }
    onconfigure -yaxisminright { value } {
	set options(-yaxisminright) [lreplace $options(-yaxisminright) 1 1 $value]
    }
    onconfigure -xyaxisauto { value } {
	set options(-xyaxisauto) $value 
	set options(-xaxismax) [lreplace $options(-xaxismax) 0 0 -1e10]
	set options(-xaxismin) [lreplace $options(-xaxismin) 0 0 1e10]
	set options(-yaxismaxleft) [lreplace $options(-yaxismaxleft) 0 0 -1e10]
	set options(-yaxismaxright) [lreplace $options(-yaxismaxright) 0 0 -1e10]
	set options(-yaxisminleft) [lreplace $options(-yaxisminleft) 0 0 1e10]
	set options(-yaxisminright) [lreplace $options(-yaxisminright) 0 0 1e10]           
    }  
    onconfigure -backgroundcolor { value } {
	set options(-backgroundcolor) $value
    }
    onconfigure -foregroundcolor { value } {
	set options(-foregroundcolor) $value
    }
    onconfigure -addgridlines { value } {
	set options(-addgridlines) $value
    }
    onconfigure -gridlinescolor { value } {
	set options(-gridlinescolor) $value
    }
    method contextual_submenu { x y } {    
	destroy $win.menu    
	
	menu $win.menu     
		    
	$win.menu add check -label [_ "Scatter plot"] -compound left -image [cu::get_image viewmulticolumn16] \
	    -command [mymethod drawingoptions scatterplot] -variable options(-scatterplot)
	
	$win.menu add separator 
	
	$win.menu add command -label [_ "Edit axis"] -compound left -image [cu::get_image oscilloscope-16] \
	    -command [mymethod edit_axis] 
	
	$win.menu add separator 
	
	$win.menu add cascade -label [_ "Displacement"] -compound left \
	    -image [cu::get_image move-16] -menu $win.menu.movement -underline 0 
		
	menu $win.menu.movement -tearoff 0    
	     
	$win.menu.movement add command -label [_ "Vector"] -compound left \
	    -image [cu::get_image move-16] -command [mymethod displacement]  
	
	$win.menu.movement add cascade -label [_ "Functions"] -compound left \
	    -image [cu::get_image funct-16] -menu $win.menu.movement.functions -underline 0                 
	menu $win.menu.movement.functions -tearoff 0             
	foreach function $options(-functionnames) {
	    if {![info exists options(-move$function)]} {
		set options(-move$function) 0
	    }              
	    $win.menu.movement.functions add check -label "$function" \
		-compound left -command [mymethod movingoptions $function] \
		-variable options(-move$function)                                            
	}  
	##################
	$win.menu add cascade -label [_ "Area"] -compound left \
	    -image [cu::get_image int-16] -menu $win.menu.area -underline 0 
		
	menu $win.menu.area -tearoff 0    
	     
	$win.menu.area add command -label [_ "Area under a function"] -compound left \
	    -image [cu::get_image int-16] -command [mymethod area areaunderfunct]  
	
	if {$options(-multiplegraphs)} {            
	    $win.menu.area add command -label [_ "Area between two functions"] -compound left \
		-image [cu::get_image int-16] -command [mymethod area areabetweenfunct] 
	    $win.menu.area add command -label [_ "Area of the region enclosed by two functions"] \
		-compound left -image [cu::get_image int-16] -command [mymethod area areaenclosedregion] 
	}                    
	
	$win.menu add separator 
	
	$win.menu add command -label [_ "Copy"] -compound left -image [cu::get_image editcopy16] \
	    -command [mymethod clipboard_copy] -acc "Ctrl-c"   
	
	$win.menu add cascade -label [_ "Import from spreadsheet"] -compound left \
	    -image [cu::get_image spreadsheet-16] -menu $win.menu.import -underline 0 

	menu $win.menu.import -tearoff 0 
       
	$win.menu.import add command -label [_ "Microsoft Excel (.xml)(*.xml)"] -compound left \
	    -image [cu::get_image spreadsheet-16] -command [mymethod import_from_excel]
      
	$win.menu.import add command -label [_ "Text (.csv)(*.csv)"] -compound left \
	    -image [cu::get_image spreadsheet-16] -command [mymethod import_from_csv]
	
	$win.menu add cascade -label [_ "Export to spreadsheet"] -compound left \
	    -image [cu::get_image spreadsheet-16] -menu $win.menu.export -underline 0 

	menu $win.menu.export -tearoff 0 
       
	$win.menu.export add command -label [_ "Microsoft Excel (.xml)(*.xml)"] -compound left \
	    -image [cu::get_image spreadsheet-16] -command [mymethod export_to_excel]
      
	$win.menu.export add command -label [_ "Text (.csv)(*.csv)"] -compound left \
	    -image [cu::get_image spreadsheet-16] -command [mymethod export_to_csv]
		
	$win.menu add separator
	
	$win.menu add command -label [_ "Save as SVG"] -compound left -image [cu::get_image editcopy16] \
	    -command [mymethod save_as_svg] 

	$win.menu add separator  
	
	$win.menu add cascade -label [_ "Hide"] -compound left \
	    -image [cu::get_image funct-16] -menu $win.menu.hide -underline 0  
	
	menu $win.menu.hide -tearoff 0 
	
	if {$options(-multiplegraphs)} {
	    $win.menu.hide add check -label [_ "Legend"] -compound left -image [cu::get_image view_sidetree-16] \
		-command [mymethod drawingoptions showlegend] -variable options(-showlegend)                                 
	}
		   
	$win.menu.hide add check -label [_ "Title"] -compound left -image [cu::get_image text_top-16] \
	    -command [mymethod drawingoptions showtitle] -variable options(-showtitle) 

	if {$options(-multiplegraphs)} {
	    $win.menu.hide add cascade -label [_ "Functions"] -compound left \
		-image [cu::get_image funct-16] -menu $win.menu.hide.functions -underline 0                 
	    menu $win.menu.hide.functions -tearoff 0             
	    foreach function $options(-functionnames) {               
		if {![info exists options(-$function)]} {
		    set options(-$function) 0
		}              
		$win.menu.hide.functions add check -label "$function" \
		    -compound left -command [mymethod drawingoptions $function] \
		    -variable options(-$function)                               
	    }     
	}  

	$win.menu add separator 
	
	$win.menu add command -label [_ "GUI preferences"] -compound left \
	    -image [cu::get_image colors-16] -command [mymethod colorchoose]         
	
	tk_popup $win.menu $x $y            
    }     
    method svg {} {
	variable xml
	
	set svg [linsert $xml 0 "<mediaobject><imageobject>"]
	append svg "</imageobject></mediaobject>"  
	
	return $svg
    }
    method save_as_svg {} {
	variable xml
	
	if {$options(-svgfile) == ""}  {     
	    set options(-svgfile) [tk_getSaveFile -parent $self -defaultextension .svg \
		    -filetypes [list [list [_ "SVG files"] {.svg}] \
		        [list [_ "All files"] *]]]
	}
	if { $options(-svgfile) eq "" } { return }        
	set err [catch {set fout [open $options(-svgfile) w]} errstring]
	if { $err } {
	    snit_messageBox -message $::errorInfo
	    return
	}        
	fconfigure $fout -encoding utf-8  
	puts $fout $xml
	close $fout                
    }       
    method area { typearea } { 
	if {$typearea eq "areaunderfunct"} {
	    set title [_ "Area between a function and the x-axis"]
	    set tooltiptext [_ "Area of the region between the x axis, the fat lines labeled a and b, and the graph."]
	    set area [image create photo area -file [file join $::topdir images area_under_function.png]]
	} elseif {$typearea == "areabetweenfunct"} {
	    set title [_ "Area between two functions"] 
	    set tooltiptext [_ "Area of the region between the x axis, the fat lines labeled a and b, and the graph."]
	    set area [image create photo area -file [file join $::topdir images area_between_functions.png]]
	} elseif {$typearea == "areaenclosedregion"} {
	    set title [_ "Area of the region enclosed by two functions"]
	    set tooltiptext [_ "The limits of integration will be the intersection points of the two curves."]
	    set area [image create photo area -file [file join $::topdir images area_between_functions.png]] 
	}
	
	set w .area
	catch {destroy $w}
	set w [dialogwin_snit $w -title $title -callback [mymethod area_do] \
		-grab 1 -transient 1 -okname [_ "Close"] -cancelname -]
	set f [$w giveframe]                                                                                                                         
	
	set farea [ttk::labelframe $f.farea -text $title \
		-relief groove -borderwidth 1] 
	grid $farea -sticky news -padx 2 -pady 2         
		
	label $farea.areac -image area 
	grid $farea.areac -sticky nsew -padx 2 -pady 2  
	grid configure $farea.areac -columnspan 2  
	
	if {$typearea != "areaenclosedregion"} {            
	    set farm [ttk::labelframe $f.farm -text [_ "Integration interval:"] \
		    -relief groove -borderwidth 1]            
	    grid $farm -sticky news -padx 2 -pady 2               
	    
	    ttk::label $farm.la -justify left -anchor n -text [_ "From (a):"]
	    entry $farm.ea -textvariable [$w give_uservar a] -width 40
	    tooltip::tooltip $farm.la [_ "Enter the start x-coordinate \nfor the range on the graph"] 
	    
	    ttk::label $farm.lb -justify left -anchor n -text [_ "To (b):"]
	    entry $farm.eb -textvariable [$w give_uservar b] -width 40
	    tooltip::tooltip $farm.lb [_ "Enter the end x-coordinate \nfor the range on the graph"] 
	    grid $farm.la $farm.ea -sticky ew -padx 2 -pady 2  
	    grid $farm.lb $farm.eb -sticky ew -padx 2 -pady 2     
	}         
	
	set funct [ttk::labelframe $f.funct -text [_ "Functions:"] \
		-relief groove -borderwidth 1]
	grid $funct -sticky news -padx 2 -pady 2       
	ttk::label $funct.fxl -justify left -anchor n -text [_ "f(x)"]
	cu::combobox $funct.fx -textvariable [$w give_uservar fx] \
	    -values $options(-functionnames) -width 20 -state readonly 

	tooltip::tooltip $funct.fxl [_ "Selected functions"]       
		
	if {$typearea eq "areabetweenfunct"} {
	    ttk::label $funct.gxl -justify left -anchor n -text [_ "g(x)"]
	    tooltip::tooltip $funct.gxl [_ "Selected functions"]      
	    cu::combobox $funct.gx -textvariable [$w give_uservar gx] \
		-values $options(-functionnames) -width 20 -state readonly 
	    set fcalc [ttk::labelframe $f.fcalc -text [_ "Area:"] \
		    -relief groove -borderwidth 1]            
	} elseif {$typearea eq "areaenclosedregion"} {
	    ttk::label $funct.gxl -justify left -anchor n -text [_ "g(x)"]
	    tooltip::tooltip $funct.gxl [_ "Selected functions"]     
	    cu::combobox $funct.gx -textvariable [$w give_uservar gx] \
		-values $options(-functionnames) -width 20 -state readonly 
	    set fcalc [ttk::labelframe $f.fcalc -text [_ "Area:"] \
		    -relief groove -borderwidth 1]
	} else {
	    set fcalc [ttk::labelframe $f.fcalc -text [_ "Area:"] \
		    -relief groove -borderwidth 1]
	}      
	grid $fcalc -sticky news -padx 2 -pady 2  
	
	ttk::label $fcalc.lcalc -text [_ "Area:"] -justify left -anchor n       
       
	entry $fcalc.earea -textvariable [$w give_uservar area] \
	    -width 40 -state disabled
	tooltip::tooltip $fcalc.lcalc [_ "Calculates the area under the graph of the \nselected functions using Simpson's formula."]                                    
	       
	##########        
	
	grid $funct.fxl $funct.fx -sticky ew -padx 2 -pady 2
	if {$typearea eq "areabetweenfunct" || $typearea eq "areaenclosedregion"} {
	    grid $funct.gxl $funct.gx -sticky ew -padx 2 -pady 2
	}
	ttk::label $fcalc.units -justify left -anchor n -text [_ "u\u00b2"] ; # square u
	
	grid $fcalc.lcalc $fcalc.earea $fcalc.units -sticky ew -padx 2 -pady 2                      
      
	if {$typearea != "areaenclosedregion"} {
	    $w set_uservar_value a [format %.5g [lindex $options(-xaxismin) 0]]        
	    $w set_uservar_value b [format %.5g [lindex $options(-xaxismax) 0]]                                                 
	}
	
	$w set_uservar_value fx [lindex $options(-functionnames) 0] 
	if {$typearea eq "areabetweenfunct" || $typearea eq "areaenclosedregion"} {
	    $w set_uservar_value gx [lindex $options(-functionnames) 1]  
	}
	if {$typearea eq "areaunderfunct" || $typearea eq "areabetweenfunct"} {
	    tk::TabToWindow $farm.ea
	}
	grid columnconfigure $f 0 -weight 1 
	if {$typearea != "areaenclosedregion"} {
	    grid columnconfigure $farea 1 -weight 1
	    grid columnconfigure $farm 1 -weight 1
	}
	grid columnconfigure $funct 1 -weight 1
	grid columnconfigure $fcalc 1 -weight 1
	
	set cmd_ab "[namespace code [mymethod update_area $w $typearea]];#"
	trace add variable [$w give_uservar a] write $cmd_ab
	bind $w <Destroy> +[list trace remove variable [$w give_uservar a] write $cmd_ab]   
		
	trace add variable [$w give_uservar b] write $cmd_ab
	bind $w <Destroy> +[list trace remove variable [$w give_uservar b] write $cmd_ab]  
	
	trace add variable [$w give_uservar fx] write $cmd_ab
	bind $w <Destroy> +[list trace remove variable [$w give_uservar fx] write $cmd_ab]  
	
	if {$typearea eq "areabetweenfunct" || $typearea eq "areaenclosedregion"} {
	    trace add variable [$w give_uservar gx] write $cmd_ab
	    bind $w <Destroy> +[list trace remove variable [$w give_uservar gx] write $cmd_ab]  
	}
	
	eval $cmd_ab
	
	bind $w <Return> [list $w invokeok]
    
	$w createwindow            
    } 
    method update_area { w typearea } {
	variable idleid
	
	if { [info exists idleid] } {
	    after cancel $idleid
	    unset idleid
	}
	if {$typearea == "areabetweenfunct"} {
	    set idleid [after 400 [mymethod area_between_functions $w]] 
	} elseif {$typearea == "areaenclosedregion"} {
	    set idleid [after 400 [mymethod area_enclosed_region $w]]            
	} else {
	    set idleid [after 400 [mymethod area_under_function $w]]   
	}     
    }
    method area_under_function { w } {
	if {[$w give_uservar_value b] < [$w give_uservar_value a]} {
	    snit_messageBox -message [_ "Incorrect integration interval."] 
	    return            
	}
	lassign [$self calc_area_interval [$w give_uservar_value fx] \
		[$w give_uservar_value a] [$w give_uservar_value b]] area
	$w set_uservar_value area [format %.5g $area]                  
    }
    method area_between_functions { w } {       
	if {[$w give_uservar_value b] < [$w give_uservar_value a]} {
	    snit_messageBox -message [_ "Incorrect integration interval."] 
	    return            
	}
	if {[$w give_uservar_value fx] == [$w give_uservar_value gx]} {
	    snit_messageBox -message [_ "Function f(x) should be different than g(x)."] 
	    return            
	}      
	lassign [$self calc_area_interval [$w give_uservar_value fx] \
		[$w give_uservar_value a] [$w give_uservar_value b]] areaf
	lassign [$self calc_area_interval [$w give_uservar_value gx] \
		[$w give_uservar_value a] [$w give_uservar_value b]] areag      
	$w set_uservar_value area [format %.5g [expr {abs($areaf) - abs($areag)}]]             
    }
    method area_enclosed_region { w } {       
	if {[$w give_uservar_value fx] == [$w give_uservar_value gx]} {
	    snit_messageBox -message [_ "Function f(x) should be different than g(x)."] 
	    return            
	}
	# First intersection between functions should be calculated        
	set interpntList ""
	# Loop in f(x) segments
	for {set i 0} {$i < [llength $options(-functionnames)]} {incr i} {
	    if {[lindex $options(-functionnames) $i] != [$w give_uservar_value fx]} { continue }                        
	    if {$options(-multiplegraphs)} {
		set numdivisions [llength [lindex $options(-xyvalues) $i]]
	    } else {
		set numdivisions [llength $options(-xyvalues)]
	    }
	    for {set j 0} {$j < [expr {$numdivisions-1}]} {incr j} {               
		if {$options(-multiplegraphs)} {
		    lassign [lindex [lindex $options(-xyvalues) $i] $j] xvalf yvalf                    
		    lassign [lindex [lindex $options(-xyvalues) $i] [expr {$j+1}]] xnextvalf ynextvalf
		} else {
		    lassign [lindex $options(-xyvalues) $j] xvalf yvalf  
		    lassign [lindex $options(-xyvalues) [expr {$j+1}]] xnextvalf ynextvalf              
		}  
		# Loop in g(x) segments
		for {set k 0} {$k < [llength $options(-functionnames)]} {incr k} {
		    if {[lindex $options(-functionnames) $k] != [$w give_uservar_value gx]} { continue }                        
		    if {$options(-multiplegraphs)} {
		        set numdivisionsg [llength [lindex $options(-xyvalues) $k]]
		    } else {
		        set numdivisionsg [llength $options(-xyvalues)]
		    }               
		    for {set kk 0} {$kk < [expr {$numdivisionsg-1}]} {incr kk} {               
		        if {$options(-multiplegraphs)} {
		            lassign [lindex [lindex $options(-xyvalues) $k] $kk] xvalg yvalg  
		            lassign [lindex [lindex $options(-xyvalues) $k] [expr {$kk+1}]] xnextvalg ynextvalg                                
		        } else {
		            lassign [lindex $options(-xyvalues) $kk] xvalg yvalg 
		            lassign [lindex $options(-xyvalues) [expr {$kk+1}]] xnextvalg ynextvalg                                
		        } 
		        set interpnt [::math::geometry::findLineSegmentIntersection \
		                "$xvalf $yvalf $xnextvalf $ynextvalf" "$xvalg $yvalg $xnextvalg $ynextvalg"]
		        if {$interpnt == "coincident"} {
		            lappend interpntList $xvalf 
		        } elseif {$interpnt != "none"} {
		            lappend interpntList [lindex $interpnt 0]
		        } 
		    }
		}                   
	    }
	}
	set interpntList [lsort -index 0 -real $interpntList]
	if {$interpntList == ""} {
	    snit_messageBox -message [_ "The functions do not intersect."] 
	    return 
	}
	foreach "a1 a2" $interpntList {          
	    lassign [$self calc_area_interval [$w give_uservar_value fx] $a1 $a2] areaf
	    lassign [$self calc_area_interval [$w give_uservar_value gx] $a1 $a2] areag
	    $w set_uservar_value area [format %.4g [expr {abs($areaf - $areag)}]]      
	}
    }
    method findLineSegmentIntersection {linesegment1 linesegment2} {
	if {[lineSegmentsIntersect $linesegment1 $linesegment2]} {
	    set lineintersect [findLineIntersection $linesegment1 $linesegment2]
	    switch -- $lineintersect {
		
		"coincident" {
		    # lines are coincident
		    set l1x1 [lindex $linesegment1 0]
		    set l1y1 [lindex $linesegment1 1]
		    set l1x2 [lindex $linesegment1 2]
		    set l1y2 [lindex $linesegment1 3]
		    set l2x1 [lindex $linesegment2 0]
		    set l2y1 [lindex $linesegment2 1]
		    set l2x2 [lindex $linesegment2 2]
		    set l2y2 [lindex $linesegment2 3]
		    # check if the line SEGMENTS overlap
		    # (NOT enough to check if the x-intervals overlap (vertical lines!))
		    set overlapx [intervalsOverlap $l1x1 $l1x2 $l2x1 $l2x2 0]
		    set overlapy [intervalsOverlap $l1y1 $l1y2 $l2y1 $l2y2 0]
		    if {$overlapx && $overlapy} {
		        return "coincident"
		    } else {
		        return "none"
		    }
		}
		
		"none" {
		    # should never happen, because we call "lineSegmentsIntersect" first
		    puts stderr "::math::geometry::findLineSegmentIntersection: suddenly no intersection?"
		    return "none"
		}
		
		default {
		    # lineintersect = the intersection point
		    return $lineintersect
		}
	    }
	} else {
	    return "none"
	}
    }
    method findLineIntersection {line1 line2} {
	set l1x1 [lindex $line1 0]
	set l1y1 [lindex $line1 1]
	set l1x2 [lindex $line1 2]
	set l1y2 [lindex $line1 3]
	set l2x1 [lindex $line2 0]
	set l2y1 [lindex $line2 1]
	set l2x2 [lindex $line2 2]
	set l2y2 [lindex $line2 3]
	
	# Is one of the lines vertical?
	if {$l1x1==$l1x2 || $l2x1==$l2x2} {
	    # One of the lines is vertical
	    if {$l1x1==$l1x2 && $l2x1==$l2x2} {
		# both lines are vertical
		if {$l1x1==$l2x1} {
		    return "coincident"
		} else {
		    return "none"
		}
	    }
	    
	    # make sure line1 is a vertical line
	    if {$l1x1!=$l1x2} {
		# interchange line 1 and 2
		set l1x1 [lindex $line2 0]
		set l1y1 [lindex $line2 1]
		set l1x2 [lindex $line2 2]
		set l1y2 [lindex $line2 3]
		set l2x1 [lindex $line1 0]
		set l2y1 [lindex $line1 1]
		set l2x2 [lindex $line1 2]
		set l2y2 [lindex $line1 3]
	    }
	    
	    # get equation of line 2 (y=a*x+b)
	    set a [expr {1.0*($l2y2-$l2y1)/($l2x2-$l2x1)}]
	    set b [expr {$l2y1-$a*$l2x1}]
	    
	    # Calculate intersection
	    set y [expr {$a*$l1x1+$b}]
	    return [list $l1x1 $y]
	} else {
	    # None of the lines are vertical
	    # - get equation of line 1 (y=a1*x+b1)
	    set a1 [expr {(1.0*$l1y2-$l1y1)/($l1x2-$l1x1)}]
	    set b1 [expr {$l1y1-$a1*$l1x1}]
	    # - get equation of line 2 (y=a2*x+b2)
	    set a2 [expr {(1.0*$l2y2-$l2y1)/($l2x2-$l2x1)}]
	    set b2 [expr {$l2y1-$a2*$l2x1}]
	    
	    if {abs($a2-$a1) > 0.0001} {
		# the lines are not parallel
		set x [expr {($b2-$b1)/($a1-$a2)}]
		set y [expr {$a1*$x+$b1}]
		return [list $x $y]
	    } else {
		# the lines are parallel
		if {abs($b1-$b2) < 0.00001} {
		    return "coincident"
		} else {
		    return "none"
		}
	    }
	}
    }
    method calc_area_interval { function a b } {         
	set area 0.0
	for {set i 0} {$i < [llength $options(-functionnames)]} {incr i} {
	    if {[lindex $options(-functionnames) $i] != $function} { continue }                        
	    if {$options(-multiplegraphs)} {
		set numdivisions [llength [lindex $options(-xyvalues) $i]]
	    } else {
		set numdivisions [llength $options(-xyvalues)]
	    }           
	    for {set j 0} {$j < [expr {$numdivisions-1}]} {incr j} {               
		if {$options(-multiplegraphs)} {
		    lassign [lindex [lindex $options(-xyvalues) $i] $j] xval yval
		    lassign [lindex [lindex $options(-xyvalues) $i] [expr {$j+1}]] xnextval ynextval
		} else {
		    lassign [lindex $options(-xyvalues) $j] xval yval
		    lassign [lindex $options(-xyvalues) [expr {$j+1}]] xnextval ynextval
		}  
		if {$options(-multiplegraphs)} {
		    set xyval [join [lindex $options(-xyvalues) $i] { }] 
		} else {
		    set xyval [join $options(-xyvalues) { }]
		}                                 
		if {$xval >= $b || $xnextval <= $a} { 
		    continue
		} elseif {$xval >= $a && $xnextval <= $b } {
		    # Simpson's formula 
		    set xmed [expr {($xval + $xnextval)/2.0}]
		    set fmed [$self interp-linear-x $xyval $xmed] 
		    set partial_area [expr {(($xnextval - $xval)/6.0)*($yval + 4*$fmed + $ynextval)}]
		} elseif {$xval <= $a && $xnextval >= $a} {                        
		    set fa [$self interp-linear-x $xyval $a] 
		    set fb [$self interp-linear-x $xyval $b] 
		    # Simpson's formula 
		    if {$xnextval > $b} {
		        set xmed [expr {($a + $b)/2.0}]
		        set fmed [$self interp-linear-x $xyval $xmed]                       
		        set partial_area [expr {(($b - $a)/6.0)*($fa + 4*$fmed + $fb)}]
		    } else {
		        set xmed [expr {($a + $xnextval)/2.0}]
		        set fmed [$self interp-linear-x $xyval $xmed] 
		        set partial_area [expr {(($xnextval - $a)/6.0)*($fa + 4*$fmed + $ynextval)}]
		    }
		} elseif {$xval <= $b && $xnextval >= $b} {
		    set fa [$self interp-linear-x $xyval $a] 
		    set fb [$self interp-linear-x $xyval $b] 
		    # Simpson's formula 
		    if {$xnextval > $b} {
		        set xmed [expr {($xval + $b)/2.0}]
		        set fmed [$self interp-linear-x $xyval $xmed]                       
		        set partial_area [expr {(($b - $xval)/6.0)*($yval + 4*$fmed + $fb)}]
		    } else {
		        set xmed [expr {($xval + $xnextval)/2.0}]
		        set fmed [$self interp-linear-x $xyval $xmed] 
		        set partial_area [expr {(($xnextval - $xval)/6.0)*($yval + 4*$fmed + $ynextval)}]
		    }
		} else {
		    continue
		}
		set area [expr {$area + $partial_area}]
	    }
	}
	return $area
    }      
    method polygonal_area { x0 y0 x1 y1 } { 
	set ymin [lindex $options(-yaxisminleft) 0]       
	if {$y0 <= $y1} {
	    set partial_area [expr {($x1-$x0)*($y0-$ymin) + (($x1-$x0)*($y1-$y0))/2.0}]
	} else {
	    set partial_area [expr {($x1-$x0)*($y1-$ymin) + (($x1-$x0)*($y0-$y1))/2.0}]
	}
	return $partial_area
    }   
    method area_do { w } {                 
	destroy $w
    }  
    method interp-linear-x { xyvalues xval } {        
	# Border cases first
	if { [lindex $xyvalues 0] > $xval } {
	    return [lindex $xyvalues 1]
	}
	if { [lindex $xyvalues end-1] < $xval } {
	    return [lindex $xyvalues end]
	}
		
	# The ordinary case        
	set idxx -2
	set idxy -1
	foreach { x y } $xyvalues {
	    if { $xval < $x } {
		break
	    }
	    incr idxx 2
	    incr idxy 2
	}
	
	set x2 [lindex $xyvalues $idxx]
	set y2 [lindex $xyvalues $idxy]
	
	if { $x2 != $x } {
	    set yval [expr {$y+($y2-$y)*($xval-$x)/double($x2-$x)}]
	} else {
	    set yval $y
	}
	return $yval
    }
    method interp-linear-y { prevx prevy x y yval } {                     
	if {$yval == $prevy} { return $prevx }
	if {$yval == $y} { return $x }
	
	set xval [expr {$x-($x-$prevx)*(($y-$yval)/double($y-$prevy))}]               
	return $xval
    }
    method intervalsOverlap {y1 y2 y3 y4 strict} {
	if {$y1>$y2} {
	    set temp $y1
	    set y1 $y2
	    set y2 $temp
	}
	if {$y3>$y4} {
	    set temp $y3
	    set y3 $y4
	    set y4 $temp
	}
	if {$strict} {
	    return [expr {$y2>$y3 && $y4>$y1}]
	} else {
	    return [expr {$y2>=$y3 && $y4>=$y1}]
	}
    }
    method displacement {} {
	set w .translation
	catch {destroy $w}
	set w [dialogwin_snit $w -title [_ "Displacement"] -callback [mymethod displacement_do] \
		-grab 1 -transient 1]
	set f [$w giveframe]
	    
	ttk::frame $f.f1
	pack $f.f1 -fill both -expand 1                                                                        
	
	ttk::label $f.f1.lx -justify left -anchor n -text [_ "X-value"]
	entry $f.f1.ex -textvariable [$w give_uservar xvalue] -width 40
	tooltip::tooltip $f.f1.lx [_ "X displacement value"] 
	
	ttk::label $f.f1.ly -justify left -anchor n -text [_ "Y-value"]
	entry $f.f1.ey -textvariable [$w give_uservar yvalue] -width 40
	tooltip::tooltip $f.f1.ly [_ "Y displacement value"] 
		                   
	grid $f.f1.lx $f.f1.ex -sticky news -padx 2 -pady 2
	grid $f.f1.ly $f.f1.ey -sticky news -padx 2 -pady 2  
	   
	grid columnconfigure $f.f1 1 -weight 1 -uniform 1         
	       
	$w set_uservar_value xvalue [lindex $options(-xymovement) 0]
	$w set_uservar_value yvalue [lindex $options(-xymovement) 1]          
		
	tk::TabToWindow $f.f1.ex
	bind $w <Return> [list $w invokeok]
    
	$w createwindow            
    }  
    method displacement_do { w } {              
	set action [$w giveaction]
	
	switch -- $action {
	    1 {             
		set options(-xymovement) "[$w give_uservar_value xvalue] [$w give_uservar_value yvalue]"
		destroy $w                
		catch { $self draw } errstring
	    }            
	    0 {
		destroy $w
	    }
	}        
    }    
    method edit_axis_do { w } {
	variable xyaxisdef
	
	set action [$w giveaction]
	
	switch -- $action {
	    1 {     
		if {[$w give_uservar_value xaxismax] < [$w give_uservar_value xaxismin]} {
		    snit_messageBox -message [_ "Incorrect x-axis range."] 
		    return
		} elseif {[$w give_uservar_value yaxismaxleft] <= [$w give_uservar_value yaxisminleft]} {
		    snit_messageBox -message [_ "Incorrect y-axis left range."] 
		    return
		} 
		if {$yaxis(rightside) != ""} {
		    if {[$w give_uservar_value yaxismaxright] <= [$w give_uservar_value yaxisminright]} {
		        snit_messageBox -message [_ "Incorrect y-axis right range."] 
		        return
		    }             
		}   
		set canvas $self                
		$canvas configure -xaxismax [$w give_uservar_value xaxismax] \
		    -xaxismin [$w give_uservar_value xaxismin] \
		    -yaxismaxleft [$w give_uservar_value yaxismaxleft] \
		    -yaxisminleft [$w give_uservar_value yaxisminleft] \
		    -yaxismaxright [$w give_uservar_value yaxismaxright] \
		    -yaxisminright [$w give_uservar_value yaxisminright] \
		    -xyaxisauto [$w give_uservar_value xyaxisauto]                
		destroy $w                
		catch { $self draw } errstring
	    }            
	    0 {
		destroy $w
	    }
	}        
    }
    method edit_axis {} {
	set w .editaxis
	catch {destroy $w}
	set w [dialogwin_snit $w -title [_ "Edit axis"] -callback [mymethod edit_axis_do] \
		-grab 1 -transient 1]
	set f [$w giveframe]
	    
	ttk::frame $f.f1
	pack $f.f1 -fill both -expand 1            
      
	ttk::notebook $f.f1.note
	pack $f.f1.note -fill both -expand 1 -padx 2 -pady 3
	
	## Enable Ctrl+Tab between tab-sheets
	ttk::notebook::enableTraversal $f.f1.note
	       
	# General
	ttk::frame $f.f1.note.general
	$f.f1.note add $f.f1.note.general -text [_ "General"] -underline 0 -padding 3    

	ttk::checkbutton $f.f1.note.general.cb -text [_ "Automatic x-y axis"] \
	    -variable [$w give_uservar xyaxisauto]     
	
	tooltip::tooltip $f.f1.note.general.cb [_ "Automatic x-y axis range"] 
	
	grid $f.f1.note.general.cb -sticky news -padx 2 -pady 2 
	      
	# x-axis
	ttk::frame $f.f1.note.xaxis                   
	$f.f1.note add $f.f1.note.xaxis -text [_ "x-axis"] -underline 0 -padding 3
	
	ttk::label $f.f1.note.xaxis.lmax -justify left -anchor n -text [_ "Maximum"]
	entry $f.f1.note.xaxis.emax -textvariable [$w give_uservar xaxismax] -width 40
	tooltip::tooltip $f.f1.note.xaxis.lmax [_ "Maximum x-axis value"] 
	   
	ttk::label $f.f1.note.xaxis.lmin -justify left -anchor n -text [_ "Minimum"]
	entry $f.f1.note.xaxis.emin -textvariable [$w give_uservar xaxismin] -width 40
	tooltip::tooltip $f.f1.note.xaxis.lmin [_ "Minimum x-axis value"]               
	
	grid $f.f1.note.xaxis.lmax $f.f1.note.xaxis.emax -sticky news -padx 2 -pady 2
	grid $f.f1.note.xaxis.lmin $f.f1.note.xaxis.emin -sticky news -padx 2 -pady 2  
      
	# y-axis        
	ttk::frame $f.f1.note.yaxis
	$f.f1.note add $f.f1.note.yaxis -text [_ "y-axis"] -underline 0 -padding 3
	  
	set fl [ttk::labelframe $f.f1.note.yaxis.l -text "Left axis" -relief groove -borderwidth 1]         
	ttk::label $fl.lmaxl -justify left -anchor n -text [_ "Maximum"]
	entry $fl.emaxl -textvariable [$w give_uservar yaxismaxleft] -width 40
	tooltip::tooltip $fl.lmaxl [_ "Maximum y-axis value"]               
	ttk::label $fl.lminl -justify left -anchor n -text [_ "Minimum"]
	entry $fl.eminl -textvariable [$w give_uservar yaxisminleft] -width 40
	tooltip::tooltip $fl.lminl [_ "Minimum y-axis value"]
		
	grid $fl -sticky news -padx 2 -pady 2 
	grid $fl.lmaxl $fl.emaxl -sticky ew -pady 2
	grid $fl.lminl $fl.eminl -sticky ew -pady 2      
	grid columnconfigure $fl 1 -weight 1
	grid configure $fl -columnspan 3
	
	set fr ""
	if {$yaxis(rightside) != ""} {
	    set fr [ttk::labelframe $f.f1.note.yaxis.r -text "Right axis" -relief groove -borderwidth 1]       
	    ttk::label $fr.rmaxr -justify left -anchor n -text [_ "Maximum"]
	    entry $fr.emaxr -textvariable [$w give_uservar yaxismaxright] -width 40 
	    tooltip::tooltip $fr.rmaxr [_ "Maximum y-axis value"]           
	    ttk::label $fr.rminr -justify left -anchor n -text [_ "Minimum"]
	    entry $fr.eminr -textvariable [$w give_uservar yaxisminright] -width 40 
	    tooltip::tooltip $fr.rminr [_ "Minimum y-axis value"]         
	    grid $fr -sticky news -padx 2 -pady 2
	    grid $fr.rmaxr $fr.emaxr -sticky ew -pady 2
	    grid $fr.rminr $fr.eminr -sticky ew -pady 2
	    grid columnconfigure $fr 1 -weight 1
	    grid configure $fr -columnspan 2
	}
	
	grid columnconfigure $f.f1.note.general 1 -weight 1 -uniform 1  
	grid columnconfigure $f.f1.note.xaxis 1 -weight 1 -uniform 1  
	grid columnconfigure $f.f1.note.yaxis 1 -weight 1 -uniform 1  
	
	if {$options(-xyaxisauto)} {
	    foreach n [list xaxismax xaxismin yaxismaxleft yaxisminleft yaxismaxright yaxisminright xyaxisauto] \
		v [list [format %.5g [lindex $options(-xaxismax) 0]] [format %.5g [lindex $options(-xaxismin) 0]] \
		    [format %.5g [lindex $options(-yaxismaxleft) 0]] [format %.5g [lindex $options(-yaxisminleft) 0]] \
		    [format %.5g [lindex $options(-yaxismaxright) 0]] [format %.5g [lindex $options(-yaxisminright) 0]]] {
		$w set_uservar_value $n $v
	    }             
	} else {
	    foreach n [list xaxismax xaxismin yaxismaxleft yaxisminleft yaxismaxright yaxisminright xyaxisauto] \
		v [list [format %.5g [lindex $options(-xaxismax) 1]] [format %.5g [lindex $options(-xaxismin) 1]] \
		    [format %.5g [lindex $options(-yaxismaxleft) 1]] [format %.5g [lindex $options(-yaxisminleft) 1]] \
		    [format %.5g [lindex $options(-yaxismaxright) 1]] [format %.5g [lindex $options(-yaxisminright) 1]]] {
		$w set_uservar_value $n $v    
	    }    
	}
	$w set_uservar_value xyaxisauto $options(-xyaxisauto) 
	
	set cmd "[mymethod default_xyaxis $w $fl $f $fr];#"
	trace add variable [$w give_uservar xyaxisauto] write $cmd
	bind $w <Destroy> [list trace remove variable [$w give_uservar xyaxisauto] write $cmd]
	
	eval $cmd
		
	tk::TabToWindow $f.f1.note.xaxis.emax
	bind $w <Return> [list $w invokeok]
    
	$w createwindow            
    }     
    method default_xyaxis { w fl f fr } {     
	if {[$w give_uservar_value xyaxisauto]} {
	    set state disabled          
	} else {
	    set state normal           
	}     
	$fl.emaxl configure -state $state                
	$fl.eminl configure -state $state
	if {$yaxis(rightside) != ""} {
	    $fr.emaxr configure -state $state
	    $fr.eminr configure -state $state
	}
	$f.f1.note.xaxis.emin configure -state $state            
	$f.f1.note.xaxis.emax configure -state $state         
    } 
    method clipboard_copy { } {
	clipboard clear
	
	set dataList ""      
	set colxylabel "$options(-xlabel) $options(-ylabel)"
	set colxylabelunit "$options(-xlabelunit) $options(-ylabelunit)"                
	
	for {set i 0} {$i < [llength $options(-functionnames)]} {incr i} {           
	    if {$options(-functionnames) != "Function"} {
		set functionname [string map [list \n " "] [lindex $options(-functionnames) $i]]
		lappend dataList $functionname 
	    }  
	    lappend dataList [join $colxylabel \t]
	    lappend dataList [join $colxylabelunit \t]
	    if {$options(-multiplegraphs)} {
		set numdivisions [llength [lindex $options(-xyvalues) $i]]
	    } else {
		set numdivisions [llength $options(-xyvalues)]
	    }  
	    for {set j 0} {$j < $numdivisions} {incr j} {
		if {$options(-multiplegraphs)} {
		    lassign [lindex [lindex $options(-xyvalues) $i] $j] xval yval                   
		} else {
		    lassign [lindex $options(-xyvalues) $j] xval yval
		}  
		lappend dataList [join "$xval $yval" \t]
	    }   
	    lappend dataList "\n"            
	}     
	clipboard append [join $dataList \n]        
    } 
    method import_from_csv { } {       
	set c $self
	
	set file [tk_getOpenFile -defaultextension .csv -initialdir $defaultdir -filetypes \
		[list [list [_ "CSV files"] {.csv}] \
		    [list [_ "All files"] *]] \
		-title [_ "Import data spreadsheet file (CSV file)"] -parent $c]       
	set file [string trim $file]
	if { $file eq "" } {
	    error [_ "It is necessary to enter a file name"]
	} elseif { ![file exists $file] } {
	    error [_ "File '%s' does not exist"]
	}
	if { [file extension $file] ne ".csv"} {
	    error [_ "File extension should be csv"]
	}        
	set defaultdir [file dirname $file]        
	set fin [open $file r]                       
	package require csv      
	      
	array set functionxy ""  
	lassign "" "" "" xylabelList options(-functionnames) options(-title) 
	
	set icount 0    
	while { [gets $fin line] != -1 } { 
	    set v [csv::split $line ";"]            
	    if {![string is double [lindex $v 0]]} {
	       if {$icount == "0"} {
		    set function $v  
		    lappend options(-functionnames) $function
		    set options(-$function) 0                     
		} else {
		    foreach xylab $v {
		        lappend xylabelList $xylab
		    }
		}
	    } else {
		foreach i $v {
		    lappend functionxy($function) $v 
		}
		set icount 0; continue
	    }
	    incr icount            
	}
	
	close $fin               
	array set functval ""
	foreach ifunction $options(-functionnames) {              
	    foreach "x y" $functionxy($ifunction) {
		lappend functval($ifunction) "$x $y" 
	    }            
	}                
	$c configure -xyaxisauto 1        
	lassign $xylabelList options(-xlabel) options(-ylabel) \
	    options(-xlabelunit) options(-ylabelunit)       
		       
	if {[llength [array names functval]] > 1} {
	    set options(-multiplegraphs) 1
	    set options(-xyvalues) ""
	    foreach ifunction $options(-functionnames) {        
		lappend options(-xyvalues) $functval($ifunction)
	    }         
	} else {
	    set options(-multiplegraphs) 0    
	    set options(-xyvalues) $functval($function)
	}                   
	$c draw
    }
    method import_from_excel { } {             
	set c $self                
	      
	set file [tk_getOpenFile -defaultextension .xls -filetypes [list [list \
		        [_ "XML files"] [list ".xml"]] [list [_ "All files"] ".*"]] \
		-initialdir $defaultdir -parent $c \
		-title [_ "Import data from spreadsheet file (XML file)"]]
	if { $file eq "" } { return }        
	set defaultdir [file dirname $file]

	set file [string trim $file]
	if { $file eq "" } {
	    error [_ "It is necessary to enter a file name"]
	} elseif { ![file exists $file] } {
	    error [_ "File '%s' does not exist"]
	}
	if { [file extension $file] ne ".xml"} {
	    error [_ "File extension should be xml"]
	}
	set tmp_file $file
	set xml [tDOM::xmlReadFile $tmp_file]
	set doc [dom parse $xml]
	lappend ns e urn:schemas-microsoft-com:office:spreadsheet \
	    ee urn:schemas-microsoft-com:office:excel
	
	lassign [list "" "" 1] vals snameList sheetNum 
	
	set sheetNode [lindex [$doc selectNodes -namespaces $ns {/*/e:Worksheet}] 0]
	set sname [$sheetNode @ss:Name N$sheetNum]
	 
	array set functionxy ""  
	lassign "" "" "" xylabelList options(-functionnames) options(-title)      
	      
	foreach cndNode [$sheetNode selectNode -namespaces $ns {/*/e:Worksheet/e:Table}] {                          
	    foreach rowNode [$cndNode selectNodes -namespaces $ns .//e:Row] {           
		set cellNode [$rowNode selectNodes -namespaces $ns .//e:Cell/e:Data]
		if {[llength $cellNode] == "1"} {
		    set function [$cellNode text]  
		    lappend options(-functionnames) $function
		    set options(-$function) 0 
		    set isfound 1
		} else {
		    foreach icell $cellNode {
		        if {![string is double [$icell text]]} {
		            lappend xylabelList [$icell text]                        
		        } elseif {[$icell text] != ""} {                 
		            lappend functionxy($function) [$icell text]                     
		        } 
		    }
		}
	    }
	}
	      
	array set functval ""
	foreach ifunction $options(-functionnames) {              
	    foreach "x y" $functionxy($ifunction) {
		lappend functval($ifunction) "$x $y" 
	    }            
	}
		
	$c configure -xyaxisauto 1
	
	lassign $xylabelList options(-xlabel) options(-ylabel) \
	    options(-xlabelunit) options(-ylabelunit)       
		       
	if {[llength [array names functval]] > 1} {
	    set options(-multiplegraphs) 1
	    set options(-xyvalues) ""
	    foreach ifunction $options(-functionnames) {        
		lappend options(-xyvalues) $functval($ifunction)
	    }         
	} else {
	    set options(-multiplegraphs) 0    
	    set options(-xyvalues) $functval($function)
	}       
	    
	$c draw
    }  
    method export_to_csv { } {              
	set c $self
	
	set file [tk_getSaveFile -initialdir $defaultdir -filetypes \
		[list [list [_ "CSV files"] {.csv}] \
		    [list [_ "All files"] *]] -defaultextension .csv \
		-title [_ "Save graph as spreadsheet (CSV file)"] -parent $c]            
	if { $file eq "" } { return }                                            
	set defaultdir [file dirname $file]  
	set fout [open $file w]
	for {set i 0} {$i < [llength $options(-functionnames)]} {incr i} {                   
	    if {$options(-functionnames) != "Function"} {
		set functionname [string map [list \n " "] [lindex $options(-functionnames) $i]]
		puts $fout [cu::string::csv_join $functionname ";"]                   
	    } else {
		puts $fout "Function"
	    }                
	    puts $fout [cu::string::csv_join "$options(-xlabel) $options(-ylabel)" ";"]                                     
	    puts $fout [cu::string::csv_join "$options(-xlabelunit) $options(-ylabelunit)" ";"]             
	    
	    if {$options(-multiplegraphs)} {
		set numdivisions [llength [lindex $options(-xyvalues) $i]]
	    } else {
		set numdivisions [llength $options(-xyvalues)]
	    }
	    for {set j 0} {$j < $numdivisions} {incr j} {
		if {$options(-multiplegraphs)} {
		    lassign [lindex [lindex $options(-xyvalues) $i] $j] xval yval                   
		} else {
		    lassign [lindex $options(-xyvalues) $j] xval yval
		}  
		puts $fout [cu::string::csv_join "$xval $yval" ";"]                  
	    }            
	}            
	close $fout        
    }
    method export_to_excel { } {
	set c $self
	      
	set filename [tk_getSaveFile  -defaultextension .xls -filetypes [list [list \
		        [_ "Excel files"] ".xml" ".xls"] [list [_ "All files"] ".*"]] \
		-initialdir $defaultdir -parent $c \
		-title [_ "Save graph as spreadsheet (Excel...)"]]
	if { $filename == "" } { return }
	set defaultdir [file dirname $filename]        
	set currentfilename $filename
	
	set _ {<?xml version="1.0"?>
	    <?mso-application progid="Excel.Sheet"?>
	    <Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
	    xmlns:o="urn:schemas-microsoft-com:office:office"
	    xmlns:x="urn:schemas-microsoft-com:office:excel"
	    xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
	    xmlns:html="http://www.w3.org/TR/REC-html40">            
	    <Styles>
	    <Style ss:ID="Default" ss:Name="Normal">
	    <Alignment ss:Vertical="Bottom"/>
	    <Borders/>
	    <Font ss:FontName="Calibri" x:Family="Swiss" ss:Size="11" ss:Color="#000000"/>
	    <Interior/>
	    <NumberFormat/>
	    <Protection/>
	    </Style>
	    <Style ss:ID="s62">
	    <Alignment ss:Vertical="Bottom"/>
	    <Borders/>
	    <Font ss:FontName="Arial" x:Family="Swiss" ss:Bold="1"/>
	    <Interior/>
	    <NumberFormat/>
	    <Protection/>
	    </Style>
	    </Styles>
	    <Worksheet ss:Name="Hoja1">
	    <Table ss:ExpandedColumnCount="15" ss:ExpandedRowCount="198" x:FullColumns="1"
	    x:FullRows="1" ss:DefaultColumnWidth="60">
	    <Column ss:AutoFitWidth="0" ss:Width="44.25"/>
	    <Column ss:AutoFitWidth="0" ss:Width="147"/>
	    <Column ss:Index="14" ss:AutoFitWidth="0" ss:Width="348.75"/>
	}                 
	for {set i 0} {$i < [llength $options(-functionnames)]} {incr i} {                   
	    if {$options(-functionnames) != "Function"} {
		set functionname [string map [list \n " "] [lindex $options(-functionnames) $i]]
		append _ "<Row><Cell ss:StyleID='s62'><Data ss:Type='String'>$functionname</Data></Cell></Row>"                        
	    } else {
		set functionname [string map [list \n " "] [lindex $options(-functionnames) $i]]
		append _ "<Row><Cell ss:StyleID='s62'><Data ss:Type='String'>$functionname</Data></Cell></Row>"   
	    }                
	       
	    append _ "<Row>"
	    foreach j "$options(-xlabel) $options(-ylabel)" {
		append _ "<Cell ss:StyleID='s62'><Data ss:Type='String'>$j</Data></Cell>"
	    }
	    append _ "</Row>"
	    
	    append _ "<Row>"
	    foreach j "$options(-xlabelunit) $options(-ylabelunit)" {
		append _ "<Cell ss:StyleID='s62'><Data ss:Type='String'>$j</Data></Cell>"
	    }
	    append _ "</Row>"
	    
	    if {$options(-multiplegraphs)} {
		set numdivisions [llength [lindex $options(-xyvalues) $i]]
	    } else {
		set numdivisions [llength $options(-xyvalues)]
	    }
	    for {set j 0} {$j < $numdivisions} {incr j} {
		if {$options(-multiplegraphs)} {
		    lassign [lindex [lindex $options(-xyvalues) $i] $j] xval yval                   
		} else {
		    lassign [lindex $options(-xyvalues) $j] xval yval
		}  
		append _ "<Row>"
		append _ "<Cell><Data ss:Type='String'>$xval</Data></Cell>"
		append _ "<Cell><Data ss:Type='String'>$yval</Data></Cell>"
		append _ "</Row>"
	    }   
	    append _ "<Row></Row>"
	}            
	
	append _ {
	    </Table>
	    <x:WorksheetOptions/>
	    </Worksheet>
	    </Workbook>            
	}
	
	set err [catch {set fout [open $currentfilename w]} errstring]
	if { $err } {
	    snit_messageBox -message $::errorInfo
	    return
	}
	fconfigure $fout -encoding utf-8
	puts $fout $_
	close $fout
    }  
    method drawingoptions { optiontype } {        
	if {$options(-$optiontype)} {
	   set options(-$optiontype) 0
	} else {
	    set options(-$optiontype) 1
	}
	catch { $self draw } errstring
    }   
    method movingoptions { optiontype } {        
	if {$options(-move$optiontype)} {
	   set options(-move$optiontype) 0
	} else {
	    set options(-move$optiontype) 1
	}
	catch { $self draw } errstring
    }     
    method draw {} {       
	set err [catch { $self draw_do } errstring]
	if { $err } {
	    snit_messageBox -message $::errorInfo
	}                    
	#set debugstop 0
    }   
    method drawline_svg { x1 y1 x2 y2 color lwidth fillcolor } {
	set svg "<line x1='$x1' y1='$y1' x2='$x2' y2='$y2' fill='$fillcolor' \
	    stroke='$color' stroke-width='$lwidth'/>\n"                           
	return $svg
    }
    method drawcircle_svg { x y radius color lwidth fillcolor } {     
	set svg "<circle cx='$x' cy='$y' r='$radius' \
	    fill='$fillcolor' stroke='$color' stroke-width='$lwidth'/>\n"    
	return $svg
    }
    method drawtext_svg { font fillcolor x y txt } {        
	set fsize [expr {[font metrics $font -ascent]+int(.5*[font metrics $font -descent])}]
	set slant [font actual $font -slant]
	set a middle
	       
	append svg "<text font-family='[xml_map [font actual $font -family]]' " \
	    "font-size='$fsize' fill='$fillcolor' " \
	    "font-weight='[font actual $font -weight]' " \
	    "font-style='$slant' text-anchor='$a'"
	foreach n [list x y] {
	    append svg " $n='[set $n]'"
	}
	append svg ">[xml_map $txt]</text>"               
	
	return $svg
    }
    method drawbox_svg { color lwidth x0 y0 x1 y1 } {
	set pointsList [join "$x0 $y0 $x1 $y0 $x1 $y1 $x0 $y1" ,]
	set svg "<polyline fill='none' stroke='$color' stroke-width='$lwidth' points='$pointsList'/>"
	
	return $svg
    }
    method draw_do {} {                  
	variable fg      
	variable yaxis 
	variable xml  
	
	set canvas $self
	
	$canvas delete curve
	$canvas delete axistext
	$canvas delete titletext
	$canvas delete legendbox       
	$canvas delete zeroline       
	$canvas delete curvepoint
	$canvas delete curvecurrent
	$canvas delete coords   
	$canvas delete gridlines 
	
	if {$options(-canvasheightset) != ""} {            
	    set ymax $options(-canvasheightset)
	} else {
	    set ymax [winfo height $canvas]  
	}
	if {$options(-canvaswidthset) != ""} {
	    set xmax $options(-canvaswidthset)  
	} else {
	    set xmax [winfo width $canvas]                        
	} 
	
	set xml ""
	append xml "<svg xmlns='http://www.w3.org/2000/svg' " \
	    "xmlns:xlink='http://www.w3.org/1999/xlink' " \
	    "width='$xmax' height='$ymax' version='1.1' "\
	    "viewBox='0.0 0.0 $xmax $ymax'>\n"              
#         append xml {
#             <defs>
#             <marker id="Triangle1" viewBox="0 0 10 10" refX="0" refY="5" markerUnits="strokeWidth" markerWidth="4" markerHeight="3" orient="auto">
#             <path d="M 10 0 L 0 5 L 10 10 z"/>
#             </marker>
#             <marker id="Triangle2" viewBox="0 0 10 10" refX="10" refY="5" markerUnits="strokeWidth" markerWidth="4" markerHeight="3" orient="auto">
#             <path d="M 0 0 L 10 5 L 0 10 z"/>
#             </marker>
#             <marker id="Line1" viewBox="0 0 1 1" refX="1" refY=".5" markerUnits="strokeWidth" markerWidth=".5" markerHeight="1" orient="auto">
#             <path d="M 0 0 L 0 1 L 1 1 L 1 0 z" stroke="none"/>
#             </marker>
#             <marker id="Line2" viewBox="0 0 1 1" refX="0" refY=".5" markerUnits="strokeWidth" markerWidth=".5" markerHeight="1" orient="auto">
#             <path d="M 0 0 L 0 1 L 1 1 L 1 0 z" stroke="none"/>
#             </marker>
#             </defs>
#         }                                        
	
	foreach i $options(-functionnames) {
	    if {![info exists options(-$i)]} { set options(-$i) 0 }
	    if {![info exists options(-move$i)]} { set options(-move$i) 0 }
	}  

	array set yaxis [list leftside "" rightside ""]                     
	for {set i 0} {$i < [llength $options(-functionnames)]} {incr i} {
	    if {$options(-[lindex $options(-functionnames) $i])} { continue }   
	    set ifunction [lindex $options(-functionnames) $i]
	    if {$yaxis(leftside) == "" || [lsearch -exact $yaxis(leftside) $ifunction] != -1 || \
		$yaxis(rightside) != "" || [llength $options(-functionnames)] > 2} {
		lappend yaxis(leftside) $ifunction
	    } else {
		lappend yaxis(rightside) $ifunction 
	    }                                 
	}
	
	$canvas configure -background $options(-backgroundcolor)  
	set fg $options(-foregroundcolor)  
	
	set numfunctions 1
	if {$options(-multiplegraphs)} {            
	    set numfunctions [llength $options(-xyvalues)] 
	}                                                       
	
	if {$options(-xyaxisauto)} {  
	    $canvas configure -xyaxisauto 1
	    if {$options(-includeyzero)} {                       
		set options(-yaxisminleft) [lreplace $options(-yaxisminleft) 0 0 0.0]            
		if {$yaxis(rightside) != ""} {   
		    set options(-yaxisminright) [lreplace $options(-yaxisminright) 0 0 0.0]
		}
	    }
	}
		   
	for {set i 0} {$i < [llength $options(-functionnames)]} {incr i} {
	    if {$options(-[lindex $options(-functionnames) $i])} { continue }             
	    set ifunction [lindex $options(-functionnames) $i]
	    if {$options(-multiplegraphs)} {
		set numdivisions [llength [lindex $options(-xyvalues) $i]]
	    } else {
		set numdivisions [llength $options(-xyvalues)]
	    }
	    for {set j 0} {$j < $numdivisions} {incr j} {               
		if {$options(-multiplegraphs)} {
		    lassign [lindex [lindex $options(-xyvalues) $i] $j] xval yval
		} else {
		    lassign [lindex $options(-xyvalues) $j] xval yval
		}  
		if {$options(-move$ifunction)} {
		    set xval [expr {$xval + [lindex $options(-xymovement) 0]}]
		    set yval [expr {$yval + [lindex $options(-xymovement) 1]}]
		} 
		if {$options(-xyaxisauto)} {    
		    if { $xval < [lindex $options(-xaxismin) 0] } { 
		        set options(-xaxismin) [lreplace $options(-xaxismin) 0 0 $xval] 
		    }
		    if { $xval > [lindex $options(-xaxismax) 0] } {
		        set options(-xaxismax) [lreplace $options(-xaxismax) 0 0 $xval] 
		    }                              
		    if {[lsearch -exact $yaxis(leftside) $ifunction] != -1} {
		        if { $yval < [lindex $options(-yaxisminleft) 0] } { 
		            set options(-yaxisminleft) [lreplace $options(-yaxisminleft) 0 0 $yval] 
		        }
		        if { $yval > [lindex $options(-yaxismaxleft) 0] } { 
		            set options(-yaxismaxleft) [lreplace $options(-yaxismaxleft) 0 0 $yval] 
		        }
		    } 
		    if {[lsearch -exact $yaxis(rightside) $ifunction] != -1} {
		        if { $yval < [lindex $options(-yaxisminright) 0] } { 
		            set options(-yaxisminright) [lreplace $options(-yaxisminright) 0 0 $yval] 
		        }
		        if { $yval > [lindex $options(-yaxismaxright) 0] } { 
		            set options(-yaxismaxright) [lreplace $options(-yaxismaxright) 0 0 $yval] 
		        }
		    }                                 
		}               
	    }
	}   
	if { [lindex $options(-xaxismax) 0] == [lindex $options(-xaxismin) 0] } { 
	    set options(-xaxismax) [lreplace $options(-xaxismax) 0 0 [expr {[lindex $options(-xaxismax) 0]+1}]] 
	}   
	if { [lindex $options(-yaxismaxleft) 0] == [lindex $options(-yaxisminleft) 0] } { 
	    set options(-yaxismaxleft) [lreplace $options(-yaxismaxleft) 0 0 [expr {[lindex $options(-yaxismaxleft) 0]+1}]] 
	}        
	if {$yaxis(rightside) != ""} { 
	    if { [lindex $options(-yaxismaxright) 0] == [lindex $options(-yaxisminright) 0] } { 
		set options(-yaxismaxright) [lreplace $options(-yaxismaxright) 0 0 [expr {[lindex $options(-yaxismaxright) 0]+1}]] 
	    }  
	}        
	if {$options(-xyaxisauto)} {  
	    foreach v [list xaxismin xaxismax yaxisminleft yaxismaxleft yaxisminright yaxismaxright] \
		n [list [lindex $options(-xaxismin) 0] [lindex $options(-xaxismax) 0] \
		    [lindex $options(-yaxisminleft) 0] [lindex $options(-yaxismaxleft) 0] \
		    [lindex $options(-yaxisminright) 0] [lindex $options(-yaxismaxright) 0]] {
		set $v $n
	    }
	} else {
	    foreach v [list xaxismin xaxismax yaxisminleft yaxismaxleft yaxisminright yaxismaxright] \
		n [list [lindex $options(-xaxismin) 1] [lindex $options(-xaxismax) 1] \
		    [lindex $options(-yaxisminleft) 1] [lindex $options(-yaxismaxleft) 1] \
		    [lindex $options(-yaxisminright) 1] [lindex $options(-yaxismaxright) 1]] {
		set $v $n
	    }
	}
		   
	set textwidth 0
	set inumtics 8
	for {set i 0 } { $i < $inumtics } { incr i } {
	    set yvaltext \
		[format "%.4g" [expr $yaxisminleft+$i/double($inumtics-1)*($yaxismaxleft-$yaxisminleft)]]
	    regsub {e([+-])00} $yvaltext {e\10} yvaltext
	    set tt [font measure $options(-font) $yvaltext]
	    if { $tt > $textwidth } { set textwidth $tt }
	    if {$yaxis(rightside) != ""} {
		set yvaltext \
		    [format "%.4g" [expr $yaxisminright+$i/double($inumtics-1)*($yaxismaxright-$yaxisminright)]]
		regsub {e([+-])00} $yvaltext {e\10} yvaltext
		set tt [font measure $options(-font) $yvaltext]
		if { $tt > $textwidth } { set textwidth $tt }
	    }
	}
	
	set xm [expr $textwidth+10]
	set textheight [font metrics $options(-font) -linespace]
	set ym [expr int($textheight*1.5)+20]
		          
	if {$options(-showlegend)} {
	    lassign 5 ypos         
	    lassign "1e10 [expr $xmax-5.0]" xlegendmin xlegendmax
	    for {set i 0} {$i < $numfunctions} {incr i} { 
		if {$options(-[lindex $options(-functionnames) $i])} { continue }                
		set textwidthlegend [font measure $options(-font) [lindex $options(-functionnames) $i]]                 
		if {$xlegendmin > [expr $xmax-$textwidthlegend-21.0]} { 
		    set xlegendmin [expr $xmax-$textwidthlegend-21.0]
		}  
	    }            
	    for {set i 0} {$i < $numfunctions} {incr i} { 
		if {$options(-[lindex $options(-functionnames) $i])} { continue }                
		set textwidthlegend [font measure $options(-font) [lindex $options(-functionnames) $i]]   
		set textheight [font metrics $options(-font) -linespace]                 
		$canvas create line $xlegendmin [expr $ypos+$textheight/2.0] \
		    [expr $xlegendmin+10.0] [expr $ypos+$textheight/2.0] -width 2 \
		    -fill [lindex $options(-colorset) $i] -tags legendbox                     
		append xml [$canvas drawline_svg $xlegendmin [expr $ypos+$textheight/2.0] \
		        [expr $xlegendmin+10.0] [expr $ypos+$textheight/2.0] [lindex $options(-colorset) $i] \
		        1 [lindex $options(-colorset) $i]]            
		$canvas create text [expr $xlegendmin+15.0] $ypos -anchor nw -justify left \
		    -text [lindex $options(-functionnames) $i] -font $options(-font) -fill $fg -tags legendbox                                   
		append xml [$canvas drawtext_svg $options(-font) $fg [expr $xlegendmin+15.0] $ypos [lindex $options(-functionnames) $i]]                           
		set ypos [expr $ypos+$textheight]                          
	    }
	    if {$xlegendmin < 1e10} {
		$canvas create rectangle [expr $xlegendmin-3.0] [expr $ypos+2.0] $xlegendmax 2 -width 1 \
		    -tags legendbox -outline $fg                      
		append xml [$canvas drawbox_svg $fg 1.0 [expr $xlegendmin-3.0] [expr $ypos+2.0] \
		        $xlegendmax 2]           
	    }
	}                
	if {$options(-showtitle)} {
	    set fam [font actual $options(-fonttitle) -family]
	    set tsize [expr {round([font actual $options(-fonttitle) -size]*1.8)}]   
	    $canvas create text [expr $xmax/2.0] 6 -anchor n -justify center \
		-text $options(-title) -font [list $fam $tsize] -fill $fg -tags titletext                
	    append xml [$canvas drawtext_svg [list $fam $tsize] $fg [expr $xmax/2.0] 6 $options(-title)] 
	}
		        
	if {$yaxis(rightside) != ""} {
	    $canvas create line [expr $xmax-$xm] [expr $ymax-$ym] [expr $xmax-$xm] \
		[expr $ym-6.0] -arrow last -fill $fg -tags axistext
	    append xml [$canvas drawline_svg [expr $xmax-$xm] [expr $ymax-$ym] [expr $xmax-$xm] \
		    [expr $ym-6.0] $fg 1 $fg]  
	    $canvas create line $xm [expr $ymax-$ym] $xm [expr $ym-6.0] -arrow last -fill $fg -tags axistext
	    append xml [$canvas drawline_svg $xm [expr $ymax-$ym] $xm [expr $ym-6.0] $fg 1 $fg]
	    $canvas create line $xm [expr $ymax-$ym] [expr $xmax-$xm] [expr $ymax-$ym] \
		-fill $fg -tags axistext   
	    append xml [$canvas drawline_svg $xm [expr $ymax-$ym] [expr $xmax-$xm] [expr $ymax-$ym] $fg 1 $fg] 
	} else {
	    $canvas create line $xm [expr $ymax-$ym] $xm [expr $ym-6.0] -arrow last -fill $fg -tags axistext  
	    append xml [$canvas drawline_svg $xm [expr $ymax-$ym] $xm [expr $ym-6.0] $fg 1 $fg] 
	    $canvas create line $xm [expr $ymax-$ym] [expr $xmax-$xm] [expr $ymax-$ym] \
		-arrow last -fill $fg -tags axistext   
	    append xml [$canvas drawline_svg $xm [expr $ymax-$ym] [expr $xmax-$xm] [expr $ymax-$ym] $fg 1 $fg] 
	}
	 
	if {$options(-ylabelunit) != ""} {
	    set ylabeltext "$options(-ylabel) ($options(-ylabelunit))"
	} else {
	    set ylabeltext "$options(-ylabel)"
	}      
	$canvas create text 6 20 -anchor nw -justify left \
	    -text $ylabeltext -font $options(-font) -fill $fg -tags axistext                        
	append xml [$canvas drawtext_svg $options(-font) $fg 6 6 $ylabeltext]        
	
	if {$options(-xlabelunit) != ""} {
	    set xlabeltext "$options(-xlabel) ($options(-xlabelunit))"
	} else {
	    set xlabeltext "$options(-xlabel)"
	}
	set textwidthlabel [font measure $options(-font) $xlabeltext]
	
	$canvas create text [expr $xmax-$textwidthlabel-10.0] [expr $ymax-$ym+$textheight] \
	    -anchor nw -justify left -text $xlabeltext -font $options(-font) -fill $fg -tags axistext 
	append xml [$canvas drawtext_svg $options(-font) $fg [expr $xmax-$textwidthlabel-10.0] [expr $ymax-$ym+$textheight] \
		$xlabeltext]
	
	set xfact [expr ($xmax-2.0*$xm)/double($xaxismax-$xaxismin)]
	
	set yfactleft [expr ($ymax-2.0*$ym)/double($yaxismaxleft-$yaxisminleft)]
	set yvalleft [expr $ymax-$ym-(0.0-$yaxisminleft)*$yfactleft]     
	if { $yvalleft > $ym && $yvalleft <= [expr $ymax-$ym] } {
	    $canvas create line $xm $yvalleft [expr $xmax-$xm] $yvalleft -fill $fg \
		-tags zeroline -dash -.-
	    append xml [$canvas drawline_svg $xm $yvalleft [expr $xmax-$xm] $yvalleft $fg 1 $fg] 
	}
	if {$yaxis(rightside) != ""} {        
	    set yfactright [expr ($ymax-2.0*$ym)/double($yaxismaxright-$yaxisminright)]
	    set yvalright [expr $ymax-$ym-(0.0-$yaxisminright)*$yfactright]
	    if { $yvalright > $ym && $yvalright <= [expr $ymax-$ym] } {
		$canvas create line $xm $yvalright [expr $xmax-$xm] $yvalright -fill $fg \
		    -tags zeroline -dash -.-
		append xml [$canvas drawline_svg $xm $yvalright [expr $xmax-$xm] $yvalright $fg 1 $fg] 
	    }
	}                                              
	set m [expr {abs($xaxismax)>abs($xaxismin)?abs($xaxismax):abs($xaxismin)}]
	if { $m == 0 } { set m 1 }         
	set x_tic_m [expr {pow(10,int(1+log10($m)))}]      
	set fm [font measure $options(-font) [format %.3g $x_tic_m]]
	foreach i [list 100.0 50.0 20.0 10.0 5.0 2.0] {
	    set x_tic_delta [expr {$x_tic_m/$i}]
	    if { [expr {$x_tic_delta*$xfact}] > [expr {$fm+6}] } { break }
	}
	if {[info exists xprev]} { unset xprev }
	foreach x [rangeF -$x_tic_m $x_tic_m $x_tic_delta] {
	    if { $x < $xaxismin || $x > $xaxismax } { continue }    
	    if {[info exists xprev]} {
		if {$xprev < 0 && $x > 0} { set x 0.0 }
	    }            
	    set xval [expr $xm+($x-$xaxismin)*$xfact]           
	    if {$options(-addgridlines)} { 
		$canvas create line $xval $ym $xval [expr {$ymax-$ym}] -fill $options(-gridlinescolor) -tags gridlines
	    }
	    $canvas create line $xval [expr {$ymax-$ym+2}] $xval [expr {$ymax-$ym}] -fill $fg \
		-tags axistext 
	    append xml [$canvas drawline_svg $xval [expr {$ymax-$ym+2}] $xval [expr $ymax-$ym] $fg 1 $fg] 
	    $canvas create text $xval [expr {$ymax-$ym+2}] -anchor n -justify center \
		-text [format %.3g $x] -font $options(-font) -fill $fg -tags axistext
	    append xml [$canvas drawtext_svg $options(-font) $fg $xval [expr {$ymax-$ym+2}] [format %.3g $x]] 
	    set xprev $x
	}
	
	set m [expr {abs($yaxismaxleft)>abs($yaxisminleft)?abs($yaxismaxleft):abs($yaxisminleft)}]
	if { $m == 0 } { set m 1 }
	set y_tic_m [expr {pow(10,int(1+log10($m)))}]
	foreach i [list 100.0 50.0 20.0 10.0 5.0 2.0] {
	    set y_tic_delta [expr {$y_tic_m/$i}]
	    if { $y_tic_delta*$yfactleft > $textheight+3 } { break }
	}  
	if {[info exists yprev]} { unset yprev }
	foreach y [rangeF -$y_tic_m $y_tic_m $y_tic_delta] {
	    if { $y < $yaxisminleft-1e-30 || $y > $yaxismaxleft+1e-30 } { continue }         
	    if {[info exists yprev]} {
		if {$yprev < 0 && $y > 0} { set y 0.0 }
	    }
	    set yval [expr {$ymax-$ym-($y-$yaxisminleft)*$yfactleft}]          
	    if {$options(-addgridlines)} { 
		$canvas create line $xm $yval [expr {$xmax-$xm}] $yval -fill $options(-gridlinescolor) -tags gridlines                        
	    }
	    $canvas create line [expr {$xm-2}] $yval $xm $yval -fill $fg -tags axistext
	    append xml [$canvas drawline_svg [expr {$xm-2}] $yval $xm $yval $fg 1 $fg] 
	    $canvas create text [expr {$xm-3}] $yval -anchor e -justify right \
		-text [format %g $y] -font $options(-font) -fill $fg -tags axistext                     
	    append xml [$canvas drawtext_svg $options(-font) $fg [expr {$xm-3}] $yval [format %g $y]] 
	    set yprev $y
	}                
	if {$yaxis(rightside) != ""} {   
	    set m [expr {abs($yaxismaxright)>abs($yaxisminright)?abs($yaxismaxright):abs($yaxisminright)}]
	    if { $m == 0 } { set m 1 }
	    set y_tic_m [expr {pow(10,int(1+log10($m)))}]
	    foreach i [list 100.0 50.0 20.0 10.0 5.0 2.0] {
		set y_tic_delta [expr {$y_tic_m/$i}]
		if { $y_tic_delta*$yfactright > $textheight+3 } { break }
	    }   
	    if {[info exists yprevr]} { unset yprevr }      
	    foreach y [rangeF -$y_tic_m $y_tic_m $y_tic_delta] {
		if { $y < $yaxisminright-1e-10 || $y > $yaxismaxright+1e-10 } { continue }
		if {[info exists yprevr]} {
		    if {$yprevr < 0 && $y > 0} { set y 0.0 }
		}                
		set yval [expr {$ymax-$ym-($y-$yaxisminright)*$yfactright}]
		$canvas create line [expr {$xmax-$xm-2}] $yval [expr {$xmax-$xm}] $yval -fill $fg -tags axistext
		append xml [$canvas drawline_svg [expr {$xmax-$xm-2}] $yval [expr {$xmax-$xm}] $yval $fg 1 $fg] 
		$canvas create text [expr {$xmax-$xm-3}] $yval -anchor e -justify right \
		    -text [format %g $y] -font $options(-font) -fill $fg -tags axistext
		append xml [$canvas drawtext_svg $options(-font) $fg [expr {$xmax-$xm-3}] $yval [format %g $y]] 
		set yprevr $y
	    }     
	}       
	
	for {set i 0} {$i < [llength $options(-functionnames)]} {incr i} {
	    if {$options(-[lindex $options(-functionnames) $i])} { continue }             
	    set ifunction [lindex $options(-functionnames) $i]                     
	    if {$options(-multiplegraphs)} {
		set numdivisions [expr [llength [lindex $options(-xyvalues) $i]]-1]
	    } else {
		set numdivisions [expr [llength $options(-xyvalues)]-1]
	    }
	    for {set j 0} {$j <= $numdivisions} {incr j} {
		if {$options(-multiplegraphs)} {
		    lassign [lindex [lindex $options(-xyvalues) $i] $j] xval yval
		} else {
		    lassign [lindex $options(-xyvalues) $j] xval yval
		}   
		if {$options(-move$ifunction)} {       
		    set xval [expr {$xval + [lindex $options(-xymovement) 0]}]
		    set yval [expr {$yval + [lindex $options(-xymovement) 1]}]         
		}
		if {!$options(-xyaxisauto)} {                              
		    if {$j == 0} {                                            
		        continue 
		    } else {
		        if {$options(-multiplegraphs)} {
		            lassign [lindex [lindex $options(-xyvalues) $i] [expr {$j-1}]] prevxval prevyval
		        } else {
		            lassign [lindex $options(-xyvalues) [expr {$j-1}]] prevxval prevyval
		        }   
		        if {$options(-move$ifunction)} {
		            set prevxval [expr {$prevxval + [lindex $options(-xymovement) 0]}]
		            set prevyval [expr {$prevyval + [lindex $options(-xymovement) 1]}] 
		        }
		        if {[lsearch -exact $yaxis(rightside) $ifunction] != -1} { 
		            if {![$self isinbox $xval $yval $xaxismin $xaxismax $yaxisminright $yaxismaxright] || \
		                ![$self isinbox $prevxval $prevyval $xaxismin $xaxismax $yaxisminright $yaxismaxright]} {
		                lassign [$canvas islineinbox $prevxval $prevyval $xval $yval $ifunction \
		                        $xaxismin $xaxismax $yaxisminright $yaxismaxright] pntList      
		            } elseif {[$self isinbox $xval $yval $xaxismin $xaxismax $yaxisminright $yaxismaxright] && \
		                [$self isinbox $prevxval $prevyval $xaxismin $xaxismax $yaxisminright $yaxismaxright]} {
		                set pntList [list "$prevxval $prevyval" "$xval $yval"]
		            } else {
		                set pntList "none"
		            }
		        } else {
		            if {![$self isinbox $xval $yval $xaxismin $xaxismax $yaxisminleft $yaxismaxleft] || \
		                ![$self isinbox $prevxval $prevyval $xaxismin $xaxismax $yaxisminleft $yaxismaxleft]} {                                
		                lassign [$canvas islineinbox $prevxval $prevyval $xval $yval $ifunction \
		                        $xaxismin $xaxismax $yaxisminleft $yaxismaxleft] pntList                                             
		            } elseif {[$self isinbox $xval $yval $xaxismin $xaxismax $yaxisminleft $yaxismaxleft] && \
		                [$self isinbox $prevxval $prevyval $xaxismin $xaxismax $yaxisminleft $yaxismaxleft]} {
		                set pntList [list "$prevxval $prevyval" "$xval $yval"]
		            } else {
		                set pntList "none"
		            }             
		        }                           
		        if {$pntList == ""} {                           
		            continue 
		        } elseif {$pntList != "none"} {                     
		            for {set icount 0} {$icount < [expr {[llength $pntList]-1}]} {incr icount} {                                                                                                                           
		                lassign [lindex $pntList $icount] x0val y0val                              
		                lassign [lindex $pntList [expr {$icount+1}]] x1val y1val                                                             
		                set xval $x1val               
		                set yval $y1val 
		                if {[lsearch -exact $yaxis(rightside) $ifunction] != -1} {
		                    set yaxismaxpos $yaxismaxright
		                    set yaxisminpos $yaxisminright
		                } else {
		                    set yaxismaxpos $yaxismaxleft
		                    set yaxisminpos $yaxisminleft
		                }
		                if {[$self isinbox $x0val $y0val $xaxismin $xaxismax $yaxisminpos $yaxismaxpos] && \
		                    [$self isinbox $x1val $y1val $xaxismin $xaxismax $yaxisminpos $yaxismaxpos]} {                                                                                                            
		                    set x0val [expr $xm+($x0val-$xaxismin)*$xfact]  
		                    set x1val [expr $xm+($x1val-$xaxismin)*$xfact] 
		                    if {[lsearch -exact $yaxis(rightside) $ifunction] != -1} {
		                        set y0val [expr $ymax-$ym-($y0val-$yaxisminpos)*$yfactright]  
		                        set y1val [expr $ymax-$ym-($y1val-$yaxisminpos)*$yfactright]                                   
		                        set yaxisside right
		                    } else {
		                        set y0val [expr $ymax-$ym-($y0val-$yaxisminpos)*$yfactleft]
		                        set y1val [expr $ymax-$ym-($y1val-$yaxisminpos)*$yfactleft]                                   
		                        set yaxisside left
		                    }                                                                                                                      
		                    if {!$options(-scatterplot)} {
		                        $canvas create line $x0val $y0val $x1val $y1val \
		                            -tags "curve yaxisside=$yaxisside function=$i" -width 3 \
		                            -fill [lindex $options(-colorset) $i]
		                        append xml [$canvas drawline_svg $x0val $y0val $x1val $y1val \
		                                [lindex $options(-colorset) $i] 1 [lindex $options(-colorset) $i]]                                         
		                    } else {                                      
		                        $canvas create oval [expr $x0val-2.8] [expr $y0val-2.8] \
		                            [expr $x0val+2.8] [expr $y0val+2.8] -width 1 \
		                            -outline darkblue -fill [lindex $options(-colorset) $i] \
		                            -tags "curvepoint curve yaxisside=$yaxisside function=$i"
		                        append xml [$canvas drawcircle_svg $x0val $y0val 2.5 \
		                                [lindex $options(-colorset) $i] 1 [lindex $options(-colorset) $i]]                                                 
		                        $canvas create oval [expr $x1val-2.8] [expr $y1val-2.8] \
		                            [expr $x1val+2.8] [expr $y1val+2.8] -width 1 \
		                            -outline darkblue -fill [lindex $options(-colorset) $i] \
		                            -tags "curvepoint curve yaxisside=$yaxisside function=$i"
		                        append xml [$canvas drawcircle_svg $x1val $y1val 2.5 \
		                                [lindex $options(-colorset) $i] 1 [lindex $options(-colorset) $i]]
		                    }                                     
		                }                               
		            }                              
		        }                    
		    }             
		} else {                
		    set xval [expr $xm+($xval-$xaxismin)*$xfact]                               
		    if {[lsearch -exact $yaxis(rightside) $ifunction] != -1} {
		        set yval [expr $ymax-$ym-($yval-$yaxisminright)*$yfactright]
		        set yaxisside right
		    } else {
		        set yval [expr $ymax-$ym-($yval-$yaxisminleft)*$yfactleft]                    
		        set yaxisside left
		    }
		    if { $j > 0 } {
		        if {!$options(-scatterplot)} {
		            $canvas create line $prevxval $prevyval $xval $yval \
		                -tags "curve yaxisside=$yaxisside function=$i" -width 3 \
		                -fill [lindex $options(-colorset) $i]
		            append xml [$canvas drawline_svg $prevxval $prevyval $xval $yval \
		                    [lindex $options(-colorset) $i] 1 [lindex $options(-colorset) $i]]   
		        } else {                           
		            $canvas create oval [expr $prevxval-2.8] [expr $prevyval-2.8] \
		                [expr $prevxval+2.8] [expr $prevyval+2.8] -width 1 \
		                -outline darkblue -fill [lindex $options(-colorset) $i] \
		                -tags "curvepoint curve yaxisside=$yaxisside function=$i"
		            append xml [$canvas drawcircle_svg $prevxval $prevyval 2.5 \
		                    [lindex $options(-colorset) $i] 1 [lindex $options(-colorset) $i]]
		            $canvas create oval [expr $xval-2.8] [expr $yval-2.8] \
		                [expr $xval+2.8] [expr $yval+2.8] -width 1 \
		                -outline darkblue -fill [lindex $options(-colorset) $i] \
		                -tags "curvepoint curve yaxisside=$yaxisside function=$i"
		            append xml [$canvas drawcircle_svg $xval $yval 2.5 \
		                    [lindex $options(-colorset) $i] 1 [lindex $options(-colorset) $i]]
		        }
		    } 
		    if { $j == $numdivisions && $options(-scatterplot) } {                      
		        $canvas create oval [expr $xval-2.8] [expr $yval-2.8] \
		            [expr $xval+2.8] [expr $yval+2.8] -width 1 \
		            -outline darkblue -fill [lindex $options(-colorset) $i] \
		            -tags "curvepoint curve yaxisside=$yaxisside function=$i"
		        append xml [$canvas drawcircle_svg $xval $yval 2.5 \
		                [lindex $options(-colorset) $i] 1 [lindex $options(-colorset) $i]]
		    }  

		    set prevxval $xval               
		    set prevyval $yval
		}                
	    }
	}      
	append xml "</svg>"

	$canvas bind curve <ButtonPress-1> [mymethod draw_graph_coords %x %y]        
	$canvas bind curvepoint <ButtonPress-1> [mymethod draw_graph_coords %x %y] 
	
	$canvas bind legendbox <ButtonPress-1> [mymethod move_entity legendbox BP1 %x %y]        
	$canvas bind legendbox <B1-Motion> [mymethod move_entity legendbox BM1 %x %y]                   
    }  
    
    method isinbox { x y xaxismin xaxismax yaxismin yaxismax } {
	variable yaxis  
	
	set isinbox 1    
	if { $x < $xaxismin || $x > $xaxismax || $y < $yaxismin || $y > $yaxismax } {
	    set isinbox 0
	}            
	return $isinbox
    }    
    method islineinbox { prevx prevy x y ifunction xaxismin xaxismax yaxismin yaxismax } {  
	set pntList ""    
	# xaxismin
	if {[expr abs($x-$prevx)] > 1e-30} {           
	    set yval [expr {($y-$prevy)*(($xaxismin-$prevx)/double($x-$prevx))+$prevy}]        
	    if {[$self isinbox $xaxismin $yval $xaxismin $xaxismax $yaxismin $yaxismax] && \
		$xaxismin > $prevx && $xaxismin < $x && (($yval > $prevy && $yval < $y) || \
		    ($yval > $y && $yval < $prevy))} {
		lappend pntList "$xaxismin $yval" 
	    }
	} 
	# xaxismax 
	if {[expr abs($x-$prevx)] > 1e-30} {                  
	    set yval [expr {($y-$prevy)*(($xaxismax-$prevx)/double($x-$prevx))+$prevy}]        
	    if {[$self isinbox $xaxismax $yval $xaxismin $xaxismax $yaxismin $yaxismax] && \
		$xaxismax > $prevx && $xaxismax < $x && (($yval > $prevy && $yval < $y) || \
		    ($yval > $y && $yval < $prevy))} {
		lappend pntList "$xaxismax $yval" 
	    }
	}       
	# yaxismin
	if {[expr abs($y-$prevy)] > 1e-30} {                   
	    set xval [expr {($x-$prevx)*(($yaxismin-$prevy)/double($y-$prevy))+$prevx}]         
	    if {[$self isinbox $xval $yaxismin $xaxismin $xaxismax $yaxismin $yaxismax] && \
		$xval > $prevx && $xval < $x && (($yaxismin > $prevy && $yaxismin < $y) || \
		    ($yaxismin > $y && $yaxismin < $prevy))} {
		lappend pntList "$xval $yaxismin" 
	    }
	} 
	# yaxismax 
	if {[expr abs($y-$prevy)] > 1e-30} {          
	    set xval [expr {($x-$prevx)*(($yaxismax-$prevy)/double($y-$prevy))+$prevx}]                 
	    if {[$self isinbox $xval $yaxismax $xaxismin $xaxismax $yaxismin $yaxismax] && \
		$xval > $prevx && $xval < $x && (($yaxismax > $prevy && $yaxismax < $y) || \
		    ($yaxismax > $y && $yaxismax < $prevy))} {
		lappend pntList "$xval $yaxismax" 
	    }
	} 
	if {$pntList != ""} {
	    lappend pntList "$prevx $prevy" "$x $y"
	}
	return [list [lsort -index 0 -real $pntList]]
    }
    method move_entity { entitytype action x y } {
	set c $self
	set tag [$c gettags $entitytype]          
	switch $action {
	    BP1 {
		$c raise $tag
	    }
	    BM1 {               
		lassign [$c coords $tag] x0 y0 - - x1 y1 - -  
		if {[string is double $x0] && [string is double $x1]} {
		    $c move $tag [expr $x-$x0] [expr $y-$y0] 
		}
	    }
	}
    }            
    method find_closest_point { x y } {      
	set c $self
	set mindist2 1e20
	foreach i [$c find withtag curve] {
	    foreach "ax ay bx by" [$c coords $i] break
	    if {$ax == $bx && $ay == $by} { continue }
	    set vx [expr $bx-$ax]
	    set vy [expr $by-$ay]
	    set alpha [expr $vx*($ax-$x)+$vy*($ay-$y)]
	    set landa [expr -1*$alpha/double($vx*$vx+$vy*$vy)]
	    if { $landa < 0.0 } { set landa 0.0 }
	    if { $landa > 1.0 } { set landa 1.0 }
	    set px [expr $ax+$landa*$vx]
	    set py [expr $ay+$landa*$vy]
	    
	    set dist2 [expr ($px-$x)*($px-$x)+($py-$y)*($py-$y)]
	    if { $dist2 < $mindist2 } {
		set mindist2 $dist2
		set minpx $px
		set minpy $py
	    }
	}
	return [list $minpx $minpy]
    }       
    method draw_graph_coords { x y } {
	set c $self
	
	$c delete coords
	$c delete curvecurrent
	
	set tagsList [$c gettags current]   
	  
	regexp {yaxisside=(\S+) function=(\S+)} $tagsList {} yaxisside numfunction
	
	set functionname [lindex $options(-functionnames) $numfunction]
	
	if {$options(-canvasheightset) != ""} {
	    set ymax $options(-canvasheightset)  
	} else {            
	    set ymax [winfo height $c]
	}
	
	if {$options(-canvaswidthset) != ""} {
	    set xmax $options(-canvaswidthset)  
	} else {
	    set xmax [winfo width $c]                        
	}     
		
	foreach "xcurve ycurve" [$self find_closest_point $x $y] break
		
	$c create oval [expr $xcurve-2.5] [expr $ycurve-2.5] [expr $xcurve+2.5] [expr $ycurve+2.5] \
	    -fill red -outline darkblue -width 1 -tags "curvecurrent"
	
	if {$options(-xyaxisauto)} {
	    set xtext [expr ($xcurve-$xm)/double($xfact)+[lindex $options(-xaxismin) 0]]
	} else {
	    set xtext [expr ($xcurve-$xm)/double($xfact)+[lindex $options(-xaxismin) 1]]
	}
	regsub {e([+-])00} $xtext {e\10} xtext
	
	if {$yaxisside == "right"} {
	    if {$options(-xyaxisauto)} {
		set ytext [expr ($ymax-$ym-$ycurve)/double($yfactright)+[lindex $options(-yaxisminright) 0]]            
	    } else {
		set ytext [expr ($ymax-$ym-$ycurve)/double($yfactright)+[lindex $options(-yaxisminright) 1]]  
	    }
	} elseif {$yaxisside == "left"} {
	    if {$options(-xyaxisauto)} {
		set ytext [expr ($ymax-$ym-$ycurve)/double($yfactleft)+[lindex $options(-yaxisminleft) 0]]
	    } else {
		set ytext [expr ($ymax-$ym-$ycurve)/double($yfactleft)+[lindex $options(-yaxisminleft) 1]]
	    }
	}
	
	regsub {e([+-])00} $ytext {e\10} ytext  
	      
	set xtextwidth [font measure $options(-font) \
		[format "%s: %.4g %s" $options(-xlabel) $xtext $options(-xlabelunit)]]        
	set xtextwidth2 [font measure $options(-font) \
		[format "%s: %.4g %s" $options(-ylabel) $ytext $options(-ylabelunit)]]  
	if {$options(-multiplegraphs)} {
	    set xtextwidth3 [font measure $options(-font) \
		    [format "%s %s %.4g" $functionname $options(-xlabel) $xtext]]          
	} else {
	    set xtextwidth3 [font measure $options(-font) $options(-title)]    
	}
	if {$xtextwidth2 > $xtextwidth } { set xtextwidth $xtextwidth2 }
	if {$xtextwidth3 > $xtextwidth } { set xtextwidth $xtextwidth3 }
	
	set textheight [font metrics $options(-font) -linespace]                      
	
	$c create rectangle [expr $xcurve-$xtextwidth/2.0-3.5] [expr $ycurve-4.0] \
	    [expr $xcurve+$xtextwidth/2.0+3.5] [expr $ycurve-3.0*$textheight-4.0] \
	    -fill "LightYellow" -width 1 -tags coords -outline black              
		 
	if {$options(-multiplegraphs)} {
	    $c create text $xcurve [expr $ycurve-3*$textheight-2.5] -anchor n -font $options(-font) \
		-text [format "%s\n%s: %.4g %s\n %s: %.4g %s" \
		    $functionname $options(-xlabel) \
		    $xtext $options(-xlabelunit) [lindex $options(-functionnames) $numfunction] $ytext $options(-ylabelunit)] -justify left -tags coords    
	} else {
	    $c create text $xcurve [expr $ycurve-3*$textheight-2.5] -anchor n -font $options(-font) \
		-text [format "%s\n%s: %.4g %s\n %s: %.4g %s" \
		    $options(-title) $options(-xlabel) $xtext $options(-xlabelunit) $options(-ylabel) \
		    $ytext $options(-ylabelunit)] -justify left -tags coords  
	}
	
	$c bind curve <ButtonRelease-1> "$c delete coords coordpoint curvecurrent; $c bind curve <B1-Motion> {}"                
    }   
    method colorchoose {} {
	set w .backgroundcolor
	catch {destroy $w}
	set w [dialogwin_snit $w -title [_ "Graphical appearance"] \
		-callback [mymethod colorchoose_do] -grab 1 -transient 1]
	set f [$w giveframe]
	
	wm iconname $w [_ "colors"]
	
	set msgbg [ttk::label $f.msgbg -text [_ "Background color view"]]                     
	set lbg [label $f.lbg -text "  " -relief solid -bd 1 -width 3 -background $options(-backgroundcolor)]
	tooltip::tooltip $lbg [_ "Color view"]        
	set bbg [cu::menubutton_button $f.bbg -command [mymethod setcolor $w background background $lbg] -image [cu::get_image colors-16] -menu $f.bbg.m]
	tooltip::tooltip $bbg [_ "Color selection"]
	menu $f.bbg.m -tearoff 0      
	$f.bbg.m add command -label [_ "Select color"]... -image [cu::get_image colors-16] \
	    -command [mymethod setcolor $w background background $lbg] -compound left
	$f.bbg.m add separator
	$f.bbg.m add command -label [_ "Initial value"] -command [mymethod initialcolor $w backgroundcolor $lbg] -image \
	    [formulae::create_give_image_color $options(-backgroundcolor)] -compound left
	
	set msgfg [ttk::label $f.msgfg -text [_ "Foreground color view"]]                     
	set lfg [label $f.lfg -text "  " -relief solid -bd 1 -width 3 -background $options(-foregroundcolor)]
	tooltip::tooltip $lfg [_ "Color view"]        
	set bfg [cu::menubutton_button $f.bfg -command [mymethod setcolor $w background foreground $lfg] \
		-image [cu::get_image colors-16] -menu $f.bfg.m]
	tooltip::tooltip $bfg [_ "Color selection"]
	menu $f.bfg.m -tearoff 0      
	$f.bfg.m add command -label [_ "Select color"]... -image [cu::get_image colors-16] \
	    -command [mymethod setcolor $w background foreground $lfg] -compound left
	$f.bfg.m add separator                   
	$f.bfg.m add command -label [_ "Initial value"] -command [mymethod initialcolor $w foregroundcolor $lfg] -image \
	    [formulae::create_give_image_color $options(-foregroundcolor)] -compound left             
		  
	#### 
	cu::combobox $f.fx -textvariable [$w give_uservar fx] \
	    -values $options(-functionnames) -width 20 -state readonly 
	tooltip::tooltip $f.fx [_ "Selected functions"]                 
	set lfg2 [label $f.lfg2 -text "  " -relief solid -bd 1 -width 3 -background [lindex $options(-colorset) 0]]
	tooltip::tooltip $lfg2 [_ "Color view"]        
	set bfg2 [cu::menubutton_button $f.bfg2 -command [mymethod setcolor $w background function $lfg2] \
		-image [cu::get_image colors-16] -menu $f.bfg2.m]
	tooltip::tooltip $bfg2 [_ "Color selection"]
	menu $f.bfg2.m -tearoff 0      
	$f.bfg2.m add command -label [_ "Select color"]... -image [cu::get_image colors-16] \
	    -command [mymethod setcolor $w background function $lfg2] -compound left       
	 
	ttk::checkbutton $f.cb -text [_ "Add grid lines to graph"] \
	    -variable [$w give_uservar addgridlines]     
		
	tooltip::tooltip $f.cb [_ "To control the presence and appearance of grid lines on the graph"]         
	set lfg1 [label $f.lfg1 -text "  " -relief solid -bd 1 -width 3 -background $options(-gridlinescolor)]
	tooltip::tooltip $lfg1 [_ "Color view"]        
	set bfg1 [cu::menubutton_button $f.bfg1 -command [mymethod setcolor $w background grid $lfg1] \
		-image [cu::get_image colors-16] \
		-menu $f.bfg1.m]
	tooltip::tooltip $bfg1 [_ "Color selection"]
	menu $f.bfg1.m -tearoff 0      
	$f.bfg1.m add command -label [_ "Select color"]... -image [cu::get_image colors-16] \
	    -command [mymethod setcolor $w background grid $lfg1] -compound left
	$f.bfg1.m add separator                   
	$f.bfg1.m add command -label [_ "Initial value"] -command [mymethod initialcolor $w gridlinescolor $lfg1] -image \
	    [formulae::create_give_image_color $options(-gridlinescolor)] -compound left  
	       
	grid $msgbg $lbg $bbg -sticky nsew -padx 2 -pady 2 
	grid $msgfg $lfg $bfg -sticky nsew -padx 2 -pady 2               
	grid $f.fx $lfg2 $bfg2 -sticky news -padx 2 -pady 2     
	grid $f.cb $lfg1 $bfg1 -sticky nsew -padx 2 -pady 2        
	
	grid columnconfigure $f 1 -weight 1
	
	$w set_uservar_value addgridlines $options(-addgridlines)
	$w set_uservar_value fx [lindex $options(-functionnames) 0] 
	$w set_uservar_value colorset $options(-colorset)
	
	set cmd_fx "[namespace code [mymethod updatecolor $w $lfg2]];#"
	trace add variable [$w give_uservar fx] write $cmd_fx
	bind $w <Destroy> +[list trace remove variable [$w give_uservar fx] write $cmd_fx] 
	      
	bind $w <Return> [list $w invokeok]
	 
	$w createwindow
    }
    method updatecolor { w label } {
	set fx [$w give_uservar_value fx]
	set colorset [$w give_uservar_value colorset]
	set ipos [lsearch -exact $options(-functionnames) $fx]
	$label config -background [lindex $colorset $ipos]
    }
    method initialcolor { w name label } {        
	$label config -background $options(-$name)                   
    }
    method setcolor { w intname name label } {       
	set c $self
	
	set f [$w giveframe]
	
	grab $w
	set initialColor [$label cget -$intname]        
	set color [tk_chooseColor -title [_ "Choose a %s color" $name] -parent $w \
		-initialcolor $initialColor]
	if {[string compare $color ""]} {
	    $label config -$intname $color            
	}
	if {$name eq "function"} {
	    set fx [$w give_uservar_value fx]
	    set ipos [lsearch -exact $options(-functionnames) $fx]
	    set colorset [$w give_uservar_value colorset]
	    $w set_uservar_value colorset [lreplace $colorset $ipos $ipos $color]
	}
	grab release $w                        
    }
    method colorchoose_do { w  } {
	set action [$w giveaction]
	
	set f [$w giveframe]
		 
	switch -- $action {
	    1 {                           
		set options(-backgroundcolor) [$f.lbg cget -background]
		set options(-foregroundcolor) [$f.lfg cget -background]
		set options(-addgridlines) [$w give_uservar_value addgridlines]
		set options(-gridlinescolor) [$f.lfg1 cget -background]  
		set options(-colorset) [$w give_uservar_value colorset]            
		destroy $w                
		catch { $self draw } errstring
	    }            
	    -1 - 0 {               
		destroy $w
	    }                      
	}    
    }   
}
    
if 0 {   
    # interp alias "" _ "" msgcat::mc

#     pack [cu::draw_graphs .d -xyvalues [list "1 1" "2 1.02" "3 1.05" "4 0.99" "5 0.96" \
#                 "6 6" "7 7" "8 8"] \
#             -xlabel t -ylabel f -title "My function" -xlabelunit "s" -ylabelunit "N"] -fill both -expand 1  
    
#     pack [cu::draw_graphs .d -xyvalues [list "1 1" "2 5" "3 3" "4 2" "5 7" \
#                 "6 6" "7 7" "8 8"] \
#             -xlabel t -ylabel f -title "My function" -xlabelunit "s" -ylabelunit "N"] -fill both -expand 1   

#     pack [cu::draw_graphs .d -xyvalues [list "-10 2" "1 3" "2 4" "3 5" "4 6" \
#                 "5 7" "8 15" "9 8" "10 9"] \
#             -xlabel t -ylabel f -title "My function" -xlabelunit "s" -ylabelunit "N"] -fill both -expand 1   

#     pack [cu::draw_graphs .d -xyvalues [list "0 20"] -xlabel "t" -ylabel "f" -title "My point" \
#             -functionnames "Function" -scatterplot 1] -fill both -expand 1   
    
#     pack [cu::draw_graphs .d -xyvalues [list "0.0 -1.0e-20" "0.1 1.0e+20 0.2 -1.0e-20"] -xlabel "t" -ylabel "f" -title "My point" \
#             -functionnames "Function" -scatterplot 1] -fill both -expand 1
 
#     pack [cu::draw_graphs .d -xyvalues [list [list "-5 2" "-4 -1" "2 -3" "3 2" "4 -2.5"] \
#                 [list "-5 -2" "-4 3" "0.5 -1" "5 4"] [list "-5 -2" "-4 3" "5 -1" "5 4"]] -xlabel "time" -ylabel "f" -title "My functions" \
#             -functionnames [list "Function 1" "Function 2" "Function 3"] -multiplegraphs 1 \
#         -showlegend 1 -xlabelunit "s" -ylabelunit "m" ] -fill both -expand 1   
      
#     pack [cu::draw_graphs .d -xyvalues [list [list {1.0 0.0} {2.0 2.0} {4.0 2.0} {5.0 0.0}] \
#                 [list {0 1} {3 1} {6 1}]] -xlabel "time" -ylabel "f" -title "My functions" \
#             -functionnames [list "Function 1" "Function 2"] -multiplegraphs 1 \
#             -showlegend 1 -xlabelunit "s" -ylabelunit "m" ] -fill both -expand 1     
#     
#     pack [cu::draw_graphs .d -xyvalues [list [list "0 1" "2 3" "5 7"] \
#                 [list "0 4" "2 6" "3 5"]] -xlabel "s" -functionnames [list "\u0192" "\u019F"] -multiplegraphs 1 \
#             -backgroundcolor white -showlegend 1 -colorset [list "medium slate blue" oliveDrab1] \
#             -canvaswidthset ""] -fill both -expand 1 
#     
#     pack [cu::draw_graphs .d -xyvalues {{{1 4.733341e-13} {2 9.466683e-13} {3 1.420002e-12} {4 1.893337e-12} \
#                 {5 2.366671e-12} {6 2.840005e-12} {7 3.313339e-12} {8 3.786673e-12} {9 4.260007e-12} {10 4.733341e-12}}} \
#             -xlabel "X" -ylabel "Y" -title "" -functionnames [list "Steps vs Desp-X"] -multiplegraphs 1 -showlegend 1] -fill both -expand 1
 
#     pack [cu::draw_graphs .d -xyvalues [list [list {0.0 0.0} {0.1 0.010000000000000002} {0.2 0.04000000000000001} {0.30000000000000004 0.09000000000000002} {0.4 0.16000000000000003} {0.5 0.25} {0.6 0.36} {0.7 0.48999999999999994} {0.7999999999999999 0.6399999999999999} {0.8999999999999999 0.8099999999999998} {0.9999999999999999 0.9999999999999998} {1.0999999999999999 1.2099999999999997} {1.2 1.44} {1.3 1.6900000000000002} {1.4000000000000001 1.9600000000000004} {1.5000000000000002 2.250000000000001} {1.6000000000000003 2.560000000000001} {1.7000000000000004 2.8900000000000015} {1.8000000000000005 3.2400000000000015} {1.9000000000000006 3.610000000000002} {2.0000000000000004 4.000000000000002} {2.1000000000000005 4.410000000000002} {2.2000000000000006 4.8400000000000025} {2.3000000000000007 5.290000000000004} {2.400000000000001 5.760000000000004} {2.500000000000001 6.250000000000004}] \
#                 [list {0.0 0.0} {0.1 0.31622776601683794} {0.2 0.4472135954999579} {0.30000000000000004 0.5477225575051662} {0.4 0.6324555320336759} {0.5 0.7071067811865476} {0.6 0.7745966692414834} {0.7 0.8366600265340756} {0.7999999999999999 0.8944271909999159} {0.8999999999999999 0.9486832980505138} {0.9999999999999999 0.9999999999999999} {1.0999999999999999 1.0488088481701514} {1.2 1.0954451150103321} {1.3 1.140175425099138} {1.4000000000000001 1.1832159566199232} {1.5000000000000002 1.2247448713915892} {1.6000000000000003 1.2649110640673518} {1.7000000000000004 1.30384048104053} {1.8000000000000005 1.341640786499874} {1.9000000000000006 1.3784048752090223} {2.0000000000000004 1.4142135623730951} {2.1000000000000005 1.449137674618944} {2.2000000000000006 1.4832396974191329} {2.3000000000000007 1.5165750888103104} {2.400000000000001 1.549193338482967} {2.500000000000001 1.58113883008419} ]] -xlabel "time" -ylabel "f" -title "My functions" \
#             -functionnames [list "Function 1" "Function 2"] -multiplegraphs 1 \
#             -showlegend 1 -xlabelunit "s" -ylabelunit "m" ] -fill both -expand 1    
}
