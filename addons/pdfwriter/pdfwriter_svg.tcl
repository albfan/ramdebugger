
catch {
    package require img::gif
    package require img::jpeg
    package require img::png
}
package require base64
package require textutil

namespace eval PDFWriter::svg {
    variable svgfile
    variable xml
    variable fonts
    variable xmax
    variable ymax
    variable xfac
    variable yfac
    variable xadd
    variable yadd
    variable ymax_real
    variable cacheimages
    variable fontscale

    namespace export *
}

proc PDFWriter::svg::cm2pt { cm } {
    return [expr $cm*72/2.54]
}

proc PDFWriter::svg::pt2cm { pt } {
    return [expr $pt*2.54/72.0]
}

proc PDFWriter::svg::_map { args } {
    variable xfac
    variable yfac
    variable xadd
    variable yadd
    variable ymax_real

    if { [llength $args] == 1 } { set args [lindex $args 0] }
    set l ""
    foreach "x y" $args {
	set newx [expr {$x*$xfac+$xadd}]
	set newy [expr {$y*$yfac+$yadd}]
	if { $newy > $ymax_real } { set ymax_real $newy }
	lappend l $newx $newy
    }
    return $l
}

proc PDFWriter::svg::_check_ymax { ymax_test } {
    variable ymax_real
    if { $ymax_test > $ymax_real } { set ymax_real $ymax_test }
}

proc PDFWriter::svg::GiveEncoding {} {
    return utf-8
}

proc PDFWriter::svg::Init { { papertype A4 } { _svgfile "" } } {
    variable svgfile
    variable xml
    variable fonts
    variable xmax
    variable ymax
    variable ymax_real
    variable xfac
    variable yfac
    variable xadd
    variable yadd
    variable fontscale

    if { $_svgfile == "" } {
	set ic 0
	while { $ic < 50 } {
	    set tmpdir /windows/temp
	    catch { set tmpdir [GiveTempDir] }
	    set svgfile [file join $tmpdir tmpfile$ic.svg]
	    if { [file exists $svgfile] && [clock seconds] - [file mtime $svgfile] < 30 } {
		incr ic; continue
	    }
	    break
	    incr ic
	}
	if { $ic == 50 } {
	    error [_ "Error: could not open temporal file"]
	}
    } else {
	set svgfile $_svgfile
    }

    switch -glob -- $papertype {
	A4 {
	    set xmax [cm2pt 21]
	    set ymax [cm2pt 29.7]
	    set bigmarginh [cm2pt 1.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	    set orientation portrait
	}
	A4L {
	    set xmax [cm2pt 29.7]
	    set ymax [cm2pt 21]
	    set bigmarginh [cm2pt 2.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	    set orientation landscape
	}
	A3 {
	    set xmax [cm2pt 29.7]
	    set ymax [cm2pt 42.0]
	    set bigmarginh [cm2pt 1.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	    set orientation portrait
	}
	A3L {
	    set xmax [cm2pt 42.0]
	    set ymax [cm2pt 29.7]
	    set bigmarginh [cm2pt 1.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	    set orientation landscape
	}
	userdefined* {
	    set xmax [cm2pt [lindex $papertype 1]]
	    set ymax [cm2pt [lindex $papertype 2]]
	    if { [llength $papertype] > 3 } {
		set bigmarginh [cm2pt [lindex $papertype 3]]
		set bigmarginv [cm2pt [lindex $papertype 4]]
	    } else {
		set bigmarginh [cm2pt 1.5]
		set bigmarginv [cm2pt 1.5]
	    }
	    set ysep 5
	    set orientation portrait
	}
	default {
	    error "error unknown paper type"
	}
    }

    set xfac 1.0
    set xadd 0.0
    set yfac -1.0
    set yadd $ymax
    set ymax_real 0.0

    set xml ""
    append xml "<svg width='$xmax' height='$ymax' "
    append xml "viewBox='0 0 $xmax $ymax' version='1.1' "\
	"xmlns='http://www.w3.org/2000/svg' "\
	"xmlns:xlink='http://www.w3.org/1999/xlink' "\
	"xmlns:ramdraw='http://example.org/ramdraw'>\n"

    set fontscale 0.8
    #set font {MS Sans Serif}
    set font Tahoma
    set fonts(Helvetica) [list -family $font]
    set fonts(Helvetica-Bold) [list -family $font -weight bold]

    set fonts(Times-Roman) [list -family $font]
    set fonts(Times-Bold) [list -family $font -weight bold]
    set fonts(Times-Italic) [list -family $font -slant italic]
    set fonts(Times-BoldItalic) [list -family $font -weight bold -slant italic]
    set fonts(Helvetica-Oblique) [list -family $font]
    set fonts(Helvetica-BoldOblique) [list -family $font -weight bold]
    set fonts(Courier-Bold) [list -family {Courier} -weight bold]
    set fonts(Courier) [list -family {Courier}]
    set fonts(Symbol) [list -family {Symbol}]

    return [list $xmax $ymax $bigmarginh $bigmarginv $ysep]
    
}

proc PDFWriter::svg::GiveTempDir {} {
    variable tmpdir

    if { [info exists tmpdir] } {
	return $tmpdir
    }

    if { [info exists ::env(TEMP)] && [file isdir $::env(TEMP)] } {
	set tmpdir $::env(TEMP)
    } elseif { [info exists ::env(TMP)] && [file isdir $::env(TMP)] } {
	set tmpdir $::env(TMP)
    } elseif { [info exists ::env(WINDIR)] && [file isdir [file join $::env(WINDIR) temp]] } {
	set tmpdir [file join $::env(WINDIR) temp]
    } elseif { [file isdir /tmp] } {
	set tmpdir /tmp
    } else {
	set tmpdir /
    }
    return $tmpdir
}

proc PDFWriter::svg::AddBookmark { text parent } {


    return 0

}

proc PDFWriter::svg::NewPage { { papertype "" } } {
    variable xmax
    variable ymax

    return [list $xmax $ymax]
}


# TCL font is supposed to be in the format: -size .. -family ..
proc PDFWriter::svg::TclfontToPDF { font sizedefault } {

    set ipos [lsearch $font -size]
    if { $ipos != -1 } {
	incr ipos
	set size [lindex $font $ipos]
	set sizefac [expr double($size)/double($sizedefault)]
    } else { set sizefac 1.0 }
    
    set ipos [lsearch $font -family]
    if { $ipos != -1 } {
	incr ipos
	set family [lindex $font $ipos]
	if { [string tolower $family] == "symbol" } {
	    return [list Symbol $sizefac]
	}
    }
    if { [lsearch $font bold] != -1 } {
	if { [lsearch $font italic] != -1 } {
	    return [list Helvetica-BoldOblique $sizefac]
	} else {
	    return [list Helvetica-Bold $sizefac]
	}
    } elseif { [lsearch $font italic] != -1 } {
	return [list Helvetica-Oblique $sizefac]
    } else {
	return [list Helvetica $sizefac]
    }
}

proc PDFWriter::svg::_parse_scale { scale width height } {
    
    if { [regexp {(?i)(h|v)=([0-9.]+)cm} $scale {} orient cms] } {
	switch $orient {
	    h { set scale [expr [cm2pt $cms]/double($width)] }
	    v { set scale [expr [cm2pt $cms]/double($height)] }
	}
    }
    if { [regexp {(?i)(h|v)=([0-9.]+)pt} $scale {} orient pts] } {
	switch $orient {
	    h { set scale [expr $pts/double($width)] }
	    v { set scale [expr $pts/double($height)] }
	}
    }
    return $scale
}

# scale can be either a number or: h=2.3cm or v=1cm or h=234pt or two of
# the previous separated by comma

proc PDFWriter::svg::InsertImageNoDisk { image x y anchor { scale 1 } } {
    variable xml

    set width [image width $image]
    set height [image height $image]
    
    if { [llength [split $scale ","]] == 2 } {
	lassign [split $scale ","] scalex scaley
	foreach i [list scalex scaley] {
	    set $i [_parse_scale [set $i] $width $height]
	}
    } else {
	set scale [_parse_scale $scale $width $height]
	lassign [list $scale $scale] scalex scaley
    }
    if { $anchor eq "center" } { set anchor "" }
    if { [string first n $anchor] != -1 } {
	# nothing
    } elseif { [string first s $anchor] != -1 } {
	set y [expr $y+$height*$scaley]
    } else {
	set y [expr $y+$height*$scaley/2.0]
    }
    if { [string first w $anchor] != -1 } {
	# nothing
    } elseif { [string first e $anchor] != -1 } {
	set x [expr $x-$width*$scalex]
    } else {
	set x [expr $x-$width*$scalex/2.0]
    }
    
    foreach "x y" [_map $x $y] break
    append xml "<image x='$x' y='$y' width='[expr {$width*$scalex}]' "\
	"height='[expr {$height*$scaley}]' xlink:href='$image'/>"
    _check_ymax [expr {$y+$height*$scaley}]
    return [list [expr $width*$scalex] [expr $height*$scaley]]
}

proc PDFWriter::svg::InsertImageFromData { imgdata x y anchor { scale 1 } } {

    if { [string range $imgdata 0 2] == "GIF" } {
	set type gif
    } elseif { [string range $imgdata 1 3] == "PNG" } {
	set type png
    } else {
	set type jpg
    }
    set file [file join [GiveTempDir] tmpimage.$type]

    set fout [open $file w]
    fconfigure $fout -translation binary
    puts -nonewline $fout $imgdata
    close $fout

    return [InsertImageFile $file $x $y $anchor $scale]
}

proc PDFWriter::svg::InsertImage { imagename x y anchor { scale 1 } } {

    set type [set Images::${imagename}_data_type]
    set file [file join [GiveTempDir] $imagename.$type]

    if { ![file exists $file] || [file size $file] == 0 } {
	#$image write $file -format $type
	set fout [open $file w]
	fconfigure $fout -translation binary
	puts -nonewline $fout [base64::decode [set Images::${imagename}_data]]
	close $fout
    }
    return [InsertImageFile $file $x $y $anchor $scale]
}

proc PDFWriter::svg::InsertImg { img x y anchor { scale 1 } } {
    variable cacheimages
    
    set imgdata [base64::decode [$img data -format png]]

    if { [string range $imgdata 0 2] == "GIF" } {
	set type gif
    } elseif { [string range $imgdata 1 3] == "PNG" } {
	set type png
    } else {
	set type jpg
    }
    set file [file join [GiveTempDir] tmpimage.$type]
    if { [info exists cacheimages($file)] } {
	# WARNING: necessary to delete this image after destroying the widget
	#image delete $cacheimages($file)
	unset cacheimages($file)
    }
    set fout [open $file w]
    fconfigure $fout -translation binary
    puts -nonewline $fout $imgdata
    close $fout

    return [InsertImageFile $file $x $y $anchor $scale]
}

proc PDFWriter::svg::_trycopyingfile { file } {

    # useful for tclkit
    if { ![file exists $file] } { return "" }

    if { [info exists ::env(APPDATA)] } {
	set dir $::env(APPDATA)
    } elseif { [info exists ::env(TEMP)] } {
	set dir $::env(TEMP)
    } elseif { [file isdirectory /WINDOWS/Temp] } {
	set dir /WINDOWS/Temp
    } else { set dir c:/ }

    set base [file root [file tail [info nameofexecutable]]]
    if { $base == "" } {
	set base [file root [file tail $::argv0]]
    }
    if { $base != "" } {
	set dir [file join $dir $base]
	file mkdir $dir
    }
    if { ![file exists [file join $dir [file tail $file]]] } {
	file copy $file $dir
    }
    return [file join $dir [file tail $file]]
}

# scale can be either a number or: h=2.3cm or v=1cm or h=234pt or two of
# the previous separated by comma

proc PDFWriter::svg::InsertImageFile { file x y anchor { scale 1 } } {
    variable cacheimages
    variable xml

    switch [file ext $file] {
	.gif { set type gif }
	.jpg - .jpeg { set type jpeg }
	.png { set type png }
	default {
	    error "unknown extension for image file '$file'"
	    return
	}
    }
    if { [info exists cacheimages($file)] } {
	set image $cacheimages($file)
    } else {
	set image [image create photo -file $file]
	set cacheimages($file) $image
    }

    set width [image width $image]
    set height [image height $image]

    if { [llength [split $scale ","]] == 2 } {
	lassign [split $scale ","] scalex scaley
	foreach i [list scalex scaley] {
	    set $i [_parse_scale [set $i] $width $height]
	}
    } else {
	set scale [_parse_scale $scale $width $height]
	lassign [list $scale $scale] scalex scaley
    }
    if { $anchor eq "center" } { set anchor "" }
    if { [string first n $anchor] != -1 } {
	# nothing
    } elseif { [string first s $anchor] != -1 } {
	set y [expr $y+$height*$scaley]
    } else {
	set y [expr $y+$height*$scaley/2.0]
    }
    if { [string first w $anchor] != -1 } {
	# nothing
    } elseif { [string first e $anchor] != -1 } {
	set x [expr $x-$width*$scalex]
    } else {
	set x [expr $x-$width*$scalex/2.0]
    }

    foreach "x y" [_map $x $y] break
    append xml "<image x='$x' y='$y' width=' [expr {$width*$scalex}]' "\
	"height='[expr {$height*$scaley}]' "\
	"xlink:href='$image'/>"
    _check_ymax [expr {$y+$height*$scaley}]
    return [list [expr $width*$scalex] [expr $height*$scaley]]
}

proc PDFWriter::svg::ImageFileSize { file } {

    
    switch [file ext $file] {
	.gif { set type gif }
	.jpg - .jpeg { set type jpeg }
	.png { set type png }
	default {
	    error "unknown extension for image file '$file'"
	    return
	}
    }

    set image [image create photo -file $file]
    set width [image width $image]
    set height [image height $image]
    image delete $file
    return [list $width $height]
}

proc PDFWriter::svg::CreateLine { x1 y1 x2 y2 lwidth color { dash 0 } } {
    variable xml

    foreach "x1 y1 x2 y2" [_map $x1 $y1 $x2 $y2] break
    # dash is needed
    append xml "<line stroke-width='$lwidth' stroke='$color' " \
	"x1='$x1' y1='$y1' x2='$x2' y2='$y2'/>\n"
}

proc PDFWriter::svg::CreateLineC { coords lwidth color { dash 0 } } {
    variable xml

    foreach "x1 y1 x2 y2" [_map $coords] break
    # dash is needed
    append xml "<line stroke-width='$lwidth' stroke='$color' " \
	"x1='$x1' y1='$y1' x2='$x2' y2='$y2'/>\n"
}

proc PDFWriter::svg::CreateLineArrow { x1 y1 x2 y2 arrowtype lwidth color { dash 0 } } {
    variable xml
    
    # arrow is needed
    append xml "<line stroke-width='$lwidth' stroke='$color' " \
	"x1='$x1' y1='$y1' x2='$x2' y2='$y2'/>\n"
}

proc PDFWriter::svg::CreateBezierD2 { coordList lwidth color { dash 0 } { isfilled 0 } } {
    variable xml

    set coordList [_map $coordList]
    if { $isfilled } {
	append xml "<path d='M [lrange $coordList 0 1] Q [lrange $coordList 2 end]'
	    fill='$color' strokecolor='$color' stroke-width='$lwidth'/>\n"
    } else {
	append xml "<path d='M [lrange $coordList 0 1] C [lrange $coordList 2 end]'
	    strokecolor='$color' stroke-width='$lwidth'/>\n"
    }
}

proc PDFWriter::svg::CreateBezierD3 { coordList lwidth color { dash 0 } { isfilled 0 } } {
    variable xml

    set coordList [_map $coordList]
    if { $isfilled } {
	append xml "<path d='M [lrange $coordList 0 1] C [lrange $coordList 2 end]'
	    fill='$color' strokecolor='$color' stroke-width='$lwidth'/>\n"
    } else {
	append xml "<path d='M [lrange $coordList 0 1] C [lrange $coordList 2 end]'
	    strokecolor='$color' stroke-width='$lwidth'/>\n"
    }
}

proc PDFWriter::svg::CreateCircle { x y radius lwidth color { dash 0 } { isfilled 0 } } {
    variable xml

    foreach "x y" [_map $x $y] break
    if { $isfilled } {
	append xml "<circle cx='$x' cy='$y' r='$radius'"\
	    "fill='$color' strokecolor='$color' stroke-width='$lwidth'/>"
    } else {
	append xml "<circle cx='$x' cy='$y' r='$radius'"\
	    " strokecolor='$color' stroke-width='$lwidth'/>"
    }
}

proc PDFWriter::svg::CreateArc { x y radius Sangle Eangle lwidth color { dash 0 } { isfilled 0 } } {
    variable xml
    
# TODO
#     append xml "<path d='M600,350 l 50,-25 
#            a25,25 -30 0,1 50,-25 l 50,-25 
#            a25,50 -30 0,1 50,-25 l 50,-25 
#            a25,75 -30 0,1 50,-25 l 50,-25 
#            a25,100 -30 0,1 50,-25 l 50,-25'
#         fill='none' stroke='red' stroke-width='5'/>"

}

proc PDFWriter::svg::CreateEllipse { x y radiusx radiusy Sangle Eangle lwidth color { dash 0 } { isfilled 0 } } {
    variable pdfhandle
    
    ScaleCoordSystem $x $y 1.0 [expr {$radiusy/double($radiusx)}]
    CreateArc $x $y $radiusx $Sangle $Eangle $lwidth $color $dash $isfilled
    RestoreCoordSystem
}

proc PDFWriter::svg::CreatePolygon { coordList color { isfilled 0 } { lwidth 0 } { dash 0 } } {
    variable xml
    
    set coordList [_map $coordList]
    if { $isfilled } {
	append xml "<polygon fill='$color' 
	    points='$coordList'/>"
    } else {
	append xml "<polygon stroke='$color' stroke-width='$lwidth'
	    points='$coordList'/>"
    }
}

proc PDFWriter::svg::CreateFilledBox { x y width height anchor lwidth colorL colorF { dash 0 } } {
    variable xml

    if { $anchor eq "center" } { set anchor "" }
    if { [string first s $anchor] != -1 } {
	set y [expr $y-$height]
    } elseif { [string first n $anchor] != -1 } {
	# nothing
    } else {
	set y [expr $y-$height/2.0]
    }
    if { [string first w $anchor] != -1 } {
	# nothing
    } elseif { [string first e $anchor] != -1 } {
	set x [expr $x-$width]
    } else {
	set x [expr $x-$width/2.0]
    }
    set x2 [expr {$x+$width}]
    set y2 [expr {$y+$height}]

    if { $lwidth eq "" } { set lwidth 0 }
    if { $lwidth == 0 || $colorL eq "" } {
	set colorL $colorF
    }

    foreach "x y" [_map $x $y] break
    append xml "<rect x='$x' y='$y' width='$width' height='$height'
	fill='$colorF' stroke=' $colorL' stroke-width='$lwidth'/>\n"
}

proc PDFWriter::svg::DrawGrid {} {
    variable xmax
    variable ymax

    set xmaxcm [PDFWriter::svg::pt2cm $xmax]
    set ymaxcm [PDFWriter::svg::pt2cm $ymax]

    for { set x 0 } { $x < $xmax } { set x [expr $x+[cm2pt 1]] } {
	PDFWriter::svg::CreateLine $x 0 $x $ymax 1 black
	PDFWriter::svg::CreateLine [expr $x+[cm2pt 0.5]] 0 [expr $x+[cm2pt 0.5]] $ymax 1 black 1
	set text [expr int([pt2cm $x])]
	WriteText $text $x [expr $ymax-[cm2pt 1]] Helvetica 10 nw
	WriteText $text $x [cm2pt 1] Helvetica 10 nw
    }
    for { set y 0 } { $y < $ymax } { set y [expr $y+[cm2pt 1]] } {
	PDFWriter::svg::CreateLine 0 $y $xmax $y 1 black
	PDFWriter::svg::CreateLine 0 [expr $y+[cm2pt 0.5]] $xmax [expr $y+[cm2pt 0.5]] 1 black 1
	set text [expr int([pt2cm $y])]
	WriteText $text [expr $xmax-[cm2pt 1]] $y Helvetica 10 nw
	WriteText $text [cm2pt 1] $y Helvetica 10 nw
    }
}

proc PDFWriter::svg::LineSpace { font size } {
    variable fonts
    variable fontsLS
    variable fontscale

    if { ![info exists fontsLS($font,$size)] } {
	set f $fonts($font)
	lappend f -size [expr { round(-$size) }]
	set fontsLS($font,$size) [expr {[font metrics $f -linespace]*$fontscale}]
    }
    return $fontsLS($font,$size)
}

proc PDFWriter::svg::CapHeight { font size } {
    variable fonts
    return 0.67
}

proc PDFWriter::svg::Ascender { font size } {
    variable fonts
    variable fontsAS
    variable fontscale
    
    if { ![info exists fontsAS($font,$size)] } {
	set f $fonts($font)
	lappend f -size [expr { round(-$size) }]
	set fontsAS($font,$size) [expr {0.7*[font metrics $f -ascent]*$fontscale/
	    [LineSpace $font $size]}]
    }
    return $fontsAS($font,$size)
}

proc PDFWriter::svg::Descender { font size } {
    variable fonts
    variable fontsDES
    variable fontscale
    
    if { ![info exists fontsDES($font,$size)] } {
	set f $fonts($font)
	lappend f -size [expr { round(-$size) }]
	set fontsDES($font,$size) [expr {1.0*[font metrics $f -descent]*$fontscale/
	    [LineSpace $font $size]}]
    }
    return $fontsDES($font,$size)
}

proc PDFWriter::svg::TextWidth { text font size } {
    variable fonts

    set f $fonts($font)
    lappend f -size [expr { round(-$size) }]

    return [font measure $f $text]
}

proc PDFWriter::svg::xml { args } {
    set map [list > "&gt;" < "&lt;" & "&amp;" \" "&quot;" ' "&apos;"]
    return [string map $map [join $args ""]]
}

proc PDFWriter::svg::WriteText { text x y font size anchor { color "" } } {
    variable xml
    variable fonts

    if { $anchor eq "center" } { set anchor "" }
    if { [string first n $anchor] != -1 } {
	# nothing
    } elseif { [string first s $anchor] != -1 } {
	set y [expr $y+[LineSpace $font $size]]
    } else {
	set y [expr $y+0.5*[LineSpace $font $size]]
    }
    if { [string first w $anchor] != -1 } {
	# nothing
    } elseif { [string first e $anchor] != -1 } {
	set x [expr $x-[TextWidth $text $font $size]]
    } else {
	set x [expr $x-0.5*[TextWidth $text $font $size]]
    }
    if { $color eq "" } { set color black }
    foreach "x y" [_map $x $y] break

    append xml "<text x='$x' y='$y' "
    if { [set ipos [lsearch -exact $fonts($font) -family]] != -1 } {
	append xml "font-family='[lindex $fonts($font) [incr ipos]]' "
    }
    if { [set ipos [lsearch -exact $fonts($font) -weight]] != -1 } {
	append xml "font-weight='[lindex $fonts($font) [incr ipos]]' "
    }
    if { [set ipos [lsearch -exact $fonts($font) -slant]] != -1 } {
	set slant [lindex $fonts($font) [incr ipos]]
	if { $slant eq "roman" } { set slant normal }
	append xml "font-style='$slant' "
    }
    append xml "font-size='$size' fill='$color'>[xml $text]</text>\n"
 
    return [list [TextWidth $text $font $size] [LineSpace $font $size]]
}

proc PDFWriter::svg::ScaleCoordSystem { xc yc scalefact } {

#TODO
}

proc PDFWriter::svg::RotateCoordSystem { x y angle } {
    #TODO
}

proc PDFWriter::svg::RestoreCoordSystem {} {

    #TODO
}

# angle in degrees counterclockwise from the positive x axis
proc PDFWriter::svg::WriteTextRotated { text x y font size anchor color angle } {


    error "error in PDFWriter::svg::WriteTextRotated"
}

# mode can be left, right, center, justify or fulljustify
proc PDFWriter::svg::WriteTextBox { text x y width mode font size } {

    variable fonts

    error "error in PDFWriter::svg::WriteTextBox"
}

proc PDFWriter::svg::LocalLink { llx lly urx ury page_num } {
    # nothing
}

proc PDFWriter::svg::clean_images {} {
    variable cacheimages

    foreach i [array names cacheimages] {
	image delete $cacheimages($i)
    }
    array unset cacheimages

}

proc PDFWriter::svg::End { { outputPDF 1 } } {
    variable xml
    variable svgfile
    variable ymax_real
    variable cacheimages

    append xml "</svg>"

    set y [expr {$ymax_real+20}]
    regsub {(<svg.*?height=)'(.*?)'} $xml "\\1'$y'" xml
    regsub {(<svg.*?viewBox='.*?)([\d.]+)'} $xml "\\1$y'" xml

    if { [winfo exists $svgfile] && [winfo class $svgfile] eq "Canvas" } {
	set doc [dom parse $xml]
	set ns { svg http://www.w3.org/2000/svg } 
	set svgNode [[$doc documentElement] selectNodes -namespaces $ns /*/svg:svg|/svg:svg]
	set svg2draw [svg2draw %AUTO% -canvas $svgfile -destroy_with_canvas 1 \
		-svgnode $svgNode -delete_svgnode_on_end 1]
	$svg2draw draw
    } elseif 1 {
	set w [svg2canvas_window::create_window . $xml]
	bind $w <Destroy> PDFWriter::svg::clean_images
    } else {
	set fout [open $svgfile w]
	fconfigure $fout -encoding utf-8
	puts $fout $xml
	close $fout

	set iexplore {C:\Archivos de programa\Internet Explorer\IEXPLORE.EXE}
	exec $iexplore $svgfile &
    }
    unset xml

    return $svgfile
}

if { [info exists argv0] && [info script] == $argv0 } {
    wm withdraw .
    PDFWriter::svg::Init
    foreach "- hei" [PDFWriter::svg::WriteText "Hola maco" 100 100 "Helvetica" 10 nw red] break
    PDFWriter::svg::WriteText "Adeu maco" 100 [expr {100-$hei}] "Helvetica bold" 10 nw red
    PDFWriter::svg::CreateLine 100 100 400 100 1 red
    PDFWriter::svg::CreateFilledBox 100 200 300 300 sw 0 blue blue
    PDFWriter::svg::CreateFilledBox 61.9086614173 762.16280315 476.815748032 706.177133859 nw 3 #CEC2C2 ""
    PDFWriter::svg::CreateArc 250 350 150 20 20 0 red 0 1
    PDFWriter::svg::End
    exit
}
