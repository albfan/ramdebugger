
if { $::tcl_platform(platform) eq "windows" } {
    package require printer
    package require gdi
}
catch {
    package require img::gif
    package require img::jpeg
    package require img::png
}
package require base64
package require textutil

namespace eval PDFWriter::print {
    variable pdffile
    variable hdc ""
    variable fonts
    variable xmax
    variable ymax
    variable cacheimages
    variable xfac
    variable yfac
    variable xadd
    variable yadd
    variable fontscale

    namespace export *
}

proc PDFWriter::print::cm2pt { cm } {
    return [expr $cm*72/2.54]
}

proc PDFWriter::print::pt2cm { pt } {
    return [expr $pt*2.54/72.0]
}

proc PDFWriter::print::_map { args } {
    variable xfac
    variable yfac
    variable xadd
    variable yadd

    if { [llength $args] == 1 } { set args [lindex $args 0] }
    set l ""
    foreach "x y" $args {
	lappend l [expr {$x*$xfac+$xadd}] [expr {$y*$yfac+$yadd}]
    }
    return $l
}

proc PDFWriter::print::GiveEncoding {} {
    return utf-8
}

proc PDFWriter::print::Init { { papertype A4 } { _pdffile "" } } {
    variable pdffile
    variable hdc
    variable fonts
    variable xmax
    variable ymax
    variable xfac
    variable yfac
    variable xadd
    variable yadd
    variable fontscale

    if { $_pdffile == "" } {
	set pdffile tmpfile.pdf
    } else {
	set pdffile $_pdffile
    }

    foreach "hdc okcancel" [printer dialog select] break
    if { $okcancel == 0 } { error "User cancelled printing" }
    #set hdc [printer open -name "Acrobat Distiller"]

    switch -glob -- $papertype {
	A5 {
	    set xmax [cm2pt 14.85]
	    set ymax [cm2pt 21]
	    set bigmarginh [cm2pt 1.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	    set orientation portrait
	}
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
    
    set res [lindex [printer attr -hDC $hdc -get [list "pixels per inch"]] 0 1]
    foreach "resx resy" $res break
    set margins [lindex [lindex [printer attr -hDC $hdc -get [list "page margins"]] 0] 1]
    foreach "m1 m2 m3 m4" $margins break
    set px [expr {$xmax/72.0*$resx-$m1-$m3}]
    set py [expr {$ymax/72.0*$resy-$m2-$m4}]

#     printer close -hDC $hdc
#     set hdc [printer open -name "Acrobat Distiller" -attr [list \
#                 [list {page dimensions} [list $px $py]] \
#                 [list {page orientation} $orientation]]]

#     set margins [lindex [lindex [printer attr -hDC $hdc -get [list "page margins"]] 0] 1]
#     foreach "m1 m2 m3 m4" $margins break
    printer attr -hDC $hdc -set [list [list {page dimensions} [list $px $py]]\
		                     [list {page orientation} $orientation]]
    set dims [lindex [lindex [printer attr -hDC $hdc -get [list "page dimensions"]] 0] 1]
    foreach "width height" $dims break

    set xfac [expr {($width+$m1+$m3)/$xmax}]
    set yfac [expr {-1*($height+$m2+$m4)/$ymax}]
    set xadd 0
    set yadd [expr {($height+$m2+$m4)}]
    set fontscale 1

    #tk_messageBox -message [printer job -hdc $hdc start -name $pdffile]
    printer job -hdc $hdc start -name $pdffile
    printer page -hDC $hdc start

#     if { 0 && $::tcl_platform(platform) != "windows" || [catch {
#         set fonts(Helvetica) [PDF_findfont $hdc "Tahoma" "host" 0]
#         set fonts(Helvetica-Bold) [PDF_findfont $hdc "Tahoma Negrita" "host" 0]
#     }] } {
#         set fonts(Helvetica) [PDF_findfont $hdc "Helvetica" "host" 0]
#         set fonts(Helvetica-Bold) [PDF_findfont $hdc "Helvetica-Bold" "host" 0]
#     }
#     set fonts(Times-Roman) [PDF_findfont $hdc "Times-Roman" "host" 0]
#     set fonts(Times-Bold) [PDF_findfont $hdc "Times-Bold" "host" 0]
#     set fonts(Times-Italic) [PDF_findfont $hdc "Times-Italic" "host" 0]
#     set fonts(Times-BoldItalic) [PDF_findfont $hdc "Times-BoldItalic" "host" 0]
#     set fonts(Helvetica-Oblique) [PDF_findfont $hdc "Helvetica-Oblique" "host" 0]
#     set fonts(Helvetica-BoldOblique) [PDF_findfont $hdc "Helvetica-BoldOblique" "host" 0]
#     set fonts(Courier-Bold) [PDF_findfont $hdc "Courier-Bold" "host" 0]
#     set fonts(Courier) [PDF_findfont $hdc "Courier" "host" 0]
#     set fonts(Symbol) [PDF_findfont $hdc "Symbol" "builtin" 0]

    return [list $xmax $ymax $bigmarginh $bigmarginv $ysep]
    
}

proc PDFWriter::print::GiveTempDir {} {
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

proc PDFWriter::print::AddBookmark { text parent } {
    variable hdc

    return 0

}

proc PDFWriter::print::NewPage { { papertype "" } } {
    variable hdc
    variable xmax
    variable ymax

    printer page -hDC $hdc end
    printer page -hDC $hdc start
}


# TCL font is supposed to be in the format: -size .. -family ..
proc PDFWriter::print::TclfontToPDF { font sizedefault } {

    set sizefac 1
    if { [lsearch $font bold] != -1 } {
	if { [lsearch $font italic] != -1 } {
	    return [list "Helvetica bold italic" $sizefac]
	} else {
	    return [list "Helvetica bold" $sizefac]
	}
    } elseif { [lsearch $font italic] != -1 } {
	return [list "Helvetica italic" $sizefac]
    } else {
	return [list Helvetica $sizefac]
    }
}

proc PDFWriter::print::InsertImageNoDisk { image x y anchor { scale 1 } } {
    variable hdc
    variable xfac
    variable yfac

    set width [image width $image]
    set height [image height $image]

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
    lassign [list $scale $scale] scalex scaley

    if { [llength [split $scale ","]] == 2 } {
	lassign [split $scale ","] scalex scaley
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
    gdi photo $hdc -destination [list $x $y [expr {$width*$scalex*$xfac}] \
	    [expr {abs($height*$scaley*$yfac)}]] -photo $image

    return [list [expr $width*$scalex] [expr $height*$scaley]]
}

proc PDFWriter::print::InsertImageFromData { imgdata x y anchor { scale 1 } } {

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

proc PDFWriter::print::InsertImage { imagename x y anchor { scale 1 } } {

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

proc PDFWriter::print::InsertImg { img x y anchor { scale 1 } } {
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
	image delete $cacheimages($file)
	unset cacheimages($file)
    }
    set fout [open $file w]
    fconfigure $fout -translation binary
    puts -nonewline $fout $imgdata
    close $fout

    return [InsertImageFile $file $x $y $anchor $scale]
}

proc PDFWriter::print::_trycopyingfile { file } {

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

proc PDFWriter::print::_parse_scale { scale width height } {
    
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

proc PDFWriter::print::InsertImageFile { file x y anchor { scale 1 } } {
    variable hdc
    variable cacheimages
    variable xfac
    variable yfac

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
    gdi photo $hdc -destination [list $x $y [expr {$width*$scalex*$xfac}] \
	[expr {abs($height*$scaley*$yfac)}]] -photo $image

    return [list [expr $width*$scalex] [expr $height*$scaley]]
}

proc PDFWriter::print::ImageFileSize { file } {
    variable hdc
    
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

proc PDFWriter::print::CreateLine { x1 y1 x2 y2 lwidth color { dash 0 } } {
    variable hdc
    variable xfac

    set lwidth [expr {int($lwidth*$xfac*.5)}]
    eval gdi line $hdc [_map $x1 $y1 $x2 $y2] [list -width $lwidth -fill $color]
 
}

proc PDFWriter::print::CreateLineC { coords lwidth color { dash 0 } } {
    variable hdc
    variable xfac

    set lwidth [expr {int($lwidth*$xfac*.5)}]

    eval gdi line $hdc [_map $coords] [list -width $lwidth -fill $color]
}

proc PDFWriter::print::CreateLineArrow { x1 y1 x2 y2 arrowtype lwidth color { dash 0 } } {
    variable hdc
    variable xfac

    set lwidth [expr {int($lwidth*$xfac*.5)}]

    eval gdi line $hdc [_map $x1 $y1 $x2 $y2] [list -width $lwidth -fill $color -arrow $arrowtype]
}

proc PDFWriter::print::CreateBezierD2 { coordList lwidth color { dash 0 } { isfilled 0 } } {
    variable hdc
    variable xfac

    set lwidth [expr {int($lwidth*$xfac*.5)}]

    if { !$isfilled } {
	gdi line $hdc {*}[_map $coordList] -width $lwidth -fill $color -smooth bezier
    } else {
	gdi polygon $hdc {*}[_map $coordList] -fill $color -smooth bezier
    }
}

proc PDFWriter::print::CreateBezierD3 { coordList lwidth color { dash 0 } { isfilled 0 } } {
    variable hdc
    variable xfac

    set lwidth [expr {int($lwidth*$xfac*.5)}]

    if { !$isfilled } {
	gdi line $hdc {*}[_map $coordList] -width $lwidth -fill $color -smooth bezier
    } else {
	gdi polygon $hdc {*}[_map $coordList] -fill $color -smooth bezier
    }
}

proc PDFWriter::print::CreateCircle { x y radius lwidth color { dash 0 } { isfilled 0 } } {
    variable hdc
    variable xfac

    set lwidth [expr {int($lwidth*$xfac*.5)}]

    set x1 [expr {$x-$radius}]
    set y1 [expr {$y+$radius}]
    set x2 [expr {$x+$radius}]
    set y2 [expr {$y-$radius}]
    if { !$isfilled } {
	eval gdi oval $hdc [_map $x1 $y1 $x2 $y2] [list -width $lwidth -outline $color]
    } else {
	eval gdi oval $hdc [_map $x1 $y1 $x2 $y2] [list -fill $color -outline $color]
    }
}

proc PDFWriter::print::CreateArc { x y radius Sangle Eangle lwidth color { dash 0 } { isfilled 0 } } {
    variable hdc
    variable xfac

    set lwidth [expr {int($lwidth*$xfac*.5)}]

    set x1 [expr {$x-$radius}]
    set y1 [expr {$y+$radius}]
    set x2 [expr {$x+$radius}]
    set y2 [expr {$y-$radius}]

    if { !$isfilled } {
	eval gdi arc $hdc [_map $x1 $y1 $x2 $y2] [list -width $lwidth -outline $color \
		                                      -start $Sangle -extent $Eangle]
    } else {
	eval gdi arc $hdc [_map $x1 $y1 $x2 $y2] [list -fill $color -start $Sangle -extent $Eangle]
    }
}

proc PDFWriter::print::CreateEllipse { x y radiusx radiusy Sangle Eangle lwidth color { dash 0 } { isfilled 0 } } {
    variable pdfhandle

    ScaleCoordSystem $x $y 1.0 [expr {$radiusy/double($radiusx)}]
    CreateArc $x $y $radiusx $Sangle $Eangle $lwidth $color $dash $isfilled
    RestoreCoordSystem
}

proc PDFWriter::print::CreatePolygon { coordList color { isfilled 0 } { lwidth 0 } { dash 0 } } {
    variable hdc
    variable xfac

    set lwidth [expr {int($lwidth*$xfac*.5)}]

    if { !$isfilled } {
	eval gdi polygon $hdc [_map $coordList] [list -outline $color -width $lwidth]
    } else {
	eval gdi polygon $hdc [_map $coordList] [list -fill $color]
    }
}

proc PDFWriter::print::CreateFilledBox { x y width height anchor lwidth colorL colorF { dash 0 } } {
    variable hdc
    variable xfac

    if { [llength $colorF] == 2 } {
	foreach "colorF rounded" $colorF break
    } else { set rounded "" }

    if { $anchor eq "center" } { set anchor "" }
    if { [string first n $anchor] != -1 } {
	set y [expr $y-$height]
    } elseif { [string first s $anchor] != -1 } {
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

    if { [string first e $rounded] == -1 } {
	set x2 [expr {$x+$width}]
	set y2 [expr {$y+$height}]
    } else {
	set x2 [expr {$x+$width-.5*$height}]
	set y2 [expr {$y+$height}]
	set x1r [expr {$x2-.5*$height}]
	set x2r [expr {$x2+.5*$height}]
    }

    if { $lwidth eq "" } { set lwidth 0 }
    if { $lwidth == 0 || $colorL eq "" } {
	set colorL $colorF
    }

    set lwidth [expr {int($lwidth*$xfac*.5)}]
    if { $colorF ne "" } {
	eval gdi rectangle $hdc [_map $x $y $x2 $y2] -fill $colorF -outline $colorL \
	    -width $lwidth
	if { [string first e $rounded] != -1 } {
	    eval gdi arc $hdc [_map $x1r $y $x2r $y2] -fill $colorF -outline $colorF \
		-style chord -start 90 -extent 180
	    if { $colorL ne "" } {
		eval gdi arc $hdc [_map $x1r $y $x2r $y2] -fill $colorL -outline $colorL \
		    -style arc -width $lwidth -start 90 -extent 180
	    }
	}
    } else {
	eval gdi rectangle $hdc [_map $x $y $x2 $y2] -outline $colorL \
	    -width $lwidth
	if { [string first e $rounded] != -1 } {
	    eval gdi arc $hdc [_map $x1r $y $x2r $y2] -outline $colorL -fill $colorL \
		-style arc -width $lwidth -start 90 -extent 180
	}
    }
}

proc PDFWriter::print::DrawGrid {} {
    variable xmax
    variable ymax

    set xmaxcm [PDFWriter::print::pt2cm $xmax]
    set ymaxcm [PDFWriter::print::pt2cm $ymax]

    for { set x 0 } { $x < $xmax } { set x [expr $x+[cm2pt 1]] } {
	PDFWriter::print::CreateLine $x 0 $x $ymax 1 black
	PDFWriter::print::CreateLine [expr $x+[cm2pt 0.5]] 0 [expr $x+[cm2pt 0.5]] $ymax 1 black 1
	set text [expr int([pt2cm $x])]
	WriteText $text $x [expr $ymax-[cm2pt 1]] Helvetica 10 nw
	WriteText $text $x [cm2pt 1] Helvetica 10 nw
    }
    for { set y 0 } { $y < $ymax } { set y [expr $y+[cm2pt 1]] } {
	PDFWriter::print::CreateLine 0 $y $xmax $y 1 black
	PDFWriter::print::CreateLine 0 [expr $y+[cm2pt 0.5]] $xmax [expr $y+[cm2pt 0.5]] 1 black 1
	set text [expr int([pt2cm $y])]
	WriteText $text [expr $xmax-[cm2pt 1]] $y Helvetica 10 nw
	WriteText $text [cm2pt 1] $y Helvetica 10 nw
    }
}

proc PDFWriter::print::LineSpace { font size } {
    variable hdc
    variable fontsLS
    variable yfac
    
    if { ![info exists fontsLS($font,$size)] } {
	set fontsLS($font,$size) [gdi text $hdc -10 -10 -anchor se -text "Prova" \
		                      -font [list $font $size]]
    }
    return [expr {$fontsLS($font,$size)/abs($yfac)}]
}

proc PDFWriter::print::CapHeight { font size } {
    variable hdc
    variable fonts
    return 0.67
}

proc PDFWriter::print::Ascender { font size } {
    variable hdc
    variable fonts

   return 0.77

}

proc PDFWriter::print::Descender { font size } {
    variable hdc
    variable fonts

    return -0.2
}

proc PDFWriter::print::TextWidth { text font size } {
    variable hdc
    variable fonts
    variable fontsTW
    variable xfac
    variable fontscale

    if { ![info exists fontsTW($font,$size)] } {
	set array [gdi characters $hdc -font [linsert $font 1 $size]]
	set tmplist [uplevel \#0 [list array get $array]]
	foreach "name value" $tmplist {
	    # corrects an error in non UTF-8 encoding
	    if { $name eq "" } { continue }
	    set name [format %c [scan $name %c]]
	    set tmp($name) $value
	}
	set fontsTW($font,$size) [array get tmp]
    }
    array set tmp $fontsTW($font,$size)
    
    set len 0.0
    foreach c [split $text ""] {
	if { [info exists tmp($c)] } {
	    set len [expr {$len+$tmp($c)}]
	} else { set len [expr {$len+$tmp(0)}] }
    }
    return [expr {$fontscale*$len/$xfac}]
}

proc PDFWriter::print::WriteText { text x y font size anchor { color "" } } {
    variable hdc
    variable fonts
    variable xfac
    variable yfac
    variable fontscale

    set size [expr {$fontscale*$size}]

    if { $color eq "" } { set color black }
    set hei [eval gdi text $hdc [_map $x $y] [list -anchor $anchor -text $text -font \
		[linsert $font 1 $size] -fill $color] -unicode]
    set width [TextWidth $text $font $size]
 
    return [list $width [expr {$hei/abs($yfac)}]]
}

proc PDFWriter::print::ScaleCoordSystem { xc yc scalefactx { scalefacty "" } } {
    variable hdc
    variable xfac
    variable yfac
    variable xadd
    variable yadd
    variable fontscale
    variable scalecoordsystem_save [list $xfac $yfac $xadd $yadd]

    if { $scalefacty eq "" } {
	set scalefacty $scalefactx
    }
    set fontscale $scalefacty
    set xadd [expr {(1.0-$scalefactx)*$xc*$xfac+$xadd}]
    set yadd [expr {(1.0-$scalefacty)*$yc*$yfac+$yadd}]
    set xfac [expr {$xfac*$scalefactx}]
    set yfac [expr {$yfac*$scalefacty}]
}

proc PDFWriter::print::RotateCoordSystem { x y angle } {
    # nothing by now
    variable scalecoordsystem_save [list $xfac $yfac $xadd $yadd]
    return
}

proc PDFWriter::print::RestoreCoordSystem {} {
    variable hdc
    variable xfac
    variable yfac
    variable xadd
    variable yadd
    variable scalecoordsystem_save
    variable fontscale 1

    foreach "xfac yfac xadd yadd" $scalecoordsystem_save break
}

# angle in degrees counterclockwise from the positive x axis
proc PDFWriter::print::WriteTextRotated { text x y font size anchor color angle } {
    variable hdc

    error "error in PDFWriter::print::WriteTextRotated"
}

# mode can be left, right, center, justify or fulljustify
proc PDFWriter::print::WriteTextBox { text x y width mode font size } {
    variable hdc
    variable fonts

    error "error in PDFWriter::print::WriteTextBox"
}

proc PDFWriter::print::LocalLink { llx lly urx ury page_num } {
    # nothing
}

proc PDFWriter::print::End { { outputPDF 1 } } {
    variable pdffile
    variable hdc
    variable cacheimages

    foreach i [array names cacheimages] {
	image delete $cacheimages($i)
    }
    array unset cacheimages

    if { $hdc == "" } {
	error "error in PDFWriter::print::End. There is no handle"
    }
    printer page -hDC $hdc end
    printer job -hDC $hdc end
    printer close -hDC $hdc
    set hdc ""

    return ""
}

if { [info exists argv0] && [info script] == $argv0 } {
    wm withdraw .
    PDFWriter::print::Init
    foreach "- hei" [PDFWriter::print::WriteText "Hola maco aqui caña \u3b1 \u2265 \u2207\u3b2" \
		         100 100 "Helvetica" 10 nw red] break
    PDFWriter::print::WriteText "Adeu maco" 100 [expr {100-$hei}] "Helvetica bold" 10 nw red
    PDFWriter::print::CreateLine 100 100 400 100 1 red
    PDFWriter::print::CreateFilledBox 100 200 300 300 sw 0 blue blue
    PDFWriter::print::CreateFilledBox 61.9086614173 762.16280315 476.815748032 706.177133859 nw 3 #CEC2C2 ""
    PDFWriter::print::CreateArc 250 350 150 20 20 0 red 0 1
    PDFWriter::print::End
    exit
}
