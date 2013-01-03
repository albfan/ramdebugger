
catch {
    package require img::gif
    package require img::jpeg
    package require img::png
}

package require textutil
package require math::constants
package require compass_utils::c

namespace eval PDFWriter::PDF {
    variable pdffile
    variable pdfhandle ""
    variable pagehandle
    variable fonts
    variable xmax
    variable ymax
    variable cacheimages
    variable tcl_encoding
    variable pdf_encoding
    
    namespace export *
}

proc PDFWriter::PDF::cm2pt { cm } {
    return [expr $cm*72/2.54]
}

proc PDFWriter::PDF::pt2cm { pt } {
    return [expr $pt*2.54/72.0]
}

proc PDFWriter::PDF::GiveEncoding {} {
    variable tcl_encoding

    return $tcl_encoding
}

proc PDFWriter::PDF::Init { { papertype A4 } { _pdffile "" } } {
    variable pdffile
    variable pdfhandle
    variable pagehandle
    variable fonts
    variable xmax
    variable ymax
    variable cacheimages
    variable cachefonts
    variable local_link_stack
    variable tcl_encoding
    variable pdf_encoding
    
    set use_uff8 1
    
    if { !$use_uff8 } {
	set tcl_encoding cp1252
	set pdf_encoding WinAnsiEncoding
    } else {
	set tcl_encoding utf-8
	set pdf_encoding UTF-8
    }    
    
    array unset cacheimages
    set pdfhandle [hpdf::new]
    $pdfhandle setCompressionMode all
    
    #PDF_set_parameter $pdfhandle serial "Compass"

    if { $_pdffile eq "" } {
	set ic 0
	while { $ic < 50 } {
	    set tmpdir /windows/temp
	    catch { set tmpdir [GiveTempDir] }
	    set pdffile [file join $tmpdir tmpfile$ic.pdf]
	    if { [file exists $pdffile] && [clock seconds] - [file mtime $pdffile] < 30 } {
		incr ic; continue
	    }
	    set err [catch { $pdfhandle saveToFile $pdffile }]
	    if { !$err } { break }
	    incr ic
	}
	if { $ic == 50 } {
	    error [_ "Error: could not open temporal file"]
	}
    } else {
	set pdffile $_pdffile
	set err [catch { $pdfhandle saveToFile $pdffile }]
	if { $err } {
	    error [_ "Error: could not open file '%s'" $pdffile]
	}
    }

    switch -glob -- $papertype {
	A5 {
	    set xmax [cm2pt 14.85]
	    set ymax [cm2pt 21]
	    set bigmarginh [cm2pt 1.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	}
	A5L {
	    set xmaxL [cm2pt 21]
	    set ymaxL [cm2pt 14.85]
	    set bigmarginh [cm2pt 1.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	}
	A4 {
	    set xmax [cm2pt 21]
	    set ymax [cm2pt 29.7]
	    set bigmarginh [cm2pt 1.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	}
	A4L {
	    set xmax [cm2pt 29.7]
	    set ymax [cm2pt 21]
	    set bigmarginh [cm2pt 2.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	}
	A3 {
	    set xmax [cm2pt 29.7]
	    set ymax [cm2pt 42.0]
	    set bigmarginh [cm2pt 1.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
	}
	A3L {
	    set xmax [cm2pt 42.0]
	    set ymax [cm2pt 29.7]
	    set bigmarginh [cm2pt 1.5]
	    set bigmarginv [cm2pt 1.5]
	    set ysep 5
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
	}
	default {
	    error "error unknown paper type"
	}
    }

    $pdfhandle setInfoAttr "creator" "Compass"
    $pdfhandle setInfoAttr "producer" "pdfwriter"
    $pdfhandle setInfoAttr "author" "RAMSAN"
    $pdfhandle setInfoAttr "title" "Compass"

    #PDF_set_border_style $pdfhandle solid 0

    set pagehandle [$pdfhandle addPage]
    $pagehandle setWidth $xmax
    $pagehandle setHeight $ymax
    
    set  local_link_stack ""
    array unset fonts
    set cachefonts ""
    
    if { $use_uff8 } {
	$pdfhandle UseUTFEncodings
	$pdfhandle setCurrentEncoder UTF-8
	
	if { $::tcl_platform(platform) ne "windows" } {
	    set families [list -family Ubuntu -family FreeSans -family Arial]
	    foreach "n weight slant" [list "" normal roman "-Bold" bold roman "-Oblique" normal italic \
		    "-BoldOblique" bold italic] {
		if { ![info exists fonts(Helvetica$n)] } {
		    set file [cu::find_font_file {*}$families -weight $weight -slant $slant]
		    if { ![dict exists $cachefonts files $file] } {
		        set f [$pdfhandle loadTTFontFromFile $file 1]
		        dict set cachefonts files $file $f
		    } else {
		        set f [dict get $cachefonts files $file]
		    }
		    set fonts(Helvetica$n) [$pdfhandle getFont $f "UTF-8"]
		}
		if { ![info exists fonts(Courier$n)] } {
		    set file [cu::find_font_file {*}$families -weight $weight -slant $slant -fixed 1]
		    if { ![dict exists $cachefonts files $file] } {
		        set f [$pdfhandle loadTTFontFromFile $ile 1]
		        dict set cachefonts files $file $f
		    }
		    set fonts(Courier$n) [$pdfhandle getFont $f "UTF-8"]
		}
	    }
	} else {
	    set no [list verdana tahoma arial]
	    set b [list verdanab tahomabd arialbd]
	    set i [list verdanai tahomabd ariali]
	    set bi [list verdanaz tahomabd arialbi]
	    foreach "n fileList" [list "" $no "-Bold" $b "-Oblique" $i "-BoldOblique" $bi] {
		if { ![info exists fonts(Helvetica$n)] } {
		    foreach fl $fileList {
		        set file [file join $::env(windir) fonts $fl.ttf]
		        if { ![file exists $file] } { continue }
		        if { ![dict exists $cachefonts files $file] } {
		            set f [$pdfhandle loadTTFontFromFile $file 1]
		            dict set cachefonts files $file $f
		        } else {
		            set f [dict get $cachefonts files $file]
		        }
		        break
		    }
		    set fonts(Helvetica$n) [$pdfhandle getFont $f "UTF-8"]
		}
	    }
	    set no [list cour verdana tahoma arial]
	    set b [list courbd verdanab tahomabd arialbd]
	    set i [list couri verdanai tahomabd ariali]
	    set bi [list courbi verdanaz tahomabd arialbi]
	    foreach "n fileList" [list "" $no "-Bold" $b "-Oblique" $i "-BoldOblique" $bi] {
		if { ![info exists fonts(Courier$n)] } {
		    foreach fl $fileList {
		        set file [file join $::env(windir) fonts $fl.ttf]
		        if { ![file exists $file] } { continue }
		        if { ![dict exists $cachefonts files $file] } {
		            set f [$pdfhandle loadTTFontFromFile $file 1]
		            dict set cachefonts files $file $f
		        } else {
		            set f [dict get $cachefonts files $file]
		        }
		        break
		    }
		    set fonts(Courier$n) [$pdfhandle getFont $f "UTF-8"]
		}
	    }
	}
    } else {
#     set fonts(Helvetica) [PDF_findfont $pdfhandle "Tahoma" "host" 0]
#     set fonts(Helvetica-Bold) [PDF_findfont $pdfhandle "Tahoma Bold" "host" 0]

	set fonts(Helvetica) [$pdfhandle getFont "Helvetica" "WinAnsiEncoding"]
	set fonts(Helvetica-Bold) [$pdfhandle getFont "Helvetica-Bold" "WinAnsiEncoding"]
	set fonts(Helvetica-Oblique) [$pdfhandle getFont "Helvetica-Oblique" "WinAnsiEncoding"]
	set fonts(Helvetica-BoldOblique) [$pdfhandle getFont "Helvetica-BoldOblique" "WinAnsiEncoding"]
    }
    if { $::tcl_platform(platform) eq "windows" } {
	if 0 {
	    set err [catch {
		    set fontname {\Windows\Fonts\tahoma.ttf}
		    set font [$pdfhandle loadTTFontFromFile $fontname false]
		    
		    set fontname {\Windows\Fonts\arial.ttf}
		    set font_Arial [$pdfhandle loadTTFontFromFile $fontname false]
		}]
	    if { $err } {
		set font Helvetica
		set font_Arial Times-Roman
	    }
	} else {
	    set font Helvetica
	    set font_Arial Times-Roman
	}
    } else {
	set font Helvetica
	set font_Arial Times-Roman
    }
    set fonts(Times-Roman) [$pdfhandle getFont $font "WinAnsiEncoding"]
    set fonts(Times-Bold) [$pdfhandle getFont $font "WinAnsiEncoding"]
    set fonts(Times-Italic) [$pdfhandle getFont $font "WinAnsiEncoding"]
    set fonts(Times-BoldItalic) [$pdfhandle getFont $font "WinAnsiEncoding"]
#     set fonts(Helvetica-Oblique) [PDF_findfont $pdfhandle "Tahoma" "WinAnsiEncoding" 0]
#     set fonts(Helvetica-BoldOblique) [PDF_findfont $pdfhandle "Tahoma" "WinAnsiEncoding" 0]
    
    if { !$use_uff8 } {
	set fonts(Courier-Bold) [$pdfhandle getFont "Courier-Bold" "WinAnsiEncoding"]
	set fonts(Courier) [$pdfhandle getFont "Courier" "WinAnsiEncoding"]
    }
    set fonts(Symbol) [$pdfhandle getFont "Symbol" ""]

    set fonts(Arial) [$pdfhandle getFont $font_Arial "WinAnsiEncoding"]
    #set fonts(Arial-Bold) [PDF_findfont $pdfhandle "Arial,Bold" "WinAnsiEncoding" 0]


    if 0 {
	# this code crashes in some computers
	set fonts(Times-Roman) [PDF_findfont $pdfhandle "Times-Roman" "WinAnsiEncoding" 0]
	set fonts(Times-Bold) [PDF_findfont $pdfhandle "Times-Bold" "WinAnsiEncoding" 0]
	set fonts(Times-Italic) [PDF_findfont $pdfhandle "Times-Italic" "WinAnsiEncoding" 0]
	set fonts(Times-BoldItalic) [PDF_findfont $pdfhandle "Times-BoldItalic" "WinAnsiEncoding" 0]
	set fonts(Helvetica-Oblique) [PDF_findfont $pdfhandle "Helvetica-Oblique" "WinAnsiEncoding" 0]
	set fonts(Helvetica-BoldOblique) [PDF_findfont $pdfhandle "Helvetica-BoldOblique" "WinAnsiEncoding" 0]
	set fonts(Courier-Bold) [PDF_findfont $pdfhandle "Courier-Bold" "WinAnsiEncoding" 0]
	set fonts(Courier) [PDF_findfont $pdfhandle "Courier" "WinAnsiEncoding" 0]
	set fonts(Symbol) [PDF_findfont $pdfhandle "Symbol" "WinAnsiEncoding" 0]
    }

    #PDF_set_parameter $pdfhandle nativeunicode true

    return [list $xmax $ymax $bigmarginh $bigmarginv $ysep]
}

proc PDFWriter::PDF::GiveTempDir {} {
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

proc PDFWriter::PDF::AddBookmark { text parent } {
    variable pdfhandle
    variable pagehandle
    variable pdf_encoding
    
#     if { $parent } {
#         set open 1
#     } else { set open 0 }

    set open 0

    if { $parent == 0 } { set parent "" }
    set out_handle [$pdfhandle createOutline $parent [_encode_text $text] \
	    [$pdfhandle getEncoder $pdf_encoding]]
    $out_handle setOpened 0
    $out_handle setDestination [$pagehandle createDestination]
    return $out_handle
}

proc PDFWriter::PDF::NewPage { { papertype "" } } {
    variable pdfhandle
    variable pagehandle
    variable xmax
    variable ymax

    switch -glob -- $papertype {
	"" { 
	    lassign [list $xmax $ymax] xmaxL ymaxL
	}
	A5 {
	    set xmaxL [cm2pt 14.85]
	    set ymaxL [cm2pt 21]
	}
	A5L {
	    set xmaxL [cm2pt 21]
	    set ymaxL [cm2pt 14.85]
	}
	A4 {
	    set xmaxL [cm2pt 21]
	    set ymaxL [cm2pt 29.7]
	}
	A4L {
	    set xmaxL [cm2pt 29.7]
	    set ymaxL [cm2pt 21]
	}
	A3 {
	    set xmaxL [cm2pt 29.7]
	    set ymaxL [cm2pt 42.0]
	}
	A3L {
	    set xmaxL [cm2pt 42.0]
	    set ymaxL [cm2pt 29.7]
	}
	userdefined* {
	    set xmaxL [cm2pt [lindex $papertype 1]]
	    set ymaxL [cm2pt [lindex $papertype 2]]
	}
	default {
	    error "error unknown paper type"
	}
    }
    set pagehandle [$pdfhandle addPage]
    $pagehandle setWidth $xmaxL
    $pagehandle setHeight $ymaxL
    
    return [list $xmaxL $ymaxL]
}


# TCL font is supposed to be in the format: -size .. -family ..
proc PDFWriter::PDF::TclfontToPDF { font sizedefault } {
    
    set ipos [lsearch $font -size]
    if { $ipos != -1 } {
	incr ipos
	set size [font actual $font -size]
	if { $size < 0 } {
	    set size [expr {round(abs($size)/[tk scaling])}]
	}
	set sizefac [expr double($size)/double($sizedefault)]
    } else {
	set sizefac 1.0
    }
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


#     set size [font actual $font -size]
#     set sizefac [expr double($size)/double($sizedefault)]
# 
#     set family [font actual $font -family]
#     if { [string tolower $family] == "symbol" } {
#         return [list Symbol $sizefac]
#     }
#     set weight [font actual $font -weight]
#     set slant [font actual $font -slant]
#     if { $weight eq "bold" } {
#         if { $slant eq "italic" } {
#             return [list Helvetica-BoldOblique $sizefac]
#         } else {
#             return [list Helvetica-Bold $sizefac]   
#         }
#     } elseif { $slant eq "italic" } {
#         return [list Helvetica-Oblique $sizefac]
#     } else {
#         return [list Helvetica $sizefac]
#     }
}

proc PDFWriter::PDF::is_png { data } {
    return [string equal [string range $data 0 7] "\x89\x50\x4e\x47\xd\xa\x1a\xa"]
}

proc PDFWriter::PDF::InsertImageNoDisk { img x y anchor { scale 1 } } {
    return [InsertImageFromData [cu::base64 decode [$img data -format png]] $x $y $anchor $scale]
}

proc PDFWriter::PDF::InsertImageFromData { imgdata x y anchor { scale 1 } } {
    variable pdfhandle
    
    if { [string range $imgdata 0 2] == "GIF" } {
	set img [image create photo -data $imgdata]
	set imgdata [cu::base64 decode [$img data -format png]]
	image delete $img
	set type png
    } elseif { [string range $imgdata 1 3] == "PNG" } {
	set type png
    } else {
	set type jpeg
    }
    switch $type {
	png {
	    set image [$pdfhandle loadPngImageFromMem $imgdata]
	}
	jpeg {
	    set image [$pdfhandle loadJpegImageFromMem $imgdata]
	}
    }
    return [InsertImageFromPDFImage $image $x $y $anchor $scale]
}


proc PDFWriter::PDF::_parse_scale { scale width height } {
    
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

proc PDFWriter::PDF::InsertImageFromPDFImage { PDFImage x y anchor { scale 1 } } {
    variable pdfhandle
    variable pagehandle
	
    set width [$PDFImage getWidth]
    set height [$PDFImage getHeight]

    if { [llength [split $scale ","]] == 2 } {
	lassign [split $scale ","] scalex scaley
	foreach i [list scalex scaley] {
	    set $i [_parse_scale [set $i] $width $height]
	}
    } else {
	set scale [_parse_scale $scale $width $height]
	lassign [list $scale $scale] scalex scaley
    }
    set width [expr {$width*$scalex}]
    set height [expr {$height*$scaley}]
    
    if { $anchor eq "center" } { set anchor "" }
    if { [string first n $anchor] != -1 } {
	set y [expr {$y-$height}]
    } elseif { [string first s $anchor] != -1 } {
	# nothing
    } else {
	set y [expr {$y-$height/2.0}]
    }
    if { [string first w $anchor] != -1 } {
	# nothing
    } elseif { [string first e $anchor] != -1 } {
	set x [expr {$x-$width}]
    } else {
	set x [expr {$x-$width/2.0}]
    }
    
#     if { $scalex != $scaley } {
#         set fac [expr {$scaley/double($scalex)}]
#         $pagehandle gSave
#         $pagehandle concat 1.0 0 0 $fac 0 0
#         set y [expr {$y/$fac}]
#     }
    $pagehandle drawImage $PDFImage $x $y $width $height
    
#     if { $scalex != $scaley } {
#         $pagehandle gRestore
#     }
    return [list $width $height]
}

proc PDFWriter::PDF::InsertImage { imagename x y anchor { scale 1 } } {
    
    return [InsertImageFromData [cu::base64 decode [set Images::${imagename}_data]] $x $y $anchor $scale]
}


proc PDFWriter::PDF::InsertImg { img x y anchor { scale 1 } } {
    return [InsertImageFromData [cu::base64 decode [$img data -format png]] $x $y $anchor $scale]
}

proc PDFWriter::PDF::_trycopyingfile { file } {

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

proc PDFWriter::PDF::InsertImageFile { file x y anchor { scale 1 } } {
    variable pdfhandle
    variable cacheimages

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
	set PDFImage $cacheimages($file)
    } else {
	switch $type {
	    gif {
		set img [image create photo -file $file]
		set imgdata [$img data -format png]
		catch { set imgdata [cu::base64 decode $imgdata] }
		image delete $img
		set PDFImage [$pdfhandle loadPngImageFromMem $imgdata]
	    }
	    png {
		set img [image create photo -file $file]
		set imgdata [$img data -format png]
		catch { set imgdata [cu::base64 decode $imgdata] }
		image delete $img
		set PDFImage [$pdfhandle loadPngImageFromMem $imgdata]
	    }
	    jpeg {
		set img [image create photo -file $file]
		set imgdata [$img data -format jpeg]
		catch { set imgdata [cu::base64 decode $imgdata] }
		image delete $img
		set PDFImage [$pdfhandle loadJpegImageFromMem $imgdata]
	    }
	}
	set cacheimages($file) $PDFImage
    }
    return [InsertImageFromPDFImage $PDFImage $x $y $anchor $scale]
}

proc PDFWriter::PDF::ImageFileSize { file } {
    variable pdfhandle
    
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
	set PDFImage $cacheimages($file)
    } else {
	switch $type {
	    gif {
		set img [image create photo -file $file]
		set imgdata [cu::base64 decode [$img data -format png]]
		image delete $img
		set PDFImage [$pdfhandle loadPngImageFromMem $imgdata]
	    }
	    png {
		set img [image create photo -file $file]
		set imgdata [cu::base64 decode [$img data -format png]]
		image delete $img
		set PDFImage [$pdfhandle loadPngImageFromMem $imgdata]
	    }
	    jpeg {
		set img [image create photo -file $file]
		set imgdata [cu::base64 decode [$img data -format jpeg]]
		image delete $img
		set PDFImage [$pdfhandle loadJpegImageFromMem $imgdata]
	    }
	}
	set cacheimages($file) $PDFImage
    }
    set width [$PDFImage getWidth]
    set height [$PDFImage getHeight]

    return [list $width $height]
}

proc PDFWriter::PDF::_SetColorDashLineWidth { color dash lwidth } {
    variable pdfhandle
    variable pagehandle
	
    if { $color ne "" } {
	lassign [winfo rgb . $color] c1 c2 c3
	set c1 [expr $c1/65535.0]
	set c2 [expr $c2/65535.0]
	set c3 [expr $c3/65535.0]

	$pagehandle setRGBFill $c1 $c2 $c3
	$pagehandle setRGBStroke $c1 $c2 $c3
    }
    if { ![string equal $dash 0] && $dash ne "" } { 
	if { [llength $dash] == 2 } {
	    $pagehandle setDash $dash 0
	} else {
	    $pagehandle setDash [list 5 5] 0
	}
    }
    if { $lwidth ne "" && $lwidth != 0 } {
	$pagehandle setLineWidth $lwidth
    }
}

proc PDFWriter::PDF::_ResetColorDashLineWidth {} {
    variable pdfhandle
    variable pagehandle
    
    $pagehandle setRGBFill 0 0 0
    $pagehandle setRGBStroke 0 0 0
    $pagehandle setLineWidth 1
    $pagehandle setDash "" 0
}

proc PDFWriter::PDF::CreateLine { x1 y1 x2 y2 lwidth color { dash 0 } } {
    variable pdfhandle
    variable pagehandle
    
    _SetColorDashLineWidth $color $dash $lwidth
    $pagehandle moveTo $x1 $y1
    $pagehandle lineTo $x2 $y2
    $pagehandle stroke
    _ResetColorDashLineWidth
}

proc PDFWriter::PDF::CreateLineC { coords lwidth color { dash 0 } } {
    variable pdfhandle
    variable pagehandle
    
    _SetColorDashLineWidth $color $dash $lwidth
    $pagehandle moveTo {*}[lrange $coords 0 1]
    foreach "x y" [lrange $coords 2 end] {
	$pagehandle lineTo $x $y
    }
    $pagehandle stroke
    _ResetColorDashLineWidth
}

proc PDFWriter::PDF::CreateLineArrow { x1 y1 x2 y2 arrowtype lwidth color { dash 0 } } {
    variable pdfhandle
    variable pagehandle
    
    set PI [expr {atan2(0,-1)}]
    _SetColorDashLineWidth $color $dash $lwidth
    
    $pagehandle moveTo $x1 $y1
    $pagehandle lineTo $x2 $y2
    $pagehandle stroke

    if { $arrowtype in "first both" } {
	$pagehandle gSave
	set angle [expr {atan2($y1-$y2,$x1-$x2)}]
	$pagehandle concat [expr {cos($angle)}] [expr {sin($angle)}] \
	    [expr {-sin($angle)}] [expr {cos($angle)}] $x1 $y1
	$pagehandle moveTo 0 0
	$pagehandle lineTo -6 -3
	$pagehandle lineTo -6 +3
	$pagehandle closePath
	$pagehandle fill
	$pagehandle gRestore
    }
    if { $arrowtype == "last" ||$arrowtype == "both" } {
	$pagehandle gSave
	set angle [expr {atan2($y2-$y1,$x2-$x1)}]
	$pagehandle concat [expr {cos($angle)}] [expr {sin($angle)}] \
	    [expr {-sin($angle)}]  [expr {cos($angle)}] $x2 $y2
	$pagehandle moveTo 0 0
	$pagehandle lineTo -6 -3
	$pagehandle lineTo -6 +3
	$pagehandle closePath
	$pagehandle fill
	$pagehandle gRestore
    }
    _ResetColorDashLineWidth $pagehandle
}

proc PDFWriter::PDF::CreateBezierD2 { coordList lwidth color { dash 0 } { isfilled 0 } } {
    
#     set idx 1
#     for { set i 0 } { $i < [llength $coordList] } { incr i 2 } {
#         lassign [lrange $coordList $i $i+1] x y
#         CreateCircle $x $y 2 1 blue 0 1
#         WriteText $idx [expr {$x+2}] [expr {$y+2}] Arial 12 sw blue
#         incr idx
#     }

    set points3 [lrange $coordList 0 1]
    for { set i 0 } { $i < [llength $coordList]-2 } { incr i 4 } {
	for { set k 1 } { $k < 3 } { incr k } {
	    set Pkm1 [lrange $coordList [expr {$i+2*$k-2}] [expr {$i+2*$k-1}]]
	    set Pk [lrange $coordList [expr {$i+2*$k}] [expr {$i+2*$k+1}]]
	    lappend points3 {*}[m::add [m::scale [expr {$k/3.0}] $Pkm1] \
		    [m::scale [expr {1-$k/3.0}] $Pk]]
	}
	lappend points3 {*}[lrange $coordList $i+4 $i+5]
    }
    CreateBezierD3 $points3 $lwidth $color $dash $isfilled
}

proc PDFWriter::PDF::CreateBezierD3 { coordList lwidth color { dash 0 } { isfilled 0 } } {
    variable pdfhandle
    variable pagehandle
    
    _SetColorDashLineWidth $color $dash $lwidth
    $pagehandle moveTo {*}[lrange $coordList 0 1]
    for { set i 2 } { $i < [llength $coordList] } { incr i 6 } {
	$pagehandle curveTo {*}[lrange $coordList $i $i+5]
    }
    if { !$isfilled } {
	$pagehandle stroke
    } else {
	$pagehandle fill
    }

#     set idx 1
#     for { set i 0 } { $i < [llength $coordList] } { incr i 2 } {
#         lassign [lrange $coordList $i $i+1] x y
#         CreateCircle $x $y 2 1 green 0 1
#         WriteText $idx [expr {$x+2}] [expr {$y+2}] Arial 12 sw green
#         incr idx
#     }
    
    _ResetColorDashLineWidth
}

proc PDFWriter::PDF::CreateCircle { x y radius lwidth color { dash 0 } { isfilled 0 } } {
    variable pdfhandle
    variable pagehandle

    _SetColorDashLineWidth $color $dash $lwidth

    $pagehandle circle $x $y $radius
    if { !$isfilled } {
	$pagehandle stroke
    } else {
	$pagehandle fill
    }
    _ResetColorDashLineWidth
}

# arcs begin in east and are counterclockwise (in degrees)
proc PDFWriter::PDF::CreateArc { x y radius Sangle Eangle lwidth color { dash 0 } { isfilled 0 } } {
    variable pdfhandle
    variable pagehandle
    
    _SetColorDashLineWidth $color $dash $lwidth

    # angles begin up and go clockwise
    foreach i [list Sangle Eangle] {
	set $i [expr {360-([set $i]-90)}]
    }
    if { $Sangle-$Eangle >= 360 } {
	set Sangle [expr {$Eangle+359.9}]
    }
    $pagehandle arc $x $y $radius $Eangle $Sangle

    if { !$isfilled } {
	$pagehandle stroke
    } else {
	$pagehandle fill
    }
    _ResetColorDashLineWidth
}

proc PDFWriter::PDF::CreateEllipse { x y radiusx radiusy Sangle Eangle lwidth color { dash 0 } { isfilled 0 } } {
    
    ScaleCoordSystem $x $y 1.0 [expr {$radiusy/double($radiusx)}]
    CreateArc $x $y $radiusx $Sangle $Eangle $lwidth $color $dash $isfilled
    RestoreCoordSystem
}

proc PDFWriter::PDF::CreatePolygon { coordList color { isfilled 0 } { lwidth "" } { dash 0 } } {
    variable pdfhandle
    variable pagehandle
    
    _SetColorDashLineWidth $color $dash $lwidth

    $pagehandle moveTo {*}[lrange $coordList 0 1]

    foreach "x y" [lrange $coordList 2 end] {
	$pagehandle lineTo $x $y
    }
    $pagehandle closePath

    if { !$isfilled } {
	$pagehandle stroke
    } else {
	$pagehandle fill
    }
    _ResetColorDashLineWidth
}

proc PDFWriter::PDF::CreateFilledBox { x y width height anchor lwidth colorL colorF { dash 0 } } {
    variable pdfhandle
    variable pagehandle

    if { [llength $colorF] == 2 } {
	lassign $colorF colorF rounded
    } else {
	set rounded ""
    }
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
    foreach i "F L" {
	if { [set color$i] == "" } { continue }
	if { $i eq "L" && ($lwidth eq "" || $lwidth == 0) } { continue }
	_SetColorDashLineWidth [set color$i] $dash $lwidth
	$pagehandle moveTo $x $y
	if { [string first e $rounded] != -1 } {
	    set r [expr {.5*$height}]
	    $pagehandle lineTo [expr $x+$width-2*$r] $y
	    $pagehandle curveTo [expr $x+$width] $y [expr $x+$width] [expr $y+$height] \
		[expr $x+$width-2*$r] [expr $y+$height]
	} else {
	    $pagehandle lineTo [expr $x+$width] $y
	    $pagehandle lineTo [expr $x+$width] [expr $y+$height]
	}
	$pagehandle lineTo $x [expr $y+$height]
	switch $i {
	    L { $pagehandle closePathStroke }
	    F { $pagehandle fill }
	}
    }
    _ResetColorDashLineWidth
}

proc PDFWriter::PDF::DrawGrid { { unit cm } } {
    variable xmax
    variable ymax

    switch $unit {
	cm { set delta [cm2pt 1] }
	pt {  set delta 50 }
    }
    set width 0.1
    
    for { set x 0 } { $x < $xmax } { set x [expr {$x+$delta}] } {
	PDFWriter::PDF::CreateLine $x 0 $x $ymax $width black
	PDFWriter::PDF::CreateLine [expr {$x+0.5*$delta}] 0 [expr {$x+0.5*$delta}] $ymax $width black 1
	switch $unit {
	    cm { set text [expr int([pt2cm $x])] }
	    pt {  set text $x }
	}
	WriteText $text $x [expr {$ymax-[cm2pt 1]}] Helvetica 10 nw
	WriteText $text $x [cm2pt 1] Helvetica 10 nw
    }
    for { set y 0 } { $y < $ymax } { set y [expr {$y+$delta}] } {
	PDFWriter::PDF::CreateLine 0 $y $xmax $y $width black
	PDFWriter::PDF::CreateLine 0 [expr {$y+0.5*$delta}] $xmax [expr {$y+0.5*$delta}] $width black 1
	switch $unit {
	    cm { set text [expr int([pt2cm $y])] }
	    pt {  set text $y }
	}
	WriteText $text [expr  {$xmax-[cm2pt 1]}] $y Helvetica 10 nw
	WriteText $text [cm2pt 1] $y Helvetica 10 nw
    }
}

proc PDFWriter::PDF::LineSpace { font size } {
    variable pdfhandle
    variable pagehandle
    variable fonts

    $pagehandle setFontAndSize $fonts($font) $size
    set leading [$pagehandle getTextLeading]
    if { $leading == 0 } { set leading $size }
    return [expr $leading*1.2]
}

proc PDFWriter::PDF::CapHeight { font size } {
    variable pdfhandle
    variable fonts

    set s [expr {[$fonts($font) getCapHeight]/1000.0}]
    if { $s == 0 } {
	set s [expr {[$fonts(Helvetica) getCapHeight]/1000.0}]
    }
    return $s
}

proc PDFWriter::PDF::Ascender { font size } {
    variable pdfhandle
    variable fonts
    
    set s [expr {[$fonts($font) getAscent]/1000.0}]
    if { $s == 0 } {
	set s [expr {[$fonts(Helvetica) getAscent]/1000.0}]
    }
    return $s
}

proc PDFWriter::PDF::Descender { font size } {
    variable pdfhandle
    variable fonts

    set s [expr {[$fonts($font) getDescent]/1000.0}]
    if { $s == 0 } {
	set s [expr {[$fonts(Helvetica) getDescent]/1000.0}]
    }
    return $s
}

proc PDFWriter::PDF::TextWidth { text font size } {
    variable pdfhandle
    variable pagehandle
    variable fonts

    $pagehandle setFontAndSize $fonts($font) $size
    return [$pagehandle textWidth [_encode_text $text]]
}

proc PDFWriter::PDF::_encode_text { text } {
    variable tcl_encoding

    return [encoding convertto $tcl_encoding $text]
}

proc PDFWriter::PDF::WriteText { text x y font size anchor { color "" } } {
    variable pdfhandle
    variable pagehandle
    variable fonts

    
    if 0 {
	_SetColorDashLineWidth blue "" ""
	$pagehandle circle $x $y .5
	$pagehandle fill
	$pagehandle setFontAndSize $fonts($font) 6
	$pagehandle beginText
	$pagehandle textOut [expr {$x+2}] $y [_encode_text $anchor]
	$pagehandle endText
	_ResetColorDashLineWidth
    }
    _SetColorDashLineWidth $color "" ""
    $pagehandle setFontAndSize $fonts($font) $size

    set width 0
    set NL 0
    set height1 [$pagehandle getTextLeading]
    if { $height1 == 0 } { set height1 $size }
    set height2 [expr {[Ascender $font $size]*$size}]

    foreach i [::textutil::split::splitx $text {\s*(?:\n|\\n)\s*}] {
	if { [$pagehandle textWidth [_encode_text $i]] > $width } {
	    set width [$pagehandle textWidth [_encode_text $i]]
	}
	incr NL
    }
    set height [expr ($NL-1)*1.2*$height1+$height2]

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
    $pagehandle beginText

    foreach i [::textutil::split::splitx $text {\s*(?:\n|\\n)\s*}] {
	$pagehandle textOut $x $y [_encode_text $i]
	set y [expr $y-1.2*$height1]
    }
    $pagehandle endText
    _ResetColorDashLineWidth


#     _SetColorDashLineWidth red "" ""
#     PDF_circle $pdfhandle $x [expr $y+1.2*$height1] .5
#     PDF_fill $pdfhandle
#     _ResetColorDashLineWidth


    return [list $width [expr $NL*1.2*$height1]]
}

proc PDFWriter::PDF::ScaleCoordSystem { xc yc scalefactx { scalefacty "" } } {
    variable pdfhandle
    variable pagehandle

    if { $scalefacty eq "" } { set scalefacty $scalefactx }

    $pagehandle gSave
    $pagehandle concat $scalefactx 0 0 $scalefacty $xc $yc
    $pagehandle concat 1 0 0 1 [expr {-1*$xc}] [expr {-1*$yc}]
}

proc PDFWriter::PDF::RotateCoordSystem { x y angle } {
    variable pdfhandle
    variable pagehandle
    
    math::constants::constants degtorad
    set angle [expr {$angle*$degtorad}]

    $pagehandle gSave
    $pagehandle concat [expr {cos($angle)}] [expr {sin($angle)}] \
	[expr {-sin($angle)}] [expr {cos($angle)}] \
	$x $y
    $pagehandle concat 1 0 0 1 [expr {-1*$x}] [expr {-1*$y}]
}

proc PDFWriter::PDF::RestoreCoordSystem {} {
    variable pdfhandle
    variable pagehandle

    $pagehandle gRestore
}

# angle in degrees counterclockwise from the positive x axis
proc PDFWriter::PDF::WriteTextRotated { text x y font size anchor color angle } {
    variable pdfhandle
    variable pagehandle
    
    math::constants::constants degtorad
    set angle [expr {$angle*$degtorad}]

    $pagehandle gSave
    $pagehandle concat [expr {cos($angle)}] [expr {sin($angle)}] \
	[expr {-sin($angle)}] [expr {cos($angle)}] \
	$x $y
    WriteText $text 0 0 $font $size $anchor $color
    $pagehandle gRestore
}

# mode can be left, right, center, justify or fulljustify
proc PDFWriter::PDF::WriteTextBox { text x y width mode font size } {
    variable pdfhandle
    variable pagehandle
    variable fonts

    $pagehandle setFontAndSize $fonts($font) $size

    if { $width == 0 } {
	switch $mode {
	    left { set anchor w }
	    right { set anchor e }
	    center { set anchor center }
	}
	WriteText $text $x $y $font $size $anchor
	return
    }
    set x [expr $x+[cm2pt 0.25]]
    set width [expr $width-2*[cm2pt 0.25]]

    set heightbase [$pagehandle getTextLeading]
    if { $heightbase == 0 } { set heightbase $size }
    set height $heightbase
    set textL $text
    
    while 1 {
	set r [$pagehandle measureText [_encode_text $textL] $width 0]
	if { $r == [string length $textL] } { break }
	set height [expr $height+$heightbase]
	set textL [string range $textL $r end]
    }
    set heightT [expr {$height+0.2*$heightbase}]
    $pagehandle beginText
    $pagehandle textRect $x [expr {$y+$heightbase/2}] [expr {$x+$width}] [expr $y-$heightT] \
	[_encode_text $text] $mode
    $pagehandle endText
    return [expr $y-$height+$heightbase/2]
}

proc PDFWriter::PDF::LocalLink { llx lly urx ury page_num } {
    variable local_link_stack
    variable pagehandle
    
    lappend local_link_stack [list $pagehandle $llx $lly $urx $ury $page_num]
}

proc PDFWriter::PDF::LocalLinkDo { pagehandleL llx lly urx ury page_num } {
    variable pdfhandle
    variable pagehandle

    set dest_pagehandle [$pdfhandle getPageByIndex [expr {$page_num-1}]]
    $pagehandleL createLinkAnnot [list $llx $lly $urx $ury] [$dest_pagehandle createDestination]
}

proc PDFWriter::PDF::End { { outputPDF 1 } } {
    variable pdffile
    variable pagehandle
    variable pdfhandle
    variable cacheimages
    variable local_link_stack
    
    array unset cacheimages

    if { $pdfhandle eq "" } {
	error "error in PDFWriter::PDF::End. There is no handle"
    }
    
    foreach i $local_link_stack {
	lassign $i pagehandleL llx lly urx ury page_num
	LocalLinkDo $pagehandleL $llx $lly $urx $ury $page_num
    }
    set local_link_stack ""
    
    $pdfhandle saveToFile $pdffile
    hpdf::free $pdfhandle
    set pdfhandle ""
    set pagehandle ""

    if { !$outputPDF } {
	return $pdffile
    }

    if { $::tcl_platform(platform) == "windows" } {
	set err [catch {
	    package require registry
	    set root HKEY_CLASSES_ROOT
	    set appKey [registry get $root\\.pdf ""]
	    set appCmd [registry get $root\\$appKey\\shell\\open\\command ""]
	    set appCmd [string map [list %1 [file native $pdffile]] $appCmd]
	    #regsub {%1} $appCmd [file native $pdffile] appCmd
	    regsub -all {\\} $appCmd {\\\\} appCmd
	    eval exec $appCmd &
	} errstring]
	if { $err } {
	    set file [tk_getSaveFile -filetypes \
		          {{{PDF files} ".pdf"} {{All files} "*"}}  \
		          -initialdir [pwd] \
		          -title "Save PDF file" -defaultextension .pdf]
	    if { $file == "" } { return }
	    if { [string equal [file normalize $pdffile] [file normalize $file]] } {
		return $pdffile
	    }
	    file copy -force $pdffile $file
	}
	#eval exec [auto_execok start] [list "" [file native $pdffile]] &
    } else {
	set programs [list xdg-open gnome-open acroread evince]
	if { $::tcl_platform(os) eq "Darwin" } {
	    set programs [linsert $programs 0 open]
	}
	set found 0
	foreach p $programs {
	    if { [auto_execok $p] ne "" } {
		exec {*}[auto_execok $p] $pdffile &
		set found 1
		break
	    }
	}
	if { !$found } {
	    WarnWin [_ "File cannot be visualized. Neither of programs '%s' is installed in the system" $programs]
	}
    }
    return $pdffile
}

