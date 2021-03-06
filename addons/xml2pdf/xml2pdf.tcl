
#package require resizer
# note: there are some problems with the png library between img::png and pdfwriter
set fail_tk [catch { package present Tk }]
if { !$fail_tk } {
    package require img::gif
    package require img::jpeg
    package require img::png
}
package require pdfwriter
package require tdom
package require msgcat
package require math::constants
package require compass_utils
package require base64

if 0 {
    # this is a trick for the automatic installers to find the package
}

if { [info commands ::_] eq "" } {
    proc ::_ { args } {
	set ret [uplevel 1 ::msgcat::mc $args]
	regexp {(.*)#C#(.*)} $ret {} ret
	return $ret
    }
}

namespace eval ::base64 {
    proc isbase64 { data } {
	if { [regexp {^[a-zA-Z0-9+/=\s]*$} $data] } {
	    return 1
	} else { return 0 }
    }
}

msgcat::mcload [file join [file dirname [info script]] msgs]

package provide xml2pdf 1.3
lappend ::auto_path [file dirname [info script]]

################################################################################
#    Print PDF
################################################################################

namespace eval xml2pdf {
    variable itemslist [lsort -ascii [list \#text blockquote emphasis itemizedlist listitem literal \
	orderedlist para subscript superscript sub sup mediaobject imageobject imagedata \
	caption table ulink beginpage]]

    set fail_tk [catch { package present Tk }]
    if { !$fail_tk } {

    variable dot_file [file join [file dirname [info script]] dot.png]
# table row entry tabstyle
    #<ulink url="http://www.ora.com/catalog/tex/">Making TeX</ulink>.
	if { [lsearch [image names] dot] == -1 } {
	    variable dot_data [base64::decode {
		    iVBORw0KGgoAAAANSUhEUgAAABQAAAATCAYAAAHnix0jAAAABGdBTUEAAYagMeiWXwAAAtNJ
		    REFUGJW9kk2IVlUYx3/nOed+NPd1ctRFpc1sLA2RiBZRFJQLA1sKuYiIIImkIiFq00YQXYhF
		    JfRBGyOChBQmhCTCkWFgMGuRTFOEmIU2unhn3nydee/HOU+LO87MZcZFUP135/Cc//l/PLAE
		    BmDHE8O6afNAffP50V+UxsjuZ7/VzkxRnyYn2moATp28pF989itRLHx0dJtxAF+d+I089yhw
		    4tiFJlHjzxefO61F4Vm3LuXQ+48aGR25rDPTOVNXZmm3cwDkscfXm6zlWLM2IU1tk+fD985r
		    g3MpPv1kUjudgm3bN7Bl61qzbPCl509rt1vSm/OoKmnqyFqO7TsG2blroxGA0ZEr6r1SVUqR
		    e4o8UFUB75WxM382Gb/5+ncdPn6Rsgyg4CKhf1XEgbcfWSbv30GDdnxsSsfHprhrfcbTz9xj
		    Vhx8+YUR7fU83isihjS1vPr6/Wy6b8AACMDBfed0bs7TvV7yV6ege71kdrbiyDs/LjA6gMt/
		    dCnLQN7z3LhRclufI4oFXy02LwDGGMSAOEOcWKwVRAxmiUoBuHuoRZJasj5Hf39MljmSxBLF
		    stzM3j2jmvc8IdRm4ljY89pW7t1cm2lEcHb8qn43fpU77uxj566N/1Ej/xQryvj+7DU9fuwC
		    09M5ISjMpyliEGsYHGrxxlsPrvi2cTk50dYP3j1PnnvKMlCVivcBVTAGxBqcE5wT4lgYWJOw
		    /9DDK68OwCu7z2iee4rckxeBovBURcAHRYzBRTVRnFjiWEgSy+2rEw4cXiRdKPLUyUuqqqgq
		    QSF4JVRKWQWKecXeB0JQQlBUQRXmZquG5QXCJ58aMtbWdqJIiBOplymLWNUfk7UcaeqIY0sU
		    Cc4ZrDNkLXfrDH/+qa0fH5mgKDzlfH7B12owdSnW1tajSFg9kLDv4EO3zvAmfjh3TYe/vEhn
		    piDoYstGwIphw2CLvW8+8P8s6t8UcTkB9TRvWAAAAABJRU5ErkJggg==
		}]
	    image create photo dot -data $dot_data
	    image create photo no_image -data {
		R0lGODlhbgAkAKIAANnZ2cz//5n//2b//zP//wD//////////yH5BAEAAAAALAAAAABuACQA
		AAP/CLrc/jDKSau9OOvNu/9gKI5kaZ5oqq5s675wLM90bd8VEjISCLrc/jDKCVFAIOjG7q61
		FpIhEgRdbn8Y5XQoZkYCAyBmcARBl5tRqEgQdLn9YZTTIRmiEQwAGUISBF1uxpgZQdDl9odR
		ToeEiGhQAGQISRB0uRsDAkGX2x9GOR+SoRnBCIghJAGAwBARCQwE3Q3BAAgRDBGRQAkU1QBA
		CBRdiUBQDRTVkMAAgBAUkYhA0OUOHCIBQYkYIhQAEBxVEQRdlUGaABmkoRnC0VUBBBCiwaGZ
		EQSFkMGhGRocAQjBURoJBF1uQaIREJyAIUKBEMHRnQgEXZlCEhgipJEZ/0KaERkkkgDBkRER
		HJWAQBkaGRwqEsCQGRGUGQkEXWbBIRIQnIAhQoEYGhQAGcIRAAgEkCHCACEaDAAZwhEAGaQR
		BF2VQZoAGaQJAMGhCRAcmgBAIZJA0OUNJBqBQAkZIhQQosEAkCEcQdCVISQBIcIBACGkEYDA
		IRoAhBARwRkiARSiCUAQohGEIRpBmSEiDARdXsEhEgAMGSJCASEaFAAZQhIEXRnCERAinACQ
		QSIBEBwiAcCYoSEcohFAIZoABJmaQBCiIaQZmsFA0OUVHBoBwJgZGhQIosEACCIcAQhAABki
		DBCiwQAQIhwBkEEigRikkQjBIQoQJBoAQCEaAf8UoolAiRAEXW7BKRIADBkiQoGYGZQIocEJ
		BF2ZQhIQIpwAEEKiABAcIgFBopEAFCISwBgaCQwhGkGIoQkMiIBA0GUWnBoBQAgZIhyAmEGa
		oREMBF2QwRoBKaQJAEFSARCcIoFAIZoZHCISwIihGaQhGkEIERxV0kDQ5Q0cFQRVoUEBgBAc
		HQkEXRXBmQDRQaAyIygAIYMjECE4uiqAACI4qkSCoCE4qjSBoMsdCLrcgQABoaqUAigQVGN3
		F0CBoBoQGBAAgaC7QkgTCKoRgQERCLrc/jDKSSUS1ZlBCQRdbn8Y5aQVohjCURUEXW5/GOWk
		dSKRkVEMBF1ufxjlpNUUXpz15t1/MBRHsjRPNFVXtnVfCUkAOw==
	    }
	}
    }
    set scriptdir [file dirname [info script]]
    set templatesdirList [list [file join $scriptdir templates]]
    set WordtemplatesdirList [list [file join $scriptdir word_templates]]
    variable svg2draw
}

proc xml2pdf::TakeOutAttachedItems {} {
    variable paragraphstate

    set p $paragraphstate(ItemsToPrint)

    for { set i [expr {[llength $p]-1}] } { $i >= 0 } { incr i -1 } {
	if { [regexp {[-\s+*/.,;·]$} [lindex $p $i 0]] } {
	    break
	}
    }
    if { $i < 0 } { return "" }

    set paragraphstate(ItemsToPrint) [lrange $p 0 $i]
    return [lrange $p [expr {$i+1}] end]
}

proc xml2pdf::FlushItemsToPrint { justify } {
    variable paragraphstate
    variable state ;# only using maxxreal
    variable dot_file

    set maxyposdown $paragraphstate(ypos)
    foreach i $paragraphstate(ItemsToPrint) {
	foreach "v xpos ypos font size fontcolor width height -" $i break
	# here should be added a bigger interline space, if necessary
	# .66 is necessary because we are moving .33 in AddTextToParagraph
	
	if { $font ne "formula" } {
	    set yposdown [expr {$ypos-$paragraphstate(interline)*.66*$height}]
	} else {
	    lassign $fontcolor hup hdown
	    set yposdown [expr {$ypos-$hdown-$paragraphstate(interline)*.66*$height}]
	}
	if { $yposdown < $maxyposdown } {
	    set maxyposdown $yposdown
	}
	if { $xpos+$width > $state(maxxreal) } { set state(maxxreal) [expr {$xpos+$width}] }
    }
    
    set delta_ypos 0
    foreach i $paragraphstate(ItemsToPrint) {
	foreach "v xpos ypos font size fontcolor width height -" $i break
	
	if { $font eq "formula" } {
	    lassign $fontcolor hup hdown
	    set d  [expr {$ypos+$hup-$paragraphstate(ypos)}]
	} else {
	    set d  [expr {$ypos-$paragraphstate(ypos)}]
	}
	if { $d > 0 && $d > $delta_ypos } { set delta_ypos $d }
    }
    if { $delta_ypos > 0 } {
	set idx 0
	foreach i $paragraphstate(ItemsToPrint) {
	    foreach "v xpos ypos font size fontcolor width height -" $i break
	    lset paragraphstate(ItemsToPrint) $idx 2 [expr {$ypos-$delta_ypos}]
	    incr idx
	}
    }
    
    if { $paragraphstate(print) == 0 } {
	set paragraphstate(ItemsToPrint) ""
	return $maxyposdown
    }

    if { $paragraphstate(list_type) != "" } {
	lassign $paragraphstate(list_type) type xposi txt x_indented
	lassign [lrange [lindex $paragraphstate(ItemsToPrint) 0] 2 7] ypos font size fontcolor - height
	switch $type {
	    itemizedlist {
		set yposi [expr {$ypos-0.3*$height}]
		PDFWriter::InsertImg dot $xposi $yposi w $paragraphstate(ImgDocFactor)
	    }
	    orderedlist {
		if { $font == "-" } { set fontt $state(font) } else { set fontt $font }
		PDFWriter::WriteText $txt $xposi $ypos $fontt $size ne $fontcolor
	    }
	}
	set paragraphstate(list_type) ""
	
	if { $paragraphstate(xend)-$paragraphstate(xini) < 180 } {
	    # 180 is arbitrary. Small columns do not indent following lines in listitems
	    set paragraphstate(xini) $x_indented
	}
    }
    
    if { $paragraphstate(background) ne "" } {
	set yposup ""
	set yposdown ""
	foreach i $paragraphstate(ItemsToPrint) {
	    lassign $i v xpos ypos font size fontcolor width height -
	    set ypos [expr {$ypos+0.2*$height}]
	    if { $yposdown eq "" } { set yposup $ypos ; set yposdown $ypos }
	    set yposdownL [expr {$ypos-$height}]
	    if { $yposdownL < $yposdown } {
		set yposdown $yposdownL
	    }
	    if { $yposdown != "" } {
		set x1 [expr {$paragraphstate(xini)-1}]
		set y1 [expr {$yposup+1}]
		set width [expr {$paragraphstate(xend)-$paragraphstate(xini)+2}]
		set height [expr {$yposup-$yposdown+2}]
		PDFWriter::CreateFilledBox $x1 $y1 $width $height \
		    nw 0 "" $paragraphstate(background)
	    }
	}
    }
    if { $justify == "left" } {
	foreach i $paragraphstate(ItemsToPrint) {
	    lassign $i v xpos ypos font size fontcolor width height -
	    if { $font eq "image" } {
		set scale [expr {$width/double([image width $v])}]
		PDFWriter::InsertImg $v $xpos $ypos nw $scale
		set needs_delete $fontcolor
		if { $needs_delete } { image delte $v }
	    } elseif { $font eq "formula" } {
		drawformula::drawformula $size $v 1 $xpos $ypos [expr {$width+1}]
	    } elseif { $font == "-" } {
		WriteUserDefined $v $xpos $ypos $font $size $fontcolor $width $height
	    } else {
		#PDFWriter::CreateLine $xpos $ypos [expr {$xpos-100}] $ypos 0.1 red
		PDFWriter::WriteText $v $xpos $ypos $font $size nw $fontcolor
	    }
	}
    } elseif { $justify == "center" || $justify == "right" } {
	set delta 0.0
	foreach i $paragraphstate(ItemsToPrint) {
	    foreach "v xpos ypos font size fontcolor width height -" $i break
	    # just to set the last one
	    set delta [expr $paragraphstate(xend)-$xpos-$width]
	}
	if { $justify == "center" } { set delta [expr {$delta/2.0}] }
	foreach i $paragraphstate(ItemsToPrint) {
	    foreach "v xpos ypos font size fontcolor width height -" $i break
	    if { $font eq "image" } {
		PDFWriter::InsertImg $v [expr {$xpos+$delta}] $ypos nw $size
		set needs_delete $fontcolor
		if { $needs_delete } { image delte $v }
	    } elseif { $font eq "formula" } {
		drawformula::drawformula $size $v 1 [expr {$xpos+$delta}] $ypos [expr {$width+1}]
	    } elseif { $font == "-" } {
		WriteUserDefined $v [expr {$xpos+$delta}] $ypos $font $size $fontcolor $width $height
	    } else {
		PDFWriter::WriteText $v [expr {$xpos+$delta}] $ypos $font $size nw $fontcolor
	    }
	}
    } else {
	set i 0
	set totalfac 0.0
	foreach j $paragraphstate(ItemsToPrint) {
	    foreach "v xpos ypos font size fontcolor width height -" $j break
	    # just to set the last one
	    set delta [expr $paragraphstate(xend)-$xpos-$width]
	    
	    if { $i > 0 } {
		set fac($i) $width
		set totalfac [expr $totalfac+$fac($i)]
	    }
	    incr i
	}
	set accumulated 0
	set i 0
	foreach j $paragraphstate(ItemsToPrint) {
	    foreach "v xpos ypos font size fontcolor width height -" $j break
	    if { $i > 0 && $fac($i) > 0 } {
		set accumulated [expr $accumulated+double($fac($i))/$totalfac*$delta]
	    }
	    if { $font eq "image" } {
		PDFWriter::InsertImg $v [expr {$xpos+$accumulated}] $ypos nw $size
		set needs_delete $fontcolor
		if { $needs_delete } { image delete $v }
	    }  elseif { $font eq "formula" } {
		drawformula::drawformula $size $v 1 [expr {$xpos+$accumulated}] $ypos [expr {$width+1}]
	    } elseif { $font == "-" } {
		WriteUserDefined $v [expr {$xpos+$accumulated}] $ypos $font $size $fontcolor \
		    $width $height
	    } else {
		PDFWriter::WriteText $v [expr $xpos+$accumulated] $ypos $font $size nw $fontcolor 
	    }
	    incr i
	}
    }
    set paragraphstate(ItemsToPrint) ""
    return $maxyposdown
}


proc xml2pdf::BeginParagraph { xini xend ypos ymin SpaceAfterLineText interline list_type \
    print newpagescript ImgDocFactor justify { background ""} } {
    variable paragraphstate

    set paragraphstate(ItemsToPrint) ""
    set paragraphstate(xini) $xini
    set paragraphstate(xend) $xend
    set paragraphstate(xpos) $xini
    set paragraphstate(ypos) $ypos
    set paragraphstate(isactive) 1
    set paragraphstate(print) $print
    set paragraphstate(newpagescript) $newpagescript
    set paragraphstate(ymin) $ymin
    set paragraphstate(SpaceAfterLineText) $SpaceAfterLineText
    set paragraphstate(interline) $interline
    set paragraphstate(list_type) $list_type
    set paragraphstate(background) $background
    set paragraphstate(ImgDocFactor) $ImgDocFactor
    set paragraphstate(justify) $justify
}

proc xml2pdf::EndParagraph {} {
    variable paragraphstate

    if { $paragraphstate(justify) != "justify" } {
	set paragraphstate(ypos) [FlushItemsToPrint $paragraphstate(justify)]
    } else {
	set paragraphstate(ypos) [FlushItemsToPrint left]
    }
    set paragraphstate(xpos) $paragraphstate(xini)
    set paragraphstate(isactive) 0
    return $paragraphstate(ypos)
}

proc xml2pdf::IsParagraphActive {} {
    variable paragraphstate

    if { ![info exists paragraphstate(isactive)] } { return 0 }
    return $paragraphstate(isactive)
}

proc xml2pdf::AddTextToParagraph { text font_base size_base fontcolor relativev_base } {
    variable paragraphstate
    variable greekletters

    if { !$paragraphstate(isactive) } { error "error in xml2pdf::AddTextToParagraph" }

    regsub -all {\\n} $text { } text
    regsub -all {\n} $text { } text
    set nonvalid {[\u0096\u0097]}
    regsub -all $nonvalid $text {} text
    while 1 {
	lassign [list $font_base $size_base $relativev_base] font size relativev
	set font $font_base
	set ret [regexp {^(\s*\S+)(\s*)(.*)$} $text {} vres space rest]
	if { !$ret } {
	    set space $text
	    set text ""
	    set rest ""
	} else {
	    set len [string length $vres]
	    set text ""
	    for { set i 0 } { $i < $len } { incr i } {
		set c [string index $vres $i]
		set csymbol ""
		
		set cidx [scan $c %c]

		set map [list \
		        \u2018 ' \
		        \u2019 ' \
		        \u0009 "        " \
		        ]

		set c [string map $map $c]

		if {  [PDFWriter::GiveEncoding] eq "utf-8" } {
		    #nothing
		} elseif { $c == "\u20ac" } {
		    #nothing
		} elseif { $cidx == 0x226e || $cidx == 0x226f } {
		    # not smaller than ; not bigger than
		    if { $i == 0 } {
		        append text $c
		        set font -
		        set rest [string range $vres 1 end]$space$rest
		    } else {
		        set rest [string range $vres $i end]$space$rest
		    }
		    set space ""
		    break
		} elseif { $cidx == 0x2074 } {
		    # ^4
		    if { $i == 0 } {
		        set size [expr int(.8*$size)]
		        set relativev raise
		        append text "4"
		        set rest [string range $vres 1 end]$space$rest
		    } else {
		        set rest [string range $vres $i end]$space$rest
		    }
		    set space ""
		    break
		} elseif { $cidx >= 0x391 } {
		    set ipos [lsearch -exact $greekletters [format %x $cidx]]
		    if { $ipos != -1 && [lindex $greekletters [expr {$ipos-2}]] != "-" } {
		        set csymbol [lindex $greekletters [expr {$ipos-2}]]
		    }
		}
		if { $cidx >= 0x391 && $csymbol != "" } {
		    if { $i == 0 } {
		        append text [format %c 0x$csymbol]
		        set font Symbol
		        set rest [string range $vres 1 end]$space$rest
		    } else {
		        set rest [string range $vres $i end]$space$rest
		    }
		    set space ""
		    break
		} else {
		    append text $c
		}
	    }
	}
	if { [string length $space] > 1 } { set space " " }
	
	if { $font == "-" } {
	    set width0 [PDFWriter::TextWidth $text Symbol $size]
	    set height [PDFWriter::LineSpace Symbol $size]
	} else {
	    set width0 [PDFWriter::TextWidth $text $font $size]
	    set height [PDFWriter::LineSpace $font $size]
	}

	if { $paragraphstate(newpagescript) != "" && $paragraphstate(print) && \
	    $paragraphstate(ypos)-(1.0+$paragraphstate(SpaceAfterLineText))*$height\
		 < $paragraphstate(ymin) } {
	    set paragraphstate(ypos) [eval $paragraphstate(newpagescript)]
	}

	if { $paragraphstate(xpos)+$width0 > $paragraphstate(xend) && \
	    $paragraphstate(xpos) > $paragraphstate(xini)+1 } {

	    set items [TakeOutAttachedItems]
	    set paragraphstate(ypos) [FlushItemsToPrint $paragraphstate(justify)]
	    set paragraphstate(xpos) $paragraphstate(xini)

	    foreach i $items {
		foreach "v_in xpos_in ypos_in font_in size_in fontcolor_in width_in height_in relativev_in" \
		    $i break
		_AddItemToParagraph $v_in $font_in $size_in $fontcolor_in $width_in $height_in $relativev_in
	    }

	    if { $paragraphstate(newpagescript) != "" && $paragraphstate(print) && \
		$paragraphstate(ypos)-(1.0+$paragraphstate(SpaceAfterLineText))*$height \
		     < $paragraphstate(ymin) } {
		set paragraphstate(ypos) [eval $paragraphstate(newpagescript)]
	    }
	}
	append text $space
	if { $font == "-" } {
	    set width [PDFWriter::TextWidth $text Symbol $size]
	} else {
	    set width [PDFWriter::TextWidth $text $font $size]
	}
	set yposL $paragraphstate(ypos)
	#PDFWriter::CreateLine $paragraphstate(xpos) $yposL \
	#    [expr {$paragraphstate(xpos)-100}] $yposL 0.1 green
	if { $relativev == "raise" } {
	    set yposL [expr {$yposL-0.1*$height}]
	} elseif { $relativev == "lower" } {
	    set yposL [expr {$yposL-0.8*$height}]
	} else {
	    set yposL [expr {$yposL-.33*$height}]
	}
	lappend paragraphstate(ItemsToPrint) [list $text $paragraphstate(xpos) $yposL $font $size \
		                                  $fontcolor $width $height $relativev]
	set paragraphstate(xpos) [expr {$paragraphstate(xpos)+$width}]
	if { $paragraphstate(xpos) > $paragraphstate(xend) } {
	    set paragraphstate(ypos) [FlushItemsToPrint $paragraphstate(justify)]
	    set paragraphstate(xpos) $paragraphstate(xini)
	    
	    if { $paragraphstate(newpagescript) != "" && $paragraphstate(print) && \
		$paragraphstate(ypos)-(1.0+$paragraphstate(SpaceAfterLineText))*$height \
		     < $paragraphstate(ymin) } {
		set paragraphstate(ypos) [eval $paragraphstate(newpagescript)]
	    }
	}
	if { $rest == "" } { break }
	set text $rest
    }
    return $paragraphstate(ypos)
}

proc xml2pdf::_AddItemToParagraph { v font size fontcolor width height relativev } {
    variable paragraphstate
    
    set can_change_page 0
    if { $paragraphstate(newpagescript) ne "" && $paragraphstate(print) } { set can_change_page 1 }
    
    if { $can_change_page && $paragraphstate(ypos)-(1.0+$paragraphstate(SpaceAfterLineText))*$height\
	    < $paragraphstate(ymin) } {
	set yposL $paragraphstate(ypos)
	set paragraphstate(ypos) [eval $paragraphstate(newpagescript)]
	set delta [expr {$paragraphstate(ypos)-$yposL}]
	for { set i 0 } { $i < [llength $paragraphstate(ItemsToPrint)] } { incr i } {
	    lset paragraphstate(ItemsToPrint) $i 2 [expr {[lindex $paragraphstate(ItemsToPrint) $i 2]+$delta}]
	}
    }
    
    if { $paragraphstate(xpos)+$width > $paragraphstate(xend) && \
	$paragraphstate(xpos) > $paragraphstate(xini)+1 } {
	
	set items [TakeOutAttachedItems]
	set paragraphstate(ypos) [FlushItemsToPrint $paragraphstate(justify)]
	set paragraphstate(xpos) $paragraphstate(xini)
	
	foreach i $items {
	    foreach "v_in xpos_in ypos_in font_in size_in fontcolor_in width_in height_in relativev_in" \
		$i break
	    _AddItemToParagraph $v_in $font_in $size_in $fontcolor_in $width_in $height_in $relativev_in
	}
	return [_AddItemToParagraph $v $font $size $fontcolor $width $height $relativev]
    }
    set yposL $paragraphstate(ypos)
    
    if { [string is double -strict $relativev] } {
	set fac $relativev
    } elseif { $font eq "image" } {
	set fac 0
    } elseif { $font eq "formula" } {
	set fac 0
    } else {
	switch $relativev {
	    raise { set fac -0.3 }
	    lower { set fac -1.2 }
	    default { set fac -0.33 }
	}
    }
    set yposL [expr {$yposL+$fac*$height}]
    
    lappend paragraphstate(ItemsToPrint) [list $v $paragraphstate(xpos) $yposL $font $size \
	    $fontcolor $width $height $relativev]
    set paragraphstate(xpos) [expr {$paragraphstate(xpos)+$width}]
    if { $paragraphstate(xpos) > $paragraphstate(xend) } {
	set paragraphstate(ypos) [FlushItemsToPrint $paragraphstate(justify)]
	set paragraphstate(xpos) $paragraphstate(xini)

	if { $can_change_page && \
	    $paragraphstate(ypos)-(1.0+$paragraphstate(SpaceAfterLineText))*$height \
		< $paragraphstate(ymin) } {
	    set paragraphstate(ypos) [eval $paragraphstate(newpagescript)]
	}
    }
    return $paragraphstate(ypos)
}   

proc xml2pdf::AddImagesToParagraph { image width height needs_delete } {
    variable paragraphstate

    if { !$paragraphstate(isactive) } { error "error in xml2pdf::AddImagesToParagraph" }

    return [_AddItemToParagraph $image image 1.0 $needs_delete $width $height ""]
}

proc xml2pdf::AddFormulaToParagraph { formula width height size relativev hup hdown } {
    variable paragraphstate

    if { !$paragraphstate(isactive) } { error "error in xml2pdf::AddFormulaToParagraph" }

    return [_AddItemToParagraph $formula formula $size [list $hup $hdown] $width $height \
	    $relativev]
}


proc xml2pdf::WriteUserDefined { txt xpos ypos font size fontcolor width height } {

    set cidx [scan [lindex $txt 0] %c]
	
    if { $cidx == 0x226e || $cidx == 0x226f } {
	if { $cidx == 0x226e } {
	    PDFWriter::WriteText < $xpos $ypos Symbol $size nw $fontcolor
	} else {
	    PDFWriter::WriteText > $xpos $ypos Symbol $size nw $fontcolor
	}
	PDFWriter::CreateLine [expr {$xpos+2}] [expr {$ypos-$size}]  \
	    [expr {$xpos+$width-2}] [expr {$ypos-4}] \
	    .5 $fontcolor
    }

}

proc alpha:number {i} {

    #set num [expr {[scan z %c]-[scan a %c]+1}]
    # only from 0 to 26
    return [format %c [expr {[scan A %c]+$i-1}]]
}

proc roman:number {i} {
    set res ""
    foreach {value roman} {
	1000 M 900 CM 500 D 400 CD 100 C 90 XC 50 L 40 XL 10 X 9 IX 5 V 4 IV 1 I} {
	while {$i>=$value} {
	    append res $roman
	    incr i -$value
	}
    }
    return $res
}

proc xml2pdf::EncodeSecName { list Numeration } {

    for { set i 0 } { $i < [llength $list] } { incr i } {
	set num [lindex $list $i]
	if { $num eq "" } { continue }
	switch $Numeration {
	    arabic {
		#nothing
	    }
	    loweralpha  {
		set num [string tolower [alpha:number $num]]
	    }
	    upperalpha {
		set num [alpha:number $num]
	    }
	    lowerroman {
		set num [string tolower [roman:number $num]]
	    }
	    upperroman {
		set num [roman:number $num]
	    }
	}
	set list [lreplace $list $i $i $num]
    }
    return [join $list .]
}

proc xml2pdf::_DrawGraphics { graphicsnode x y scale } {
    variable state

    PDFWriter::ScaleCoordSystem $x $y $scale

#     package require mather
#     set mat [math::MatriuPerMatriu [math::TranslateMatrix2D [list $x $y]] [math::ScaleMatrix2D $scale $scale]]

    foreach node [$graphicsnode childNodes] {
	set coords ""
	foreach "xc yc" [$node text] {
	    #eval lappend coords [math::Matriu3PerVec2 $mat [list $xc $yc]]
	    lappend coords [expr {$x+$xc}]
	    lappend coords [expr {$y+$yc}]
	}
	set color [$node @color black]
	if { $color == "" } { set color black }
	switch [$node nodeName] {
	    "line" {
		eval PDFWriter::CreateLine $coords [$node @lwidth] \
		    $color [list [$node @dash]]
	    }
	    "linearrow" {
		eval PDFWriter::CreateLineArrow $coords [$node @arrow] \
		    [$node @lwidth] $color \
		    [list [$node @dash]]
	    }
	    "text" {
		if { [$node @orient ""] eq "vertical" } {
		    set size [expr {[$node @size]/double($scale)}]
		    eval PDFWriter::PDF::WriteTextRotated [list [$node @text]] $coords [list [$node @font]] \
		        $size [$node @anchor] $color 90
		} elseif { $state(defaultformula_ingraphics) || [regexp {<su(b|p)>} [$node @text]] } {
		    set size [$node @size]
		    # check for autotransform
		    set size [expr {$size/double($scale)}]
		    drawformula::Init pdf "" [$node @font] $size $color
		    foreach "tx ty" $coords break
		    foreach "w hup hdown" [drawformula::drawformula $size [$node @text] 0 \
		                               $tx $ty] break
		    set anchor [$node @anchor]
		    if { $anchor eq "center" } { set anchor "" }

		    if { [regexp s $anchor] } {
		        set ty [expr {$ty+$hup+$hdown}]
		    } elseif { ![regexp n $anchor] } {
		        set ty [expr {$ty+$hup}]
		    } else {
		        #set ty [expr {$ty-$hup}]
		    }

#                     if { [regexp s $anchor] } {
#                         #set ty [expr {$ty+$hup+$hdown}]
#                         set ty [expr {$ty+$hdown}]
#                     } elseif { ![regexp n $anchor] } {
#                         #set ty [expr {$ty+($hup+$hdown)/2.0}]
#                     } else {
#                         set ty [expr {$ty-$hup}]
#                     }
		    if { [regexp e $anchor] } {
		        set tx [expr {$tx-$w}]
		    } elseif { ![regexp w $anchor] } {
		        set tx [expr {$tx-$w/2.0}]
		    }
		    drawformula::drawformula $size [$node @text] 1 $tx $ty
		} else {
		    set size [expr {[$node @size]/double($scale)}]
		    eval PDFWriter::WriteText [list [$node @text]] $coords [list [$node @font]] \
		        $size [$node @anchor] $color
		}
	    }
	    "circle" {
		eval PDFWriter::CreateCircle $coords [$node @radius] [$node @lwidth] \
		    $color [list [$node @dash]] [$node @fill]
	    }
	    "arc" {
		eval PDFWriter::CreateArc $coords [$node @radius] [$node @startangle] \
		    [$node @endangle] [$node @lwidth] \
		    $color [list [$node @dash]]
	    }
	    "polygon" {
		PDFWriter::CreatePolygon $coords $color [$node @fill]
	    }
	    "bezier" {
		PDFWriter::CreateBezierD3 $coords [$node @lwidth] \
		    $color [$node @dash]
	    }
	    "image" {
		# it was commented out. Why?
		foreach "imgwidth imgheight" [PDFWriter::InsertImageNoDisk [$node @image] \
		        [lindex $coords 0] [lindex $coords 1] [$node @anchor] 1] break
	    }
	}
    }
    PDFWriter::RestoreCoordSystem
}

proc xml2pdf::_push_state { name value } {
    variable state
    variable state_stack
    
    if { [info exists state($name)] } {
	lappend state_stack [list $name 1 $state($name)]
    } else {
	lappend state_stack [list $name 0]
    }
    set state($name) $value
}

proc xml2pdf::_pop_state { name } {
    variable state
    variable state_stack
    
    if { [lindex $state_stack end 0] ne $name } {
	error "error in xml2pdf::_pop_state. name=$name state_stack=$state_stack"
    }
    if { [lindex $state_stack end 1] } {
	set state($name) [lindex $state_stack end 2]
    } else {
	unset state($name)
    }
    set state_stack [lrange $state_stack 0 end-1]
}

proc xml2pdf::TableToPdf { tableNode { print 1 } } {
    variable state

    if { [$tableNode @orient ""] eq "land" && $print } {
	set state(papersize_save) $state(papersize)
	set swap_page 1
	switch $state(papersize) {
	    A3 { set state(papersize) A3L }
	    A4 { set state(papersize) A4L }
	    A5 { set state(papersize) A5L }
	    default { set swap_page 0 }
	}
	if { $swap_page } {
	    set state(ypos) [PrintNewPage]
	}
    } else { set swap_page 0 }

    set save_state_items [list bigmarginh1 bigmarginh2 justify fontcolor size \
	    newpagescript]
    foreach i $save_state_items {
	set state_save($i) $state($i)
    }
    set state(newpagescript) ""

    if { [$tableNode @font-size ""] ne "" } {
	set state(size) [$tableNode @font-size]
    }
    set yspc [expr {$state(SpaceAfterLineText)*[PDFWriter::LineSpace $state(font) \
		                                   $state(size)]}]
    set xspc [expr {$yspc/2.0}]


    set percents [$tableNode @relativesizes ""]
    if { $percents eq "" } {
	set numcols 0
	set xp {row[1]/entry|tgroup/*/row[1]/entry}
	foreach i [$tableNode selectNodes $xp] {
	    incr numcols
	    incr numcols [$i @morerows 0]
	}
	if { !$numcols } {
	    error "error: incorrect table [$tableNode asXML]"
	}
	set percents [lrepeat $numcols [expr {100.0/$numcols}]]
	set percents [linsert $percents 0 0]
    }
    if { [lindex $percents 0] == "-" } {
	set sum [expr [join [lrange $percents 1 end] +]]
	lset percents 0 [expr {.5*(100-$sum)}]
    }

    set tabstyle [$tableNode @tabstyle greenbg]
    switch -glob -- $tabstyle {
	plain {
	    foreach "titlecolorT colorT" [list "" ""] break
	    set rowsepT 0
	    set colsepT 0
	}
	plainlines {
	    foreach "titlecolorT colorT" [list "" ""] break
	    set rowsepT 1
	    set colsepT 1
	}
	greenbg {
	    lassign [list \#0f9171 \#cdffcd] titlecolorT colorT

	    if { [info exists state(TableBackgroundColor)] } {
		set colorT $state(TableBackgroundColor)
	    }
	    if { [info exists state(TableTitleBackgroundColor)] } {
		set titlecolorT $state(TableTitleBackgroundColor)
	    }
	    set rowsepT 0
	    set colsepT 0
	}
	default {
	    foreach "titlecolorT colorT" [list "" ""] break
	    set rowsepT 0
	    set colsepT 0
	}
    }
    if { [$tableNode @rowsep ""] != "" } { set rowsepT [$tableNode @rowsep] }
    if { [$tableNode @colsep ""] != "" } { set colsepT [$tableNode @colsep] }

    set width_total [expr {$state(xmax)-$state(bigmarginh2)-$state(bigmarginh1)}]
    set ypos [expr {$state(ypos)-$yspc}]
    set yposlist $ypos
    set xp {row|tgroup/*/row}
    foreach row [$tableNode selectNodes $xp] {
	set width [expr {[lindex $percents 0]/100.0*$width_total}]
	set state(bigmarginh1) [expr {$state_save(bigmarginh1)+$width}]
	set ic 1
	set newypos $ypos
	foreach entry [$row selectNodes entry] {
	    set numrows [expr {1+[$entry @morerows 0]}]
	    set width 0.0
	    for { set i 0 } { $i < $numrows } { incr i } {
		set percent [lindex $percents [expr {$ic+$i}]]
		if { $percent eq "" } { set percent 10 }
		set width [expr {$width+$percent/100.0*$width_total}]
	    }
	    set bigmarginh1_next [expr {$state(bigmarginh1)+$width}]
	    set state(bigmarginh2) [expr {$state(xmax)-$bigmarginh1_next}]

	    set state(justify) center
	    foreach nod [list $tableNode $row $entry] {
		set j [$nod @cols_justify ""]
		if { $j eq "" } { continue }
		set idx [expr {($nod == $entry)?0:$ic-1}]
		if { [lindex $j $idx] ne "" } {
		    set state(justify) [lindex $j $idx]
		} elseif { [llength $j] == 1 } {
		    set state(justify) [lindex $j 0]
		}
	    }
	    set state(bigmarginh1) [expr {$state(bigmarginh1)+$xspc}]
	    set state(bigmarginh2) [expr {$state(bigmarginh2)+$xspc}]
	    set state(ypos) $ypos
	    
	    if { $width < 200 } { _push_state formula_indent_second_line 0 }
	    
	    #set state(ypos) [expr {$ypos+$yspc}]
	    TextdataToPDF [$entry childNodes] 0

	    if { [IsParagraphActive] } { set state(ypos) [EndParagraph] }
	    #set state(ypos) [expr {$state(ypos)+$yspc}]

	    if { $width < 200 } { _pop_state formula_indent_second_line }
	    
	    set state(bigmarginh1) [expr {$state(bigmarginh1)-$xspc}]
	    set state(bigmarginh2) [expr {$state(bigmarginh2)-$xspc}]

	    if { $state(ypos) < $newypos } { set newypos $state(ypos) }
	    set state(bigmarginh1) $bigmarginh1_next
	    incr ic $numrows
	}
	set ypos $newypos
	lappend yposlist $newypos
    }
    foreach i $save_state_items {
	set state($i) $state_save($i)
    }
    set state(newpagescript) ""

    if { [$tableNode @font-size ""] ne "" } {
	set state(size) [$tableNode @font-size]
    }
    
    set irow_page 0
    set xp {row|tgroup/*/row}
    set rowHeaderList [$tableNode selectNodes {tgroup/theader/row}]
    set rowList [$tableNode selectNodes {row|tgroup/*/row}]
    for { set irow 0 } { $irow < [llength $rowList] } { incr irow } {
	set row [lindex $rowList $irow]
	set width [expr {[lindex $percents 0]/100.0*$width_total}]
	set state(bigmarginh1) [expr {$state_save(bigmarginh1)+$width}]
	set ic 1
	
	set ypos [lindex $yposlist $irow]
	if { [lindex $yposlist [expr {$irow+1}]]-$yspc < $state(bigmarginv2) && $irow_page >= [llength $rowHeaderList] } {
	    set deltabigmarginh1 [expr {$state(bigmarginh1)-$state_save(bigmarginh1)}]
	    if { $print } {
		set ypos [PrintNewPage]
	    }
	    set state_save(bigmarginh1) $state(bigmarginh1)
	    set state_save(bigmarginh2) $state(bigmarginh2)
	    set state(bigmarginh1) [expr {$state(bigmarginh1)+$deltabigmarginh1}]
	    set irow_page 0
	}
	if { $irow >= [llength $rowHeaderList] && $irow_page <= [llength $rowHeaderList] } {
	    if { $irow_page < [llength $rowHeaderList] } {
		set row [lindex $rowList $irow_page]
		set ypos_next [expr {$ypos+[lindex $yposlist $irow_page+1]-[lindex $yposlist $irow_page]}]
		set diff [expr {$ypos_next-[lindex $yposlist $irow]}]
		for { set i $irow } { $i < [llength $yposlist] } { incr i } {
		    lset yposlist $i [expr {[lindex $yposlist $i]+$diff}]
		}
		incr irow -1
	    } else {
		set diff [expr {$ypos-[lindex $yposlist $irow]}]
		for { set i $irow } { $i < [llength $yposlist] } { incr i } {
		    lset yposlist $i [expr {[lindex $yposlist $i]+$diff}]
		}
		set ypos_next [lindex $yposlist [expr {$irow+1}]]
	    }
	} else {
	    set ypos_next [lindex $yposlist [expr {$irow+1}]]
	}
	if { $print } {
	    if { [$row @background-color ""] ne "" } {
		set color [$row @background-color]
	    } elseif { $irow_page == 0 } {
		set color $titlecolorT
	    } elseif { (($irow_page+1)/2)%2 != 0 } {
		set color $colorT
	    } else {
		set color ""
	    }
	    if { $color ne "" } {
		set wt 0.0
		foreach p [lrange $percents 1 end] {
		    set wt [expr {$wt+$p/100.0*$width_total}]
		}
		PDFWriter::CreateFilledBox $state(bigmarginh1) [expr {$ypos-1}] $wt \
		    [expr {$ypos-$ypos_next}] nw 0 "" $color
	    }
	}
	set rowsepR $rowsepT
	if { [$row @rowsep ""] != "" } { set rowsepR [$row @rowsep] }
	foreach entry [$row selectNodes entry] {
	    set numrows [expr {1+[$entry @morerows 0]}]
	    set width 0.0
	    for { set i 0 } { $i < $numrows } { incr i } {
		set percent [lindex $percents [expr {$ic+$i}]]
		if { $percent eq "" } { set percent 10 }
		set width [expr {$width+$percent/100.0*$width_total}]
	    }
	    set bigmarginh1_next [expr {$state(bigmarginh1)+$width}]
	    set state(bigmarginh2) [expr {$state(xmax)-$bigmarginh1_next}]

	    set state(ypos) $ypos

	    if { [$entry @background-color ""] ne "" } {
		PDFWriter::CreateFilledBox $state(bigmarginh1) [expr {$ypos-1}] $width \
		    [expr {$ypos-$ypos_next}] nw 0 "" [$entry @background-color]
	    }
	    set state(justify) center
	    foreach nod [list $tableNode $row $entry] {
		set j [$nod @cols_justify ""]
		if { $j eq "" } { continue }
		set idx [expr {($nod == $entry)?0:$ic-1}]
		if { [lindex $j $idx] ne "" } {
		    set state(justify) [lindex $j $idx]
		} elseif { [llength $j] == 1 } {
		    set state(justify) [lindex $j 0]
		}
	    }
	    if { $irow_page == 0 && $colorT != "" } {
		set fontcolor $state(fontcolor)
		set state(fontcolor) white
	    }

	    set state(bigmarginh1) [expr {$state(bigmarginh1)+$xspc}]
	    set state(bigmarginh2) [expr {$state(bigmarginh2)+$xspc}]

	    if { $width < 200 } { _push_state formula_indent_second_line 0 }
	    #set state(ypos) [expr {$state(ypos)+$yspc}]
	    TextdataToPDF [$entry childNodes] $print
	    if { [IsParagraphActive] } { set state(ypos) [EndParagraph] }
	    #set state(ypos) [expr {$state(ypos)+$yspc}]
	    
	    if { $width < 200 } { _pop_state formula_indent_second_line }

	    set state(bigmarginh1) [expr {$state(bigmarginh1)-$xspc}]
	    set state(bigmarginh2) [expr {$state(bigmarginh2)-$xspc}]

	    if { $print && $irow == [llength $yposlist]-2 && $colorT != "" } {
		PDFWriter::CreateFilledBox $state(bigmarginh1) $ypos_next \
		    $width 2 nw 0 "" $colorT
	    }
	    if { $print && $irow_page == 0 && $rowsepT ne "" && $rowsepT != 0 } {
		if { ![string is boolean $rowsepT] } {
		    set color $rowsepT
		} else {
		    set color black 
		}
		PDFWriter::CreateLine $state(bigmarginh1) $ypos $bigmarginh1_next $ypos 1 $color
	    }
	    if { $print && $ic == 1 && $colsepT ne "" && $colsepT != 0 } {
		if { ![string is boolean $colsepT] } {
		    set color $colsepT
		} else {
		    set color black 
		}
		PDFWriter::CreateLine $state(bigmarginh1) $ypos $state(bigmarginh1) $ypos_next 1 $color
	    }
	    set rowsep $rowsepR
	    if { [$entry @rowsep ""] != "" } { set rowsep [$entry @rowsep] }
	    set colsep $colsepT
	    if { [$entry @colsep ""] != "" } { set colsep [$entry @colsep] }
	    
	    if { $print && $rowsep ne "" && $rowsep != 0 } {
		if { ![string is boolean $rowsep] } {
		    set color $rowsep
		} else {
		    set color black 
		}
		PDFWriter::CreateLine $state(bigmarginh1) $ypos_next $bigmarginh1_next \
		    $ypos_next 1 $color
	    }
	    if { $print && $colsep ne "" && $colsep != 0 } {
		if { ![string is boolean $colsep] } {
		    set color $colsep
		} else {
		    set color black 
		}
		PDFWriter::CreateLine $bigmarginh1_next $ypos $bigmarginh1_next \
		    $ypos_next 1 $color
	    }
	    set state(bigmarginh1) $bigmarginh1_next
	    if { $irow_page == 0 && $colorT != "" } { set state(fontcolor) $fontcolor }
	    incr ic $numrows
	}
	incr irow_page
    }

    foreach i $save_state_items {
	set state($i) $state_save($i)
    }
    set state(ypos) [expr {[lindex $yposlist end]-$yspc}]
    if { $swap_page } {
	set state(papersize) $state(papersize_save)
	set state(ypos) [PrintNewPage]
    }
}

proc xml2pdf::ImagesToPDF { align valign mediaobjectstack print } {
    variable state
    variable svg2draw

    set ic 0
    set totalwidth0 0
    set totalwidth 0
    set maxheight 0
    set pagewidth [expr {$state(xmax)-$state(bigmarginh1)-$state(bigmarginh2)}]
    set pageheight [expr {$state(ymax)-$state(bigmarginv1)-$state(bigmarginv2)}]
    set preserve_ratio 1
    
    set stack ""
    
    foreach "type file caption width" $mediaobjectstack {
	set needs_delete 0
	if { $type == "inlineimage" } {
	    set file [image create photo -data $file]
	    set imgwidth [image width $file]
	    set imgheight [image height $file]
	    set needs_delete 1
	} elseif { $type eq "image" } {
	    if { [lsearch [image names] $file] == -1 } {
		set file [image create photo -file $file]
		set imgwidth [image width $file]
		set imgheight [image height $file]
		set needs_delete 1
	    } else {
		set imgwidth [image width $file]
		set imgheight [image height $file]
	    }
	} else {
	    set imgwidth [$file @width ""]
	    if { $imgwidth eq "" } {
		set imgwidth [expr {0.9*$pagewidth}]
	    }
	    set imgheight [$file @height ""]
	    if { $imgheight eq "" } {
		set v [$file @viewBox]
		set imgheight [expr {$imgwidth*[lindex $v 3]/double([lindex $v 2])}]
	    }
	    set fac 1.0
	    if { [regexp {([\d.]+)%} $imgwidth {} imgwidth_percent] } {
		set imgwidth [expr {$fac*$imgwidth_percent/100.0*$pagewidth}]
	    }
	    if { [regexp {([\d.]+)%} $imgheight {} imgheight_percent] } {
		set imgheight [expr {$fac*$imgheight_percent/100.0*$pagewidth}]
	    }
	    if { [$file @padding ""] ne "" } {
		set imgwidth [expr {$imgwidth+[lindex [$file @padding] 0]+
		        [lindex [$file @padding] 2]}]
		set imgheight [expr {$imgheight+[lindex [$file @padding] 1]+
		        [lindex [$file @padding] 3]}]
	    }
	    set preserve_ratio 0
	}
	if { $width ne "" } {
	    if { [regexp {([\d.]+)%} $width {} width_percent] } {
		set width [expr {$width_percent/100.0*$pagewidth}]
		if { $width > $imgwidth } {
		    set width $imgwidth
		}
	    }
	    if { $type eq "svg" && [$file @padding ""] ne "" } {
		set width [expr {$width+[lindex [$file @padding] 0]+
		        [lindex [$file @padding] 2]}]
	    }
	    if { $preserve_ratio } {
		set height [expr {int($imgheight/double($imgwidth)*$width)}]
	    } else {
		set pageheight [expr {$state(ymax)-$state(bigmarginv1)-$state(bigmarginv2)}]
		set height $imgheight
		if  { $height > $pageheight } {
		    set height [expr {0.9*$pageheight}]
		}
	    }
	} else {
	    set width $imgwidth
	    if { $width > .9*$pagewidth } { set width [expr {.9*$pagewidth}] }
	    set height [expr {$imgheight*1.0*$width/$imgwidth}]
	    if { $height > .35*$pageheight } {
		set height [expr {.35*$pageheight}]
		set width [expr {$imgwidth*1.0*$height/$imgheight}]
	    }
	}
	set totalwidth [expr {$totalwidth+$width}]
	#set totalwidth0 [expr {$totalwidth0+$imgwidth}]
	if { $height > $maxheight } { set maxheight $height }
	lappend stack [list $type $file $caption $width $height $needs_delete]
	incr ic
    }
    if { [IsParagraphActive] && [llength $mediaobjectstack] == 4 && $width <= 32 &&
	$maxheight <= 32 &&  [string trim $caption] eq "" &&
	($type eq "image" || $type eq "inlineimage") } {
	if { $type eq "image" } {
	    if { [lsearch [image names] $file] == -1 } {
		set img [image create photo -file $file]
		set needs_delete 1
	    } else {
		set img $file
		set needs_delete 0
	    }
	} else {
	    set img [image create photo -data $file]
	    set needs_delete 1
	}
	# check when to delete the image
	AddImagesToParagraph $img $width $maxheight $needs_delete
	return
    }
    if { [IsParagraphActive] } { set state(ypos) [EndParagraph] }

    set imagesep 10
    if {$totalwidth <= $pagewidth-($ic-1)*$imagesep } {
	#set fac [expr {$totalwidth/double($totalwidth0)}]
	set xcenter [expr {$state(bigmarginh1)+.5*$pagewidth}]
	switch $align {
	    left {
		set xini $state(bigmarginh1)
	    }
	    right {
		set xini [expr {$state(xmax)-$state(bigmarginh2)-\
		        $totalwidth-($ic-1)*$imagesep}]
	    }
	    default {
		set xini [expr {$xcenter-.5*($totalwidth+($ic-1)*$imagesep)}]
	    }
	}
	set xend [expr {$xini+($totalwidth+($ic-1)*$imagesep)}]
    } else {
	#set fac0 [expr {$totalwidth/double($totalwidth0)}]
	set fac1 [expr {($pagewidth-($ic-1)*$imagesep)/double($totalwidth)}]
	set xini $state(bigmarginh1)
	set xend [expr {$state(xmax)-$state(bigmarginh2)}]
	set idx 0
	foreach i $stack {
	    lassign $i - - - width height -
	    lset stack $idx 3 [expr {$fac1*$width}]
	    lset stack $idx 4 [expr {$fac1*$height}]
	    incr idx
	}
	set maxheight [expr {$fac1*$maxheight}]
    }
    if { [string trim $caption] ne "" } {
	BeginParagraph $xini $xend [expr $state(ypos)-$maxheight] $state(bigmarginv2) \
	    $state(SpaceAfterLineText) $state(interline) "" 0 "" \
	    $state(ImgDocFactor) $state(justify)
	AddTextToParagraph $caption $state(font) [expr $state(size)-2] $state(fontcolor) 0
	set yposnew [EndParagraph]
    } else {
	set yposnew [expr $state(ypos)-$maxheight]
    }
    if { $print } {
	if { $yposnew < $state(bigmarginv2) } {
	    set pagewidth_old $pagewidth
	    #set deltay [expr {$state(ypos)-$yposnew}]
	    set state(ypos) [PrintNewPage]
	    set yposnew [expr {$state(ypos)}]
	    
	    if { [string trim $caption] ne "" } {
		BeginParagraph $xini $xend [expr $yposnew-$maxheight] $state(bigmarginv2) \
		    $state(SpaceAfterLineText) $state(interline) "" 0 "" \
		    $state(ImgDocFactor) $state(justify)
		AddTextToParagraph $caption $state(font) [expr $state(size)-2] $state(fontcolor) 0
		set yposnew [EndParagraph]
	    } else {
		set yposnew [expr $state(ypos)-$maxheight]
	    }
	    set pagewidth [expr {$state(xmax)-$state(bigmarginh1)-$state(bigmarginh2)}]
	    
	    if { $pagewidth != $pagewidth_old } {
		if {$totalwidth <= $pagewidth-($ic-1)*$imagesep } {
		    set xcenter [expr {$state(bigmarginh1)+.5*$pagewidth}]
		    switch $align {
		        left {
		            set xini $state(bigmarginh1)
		        }
		        right {
		            set xini [expr {$state(xmax)-$state(bigmarginh2)-\
		                    $totalwidth-($ic-1)*$imagesep)}]
		        }
		        default {
		            set xini [expr {$xcenter-.5*($totalwidth+($ic-1)*$imagesep)}]
		        }
		    }
		    set xend [expr {$xini+($totalwidth+($ic-1)*$imagesep)}]
		} else {
		    set xini $state(bigmarginh1)
		    set xend [expr {$state(xmax)-$state(bigmarginh2)}]
		}
		set fac1 [expr {$pagewidth/double($pagewidth_old)}]
		set idx 0
		foreach i $stack {
		    lassign $i - - - width height -
		    lset stack $idx 3 [expr {$fac1*$width}]
		    lset stack $idx 4 [expr {$fac1*$height}]
		    incr idx
		}
		set maxheight [expr {$fac1*$maxheight}]
	    }
	}
	set xiniL $xini
	foreach i $stack {
	    lassign $i type img caption width height needs_delete

	    if { $type in "image inlineimage" } {                
		if 0 {
		    # create an image border
		    set c [list [expr {$xiniL-1}] [expr {$state(ypos)+1}] \
		            [expr {$xiniL+$fac*[image width $img]+1}] \
		            [expr {$state(ypos)-$fac*[image height $img]-1}]]
		    set coords [list [lindex $c 0] [lindex $c 1] \
		            [lindex $c 2] [lindex $c 1] \
		            [lindex $c 2] [lindex $c 3] \
		            [lindex $c 0] [lindex $c 3]]
		    PDFWriter::CreatePolygon $coords black 1
		}
		lassign [PDFWriter::InsertImg $img $xiniL $state(ypos) nw h=${width}pt,v=${height}pt] \
		    imgwidth imgheight
		if { $needs_delete } { image delete $img }
	    } elseif { $type eq "svg" } {
		if { ![info exists svg2draw] } {
		    set svg2draw [svg2draw %AUTO%]
		}
		$svg2draw configure -basedir $state(basedir)
		set x $xiniL
		set y $state(ypos)
		if { [$file @padding ""] ne "" } {
		    set x [expr {$x+[lindex [$file @padding] 0]}]
		    set y [expr {$y-[lindex [$file @padding] 1]}]
		}
		$svg2draw draw -svgNode $file -x $x -y $y \
		    -width $width -height $height \
		    -mirror_y 1
	    } else {
		set fac [expr {$width/double([$file @width])}]
		set x [expr {$xiniL-[$file @x_trans]*$fac}]
		set y $state(ypos)
		xml2pdf::_DrawGraphics $file $x $y $fac
	    }
	    set xiniL [expr {$xiniL+$width+$imagesep}]
	}
	set state(ypos) [expr {$state(ypos)-$maxheight}]
	# taken out the newpagescript argument
	if { [string trim $caption] ne "" } {
	    BeginParagraph $xini $xend $state(ypos) $state(bigmarginv2) \
		$state(SpaceAfterLineText) $state(interline) "" $print "" \
		$state(ImgDocFactor) \
		center
	    AddTextToParagraph $caption $state(font) [expr $state(size)-2] \
		$state(fontcolor) 0
	    EndParagraph
	}
    }
    set state(ypos) $yposnew
}

proc xml2pdf::ActivateParagraph { print } {
    variable state

    if { ![IsParagraphActive] } {
	if { [lindex $state(list_type) end] == "orderedlist" } {
	    switch $state(orderedlist_InheritNum) {
		ignore { set lst [lrange $state(list_number) end end] }
		inherit { set lst $state(list_number) }
	    }
	    set txt [EncodeSecName $lst $state(orderedlist_Numeration)]
	    set width0 [PDFWriter::TextWidth $txt $state(font) $state(size)]
	} elseif { [lindex $state(list_type) end] == "itemizedlist" } {
	    set width0 [expr {$state(ImgDocFactor)*[image width dot]}]
	} else {
	    set width0 0
	}
	set lm1 [expr {$state(level)-1}]
	if { $lm1 < 0 } { set lm1 0 }
	if { $print && [lindex $state(listitem) end] == 1 } {
	    set x_indented [expr {$state(bigmarginh1)+$lm1*$state(indent_size)}]
	    if { [lindex $state(list_type) end] == "itemizedlist" } {
		set xposi $x_indented
		set ll [list itemizedlist $xposi "" $x_indented]
	    } elseif { [lindex $state(list_type) end] == "orderedlist" } {
		set xposi [expr {$x_indented+$width0}]
		
		set state(list_number) [lreplace $state(list_number) end end \
		        [expr {[lindex $state(list_number) end]+1}]]
		
		set ll [list orderedlist $xposi $txt $x_indented]
	    } else { set ll "" }
	    set state(listitem) [lreplace $state(listitem) end end -1]
	} else { set ll "" }
	
	set xmin [expr {$state(bigmarginh1)+$lm1*$state(indent_size)}]
	if { $state(list_type) != "" } {
	    set xmin [expr {$xmin+$width0+5}]
	}
	BeginParagraph $xmin [expr {$state(xmax)-$state(bigmarginh2)}] $state(ypos) \
	    $state(bigmarginv2) $state(SpaceAfterLineText) $state(interline) $ll \
	    $print $state(newpagescript) $state(ImgDocFactor) $state(justify)
    }
    
}

proc xml2pdf::PrintError { txt print } {
    set xml "<para><emphasis role='warning'>[map $txt]</emphasis></para>"
    dom parse $xml doc_e
    $doc_e documentElement root_e
    return [TextdataToPDF $root_e $print]
}

proc xml2pdf::_vec_to_len { v len } {
    set l [llength $v]
    if { $len < $l } {
	set v [lreplace $v $len end]
    } elseif { $len > $l } {
	lappend v {*}[lrepeat [expr {$len-$l}] ""]
    }
    return $v
}
proc xml2pdf::TextdataToPDF { domNodes { print 1 } } {
    variable state
    variable indexterm
    variable idpages
    variable scriptdir

    set len [llength $domNodes]
    for { set i 0 } { $i < $len } { incr i } {
	set domNode [lindex $domNodes $i]
	switch [$domNode nodeName] {
	    "beginpage" {
		if { [IsParagraphActive] } { set state(ypos) [EndParagraph] }
		if { [$domNode @part ""] ne "" } {
		    set state(part) [$domNode @part]
		    foreach key [array names state -glob part$state(part),*] {
		        regexp {^part\d+,(.*)$} $key {} n
		        set state($n) $state($key)
		    }
		}
		set state(ypos) [PrintNewPage]
	    }
	    "blockquote" {
		incr state(level)
		TextdataToPDF [$domNode childNodes] $print
		incr state(level) -1
	    }
	    "itemizedlist" - "orderedlist" {
		lappend state(list_type) [$domNode nodeName]
		incr state(level)
		set level [expr {$state(level)+[llength $state(section)]-1}]
		if { ![info exists state(list_number_last)] } {
		    set state(list_number_last) ""
		}
		set state(list_number) [_vec_to_len $state(list_number_last) $level]
		set idx [expr {$level-1}]
		
		if { [$domNode nodeName] == "orderedlist" } {
		    set state(orderedlist_Numeration) [string tolower [$domNode @numeration arabic]]
		    set state(orderedlist_InheritNum) [string tolower [$domNode @inheritnum ignore]]
		    set state(orderedlist_Continuation) [string tolower [$domNode @continuation \
		                restarts]]               
		    switch $state(orderedlist_Continuation) {
		        restarts {
		            lset state(list_number) $idx 1
		        }
		        continues {
		            if { [lindex $state(list_number) $idx] eq "" } {
		                lset state(list_number) $idx 1
		            }
		        }
		    }
		}
		TextdataToPDF [$domNode childNodes] $print
		set state(list_type) [lreplace $state(list_type) end end]
		if { [$domNode nodeName] == "orderedlist" } {
		    set state(list_number_last) $state(list_number)
		    set state(list_number) [lreplace $state(list_number) end end]
		}
		incr state(level) -1
	    }
	    "listitem" {
		if { [IsParagraphActive] } {
		    set state(ypos) [EndParagraph]
		    set state(ypos) [expr {$state(ypos)-$state(SpaceAfterListItem)*\
		            [PDFWriter::LineSpace $state(font) $state(size)]}]
		}
		lappend state(listitem) 1
		TextdataToPDF [$domNode childNodes] $print
		set state(listitem) [lreplace $state(listitem) end end]
	    }
	    "table" {
		if { [IsParagraphActive] } {
		    set state(ypos) [EndParagraph]
		}
		TableToPdf $domNode $print
	    }
	    "para" {
		if { [IsParagraphActive] } {
		    set state(ypos) [EndParagraph]
		    set state(ypos) [expr {$state(ypos)-$state(SpaceAfterPara)*\
		        [PDFWriter::LineSpace $state(font) $state(size)]}]
		}
		TextdataToPDF [$domNode childNodes] $print
		if { [IsParagraphActive] } { set state(ypos) [EndParagraph] }
		set state(ypos) [expr {$state(ypos)-$state(SpaceAfterPara)*\
		    [PDFWriter::LineSpace $state(font) $state(size)]}]
	    }
	    "emphasis" {
		set role [$domNode getAttribute role ""]
		set fontcolor $state(fontcolor)
		set justify $state(justify)
		#set state(size) $state(sizedefault)
		set state_save(size) $state(size)
		switch [$domNode getAttribute role ""] {
		    strong { set type bold }
		    warning { set type bold; set state(fontcolor) red }
		    formula { set type "" }
		    header {
		        set type $state(HeaderType)
		        set state(size) [expr {$state(HeaderSizeFac)*$state(size)}]
		        if { $state(HeaderColor) ne "" } {
		            set state(fontcolor) $state(HeaderColor)
		        }
		    }
		    default { set type italic }
		}
		set old_state_font $state(font)
		foreach "state(font) fac" [PDFWriter::TclfontToPDF $type \
		    $state(fontsize)] break
		set state(size) [expr int($fac*$state(size))]
		if { $type != "" } {
		    TextdataToPDF [$domNode childNodes] $print
		} else {
		    set size $state(size)
		    if { [$domNode @size ""] != "" && [$domNode @size ""] != "normal" } {
		        set size [$domNode @size]
		    }
		    drawformula::Init pdf "" $state(font) $size $fontcolor
		    
		    if { [info exists state(formula_indent_second_line)] } {
		        drawformula::indent_second_line $state(formula_indent_second_line)
		    }
		    
		    set formula ""
		    foreach node [$domNode childNodes] {
		        append formula [$node asXML -indent none]
		    }
		    set formula [string trim $formula]
		    #regsub -all {\n} $formula { } formula
		    if { [$domNode @okfail ""] != "" } {
		        append formula " ([$domNode @okfail])"
		    }

		    set x [expr {$state(bigmarginh1)+$state(indent_size)}]
		    set width [expr {$state(xmax)-$state(bigmarginh2)-$x}]
		    
		    foreach "w hup hdown" [drawformula::drawformula $size $formula 0 \
		                               $x $state(ypos) $width] break
		    
		    if { [IsParagraphActive] } {
		         set h [expr {$hup+$hdown}]
#                         set deltah [expr {-4+.5*($h-$size)}]

		        set deltah [expr {-1.4*$size}]
		        #set relativev [expr {$deltah/double($h)}]
		        set relativev ""
		        set state(ypos) [AddFormulaToParagraph $formula $w [expr {$hup+$hdown}] \
		                $size $relativev $hup $hdown]
		    } else {
		        #if { [IsParagraphActive] } { set state(ypos) [EndParagraph] }
		        set x [expr {$state(bigmarginh1)+$state(indent_size)}]
		        set width [expr {$state(xmax)-$state(bigmarginh2)-$x}]
		        
		        if { $print } {
		            if { $state(ypos)-$hup-$hdown < $state(bigmarginv2) } {
		                set state(ypos) [PrintNewPage]
		                set x [expr {$state(bigmarginh1)+$state(indent_size)}]
		                set width [expr {$state(xmax)-$state(bigmarginh2)-$x}]
		            }
		        }
		        #set state(ypos) [expr {$state(ypos)-$state(size)}]
		        #set state(ypos) [expr {$state(ypos)-$hup}]
		        #set y [expr {$state(ypos)-$hup}]
		        set y $state(ypos)
		        lassign [drawformula::drawformula $size $formula $print \
		                $x $y $width] w hup hdown
		        set state(ypos) [expr {$state(ypos)-$hup-$hdown}]
		        #set state(ypos) [expr {$state(ypos)-$hdown}]
		    }
		}
		set state(font) $old_state_font
		#set state(size) $state(sizedefault)
		set state(size) $state_save(size)
		set state(fontcolor) $fontcolor
		set state(justify) $justify
	    }
	    "superscript" - "sup" {
		set state(size) [expr int(.8*$state(sizedefault))]
		set state(relativev) raise
		TextdataToPDF [$domNode childNodes] $print
		#set state(font) $state(fontdefault)
		set state(size) $state(sizedefault)
		set state(relativev) ""
	    }
	    "subscript" - "sub" {
		set state(size) [expr int(.8*$state(sizedefault))]
		set state(relativev) lower
		TextdataToPDF [$domNode childNodes] $print
		#set state(font) $state(fontdefault)
		set state(size) $state(sizedefault)
		set state(relativev) ""
	    }
	    "mediaobject" {
		set type ""
		set width ""
		set align center
		set valign ""
		foreach imagedataNode [$domNode selectNodes imageobject/imagedata] {
		    if { [$imagedataNode @inlinedata ""] != "" } {
		        set type inlineimage
		        set data [$imagedataNode @inlinedata]
		    } elseif { [$imagedataNode @fileref ""] != "" } {
		        set file [$imagedataNode @fileref]

		        if { [regexp {^\s*data:([^;]+);base64,(.*)} $file {} mtype data] } {
		            unset -nocomplain type
		            switch -- $mtype {
		                image/png { set type png }
		                image/gif { set type gif }
		                image/jpeg { set type jpeg }
		            }
		            if { [info exists type] } {
		                set img [image create photo -format $type -data $data]
		                set data [$img data -format $type]
		                if { [base64::isbase64 $data] } {
		                    set data [base64::decode $data]
		                }
		                set type inlineimage
		            } else {
		                set data no_image
		                set type image
		            }
		        } elseif { [lsearch [image names] $file] == -1 } {
		            set file [file join $state(basedir) [$imagedataNode @fileref ""]]
		            if { ![file exists $file] } {
		                set file [file join $scriptdir templates [$imagedataNode @fileref ""]]
		            }
		            if { ![file exists $file] } {
		                set file no_image
		                #error "file $file does not exist"
		            }
		            set type image
		            set data $file
		        } else {
		            set type image
		            set data $file
		        }
		        set width [$imagedataNode @width ""]
		    }
		    set align [$imagedataNode @align "center"]
		    set valign [$imagedataNode @valign ""]
		}
		foreach graphicdataNode [$domNode selectNodes imageobject/graphicdata] {
		    set type graphics
		    set data $graphicdataNode
		    set width [$graphicdataNode @percentwidth ""]
		    if { $width ne "" && ![string match *% $width] } {
		        error "error: graphicdata percentwidth must be of type: 80%"
		    }
		}
		set ns { svg http://www.w3.org/2000/svg }
		foreach svgNode [$domNode selectNodes -namespaces $ns imageobject/svg:svg] {
		    set type svg
		    set data $svgNode
		    set width [$svgNode @width]
		    set align [$svgNode @align "center"]
		    set valign [$svgNode @valign ""]
		}
		if { $type == "" } {
		    #puts stderr [$domNode asXML]
		    set title [$domNode selectNodes {string(ancestor::section[1]/title)}]
		    PrintError "there is a mediaobject without data. section: '$title'" $print
		}
		
		set captionNode [lindex [$domNode selectNodes caption] 0]
		if { $captionNode != "" } {
		    set caption [[$domNode selectNodes caption] text]
		} else { set caption "" }
		
		lappend state(mediaobjectstack) $type $data $caption $width
		
		if { $i == $len-1 || [[lindex $domNodes [expr {$i+1}]] nodeName] != "mediaobject" } {
		    ImagesToPDF $align $valign $state(mediaobjectstack) $print
		    set state(mediaobjectstack) ""
		}
	    }
	    "indexterm" {
		if { [llength [$domNode childNodes]] != 1 } {
		    #puts stderr "indexterm with [llength [$domNode childNodes]] children"
		}
		set primary [$domNode selectNodes "primary"]
		if { [llength $primary] != 1 } {
		    #puts stderr "indexterm with no primary"
		} else {
		    set indexterm([$primary text]) [lindex $state(sectionpagesstack) end]
		}
	    }
	    "xref" {
		set link [$domNode @linkend ""]
		if { [info exists idpages($link)] } {
		    set txt "see [lindex $idpages($link) 0], pag. [lindex $idpages($link) 2]"
		} else {
		    set txt "see xxx, pag. XXX"
		}
		set textNode [[$domNode ownerDocument] createTextNode $txt]
		TextdataToPDF $textNode $print
		$textNode delete
#                 set state(ypos) [AddTextToParagraph $txt $state(font) $state(size) \
#                         $state(fontcolor) $state(relativev)]
	    }
	    "ulink" {
		if { [$domNode selectNodes mediaobject] ne "" } {
		    TextdataToPDF [$domNode childNodes] $print
		} else {
		    ActivateParagraph $print
		    set txt [string trim [$domNode text]]
		    set link [$domNode @url ""]
		    if { $txt eq "" } { set txt $link }
		    if { [$domNode @linktype ""] in "local local_ext" && [info exists idpages($link)] } {
		        append txt " -pag. [lindex $idpages($link) 2]-"
		    } else {
		        set state(need_second_pass) 1
		    }
		    set txt [string map $state(map) $txt]
		    set x $xml2pdf::paragraphstate(xpos)
		    set ls [PDFWriter::LineSpace $state(font) $state(size)]
		    set y [expr {$xml2pdf::paragraphstate(ypos)-.3*$ls}]
		    set state(ypos) [AddTextToParagraph $txt $state(font) $state(size) \
		            blue $state(relativev)]
		    set xnew $xml2pdf::paragraphstate(xpos)
		    set y1  [expr {$y-1.3*$ls}]
		    set ynew [expr {$state(ypos)-1.3*$ls}]
		    
		    if { $print && [$domNode @linktype ""] in "local local_ext" && [info exists idpages($link)] } {
		        if { $state(ypos) > $y } {
		            PDFWriter::LocalLink $x $y $xnew \
		                $ynew [lindex $idpages($link) 1]
		        } else {
		            PDFWriter::LocalLink $x $y \
		                $xml2pdf::paragraphstate(xend) $y1 \
		                [lindex $idpages($link) 1]
		            PDFWriter::LocalLink \
		                $xml2pdf::paragraphstate(xini) $y1 \
		                $xnew $ynew \
		                [lindex $idpages($link) 1]
		        }
		    }
		}
	    }
	    "\#text" {
		ActivateParagraph $print
		set txt [string map $state(map) [$domNode nodeValue]]
		set state(ypos) [AddTextToParagraph $txt $state(font) $state(size) \
		    $state(fontcolor) $state(relativev)]
	    }
	    "literal" {
		if { [info exists state(font)] } {
		    set prevfont $state(font)
		}
		set state(font) Courier
		TextdataToPDF [$domNode childNodes] $print
		if { [info exists prevfont] } {
		    set state(font) $prevfont
		} else {
		    unset state(font)
		}
	    }
	    "\#comment" {
		#kike
		continue
	    }
	    "user_defined" {
		# nothing
	    }
	    default {
		#catch { puts stderr "lost tag 2: [$domNode nodeName] value: [$domNode text]" }
	    }
	}
    }
    return $state(ypos)
}

proc xml2pdf::_percenttosize { box } {
    variable state

    foreach "x1 y1 x2 y2" $box break

    set x1 [expr {[string trim $x1 %]*$state(xmax)/100.0}]
    set y1 [expr {$state(ymax)-[string trim $y1 %]*$state(ymax)/100.0}]
    set x2 [expr {[string trim $x2 %]*$state(xmax)/100.0}]
    set y2 [expr {$state(ymax)-[string trim $y2 %]*$state(ymax)/100.0}]
    
    return [list $x1 $y1 $x2 $y2]
}

proc xml2pdf::xpath { xpath } {
    variable root
    return [$root selectNodes string($xpath)]
}

proc xml2pdf::xpathfull { xpath } {
    variable root
    return [[$root selectNodes $xpath] asXML]
}

proc xml2pdf::sql { cmd } {
    variable state

    set lognoter_db $state(lognoter_db)
#     if { [info exists state(title_database_table)] } {
#         set l0 [string length {$title_database_table}]
#         set l1 [string length $state(title_database_table)]
#         set i 0
#         while { [set i [string first {$title_database_table} $cmd $i]] != -1 } {
#             set cmd [string replace $cmd $i [expr {$i+$l0-1}] \
#                     $state(title_database_table)]
#             incr i $l1
#         }
#     }
    set err [catch { $lognoter_db sql onecolumn $cmd } ret]
    if { $err } { set ret "ERROR!! executing 'sql $cmd' ($ret)" }
    return $ret
}

proc xml2pdf::sql_field { field } {
    variable state
    
    if { ![info exists state(title_database_table)] } {
	return "ERROR!! executing 'sql_field \"$field\"' (database does not exist)"
    }
    set lognoter_db $state(lognoter_db)
    
    set cmd "select \"$field\" from \"$state(title_database_table)\""
    if { [info exists state(title_database_id)] && $state(title_database_id) ne "" } {
	append cmd " where id=$state(title_database_id)"
    }
    set err [catch { $lognoter_db sql onecolumn $cmd } ret]
    if { $err } { set ret "ERROR!! executing 'sql_field \"$field\"' ($ret)" }
    return $ret
}

proc xml2pdf::sql_image { field } {
    variable state
    
    if { ![info exists state(title_database_table)] } {
	return "ERROR!! executing 'sql_image \"$field\"' (database does not exist)"
    }
    set lognoter_db $state(lognoter_db)
    set cmd "select \"${field}__contents\" from \"$state(title_database_table)\""
    if { [info exists state(title_database_id)] && $state(title_database_id) ne "" } {
	append cmd " where id=$state(title_database_id)"
    }
    set err [catch { cu::inflate [$lognoter_db sql onecolumn $cmd] } ret]
    if { $err } { set ret "ERROR!! executing 'sql_image \"$field\"' ($ret)" }
    return $ret
}

proc xml2pdf::sql_get_state_values {} {
    variable state
    
    if { ![info exists state(title_database_table)] } {
	return
    }
    set lognoter_db $state(lognoter_db)
    set cols [$lognoter_db sql column_names $state(title_database_table)]
    set cmd "select * from \"$state(title_database_table)\""
    if { [info exists state(title_database_id)] && $state(title_database_id) ne "" } {
	append cmd " where id=$state(title_database_id)"
    }
    set values [$lognoter_db sql sel $cmd]
    foreach c $cols v $values {
	if { [info exists state($c)] } {
	    set state($c) $v
	}
    }
}

proc xml2pdf::roman_number {i} {
    if { $i eq "" } { return "" }

    set res ""
    foreach {value roman} {
	1000 M 900 CM 500 D 400 CD 100 C 90 XC 50 L 40 XL 10 X 9 IX 5 V 4 IV 1 I} {
	while {$i>=$value} {
	    append res $roman
	    incr i -$value
	}
    }
    return $res
}

proc xml2pdf::PageBackground {} {
    variable state
    variable paragraphstate
    variable scriptdir
    variable images_cache

    lassign [list 0 0 0] has_odd_even has_firstpage has_secondpage
    foreach i $state(boxList) {
	lassign $i type page part
	if { $part ne "" && $part != $state(part) } { continue }
	switch $page {
	    odd - even { set has_odd_even 1 }
	    firstpage { set has_firstpage 1 }
	    secondpage { set has_secondpage 1 }
	    firstsectionpage { set has_firstsectionpage 1 }
	}
    }
    set nfs [list nofirstpage odd even]

    if { [info exists state(title_database_table)] } {
	set title_database_table $state(title_database_table)

	if { [info exists state(title_database_id)] } {
	    set title_database_id $state(title_database_id)
	} else {
	    set title_database_id "" 
	}
    }
    foreach i $state(boxList) {
	lassign $i type page part box group properties condition
	if { $part ne "" && $part != $state(part) } { continue }
	array set p $properties
	
	if { $condition ne "" } {
	    if !($condition) { continue }
	}
	if { $state(pagetype) eq "firstsectionpage" && $page ne "firstsectionpage" } { continue }
	if { $page eq "firstsectionpage" && $state(pagetype) ne "firstsectionpage" } { continue }

	if { $page eq "lastpage" && $state(pagetype) ne "lastpage" } { continue }
	if { $page ne "lastpage" && $state(pagetype) eq "lastpage" } { continue }

	if { $page eq "firstpage" && $state(pagenum_logical) != 1 } { continue }
	if { $page eq "secondpage" && $state(pagenum_logical) != 2 } { continue }

	if { $state(print_title_pages) && $has_firstpage && \
	    $state(pagenum_logical) == 1 && [lsearch $nfs $page] != -1 } {
	    continue
	}
	if { $state(print_title_pages) && $has_secondpage && \
	    $state(pagenum_logical) == 2 && [lsearch $nfs $page] != -1 } {
	    continue
	}
	if { $page eq "odd" && $state(pagenum)%2 == 0 } { continue }
	if { $page eq "even" && $state(pagenum)%2 != 0 } { continue }

	lassign [_percenttosize $box] x1 y1 x2 y2

	switch $type {
	    "image" {
		lassign [list "" "" 0 0] img file err delete_image
		if { $p(substitutions) == 1 } {
		    set err [catch {
		            set data [subst $p(imagename)]
		            set img [image create photo -data $data]
		            set delete_image 1
		        } errstring]
		} elseif { [regexp {^\s*data:image/(png|gif|jpeg);base64,(.*)} $p(imagename) {} mtype data] } {
		    set err [catch { image create photo -format $mtype -data [cu::base64 decode $data] } img]
		    if { !$err } {
		        set delete_image 1
		    } else {
		        set errstring $img
		    }
		} elseif { [cu::img::exists $p(imagename)] } {
		    set img $p(imagename)
		} else {
		    set file [file normalize [file join $state(basedefdir) $p(imagename)]]
		    if { ![file exists $file] } {
		        set file [file normalize [file join $scriptdir templates $p(imagename)]]
		    }
		    if { [file exists $file] } {
		        if { ![info exists images_cache($file,w_div_h)] } {
		            set err [catch { image create photo -file $file } imgT]
		            if { !$err } {
		                set images_cache($file,w_div_h) \
		                    [expr {[image width $imgT]/double([image height $imgT])}]
		                image delete $imgT
		            } 
		        }
		    } else {
		        set err 1
		        set errstring [_ "File '%s' does not exist" $file]
		    }
		}
		if { $err } {
		    PDFWriter::WriteText $errstring $x1 $y1 Arial 12 w red
		} elseif { $img ne "" } {
		    set w_div_h [expr {[image width $img]/double([image height $img])}]
		    if { $w_div_h < ($x2-$x1)/double($y1-$y2) } {
		        set deltax [expr {$w_div_h*($y1-$y2)}]
		    } else {
		        set deltax [expr {$x2-$x1}]
		    }
		    set x [expr {.5*($x1+$x2)}]
		    PDFWriter::InsertImg $img $x $y1 n h=${deltax}pt
		    if { $delete_image } {
		        image delete $img
		    }
		} else {
		    set x [expr {.5*($x1+$x2)}]
		    if { $images_cache($file,w_div_h) < ($x2-$x1)/double($y1-$y2) } {
		        set deltax [expr {$images_cache($file,w_div_h)*($y1-$y2)}]
		    } else {
		        set deltax [expr {$x2-$x1}]
		    }
		    PDFWriter::InsertImageFile $file $x $y1 n h=${deltax}pt
		}
	    }
	    "colorbox" {
		if { $p(bordercolor) ne "" } {
		    set l $p(outlinewidth)
		    if { $l eq "" || $l == 0 } { set l 1 }
		    foreach i "x1 x2" { set ${i}_b [expr {[set $i]+.5*$l}] }
		    foreach i "y1 y2" { set ${i}_b [expr {[set $i]+.5*$l}] }
		    PDFWriter::CreateFilledBox $x1_b $y1_b \
		    [expr {$x2_b-$x1_b}] [expr {$y1_b-$y2_b}] nw \
		        $l $p(bordercolor) ""
		}
		PDFWriter::CreateFilledBox $x1 $y1 \
		    [expr {$x2-$x1}] [expr {$y1-$y2}] nw $p(outlinewidth) \
		    $p(outlinecolor) $p(fillcolor)
	    }
	    "text" {
		if { $p(anchor) eq "center" } { set p(anchor) "" }
		
		if { $p(substitutions) != 0 } {
		    if { [info exists state(pagetotalnum)] } {
		        set pagetotalnum $state(pagetotalnum)
		    } else {
		        set pagetotalnum ""
		    }
		    switch [lindex $state(pagenum_style) end] {
		        roman {
		            set pagenum [string tolower [roman_number $state(pagenum)]]
		            set pagetotalnum [string tolower [roman_number $pagetotalnum]]
		        }
		        ROMAN {
		            set pagenum [string toupper [roman_number $state(pagenum)]]
		            set pagetotalnum [string toupper [roman_number $pagetotalnum]]
		        }
		        default {
		            set pagenum $state(pagenum)
		        }
		    }
		    set sectiontitle $state(title)
		    if { [info exists state(printing_TOC)] } {
		        set sectionnumber ""
		    } else {
		        set sectionnumber [lrange $state(section) 0 end-1]
		    }
		    if { [info exists title_database_table] } {
		        set cmd "select Title from $title_database_table"
		        if { $title_database_id ne "" } {
		            append cmd " where id=$title_database_id"
		        }
		        set lognoter_db $state(lognoter_db)
		        set err [catch { $lognoter_db sql onecolumn $cmd } documenttitle]
		    } else { set err 1 }
		    if { $err } { set documenttitle [lindex $state(titlestack) 0] }
		    
		    if { [regexp {\$pagetotalnum} $p(text)] } {
		        set state(need_second_pass) 1
		    }
		}
		set err [catch {dom parse $p(text)} doc]
		if { $err && $p(substitutions) } {
		    set err [catch { subst $p(text) } val]
		    if { !$err } {
		        set err [catch {dom parse $val} doc]
		        if { !$err } { set p(substitutions) 0 }
		    }
		}
		if { $p(fontbg) != "white" && $p(fontbg) != "#FFFFFF" } {
		    PDFWriter::CreateFilledBox $x1 $y1 \
		        [expr {$x2-$x1}] [expr {$y1-$y2}] nw 0 "" $p(fontbg)
		}
		if { !$err } {
		    if { $p(substitutions) != 0 } {
		        set root [$doc documentElement]
		        foreach node [$root selectNodes //text()] {
		            catch { subst [$node nodeValue] } val
		            if { [regexp {\[xpathfull} [$node nodeValue]] } {
		                set pnode $node
		                while { [$pnode parentNode] ne $root } {
		                    set pnode [$pnode parentNode]
		                }
		                set err [catch {dom parse $val} doc_into]
		                if { !$err } {
		                    foreach j [[$doc_into documentElement] childNodes] {
		                        $root appendXML [$j asXML]
		                    }
		                    $pnode delete
		                }
		            } elseif { [string trim $val] eq "" } {
		                $node nodeValue $val
		                if { [[$node parentNode] nodeName] eq "para" } {
		                    [$node parentNode] delete
		                }
		            } elseif { [catch { dom parse $val } new_doc] == 0 } {
		                $doc delete
		                set doc $new_doc
		                set root [$doc documentElement]
		                break
		            } else {
		                $node nodeValue $val
		            }
		        }
		    }
		    array set state_save [array get state]
		    array set paragraphstate_save [array get paragraphstate]
		    set paragraphstate(isactive) 0

		    set state(font) $p(font)
		    set state(sizedefault) $p(fontsize)
		    set state(size) $state(sizedefault)
		    set state(fontcolor) $p(fontfg)
		    set state(pageblock) 1

		    if { $p(justify) eq "45 degrees" } {
		        set state(justify) center
		        PDFWriter::RotateCoordSystem [expr {.5*($x1+$x2)}] \
		            [expr {.5*($y1+$y2)}] 45
		    } else {
		        set state(justify) $p(justify)
		    }
		    set state(SpaceAfterPara) 0
		    set state(interline) 1
		    set state(newpagescript) ""
		    set state(level) 0
		    set state(list_type) ""
#                     set state(SpaceAfterLineText) 0

		    set state(bigmarginh1) $x1
		    set state(maxxreal) $state(bigmarginh1)
		    set state(bigmarginh2) [expr {$state(xmax)-$x2}]
		    set state(bigmarginv1) [expr {$state(ymax)-$y1}]
		    set state(bigmarginv2) $y2
		    # factors .5 and .7 are arbitrary
		    set state(ypos) [expr $state(ymax)-$state(bigmarginv1)+.5*\
		            [PDFWriter::LineSpace $p(font) $p(fontsize)]]

		    set print_done 0
		    if { [regexp {w} $p(anchor)] && ![regexp {e} $p(anchor)] && 
		        $p(justify) eq "right" } {
		        set state(justify) left
		        _TextDataToPDFDom [[$doc documentElement] childNodes] 0
		        set print_done 1
		        set owidth [expr {$x2-$x1}]
		        set twidth [expr {$state(maxxreal)-$state(bigmarginh1)}]
		        set state(bigmarginh2) [expr {$state(bigmarginh2)-$owidth+$twidth}]
		        set state(justify) right
		    }
		    if { [regexp {e} $p(anchor)] && ![regexp {w} $p(anchor)] &&
		        $p(justify) eq "left" } {
		        _TextDataToPDFDom [[$doc documentElement] childNodes] 0
		        set print_done 1
		        set owidth [expr {$x2-$x1}]
		        set twidth [expr {$state(maxxreal)-$state(bigmarginh1)}]
		        set state(bigmarginh1) [expr {$state(bigmarginh1)+$owidth-$twidth}]
		    }
		    if { ![regexp {n} $p(anchor)] } {
		        if { !$print_done } { _TextDataToPDFDom [[$doc documentElement] childNodes] 0 }
		        if { [regexp {s} $p(anchor)] } {
		            set state(bigmarginv1) [expr {$state(bigmarginv1)-$y2+$state(ypos)}]
		            set state(ypos) [expr $state(ymax)-$state(bigmarginv1)+.7*\
		                    [PDFWriter::LineSpace $p(font) $p(fontsize)]]
		        } else {
		            set deltay [expr {.5*($y1-$state(ypos))}]
		            set deltaymiddle [expr {.5*($y1-$y2)}]
		            set state(bigmarginv1) [expr {$state(bigmarginv1)-$deltay-$deltaymiddle}]
		            set state(ypos) [expr $state(ymax)-$state(bigmarginv1)+.7*\
		                    [PDFWriter::LineSpace $p(font) $p(fontsize)]]
		        }
		    } else {
#                         set state(bigmarginv1) [expr {$state(ymax)-$y1}]
#                         set state(bigmarginv2) $y2
		        # factors .2 and .7 are arbitrary
		        set state(ypos) [expr $state(ymax)-$state(bigmarginv1)+.2*\
		                [PDFWriter::LineSpace $p(font) $p(fontsize)]]
		    }
		    _TextDataToPDFDom [[$doc documentElement] childNodes]
		    array set state [array get state_save]
		    array set paragraphstate [array get paragraphstate_save]
		    $doc delete
		    if { $p(justify) eq "45 degrees" } {
		        PDFWriter::RestoreCoordSystem
		    } 
		} else {
		    if { $p(substitutions) != 0 } { 
		        catch { subst $p(text) } p(text)
		    }
		    set numlines [llength [split $p(text) \n]]
		    set height [expr {[PDFWriter::LineSpace $p(font) $p(fontsize)]*$numlines}]
		    
		    if { [regexp w $p(anchor)] && ![regexp e $p(anchor)] } {
		        set x $x1
		        set lanchor nw
		    } elseif { ![regexp w $p(anchor)] && [regexp e $p(anchor)] } {
		        set x $x2
		        set lanchor ne
		    } else {
		        set x [expr {.5*($x1+$x2)}]
		        set lanchor n
		    }

		    if { [regexp n $p(anchor)] && ![regexp s $p(anchor)] } {
		        set y $y1
		    } elseif { ![regexp n $p(anchor)] && [regexp s $p(anchor)] } {
		        set y [expr {$y2+$height}]
		    } else { set y [expr {.5*($y1+$y2+$height)}] }

		    foreach i [split $p(text) \n] {
		        set i [string trim $i]
		        PDFWriter::WriteText $i $x $y $p(font) $p(fontsize) $lanchor $p(fontfg)
		        set y [expr {$y-[PDFWriter::LineSpace $p(font) $p(fontsize)]}]
		    }
		}
	    }
	}
    }
}

proc xml2pdf::HasFirstSectionPage {} {
    variable state
    
    foreach i $state(boxList) {
	lassign $i type page part
	if { $part ne "" && $part != $state(part) } { continue }
	if { $type eq "mainbox" && $page eq "firstsectionpage" } {
	    return 1
	}
    }
    return 0
}

proc xml2pdf::_has_page_types { boxList } {
    variable state
    
    lassign [list 0 0 0] has_odd_even has_firstpage has_secondpage
    foreach i $boxList {
	lassign $i type page part box
	if { $part ne "" && $part != $state(part) } { continue }
	switch $page {
	    odd - even { set has_odd_even 1 }
	    firstpage { set has_firstpage 1 }
	    secondpage { set has_secondpage 1 }
	    firstsectionpage { set has_firstsectionpage 1 }
	}
    }
    return [list $has_odd_even $has_firstpage $has_secondpage]
}

proc xml2pdf::_calc_blocks_for_page { pagetype pagenum pagenum_logical boxList } {
    variable state

    if { $pagenum_logical == 0 } { return "" } 

    lassign [list 0 0 0] has_odd_even has_firstpage has_secondpage
    foreach i $boxList {
	lassign $i type page part
	if { $part ne "" && $part != $state(part) } { continue }
	switch $page {
	    odd - even { set has_odd_even 1 }
	    firstpage { set has_firstpage 1 }
	    secondpage { set has_secondpage 1 }
	    firstsectionpage { set has_firstsectionpage 1 }
	}
    }
    set nfs [list nofirstpage odd even]
    set blocks ""
    foreach i $boxList {
	lassign $i type page part box group properties condition
	if { $part ne "" && $part != $state(part) } { continue }

	if { $type ne "mainbox" } { continue }
	if { $pagetype eq "lastsectionpage" } { continue }
	if { $pagetype eq "firstsectionpage" && $page ne "firstsectionpage" } { continue }
	if { $page eq "firstsectionpage" && $pagetype ne "firstsectionpage" } { continue }

	if { $page eq "firstpage" && $pagenum_logical != 1 } { continue }
	if { $page eq "secondpage" && $pagenum_logical != 2 } { continue }

	if { $state(print_title_pages) && $has_firstpage && \
	    $pagenum_logical == 1 && [lsearch $nfs $page] != -1 } {
	    continue
	}
	if { $state(print_title_pages) && $has_secondpage && \
	    $pagenum_logical == 2 && [lsearch $nfs $page] != -1 } {
	    continue
	}
	if { $page eq "odd" && $pagenum%2 == 0 } { continue }
	if { $page eq "even" && $pagenum%2 != 0 } { continue }
	dict set blocks [dict get $properties flownumber] $box
    }
    return $blocks
}

proc xml2pdf::PrintNewPage {} {
    variable state
    variable paragraphstate
    
    if { [info exists state(printingnewpage)] } { return $state(ypos) }
    set state(printingnewpage) 1

    lassign [_has_page_types $state(boxList)] has_odd_even has_firstpage has_secondpage

    if { $state(pageblock) == 0 && $state(pagenum_logical) == 1 } {
	set state(pagenum_logical) 0
	set state(pagenum) 0
    } elseif { $state(pageblock) == 0 && $state(pagenum) == 1 } {
	set state(pagenum) 0
    }
    set save_pagetype $state(pagetype)
    while 1 {
	incr state(pageblock)
	set blocks [_calc_blocks_for_page $state(pagetype) \
		$state(pagenum) $state(pagenum_logical) $state(boxList)]
	if { $state(boxList) eq "" } { break }
	if { ![dict exists $blocks $state(pageblock)] } {
	    set dobreak 0
	    set state(pageblock) 0
	    incr state(pagenum)
	    incr state(pagenum_logical)

	    if { $state(print_title_pages) && $has_firstpage && \
		$state(pagenum_logical) == 1 } {
		set state(pagetype) firstpage
	    } elseif { $state(print_title_pages) && $has_secondpage && \
		$state(pagenum_logical) == 2 } {
		set state(pagetype) secondpage
	    } elseif { $state(pagetype) eq "lastsectionpage" } {
		if { $has_odd_even && $state(pagenum)%2 == 0 } {
		    foreach "state(xmax) state(ymax)" [PDFWriter::NewPage $state(papersize)] break
		    PageBackground
		    incr state(pagenum)
		    incr state(pagenum_logical)
		}
		set state(pagetype) "firstsectionpage"
	    } elseif { $state(pagetype) eq "firstsectionpage" } {
		set state(pagetype) normal
	    } elseif { $state(pagetype) eq "firstpage" || \
		$state(pagetype) eq "secondpage" } {
		set state(pagetype) $save_pagetype
		if { $state(pagetype) eq "lastsectionpage" } {
		    set state(pagetype) "firstsectionpage"
		}
	    } elseif { $state(pagenum_logical) > 10 && ![dict exists $blocks 1] } {
		# only to break infinite loops
		set dobreak 1
	    }
	    if { $state(pagenum_logical) != 1 } {
		lassign [PDFWriter::NewPage $state(papersize)] state(xmax) state(ymax)
	    }
	    PageBackground
	    if { $dobreak } { break }
	} else {
	    if { $state(pagenum_logical) == 0 } {
		incr state(pagenum)
		incr state(pagenum_logical)
		PageBackground
	    }
	    break
	}
    }

    array set state_save [array get state]
    if { [dict exists $blocks $state(pageblock)] } {
	foreach "x1 y1 x2 y2" [_percenttosize [dict get $blocks $state(pageblock)]] break
	
	set state(bigmarginh1) $x1
	set state(bigmarginh2) [expr {$state(xmax)-$x2}]
	set state(indent_size) [expr {($state(xmax)-$state(bigmarginh1)-$state(bigmarginh2))/30.0}]
	set state(bigmarginv1) [expr {$state(ymax)-$y1}]
	set state(bigmarginv2) $y2

	if { ![info exists paragraphstate(xpos)] } {
	    set paragraphstate(xpos) $state(bigmarginh1)
	} else {
	    set paragraphstate(xpos) [expr {$paragraphstate(xpos)+$state(bigmarginh1)-\
		                                $state_save(bigmarginh1)}]
	}
	if { [info exists paragraphstate(list_type)] && $paragraphstate(list_type) ne "" } {
	    lassign $paragraphstate(list_type) type xposi txt x_indented
	    set x_indented [expr {$x_indented+$state(bigmarginh1)-$state_save(bigmarginh1)}]
	    lset paragraphstate(list_type) 3 $x_indented
	}
	if { ![info exists paragraphstate(xini)] } {
	    set paragraphstate(xini) $state(bigmarginh1)
	} else {
	    set paragraphstate(xini) [expr {$paragraphstate(xini)+$state(bigmarginh1)-\
		                                $state_save(bigmarginh1)}]
	}
	if { ![info exists paragraphstate(xend)] } {
	    set paragraphstate(xend) [expr {$state(xmax)-$state(bigmarginh2)}]
	} else {
	    set paragraphstate(xend) [expr {$paragraphstate(xend)-$state(bigmarginh2)+\
		                                $state_save(bigmarginh2)}]
	}
    }
    # leftrightpages is an old system not used now normally
    if { $state(leftrightpages) } {
	set temp $state(bigmarginh1)
	set state(bigmarginh1) $state(bigmarginh2)
	set state(bigmarginh2) $temp
	set paragraphstate(xpos) [expr {$paragraphstate(xpos)+$state(bigmarginh1)-\
		                            $state_save(bigmarginh1)}]
	set paragraphstate(xini) $paragraphstate(xpos)
	set paragraphstate(xend) [expr {$paragraphstate(xend)-$state(bigmarginh2)+\
		                            $state_save(bigmarginh2)}]
    }
    if { [info exists paragraphstate(list_type)] && $paragraphstate(list_type) ne "" } {
	set lm1 [expr {$state(level)-1}]
	if { $lm1 < 0 } { set lm1 0 }

	if { [lindex $state(list_type) end] == "itemizedlist" } {
	    set xposi [expr {$state(bigmarginh1)+$lm1*$state(indent_size)}]
	} else {
	    set txt [lindex $paragraphstate(list_type) 2]
	    set xposi [expr {$state(bigmarginh1)+$lm1*$state(indent_size)+\
		                 [PDFWriter::TextWidth $txt $state(font) $state(size)]}]
	}
	lset paragraphstate(list_type) 1 $xposi
    }                
    unset state(printingnewpage)
    return [expr $state(ymax)-$state(bigmarginv1)]
}

proc xml2pdf::PrintChapterTitle { title { print 1 } } {
    variable state

    if { [llength $state(titlestack)] == 2 && [HasFirstSectionPage] } {
	set state(pagetype) "lastsectionpage"
	set state(ypos) [PrintNewPage]
	return
    }
    if { $state(pageblock)==0 } {
	set state(ypos) [PrintNewPage]
    }

    set size [expr {$state(TitleSizeFac)*$state(size)}]
    
    set state(ypos) [expr {$state(ypos)-$state(SpaceBeforeSection)*\
	    [PDFWriter::LineSpace Helvetica-Bold $size]}]

    set lm1 [expr {$state(level)-1}]
    if { $lm1 < 0 } { set lm1 0 }

    if { $print } {
	set ypos $state(ypos)
	set xmin [expr {$state(bigmarginh1)+$lm1*$state(indent_size)}]
	if { $state(list_type) != "" } {
	    set xmin [expr {$xmin+$state(ImgDocFactor)*[image width dot]+5}]
	}
	BeginParagraph $xmin [expr {$state(xmax)-$state(bigmarginh2)}] $state(ypos) \
	    $state(bigmarginv2) $state(SpaceAfterLineText) $state(interline) "" 0 "" \
	    $state(ImgDocFactor) $state(justify)
	AddTextToParagraph $title Helvetica-Bold $size $state(fontcolor) 0
	set state(ypos) [EndParagraph]
	set state(ypos) [expr {$state(ypos)-$state(SpaceAfterSectionText)*\
		[PDFWriter::LineSpace Helvetica-Bold $size]}]
	if { $state(ypos) < $state(bigmarginv2) } {
	    set state(ypos) [PrintNewPage]
	} else { set state(ypos) $ypos }
    }
    if { [info exists state(TitleBackgroundColor)] } {
	set color $state(TitleBackgroundColor)
    } else { set color "" }
    
    set xmin [expr {$state(bigmarginh1)+$lm1*$state(indent_size)}]
    if { $state(list_type) != "" } {
	set xmin [expr {$xmin+$state(ImgDocFactor)*[image width dot]+5}]
    }
    BeginParagraph $xmin [expr {$state(xmax)-$state(bigmarginh2)}] $state(ypos) \
	$state(bigmarginv2) $state(SpaceAfterLineText) $state(interline) "" $print "" \
	$state(ImgDocFactor) \
	$state(justify) $color

    if { $state(TitleForegroundColor) ne "" } {
	set color $state(TitleForegroundColor)
    } else { set color $state(fontcolor) }
    AddTextToParagraph $title Helvetica-Bold $size $color 0
    set state(ypos) [EndParagraph]
    set state(ypos) [expr {$state(ypos)-$state(SpaceAfterSection)*\
	    [PDFWriter::LineSpace Helvetica-Bold $size]}]
}

proc xml2pdf::_TextDataToPDFDom { domNodes { print 1 } } {
    variable itemslist
    variable state
    variable idpages
    
    foreach domNode $domNodes {
	if { [lsearch -ascii -sorted $itemslist [$domNode nodeName]] != -1 } {
	    if { $state(pageblock)==0 } {
		set state(ypos) [PrintNewPage]
	    }
	    TextdataToPDF $domNode $print
	} elseif { [$domNode nodeName] == "section" || [$domNode nodeName] == "chapter" } {
	    set sect [lindex $state(section) end]
	    if { ![string is integer -strict $sect] } {
		set sect 0
	    }
	    incr sect
	    set state(section) [lreplace $state(section) end end $sect 0]
	    set title [$domNode selectNodes string(title)]
	    set title [lindex [split $title >] end]
	    set state(title) $title
	    lappend state(titlestack) $title
	    lappend state(sectionpagesstack) $state(pagenum)
	    set state(list_number) [lrange $state(section) 0 end-1]
	    set state(list_number_last) $state(list_number)
	    
	    if { $state(printTitleNumbers) } {
		set title "[join [lrange $state(section) 0 end-1] .] "
	    } else { set title "" }
	    append title $state(title)
	    PrintChapterTitle $title $print
	    set id [$domNode @id ""]
	    if { $id != "" } {
		set idpages($id) [list $state(title) $state(pagenum_logical) $state(pagenum)]
	    }
	    set parentbookmark [lindex $state(sectionbookmark) end]
	    if { $print && $state(print_bookmarks) } {
		set bookmark [PDFWriter::AddBookmark $title $parentbookmark]
	    } else {
		set bookmark ""
	    }
	    lappend state(sectionbookmark) $bookmark
	    $domNode setAttribute page $state(pagenum) title $title
	    _TextDataToPDFDom [$domNode childNodes]
	    set state(sectionbookmark) [lreplace $state(sectionbookmark) end end]
	    set state(section) [lreplace $state(section) end end]
	    set state(titlestack) [lreplace $state(titlestack) end end]
	    set state(sectionpagesstack) [lreplace $state(sectionpagesstack) end end]
	} else {
	    if { [$domNode nodeName] == "title" } { continue }
	    #catch { puts stderr "lost tag when printing: [$domNode nodeName]" }
	    #_TextDataToPDFDom [$domNode childNodes]
	}
    }
    if { [IsParagraphActive] } {
	set state(ypos) [EndParagraph]
    }
}


namespace eval xml2pdf {
    # id in symbol font - letter occidental - unicode
    variable greekletters {
	41 A 391     61 a 3b1
	42 B 392     62 b 3b2   
	43 C 3a7     63 c 3c7
	44 D 394     64 d 3b4
	45 E 395     65 e 3b5
	46 F 3a6     66 f 3a6
	47 G 393     67 g 3b3
	48 H 397     68 h 3b7
	49 I 399     69 i 3b9
	4a J -       6a j 3c6 
	4b K 39a     6b k 3ba 
	4c L -       6c l 3bb
	4d M 39c     6d m 3bc
	4e N 39d     6e n 3bd
	4f O 39f     6f o 3bf
	50 P 3a0     70 p 3c0
	51 Q 398     71 q 3b8 
	52 R 3a1     72 r 3c1 
	53 S 3a3     73 s 3c3
	54 T 3a4     74 t 3c4 
	55 U 3a5     75 u 3c5 
	56 V -       76 v 3c9
	57 W 3a9     77 w 3c9
	58 X 39e     78 x 3be 
	59 Y 3a8     79 y 3c8
	5a Z 396     7a z 3b6
	-  ^1 0b9
	-  ^2 0b2
	-  ^3 0b3
	-  ^4 2074
	b0 degrees  00b0
	b1 +- 0b1
	ce pertenece 2208
	d5 productorio 220f
	e5 sum 2211
	d6 raiz 221a
	a5 inf 221e
	f2 int 222b
	b9 diferente 2260
	a3 semejante 2248
	a3 <= 2264
	b3 >= 2265
	-  !<= 226e
	-  !>= 226f
	-  libra 00a3
	a0  euro 20ac
	-  derivative 0307
	-  derivative2 0308
	b7 middledot 0b7
	d1 gradient 2207
	b6 partial_deriv 2202
	bc ... 2026
	cf no_pertenece 2209
	3a vdots 22ee
	3a ddots 22f1
    }
}

proc xml2pdf::xml { args } {
    set map [list > "&gt;" < "&lt;" & "&amp;" \" "&quot;" ' "&apos;"]
    return [string map $map [join $args ""]]
}

proc xml2pdf::xml1 { args } {
    set map [list > "&gt;" < "&lt;" & "&amp;"]
    return [string map $map [join $args ""]]
}

proc xml2pdf::BQ { txt } {
    return "<blockquote>$txt</blockquote>"
}

proc xml2pdf::strong { txt } {
    return "<emphasis role='strong'>$txt</emphasis>"
}

proc xml2pdf::warning { txt } {
    return "<emphasis role='warning'>$txt</emphasis>"
}
proc xml2pdf::header { txt } {
    return "<emphasis role='header'>$txt</emphasis>"
}

proc xml2pdf::para { txt } {
    return "<para>$txt</para>"
}

proc xml2pdf::row { args } {
    set _ "<row"
    if { [lindex $args 0] eq "-cols_justify" } {
	append _ " cols_justify='[lindex $args 1]'"
	set args [lrange $args 2 end]
    }
    append _ ">"
    foreach i $args { append _ "<entry>$i</entry>" }
    append _ "</row>"
    return $_
}

proc xml2pdf::F { args } {

    set ret "<emphasis role='formula'"
    lassign [list "" ""] okfail size
    
    foreach arg [list -okfail -size] {
	set ipos [lsearch $args $arg]
	if { $ipos != -1 } {
	    set val [lindex $args [expr {$ipos+1}]]
	    set args [lreplace $args $ipos [expr {$ipos+1}]]
	    set [string trimleft $arg -] $val
	}
    }
    if { $size ne "" } {
	append ret " size='$size'"
    }
    append ret ">[join $args ""]"
    if { $okfail ne "" } {
	append ret " ($okfail)"
    }
    append ret "</emphasis>"
    return $ret
}

proc xml2pdf::nice { val { precision 3 } } {
    return [format %.${precision}g $val]
}

proc xml2pdf::niceF { val { precision 2 } } {
    return [format %.${precision}f $val]
}

proc xml2pdf::symbol2_hex_unicode { hex } {
    variable greekletters
    variable gList

    if { ![info exists gList] } {
	set gList ""
	foreach "symbol name uni" $greekletters {
	    lappend gList [list $symbol $name $uni]
	}
	set gList [lsort -index 0 $gList]
    }

    set hex [string range $hex end-1 end]
    set ipos [lsearch -index 0 -sorted $gList $hex]
    if { $ipos == -1 } {
	return ""
    } else {
	return [subst "\\u[lindex $gList $ipos 2]"]
    }
}

proc xml2pdf::symbol2unicode { txt } {
    variable greekletters
    variable gList

    if { ![info exists gList] } {
	set gList ""
	foreach "symbol name uni" $greekletters {
	    lappend gList [list $symbol $name $uni]
	}
	set gList [lsort -index 0 $gList]
    }
    set ret ""
    foreach letter [split $txt ""] {
	set symbol [format %x [scan [encoding convertfrom ascii $letter] %c]]
	set ipos [lsearch -index 0 -sorted $gList $symbol]
	if { $ipos == -1 } {
	    append ret $letter
	} elseif { [lindex $gList $ipos 2] eq "-" } {
	    append ret "?"
	} else {
	    append ret [subst "\\u[lindex $gList $ipos 2]"]
	}
    }
    return $ret
}

proc xml2pdf::sym { txt } {
    variable greekletters

    set ipos [lsearch $greekletters $txt]
    if { $ipos != -1 } {
	incr ipos
	return [subst "\\u[lindex $greekletters $ipos]"]
    }

    set ret ""
    foreach i [split $txt ""] {
	if { $i == "'" } {
	    append ret "&apos;"
	    continue
	}
	set ipos [lsearch $greekletters $i]
	incr ipos
	append ret [subst "\\u[lindex $greekletters $ipos]"]
    }
    return $ret
}

proc xml2pdf::sym_tag { txt } {
    variable greekletters

    set len [string length $txt]
    set text ""
    for { set i 0 } { $i < $len } { incr i } {
	set c [string index $txt $i]
	set cidx [scan $c %c]
	if { $cidx >= 0x391 } {
	    set ipos [lsearch $greekletters [format %x $cidx]]
	    if { $ipos != -1 && [lindex $greekletters [expr {$ipos-2}]] != "-" } {
		set csymbol [lindex $greekletters [expr {$ipos-2}]]
	    } else { set csymbol "" }
	}
	if { $cidx >= 0x391 &&  $csymbol != "" } {
	    append text "<sym>[format %c 0x$csymbol]</sym>"
	} else { append text $c }
    }
    return $text
}

proc xml2pdf::inlineimage { imgdata { width "" } } {
    set xml "<mediaobject><imageobject><imagedata inlinedata='$imgdata' width='$width'/>"
    append xml "</imageobject></mediaobject>"
    return $xml
}
proc xml2pdf::fileimage { file { width "" } } {

    set xml ""
    append xml "<mediaobject><imageobject><imagedata fileref='$file' width='$width'/>"\
	"</imageobject></mediaobject>"
    return $xml
}

proc xml2pdf::filegraph { file { percentwidth "" } { imagefile "" } { imagewidth "" } } {

    set xml [tDOM::xmlReadFile $file]
    dom parse $xml doc
    set mediaNode [[$doc documentElement] selectNodes mediaobject]
    set imageNode [$mediaNode selectNodes imageobject]
    set graphNode [$imageNode selectNodes graphicdata]
    $graphNode setAttribute percentwidth $percentwidth

    if { $imagefile ne "" } {
	$imageNode appendXML "<imagedata fileref='$imagefile' width='$imagewidth'/>"

    }
    set xml [$mediaNode asXML]
    $doc delete
    return $xml
}

proc xml2pdf::euro {} {
    return "\u20ac"
}

proc xml2pdf::images_to_inline_images { doc default_image_width images_dir } {

    set root [$doc documentElement]
    $doc selectNodesNamespaces {svg http://www.w3.org/2000/svg}
    set xp "//mediaobject/imageobject/imagedata|//svg:image"
    
    foreach i [$root selectNodes $xp] {
	if { [$i nodeName] eq "imagedata" } {
	    set attribute fileref
	} else {
	    set attribute xlink:href
	}
	set data ""
	set file [$i @$attribute]
	set err [catch { open [file join $images_dir $file] r } fin]
	if { !$err } {
	    switch -- [string tolower [file extension $file]] {
		.png { set mtype image/png }
		.gif { set mtype image/gif }
		.jpg - .jpeg { set mtype image/jpeg }
	    }
	    fconfigure $fin -translation binary
	    set data [read $fin]
	    close $fin
	    set err [catch { image create photo -data $data } img]
	    if { !$err } {
		set width0 [image width $img]
		set height0 [image height $img]
		image delete $img
	    } else { set data "" }
	}
	if { $data ne "" } {
	    if { [$i @width ""] ne "" } {
		if { [regexp {(\d+)%} [$i @width] {} percent] } {
		    set width [expr {int($percent*$default_image_width/100)}]
		} else {
		    set width [$i @width]
		    if { $width > $default_image_width } {
		        set width $default_image_width
		    }
		}
	    } else {
		if { $width0 > $default_image_width } {
		    set width $default_image_width
		} else {
		    set width $width0
		}
	    }
	    set height [expr {$width*$height0/$width0}]
	    set data "data:$mtype;base64,[base64::encode -maxlen 60 $data]"
	    $i setAttribute width_p $width
	    $i setAttribute height_p $height
	    $i setAttribute localname [$i @$attribute]
	    $i setAttribute $attribute $data
	}
    }
}

proc xml2pdf::PrintIndex {} {
    variable indexterm
    variable state

    if { [llength [array names indexterm]] == 0 } { return }
    set xml "<chapter><title>INDEX</title>\n"
    set lastletter ""
    foreach i [lsort -dictionary [array names indexterm]] {
	if { $lastletter ne [string toupper [string index $i 0]] } {
	    set lastletter [string toupper [string index $i 0]]
	    append xml "<para>[strong [string map $state(map) $lastletter]]</para>\n"
	}
	append xml <para>[string map $state(map) "$i $indexterm($i)"]</para>\n
    }
    append xml "</chapter>"


    if { $state(ypos) < $state(ymax)-1.05*$state(bigmarginv1) } {
	set state(ypos) [PrintNewPage]
    }
    set state_save(boxList) $state(boxList)
    set state(boxList) ""
    set pages ""
    foreach i $state_save(boxList) {
	lassign $i type page part box group properties condition
	if { $part ne "" && $part != $state_save(part) } { continue }
	if { $type ne "mainbox" } {
	    lappend state(boxList) $i
	} else {
	    lappend pages $page
	    if { ![info exists maxblock($page)] } { set  maxblock($page) 0 }
	    set f [dict get $properties flownumber]
	    if { $f > $maxblock($page) } { set maxblock($page) $f }
	}
    }
    foreach i $state_save(boxList) {
	lassign $i type page part box group properties condition
	if { $part ne "" && $part != $state_save(part) } { continue }
	if { $type ne "mainbox" } { continue }
	if { [info exists maxblock($page)] && $maxblock($page) > 1 } {
	    lappend state(boxList) $i
	    continue
	}
	lassign $box x1 y1 x2 y2
	
	set y1_num [string trim $y1 %]
	set y2_num [string trim $y2 %]
	set y2A [expr {$y1_num+2}]%
	set x1_num [string trim $x1 %]
	set x2_num [string trim $x2 %]
	set xmid [expr {$x1_num+.5*($x2_num-$x1_num)}]%
	
	set box0 [list $x1 $y1 $x2 $y2A]
	set box1 [list $x1 $y2A $xmid $y2]
	set box2 [list $xmid $y2A $x2 $y2]
	
#             if { $page eq "even" || $page eq "" } {
#                 lappend state(boxList) [list text firstsectionpage $box0 Helvetica 14 \
#                     black "" nw {$sectiontitle} 1]
#                 lappend state(boxList) [list $type firstsectionpage $box1 1]
#                 lappend state(boxList) [list $type firstsectionpage $box2 2]
#             }
	set box1 [list $x1 $y1 $xmid $y2]
	lappend state(boxList) [list $type $page $part $box1 "" "flownumber 1" ""]
	set box2 [list $xmid $y1 $x2 $y2]
	lappend state(boxList) [list $type $page $part $box2 "" "flownumber 2" ""]
    }
    set domDoc [dom parse $xml]
    _TextDataToPDFDom [$domDoc documentElement]
    $domDoc delete
    set state(boxList) $state_save(boxList)
}

proc xml2pdf::PrintTOC { parentNode {level 0 } } {
    variable state

    if { $level == 0 } {
	set xml "<chapter><title>[_ {Table of Contents}]</title>\n"
	append xml "<table relativesizes='0 5 5 5 5 5 65 10' tabstyle='plain' "\
	    "cols_justify='left left left left left left right'>"
	append xml "<row cols_justify='center center center center center center right'>"\
	    "<entry morerows='5'>[_ {Chapters}]</entry>"\
	    "<entry>[_ {Pag.}]</entry></row>"
    } else { set xml "" }
    foreach domNode [$parentNode childNodes] {
	if { [$domNode nodeName] in "section chapter" } {
	    set title [lindex [split [$domNode @title ""] >] end]
	    append xml "<row>[string repeat <entry/> $level]"\
		"<entry morerows='[expr {5-$level}]'>[xml_map $title]</entry>"\
		"<entry>[xml_map [$domNode @page ""]]</entry></row>"
	    set lm1 [expr {($level<5)?$level+1:5}]
	    append xml [PrintTOC $domNode $lm1]
	}
    }
    if { $level == 0 } {
#         if { [info exists state(ypos)] && $state(ypos) < $state(ymax)-1.05*$state(bigmarginv1) } {
#             set state(ypos) [PrintNewPage]
#         }
	set state(printing_TOC) 1
	lappend state(pagenum_style) roman
	append xml "</table></chapter>"
	set domDoc [dom parse $xml]
	_TextDataToPDFDom [$domDoc documentElement]
	$domDoc delete
	if { $state(pagenum)%2==1 } {
	    PrintNewPage
	}
	set state(pagenum_style) [lrange $state(pagenum_style) 0 end-1]
	set state(pageblock) 0

	set state(pagenum) 1
	set state(section) [lreplace $state(section) end end 0]
	unset state(printing_TOC)
    } else { return $xml }
}

proc xml2pdf::Print { T } {
    variable domNodes
    variable state
    variable indexterm

    UpdateDisplayedNode $T ""
    array unset state
    array unset indexterm

    set document $domNodes(document)
    set root [$document documentElement]

    switch [$root nodeName] {
	letter { PrintLetter }
	default { PrintGeneric }
    }
}

proc xml2pdf::PrintGeneric {} { 
    variable domNodes
    variable state
    variable indexterm

    set paper [list userdefined 17 21 2 2]
    set state(DrawHeaderFooter) DrawHeaderFooterGiD
    set state(fontdefault) Helvetica
    set state(font) Helvetica
    set state(sizedefault) 8
    set state(fontsize) 9
    set state(xmax) [PDFWriter::cm2pt [lindex $paper 1]]
    set state(ymax) [PDFWriter::cm2pt [lindex $paper 2]]
    set state(ysep) 5
    set state(bigmarginh1) [PDFWriter::cm2pt [expr {2*1.5}]]
    set state(bigmarginh2) [PDFWriter::cm2pt 2]
    set state(bigmarginv1) [PDFWriter::cm2pt 2]
    set state(bigmarginv2) [PDFWriter::cm2pt [expr {2*1.5}]]
    set state(leftrightpages) 1

    array set state {
	SpaceBeforeSection 0.8
	SpaceAfterSectionText 3.5
	SpaceAfterSection 0.3
	SpaceAfterPara 0.4
	SpaceAfterLineText 0.5
	ImgDocFactor 0.3
    }

    array set state [list size $state(sizedefault)]
    set state(map) [list "&gt;" > "&lt;" < "&amp;" & "&quot;" \" "&apos;" ' \u201c \"]

    array set state {
	level 0
	relativev ""
	mediaobjectstack ""
	section 0
	sectionbookmark 0
	list_type ""
	listitem 0
	pagenum 1
	title ""
    }
    
    PDFWriter::Init $paper
    set state(ypos) [$state(DrawHeaderFooter) state]
    set document $domNodes(document)
    set root [$document documentElement]
    _TextDataToPDFDom [$root childNodes]

    PrintIndex
    PDFWriter::End

    #set filename [PDFWriter::End $outputPDF]
}

proc date_sql2Nice { date lan } {

    switch -glob $lan {
	es* {
	    set m [list enero febrero marzo abril mayo junio julio agosto setiembre \
		       octubre noviembre diciembre]
	}
	cat* {
	    set m [list gener febrer març abril maig juny juliol agost septembre \
		       octubre novembre desembre]
	}
	en* {
	    set m [list january february march april may june july august \
		       september october november december]
	}
    }
    if { [regexp {^[-: 0.]*$} $date] } { return "" }
    set day [scan [clock format [clock scan $date] -format "%d"] %d]
    set month [scan [clock format [clock scan $date] -format "%m"] %d]
    set year [scan [clock format [clock scan $date] -format "%Y"] %d]
    
    set monthL [lindex $m [expr $month-1]]
    switch -glob $lan {
	es* {
	    return "$day de $monthL de $year"
	}
	cat* {
	    if {[regexp {^[aeiouAEIOU]} $monthL] } {
		 return "$day d'$monthL de $year"
	    } else {
		 return "$day de $monthL de $year"
	    }
	}
	en* {
	    switch $day {
		1 { return "$monthL, 1st $year" }
		2 { return "$monthL, 2nd $year" }
		default { return "[string totitle $monthL], ${day}th $year" }
	    }
	}
    }
}


proc xml2pdf::PrintLetter {} { 
    variable domNodes
    variable state
    variable indexterm

    set paper [list A4]
    set state(DrawHeaderFooter) LettersDrawHeaderFooter
    set state(font) Helvetica
    set state(sizedefault) 12
    set state(fontsize) 9
    set state(xmax) [PDFWriter::cm2pt 21]
    set state(ymax) [PDFWriter::cm2pt 29.7]
    set state(ysep) 5
    set state(bigmarginh1) [PDFWriter::cm2pt 1.5]
    set state(bigmarginh2) $state(bigmarginh1)
    set state(bigmarginv1) [PDFWriter::cm2pt 2]
    set state(bigmarginv2) $state(bigmarginv1)
    set state(leftrightpages) 0

    array set state {
	SpaceBeforeSection 0.8
	SpaceAfterSectionText 3.5
	SpaceAfterSection 0.3
	SpaceAfterPara 0.4
	SpaceAfterLineText 0.5
	ImgDocFactor 0.3
    }

    array set state [list size $state(sizedefault)]
    set state(map) [list "&gt;" > "&lt;" < "&amp;" & "&quot;" \" "&apos;" ' \u201c \"]

    array set state {
	level 0
	relativev ""
	mediaobjectstack ""
	section 0
	sectionbookmark 0
	list_type ""
	listitem 0
	pagenum 1
	title ""
    }
    
    PDFWriter::Init $paper
    set state(ypos) [$state(DrawHeaderFooter) state 1]
    set document $domNodes(document)
    set root [$document documentElement]

    set lines ""
    #foreach i "//date/day //date/month //date/year"

    set date [$root SelectNodesText //date/year]-
    append date [$root SelectNodesText //date/month]-
    append date [$root SelectNodesText //date/day]
    set date [date_sql2Nice $date es]

    foreach i "name address {zip_code city} country" {
	set line ""
	foreach j $i {
	    set node [$root selectNodes "//recipient/$j"]
	    if { $node != "" } {
		if { $line != "" } { append line " " }
		append line [$node text]
	    }
	}
	lappend lines $line
    }
    
    set sps [list [PDFWriter::cm2pt 1.5] [PDFWriter::cm2pt 0.5] [PDFWriter::cm2pt 0.5] \
	    [PDFWriter::cm2pt 0.2] [PDFWriter::cm2pt 1.5]]
    
    for { set print 0 } { $print <= 1 } { incr print } {
	set state(ypos) [expr {$state(ymax)-$state(bigmarginv1)-[lindex $sps 0]}]
	set state(ypos) [PrintLines state [list "Barcelona, $date"] $state(bigmarginh1) $state(xmax) $print]
	set state(ypos) [expr {$state(ypos)-[lindex $sps 1]}]
	
	set state(ypos) [PrintLines state $lines $state(bigmarginh1) $state(xmax) $print]
	set state(ypos) [expr {$state(ypos)-[lindex $sps 2]}]
	
	set state(ypos) [PrintLines state [list [[$root selectNodes "//salutation/salutation_text"] text]]\
		$state(bigmarginh1) $state(xmax) $print]
	set state(ypos) [expr {$state(ypos)-[lindex $sps 3]}]
	
	_TextDataToPDFDom [$root childNodes] $print
	set state(ypos) [expr {$state(ypos)-[lindex $sps 4]}]
	set state(ypos) [PrintLines state [list [[$root selectNodes "//salutation/signer"] text]]\
		$state(bigmarginh1) $state(xmax) $print]
	set state(ypos) [PrintLines state [list [[$root selectNodes "//salutation/signer_position"] text]]\
		$state(bigmarginh1) $state(xmax) $print]

	set rest [expr {$state(ypos)-1.3*$state(bigmarginv2)}]
	if { $rest > 0 } {
	    lset sps 2 [expr {[lindex $sps 2]+.3*$rest}]
	    lset sps 4 [expr {[lindex $sps 4]+.1*$rest}]
	}
    }
    PDFWriter::End

    #set filename [PDFWriter::End $outputPDF]
}

proc xml2pdf::PrintLines { statename lines x xmax { print 1 } } {
    upvar $statename state

    foreach i $lines {
	if { $print } { 
	    PDFWriter::WriteText $i $x $state(ypos) $state(font) $state(size) nw
	}
	set state(ypos) [expr $state(ypos)-[PDFWriter::LineSpace $state(font) $state(size)]]
    }
    return $state(ypos)
}

proc xml2pdf::LettersDrawHeaderFooter { statename { firstpage 0 } } {
    upvar $statename state
    
    foreach i "xmax ymax bigmarginh1 bigmarginv1 bigmarginh2 bigmarginv2 ysep" { set $i $state($i) }

    set compass [split {
	COMPASS Ingeniería y Sistemas S.A.
	c/ Tuset, 8 7-2
	08006 Barcelona
	http://www.compassis.com
    } \n]


    set xpos [expr $xmax-$bigmarginh2]
    set ypos [expr $ymax-$bigmarginv1]

    if { $firstpage } {
	foreach "width height" [PDFWriter::InsertImageFile \
		{C:\Documents and Settings\ramsan\Mis documentos\myTclTk\compasser\images\compass_logo300-4cm.png} \
		$xpos $ypos ne h=4cm] break
	
	set ypos [expr $ypos-$height-$ysep/2]
	set xpos [expr $xmax-$bigmarginh2-$width/2]
	
	foreach i $compass {
		    PDFWriter::WriteTextBox $i $xpos $ypos 0 center Helvetica 8
		    set ypos [expr $ypos-[PDFWriter::LineSpace Helvetica 8]]
	}
    }
    set yposheader [expr {$ypos-$ysep}]
    set lan en

    set compass {
	{COMPASS Ingeniería y Sistemas S.A.}
	{c/ Tuset, 8 7-2. 08006 Barcelona}
	{Tel: +34 93 218 19 89 Fax: +34 93 396 97 46 Email: info@compassis.com}
	{Sociedad inscrita en el R.M. de Barcelona, T.33954, fol.128, hoja B236843,\
	    inscrip. 1. CIF. A-62485180}
    }
    if { $lan == "en" } {
	set compass [lreplace $compass 1 1 {c/ Tuset, 8 7-2. 08006 Barcelona (Spain)}]
    }
    set totalw 2*$ysep
    set ic 0
    foreach i $compass {
	if { $ic > 0 } { set font Helvetica } else { set font Helvetica-Bold }
	set totalw [expr $totalw+[PDFWriter::TextWidth $i $font 6]]
	if { $ic == 2 } { break }
	incr ic
    }
    set xend [expr $xmax-$bigmarginh2]
    set xpos [expr ($bigmarginh1+$xend)/2-$totalw/2]
    set ypos [expr $bigmarginv1]

    set yposL [expr $ypos+[PDFWriter::LineSpace $font 6]]
    set x1 [expr ($bigmarginh1+$xend)/2-$totalw/2-2*$ysep]
    set x2 [expr ($bigmarginh1+$xend)/2+$totalw/2+2*$ysep]
    PDFWriter::CreateLine $x1 $yposL $x2 $yposL 1 #8ba3cf

    set ic 0
    foreach i $compass {
	if { $ic > 0 } { set font Helvetica } else { set font Helvetica-Bold }
	PDFWriter::WriteText $i $xpos $ypos $font 6 sw
	set xpos [expr $xpos+[PDFWriter::TextWidth $i $font 6]+$ysep]

	if { $ic == 0 } { set xpos0 $xpos }
	if { $ic == 2 } {
	    set xpos $xpos0
	    set ypos [expr $ypos-[PDFWriter::LineSpace $font 6]]
	}
	incr ic
    }
    return [expr $yposheader-$ysep]
}

proc xml2pdf::DrawHeaderFooter { statename } {
    upvar $statename state

    foreach i "xmax ymax bigmarginh1 bigmarginv1 bigmarginh2 bigmarginv2 ysep" { set $i $state($i) }

    set xpos [expr $xmax-$bigmarginh2]
    set ypos [expr $ymax-$bigmarginv1]
    foreach "width height" [PDFWriter::InsertImageFile \
	{C:\Documents and Settings\ramsan\Mis documentos\myTclTk\compasser\images\compass_logo300-4cm.png} $xpos $ypos ne h=4cm] break

    set ypos [expr $ypos-$height-$ysep]
    PDFWriter::CreateLine $bigmarginh1 $ypos [expr $xmax-$bigmarginh2] $ypos 2 #8ba3cf

    set yposheader $ypos
    set lan en

    set compass {
	{COMPASS Ingeniería y Sistemas S.A.}
	{c/ Tuset, 8 7-2. 08006 Barcelona}
	{Tel: +34 93 218 19 89 Fax: +34 93 396 97 46 Email: info@compassis.com}
    }
    if { $lan == "en" } {
	set compass [lreplace $compass 1 1 {c/ Tuset, 8 7-2. 08006 Barcelona (Spain)}]
    }
    set totalw 2*$ysep
    set ic 0
    foreach i $compass {
	if { $ic > 0 } { set font Helvetica } else { set font Helvetica-Bold }
	set totalw [expr $totalw+[PDFWriter::TextWidth $i $font 6]]
	incr ic
    }
    set xend [expr $xmax-$bigmarginh2]
    set xpos [expr ($bigmarginh1+$xend)/2-$totalw/2]
    set ypos [expr $bigmarginv2-[PDFWriter::LineSpace $font 6]]
    set ic 0
    foreach i $compass {
	if { $ic > 0 } { set font Helvetica } else { set font Helvetica-Bold }
	PDFWriter::WriteText $i $xpos $ypos $font 6 sw
	set xpos [expr $xpos+[PDFWriter::TextWidth $i $font 6]+$ysep]
	incr ic
    }
    set ypos $bigmarginv2
    set x1 [expr ($bigmarginh1+$xend)/2-$totalw/2-2*$ysep]
    set x2 [expr ($bigmarginh1+$xend)/2+$totalw/2+2*$ysep]
    PDFWriter::CreateLine $x1 $ypos $x2 $ypos 1 #8ba3cf

    return [expr $yposheader-$ysep]
}

proc xml2pdf::DrawHeaderFooterGiD { statename } {
    upvar $statename state

    set maintitle "GiD REFERENCE MANUAL"
    foreach i "xmax ymax bigmarginh1 bigmarginv1 bigmarginh2 bigmarginv2 ysep" { set $i $state($i) }

    set ypos [expr $ymax-$bigmarginv1]
    if { !($state(pagenum)%2) } {
	PDFWriter::WriteText $state(pagenum) $bigmarginh1 $ypos $state(font) \
	    $state(sizedefault) sw
	PDFWriter::WriteText $maintitle [expr $xmax-$bigmarginh2] $ypos \
	    $state(font) $state(sizedefault) se        
    } else {
	PDFWriter::WriteText $state(title) $bigmarginh1 $ypos $state(font) \
	    $state(sizedefault) sw
	PDFWriter::WriteText $state(pagenum) [expr $xmax-$bigmarginh2] $ypos \
	    $state(font) $state(sizedefault) se      
    }
    set ypos [expr $ypos-$ysep]
    PDFWriter::CreateLine $bigmarginh1 $ypos [expr $xmax-$bigmarginh2] $ypos 2 #8ba3cf
    return [expr $ypos-$ysep]
}

proc xml2pdf:::_TransformBox { widthF heightF widthT heightT box } {

    set x [expr {[lindex $box 0]*$widthT/double($widthF)}]
    set w [expr {[lindex $box 2]*$widthT/double($widthF)}]
    set y [expr {$heightT-[lindex $box 1]*$heightT/double($heightF)}]
    set h [expr {[lindex $box 3]*$heightT/double($heightF)}]
    return [list $x $y $w $h]
}

proc xml2pdf::addTemplatesDir { dir } {
    variable templatesdirList

    set dir [file normalize $dir]
    if { [set ipos [lsearch -exact $templatesdirList $dir]] != -1 } {
	set templatesdirList [lreplace $templatesdirList $ipos $ipos]
    }
    set templatesdirList [linsert $templatesdirList 0 $dir]
}

proc xml2pdf::GiveTemplateDirList {} {
    variable templatesdirList
    return $templatesdirList
}

proc xml2pdf::addWordTemplatesDir { dir } {
    variable WordtemplatesdirList

    set dir [file normalize $dir]
    if { [set ipos [lsearch -exact $WordtemplatesdirList $dir]] != -1 } {
	set WordtemplatesdirList [lreplace $WordtemplatesdirList $ipos $ipos]
    }
    set WordtemplatesdirList [linsert $WordtemplatesdirList 0 $dir]
}

proc xml2pdf::GiveTemplates {} {
    variable templatesdirList

    set rets ""
    foreach dir $templatesdirList {
	eval lappend rets [glob -tails -nocomplain -directory $dir -types d *]
	set ipos [lsearch $rets CVS]
	if { $ipos != -1 } {
	    set rets [lreplace $rets $ipos $ipos]
	}
    }
    return $rets
}

proc xml2pdf::GiveTemplatesDict {} {
    variable templatesdirList

    set dict ""
    foreach dir $templatesdirList {
	foreach d [glob -nocomplain -directory $dir -types d *] {
	    set err [catch { tDOM::xmlReadFile [file join $d [file tail $d].xml] } xml]
	    if { !$err } {
		set doc [dom parse $xml]
		set name [$doc selectNodes string(/*/title)]
		if { $name eq "" } {
		    set name [string totitle [file tail $d]]
		    regsub -all {_}  $name { } name
		}
		set template [file tail $d]
		dict set dict $template type directory
		dict set dict $template name0 $name
		dict set dict $template name $name
		dict set dict $template template $template
		$doc delete
	    }
	}
    }
    return $dict
}

proc xml2pdf::GiveTemplateDir { template } {
    variable templatesdirList

    foreach dir $templatesdirList {
	set deffile [file join $dir $template $template.xml]
	if { [file readable $deffile] } {
	    return [file join $dir $template]
	}
	set deffile [file join $dir $template $template.tcl]
	if { [file readable $deffile] } {
	    return [file join $dir $template]
	}
    }
    return ""
}

proc xml2pdf::save_xml_file { xml opts } {
    variable templatesdirList
    variable scriptdir
    variable state
    
    foreach "name value" $opts { set state($name) $value }

    if { [dict_getd $opts filename ""] ne "" } {
	set filename [dict get $opts filename]
    } else {
	set types [list [list [_ "XML files"] ".xml"] [list [_ "All files"] "*"]]
	set filename [tk_getSaveFile -filetypes $types -title \
		[_ "Export XML file"] -defaultextension .xml]
	if { $filename eq "" } { return }
    }
    dom parse $xml doc
    $doc documentElement root
    
    if { [dict  exists $opts template_doc] } {
	$root appendChild [[[dict get $opts template_doc] documentElement] cloneNode -deep]
    } elseif { [$root selectNodes {lognoter_pdf_template}] eq "" } {
	if { [dict exists $opts template] } {
	    set template [dict get $opts template]
	} else {
	    set template [$root nodeName]
	}
	set deffile ""
	foreach dir $templatesdirList {
	    set deffile [file join $dir $template $template.xml]
	    if { [file exists $deffile] } { break }
	}
	set state(basedefdir) [file dirname $deffile]
	if { [string equal -nocase [file extension $deffile] ".xml"] } {
	    dom parse [tDOM::xmlReadFile $deffile] boxXMLdoc
	    $root appendChild [[$boxXMLdoc documentElement] cloneNode -deep]
	}
    }
    foreach boxNode [$root selectNodes {lognoter_pdf_template/boxList/box[@boxtype='image']}] {
	set imagename [$boxNode selectNodes {string(p[@n='imagename']/@v)}]
	if {  [$boxNode selectNodes {string(p[@n='substitutions']/@v)}] == 1 } {
	    set err [catch { subst imagename } imagename]
	    if { $err } { continue }
	}
	if { [regexp {^\s*data:image/(png|gif|jpeg);base64,(.*)} $imagename {} mtype data] } {
	    continue
	}
	set delete_image 0
	if { ![cu::img::exists $imagename] } {
	    set file [file normalize [file join $state(basedefdir) $imagename]]
	    if { ![file exists $file] } {
		set file [file normalize [file join $scriptdir templates $imagename]]
	    }
	    if { [file exists $file] } {
		set err [catch { image create photo -file $file } imagename]
		if { $err } {
		    continue
		}
		set delete_image 1
	    }
	}
	set data "data:image/png;base64,[cu::base64 -maxlen 60 encode [$imagename data -format png]]"
	set pNode [$boxNode selectNodes {p[@n='imagename']}]
	$pNode setAttribute v $data
	if { $delete_image } {
	    image delete $imagename
	}
    }
    foreach boxNode [$root selectNodes {lognoter_pdf_template/boxList/box[@boxtype='text']}] {
	if {  [$boxNode selectNodes {string(p[@n='substitutions']/@v)}] == 1 } {
	    set pNode [$boxNode selectNodes {p[@n='text']}]
	    if { [$pNode hasAttribute v] } {
		set text [$pNode @v]
		set has_v 1
	    } else {
		set text ""
		foreach n [$pNode childNodes] {
		    append text [$n asXML]
		}
		set has_v 0
	    }
	    unset -nocomplain text_nn
	    set err [catch {dom parse $text d}]
	    if { $err  } {
		set text_n [regmap {\[(sql|sql_field)\s.*?\]} $text x {
		        set val [subst $x]
		        set err [catch {dom parse $val d}]
		        if { !$err } {
		            set text_nn [$d asXML]
		        }
		        set _ $val
		    }]
	    } else {
		set text_n [regmap {\[(sql|sql_field)\s.*?\]} $text x {
		        set val [subst [xml_map1_inv $x]]
		        set err [catch {dom parse $val d}]
		        if { !$err } {
		            set text_nn [$d asXML]
		        }
		        set _ [xml_map1 $val]
		    }]
	    }
	    if { [info exists text_nn] } {
		set text_n $text_nn
	    }
	    if { $text_n ne $text } {
		foreach n [$pNode childNodes] { $n delete }
		set err [catch {dom parse $text_n d}]
		if { $err } {
		    $pNode setAttribute v $text_n
		} else {
		    if { $has_v } {
		        $pNode removeAttribute v
		    }
		    foreach n [$d selectNodes {/*}] {
		        $pNode appendChild $n
		    }
		}
	    }
	}
    }    
    set fout [open $filename w]
    fconfigure $fout -encoding utf-8
    puts $fout "<?xml version=\"1.0\" encoding=\"utf-8\"?><!-- -*- coding: utf-8;-*- -->"
    $doc asXML -indent none -channel $fout
    close $fout
    
    return $filename
}

proc xml2pdf::_update_boxList_to_new_style { boxList_old } {
    lassign "" boxList
    foreach i $boxList_old {
	set values [lassign $i boxtype pagetype dimensions]
	switch $boxtype {
	    mainbox { set vars flownumber }
	    text {
		if { [llength $values] == 7} {
		    set values [linsert $values 5 left]
		}
		set vars [list font fontsize fontfg fontbg anchor justify text substitutions]
	    }
	    image { set vars [list imagename substitutions] }
	    colorbox { set vars [list fillcolor outlinecolor outlinewidth bordercolor] }
	}
	set properties ""
	foreach n $vars v $values {
	    dict set properties $n $v
	}
	lappend boxList [list $boxtype $pagetype "" $dimensions "" $properties ""]
    }
    return $boxList
}

proc xml2pdf::_read_template_from_node { boxXMLroot papersizeName boxListName xsltName } {
    variable state
    upvar 1 $boxListName boxList $xsltName xslt $papersizeName papersize
    
    set papersize [$boxXMLroot selectNodes {string(p[@n='papersize']/@v)}]
    
    set boxList ""
    foreach boxNode [$boxXMLroot selectNodes {boxList/box}] {
	set elm ""
	foreach i [list boxtype pagetype part dimensions group] {
	    lappend elm [$boxNode @$i ""]
	}
	set properties ""
	foreach pNode [$boxNode selectNodes p] {
	    if { [$pNode hasAttribute v] } {
		dict set properties [$pNode @n] [$pNode @v]
	    } else {
		set v ""
		foreach n [$pNode childNodes] {
		    append v [$n asXML]
		}
		dict set properties [$pNode @n] $v
	    }
	}
	lappend elm $properties [$boxNode @condition ""]
	lappend boxList $elm
    }
    foreach pNode [$boxXMLroot selectNodes {container[@n='state']/p}] {
	if { [$pNode @subst 0] } {
	    set v [subst [$pNode @v]]
	} else {
	    set v [$pNode @v]
	}
	if { [$pNode @part ""] ne "" } {
	    set state(part[$pNode @part],[$pNode @n]) $v
	} else {
	    set state([$pNode @n]) $v
	}
    }
    set ns {xsl http://www.w3.org/1999/XSL/Transform}
    set node [$boxXMLroot selectNodes -namespaces $ns {xsl:stylesheet}]
    if { $node ne "" } {
	set xslt [$node asXML]
    } else {
	unset -nocomplain xslt
    }    
}


proc xml2pdf::_read_template { template template_needs_exist papersizeName boxListName xsltName } {
    variable templatesdirList
    variable state
    variable root
    
    if { [info exists state(force_papersize)] && $state(force_papersize) } {
	set papersize ""
    } else {
	upvar 1 $papersizeName papersize
    }
    upvar 1 $boxListName boxList $xsltName xslt
    
    if { [info exists state(basedir)] && [file readable [file join $state(basedir) $template.style]] } {
	set deffile [file join $state(basedir) $template.style]
    } else {
	foreach dir $templatesdirList {
	    set deffile [file join $dir $template $template.tcl]
	    if { [file exists $deffile] } { break }
	    set deffile [file join $dir $template $template.xml]
	    if { [file exists $deffile] } { break }
	}
    }

    if { [set tNode [$root selectNodes {lognoter_pdf_template}]] ne "" } {
	_read_template_from_node $tNode papersize boxList xslt
    } elseif { [file exists $deffile] } {
	set state(basedefdir) [file dirname $deffile]
	if { [file extension $deffile] eq ".tcl" } {
	    source $deffile
	    set boxList [_update_boxList_to_new_style $boxList]
	} else {
	    set boxXMLdoc [dom parse [tDOM::xmlReadFile $deffile]]
	    _read_template_from_node [$boxXMLdoc documentElement] papersize boxList xslt
	}
    } elseif { $template_needs_exist } {
	error "could not find template $template"
    }
    foreach dir $templatesdirList {
	set xsltfile [file join $dir $template $template.xslt]
	if { [file exists $xsltfile] } { break }
    }
    if { [file exists $xsltfile] } {
	set xslt [tDOM::xmlReadFile $xsltfile]
    }
}

proc xml2pdf::PrintXML { xml paper boxList opts { outputPDF 1 } } {

#     save_xml_file $xml
#     return
    set root [[dom parse $xml] documentElement]
    return [PrintXMLdom $root $paper $boxList $opts $outputPDF]
}

proc xml2pdf::PrintXMLdom { _root papersize boxList opts { outputPDF 1 } } {
    variable state
    variable idpages
    variable root
    variable scriptdir
    variable indexterm
    variable svg2draw
    
    set root $_root
    
    if { [info exists svg2draw] } {
	$svg2draw destroy
	unset svg2draw
    }
    
    if { ![dict exists $opts boxlistStyle] || [dict get $opts boxlistStyle] ne "dict" } {
	set boxList [_update_boxList_to_new_style $boxList]
    }
    
    array unset state
    array unset indexterm
    array unset idpages
    
    if { [dict exists $opts xslt] } {
	set xslt [dict get $opts xslt]
    }
    
    foreach "name value" $opts { set state($name) $value }
    
    if { [info exists state(printtype)] } {
	set printtype $state(printtype)
    } else {
	set printtype pdf
    }
    PDFWriter::choosetype $printtype
    
    if { [info exists state(template)] } {
	set template $state(template)
	set template_needs_exist 1
    } else {
	set template [$root nodeName]
	set template_needs_exist 0
    }
    if { [info exists state(template_doc)] } {
	_read_template_from_node [$state(template_doc) documentElement] papersize boxList xslt
    } else {
	_read_template $template $template_needs_exist papersize boxList xslt
    }
    if { $boxList eq "" } {
	error "error in xml2pdf::PrintXMLdom. boxList={}"
    }

    if { [info exists xslt] } {
	set doc [$root ownerDocument]
	set docXSLT [dom parse [subst -nocommands -novariables $xslt]]
	set newdoc [$root xslt $docXSLT]
	set root [$newdoc documentElement]
	$docXSLT delete
	$doc delete
    }

    if { ![info exists state(filename)] } { set state(filename) "" }
    
    if { ![info exists state(open_pdf)] || $state(open_pdf) } {
	lassign [PDFWriter::Init $papersize $state(filename)] state(xmax) state(ymax)
    } else {
	lassign [PDFWriter::NewPage $papersize] state(xmax) state(ymax)
    }
    
    set state(basedir) [pwd]
    if { ![info exists state(basedefdir)] } { set state(basedefdir) [pwd] }
    set state(boxList) $boxList
    
    if { ![info exists state(font)] } { 
	set state(font) Helvetica
    }
    if { ![info exists state(sizedefault)] } { 
	set state(sizedefault) 10
    }
    if { ![info exists state(fontsize)] } { 
	set state(fontsize) 9
    }
    if { ![info exists state(TitleBackgroundColor)] } { 
	set state(TitleBackgroundColor) #dceae6
    }
    if { ![info exists state(TitleForegroundColor)] } { 
	set state(TitleForegroundColor) ""
    }
    if { ![info exists state(leftrightpages)] } { 
	set state(leftrightpages) 0
    }

    set state(papersize) $papersize
    set state(bigmarginh1) [PDFWriter::cm2pt [expr {2*1.5}]]
    set state(bigmarginh2) [PDFWriter::cm2pt 2]
    set state(bigmarginv1) [PDFWriter::cm2pt 2]
    set state(bigmarginv2) [PDFWriter::cm2pt [expr {2*1.5}]]
    set state(indent_size) [expr {($state(xmax)-$state(bigmarginh1)-$state(bigmarginh2))/30.0}]
    set state(maxxreal) $state(bigmarginh1)
    set state(newpagescript) [list xml2pdf::PrintNewPage]

    array set state {
	SpaceBeforeSection 0.8
	SpaceAfterSectionText 3.5
	SpaceAfterSection 0.3
	SpaceAfterPara 0.4
	SpaceAfterListItem 0
	SpaceAfterLineText 0.5
	ImgDocFactor 0.3
	interline 1.5
    }

    set state(map) [list "&gt;" > "&lt;" < "&amp;" & "&quot;" \" "&apos;" ' \u201c \"]

    array set state {
	level 0
	relativev ""
	mediaobjectstack ""
	section 0
	sectionbookmark 0
	list_type ""
	listitem 0
	pagenum 1
	pagenum_logical 1
	part 1
	pagenum_style normal
	pagetype normal
	pageblock 0
	title ""
	printTitleNumbers 1
	titlestack ""
	sectionpagesstack ""
	fontcolor ""
	justify justify
	defaultformula_ingraphics 1
	HeaderType bold
	HeaderSizeFac 1.2
	TitleSizeFac 1.0
	HeaderColor ""
	print_title_pages 1
	print_bookmarks 1
    }

    foreach "name value" $opts { set state($name) $value }

    if { [info exists state(template_doc)] } {
	_read_template_from_node [$state(template_doc) documentElement] papersize boxList xslt
    } else {
	_read_template $template $template_needs_exist papersize boxList xslt
    }
    sql_get_state_values
    
    set state(size) $state(sizedefault)

    #set state(ypos) [PrintNewPage]
    set titleNode [$root selectNodes title]
    if { $titleNode != "" } {
	set state(titlestack) [list [$titleNode text]]
    } else {
	set state(titlestack) [list ""]
    }

    if { ![info exists state(printtoc)] } { set state(printtoc) 0 }
    if { $state(printtoc) == 1 } { set state(printtoc) start }

    set save_state [array get state]
    
    for { set pass 0 } { $pass < 2 } { incr pass } {
	set need_second_pass 0
	if { $state(printtoc) eq "start" } {
	    switch $printtype {
		pdf {
		    PrintTOC $root
		    set need_second_pass 1
		}
		default { set state(print_title_pages) 0 }
	    }
	}
	_TextDataToPDFDom [$root childNodes]
	PrintIndex
	
	if { $pass== 0 && [info exists state(need_second_pass)] && $state(need_second_pass) &&
	    $printtype eq "pdf" } {
	    set need_second_pass 1
	}
	
	if { $state(printtoc) eq "end" } {
	    if { $state(pagenum) > 1 && $state(pagenum)%2==1 } {
		set state(ypos) [PrintNewPage]
	    }
	    PrintTOC $root
	} elseif { $state(printtoc) eq "start" && $printtype ne "pdf" } {
	    if { $state(pagenum) > 1 && $state(pagenum)%2==1 } {
		PrintNewPage
	    }
	    set state(print_title_pages) 1
	    set state(pageblock) 0
	    set state(pagenum) 1
	    set state(pagenum_logical) 1
	    lassign [PDFWriter::NewPage $state(papersize)] state(xmax) state(ymax)
	    PrintTOC $root
	}
	
	if { $pass== 0 && $need_second_pass } {
	    set pagetotalnum $state(pagenum)
	    PDFWriter::End 0
	    unset state
	    array set state $save_state
	    PDFWriter::Init $papersize $state(filename)
	    set state(pagetotalnum) $pagetotalnum
	    set state(pageblock) 0
	    set state(pagenum) 1
	} else {
	    break
	}
    }
    
    
    
 #    for { set i 0 } { $i < 2 } { incr i } {
#         if { $i == 1 } {
#             unset state
#             array set state $save_state
#             PDFWriter::Init $papersize $state(filename)
#             PrintTOC $root
#             set state(pagetotalnum) $state(pagenum)
#             set state(pageblock) 0
#             set state(pagenum) 1
#             #PrintNewPage
#         }
#         if { $i == 0 && $state(printtoc) != 0 } {
#             if { $printtype eq "pdf" && $state(printtoc) eq "start" } {
#                 PrintTOC $root
#             } elseif { $printtype eq "pdf" && $state(printtoc) eq "end" } {
#                 # nothing
#             } else {
#                 set state(print_title_pages) 0
#             }
#         }
#         _TextDataToPDFDom [$root childNodes]
#         PrintIndex
#         if { $i == 0 && $state(printtoc) != 0 } {
#             if { $printtype eq "pdf"  && $state(printtoc) eq "start" } {
#                 PDFWriter::End 0
#             } elseif { $printtype eq "pdf" && $state(printtoc) eq "end" } {
#                 if { $state(pagenum) > 1 && $state(pagenum)%2==1 } {
#                     set state(ypos) [PrintNewPage]
#                 }
#                 PrintTOC $root
#                 break
#             } else {
#                 if { $state(pagenum) > 1 && $state(pagenum)%2==1 } {
#                     PrintNewPage
#                 }
#                 set state(print_title_pages) 1
#                 set state(pageblock) 0
#                 set state(pagenum) 1
#                 set state(pagenum_logical) 1
#                 foreach "state(xmax) state(ymax)" [PDFWriter::NewPage $state(papersize)] break
#                 PrintTOC $root
#                 break
#             }
#         } else { break }
#     }

#     _TextDataToPDFDom [$root childNodes]
#     PrintIndex
#     if { [info exists state(printtoc)] && $state(printtoc) } { PrintTOC $root }
    set state(pagetype) lastpage
    PageBackground

    #PDFWriter::DrawGrid pt

    if { [info exists state(close_pdf)] && $state(close_pdf) == 0 } {
	return
    }
    return [PDFWriter::End $outputPDF]
}

if { 0&& [info exists argv0] && [info script] == $argv0 } {
    
    # set boxList {
    #     {image all {0% 0% 100% 100%} {C:/Documents and Settings/ramsan/Mis documentos/Projects/Precon/images/report_base.gif}}
    #     {mainbox all {11.4% 10.5% 89.2% 92.6%} 1} 
    #     {text all {11% 94.5% 20% 96.4%} nw {$pagenum}}
    # }

    set boxList {
	{image all {0% 0% 100% 100%} {C:/Documents and Settings/ramsan/Mis documentos/Projects/Precon/images/report_base.gif}}
	{mainbox all {11.4% 10.6% 49% 92.8%} 1}
	{mainbox all {51% 10.4% 89% 92.9%} 2}
	{text all {51% 70% 89% 92.9%} Helvetica 10 black white nw {
	    <xml>Hola que tal:
	    <itemizedlist mark="opencircle">
	    <listitem>
	    <para>Això es 1</para>
	    </listitem>
	    <listitem>
	    <para>Això  és 2</para>
	    </listitem>
	    </itemizedlist>
	    </xml>
	}}
	{colorbox all {51% 70% 89% 92.9%} "" #020202 1}
	{text all {10.8% 5.54% 55.6% 7.37%} Helvetica-Bold 11 black white nw "MuroL - Cálculo y comprobación de muros en L"}
	{text all {66.6% 96.5% 83.9% 99.1%} Helvetica 6 #2F8D72 white n {[xpath /article/title]\nDeveloped by Compass Ing. y sist.\nhttp://www.compassis.com}}
	{text all {10.7% 94.9% 15.5% 96.6%} Helvetica 10 #2F8D72 red nw {$pagenum}}
    }
    set opts {
	TitleBackgroundColor #dceae6
	sizedefault 10
    }
	    
    set file "~/GiD Project/problemtypes/abacus.gid/info/abacus.xml"
    cd [file dirname $file]
    set xml [tDOM::xmlReadFile $file]
    set file [xml2pdf:::PrintXML $xml A4 $boxList $opts]
    # set newfile [file normalize  ~/../escritorio/kk.pdf]
    # file rename -force $file $newfile
    # eval exec [auto_execok start] "\"\"" [list [file nativename $newfile]] &
    exit

}