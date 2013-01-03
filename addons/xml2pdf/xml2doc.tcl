
namespace eval xml2pdf {

    variable topdir [file dirname [info script]]
}

proc xml2pdf::PrintWordML { xml opts { outputDOC 1 } } {
    variable topdir
    variable WordtemplatesdirList
    variable xsltmessage_oktoall

    unset -nocomplain xsltmessage_oktoall

    foreach "name value" $opts { set state($name) $value }
    if { ![info exists state(filename)] } {
	set tmpdir [PDFWriter::PDF::GiveTempDir]
	set ic 0
	while { $ic < 50 } {
	    set state(filename) [file join $tmpdir tmpfile$ic.doc]
	    if { [file exists $state(filename)] && [clock seconds] - [file mtime $state(filename)] < 30 } {
		incr ic; continue
	    }
	    if { ![catch { open $state(filename) w } fout] } {
		break
	    }
	    incr ic
	}
	if { $ic == 50 } {
	    error [_ "Error: could not open temporal file"]
	}
	set filename_is_userdef 0
    } else {
	set fout [open $state(filename) w]
	set filename_is_userdef 1
    }

    set doc [dom parse $xml]
    set root [$doc documentElement]

    set filexslt [file join $topdir docbook2WordML.xslt]
    set xmlXSLT [tDOM::xmlReadFile $filexslt]
    dom parse $xmlXSLT docXSLT

    dict set d default_image_width [dict get $opts default_image_width]
    dict set d page_internal_width [dict get $opts page_internal_width]
    
    $root xslt -xsltmessagecmd xml2pdf::xsltmessage \
	-parameters $d $docXSLT resdoc
    
    if { [info exists xsltmessage_oktoall] && $xsltmessage_oktoall == -1 } {
	error [_ "Stop at user demand"]
    }
    if { [dict exists $opts template] } {
	set template [dict get $opts template]
	foreach dir $WordtemplatesdirList {
	    if { [file exists [file join $dir $template.xml]] } {
		break
	    }
	}
	set xmlTempl [tDOM::xmlReadFile [file join $dir $template.xml]]
	set docTempl [dom parse $xmlTempl]
	set ns {w "http://schemas.microsoft.com/office/word/2003/wordml" }
	foreach sec [list w:styles w:docPr] {
	    set xp w:wordDocument/$sec
	    set secOld [$resdoc selectNodes -namespaces $ns $xp]
	    set secNew [$docTempl selectNodes -namespaces $ns $xp]
	    [$secOld parentNode] insertBefore $secNew $secOld
	    $secOld delete
	}
	set secOld [$resdoc selectNodes -namespaces $ns //w:sectPr]
	set secNew [$docTempl selectNodes -namespaces $ns //w:sectPr]
	if { [llength $secOld] == 1 && [llength $secNew] >= 1 } {
	    set secNew [lindex $secNew 0]
	    [$secOld parentNode] insertBefore $secNew $secOld
	    $secOld delete
	}

	foreach styleNode [$resdoc selectNodes -namespaces $ns //w:pStyle] {
	    set id [$styleNode @w:val]
	    set xp "/*/w:styles/w:style\[w:name/@w:val=[xpath_str $id]\]"
	    set n [$resdoc selectNodes -namespaces $ns $xp]
	    if { $n ne "" } {
		$styleNode setAttribute w:val [$n @w:styleId]
	    }
	}
    }
    fconfigure $fout -encoding utf-8
    puts $fout {<?xml version="1.0" encoding="utf-8"?><!-- -*- coding: utf-8;-*- -->}
    puts $fout [$resdoc asXML]
    close $fout

    $doc delete
    $docXSLT delete
    
    if { !$outputDOC } {
	return $state(filename)
    }
    set err [catch {
	    package require registry
	    set root HKEY_CLASSES_ROOT
	    set appKey [registry get $root\\.doc ""]
	    set appCmd [registry get $root\\$appKey\\shell\\open\\command ""]
	    set ret [regsub {%1} $appCmd [file native $state(filename)] appCmd]
	    if { !$ret } {
		append appCmd " \"[file nativename $state(filename)]\""
	    }
	    regsub -all {\\} $appCmd {\\\\} appCmd
	    eval exec $appCmd &
	} errstring]
    if { $err } {
	if { $filename_is_userdef } {
	    return $state(filename)
	}
	set file [tk_getSaveFile -filetypes \
		{{{Word files} ".doc"} {{All files} "*"}}  \
		-initialdir [pwd] \
		-title [_ "Save Word file"] -defaultextension .doc]
	if { $file == "" } { return }
	if { [string equal [file normalize $state(filename)] [file normalize $file]] } {
	    return $state(filename)
	}
	file copy -force $state(filename) $file
    }
    #eval exec [auto_execok start] [list "" [file native $pdffile]] &
    return $state(filename)

}

proc xml2pdf::GiveWordTemplates {} {
    variable WordtemplatesdirList

    set rets ""
    foreach dir $WordtemplatesdirList {
	foreach f [glob -tails -nocomplain -directory $dir -types f *] {
	    lappend rets [file root $f]
	}
	set ipos [lsearch $rets CVS]
	if { $ipos != -1 } {
	    set rets [lreplace $rets $ipos $ipos]
	}
    }
    return $rets
}

# page width minus margins
proc xml2pdf::give_internal_page_width { template } {
    variable topdir
    variable WordtemplatesdirList

    if { $template ne "" } {
	foreach dir $WordtemplatesdirList {
	    if { [file exists [file join $dir $template.xml]] } {
		break
	    }
	}
	set file [file join $dir $template.xml]
    } else {
	set file [file join $topdir docbook2WordML.xslt]
    }
    set xmlTempl [tDOM::xmlReadFile $file]
    set docTempl [dom parse $xmlTempl]
    set ns {w "http://schemas.microsoft.com/office/word/2003/wordml" }
    set sec [$docTempl selectNodes -namespaces $ns //w:sectPr]
    set width [$sec selectNodes -namespaces $ns {string(w:pgSz/@w:w)}]
    set marginL [$sec selectNodes -namespaces $ns {string(w:pgMar/@w:left)}]
    set marginR [$sec selectNodes -namespaces $ns {string(w:pgMar/@w:right)}]
    #  twips (twentieths of a point, 1/1440 of an inch)
    return [expr {($width-$marginL-$marginR)/20.0}]
}

proc xml2pdf::xsltmessage { message terminate } {
    variable xsltmessage_oktoall

    if { $terminate } {
	error $message
    } else {
	if { ![info exists xsltmessage_oktoall] } {
	    set w [dialogwin_snit ._ask -title [_ "Message"] \
		    -morebuttons [list [_ "OK to all"]] -entrytext $message]
	    set action [$w createwindow]
	    destroy $w
	    update
	    if { $action <= 0 } {
		set xsltmessage_oktoall -1
		# tdom does not check the error condition
		return -code error [_ "Stop at user demand"]
	    }
	    if { $action == 2 } {
		set xsltmessage_oktoall 1
	    }
	}
    }
}















