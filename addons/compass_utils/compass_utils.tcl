
package require snit
package require msgcat
package require textutil

package provide compass_utils 1.10

msgcat::mcload [file join [file dirname [info script]] msgs]

################################################################################
#   Small utilities
################################################################################

namespace eval ::math::constants {}
proc ::math::constants::give_constant { name } {
    ::math::constants::constants $name
    return [set $name]
}

namespace eval cu {
    variable topdir [file dirname [info script]]
}

proc cu::init {} {
    variable topdir
    
    uplevel #0 [list msgcat::mcload [file join $topdir msgs]]
}

if { [info commands ::_] eq "" } {
    proc ::_ { args } {
	set ret [uplevel 1 ::msgcat::mc $args]
	regexp {(.*)#C#(.*)} $ret {} ret
	return $ret
    }
}

if { [info commands ::=] eq "" } {
    proc ::= { args } { return [_ {*}$args] }
}

proc range {from to {step 1}} {

    if { ![string is double -strict $to] } {
	error "to='$to' is not a number"
    }
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

    if { ![string is double -strict $to] } {
	error "to='$to' is not a number"
    }
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

if { [info command lreverse] eq "" } {
    proc lreverse { L } {
	set res {}
	set i [llength $L]
	#while {[incr i -1]>=0} {lappend res [lindex $L $i]}
	while {$i} {lappend res [lindex $L [incr i -1]]} ;# rmax
	return $res
    }
}

proc cu::commalist_to_list { data } {
    set map [list "&#44;" "," "&amp;" "&"]
    set list ""
    foreach i [split $data ","] {
	lappend list [string map $map $i]
    }
    return $list
}

proc cu::list_to_commalist { list } {
    set ret ""
    set map [list "," "&#44;" "&" "&amp;"]
    foreach elm $list {
	lappend ret [string map $map $elm]
    }
    return [join $ret ","]
}

proc list_unique { list } {
    return [lsort -dictionary -unique $list]
}

proc list_union { list1 list2 } {
    return [lsort -dictionary -unique [concat $list1 $list2]]
}

proc list_intersection { list1 list2 } {
    set nlist ""
    foreach i $list2 {
	if { [lsearch -exact $list1 $i] != -1 } {
	    lappend nlist $i
	}
    }
    return $nlist
}

proc list_difference { list1 list2 } {
    set nlist ""
    foreach i $list1 {
	if { [lsearch -exact $list2 $i] == -1 } {
	    lappend nlist $i
	}
    }
    return $nlist
}

proc list_expr { list } {
    set ret ""
    foreach i $list {
	lappend ret [uplevel 1 expr $i]
    }
    return $ret
}

proc dict_inverse { dict } {
    set ret ""
    dict for "n v" $dict {
	dict set ret $v $n
    }
    return $ret
}

proc linsert0 { args } {
    set optional {
	{ -max_len len "" }
    }
    set compulsory "list element"
    parse_args $optional $compulsory $args

    set ipos [lsearch -exact $list $element]
    if { $ipos != -1 } {
	set list [lreplace $list $ipos $ipos]
    }
    set list [linsert $list 0 $element]
    if { $max_len ne "" } {
	set list [lrange $list 0 $max_len-1]
    }
    return $list
}

proc list_beautify { list } {
 
    set ret ""
    foreach i $list {
	if { $ret ne "" } { append ret " " }
	if { [regexp {^[{].*[}]$} [list $i]] } {
	    set l "\"$i\""
	    if { ![catch { lindex $l 0 }] && [lindex $l 0] eq $i } {
		append ret $l
	    } else {
		append ret [list $i]
	    }
	} else {
	    append ret [list $i]
	}
    }
    return $ret
}

# regmap re string var script
# substitute all the occurrences of 're' in 'string' with
# the result of the evaluation of 'script'. Script is called
# for every match with 'var' variable set to the matching string.
#
# Example: regmap {[0-9]+} "1 2 hello 3 4 world" x {expr {$x+1}}
# Result:  "2 3 hello 4 5 world"
proc regmap {re string var script} {
    set submatches [lindex [regexp -about $re] 0]
    lappend varlist idx
    while {[incr submatches -1] >= 0} {
	lappend varlist _
    }
    set res $string
    set indices [regexp -all -inline -indices $re $string]
    set delta 0
    foreach $varlist $indices {
	foreach {start end} $idx break
	set substr [string range $string $start $end]
	uplevel 1 [list set $var $substr]
	set subresult [uplevel 1 $script]
	incr start $delta
	incr end $delta
	if { $end < $start } {
	    set res1 ""
	    append res1 [string range $res 0 [expr {$start-1}]] $subresult \
		[string range $res $start end]
	    set res $res1
	} else {
	    set res [string replace $res $start $end $subresult]
	}
	incr delta [expr {[string length $subresult]-[string length $substr]}]
    }
    return $res
}

proc regexpfile { args } {

    set idx 0
    foreach i $args {
	if { ![string match -* $i] } { break }
	if { $i eq "--" } { incr idx ; break }
	incr idx
    }
    incr idx
    set file [lindex $args $idx]
    
    set fin [open $file r]
    set data [read $fin]
    close $fin
    lset args $idx $data
    return [eval regexp $args]

}

proc regsubfile { args } {

    set opts ""
    set noopts ""
    set isopt 0
    foreach i $args {
	if { $isopt || ![string match -* $i] } {
	    lappend noopts $i
	    set isopt 1
	} else {
	    lappend opts $i
	    if { $i == "--" } { set isopt 1 }
	}
    }
    if { [llength $noopts] == 4 } {
	foreach "exp file subSpec varName" $noopts break
    } elseif { [llength $noopts] == 3 } {
	foreach "exp file subSpec" $noopts break
    } else { error "error. usage: regsubfile ?switches? exp file subSpec ?varName?" }

    set fin [open $file r]
    
    set fconf [fconfigure $fin]
    foreach opt [list -length -allocated] {
	dict unset fconf $opt
    }
    fconfigure $fin -translation binary
    set header [read $fin 1024]
    seek $fin 0
    fconfigure $fin {*}$fconf
    set file_endline auto
    set len [string length $header]
    if { $len > 0 } {
	if { [regexp -all {\r\n} $header]*1.0/$len >= 0.005 } {
	    set file_endline crlf
	} elseif { [regexp -all {\n} $header]*1.0/$len >= 0.005 } {
	    set file_endline lf
	}
    }
    set data [read $fin]
    close $fin
    set retval [regsub {*}$opts $exp $data $subSpec data_out]

    if { [info exists varName] } {
	upvar $varName x
	set x $data_out
    }

    #file copy -force $file $file~
    if { $data ne $data_out } {
	set fout [open $file w]
	fconfigure $fout -translation $file_endline
	puts -nonewline $fout $data_out
	close $fout
    } elseif { $retval } { set retval "(change already applied)" }
    return $retval
}


proc dict_getd { args } {
    
    set dictionaryValue [lindex $args 0]
    set keys [lrange $args 1 end-1]
    set default [lindex $args end]
    if { [dict exists $dictionaryValue {*}$keys] } {
	return [dict get $dictionaryValue {*}$keys]
    }
    return $default
}

proc dict_lappend_r { dictionaryVariable args } {
    upvar 1 $dictionaryVariable dict
    set list [dict_getd $dict {*}[lrange $args 0 end-1] ""]
    lappend list [lindex $args end]
    dict set dict {*}[lrange $args 0 end-1] $list
}

# prints a tree defined as struct::tree
proc print_tree { tree { node root } } {
    set _ ""
    $tree walk $node i {
	append _ "$i [$tree parent $i] [$tree getall $i]\n"
    }
    return $_
}

proc print_tree_xml { tree { node root } } {
    package require tdom
    set _ ""
    $tree walk $node -order both "action i" {
	if { $action eq "enter" } {
	    append _ "<$i "
	    foreach "n v" [$tree getall $i] {
		if { [string is integer $n] } {
		    set n integer_$n
		}
		if { [string is double -strict $v] } {
		    set v [format %.3g $v]
		}
		append _ "$n='[xml_map $v]' "
	    }
	    append _ ">"
	} else {
	    append _ "</$i>"
	}
    }
    dom parse $_ doc
    
    package require RamDebugger
    interp eval ramdebugger [list RamDebugger::OpenFileSaveHandler \
	    "*formula (xml)*" [$doc asXML] ""]
    
}

proc incr! { varName { increment 1 } } {

    upvar $varName v
    if { ![info exists v] } { set v 0 }
    return [incr v $increment]
}

proc set! { varName args } {
    
    upvar 1 $varName v
    if { [llength $args] == 0 && ![info exists v] } {
	return ""
    }
    return [uplevel 1 [list set $varName {*}$args]]
}

proc info_fullargs { procname } {
    set ret ""
    foreach arg [uplevel 1 [list info args $procname]] {
	if { [uplevel 1 [list info default $procname $arg value]] } {
	    upvar 1 value value
	    lappend ret [list $arg $value]
	} else {
	    lappend ret $arg
	}
    }
    return $ret
}

proc info_fullproc { procname procnameN } {
    set args [uplevel 1 info_fullargs $procname]
    return [list proc $procnameN $args [uplevel 1 info body $procname]]
}

namespace eval cu::ncgi_utf-8 {}

proc cu::ncgi_utf-8::value { args } { return [encoding convertfrom utf-8 [ncgi::value {*}$args]] }
proc cu::ncgi_utf-8::nvlist {} { return [encoding convertfrom utf-8 [ncgi::nvlist]] }

namespace eval cu::img {}

proc cu::img::exists { img } {
    set err [catch { image type $img }]
    if { $err } {
	return 0
    } else {
	return 1
    }
}

proc cu::img::is_png { data } {
    set png_signature "\x89PNG\x0d\x0a\x1a\x0a"
    return [string equal -length [string length $png_signature] $png_signature $data]
}

proc cu::img::is_gif { data } {
    set gif87_signature "GIF87a"
    set gif89_signature "GIF89a"
    if { [string equal -length [string length $gif87_signature] $gif87_signature $data] } {
	return 1
    }
    if { [string equal -length [string length $gif89_signature] $gif89_signature $data] } {
	return 1
    }
    return 0
}

proc cu::img::is_jpeg { data } {
    set jpeg_signature "JFIF"
    return [string equal -length 4 $jpeg_signature [string range $data 6 9]]
}

proc cu::mc { args } {
    set ret [uplevel 1 ::msgcat::mc $args]
    regexp {(.*)#C#(.*)} $ret {} ret
    return $ret
}

################################################################################
#    cu::string
################################################################################


namespace eval cu::string {
    variable unique_id 0
}

proc cu::string::give_unique_id { { prefix uid } } {
    variable unique_id
    return $prefix[incr unique_id]
}


proc cu::string::maintain_case { word case_word } {
    if { $word ne [string tolower $word] } { return $word }
    if { $case_word eq [string toupper $case_word] } { return [string toupper $word] }
    set cw0 [string index $case_word 0]
    if { $cw0 eq [string toupper $cw0] } { return [string totitle $word] }
    return $word
}

proc cu::string::csv_join {values {sepChar ,} {delChar \"}} {
    set out ""
    set sep {}
    foreach val $values {
	if {[string match "*\[${delChar}$sepChar\]*" $val]} {
	    append out $sep${delChar}[string map [list $delChar ${delChar}${delChar}] $val]${delChar}
	} else {
	    append out $sep${val}
	}
	set sep $sepChar
    }
    return $out
}


namespace eval ::base64 {
    proc isbase64 { data } {
	if { [regexp {^[a-zA-Z0-9+/=\s]*$} $data] } {
	    return 1
	} else { return 0 }
    }
}


namespace eval cu::file {
    variable tempdir    {}
    variable tempdirSet 0
}

proc cu::file::are_equal { file1 file2 } {
    return [expr {[file normalize $file1] eq [file normalize $file2]}]
}

proc cu::file::appdatadir { program_name } {
    if { [info exists ::env(APPDATA)] } {
	set AppDataDir [file join $::env(APPDATA) $program_name]
    } elseif { $::tcl_platform(platform) == "windows" } {
	package require registry
	set key {HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion}
	append key {\Explorer\Shell Folders}
	set err [catch { registry get $key AppData } AppData]
	if { !$err } {
	    set AppDataDir [file join [registry get $key AppData] $program_name]
	} else {
	    set AppDataDir [file join $::env(HOME) .$program_name]
	}
    } else { set AppDataDir [file join $::env(HOME) .$program_name] }
    
    file mkdir $AppDataDir
    
    return $AppDataDir
}

proc cu::file::give_standard_dirs {} {
    if { $::tcl_platform(platform) eq "windows" } {
	set dirs ""
	set key {HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders}
	foreach i [list Personal Desktop] {
	    lappend dirs [registry get $key $i]
	}
	lappend dirs [tempdir]
    } else {
	set dirs [list $::env(HOME) [tempdir]]
	set f [file join $::env(HOME) .config user-dirs.dirs]
	if { [file exists $f]} {
	    set fin [open $f r]
	    set data [read $fin]
	    close $fin
	    # XDG_MUSIC_DIR XDG_PICTURES_DIR XDG_VIDEOS_DIR
	    foreach i [list XDG_DESKTOP_DIR XDG_DOCUMENTS_DIR] {
		set rex "$i\s*=(.*)"
		if { [regexp -line $rex $data {} dir] } {
		    set dir [string trim $dir " \t\n\""]
		    set dir [string map [list \$HOME $::env(HOME)] $dir]
		    lappend dirs $dir
		}
	    }
	} else {
	    # Pictures Images Music Videos Movies
	    foreach i [list Desktop Documents] {
		set dir [file join $::env(HOME) $i]
		if { [file exists $dir] } {
		    lappend dirs $dir
		}
	    }
	}
    }
    return $dirs
}

proc cu::file::tempdir {args} {
    if {[llength $args] > 1} {
	return -code error {wrong#args: should be "cu::file::tempdir ?path?"}
    } elseif {[llength $args] == 1} {
	variable tempdir [lindex $args 0]
	variable tempdirSet 1
	return
    }
    return [file normalize [_tempdir]]
}

proc cu::file::_tempdir {} {
    global tcl_platform env
    variable tempdir
    variable tempdirSet

    set attempdirs [list]

    if {$tempdirSet} {
	lappend attempdirs $tempdir
    } else {
	foreach tmp {TMPDIR TEMP TMP} {
	    if { [info exists env($tmp)] } {
		lappend attempdirs $env($tmp)
	    }
	}

	switch $tcl_platform(platform) {
	    windows {
		lappend attempdirs "C:\\TEMP" "C:\\TMP" "\\TEMP" "\\TMP"
	    }
	    macintosh {
		set tmpdir $env(TRASH_FOLDER)  ;# a better place?
	    }
	    default {
		lappend attempdirs [file join / tmp] \
		        [file join / var tmp] [file join / usr tmp]
	    }
	}
	lappend attempdirs [pwd]
    }

    foreach tmp $attempdirs {
	if { [file isdirectory $tmp] } {
	    # && [file writable $tmp]
	    return $tmp
	}
    }

    # Fail if nothing worked.
    return -code error "Unable to determine a proper directory for temporary files"
}

proc cu::file::tempfile {{prefix {}} { extension "" } } {
    return [file normalize [_tempfile $prefix $extension]]
}

proc cu::file::_tempfile {prefix { extension "" } } {
    set tmpdir [tempdir]

    set chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    set nrand_chars 10
    set maxtries 10
    set access [list RDWR CREAT EXCL TRUNC]
    set permission 0600
    set channel ""
    set checked_dir_writable 0
    set mypid [pid]
    for {set i 0} {$i < $maxtries} {incr i} {
	 set newname $prefix
	 for {set j 0} {$j < $nrand_chars} {incr j} {
	     append newname [string index $chars \
		    [expr {int(rand()*62)}]]
	 }
	set newname [file join $tmpdir $newname]$extension
	 if {[file exists $newname]} {
	     after 1
	 } else {
	     if {[catch {open $newname $access $permission} channel]} {
		 if {!$checked_dir_writable} {
		     set dirname [file dirname $newname]
		     if {![file writable $dirname]} {
		         return -code error "Directory $dirname is not writable"
		     }
		     set checked_dir_writable 1
		 }
	     } else {
		 # Success
		close $channel
		return $newname
	     }
	 }
    }
    if {[string compare $channel ""]} {
	 return -code error "Failed to open a temporary file: $channel"
    } else {
	 return -code error "Failed to find an unused temporary file name"
    }
}

proc cu::file::make_path_relative { basedir path } {
    set basedirList [file split [file normalize $basedir]]
    set pathList [file split [file normalize $path]]
    
    set len1 [llength $basedirList]
    set len2 [llength $pathList]
    for { set i 0 } { $i < $len1 && $i < $len2 } { incr i } {
	if { [lindex $basedirList $i] ne [lindex $pathList $i] } { break }
    }
    if { $i < 1 } { return $path }
    set newpath [string repeat ../ [expr {$len1-$i}]]
    set newpath [eval [list file join $newpath] [lrange $pathList $i end]]
    return $newpath
}

# cu::file::stripPath (copied from ::fileutil::stripPath )
#
#        If the specified path references/is a path in prefix (or prefix itself) it
#        is made relative to prefix. Otherwise it is left unchanged.
#        In the case of it being prefix itself the result is the string '.'.
#
# Arguments:
#        prefix                prefix to strip from the path.
#        path                path to modify
#
# Results:
#        path                The (possibly) modified path.

proc cu::file::stripPath {prefix path} {
    # [file split] is used to generate a canonical form for both
    # paths, for easy comparison, and also one which is easy to modify
    # using list commands.

    if {[string equal $prefix $path]} {
	return "."
    }

    set prefix [file split $prefix]
    set npath  [file split $path]

    if {[string match ${prefix}* $npath]} {
	set path [eval [linsert [lrange $npath [llength $prefix] end] 0 file join ]]
    }
    return $path
}

# cu::file::jail (copied and fixed from fileutil::jail)
#
#        Ensures that the input path 'filename' stays within the the
#        directory 'jail'. In this way it preventsuser-supplied paths
#        from escaping the jail.
#
# Arguments:
#        jail                The path to the directory the other must
#                        not escape from.
#        filename        The path to prevent from escaping.
#
# Results:
#        path                The (possibly) modified path surely within
#                        the confines of the jail.

proc cu::file::jail {jail filename} {
    if {![string equal [file pathtype $filename]  "relative"]} {
	# Although the path to check is absolute (or volumerelative on
	# windows) we cannot perform a simple prefix check to see if
	# the path is inside the jail or not. We have to normalize
	# both path and jail and then we can check. If the path is
	# outside we make the original path relative and prefix it
	# with the original jail. We do make the jail pseudo-absolute
	# by prefixing it with the current working directory for that.

	# Normalized jail. Fully resolved sym links, if any. Our main
	# complication is that normalize does not resolve symlinks in the
	# last component of the path given to it, so we add a bogus
	# component, resolve, and then strip it off again. That is why the
	# code is so large and long.

	set njail [eval [list file join] [lrange [file split \
		[file normalize [file join $jail __dummy__]]] 0 end-1]]

	# Normalize filename. Fully resolved sym links, if
	# any. S.a. for an explanation of the complication.

	set nfile [eval [list file join] [lrange [file split \
		[file normalize [file join $filename __dummy__]]] 0 end-1]]

	if {[string match ${njail}* $nfile]} {
	    return $filename
	}

	# Outside the jail, put it inside. ... We normalize the input
	# path lexically for this, to prevent escapes still lurking in
	# the original path. (We cannot use the normalized path,
	# symlinks may have bent it out of shape in unrecognizable ways.

	return [eval [linsert [lrange [file split \
		[_lexnormalize $filename]] 1 end] 0 file join [pwd] $jail]]
    } else {
	# The path is relative, consider it as outside
	# implicitly. Normalize it lexically! to prevent escapes, then
	# put the jail in front, use PWD to ensure absoluteness.

	return [eval [linsert [file split [_lexnormalize $filename]] 0 \
		file join [pwd] $jail]]
    }
}

proc cu::file::_lexnormalize {sp} {
    set sp [file split $sp]

    # Resolution of embedded relative modifiers (., and ..).

    set np {}
    set noskip 1
    while {[llength $sp]} {
	set ele    [lindex $sp 0]
	set sp     [lrange $sp 1 end]
	set islast [expr {[llength $sp] == 0}]

	if {[string equal $ele ".."]} {
	    if {[llength $np] > 1} {
		# .. : Remove the previous element added to the
		# new path, if there actually is enough to remove.
		set np [lrange $np 0 end-1]
	    }
	} elseif {[string equal $ele "."]} {
	    # Ignore .'s, they stay at the current location
	    continue
	} else {
	    # A regular element.
	    lappend np $ele
	}
    }
    if {[llength $np] > 0} {
	return [eval file join $np]
    }
    return {}
}

proc cu::splitx {str {regexp {[\t \r\n]+}}} {
    if {[string length $str] == 0} {
	return {}
    }
    if {[string length $regexp] == 0} {
	return [::split $str ""]
    }
    set list  {}
    set start 0
    while {[regexp -start $start -indices -- $regexp $str match submatch]} {
	foreach {subStart subEnd} $submatch break
	foreach {matchStart matchEnd} $match break
	incr matchStart -1
	incr matchEnd
	lappend list [string range $str $start $matchStart]
	if {$subStart >= $start} {
	    lappend list [string range $str $subStart $subEnd]
	}
	set start $matchEnd
    }
    lappend list [string range $str $start end]
    return $list
}

#return 0 if is not a link,  1 if the file is a link or  2 if it is Windows shell link 
proc cu::islink { lnk } {
    set islink 0
    if { ![file exists $lnk] } {
	return 0
    }
    if { [string match "windows" $::tcl_platform(platform)] } {
	set type [file type $lnk]
	if { $type == "file" && [string tolower [file extension $lnk]] == ".lnk" } {
	    set fp [open $lnk]
	    fconfigure $fp -encoding binary -translation binary -eofchar {}
	    binary scan [read $fp 4] i tmp
	    if { $tmp == "76" } { 
		binary scan [read $fp 16] h32 tmp
		if { $tmp == "10412000000000000c00000000000064" } { 
		    set islink 2 ;#windows shell link
		}
	    }
	    close $fp
	} 
	if { !$islink } {
	    if { ![catch  {set res [file readlink $lnk]}] } {
		set islink 1
	    }
	}
    } else {
	if { ![catch  {set res [file readlink $lnk]}] } {
	    set islink 1
	}
    }
    return $islink
}

#return the name pointed by a link or Windows shell link
proc cu::readlink { lnk } {
    set res ""
    set islink [islink $lnk]
    if { $islink == 1 } {
	catch {set res [file readlink $lnk]} err
    } elseif { $islink == 2 } {
	catch { set res [cu::readlinkwin $lnk] }
    }
    return $res
}

proc cu::file::get_executable_path { exe } {
    set exe0 $exe
    set valueName open_$exe
    set dict [cu::get_program_preferences -valueName $valueName compass_utils]
    set needs_store 0
    if { [dict exists $dict exe] } {
	set exe [dict get $dict exe]
	if { ![file readable $exe] } {
	    set exe ""
	}
    } else {
	set exe ""
    }
    if { $exe eq "" } {
	set title [_ "Find executable for '%s'" $exe0]
	set types [list \
	    [list [_ "Executable file"] {.exe}] \
	    [list [_ "All files"] {*}] \
	    ]
	set exe [tk_getOpenFile -filetypes $types -initialdir / -initialfile $exe0 \
		-title $title -defaultextension ".exe"]
	if { $exe eq "" } { return "" }
	if { ![file readable $exe] } {
	    return ""
	}
	dict set dict exe $exe
	cu::store_program_preferences -valueName $valueName compass_utils $dict 
    }
    return $exe
}

proc cu::file::execute { args } {
    
    set optional {
	{ -workdir directory "" }
	{ -wait boolean 0 }
	{ -hide_window boolean 0 }
    }
    set compulsory "what file"

    set args [parse_args -raise_compulsory_error 0 $optional $compulsory $args]

    switch -- $what {
	gid {
	    set exe [get_executable_path gid]
	    if { $exe eq "" } { return }
	    if { $wait || $hide_window } {
		set err [catch { package require twapi }]
		if { $err } { set has_twapi 0 } else { set has_twapi 1 }
	    }
	    if { !$wait || $has_twapi } { lappend args & }
	    set pid [exec $exe $file {*}$args]
	   
	    if { !$wait && !$hide_window } { return }
	    if { !$has_twapi } { return }

	    if { $hide_window } {
		foreach hwin [twapi::find_windows -pids $pid -visible true] {
		    twapi::hide_window $hwin
		}
	    }
	    if { $wait } {
		while { [twapi::process_exists $pid] } {
		    after 200
		}
	    }
	}
	emacs {
	    exec runemacs -g 100x72 &
	}
	wish {
	    set pwd [pwd]
	    cd [file dirname $file]
	    eval exec wish [list [file normalize $file]] $args &
	    cd $pwd
	}
	tkdiff {
	    set pwd [pwd]
	    cd [file dirname $file]
	    exec wish ~/myTclTk/tkcvs/bin/tkdiff.tcl -r [file tail $file] &
	    cd $pwd
	}
	start {
	    if { $::tcl_platform(platform) eq "unix" } {
		set programs [list xdg-open gnome-open]
		if { $::tcl_platform(os) eq "Darwin" } {
		    set programs [linsert $programs 0 open]
		}
		foreach i $programs {
		    if { [auto_execok $i] ne "" } {
		        exec $i $file &
		        return
		    }
		}
		error "could not open file '$file'"
	    } elseif { [regexp {[&]} $file] } {
		set bat [file join [file dirname $file] a.bat]
		set fout [open $bat w]
		puts $fout "start \"\" \"$file\""
		close $fout
		exec $bat 
		file delete $bat
	    } else {
		eval exec [auto_execok start] \"\" [list $file] {*}$args &
	    }
	}
	url {
	    if { [regexp {^[-\w.]+$} $file] } {
		set file http://$file
	    }
	    if { ![regexp {(?i)^\w+://} $file] && ![regexp {(?i)^mailto:} $file] } {
		set txt [_ "url does not begin with a known handler like: %s. Proceed?" \
		        "http:// ftp:// mailto:"]
		set retval [tk_messageBox -default ok -icon question -message $txt \
		        -type okcancel]
		if { $retval == "cancel" } { return }
	    }
	    if { $::tcl_platform(platform) eq "windows" } {
		exec rundll32 url.dll,FileProtocolHandler $file &
	    } else {
		set programs [list xdg-open gnome-open]
		if { $::tcl_platform(os) eq "Darwin" } {
		    set programs [linsert $programs 0 open]
		}
		foreach i $programs {
		    if { [auto_execok $i] ne "" } {
		        exec $i $file &
		        return
		    }
		}
		set cmdList ""
		foreach i [list firefox konqueror mozilla opera netscape] {
		    lappend cmdList "$i \"$file\""
		}
		exec sh -c [join $cmdList "||"] & 
	    }
	}
	exec {
	    if { $workdir ne "" } {
		set pwd [pwd]
		cd $workdir
	    }
	    set err [catch { exec $file {*}$args } errstring]
	    if { $workdir ne "" } { cd $pwd }
	    if { $err } {
		error $errstring $::errorInfo
	    }
	}
	execList {
	    foreach i $file {
		if { [auto_execok [lindex $i 0]] ne "" } {
		    exec {*}$i &
		    return
		}
	    }
	  error "Could not execute files"
	}
	default {
	    if { $workdir ne "" } {
		set pwd [pwd]
		cd $workdir
	    }
	    set err [catch { exec $file {*}$args & } errstring]
	    if { $workdir ne "" } { cd $pwd }
	    if { $err } {
		error $errstring $::errorInfo
	    }
	}
    }  
}

proc cu::write_traces { variable_name what args } {
    upvar 1 $variable_name v
    switch $what {
	deactivate {
	    set cmdList ""
	    foreach i [trace info variable v] {
		lassign $i opList commandPrefix
		if { $opList eq "write" } {
		    lappend cmdList $commandPrefix
		    trace remove variable v $opList $commandPrefix
		}
	    }
	    return $cmdList
	}
	activate {
	    lassign $args cmdList
	    foreach commandPrefix $cmdList {
		trace add variable v write $commandPrefix
	    }
	}
    }
}

proc cu::get_computer_name {} {
    
    set computer_name ""
    catch {
	if { $::tcl_platform(platform) eq "windows" } {
	    package require twapi
	    set computer_name [twapi::get_computer_name]
	} else {
	    set fin [open /etc/hostname r]
	    set computer_name [string trim [read $fin]]
	    close $fin
	}
    }
    return $computer_name
}

proc cu::file::_edit_in_ramdebugger_file { w } {
    set file [tk_getOpenFile -defaultextension .tcl -filetypes [list \
		[list [_ "TCL files"] [list ".tcl"]] \
		[list [_ "All files"] [list "*"]]] \
	    -parent $w -title [_ "Find RamDebugger.tcl"]]
    $w set_uservar_value file $file
}

proc cu::file::edit_in_ramdebugger { args } {
    
    set optional {
	{ -interp interp "" }
	{ -geometry geom 750x550 }
    }
    set compulsory "wp title data callback"
    parse_args $optional $compulsory $args

    if { $interp ne "" } {
	set fullintp [list $interp ramdebugger]
    } else {
	set fullintp ramdebugger
    }
    interp eval $interp {
	if { ![interp exists ramdebugger] } { interp create ramdebugger }
    }
    interp eval $fullintp [list set argv [list -noprefswrite -rgeometry $geometry \
	    -onlytext -noopendefault]]
    
    set valueName ramdebugger_auto_path_dirs
    set dict [cu::get_program_preferences -valueName $valueName compass_utils]
    set dirs [dict_getd $dict dirs ""]

    if { [llength $dirs] } {
	interp eval $interp [list lappend ::auto_path {*}$dirs]
    }
    set err [interp eval $interp [list catch { package require RamDebugger }]]
    
    if { $err } {
	package require dialogwin
	destroy $wp._ask
	set w [dialogwin_snit $wp._ask -title [_ "Open RamDebugger"] -entrytext \
		[_ "RamDebugger cannot be open. It can be installed from http://www.compassis.com/ramdebugger."]]
	set f [$w giveframe]
	
	ttk::label $f.l1 -text [_ "Do you want to find it manually (RamDebugger.tcl)?"]
	ttk::label $f.l2 -text [_ "File"]:
	ttk::entry $f.e -textvariable [$w give_uservar file ""]
	ttk::button $f.b -image [cu::get_icon fileopen16] \
	    -style Toolbutton -command [list cu::file::_edit_in_ramdebugger_file $w]
	
	grid $f.l1 - - -sticky w
	grid $f.l2 $f.e $f.b -sticky w -padx 2 -pady 3
	grid configure $f.e -sticky ew
	grid columnconfigure $f 1 -weight 1
	
	tk::TabToWindow $f.e
	bind [winfo toplevel $f] <Return> [list $w invokeok]
	set action [$w createwindow]
	set file [$w give_uservar_value file]
	destroy $w
	if { $action < 1 || ![file exists $file] } { error [_ "RamDebugger not found"] }
	set dir [file dirname $file]
	set dirs [linsert $dirs 0 $dir]
	dict set dict dirs $dirs
	cu::store_program_preferences -valueName $valueName compass_utils $dict 

	interp eval $interp [list lappend ::auto_path {*}$dirs]
	interp eval $interp [list package require RamDebugger]
    }
    interp alias $fullintp EditEndInRamDebugger "" {*}$callback
    interp eval $fullintp [list RamDebugger::OpenFileSaveHandler \
	    "*$title*" $data EditEndInRamDebugger]
}

proc cu::file::mtime_recursive { file } {
    
    set err [catch { file mtime $file } mtime]
    if { $err } { set mtime "" }
    if { [file isdirectory $file] } {
	foreach f [glob -nocomplain -directory $file *] {
	    set m [mtime_recursive $f]
	    if { $m ne "" } {
		if { $mtime eq "" || $m > $mtime } {
		    set mtime $m
		}
	    }
	}
    }
    return $mtime
}

proc cu::file::sha1 { file } {
 
    set fin [open $file rb]
    set ret [cu::sha1 [read $fin]]
    close $fin
    return $ret
}

proc cu::nice_time { seconds } {
    if { $seconds < 0 } {
	set sign "-"
    } else {
	set sign ""
    }
    set seconds [expr {abs($seconds)}]
    set secondsI [expr {int($seconds)}]
    set centi_seconds [expr {round(100*($seconds-$secondsI))}]
    set hours [expr {$secondsI/3600}]
    set secondsI [expr {$secondsI-$hours*3600}]
    set minutes  [expr {$secondsI/60}]
    set secondsI [expr {$secondsI-$minutes*60}]
    if { $seconds != 0 && $seconds < 1 } {
	return [format "$sign%.3g" $seconds]
    } elseif { $seconds < 60 && $centi_seconds != 0 } {
	return [format "$sign%02d.%02d" $secondsI $centi_seconds]
    } elseif { $hours != 0 } {
	return [format "$sign%02d:%02d:%02d" $hours $minutes $secondsI]
    } else {
	return [format "$sign%02d:%02d" $minutes $secondsI]
    }
}

proc date_sql2tcl { date } {
    if { [regexp {^[-: 0.]*$} $date] } { return "" }
    return [clock scan $date]
}
proc date_tcl2sql { date } {
    if { $date == "" } { return "" }
    return [clock format $date -format "%Y-%m-%d %H:%M:%S"]
}

proc date_es2iso1 { date } {
    lassign [split $date "/"] d m y
    return "$y-$m-$d"
}

proc cu::bgerror { err } {
    set info $::errorInfo
    destroy ._bgerror
    package require dialogwin
    set w [dialogwin_snit ._bgerror -title [_ "Warning"] -entrytext \
	    $err -cancelname -]
    set cmd "[list set ::errorInfo $info];"
    append cmd [list ::tk::dialog::error::bgerror $err]
    bind $w <Control-x><Control-x> $cmd
    set action [$w createwindow]
    destroy $w
}

proc cu::init_release_bgerror {} {
    auto_load ::bgerror
    interp alias "" ::bgerror "" cu::bgerror
}


################################################################################
#    Package to make logging for the debug phase
################################################################################

namespace eval ::mylog {
    variable sqlite3explorer {C:\TclTk\sqlite-3.2.1-sources\sqlite3explorer\sqlite3explorer.exe}
    variable logfile ""
    variable last_delta_time

    namespace export mylog
}

# if levels is void, disconnected
# levels can contain: debug debugoff log error time_start time_end
proc ::mylog::init { args } {
    variable logfile
    
    set optional {
	{ -view_binding binding "" }
	{ -file file "" }
	{ -restart_file boolean 0 }
    }
    set compulsory { levels }

    parse_args $optional $compulsory $args

    set logfile $file

    if { [llength $levels] } {
	package require sqlite3
	if { $logfile ne "" } {
	    if { $restart_file } {
		set err [catch {
		        sqlite3 ::mylog::db $logfile
		        ::mylog::db eval { delete from log }
		        ::mylog::db close
		    }]
		if { $err } {
		    catch { ::mylog::db close }
		    file delete $file
		}
	    }
	    sqlite3 ::mylog::db $logfile
	} else {
	    if { [info command ::mylog::db] eq "" || $restart_file } {
		sqlite3 ::mylog::db :memory:
	    }
	}
	::mylog::db eval { PRAGMA synchronous = OFF }
	::mylog::db timeout 3000
	if { ![mylog::table_exists ::mylog::db log] } {
	    ::mylog::db eval { create table log(idx integer primary key,date datetimetext,delta_time numeric,
		level text,txt text,proc text,proc_args text) }
	}
    }
    foreach i $levels {
	interp alias "" ::mylog::$i "" ::mylog::mylog $i
    }
    foreach i [list debug debugoff log error time_start time_end] {
	if { [lsearch -exact $levels $i] == -1 } {
	    proc ::mylog::$i { args } {}
	}
    }
    if { [llength $levels] && $view_binding ne "" } {
	if { [info command bind] ne "" } {
	    bind all $view_binding [list mylog::view_log]
	}
    }
}

proc ::mylog::table_exists { db name } {
    
    set ret [$db eval { select name from sqlite_master where type='table' 
	    and name=$name }]
    if { ![llength $ret] } { return 0 } else { return 1 }
}

proc ::mylog::give_sqlite3_db {} {
    if { [info command ::mylog::db] eq "" } {
	error "error in mylog::give_sqlite3_db"
    }
    return ::mylog::db
}
proc ::mylog::mylog { level txt } {
    variable last_delta_time
    
    set now [clock microseconds]
    set micro [format %.6d [expr {$now%1000000}]]
    set secs [expr {$now/1000000}]
    set format "%Y-%m-%d %H:%M:%S.$micro"
    set date [clock format $secs -format $format]
    set err [catch { info level -1 } ret]
    if { !$err && [lsearch -exact "debug debugoff" $level] != -1 } {
	set proc [lindex $ret 0]
	set proc_args [lrange $ret 1 end]
    } else { 
	lassign "" proc proc_args
    }
    if { [info exists last_delta_time] } {
	set delta_time [expr {$now-$last_delta_time}]
    } else {
	set delta_time 0
    }
    set last_delta_time $now
    
    ::mylog::db eval { insert into log (date,delta_time,level,txt,proc,proc_args) 
	values($date,$delta_time,$level,$txt,$proc,$proc_args) }
}

proc ::mylog::clear_log { args } {

    set optional {
	{ -confirm boolean 0 }
	{ -parent parent_window . }
    }
    parse_args $optional "" $args

    if { $confirm } {
	set num [::mylog::db onecolumn { select count(*) from log }]
	set text [_ "There are %s message. Delete all of them?" $num]
	set retval [snit_messageBox -default ok -icon question -message $text \
		-type okcancel -parent $parent]
	if { $retval == "cancel" } { return }
    }
    ::mylog::db eval { delete from log }
}

proc ::mylog::view_log { args } {
    variable sqlite3explorer
    variable logfile

    set optional {
	{ -extlogfile file "" }
	{ -parent parent_window "" }
	{ -clear_handler handler "" }
    }
    parse_args $optional "" $args

    if { $extlogfile ne "" } {
	set file $extlogfile
    } else {
	set file $logfile
    }
    set err [catch {
	    package require fulltktree
	    package require dialogwin
	}]
    if { !$err } {
	return [_view_log_fulltktree {*}$args]
    }
    if { $file eq "" } {
	set file [file join $::env(TEMP) logfile.db3]
	_write_log_to_file $file
    }
    exec $sqlite3explorer $file &
    return ""
}

proc ::mylog::_write_log_to_file { file } {
    file delete $file
    sqlite3 mylog::other $file
    ::mylog::db eval {SELECT sql FROM sqlite_master WHERE sql NOT NULL} {
	mylog::other eval $sql
    }
    mylog::other close
    
    ::mylog::db eval "ATTACH '$file' AS app"
    ::mylog::db eval {SELECT name FROM sqlite_master WHERE type='table'} {
	::mylog::db eval "INSERT INTO app.$name SELECT * FROM $name"
    }
    ::mylog::db eval {DETACH app}
}

proc ::mylog::give_log_data {} {
    return [::mylog::db eval "select * from log order by idx"]
}

proc ::mylog::add_log_data { data } {
    ::mylog::db eval "begin"
    foreach "idx date delta_time level txt proc proc_args" $data {
	::mylog::db eval { insert into log (idx,date,delta_time,level,txt,proc,proc_args) 
	    values($idx,$date,$delta_time,$level,$txt,$proc,$proc_args) }
    }
    ::mylog::db eval "commit"
}

proc ::mylog::_clear_search { w entry } {
    $w set_uservar_value search ""
    tk::TabToWindow $entry
}

proc ::mylog::_keydelete_search { w entry } {
    if { [$entry index insert] == [$entry index end] } {
	$w set_uservar_value search ""
	tk::TabToWindow $entry
	return -code break
    }
}

proc ::mylog::_search { w tree } {
    variable _search_id
    
    if { [info exists _search_id] } { after cancel $_search_id }
    set _search_id [after 500 [list ::mylog::_search_do $w $tree]]
}

proc ::mylog::_search_do { w tree } {
    variable _search_id
    unset _search_id

    if { ![winfo exists $w] } { return }
    
    set search [string trim [$w give_uservar_value search]]
    if { [string length $search] == 1 } { return }
	
    if { $search ne "" } {
	set searchList [$w give_uservar_value searchList]
	while { [llength $searchList] } {
	    if { [string match [glob_map [lindex $searchList end]]* $search] } {
		set searchList [lrange $searchList 0 end-1]
	    } else {
		break   
	    }
	}
	lappend searchList $search
	$w set_uservar_value searchList $searchList
	
	set searchList0 $search
	if { [catch { llength $searchList0 } ] } {
	    set searchList0 [textutil::splitx $search {\s+}]
	}
	set and_or and
	set searchList ""
	foreach s $searchList0 {
	    if { [string equal -nocase $s "and"] } {
		set and_or and
	    } elseif { [string equal -nocase $s "or"] } {
		set and_or or
	    } elseif { [regexp {^idx[<>=]\d+$} $s] } {
		lappend searchList $s
	    } elseif { [regexp {^([-+.\de]+),([-+.\de]+),([-+.\de]+)(:[-+.\de]+)?$} $s {} x y z delta] } {
		if { $delta eq "" } { set delta 2.0 }
		lappend searchList "(txt like '%$s%' or is_point_in_box($x,$y,$z,txt,$delta)==1)"
	    } else {
		lappend searchList "txt like '%$s%'"
	    }
	}
	$w set_uservar_value sql_search [join $searchList " $and_or "]
    } else {
	$w set_uservar_value sql_search ""
    }
    _view_log_fill $w
}

# proc ::mylog::_search_do_old { w tree } {
#     variable _search_id
#     unset _search_id
#     
#     set search [string trim [$w give_uservar_value search]]
#     if { [string length $search] == 1 } { return }
#         
#     if { $search eq "" } {
#         foreach item [$tree item children root] {
#             $tree item configure $item -visible 1
#         }
#     } else {
#         set searchList [$w give_uservar_value searchList]
#         while { [llength $searchList] } {
#             if { [string match [glob_map [lindex $searchList end]]* $search] } {
#                 set searchList [lrange $searchList 0 end-1]
#             } else {
#                 break   
#             }
#         }
#         lappend searchList $search
#         $w set_uservar_value searchList $searchList
#         
#         set searchList0 $search
#         if { [catch { llength $searchList0 } ] } {
#             set searchList0 [textutil::splitx $search {\s+}]
#         }
#         set and_or and
#         set searchList ""
#         foreach s $searchList0 {
#             if { [string equal -nocase $s "and"] } {
#                 set and_or and
#             } elseif { [string equal -nocase $s "or"] } {
#                 set and_or or
#             } else {
#                 lappend searchList $s
#             }
#         }
#         foreach item [$tree item children root] {
#             set num_found 0
#             foreach s $searchList {
#                 if { [lsearch -nocase -glob [$tree item text $item] *[glob_map $s]*] != -1 } {
#                     incr num_found
#                 }
#             }
#             set fail 0
#             if { $and_or eq "or" && $num_found == 0 } { set fail 1 }
#             if { $and_or eq "and" && $num_found < [llength $searchList] } { set fail 1 }
#             if { $fail } {
#                 $tree item configure $item -visible 0
#             } else {
#                 $tree item configure $item -visible 1
#             }
#         }
#     }
#     if { [llength [$tree selection get]] } {
#         $tree see [lindex [$tree selection get] 0]
#     }
# }

proc ::mylog::_view_log_fulltktree { args } {
    
    set optional {
	{ -extlogfile file "" }
	{ -parent parent_window "" }
	{ -clear_handler handler "" }
	{ -frame_toplevel frame_toplevel toplevel }
	{ -frame_grid_cmd cmd "" }
	{ -destroy_handler cmd "" }
	{ -topbuttons boolean 0 }
	{ -window w "" }
	{ -width size 540 }
	{ -height size 600 }
    }
    parse_args $optional "" $args

    package require sqlite3
    package require fulltktree
    package require dialogwin
    package require tooltip
    
    if { [info command mylog::edit-clear-16] eq "" } {
	image create photo mylog::edit-clear-16 -data {
	    R0lGODlhEAAQAPZvAHhIAHlJAHtKAHxLAKsbDaEkAKA2ALg4HYBOAYRSAYZSA4dkAIppAJx/AK1C
	    E7tKKKZpCqlrCqttC7BpF7JyDLJ2C7x6D8F9EMhtM5qFAJyIAJyLAJ2MAJ6NAJ6NAZ+NAJ+NAZ+O
	    AJ+OAZ+OBJ+PBaGIAaCOAaCPAaCPBaGQCKKQCaWUEaubGq6eG7GgHbulKb6uMsCwL8W0Msa1MMa2
	    M8W1Ns29Qcy9Q82+RdexYtrCA9vDBNzEB9zFENzGENzGFd/KJuPLEeXNFOfQGO7XI/PbKvbeL/vk
	    N/zlPP3mPM/BSdLDT9jIT9bIVtzNWN3PXuTSSeTTTOXUTu3aRubWVuHTW+fXW+fYX+LUZ+fZbeja
	    bOzcaOrccuvdd+ved/bYYfvlRP3pUv3qX/PjZf3rYf3qY/3rbP3sa/3sbf3uffvqhP3vhPjqiPvt
	    i/3viv///8zMzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5
	    BAEAAHAALAAAAAAQABAAAAexgAADAgBwhoeIhgIQEoSJjwIUFxECDBsbj4YBChYXCRmXmXAAAggV
	    JTAumKJwCxo2XhMGJxyriBseTFsYBDlcNyoitnAbHS8PB19lZmlsOCQhocQFDmpna2hhYGNdSi0c
	    hw0xbmJkYUlFQz5QWCCHGyttSEdGREI7OlQ0IYgmT1NDgvDQoSOKkxHDNrDQ0oPgDytNUAwzFKLG
	    FSBSssjoMNFQsSVVZqT40PEdhw4cEwUCADs=
	}
    }
    
    set d [cu::get_program_preferences mylog_view]
    set geometry [dict_getd $d geometry ""]
	    
    if { $window eq "" } {
	set window $parent.%AUTO%
    } else {
	destroy $window
    }
    set w [dialogwin_snit $window -title Log -class Log -okname - \
	    -cancelname [_ Close] -morebuttons [list [_ Clear] [_ Save] [_ Update]] \
	    -frame_toplevel $frame_toplevel -frame_grid_cmd $frame_grid_cmd \
	    -topbuttons $topbuttons \
	    -callback [list ::mylog::_view_log_fulltktree_do $args] -grab 0 \
	    -geometry $geometry]
	
    catch { wm protocol $w WM_DELETE_WINDOW [list mylog::_view_log_close $w] }
    bind $w <Destroy> [list mylog::_view_log_destroy $w $destroy_handler]
	
    set f [$w giveframe]
    
    cu::combobox $f.e1 -textvariable [$w give_uservar search ""] -valuesvariable [$w give_uservar searchList ""]
    
    tooltip::tooltip $f.e1 [_ "Examples: word1 and word2 idx>250"]
    ttk::menubutton $f.mb1 -text . -style Toolbutton -menu $f.mb1.m
    menu $f.mb1.m -tearoff 0
    $f.mb1.m add command -label [_ "View more entries"] -command \
	[list mylog::_view_log_more_lines $w]
    $f.mb1.m add command -label [_ "View end"] -command \
	[list mylog::_view_log_fill -clear_selection $w]
    $f.mb1.m add separator
    $f.mb1.m add checkbutton -label [_ "View all entries"] -variable [$w give_uservar view_all] -command \
	[list mylog::_view_log_fill $w]

    ttk::button $f.b1 -image mylog::edit-clear-16 -style Toolbutton -command [list ::mylog::_clear_search $w $f.e1]
    tooltip::tooltip $f.b1 [_ "Clear search entry (Backspace)"]
    bind $f.e1 <Delete> [list ::mylog::_keydelete_search $w $f.e1]
    
    $w add_trace_to_uservar search [list ::mylog::_search $w $f.t]
    set cols {
	{ 10 idx left item 0 }
	{ 15 date right text 0 }
	{ 12 delta_time right text 0 }
	{ 8 level left text 0 }
	{ 50 txt left text 0 }
	{ 8 proc left text 0 }
	{ 8 proc_args left text 0 }
    }
    fulltktree $f.t -columns $cols -width $width -height $height \
	-compass_background 1 -showlines 0 \
	-indent 0 -expand 0 -sensitive_cols all \
	-selecthandler2 [list mylog::_view_log_row] \
	-contextualhandler_menu [list mylog::_view_log_contextual_menu $f.t $w]
    
    $w set_uservar_value tree $f.t
    $w set_uservar_value extlogfile $extlogfile
    $w set_uservar_value clear_handler $clear_handler
    
    if { ![$w exists_uservar reduced_view] } {
	$w set_uservar_value reduced_view [dict_getd $d reduced_view 0]
    } 
    if { ![$w exists_uservar notes_view] } {
	$w set_uservar_value notes_view [dict_getd $d notes_view 0]
    }
    $w set_uservar_value draw_faces 0

    bind $w <F3> [list mylog::_view_log_check_reduced_view -toggle $w]
    bind $w <F4> [list mylog::_view_log_check_notes_view -toggle $w]
    bind $w <F5> [list mylog::_view_log_fill $w]
    bind $w <F1> [list mylog::_view_draw_box_in_gid_post $w ""]
    bind $w <Shift-F1> [list mylog::_view_log_draw_faces -toggle $w]
    bind $w <Control-u> [list draw_post::update_gui_redraw_all]
    bind $w <Shift-F2> [list mylog::_view_point_in_gid_post $w ""]
    bind $w <F2> [list mylog::_view_point_in_gid_post_zoomed $w ""]
    bind $w <BackSpace> [list ::mylog::_clear_search $w $f.e1]
    bind $f.e1 <BackSpace> "[bind [winfo class $f.e1] <BackSpace>] ; break"
    bind $w <Delete> [list ::mylog::_clear_search $w $f.e1]
    
    for { set i 1 } { $i <= 5 } { incr i } {
	bind $w <Control-KeyPress-$i> [bind $w <F$i>]
	bind $w <Control-Shift-KeyPress-$i> [bind $w <Shift-F$i>]
    }
 
    grid $f.e1 $f.mb1 $f.b1 -sticky ew -padx 2 -pady 2
    grid $f.t - - -sticky nsew -padx 2 -pady 2
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 1
    
    $w set_uservar_value sql_search ""
    $w set_uservar_value view_all 0
    $w set_uservar_value view_lines 200
    _view_log_fill $w
    _view_log_check_reduced_view $w
    _view_log_check_notes_view $w

    tk::TabToWindow $f.e1
    $w createwindow
    return $w
}

proc ::mylog::_view_log_close { w } {
    destroy $w
}

proc ::mylog::_view_log_destroy { w destroy_handler } {
    
    #if { ![winfo exists $w] } { return }
    
    if { $w ne [winfo toplevel $w] } { return }
    
    set tree [$w give_uservar_value tree]
    if { ![winfo exists $tree] } { return }
    
    set t [winfo parent $tree].notes

    set d [cu::get_program_preferences mylog_view]
    dict set d reduced_view [$w give_uservar_value reduced_view]
    dict set d notes_view [$w give_uservar_value notes_view]
    if { [winfo exists $t] } {
	dict set d notes [$t get 1.0 end]
    }
    dict set d geometry [cu::give_window_geometry $w]
    
    cu::store_program_preferences mylog_view $d
    
    if { $destroy_handler ne "" } {
	uplevel #0 $destroy_handler $w
    }
}

proc ::mylog::_view_log_contextual_menu { tree w - menu item itemList } {

    $menu add checkbutton -label [_ "Reduced view"] -acc F3 -variable [$w give_uservar reduced_view] \
	-command [list ::mylog::_view_log_check_reduced_view $w]
    $menu add checkbutton -label [_ "Notes view"] -acc F4 -variable [$w give_uservar notes_view] \
	-command [list ::mylog::_view_log_check_notes_view $w]
    $menu add separator
    $menu add command -label [_ "Update"] -acc F5 -command [list mylog::_view_log_fill $w]
    
    if { $item ne "" && [regexp {p0=(\S+)\s+L=(\S+)} [$tree item text $item 4]] } {
	$menu add separator
	$menu add command -label [_ "Draw box"] -acc F1 -command [list mylog::_view_draw_box_in_gid_post $w $itemList]
	$menu add command -label [_ "Draw faces"] -acc Shift-F1 -command [list mylog::_view_log_draw_faces -toggle $w]
	$menu add command -label [_ "Clear ALL draw box"] -acc Ctrl-u -command [list draw_post::update_gui_redraw_all]
	$menu add command -label [_ "Draw point (zoomed)"] -acc "F2" \
	    -command [list mylog::_view_point_in_gid_post_zoomed $w $itemList]
	$menu add command -label [_ "Draw point"] -acc "Shift-F2" -command [list mylog::_view_point_in_gid_post $w $itemList]
    }
    $menu add separator
    $menu add cascade -label [_ "Color"] -menu $menu.c
    
    set colors [list \
	    mango #e0d656 \
	    taronja #d2ab50 \
	    blau_palid #5ba7cf \
	    escuma_marina #ccffcc \
	    marro #805932 \
	    vermell #e53b1a \
	    ]
    set m [menu $menu.c -tearoff 0]
    foreach "n c" $colors {
	set img [image create photo img::$n -width 32 -height 16]
	$img put $c -to 0 0 31 15
	$m add command -image $img -command [list mylog::_view_log_change_color $c $w $itemList]
    }
    $m add separator
    $m add command -label [_ "Reset"] -command [list mylog::_view_log_change_color "" $w $itemList]
}

proc mylog::_view_log_change_color { color w itemList } {
    
    set tree [$w give_uservar_value tree]
    
    if { $itemList eq "" } {
	set itemList [$tree selection get]
    }

    foreach item $itemList {
	for { set col 0 } { $col < [$tree column count] } { incr col } {
	    foreach elm [$tree item style elements $item $col] {
		$tree item element configure $item $col $elm -fill $color
	    }
	}
    }
}

proc ::mylog::_view_point_in_gid_post { w itemList } {
    
    set tree [$w give_uservar_value tree]
    
    if { $itemList eq "" } {
	set itemList [$tree selection get]
    }

    foreach item $itemList {
	set txt [$tree item text $item 4]
	if { ![regexp {([-+.\deE]+),([-+.\deE]+),([-+.\deE]+)} $txt {} px py pz] } { continue }
	draw_post::draw_signal_point 0 [list $px $py $pz]
    }
}

proc ::mylog::_view_log_draw_faces_do { w what args } {
    
    switch $what {
	s {
	    set file [tk_getSaveFile -defaultextension .txt -filetypes \
		    [list [list [_ "txt files"] [list ".txt"]] \
		        [list [_ "All files"] [list "*"]]] \
		    -initialdir $::env(HOME) -initialfile faces.txt \
		    -parent $w -title [_ "Save faces"]]
	    if { $file eq "" } { return }
	    set fout [open $file w]
	    puts $fout [draw_post::draw_point_line_indicator give]
	    close $fout
	    return
	}
	r {
	    set file [tk_getOpenFile -defaultextension .txt -filetypes \
		    [list [list [_ "txt files"] [list ".txt"]] \
		        [list [_ "All files"] [list "*"]]] \
		    -initialdir $::env(HOME) -initialfile faces.txt \
		    -parent $w -title [_ "Open faces"]]
	    if { $file eq "" } { return }
	    set fin [open $file r]
	    draw_post::draw_point_line_indicator set [string trim [read $fin]]
	    close $fin
	    return
	}
    }
    
    if { ![$w exists_uservar last_drawn_entity] } {
	$w set_uservar_value last_drawn_entity ""
    }
    set last_drawn_entity [$w give_uservar_value last_drawn_entity]
    
    if { [lindex [dict_getd $last_drawn_entity entity ""] 0] in "point ariste face" } {
	set idx [lindex [dict get $last_drawn_entity entity] 1]
	if { $idx eq "" } { set idx 0 }
	set p0 [dict get $last_drawn_entity p0]
	set L [dict get $last_drawn_entity L]
	set point [m::add [m::scale 0.5 $L] $p0]

	if { $what eq "draw" } {
	    draw_post::draw_point_line_indicator -clear 1 box_axes $p0 $L [list face $idx]
	    draw_post::draw_point_line_indicator -toggle 1 -draw_arrow_to_center 0 \
		hidden_box $p0 $L [list face $idx] "1 0 1"
	    return
	}
	set move 1
	switch $what {
	    Left { lassign [list 0 prev] axe prev_next }
	    Right { lassign [list 0 next] axe prev_next }
	    Down { lassign [list 1 prev] axe prev_next }
	    Up { lassign [list 1 next] axe prev_next }
	    Prior { lassign [list 2 prev] axe prev_next }
	    Next { lassign [list 2 next] axe prev_next }
	    BM1 - SBM1 { 
		lassign $args x y
		if { ![$w exists_uservar last_mouse] } {
		    $w set_uservar_value last_mouse [list $x $y]
		    return
		}
		switch $what {
		    BM1 { set accepted_axes [list 0 1] }
		    SBM1 { set accepted_axes [list 2] }
		}
		lassign [$w give_uservar_value last_mouse] x0 y0
		set dist [expr {sqrt(($x-$x0)**2+($y-$y0)**2)}]
		if { $dist < 10 } { return }
		lassign [draw_post::give_object_axe_orient [list $x0 $y0 0] \
		        [list $x $y 0] $accepted_axes] axe prev_next
		$w set_uservar_value last_mouse [list $x $y]
	    }
	    default { set move 0 }
	}
	draw_post::draw_point_line_indicator -clear 1 box_axes $p0 $L [list face $idx]

    } elseif { [dict exists $last_drawn_entity point] } {
	set point [dict get $last_drawn_entity point]
	set move 0
    } else {
	# [_ "Select first a face with F1 or F2"]
	return
    }
    if { $move } {
	set moveL [list -move [list $idx $axe $prev_next]]
    } else {
	set moveL ""
    }
    lassign [cu::give_octree_near_boxes {*}$moveL $point] idx subbox
    if { $subbox eq "" } {
	tk_messageBox -message [_ "Octree not defined here"]
	return
    }
    
    if { $what in "0 1 2 3 4 5" } {
	set idx $what
    }
    if { $idx eq "" } {
	set idx 0
    }
    regexp {p0=(\S+)\s+L=(\S+)} $subbox {} p0 L
    set p0 [split $p0 ","]
    set L [split $L ","]
    set entList [list face $idx]
    draw_post::draw_point_line_indicator -toggle 1 -draw_arrow_to_center 0 box_axes \
	$p0 $L $entList
    $w set_uservar_value last_drawn_entity [list p0 $p0 L $L entity $entList]
}

proc ::mylog::_view_log_draw_faces { args } {

    set optional {
	{ -toggle "" 0 }
    }
    set compulsory "w"
    parse_args $optional $compulsory $args
    
    set tree [$w give_uservar_value tree]
    
    if { $toggle } {
	if { [$w give_uservar_value draw_faces] } {
	    $w set_uservar_value draw_faces 0
	} else {
	    $w set_uservar_value draw_faces 1
	}
    } else {
	$w set_uservar_value draw_faces 1
    }
    set t [winfo parent $tree].draw_faces

    if { [$w give_uservar_value draw_faces] } {
	if { ![winfo exists $t] } {
	    text $t -width 80 -height 6 -bg [$w cget -bg]
	    grid $t -sticky nsew -columnspan 3 -padx 2 -pady 2
	    
	    $t insert end [_ "Press arrows and prev/next page to move face"]\n
	    $t insert end [_ "Press mouse-1 and move mouse to move face in XY"]\n
	    $t insert end [_ "Press shift-mouse-1 and move mouse to move face in Z"]\n
	    $t insert end [_ "Press 0,1,2,3,4,5 to change face idx"]\n
	    $t insert end [_ "Press 'Return' to draw face"]\n
	    $t insert end [_ "Press 's' to save faces. 'r' to recover"]

	    $t configure -state disabled
	    bind $t <1> [list focus $t]
	    focus $t
	    
	    foreach i [list Left Right Up Down Prior Next 0 1 2 3 4 5 s r] {
		bind $t <KeyPress-$i> [list mylog::_view_log_draw_faces_do $w $i]
	    }
	    bind $t <Return> [list mylog::_view_log_draw_faces_do $w draw]
	    bind $t <B1-Motion> [list mylog::_view_log_draw_faces_do $w BM1 %x %y]
	    bind $t <Shift-B1-Motion> [list mylog::_view_log_draw_faces_do $w SBM1 %x %y]
	} else {
	    grid $t
	}
	focus $t
    } else {
	if { [winfo exists $t] } {
	    grid remove $t
	}
    }
}
proc ::mylog::_view_point_in_gid_post_zoomed { w itemList } {
    
    set tree [$w give_uservar_value tree]
    
    if { $itemList eq "" } {
	set itemList [$tree selection get]
    }
    set found 0
    foreach item $itemList {
	set txt [$tree item text $item 4]
	if { [regexp {([-+.\deE]+),([-+.\deE]+),([-+.\deE]+)} $txt {} px py pz] } {
	    set found 1
	    break
	}
    }
    if { !$found } { return }
    
    if { [$w exists_uservar last_drawn_entity] } {
	if { [$w give_uservar_value last_drawn_entity] eq [list point [list $px $py $pz]] } {
	    $w set_uservar_value last_drawn_entity [list point [list $px $py $pz] redraw 1]
	    draw_post::update_gui_redraw_all
	    return
	}
    }
    
    set xml {
	<gidpost_batch version='1.0'>
	<a n='change_mesh_properties' a='{*} onoff off' f='1'/>
	<a n='change_mesh_properties' a='{"line to//*" "shell//*"} onoff on' f='1'/>
	<a n='draw_full::set_view' pn='Set view' a='{ortho {%ORTHO%} rotation_center {%CENTER%}
	    rotation_vector {-0.09214 -0.434906 -0.863246 -0.239108}}' f='2'/>
	<a n='change_mesh_properties' a='{"shell//1"} colorRGBA {0.988 0.706 0.169 1.0}' f='1'/>
	</gidpost_batch>
    }
    set ortho {{-11.1777 -10.9342} {-0.129088 0.121778} {-1483.85 1633.85}}
    set rotation_center {-11.0492 0 -2.09955}

    package require compass_utils::math    
    set delta [m::sub [list $px $py $pz] $rotation_center]
    for { set i 0 } { $i < 3 } { incr i } {
	lset ortho $i 0 [expr {[lindex $ortho $i 0]+[lindex $delta $i]}]
	lset ortho $i 1 [expr {[lindex $ortho $i 1]+[lindex $delta $i]}]
    }
    set xml [string map [list %ORTHO% $ortho %CENTER% [list $px $py $pz]] $xml]
    draw_post::eval_batch_data $xml

    foreach item $itemList {
	set txt [$tree item text $item 4]
	if { ![regexp {([-+.\deE]+),([-+.\deE]+),([-+.\deE]+)} $txt {} px py pz] } { continue }
	draw_post::draw_signal_point 0 [list $px $py $pz]
	$w set_uservar_value last_drawn_entity [list point [list $px $py $pz]]
    }
}

proc ::mylog::_view_draw_box_in_gid_post { w itemList } {
    
    set tree [$w give_uservar_value tree]
    
    if { $itemList eq "" } {
	set itemList [$tree selection get]
    }
    if { ![$w exists_uservar last_drawn_entity] } {
	$w set_uservar_value last_drawn_entity ""
    }
    foreach item $itemList {
	set txt [$tree item text $item 4]
	if { ![regexp {p0=(\S+)\s+L=(\S+)} $txt {} p0 L] } { continue }
	
	set p0 [split $p0 ","]
	set L [split $L ","]
	if { [llength $L] == 1 } {
	    set L [lrepeat 3 $L]
	}
	set entList ""
	foreach i [list point ariste face] {
	    set rex [format {idx_%s[^=]*=(\d+)} $i]
	    if { [regexp $rex $txt {} num] } {
		lappend entList $i $num
	    }
	}
	draw_post::draw_point_line_indicator -toggle 1 -draw_arrow_to_center 0 box_axes $p0 $L $entList
	$w set_uservar_value last_drawn_entity [list p0 $p0 L $L entity [lrange $entList end-1 end]]
    }
}

proc ::mylog::_view_log_check_notes_view { args } {

    set optional {
	{ -toggle "" 0 }
    }
    set compulsory "w"
    parse_args $optional $compulsory $args
    
    set tree [$w give_uservar_value tree]
    
    if { $toggle } {
	if { [$w give_uservar_value notes_view] } {
	    $w set_uservar_value notes_view 0
	} else {
	    $w set_uservar_value notes_view 1
	}
    }
    set t [winfo parent $tree].notes

    if { [$w give_uservar_value notes_view] } {
	if { ![winfo exists $t] } {
	    text $t -width 80 -height 6
	    grid $t -sticky nsew -columnspan 2 -padx 2 -pady 2
	    
	    set d [cu::get_program_preferences mylog_view]
	    $t insert end [dict_getd $d notes ""]
	} else {
	    grid $t
	}
	focus $t
    } else {
	if { [winfo exists $t] } {
	    grid remove $t
	}
    }
}

proc ::mylog::_view_log_check_reduced_view { args } {
    
    set optional {
	{ -toggle "" 0 }
    }
    set compulsory "w"
    parse_args $optional $compulsory $args
    
    if { $toggle } {
	switch [$w give_uservar_value reduced_view] {
	    0 {  $w set_uservar_value reduced_view 1 }
	    1 {  $w set_uservar_value reduced_view 2 }
	    2 {  $w set_uservar_value reduced_view 0 }
	}
    }
    set tree [$w give_uservar_value tree]
    
    if {  [$w give_uservar_value reduced_view] != 1 } {
	set hidden_columns [list 0 1 2 3 5 6]
    } else {
	set hidden_columns [list 1 2 3 5 6]
    }
    if { [$w give_uservar_value reduced_view] } {
	foreach c $hidden_columns {
	    $tree column configure $c -visible 0
	}
	$tree column configure 4 -width 800
    } else {
	foreach c $hidden_columns {
	    $tree column configure $c -visible 1
	}
    }
}

proc ::mylog::_view_log_fulltktree_do { args w } {
	
    switch -- [$w giveaction] {
	-1 - 0 {
	    _view_log_close $w
	}
	2 {
	    set clear_handler [ $w give_uservar_value clear_handler]
	    if { $clear_handler ne "" } {
		uplevel #0 $clear_handler
	    }
	    set db [$w give_uservar_value db]
	    $db eval "delete from log"
	    destroy $w
	    ::mylog::_view_log_fulltktree {*}$args
	}
	3 {
	    set file [tk_getSaveFile -defaultextension .db3 -filetypes \
		    [list [list [_ "sqlite files"] [list ".db3"]] \
		        [list [_ "All files"] [list "*"]]] \
		    -parent $w -title [_ "Save log"]]
	    if { $file == "" } { return }
	    ::mylog::_write_log_to_file $file  
	}
	4 {
	    _view_log_fill $w
	}
    }
}

proc ::mylog::_format_time { time } {
    if { $time <= 120 } {
	return [format "%.3g s" $time]
    } elseif { $time < 120*60 } {
	return [format "%.3g min" [expr {$time/60.0}]]
	} else {
	return [format "%.3g hr" [expr {$time/3600.0}]]
    }
}

proc ::mylog::_create_time_tree { tree dict delta_parent parent } {

    lassign [list 0 0.0] num sum
    dict for "n v" $dict {
	set delta [dict_getd $v delta 0.0]
	set deltaP [_format_time $delta]   
	set sum [expr {$sum+$delta}]
	if { $delta_parent != 0 } {
	    set percent [format "%.3g %%" [expr {100.0*$delta/$delta_parent}]]
	} else {
	    set percent ""
	}
	set item [$tree insert end [list $n $deltaP $percent "" time] $parent]
	set num_children [_create_time_tree $tree [dict_getd $v children ""] $delta $item]
	if { $num_children } {
	    set font [lindex [$tree item element cget $item 0 e_text_sel -font] 0]
	    if { $font eq "" } { set font [$tree cget -font] }
	    $tree item element configure $item 0 e_text_sel -font [list [list {*}[font actual $font] -weight bold]]
	}
	incr num
    }
    if { $delta_parent > 0 && $num && ($delta_parent-$sum)/$delta_parent > 0.01 } {
	set percent [format "%.3g %%" [expr {100.0*($delta_parent-$sum)/$delta_parent}]]
	set item [$tree insert end [list [_ "other operations"] [_format_time [expr {$delta_parent-$sum}]] $percent] $parent]
	$tree item element configure $item 0 e_text_sel -fill green
    }
    return $num
}

proc ::mylog::_view_log_more_lines { w } {

    set view_lines [$w give_uservar_value view_lines]
    $w set_uservar_value view_lines [expr {$view_lines*2}]
    _view_log_fill $w
}

proc ::mylog::_view_log_fill { args } {
    
    set optional {
	{ -clear_selection "" 0 }
    }
    set compulsory "w"
    parse_args $optional $compulsory $args

    set tree [ $w give_uservar_value tree]
    set extlogfile [ $w give_uservar_value extlogfile]

    if { $extlogfile ne "" } {
	set err [catch { sqlite3 ::mylog::db10 $extlogfile } ret]
	if { $err } {
	    error [_ "Cound not open log file '%s' (%s)" $extlogfile $ret]
	}
	set db ::mylog::db10
	::mylog::db10 timeout 3000
	$w disablebutton 2
    } else {
	set db ::mylog::db
    }
    
    set separate_time 1
    
    set selected_idx ""
    if { !$clear_selection } {
	foreach item [$tree selection get] {
	    lappend selected_idx [$tree item text $item 0]
	}
    }

    set num [$db eval "select coalesce(max(idx),0) from log"]
    
    set sql_search [$w give_uservar_value sql_search]
    if { $sql_search eq "" } {
	set view_lines [$w give_uservar_value view_lines]
	if { [$w give_uservar_value view_all] == 1 } {
	    set sql "select * from log order by idx"
	} elseif { [llength $selected_idx] } {
	    set idx [lindex $selected_idx 0]
	    set sql "select * from log where idx>[expr {$idx-$view_lines}] and idx<[expr {$idx+$view_lines}] order by idx"
	} else {
	    set sql "select * from log where idx>[expr {$num-$view_lines}] order by idx"
	}
    } else {
	set sql "select * from log where $sql_search order by idx"
    }
    lassign "" times_dict keyList
    $tree item delete all
    $db eval $sql {
	if { $separate_time && $level eq "time_start" } {
	    if { $keyList eq "" } {
		set keyList [list $txt]
	    } else {
		lappend keyList children $txt
	    }
	    regexp {(.*)\.(.*)} $date {} d frac
	    set date [clock scan $d].$frac
	    dict set times_dict {*}$keyList start_date $date
	} elseif { $separate_time && $level eq "time_end" } {
	    regexp {(.*)\.(.*)} $date {} d frac
	    set date [clock scan $d].$frac
	    set start_date [dict get $times_dict {*}$keyList start_date]
	    set delta [expr {[dict_getd $times_dict {*}$keyList delta 0.0]+$date-$start_date}]
	    dict set times_dict {*}$keyList delta $delta
	    dict unset times_dict {*}$keyList start_date
	    set keyList [lrange $keyList 0 end-2]
	} else {
	    set item [$tree insert end [list $idx $date $delta_time $level $txt $proc $proc_args]]
	    if { $idx in $selected_idx } {
		$tree selection add $item
		$tree activate $item
	    }
	}
    }
    _create_time_tree $tree $times_dict 0 0
    
    if { [llength $selected_idx] } {
	$tree see active
    } else {
	$tree see end
    }

    if { $extlogfile ne "" } {
	$db close
    } else {
	$w set_uservar_value db $db
    }
#     if { [ $w give_uservar_value search] ne "" } {
#         $w eval_uservar_traces search
#     }
}

proc ::mylog::_view_log_row { tree ids } {

    set id [lindex $ids 0]
    set w [dialogwin_snit $tree.%AUTO% -title [_ "Log"] -okname -]
    set f [$w giveframe]
    
    set text [text $f.t -xscrollcommand [list $f.sh set] -yscrollcommand \
	    [list $f.sv set]]
    scrollbar $f.sv -orient vertical -command [list $f.t yview]
    scrollbar $f.sh -orient horizontal -command [list $f.t xview]

    set idx 0
    foreach col [[winfo parent $tree] cget -columns] {
	$text insert end "[lindex $col 1]: " bold
	$text insert end [$tree item text $id $idx]\n\n
	incr idx
    }
    $text tag configure bold -font [concat [font actual [$f.t cget -font]] \
	    -weight bold]
    $f.t configure -state disabled
    bind $f.t <1> [list focus $f.t]

    grid $f.t $f.sv -sticky wns
    grid $f.sh -sticky new

    grid configure $f.t -sticky nsew
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 0 -weight 1
    $w createwindow
    destroy $w
}

################################################################################
#    instrument times in procs for debugging
################################################################################

namespace eval ::stime {
    variable tree_times
}

proc ::stime::init {} {
    variable tree_times

    package require struct::tree
    set tree_times [::struct::tree]
}

proc ::stime::register { args } {

    set cmd {
	foreach "procname text" %ARGS% {
	    set ns [namespace which -command $procname]
	    set newprocname [namespace qualifiers $ns]::_[namespace tail $procname]
	    if { [info command $newprocname] eq "" } {
		if { [info command $procname] eq "" } {
		    error "error in stime::register. proc '$procname' does not exist"
		}
		rename $procname $newprocname
		proc $procname { args } "stime::add_time \" $newprocname \$args \" {$text} 2"
	    }
	}
    }
    uplevel 1 [string map [list %ARGS% [list $args]] $cmd]
}

proc ::stime::add_time { script namestack { level 1 } } {
    variable tree_times

    set ret [time { set retval [uplevel $level $script] }]
    set t [expr {[lindex $ret 0]*1e-6}]

    set cnode root
    foreach n $namestack {
	set found 0
	foreach node [$tree_times children $cnode] {
	    if { [$tree_times get $node name] eq $n } {
		set found 1
		break
	    }
	}
	if { $found } {
	    set cnode $node
	} else {
	    set cnode [$tree_times insert $cnode end]
	    $tree_times set $cnode name $n
	}
    }
    if { ![$tree_times keyexists $cnode t] } {
	$tree_times set $cnode t $t
    } else {
	$tree_times set $cnode t [expr {$t+[$tree_times set $cnode t]}]
    }
    $tree_times set $cnode userdef 1
    return $retval
}

proc ::stime::print { { channel "" } } {
    variable tree_times

    set _ ""
    $tree_times walk root -type bfs -order post i {
	set parent [$tree_times parent $i]
	if { $i eq "root" || $parent eq "root" } { continue }
	if { ![$tree_times keyexists $parent userdef] } {
	    if { ![$tree_times keyexists $parent t] } {
		set tp 0.0
	    } else {
		set tp [$tree_times set $parent t]
	    }
	    $tree_times set $parent t [expr {$tp+[$tree_times set $i t]}]
	}
    }
    $tree_times walk root i {
	if { $i eq "root" } { continue }
	set parent [$tree_times parent $i]
	if { $parent eq "root" } {
	    set percent 100
	} else {
	    set percent [expr {[$tree_times set $i t]*100.0/[$tree_times set $parent t]}]
	}
	append _ [_print_line [$tree_times depth $i] [$tree_times set $i name] \
		      [$tree_times set $i t] $percent]
    }
    if { $channel ne "" } { puts $channel $_ }
    return $_
}

proc ::stime::_print_line { level name time percent } {

    set _ [string repeat ..... [expr {$level-1}]]
    append _ [format "%12s %8.3fmin = %8.3fs (%3.2f%%)" $name [expr {$time/60.0}] \
		  $time $percent] "\n"
    return $_
}

################################################################################
#    parse args
#
# example:
#  proc myproc { args } {
#     set optional {
#         { -view_binding binding "" }
#         { -file file "" }
#         { -restart_file boolean 0 }
#         { -flag1 "" 0 }
#     }
#     set compulsory "levels"
#     parse_args $optional $compulsory $args
#
#     if { $view_binding ne "" } { puts hohoho }
#     if { $flag1 } { puts "activated flag" }
#  }
# 
################################################################################

proc ::parse_args { args } {

    set optional {
	{ -raise_compulsory_error boolean 1 }
	{ -compulsory_min min_number "" }
    }
    set compulsory "optional compulsory arguments"

    if { [info level] == 1 } {
	set cmdname [list [info nameofexecutable]]
	if { ![info exists ::starkit::topdir] } {
	    lappend cmdname $::argv0
	}
    } else {
	set cmdname [lindex [info level [expr {[info level]-1}]] 0]
    }
    if { [string match -* [lindex $args 0]] } {
	parse_args $optional $compulsory $args
    } else {
	set raise_compulsory_error 1
	set compulsory_min ""
	if { [llength $args] != [llength $compulsory] } {
	    uplevel 1 [list error [_parse_args_string $cmdname $optional \
		        $compulsory $arguments]]
	    return ""
	}
	foreach $compulsory $args break
    }

    foreach i $optional {
	lassign $i name namevalue default
	set opts_value($name) $namevalue
	if { [llength $i] > 2 } {
	    set opts($name) $default
	}
    }
    while { [string match -* [lindex $arguments 0]] && (!$raise_compulsory_error || 
	[llength $arguments] > [llength $compulsory]) } {
	if { [lindex $arguments 0] eq "--" } {
	    set arguments [lrange $arguments 1 end]
	    break
	}
	foreach "name value" [lrange $arguments 0 1] break
	if { [regexp {(.*)=(.*)} $name {} name value] } {
	    set has_att_value 1
	} else {
	    set has_att_value 0
	}
	if { [info exists opts_value($name)] } {
	    if { $has_att_value } {
		set opts($name) $value
		set arguments [lrange $arguments 1 end]
	    } elseif { $opts_value($name) eq "" } {
		set opts($name) 1
		set arguments [lrange $arguments 1 end]
	    } else {
		set opts($name) $value
		set arguments [lrange $arguments 2 end]
	    }
	} else {
	    uplevel 1 [list error [_parse_args_string $cmdname $optional \
		        $compulsory $arguments]]
	    return ""
	}
    }
    if { $raise_compulsory_error } {
	if { $compulsory_min ne "" } {
	    if { [llength $arguments] < $compulsory_min || \
		[llength $arguments] > [llength $compulsory] } {
		uplevel 1 [list error [_parse_args_string $cmdname $optional $compulsory $arguments]]
		return ""
	    }
	} elseif { [llength $arguments] != [llength $compulsory] } {
	    uplevel 1 [list error [_parse_args_string $cmdname $optional \
		        $compulsory $arguments]]
	    return ""
	}

    }
    foreach name [array names opts] {
	uplevel 1 [list set [string trimleft $name -] $opts($name)]
    }
    set inum 0
    foreach i $compulsory {
	uplevel 1 [list set $i [lindex $arguments $inum]]
	incr inum
    }
    return [lrange $arguments $inum end]
}

proc ::_parse_args_string { cmd optional compulsory arguments } {

    set str "error. usage: $cmd "
    foreach i $optional {
	lassign $i name namevalue default
	if { $namevalue ne "" } {
	    append str "?$name $namevalue? "
	} else {
	    append str "?$name? "
	}
    }
    append str $compulsory
    append str "\n\targs: '$cmd $arguments'"
    return $str
}

proc ::parse_args_create_cmd { optional compulsory } {

    set frame [info frame -1]
    if { ![dict exists $frame proc] } { return "" }
    set cmdname [dict get $frame proc]
    set cmdname2 [dict get $frame cmd]
    set cmd [list $cmdname]
    
    foreach i $optional {
	lassign $i name namevalue default
	set n [string trimleft $name "-"]
	set v [uplevel 1 set $n]
	if { $namevalue eq "" } {
	    if { $v } {
		lappend cmd $name
	    }
	} elseif { $v ne $default } {
	    lappend cmd $name $v
	}
    }
    foreach i $compulsory {
	set v [string trimleft $i "-"]
	lappend cmd [uplevel 1 set $v]
    }
    return $cmd
}

################################################################################
#    XML & xpath utilities
#
################################################################################

proc xml_map { args } {
    set map [list > "&gt;" < "&lt;" & "&amp;" \" "&quot;" ' "&apos;"]
    return [string map $map [join $args ""]]
}

proc xml_map_inv { args } {
    set map [list "&gt;" > "&lt;" < "&amp;" & "&quot;" \" "&apos;" ']
    return [string map $map [join $args ""]]
}

proc xml_map1 { args } {
    set map [list > "&gt;" < "&lt;" & "&amp;"]
    return [string map $map [join $args ""]]
}

proc xml_map1_inv { args } {
    set map [list "&gt;" > "&lt;" < "&amp;" &]
    return [string map $map [join $args ""]]
}

proc js_map { args } {
    set map [list \" \\\" ' \\' & \\&]
    return [string map $map [join $args ""]]
}

proc glob_map { args } {
    set map {
	? \\?
	* \\*
	\[ \\\[
	\] \\\]
	\\ \\\\
	\{ \\\{
	\} \\\}
    }
    return [string map $map [join $args ""]]
}

proc xpath_str { str } {
    
    foreach "strList type pos" [list "" "" 0] break
    while 1 {
	switch $type {
	    "" {
		set ret [regexp -start $pos -indices {['"]} $str idxs]
		if { !$ret } {
		    lappend strList "\"[string range $str $pos end]\""
		    break
		}
		set idx [lindex $idxs 0]
		switch -- [string index $str $idx] {
		    ' { set type apostrophe }
		    \" { set type quote }
		}
	    }
	    apostrophe {
		set ret [regexp -start $pos -indices {["]} $str idxs]
		if { !$ret } {
		    lappend strList "\"[string range $str $pos end]\""
		    break
		}
		set idx [lindex $idxs 0]
		lappend strList "\"[string range $str $pos [expr {$idx-1}]]\""
		set type quote
		set pos $idx
	    }
	    quote {
		set ret [regexp -start $pos -indices {[']} $str idxs]
		if { !$ret } {
		    lappend strList "'[string range $str $pos end]'"
		    break
		}
		set idx [lindex $idxs 0]
		lappend strList "'[string range $str $pos [expr {$idx-1}]]'"
		set type apostrophe
		set pos $idx
	    }
	}
    }
    if { [llength $strList] > 1 } {
	return "concat([join $strList ,])"
    } else {
	return [lindex $strList 0]
    }
}

proc format_xpath { string args } {
    set cmd [list format $string]
    foreach i $args {
	lappend cmd [xpath_str $i]
    }
    return [eval $cmd]
}

proc format_xml { string args } {
    set cmd [list format $string]
    foreach i $args {
	lappend cmd [xml_map $i]
    }
    return [eval $cmd]
}

proc format_xml1 { string args } {
    set cmd [list format $string]
    foreach i $args {
	lappend cmd [xml_map1 $i]
    }
    return [eval $cmd]
}

proc cu::xml_correct_incorrect_characters { xml } {
   set chars "\u1\u2\u3\u4\u5\u6\u7\u8\ub\uc\ue\uf\u10\u11\u12\u13\u14\u15\u16\u17\u18\u19\u1a\u1b\u1c\u1d\u1e\u1f"

    set map ""
    foreach i [split $chars ""]  {
	lappend map $i ""
    }
    return [string map $map $xml]
}

namespace eval ::dom::domNode {}

# args can be one or more tags
proc ::dom::domNode::appendChildTag { node args } {
    if { [::llength $args] == 0 } {
	error "error in appendChildTag. At list one tag"
    }
    ::set doc [$node ownerDocument]
    foreach tag $args {
	if { [string match "text() *" $tag] } {
	    ::set newnode [$doc createTextNode [lindex $tag 1]]
	    $node appendChild $newnode
	    ::set node $newnode
	} elseif { [string match "attributes() *" $tag] } {
	    foreach "n v" [lrange $tag 1 end] {
		$node setAttribute $n $v
	    }
	} else {
	    ::set newnode [$doc createElement $tag]
	    $node appendChild $newnode
	    ::set node $newnode
	}
    }
    return $newnode
}

# args can be one or more tags
proc ::dom::domNode::appendChildTagNS { node url args } {
    if { [::llength $args] == 0 } {
	error "error in appendChildTagNS. At list one tag"
    }
    ::set doc [$node ownerDocument]
    foreach tag $args {
	if { [string match "text() *" $tag] } {
	    ::set newnode [$doc createTextNode [lindex $tag 1]]
	    $node appendChild $newnode
	    ::set node $newnode
	} elseif { [string match "attributes() *" $tag] } {
	    foreach "n v" [lrange $tag 1 end] {
		$node setAttribute $n $v
	    }
	} else {
	    ::set newnode [$doc createElementNS $url $tag]
	    $node appendChild $newnode
	    ::set node $newnode
	}
    }
    return $newnode
}

proc ::dom::domNode::appendChildText { node text } {
    ::set doc [$node ownerDocument]
    foreach child [$node selectNodes text()] { $child delete }
    ::set newnode [$doc createTextNode $text]
    $node appendChild $newnode
    return $newnode
}

proc ::dom::domNode::insertAfter { node newChild refChild } {

    ::set nextSibling [$refChild nextSibling]
    if { $nextSibling ne "" } {
	return [$node insertBefore $newChild $nextSibling]
    } else {
	return [$node appendChild $newChild]
    }
}

proc ::dom::domNode::insertFirst { node newChild } {
    ::set child0 [$node firstChild]
    if { $child0 ne "" } {
	return [$node insertBefore $newChild $child0]
    } else {
	return [$node appendChild $newChild]
    }
}

proc ::dom::domNode::insertFirstTag { node tag } {
    ::set child0 [$node firstChild]
    if { $child0 ne "" } {
	return [$node insertBeforeTag $tag $child0]
    } else {
	return [$node appendChildTag $tag]
    }
}

proc ::dom::domNode::insertBeforeTag { node tag refChild } {
    ::set doc [$node ownerDocument]
    ::set newnode [$doc createElement $tag]
    $node insertBefore $newnode $refChild
    return $newnode
}

proc ::dom::domNode::insertAfterTag { node tag refChild } {
    ::set doc [$node ownerDocument]
    ::set newnode [$doc createElement $tag]
    $node insertAfter $newnode $refChild
    return $newnode
}

proc ::dom::domNode::appendBeforeXML { node xml refChild } {
    $node appendXML $xml
    ::set newnode [$node lastChild]
    $node insertBefore $newnode $refChild
    return $newnode
}

proc ::dom::domNode::appendFirstXML { node xml } {
    ::set child0 [$node firstChild]
    $node appendXML $xml        
    ::set newnode [$node lastChild]
    if { $child0 ne "" } {
	$node insertBefore $newnode $child0
    }
    return $newnode
}

################################################################################
#    compatibility functions
#
################################################################################

if { [package vcompare 8.5 [package present Tcl]] > 0 } {
    namespace eval ::cu::compatibility8.4 {}
    proc ::cu::compatibility8.4::go {} {
	namespace eval ::cu::compatibility8.4 {}
	catch { rename ::lsearch ::cu::compatibility8.4::lsearch }
	
	
	proc ::lsearch { args } {
	    set index -1
	    for { set i 0 } { $i < [llength $args] } { incr i } {
		switch -- [lindex $args $i] {
		    -index {
		        set index [lindex $args [expr {$i+1}]]
		        set args [lreplace $args $i [expr {$i+1}]]
		        break
		    }
		    -nocase {
		        lset args end-1 [string tolower [lindex $args end-1]]
		        lset args end [string tolower [lindex $args end]]
		        set args [lreplace $args $i $i]
		    }
		    -- { break }
		}
	    }
	    if { $index >= 0 } {
		set nlist ""
		foreach i [lindex $args end-1] {
		    lappend nlist [lindex $i $index]
		}
		lset args end-1 $nlist
		return [eval ::cu::compatibility8.4::lsearch $args]
	    } else {
		return [eval ::cu::compatibility8.4::lsearch $args]
	    }
	}
    }
    ::cu::compatibility8.4::go
}

if { [info command lrepeat] eq "" } {
    proc lrepeat {count value args} {
	set values [linsert $args 0 $value]
	set result {}
	for {set i 0} {$i < $count} {incr i} {
	    eval [list lappend result] $values
	}
	return $result
    }
}

################################################################################
#    store preferences
################################################################################

proc cu::store_program_preferences { args } {

    set optional {
	{ -valueName name "" }
    }
    set compulsory "program_name data"

    parse_args $optional $compulsory $args

    if { $valueName eq "" } {
	set valueNameF IniData
    } else {
	set valueNameF IniData_$valueName
    }

    if { $::tcl_platform(platform) eq "windows" && $::tcl_platform(os) ne "Windows CE" } {
	set key "HKEY_CURRENT_USER\\Software\\Compass\\$program_name"
	package require registry
	registry set $key $valueNameF $data
    } else {
	package require tdom
	if { $::tcl_platform(os) eq "Windows CE" } {
	    set dir [file join / "Application Data" Compass $program_name]
	    file mkdir $dir
	    set file [file join $dir prefs]
	} elseif { [info exists ::env(HOME)] } {
	    set file [file normalize ~/.compass_${program_name}_prefs]
	} else {
	    set file [file normalize [file join /tmp compass_${program_name}_prefs]]
	}
	set err [catch { tDOM::xmlReadFile $file } xml]
	if { $err } { set xml "<preferences/>" }
	set err [catch { dom parse $xml } doc]
	if { $err } {
	    set doc [dom parse "<preferences/>"]
	}
	set root [$doc documentElement]
	set domNode [$root selectNodes "pref\[@n=[xpath_str $valueNameF]\]"]
	if { $domNode ne "" } { $domNode delete }
	set p [$root appendChildTag pref]
	$p setAttribute n $valueNameF
	$p appendChildText $data

	set fout [open $file w]
	fconfigure $fout -encoding utf-8
	puts $fout [$doc asXML]
	close $fout
    }
}
proc cu::get_program_preferences { args } {

    set optional {
	{ -valueName name "" }
	{ -default default_value "" }
    }
    set compulsory "program_name"

    parse_args $optional $compulsory $args

    if { $valueName eq "" } {
	set valueNameF IniData
    } else {
	set valueNameF IniData_$valueName
    }

    set data $default
    if { $::tcl_platform(platform) eq "windows" && $::tcl_platform(os) ne "Windows CE" } {
	set key "HKEY_CURRENT_USER\\Software\\Compass\\$program_name"
	package require registry
	set err [catch { registry get $key $valueNameF } data]
	if { $err } {
	    set data $default
	}
    } else {
	package require tdom
	if { $::tcl_platform(os) eq "Windows CE" } {
	    set dir [file join / "Application Data" Compass $program_name]
	    file mkdir $dir
	    set file [file join $dir prefs]
	} elseif { [info exists ::env(HOME)] } {
	    set file [file normalize ~/.compass_${program_name}_prefs]
	} else {
	    set file [file normalize [file join /tmp compass_${program_name}_prefs]]
	}
	set err [catch { tDOM::xmlReadFile $file } xml]
	if { !$err } {
	    set err [catch { dom parse $xml } doc]
	    if { !$err } {
		set root [$doc documentElement]
		set domNode [$root selectNodes "pref\[@n=[xpath_str $valueNameF]\]"]
		if { $domNode ne "" } {
		    set data [$domNode text]
		}
	    }
	}
    }
    return $data
}

################################################################################
#    zip and unzip files
################################################################################

proc cu::zipfile { args } {
    
    # this is for backwards compatibility
    set nargs $args
    set args ""
    foreach i $nargs {
	switch -regexp -- $i {
	    {^-r$} { lappend args -recursive }
	    {^-([0-9])$} {
		regsub -- {^-([0-9])$} $i {-compression \1} i
		lappend args {*}$i
	    }
	    default { lappend args $i }
	}
    }

    set optional {
	{ -recursive "" 0 }
	{ -compression 0|...|9 9 }
	{ -data "" 0 }
	{ -encoding name "" }
    }
    set compulsory "zipfile"
    set files [parse_args -raise_compulsory_error 0 \
	    $optional $compulsory $args]

    if { $data && $recursive } {
	error "error: options -data and -recursive cannot be used together in cu::zipfile"
    }

    if { $recursive } {
	for { set i 0 } { $i < [llength $files] } { incr i } {
	    if { [file isdirectory [lindex $files $i]] } {
		lappend files {*}[glob -nocomplain -dir [lindex $files $i] *]
	    }
	}
    }
    
    set h [cu::zip open $zipfile w]
    if { !$data } {
	foreach i $files {
	    if { [file isdirectory $i] } { continue }
	    cu::zip set $h $i -level $compression -time [file mtime $i]
	    set fin [open $i r]
	    fconfigure $fin -translation binary
	    if { $encoding ni [list "" "binary"] } {
		cu::zip write $h [encoding convertto $encoding [read $fin]]
	    } else {
		cu::zip write $h [read $fin]
	    }
	    close $fin
	}
    } else {
	set now [clock seconds]
	foreach i $files {
	    lassign $i name dataL comment
	    cu::zip set $h $name -level $compression -time $now -comment $comment
	    if { $encoding ni [list "" "binary"] } {
		set dataL [encoding convertto $encoding $dataL]
	    }
	    cu::zip write $h $dataL
	}
    }
    cu::zip close $h
}

proc cu::unzipfile { args } {
    
    # this is for backwards compatibility
    set nargs $args
    set args ""
    foreach i $nargs {
	if { $i eq "-o" } {
	    lappend args -overwrite always
	} else {
	    lappend args $i
	}
    }

    set optional {
	{ -overwrite ask|always|never ask }
	{ -data "" 0 }
	{ -list "" 0 }
	{ -encoding name "" }
    }
    set compulsory "zipfile"
    parse_args $optional $compulsory $args
    
    if { $data && $list } {
	error "error: options -data and -list cannot be used together in cu::unzipfile"
    }
    
    set retval ""
    set h [cu::zip open $zipfile r]
    if { $list } {
	foreach file [cu::zip files $h] {
	    lassign [cu::zip info $h $file] mtime csize size att comment
	    lappend retval [list $file $mtime]
	}
    } elseif { !$data } {
	foreach file [cu::zip files $h] {
	    lassign [cu::zip info $h $file] mtime csize size att comment
	    if { ![file exists [file dirname $file]] } { file mkdir [file dirname $file] }
	    if { [file exists $file] && $overwrite ne "always" } {
		if { $overwrite eq "ask" } {
		    package require Tk
		    package require dialogwin
		    set w [dialogwin_snit .ask -okname [_ Yes] -morebuttons \
		            [list [_ "Yes to all"] No [_ "No to all"]] -entrytext \
		            [_ "Are you sure to overwrite file '%s'?" $file]]
		    set action [$w createwindow]
		    destroy $w
		    switch -- $action {
		        -1 - 0 {
		            cu::zip close $h
		            return
		        }
		        1 { #nothing }
		        2 { set overwrite always }
		        3 { continue }
		        4 { set overwrite never ; continue }
		    }
		} elseif { $overwrite eq "never" } { continue }
	    }
	    cu::zip set $h $file
	    set fout [open $file w]
	    fconfigure $fout -translation binary
	    if { $encoding ni [list "" "binary"] } {
		set data [encoding convertfrom $encoding [cu::zip read $h]]
	    } else {
		set data [cu::zip read $h]
	    }
	    puts -nonewline $fout $data
	    close $fout
	    file mtime $file $mtime
	    lappend retval [list $file $mtime]
	}
    } else {
	foreach file [cu::zip files $h] {
	    lassign [cu::zip info $h $file] mtime csize size att comment
	    cu::zip set $h $file
	    if { $encoding ni [list "" "binary"] } {
		set data [encoding convertfrom $encoding [cu::zip read $h]]
	    } else {
		set data [cu::zip read $h]
	    }
	    lappend retval [list $file $mtime $data $comment]
	}
    }
    cu::zip close $h
    return $retval
}

################################################################################
#    Database & sqlite utilities
################################################################################

proc cu::dump_sqlite_db_to_file { db filename } {

    if { $filename eq "" } {
	set filename [tk_getSaveFile -defaultextension .db3 \
		-filetypes [list [list [_ "sqlite3 files"] {.db3}] \
		    [list [_ "All files"] *]]]
	if { $filename eq "" } { return }
    }
    file delete $filename
    sqlite3 other $filename
    # WARNING: and type='table' should not be here but if not, sometimes a unique
    # constraint failed
    $db eval {SELECT sql FROM sqlite_master WHERE sql NOT NULL and type='table'} {
	other eval $sql
    }
    other close
    
    $db eval "ATTACH '$filename' AS app"
    $db eval {SELECT name FROM sqlite_master WHERE type='table'} {
	$db eval "INSERT INTO app.$name SELECT * FROM main.$name"
    }
    $db eval {DETACH app}
}

proc cu::sql_patterns_list { args } {
    
    set optional {
	{ -sqlite_escape "" 0 }
	{ -one_pattern "" 0 }
    }
    set compulsory "searchstring"
    parse_args $optional $compulsory $args

    # these are the accented letters
#     set groups [list a\ue1\ue2\ue3\ue4\ue5 e\ue8\ue9\uea\ueb \
#             i\uec\ued\uee\uef o\uf2\uf3\uf4\uf5\uf6 u\ufa\ufb\ufc]
#     set map ""
#     foreach group $groups {
#         lappend map [string index $group 0] "\[$group\]"
#     }
    set patfull [string map [list \\ \\\\ % \\% _ \\_] $searchstring]
    set likeList ""
    if { $one_pattern } {
	set list [list - [string trim $patfull \"] -]
    } else {
	set list [regexp -inline -all {\"([^\"]+)\"|([^\s\"]+)} $patfull]
    }
    foreach "- pat1 pat2" $list {
	if { $pat1 ne "" } {
	    set pat $pat1
	} else {
	    set pat $pat2
	}
#         set pat [string map $map $pat]

	if { $sqlite_escape } {
	    set pat [string map [list ' ''] $pat]
	}
	lappend likeList "%$pat%"
    }
    return $likeList
}

proc cu::string::compare_string { str } {
    
    set groups [list a\ue1\ue2\ue3\ue4\ue5 e\ue8\ue9\uea\ueb \
	    i\uec\ued\uee\uef o\uf2\uf3\uf4\uf5\uf6 u\ufa\ufb\ufc]
    
    set maps ""
    foreach group $groups {
	set rest [lassign [split $group ""] base]
	foreach i $rest {
	    lappend map $i $base
	}
    }
    return [string map $maps [string tolower $str]]
}


################################################################################
#    cu::kill and cu::ps
################################################################################

# cu::kill is defined in c-code as:
#       cu::kill ?-pgroup? ?signal? idlist
# to specify a signal does only work on unix

proc cu::ps { args } {

    if { $::tcl_platform(platform) eq "windows" } {
	package require compass_utils::c
	return [cu::_ps_win {*}$args]
    } else {
	# does not do exactly the same than in Windows
	#set err [catch { exec pgrep -l -f [lindex $args 0] } ret]
	#set retList  [split $ret \n]
	lassign $args pattern
	if { $pattern eq "" } {
	    set err [catch { exec ps -u $::env(USER) --no-headers -o pid,stime,time,size,cmd } ret]
	} else {
	    set err [catch { exec ps -u $::env(USER) --no-headers -o pid,stime,time,size,cmd | grep -i $pattern } ret]
	}        
	if { $err } {
	    return ""
	} else {
	    set retList ""
	    foreach line [split $ret \n] {
		regexp {(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)} $line {} pid stime cputime size cmd
		if { $pattern ne "" && $cmd eq "grep -i $pattern" } { continue }
		lappend retList [list $cmd $pid $stime $cputime $size]
	    }
	    return $retList
	}
    }
}

################################################################################
#    mail and mime
################################################################################

namespace eval cu::mail {
    set encList [list \
	    ascii US-ASCII \
	    big5 Big5 \
	    cp1250 Windows-1250 \
	    cp1251 Windows-1251 \
	    cp1252 Windows-1252 \
	    cp1253 Windows-1253 \
	    cp1254 Windows-1254 \
	    cp1255 Windows-1255 \
	    cp1256 Windows-1256 \
	    cp1257 Windows-1257 \
	    cp1258 Windows-1258 \
	    cp437 IBM437 \
	    cp737 "" \
	    cp775 IBM775 \
	    cp850 IBM850 \
	    cp852 IBM852 \
	    cp855 IBM855 \
	    cp857 IBM857 \
	    cp860 IBM860 \
	    cp861 IBM861 \
	    cp862 IBM862 \
	    cp863 IBM863 \
	    cp864 IBM864 \
	    cp865 IBM865 \
	    cp866 IBM866 \
	    cp869 IBM869 \
	    cp874 "" \
	    cp932 "" \
	    cp936 GBK \
	    cp949 "" \
	    cp950 "" \
	    dingbats "" \
	    ebcdic "" \
	    euc-cn EUC-CN \
	    euc-jp EUC-JP \
	    euc-kr EUC-KR \
	    gb12345 GB12345 \
	    gb1988 GB1988 \
	    gb2312 GB2312 \
	    iso2022 ISO-2022 \
	    iso2022-jp ISO-2022-JP \
	    iso2022-kr ISO-2022-KR \
	    iso8859-1 ISO-8859-1 \
	    iso8859-2 ISO-8859-2 \
	    iso8859-3 ISO-8859-3 \
	    iso8859-4 ISO-8859-4 \
	    iso8859-5 ISO-8859-5 \
	    iso8859-6 ISO-8859-6 \
	    iso8859-7 ISO-8859-7 \
	    iso8859-8 ISO-8859-8 \
	    iso8859-9 ISO-8859-9 \
	    iso8859-10 ISO-8859-10 \
	    iso8859-13 ISO-8859-13 \
	    iso8859-14 ISO-8859-14 \
	    iso8859-15 ISO-8859-15 \
	    iso8859-16 ISO-8859-16 \
	    jis0201 JIS_X0201 \
	    jis0208 JIS_C6226-1983 \
	    jis0212 JIS_X0212-1990 \
	    koi8-r KOI8-R \
	    koi8-u KOI8-U \
	    ksc5601 KS_C_5601-1987 \
	    macCentEuro "" \
	    macCroatian "" \
	    macCyrillic "" \
	    macDingbats "" \
	    macGreek "" \
	    macIceland "" \
	    macJapan "" \
	    macRoman "" \
	    macRomania "" \
	    macThai "" \
	    macTurkish "" \
	    macUkraine "" \
	    shiftjis Shift_JIS \
	    symbol "" \
	    tis-620 TIS-620 \
	    unicode "" \
	    utf-8 UTF-8]
    
    variable encodings
    array set encodings $encList
    variable reversemap
    foreach {enc mimeType} $encList {
	if {$mimeType != ""} {
	    set reversemap([string tolower $mimeType]) $enc
	}
    } 
    
    set encAliasList [list \
	    ascii ANSI_X3.4-1968 \
	    ascii iso-ir-6 \
	    ascii ANSI_X3.4-1986 \
	    ascii ISO_646.irv:1991 \
	    ascii ASCII \
	    ascii ISO646-US \
	    ascii us \
	    ascii IBM367 \
	    ascii cp367 \
	    cp437 cp437 \
	    cp437 437 \
	    cp775 cp775 \
	    cp850 cp850 \
	    cp850 850 \
	    cp852 cp852 \
	    cp852 852 \
	    cp855 cp855 \
	    cp855 855 \
	    cp857 cp857 \
	    cp857 857 \
	    cp860 cp860 \
	    cp860 860 \
	    cp861 cp861 \
	    cp861 861 \
	    cp861 cp-is \
	    cp862 cp862 \
	    cp862 862 \
	    cp863 cp863 \
	    cp863 863 \
	    cp864 cp864 \
	    cp865 cp865 \
	    cp865 865 \
	    cp866 cp866 \
	    cp866 866 \
	    cp869 cp869 \
	    cp869 869 \
	    cp869 cp-gr \
	    cp936 CP936 \
	    cp936 MS936 \
	    cp936 Windows-936 \
	    iso8859-1 ISO_8859-1:1987 \
	    iso8859-1 iso-ir-100 \
	    iso8859-1 ISO_8859-1 \
	    iso8859-1 latin1 \
	    iso8859-1 l1 \
	    iso8859-1 IBM819 \
	    iso8859-1 CP819 \
	    iso8859-2 ISO_8859-2:1987 \
	    iso8859-2 iso-ir-101 \
	    iso8859-2 ISO_8859-2 \
	    iso8859-2 latin2 \
	    iso8859-2 l2 \
	    iso8859-3 ISO_8859-3:1988 \
	    iso8859-3 iso-ir-109 \
	    iso8859-3 ISO_8859-3 \
	    iso8859-3 latin3 \
	    iso8859-3 l3 \
	    iso8859-4 ISO_8859-4:1988 \
	    iso8859-4 iso-ir-110 \
	    iso8859-4 ISO_8859-4 \
	    iso8859-4 latin4 \
	    iso8859-4 l4 \
	    iso8859-5 ISO_8859-5:1988 \
	    iso8859-5 iso-ir-144 \
	    iso8859-5 ISO_8859-5 \
	    iso8859-5 cyrillic \
	    iso8859-6 ISO_8859-6:1987 \
	    iso8859-6 iso-ir-127 \
	    iso8859-6 ISO_8859-6 \
	    iso8859-6 ECMA-114 \
	    iso8859-6 ASMO-708 \
	    iso8859-6 arabic \
	    iso8859-7 ISO_8859-7:1987 \
	    iso8859-7 iso-ir-126 \
	    iso8859-7 ISO_8859-7 \
	    iso8859-7 ELOT_928 \
	    iso8859-7 ECMA-118 \
	    iso8859-7 greek \
	    iso8859-7 greek8 \
	    iso8859-8 ISO_8859-8:1988 \
	    iso8859-8 iso-ir-138 \
	    iso8859-8 ISO_8859-8 \
	    iso8859-8 hebrew \
	    iso8859-9 ISO_8859-9:1989 \
	    iso8859-9 iso-ir-148 \
	    iso8859-9 ISO_8859-9 \
	    iso8859-9 latin5 \
	    iso8859-9 l5 \
	    iso8859-10 iso-ir-157 \
	    iso8859-10 l6 \
	    iso8859-10 ISO_8859-10:1992 \
	    iso8859-10 latin6 \
	    iso8859-14 iso-ir-199 \
	    iso8859-14 ISO_8859-14:1998 \
	    iso8859-14 ISO_8859-14 \
	    iso8859-14 latin8 \
	    iso8859-14 iso-celtic \
	    iso8859-14 l8 \
	    iso8859-15 ISO_8859-15 \
	    iso8859-15 Latin-9 \
	    iso8859-16 iso-ir-226 \
	    iso8859-16 ISO_8859-16:2001 \
	    iso8859-16 ISO_8859-16 \
	    iso8859-16 latin10 \
	    iso8859-16 l10 \
	    jis0201 X0201 \
	    jis0208 iso-ir-87 \
	    jis0208 x0208 \
	    jis0208 JIS_X0208-1983 \
	    jis0212 x0212 \
	    jis0212 iso-ir-159 \
	    ksc5601 iso-ir-149 \
	    ksc5601 KS_C_5601-1989 \
	    ksc5601 KSC5601 \
	    ksc5601 korean \
	    shiftjis MS_Kanji \
	    utf-8 UTF8]
    
    foreach {enc mimeType} $encAliasList {
	set reversemap([string tolower $mimeType]) $enc
    }
}

proc cu::mail::reversemapencoding {mimeType} {
    variable reversemap
    
    set lmimeType [string tolower $mimeType]
    if {[info exists reversemap($lmimeType)]} {
	return $reversemap($lmimeType)
    }
    return ""
}

################################################################################
#    get/list fonts
################################################################################

proc cu::_give_truetype_font_file { font dirList } {
    foreach dir $dirList {
	if { [file readable [file join $dir $font]] } {
	    return [file join $dir $font]
	}
    }
    foreach dir $dirList {
	set ret [_give_truetype_font_file $font [glob -nocomplain -types d -dir $dir *]]
	if { $ret ne "" } {
	    return $ret
	}
    }
    return ""
}

proc cu::give_truetype_font_file { font } {
 
    if { ![string equal -nocase [file extension $font] ".ttf"] } {
	append font ".ttf"
    }
    set dirList ""
    if { $::tcl_platform(platform) eq "windows" } {
	lappend dirList "$::env(windir)/Fonts"
    } elseif { $::tcl_platform(os) eq "Darwin" } {
	lappend dirList "/Library/fonts" "/System/Library/fonts"
    } else {
	regsub -all {\s+} $font {} font
	lappend dirList "/usr/share/fonts"
    }
    return [_give_truetype_font_file $font $dirList]
}

proc cu::_give_truetype_fonts_files { dirList } {
    set ret ""
    foreach dir $dirList {
	lappend ret {*}[glob -nocomplain -types f -dir $dir *.ttf]
    }
    foreach dir $dirList {
	lappend ret {*}[_give_truetype_fonts_files [glob -nocomplain -types d -dir $dir *]]
    }
    return $ret
}

proc cu::give_truetype_fonts_files {} {
    set dirList ""
    if { $::tcl_platform(platform) eq "windows" } {
	lappend dirList "$::env(windir)/Fonts"
    } else {
	lappend dirList "/usr/share/fonts/truetype"
    }
    return [_give_truetype_fonts_files $dirList]
}

















