
package require comm
package require sha1
#package require compass_utils
package require compass_utils::c
#package require compass_utils::img

package provide compass_utils::client_server 1.12

namespace eval compass_client {
    variable topdir [file dirname [info script]]
    variable server_id_info ""
    variable block_size 200000
    variable systemtrayico_stack ""
    variable wait_variable

    namespace export copy_file_to_server copy_file_from_server send \
	send_user_password

    if { [info commands _] eq "" } {
	proc _ { args } {
	    set ret [uplevel 1 ::msgcat::mc $args]
	    regexp {(.*)#C#(.*)} $ret {} ret
	    return $ret
	}
    }
    #msgcat::mcload [file join $topdir msgs]

}

proc compass_client::init_client { args } {
    variable server_id_info

    set optional {
	{ -raise_error boolean 0 }
	{ -port port 1375 }
    }
    set compulsory "host"
    parse_args $optional $compulsory $args

    set server_id [list $port $host]
    
    if { ![info exists server_id_info] } { set server_id_info "" }
    if { ![dict exists $server_id_info servers $server_id] } {
	catch { comm::comm destroy }
	comm::comm new ::comm::comm -listen 0
	set err [catch { comm::comm connect $server_id }]
	if { $err } {
	    set txt [_ "Could not connect to host '%s' port '%s'" $host $port]
	    if { $raise_error } {
		error $txt
	    }
	    tk_messageBox -message $txt
	}
	dict set server_id_info servers $server_id num 1
    } else {
	set num [dict get $server_id_info servers $server_id num]
	dict set server_id_info servers $server_id num [incr num]
    }
    dict set server_id_info servers $server_id current_local_session ""
    dict set server_id_info current_server $server_id
    return $server_id
}

proc compass_client::send_user_password { args } {
    variable server_id_info

    set optional {
	{ -user_type type "" }
	{ -accept_no_users boolean 1 }
	{ -raise_error boolean 1 }
	{ -server_id server_id "" }
    }
    set compulsory "username password"
    parse_args $optional $compulsory $args
    
    set local_session [expr {[dict_getd $server_id_info max_local_session 0]+1}]
    dict set server_id_info max_local_session $local_session
    if { $server_id eq "" } {
	set server_id [dict get $server_id_info current_server]
    }
    set after [after 3000 [list catch [list comm::comm shutdown $server_id]]]

    set key [sha1::sha1 [list $username $password]]
    set err [catch { send user_password -user_type $user_type \
		-accept_no_users $accept_no_users $username $key } server_session]
    if { $err } {
	if { $server_session eq "Connection shutdown by request" } {
	    set server_session [_ "There has been a timeout in the connection"]
	}
	error $server_session
    }
    after cancel $after

    if { $user_type ne "database" } {
	set retry_cmd [list user_password -user_type $user_type \
		-session $server_session $username $key]
    } else {
	set retry_cmd ""
	set raise_error 1
    }

    dict set server_id_info local_sessions $local_session server_id $server_id
    dict set server_id_info local_sessions $local_session server_session $server_session
    dict set server_id_info local_sessions $local_session retry_cmd $retry_cmd
    dict set server_id_info local_sessions $local_session raise_error $raise_error

    dict set server_id_info servers $server_id current_local_session ""
    
    #puts "compass_client::send_user_password $server_id $local_session"

    return $local_session
}

proc compass_client::end_client { args } {
    variable server_id_info
	
    set optional {
	{ -session num "" }
    }
    set compulsory ""
    parse_args $optional $compulsory $args
    
    if { $server_id_info eq "" } { return }

    if { $session eq "" } {
	set server_id [dict get $server_id_info current_server]
	if { $server_id eq "" } { return }
	set session [dict get $server_id_info servers $server_id current_local_session]
    } else {
	set server_id [dict get $server_id_info local_sessions $session server_id]
    }
    if { $session ne "" } {
	catch { send_force -session $session end_session }
	
	dict unset server_id_info local_sessions $session
	if { $session ==  [dict get $server_id_info servers $server_id current_local_session] } {
	    dict set server_id_info servers $server_id current_local_session ""
	}
    }
    set num [dict get $server_id_info servers $server_id num]
    dict set server_id_info servers $server_id num [incr num -1]
    if { $num == 0 } {
	if { [dict get $server_id_info current_server] eq $server_id } {
	    dict set server_id_info current_server ""
	}
	dict unset server_id_info servers $server_id
    }
}

proc compass_client::is_init_client { args } {
    variable server_id_info
    
    set optional {
    }
    set compulsory ""
    parse_args $optional $compulsory $args
    
    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return 0 }
    set err [catch { send_force set isok 1 } errstring]
    if { !$err } { return 1 }
    return 0
}

proc compass_client::is_user_type { type } {

    set err [catch { send user_type } server_user_type]
    if { $err } { return 1 }

    if { $type eq "no_users" && $server_user_type ne "no_users" } { return 0 }
    if { $type eq $server_user_type || $server_user_type eq "all" } {
	return 1
    } else {
	return 0
    }
}

proc compass_client::exec_in_dir { args } {
    variable server_id_info
    
    set optional {
	{ -session num "" }
    }
    set compulsory "remotedir"
    set args [parse_args -raise_compulsory_error 0 $optional $compulsory $args]

    if { $session eq "" } {
	set server_id [dict get $server_id_info current_server]
	set session [dict get $server_id_info servers $server_id current_local_session]
    }
    set err [catch { send -session $session exec_in_dir $remotedir {*}$args } process_pid]
    if { $err } {
	error "error executing: $process_pid"
    }
    dict set server_id_info local_sessions $session process_pid $process_pid
    return $process_pid
}

proc compass_client::copy_file_to_server { args } {
    variable block_size

    set optional {
	{ -binary boolean 0 }
	{ -nocomplain boolean 0 }
	{ -session num "" }
	{ -save_cache boolean 0 }
    }
    set compulsory "file remotedir"
    parse_args $optional $compulsory $args
    set remotefile [file join $remotedir [file tail $file]]
    
    set err [catch { open $file r } fin]
    if { $err } {
	if { $nocomplain } { return }
	error [_ "Error opening file '%s' (%s)" $file $fin]
    }
    close $fin

    if { $save_cache } {
	set err [catch { cu::file::sha1 $file } sha1]
	if { $err } {
	    error [_ "Error checking file '%s' (%s)" $file $sha1]
	}
	if { [send file_exists $remotefile $sha1] } {
	    return
	}
    }
    set err [catch { open $file r } fin]
    if { $err } {
	error [_ "Error opening file '%s' (%s)" $file $fin]
    }
    if { $binary } { fconfigure $fin -translation binary }

    set status start
    set remotefileL $remotefile
    set fout ""
    while { ![eof $fin] } {
	set data [cu::deflate [read $fin $block_size]]
	set fout [send -session $session _recieve_file -binary $binary \
		$remotefileL $data $status $fout]
	set status continue
	set remotefileL ""
	update
    }
    send -session $session _recieve_file -binary $binary "" [cu::deflate ""] end $fout
    close $fin
    
    if { $save_cache } {
	send file_save_in_cache $remotefile
    }
}

proc compass_client::copy_file_from_server { args } {
    variable gui_w

    set optional {
	{ -binary boolean 0 }
	{ -nocomplain boolean 0 }
	{ -session num "" }
    }
    set compulsory "file localdir"
    parse_args $optional $compulsory $args

    set status start
    set fin ""
    while { $status ne "end" } {
	set err [catch { send -session $session _send_file \
		    -binary $binary $file $status $fin} ret]
	if { $err } {
	    if { $nocomplain } { return }
	    error $ret
	}
	if { $status eq "start" } {
	    set localfile [file join $localdir [file tail $file]]
	    set fout [open $localfile w]
	    if { $binary } { fconfigure $fout -translation binary }
	}
	lassign $ret data status fin
	puts -nonewline $fout [cu::inflate $data]
	set file ""
	update
    }
    close $fout
    if { [info exists gui_w] } {
	_gui_update [list _send_file "" end]
    }
}

proc compass_client::send_force { args } {
    variable server_id_info

	set optional {
	{ -session num "" }
    }
    set compulsory ""
    set args [parse_args -raise_compulsory_error 0 $optional $compulsory $args]
    
    if { $session ne "" } {
	    set server_id [dict get $server_id_info local_sessions $session server_id]
	    set server_session [dict get $server_id_info local_sessions $session server_session]
	    set args [linsert $args 0 session$server_session]
	    dict set server_id_info current_server $server_id
	    dict set server_id_info servers $server_id current_local_session $session   
    } else {
	set server_id [dict get $server_id_info current_server]
	#set session [dict get $server_id_info servers $server_id current_local_session]
    }
    #puts "compass_client::send_force $server_id $args"
    set ret [comm::comm send $server_id {*}$args]
    return $ret
}

proc compass_client::send { args } {
    variable server_id_info
    variable gui_w
    variable wait_variable
    
    set optional {
	{ -session num "" }
    }
    set compulsory ""
    set args [parse_args -raise_compulsory_error 0 $optional $compulsory $args]
    

    if { $session ne "" } {
	set server_id [dict get $server_id_info local_sessions $session server_id]
	set server_session [dict get $server_id_info local_sessions $session server_session]
	set args [linsert $args 0 session$server_session]
	dict set server_id_info current_server $server_id
	dict set server_id_info servers $server_id current_local_session $session
	#puts "sendA [clock format [clock seconds]] $session -- $server_id -- $server_session -- $args"
    } else {
	set server_id [dict get $server_id_info current_server]
	set session [dict get $server_id_info servers $server_id current_local_session]
	#puts "sendB [clock format [clock seconds]] $session $args"
    }
    set retry_cmd [dict_getd $server_id_info local_sessions $session retry_cmd ""]

    set refused_errs ""
    lappend refused_errs "Connect to remote failed: couldn't open socket: connection refused"
    lappend refused_errs "target application died or connection lost"
    lappend refused_errs "rejected connection*"
    lappend refused_errs "Connect to remote failed: couldn't open socket: invalid argument"
    while 1 {
	#puts "compass_client::send $server_id $args"
	set err [catch { comm::comm send $server_id {*}$args } ret]
	if { $err } {
	    set is_connection_error 0
	    foreach i $refused_errs {
		if { [string match $i $ret] } {
		    set is_connection_error 1
		    break
		}
	    }
	    if { $is_connection_error && $retry_cmd ne "" } {
		while 1 {
		    if { [info exists gui_w] } {
		        manageicontray $gui_w push_message [_ "Connection lost. Press here to retry"]
		        set w $gui_w._ask
		    } else { set w ._ask }
		    
		    if { ([info exists gui_w] && [wm state $gui_w] eq "normal") || \
		        ![info exists gui_w] } {
		        package require dialogwin
		        set title [_ "Connection to '%s' is lost" $server_id]
		        set txt [_ "Connection to '%s' is lost. Retry?" $server_id]
		        destroy $w
		        set w [dialogwin_snit $w -title $title \
		                -entrytext $txt -cancelname [_ Cancel]]
		        set action [$w createwindow]
		        destroy $w
		        if { $action < 1 } {
		            error $ret $::errorInfo
		        }
		    }
		    set err [catch { comm::comm send $server_id {*}$retry_cmd } ret]
		    if { !$err } {
		        if { [info exists gui_w] } {
		            manageicontray $gui_w pop_message
		        }
		        break
		    }
		    after 3000 [list set compass_client::wait_variable 1]
		    vwait compass_client::wait_variable
		}
	    } else {
		set raise_error [dict_getd $server_id_info local_sessions $session raise_error ""]
		if { $raise_error != 0 } {
		    error $ret $::errorInfo
		} else {
		    tk_messageBox -message [_ "Connection with the server has been lost (%s)" $ret]
		    return
		}
	    }
	} else { break }
    }
    if { [info exists gui_w] } {
	_gui_update $args
    }
    return $ret
}

proc compass_client::give_ports_list { args } {
    variable server_id_info
    
    set optional {
	{ -parent parent_window "" }
	{ -session num "" }
    }
    parse_args $optional "" $args

    if { $session eq "" } {
	gui_connect $parent
	
	set server_id [dict_getd $server_id_info current_server ""]
	if { $server_id eq "" } { return }
    }
    set err [catch { send -session $session give_ports_list } ret]
    if { $err } {
	error $ret
    }
    return $ret
}

proc compass_client::view_remote_log { args } {
    variable server_id_info

    set optional {
	{ -w window "" }
	{ -port port "" }
	{ -session num "" }
    }
    parse_args $optional "" $args

    if { $session eq "" } {
	if { $w eq "" } { return }
	gui_connect $w
	
	set server_id [dict_getd $server_id_info current_server ""]
	if { $server_id eq "" } { return }
    }
    mylog::init log    
    mylog::clear_log
    set err [catch { send -session $session give_log_data $port } ret]
    if { $err } {
	snit_messageBox -message $ret -parent $w
	return
    }
    mylog::add_log_data $ret
    mylog::view_log -clear_handler [list compass_client::clear_log -port $port -session $session $w]
}

proc compass_client::clear_log { args } {
    variable server_id_info
    
    set optional {
	{ -port port "" }
	{ -session num "" }
    }
    parse_args $optional "w" $args

    if { $session eq "" } {
	gui_connect $w
	
	set server_id [dict_getd $server_id_info current_server ""]
	if { $server_id eq "" } { return }
    }
    set err [catch { send -session $session clear_log $port } ret]
    if { $err } {
	snit_messageBox -message $ret -parent $w
	return
    }
}

proc compass_client::_gui_drop_files { w files X Y } {
    set v [fileutil::stripPwd [lindex $files 0]]

    set w_in [winfo containing $X $Y]
    set err [catch { $w_in cget -textvariable } var]
    if { !$err && $var ne "" } {
	set $var $v
    } else {
	$w set_uservar_value gid_model $v
    }
}

proc compass_client::_gui_get_gid_model { w } {

    set dir [$w give_uservar_value gid_model]
    set file [tk_getOpenFile -defaultextension .geo -filetypes \
	    [list [list [_ "GiD geo files"] [list ".geo"]] \
		[list [_ "All files"] [list "*"]]] \
	    -initialdir $dir \
	    -parent $w -title [_ "Open GiD file"]]
    if { $file == "" } { return }
    $w set_uservar_value gid_model [fileutil::stripPwd [file dirname $file]]
}

proc compass_client::_gui_update { send_args } {
    variable gui_w

    set info_text [$gui_w give_uservar_value info_text]
    set gid_model [$gui_w give_uservar_value gid_model]
    set infofile_ext [$gui_w give_uservar_value infofile_ext]
    set info_file [file join $gid_model [file root [file tail $gid_model]]$infofile_ext]
    if { [$gui_w exists_uservar info_mtime] } {
	set last_mtime [$gui_w give_uservar_value info_mtime]
    } else {
	set last_mtime ""
    }
    if { ![file exists $info_file] && $last_mtime ne "" } {
	$info_text configure -state normal
	$info_text delete 1.0 end
	$gui_w set_uservar_value info_mtime ""
	$info_text configure -state disabled
    } elseif { [file exists $info_file] && ($last_mtime eq "" || \
	[file mtime $info_file] > $last_mtime) } {
	$info_text configure -state normal
	$info_text delete 1.0 end
	set fin [open $info_file r]
	$info_text insert end [read $fin]
	close $fin
	$info_text see end
	$gui_w set_uservar_value info_mtime [file mtime $info_file]
	$info_text configure -state disabled
    }
    set log_text [$gui_w give_uservar_value log_text]
    $log_text configure -state normal

    if { [lindex $send_args 0] eq "user_password" } {
	set now [clock format [clock seconds] -format "%Y-%m-%d %H:%M:%S"]
	$log_text insert end "start session $now\n"
	$log_text insert end "user_password [lindex $send_args 1] ...\n"
    } elseif { [lindex $send_args 0] eq "add_user" } {
	set now [clock format [clock seconds] -format "%Y-%m-%d %H:%M:%S"]
	$log_text insert end "add_user [lindex $send_args 1] ...[lindex $send_args 3]\n"
    } elseif { [lindex $send_args 0] eq "end_session" } {
	set now [clock format [clock seconds] -format "%Y-%m-%d %H:%M:%S"]
	$log_text insert end "end session $now\n"
    } elseif { [lindex $send_args 0] eq "_recieve_file" } {
	if { [lindex $send_args 1] eq "-binary" } {
	    foreach "- - bin file -  state -" $send_args break
	} else {
	    set bin 0
	    foreach "- file -  state -" $send_args break
	}
	switch $state {
	    start {
		$log_text insert end [_ "Sending file '%s' (binary=%s) " [file tail $file] $bin]
	    }
	    continue {
		$log_text insert end "."
	    }
	    end {
		$log_text insert end "ok\n"
	    }
	}
    } elseif { [lindex $send_args 0] eq "_send_file" } {
	if { [lindex $send_args 1] eq "-binary" } {
	    foreach "- - bin file state" $send_args break
	} else {
	    set bin 0
	    foreach "- file state" $send_args break
	}
	switch $state {
	    start {
		$log_text insert end [_ "Recieving file '%s' (binary=%s) " [file tail $file] $bin]
	    }
	    continue {
		$log_text insert end "."
	    }
	    end {
		$log_text insert end "ok\n"
	    }
	}
    } elseif { [lindex $send_args 0] eq "file_exists" } {
	lassign $send_args - file
	$log_text insert end "file_exists [file tail $file] (sha1)\n"
    } else {
	set txt [join $send_args { }]
	if { [string length $txt] > 80 } {
	    set txt [string range $txt 0 76]...
	}
	$log_text insert end "$txt\n"
    }
    $log_text see end
    $log_text configure -state disabled
    update
}

proc compass_client::clear_log_tab { w } {
    set log_text [$w give_uservar_value log_text]
    $log_text configure -state normal
    $log_text delete 1.0 end
     $log_text configure -state disabled
}

proc compass_client::manageicontray { w what args } {
    variable topdir
    variable systemtrayico
    variable systemtrayico_stack
    variable port
    
    switch $what {
	start {
	    if { $::tcl_platform(platform) ne "windows" } {
		package require img::png
		set ico [file join $topdir images Compasser22.png]
	    } else {
		set ico [file join $topdir images Compasser.ico]
	    }
	    set title [_ "Compass client" ]
	    
	    if { $::tcl_platform(platform) eq "windows" } {
		if { [catch { package require Winico }] } { return }
		set systemtrayico [winico create $ico]
		winico taskbar add $systemtrayico -text $title \
		    -callback [list compass_client::manageicontray $w callback %m %i %w %l %x %y]
	    } else {
		if { [info command cu::icontray_exists] eq "" || ![cu::icontray_exists] } { return }
		if { [catch { package present Tk }] } { return }
		set w [cu::icontray .icontray]
		set img [image create photo -file $ico]
		pack [label $w.b -image $img -bd 0]
		set systemtrayico $w.b
		bind $w.b <1> [list compass_client::manageicontray $w callback WM_LBUTTONUP - - - %X %Y]
		bind $w.b <ButtonRelease-3> [list compass_client::manageicontray $w callback WM_RBUTTONUP - - - %X %Y]
		cu::tooltip register $systemtrayico $title
	    }
	    bind $w <Destroy> {
		if { "%W" eq [winfo toplevel %W] } {
		    compass_client::manageicontray %W delete
		}
	    }
	    bind $w <Unmap> {
		if { "%W" eq [winfo toplevel %W] } {
		    wm withdraw %W
		}
	    }
	}
	callback {
	    lassign $args message ico wparam lparam x y
	    switch $message {
		WM_LBUTTONUP {
		    if { [wm state $w] ne "normal" } {
		        wm deiconify $w
		    } else {
		        wm withdraw $w
		    }
		}
		WM_RBUTTONDOWN {
		    #nothing
		}
		WM_RBUTTONUP {
		    catch { destroy .traymenu }
		    set m [menu .traymenu -tearoff 0]
		    $m add command -label [_ "Deiconify"] -command \
		        [list wm deiconify $w]
		    $m add separator
		    $m add command -label [_ "Exit"] -command \
		        [list compass_client::gui_exit]
		    tk_popup $m $x $y
		}
	    }
	}
	delete {
	    if { $systemtrayico eq "" } { return }
		if { $::tcl_platform(platform) eq "windows" } {
		winico taskbar delete $systemtrayico
		winico delete $systemtrayico
	    } else {
		destroy [winfo toplevel $systemtrayico]
	    }
	}
	set_message {
	    if { ![info exists systemtrayico] } { return }
	    set txt  "Compass client: [lindex $args 0]"
	    if { $::tcl_platform(platform) eq "windows" } {
		winico taskbar modify $systemtrayico -text $txt
	    } else {
		cu::tooltip register $systemtrayico $txt
	    }
	}
	push_message {
	    lappend systemtrayico_stack [lindex $args 0]
	    manageicontray $w set_message  "Compass client: [lindex $args 0]"
	}
	pop_message {
	    set systemtrayico_stack [lrange $systemtrayico_stack 0 end-1]
	    manageicontray $w set_message  "Compass client: [lindex $systemtrayico_stack end]"
	}
    }
}


proc compass_client::add_user_gui { parent } {
    variable server_id_info
    
    gui_connect $parent

    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return }
    
    set err [catch { send give_existing_users } give_existing_users]
    if { $err } {
	snit_messageBox -message [_ "User is not allowed to add/modify other users"] -parent $parent
	return
    }

    package require dialogwin

    if { $parent eq "." } { set parent "" }

    destroy $parent.adduser
    set w [dialogwin_snit $parent.adduser -title [_ "Add user"]]
    set f [$w giveframe]
	
    ttk::label $f.l2 -text [_ User:]
    ttk::combobox $f.e2 -width 30 -textvariable [$w give_uservar user ""] \
	-values $give_existing_users
    
    ttk::label $f.l3 -text [_ Password:]
    ttk::entry $f.e3 -textvariable [$w give_uservar password ""] -show *

    ttk::label $f.l4 -text [_ "Repeat password:"]
    ttk::entry $f.e4 -textvariable [$w give_uservar password2 ""] -show *
	
    ttk::label $f.l5 -text [_ "User type:"]
    
    lassign [send give_allowed_user_types] user_types user_types_dict
    cu::combobox $f.e5 -width 30 -textvariable [$w give_uservar user_type] \
	-values $user_types -dict $user_types_dict -state readonly
    $w set_uservar_value user_type [lindex $user_types end]

    ttk::checkbutton $f.cb1 -text [_ "Remove user"] -variable \
	[$w give_uservar removeuser 0]

    grid $f.l2 $f.e2 -sticky w -padx 3 -pady 2
    grid $f.l3 $f.e3 -sticky w -padx 3 -pady 2
    grid $f.l4 $f.e4 -sticky w -padx 3 -pady 2
    grid $f.l5 $f.e5 -sticky w -padx 3 -pady 2

    grid $f.cb1 -    -sticky w -padx 3 -pady 2

    grid configure $f.e2 $f.e3 $f.e4 $f.e5 -sticky ew
    grid columnconfigure $f 1 -weight 1

    tk::TabToWindow $f.e2
    bind $w <Return> [list $w invokeok]
    set action [$w createwindow]
    while 1 {
	if { $action <= 0 } {
	    destroy $w
	    return
	}
	foreach i [list user password password2 user_type] {
	    set $i [string trim [$w give_uservar_value $i]]
	}
	if { $password ne $password2 } {
	    snit_messageBox -message [_ "Passwords are not equal"] -parent $w
	} elseif { $user eq "" } {
	    snit_messageBox -message [_ "User cannot be void"] -parent $w
	} else {
	    break
	}
	set action [$w waitforwindow]
    }
    if { [$w give_uservar_value removeuser] } {
	send remove_user [$w give_uservar_value user] \
	    [$w give_uservar_value user_type]
    } else {
	send add_user [$w give_uservar_value user] [$w give_uservar_value password] \
	    [$w give_uservar_value user_type]
	if { [is_user_type no_users] } {
	    gui_unconnect $parent
	    $parent set_uservar_value user [$w give_uservar_value user]
	    $parent set_uservar_value password [$w give_uservar_value password]
	    gui_connect $parent
	}
    }
    destroy $w
}

proc compass_client::enter_analysis_password { w } {
    variable server_id_info

    gui_connect $w
    
    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return }
    if { [is_user_type no_users] } {
	snit_messageBox -message [_ "Host does not have users. Create first one user"] \
	    -parent $w
	add_user_gui $w
	return        
    }
    if { ![is_user_type analysis] } {
	snit_messageBox -message [_ "User is not allowed to run analysis"] \
	    -parent $w
	return        
    }
    set sysinfo_handler [$w give_uservar_value sysinfo_handler]
    set err [catch { uplevel #0 $sysinfo_handler } data]
    if { $err } {
	snit_messageBox -message [_ "Sysinfo data not correct (data='%s')" $data] \
	    -parent $w
	return
    }
    set ret [regexp -line {Name:\s+(\S+)} $data {} computer_name]
    if { !$ret } {
	set err [catch { llength $data } ret]
	if { !$err && $ret == 1 } {
	    set computer_name [lindex $data 0 0]
	} elseif { !$err && $ret == 3 } {
	    set computer_name [lindex $data 0]
	} else {
	    snit_messageBox -message [_ "Sysinfo data not correct (data='%s')" $data] \
		-parent $w
	    return
	}
    }
    destroy $w.pass
    set wp [dialogwin_snit $w.pass -title [_ "Enter analysis password"]]

    set f [$wp giveframe]
    set err [catch { $f cget -background } bg]
    if { $err } { set bg white }
    text $f.t1 -width 30 -height 4 -background $bg -bd 0
    $f.t1 insert end $data
    $f.t1 configure -state disabled
    bind $f.t1 <1> [list focus $f.t1]

    ttk::label $f.l1 -text [_ "Password:"]
    ttk::entry $f.e1 -textvariable [$wp give_uservar password ""] -show * \
	-width 30

    grid $f.t1 - -sticky nswe
    grid $f.l1 $f.e1 -sticky w -padx 2 -pady 5
    grid columnconfigure $f 1 -weight 1

    tk::TabToWindow $f.e1
    set action [$wp createwindow]
    set password [$wp give_uservar_value password]
    destroy $wp
    if { $action < 1 } { return }

    set password_file [$w give_uservar_value password_file]
    set err [catch { open $password_file a } fout]
    if { $err } {
	snit_messageBox -message [_ "You do not have permissions to store the password"] \
	    -parent $w
	return
    }
    puts $fout "$computer_name $password #"
    close $fout
}

proc compass_client::update_sessions_tab { w } {
    variable server_id_info

    set t [$w give_uservar_value sessions_tree]
    $t item delete all

    gui_connect $w

    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return }

    set list [send sessions_list]
    if { [$t cget -columns] eq "" } {
	set cols ""
	foreach i [lindex $list 0] {
	    lappend cols [list 15 $i left text 0]
	}
	$t configure -columns $cols
    }
    foreach i [lrange $list 1 end] {
	$t insert end $i
    }
}

proc compass_client::kill_sessions { w tree ids } {

    set text [_ "Are you sure to terminate %d sessions?" [llength $ids]]
    set retval [snit_messageBox -default ok -icon question -message $text \
	    -type okcancel -parent $w]
    if { $retval == "cancel" } { return }

    foreach id $ids {
	set session [$tree item text $id 0]
	send end_another_session $session
    }
    update_sessions_tab $w
}

proc compass_client::update_memory_tab { w } {
    variable server_id_info

    set memory_text [$w give_uservar_value memory_text]
    $memory_text configure -state normal
    $memory_text delete 1.0 end
    $memory_text configure -state disabled

    gui_connect $w

    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return }
    set session [dict get $server_id_info servers $server_id current_local_session]
    set process_pid [dict_getd $server_id_info local_sessions $session process_pid ""]

    if { $process_pid ne "" } {
	set values [send processes_info -pid $process_pid]
    } else {
	set values [send processes_info]
    }
    $memory_text configure -state normal
    $memory_text delete 1.0 end
    $memory_text tag configure bold -font [concat [font actual \
		[$memory_text cget -font]] -weight bold]

    foreach "n c" [list [_ "Computer memory"] totalphysical] {
	if { ![dict exists $values $c] } { continue }
	$memory_text insert end "$n: " bold
	set val [format "%.4g Mb" [expr {[dict get $values $c]/1024.0/1024.0}]]
	$memory_text insert end "\t$val\n"
    }
    $memory_text insert end \n[_ "All processes"]\n bold

    foreach "n c" [list [_ "Memory"] workingset_total \
	    [_ "Full memory"] pagefilebytes_total] {
	if { ![dict exists $values $c] } { continue }
	$memory_text insert end "\t$n: " bold
	set val [format "%.4g Mb" [expr {[dict get $values $c]/1024.0/1024.0}]]
	$memory_text insert end "\t$val\n"
    }
    $memory_text insert end \n[_ "This process"]\n bold

    set inum 0
    foreach "n c" [list [_ "Memory"] workingset \
	    [_ "Full memory"] pagefilebytes] {
	if { ![dict exists $values $c] } { continue }
	$memory_text insert end "\t$n: " bold
	set val [format "%.4g Mb" [expr {[dict get $values $c]/1024.0/1024.0}]]
	$memory_text insert end "\t$val\n"
	incr inum
    }
    foreach "n c" [list [_ "Total time"] elapsedtime] {
	if { ![dict exists $values $c] } { continue }
	$memory_text insert end "$n: " bold
	$memory_text insert end "\t[dict get $values $c] s\n"
	incr inum
    }
    if { !$inum } {
	$memory_text insert end "([_ "no process"])\n"
    }
    $memory_text configure -state disabled
}

proc compass_client::gui_unconnect { w } {
    variable server_id_info
    
    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return }

    set host [$w give_uservar_value host]

    manageicontray $w pop_message
    end_client
    $w set_uservar_value state unconnected
}

proc compass_client::gui_connect { w } {
    variable server_id_info
    
    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id ne "" } {
	set err [catch { send_force set isok 1 } errstring]
	if { !$err } { return }
	manageicontray $w pop_message
	end_client
	$w set_uservar_value state unconnected
    }

    set host [$w give_uservar_value host]
    set user [$w give_uservar_value user]
    set user_type [$w give_uservar_value user_type]
    set password [$w give_uservar_value password]

    set port 1375
    if { ![regexp {(.*):(\d+)} $host {} hostL port] } {
	set hostL $host
    }
    
    set err [catch { init_client -raise_error 1 -port $port $hostL } errstring]
    if { $err } {
	snit_messageBox -message [_ "Failed connection to host '%s'" $host] \
	    -parent $w
	end_client
	compass_client::manageicontray $w set_message [_ "not connected"]
	$w set_uservar_value state unconnected
	return
    }
    set err [catch { send_user_password -user_type $user_type $user $password } errstring]
    if { $err } {
	if { $password eq "" } {
	    snit_messageBox -message [_ "It is necessary a password to connect to host '%s'" $host] \
		-parent $w
	} else {
	    snit_messageBox -message [_ "User or password not OK for host '%s'" $host] \
		-parent $w
	}
	end_client
	manageicontray $w set_message [_ "not connected"]
	$w set_uservar_value state unconnected
	return
    }
    
    #user_type
    manageicontray $w push_message [_ "Connected to host '%s'" $host]
    
    set err [catch { send test_server_password } ret]
    if { !$err } {
	lassign $ret server_pro message
    } else {
	set server_pro VERSION_ID
    }
    $w set_uservar_value server_pro $server_pro
    $w set_uservar_value state connected
    
    foreach "list el" [list hosts host users user] {
	set $list [$w give_uservar_value $list]
	set $list [linsert0 -max_len 10 [set $list] [set $el]]
	$w set_uservar_value $list [set $list]
    }
}

proc compass_client::gui_exit {} {
    variable gui_w

    if { ![info exists gui_w] || ![winfo exists $gui_w] } {
	end_client
	exit
    }
    set w $gui_w
    if { [$w give_uservar_value state] eq "calculating" } {
	set host [$w give_uservar_value host]
	set text [_ "Are you sure to unconnect from host '%s' and kill any process?" $host]
	set retval [snit_messageBox -default ok -message $text \
		-type okcancel -parent $w]
	if { $retval eq "cancel" } { return }
    }
    set exit_handler [$w give_uservar_value exit_handler]
    if { $exit_handler ne "" } {
	catch { uplevel #0 $exit_handler }
    }

    set data ""
    foreach i [list hosts users host user gid_model] {
	dict set data $i [$w give_uservar_value $i]
    }
    if { [$w give_uservar_value remember_password] && [is_init_client] } {
	dict set data password64 [cu::base64 encode [cu::encrypter encode [$w give_uservar_value password]]]
    }
    set err [catch { cu::store_program_preferences compass_client $data } errstring]
    if { $err } {
	snit_messageBox -message [_ "Could not store preferences (%s)" $errstring] -parent $w
    }
    end_client
    exit
}

proc compass_client::gui_callback { w } {
    variable gui_action
    variable server_id_info
    
    if { [$w giveaction] < 1 } { gui_exit }

    if { [$w giveaction] == 2 } {
	gui_unconnect $w
	return
    }
    
    foreach i [list gid_model info_file password_file host user user_type password administer] {
	set $i [$w give_uservar_value $i]
    }
    if { [file exists $info_file] } {
	$w set_uservar_value info_mtime [file mtime $info_file]
    }

    gui_connect $w

    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return }

    if { $administer } { return }

    if { [is_user_type no_users] } {
	snit_messageBox -message [_ "Host does not have users. Create first one user"] \
	    -parent $w
	add_user_gui $w
	return        
    }
    if { ![is_user_type analysis] } {
	snit_messageBox -message [_ "User is not allowed to run analysis"] \
	    -parent $w
	return        
    }
    $w set_uservar_value state calculating

    set full_error_info [$w give_uservar_value full_error_info]
    set run_handler [$w give_uservar_value run_handler]
    lappend run_handler $gid_model $password_file

    manageicontray $w push_message [_ "running on' %s'" $host]
    update_running_label $w running
    
    set err [catch { uplevel #0 $run_handler } errstring]

    if { $err } {
	if { $full_error_info } {
	    snit_messageBox -message [_ "Failed running (%s)" $::errorInfo] -parent $w
	} else {
	    snit_messageBox -message [_ "Failed running (%s)" $errstring] -parent $w
	}
    }
    set session [dict get $server_id_info servers $server_id current_local_session]
    if { $session ne "" } {
	dict unset server_id_info local_sessions $session process_pid
    }
    update_running_label $w stopped
    manageicontray $w pop_message

    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } {
	$w set_uservar_value state unconnected
    } else {
	$w set_uservar_value state connected
    }
    if { [$w give_uservar_value exit_after_analysis] && (!$err || !$full_error_info) } {
	gui_exit
    }
}

proc compass_client::init_gui { args } {
    variable gui_w
    variable gui_action

    set optional {
	{ -host host "" }
	{ -port port "" }
	{ -user user "" }
	{ -user_type type "" }
	{ -password password "" }
	{ -password_file password_file "" }
	{ -hide_model_password "" 0 }
	{ -gid_model gid_model "" }
	{ -infofile_ext infofile_ext .info }
	{ -run_handler handler "" }
	{ -exit_handler handler "" }
	{ -sysinfo_handler handler "" }
	{ -exit_after_analysis "" 0 }
	{ -administer "" 0 }
	{ -full_error_info boolean 0 }
    }
    set compulsory ""
    parse_args $optional $compulsory $args

    package require dialogwin
    package require tooltip
    package require autoscroll
    catch { package require tkdnd }
    package require fileutil
    package require fulltktree
    
    if { $::tcl_platform(platform) eq "windows" } {
	package require Winico
    } else {
	package require compass_utils::c   
    }

    wm withdraw .
    set w [dialogwin_snit .%AUTO% -title [_ "Compass client"] \
	    -cancelname [_ Exit] -callback compass_client:::gui_callback \
	    -grab 0 -morebuttons [list [_ "Unconnect"]]]
    set f [$w giveframe]

    manageicontray $w start

    set gui_w $w

    dict for "n v" [cu::get_program_preferences compass_client] {
	if { ![info exists $n] || [set $n] eq "" } {
	    set $n $v
	}
    }
    if { $host eq "" } { set host localhost }
    if { $user eq "" } { set user compass }
    if { $port ne "" && $port != 1375 } { append host ":$port" }
    if { ![info exists hosts] } { set hosts [list localhost] }
    if { [lsearch -exact $hosts $host] == -1 } { lappend hosts $host }
    if { ![info exists users] } { set users [list compass] }
    if { [lsearch -exact $users $user] == -1 } { lappend users $user }
    
    if { $password eq "" && [set! password64] ne "" } {
	set password [cu::encrypter decode [cu::base64 decode $password64]]
	set remember_password 1
    } else {
	set remember_password 0
    }

    set info_file [file join $gid_model [file root [file tail \
		    $gid_model]]$infofile_ext]

    foreach i [list administer hosts users infofile_ext run_handler \
	    exit_handler sysinfo_handler exit_after_analysis info_file full_error_info] {
	$w set_uservar_value $i [set $i]
    }

    if { [file exists $info_file] } {
	$w set_uservar_value info_mtime [file mtime $info_file]
    }
    $w set_uservar_value user_type $user_type

    if { !$administer && [info command dnd] ne "" } {
	dnd bindtarget $w text/uri-list <Drop> [list compass_client::_gui_drop_files \
		$w %D %X %Y]
    }

    ttk::label $f.l1 -text [_ "Host:"]
    cu::combobox $f.cb1 -textvariable [$w give_uservar host $host] \
	-valuesvariable [$w give_uservar hosts]
    set txt [_ "Examples:"]
    append txt "\nlocalhost"
    append txt "\nwww2.compassis.com"
    append txt "\nwww2.compassis.com:1375"
    tooltip::tooltip $f.l1 $txt
    tooltip::tooltip $f.cb1 $txt

    ttk::label $f.l2 -text [_ "User:"]
    cu::combobox $f.cb2 -textvariable [$w give_uservar user $user] \
	-valuesvariable [$w give_uservar users] -width 20
    ttk::label $f.l3 -text [_ "Password:"]
    ttk::entry $f.e1 -textvariable [$w give_uservar password $password] \
	-show *
    ttk::checkbutton $f.e1cb -text [_ "Remember password"] -variable \
	[$w give_uservar remember_password $remember_password]
    
    ttk::label $f.l31 -text [_ "Password file:"]
    ttk::entry $f.e11 -textvariable [$w give_uservar password_file $password_file]

    ttk::label $f.l4 -text [_ "GiD model:"]
    ttk::combobox $f.cb3 -textvariable [$w give_uservar gid_model $gid_model] \
	-values [list $gid_model]

    ttk::button $f.b1 -image [cu::get_image_or_icon filenew-16] -style Toolbutton \
	-command [list compass_client::_gui_get_gid_model $w]
    tooltip::tooltip $f.b1 [_ "Select a GiD model in browser"]

    ttk::notebook $f.nb1
    
    ttk::frame $f.nb1.f1
    text $f.nb1.f1.t1 -xscrollcommand [list  $f.nb1.f1.sh set] \
	-yscrollcommand [list $f.nb1.f1.sv set] -bd 0 \
	-width 50 -height 10 -state disabled
    bind $f.nb1.f1.t1 <1> [list focus $f.nb1.f1.t1]
    scrollbar $f.nb1.f1.sv -orient vertical -command \
	[list $f.nb1.f1.t1 yview]
    scrollbar $f.nb1.f1.sh -orient horizontal -command \
	[list $f.nb1.f1.t1 xview]
    autoscroll::autoscroll $f.nb1.f1.sv
    autoscroll::autoscroll $f.nb1.f1.sh
    $w set_uservar_value info_text $f.nb1.f1.t1

    grid $f.nb1.f1.t1 $f.nb1.f1.sv -sticky ns
    grid $f.nb1.f1.sh -sticky ew
    grid configure $f.nb1.f1.t1 -sticky nsew
    grid columnconfigure $f.nb1.f1 0 -weight 1
    grid rowconfigure $f.nb1.f1 0 -weight 1

    if { !$administer } {
	$f.nb1 add $f.nb1.f1 -sticky nsew -text [_ "Info"]
    }
    ttk::frame $f.nb1.f2
    text $f.nb1.f2.t1 -xscrollcommand [list  $f.nb1.f2.sh set] \
	-yscrollcommand [list $f.nb1.f2.sv set] -bd 0 \
	-width 50 -height 10 -state disabled
    bind $f.nb1.f2.t1 <1> [list focus $f.nb1.f2.t1]
    scrollbar $f.nb1.f2.sv -orient vertical -command \
	[list $f.nb1.f2.t1 yview]
    scrollbar $f.nb1.f2.sh -orient horizontal -command \
	[list $f.nb1.f2.t1 xview]
    autoscroll::autoscroll $f.nb1.f2.sv
    autoscroll::autoscroll $f.nb1.f2.sh
    $w set_uservar_value log_text $f.nb1.f2.t1

    grid $f.nb1.f2.t1 $f.nb1.f2.sv -sticky ns
    grid $f.nb1.f2.sh -sticky ew
    grid configure $f.nb1.f2.t1 -sticky nsew
    grid columnconfigure $f.nb1.f2 0 -weight 1
    grid rowconfigure $f.nb1.f2 0 -weight 1

    $f.nb1 add $f.nb1.f2 -sticky nsew -text [_ "Log"]

    ttk::frame $f.nb1.f3
    text $f.nb1.f3.t1 -xscrollcommand [list  $f.nb1.f3.sh set] \
	-yscrollcommand [list $f.nb1.f3.sv set] -bd 0 \
	-width 50 -height 10 -state disabled -tabs "30 left 30 left"
    bind $f.nb1.f3.t1 <1> [list focus $f.nb1.f3.t1]
    scrollbar $f.nb1.f3.sv -orient vertical -command \
	[list $f.nb1.f3.t1 yview]
    scrollbar $f.nb1.f3.sh -orient horizontal -command \
	[list $f.nb1.f3.t1 xview]
    autoscroll::autoscroll $f.nb1.f3.sv
    autoscroll::autoscroll $f.nb1.f3.sh
    ttk::button $f.nb1.f3.b1 -text [_ "Update now"] -command \
	[list compass_client::update_memory_tab $w]
    $w set_uservar_value memory_text $f.nb1.f3.t1

    grid $f.nb1.f3.t1 $f.nb1.f3.sv -sticky ns
    grid $f.nb1.f3.sh -sticky ew
    grid $f.nb1.f3.b1 - -pady 2
    grid configure $f.nb1.f3.t1 -sticky nsew
    grid columnconfigure $f.nb1.f3 0 -weight 1
    grid rowconfigure $f.nb1.f3 0 -weight 1

    $f.nb1 add $f.nb1.f3 -sticky nsew -text [_ "Memory"]

    ttk::frame $f.nb1.f4

    fulltktree $f.nb1.f4.t1 -compass_background 1 -showlines 0 -indent 0 \
	-deletehandler [list compass_client::kill_sessions $w]
    ttk::button $f.nb1.f4.b1 -text [_ "Update now"] -command \
	[list compass_client::update_sessions_tab $w]
    $w set_uservar_value sessions_tree $f.nb1.f4.t1

    grid $f.nb1.f4.t1 -sticky nsew
    grid $f.nb1.f4.b1 - -pady 2
    grid columnconfigure $f.nb1.f4 0 -weight 1
    grid rowconfigure $f.nb1.f4 0 -weight 1

    $f.nb1 add $f.nb1.f4 -sticky nsew -text [_ "Sessions"]

    ttk::menubutton $f.b2 -text [_ "Server management"] -menu \
	$f.b2.m

    ttk::button $f.bl -image [cu::get_image_or_icon media-record-16] -style Toolbutton -command \
	[list compass_client::enter_server_password $w]
    
    $w add_trace_to_uservar state [list compass_client::check_server_password $w $f.bl]
   
    ttk::label $f.run -image [cu::get_image_or_icon calc-off] -style Toolbutton
    tooltip::tooltip $f.run [_ "Program is not running"]
    $w set_uservar_value run_label $f.run
    
    menu $f.b2.m -tearoff 0
    $f.b2.m add command -label [_ "View remote log"] -command \
	[list compass_client::view_remote_log -w $w]
    $f.b2.m add command -label [_ "Clear local log"] -command \
	[list compass_client::clear_log_tab $w]
    $f.b2.m add command -label [_ "Server version"] -command \
	[list compass_client::server_version $w]
#     $f.b2.m add command -label [_ "Console"] -command \
#         [list compass_client::open_console $w]
    $f.b2.m add command -label [_ "Server password"] -command \
	[list compass_client::enter_server_password $w]
    $f.b2.m add separator
    $f.b2.m add checkbutton -label [_ "Exit after analysis"] -variable \
	[$w give_uservar exit_after_analysis]
    $f.b2.m add separator
    $f.b2.m add command -label [_ "Add/remove user"] -command \
	[list compass_client::add_user_gui $w]

    ttk::button $f.b3 -text [_ "Analysis password"] -command \
	[list compass_client::enter_analysis_password $w]
    if { $sysinfo_handler eq "" } { $f.b3 state disabled }

    grid $f.l1 $f.cb1 - - -sticky w -padx 2 -pady 2
    grid $f.l2 $f.cb2 - - -sticky w -padx 2 -pady 2
    grid $f.l3 $f.e1  - - -sticky w -padx 2 -pady 2
    grid $f.e1cb  - - -sticky w -padx 2 -pady 2
    if { !$hide_model_password && !$administer } {
	grid $f.l31 $f.e11  - - -sticky w -padx 2 -pady 2
	grid $f.l4 $f.cb3 $f.b1 - -sticky w -padx 2 -pady 2
	grid configure $f.e11 $f.cb3 -sticky ew
	set row_weight1 5
    } else {
	set row_weight1 3
    }
    grid $f.nb1  -    -    - -sticky ewns -pady 1
    grid $f.b3 $f.b2 $f.bl $f.run -sticky w -padx 2 -pady 1
    grid configure $f.cb1 $f.cb2 $f.e1 -sticky ew
    grid configure $f.bl -padx "2 0"
    grid configure $f.run -padx "0 2"
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f $row_weight1 -weight 1

    set d ""
    dict set d unconnected [list $f.cb1 $f.cb2 $f.e1 $f.e11 $f.cb3 $f.b1 1]
    dict set d connected [list $f.e11 $f.cb3 $f.b1 1 2]
    dict set d calculating [list 2]

    $w enable_disable_on_key state $d
    $w set_uservar_value state unconnected

    bind $w <Return> [list $w invokeok]
    $w createwindow
}

proc compass_client::enter_server_password { w } {
    variable server_id_info

    gui_connect $w

    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return }
    
    if { [is_user_type no_users] } {
	snit_messageBox -message [_ "Host does not have users. Create first one user"] \
	    -parent $w
	add_user_gui $w
	return        
    }
    switch -- [$w give_uservar_value server_pro] {
	"VERSION_ID" {
	    set txt [_ "Server is in evaluation version mode"]
	}
	"VERSION_PRO"  - "VERSION_TMPPRO" {
	    set txt [_ "Server is in professional version mode"]
	}
    }
    append txt ". " [_ "Do you want to introduce a password?"]

    set wm [dialogwin_snit $w.m -title [_ "Server professional mode"] \
	    -entrytext $txt]
    set action [$wm createwindow]
    destroy $wm
    if { $action < 1 } { return }
    
    package require verifp::gui
    
    set web_page_params [dict create \
	    name "p_Computer name" \
	    os "p_Operating system" \
	    sysinfo "p_sysinfo" \
	    others [list p_Module "Compass server version 1"] \
	    ]

    set myverifp [verifp %AUTO% -program compass_server -version 1.0 \
	    -module "compass_server_professional" \
	    -web_page_show "http://www.compassis.com" \
	    -web_page "http://www.compassis.com/compass/Passwords" \
	    -web_page_params $web_page_params \
	    -key compass_99_server_1 \
	    ]

    set err [catch { send give_server_sysinfo } sysinfoList]
    if { $err } {
	snit_messageBox -message [_ "Server does not accept passwords"] \
	    -parent $w
	return
    }
    $myverifp enter_password_win $w $sysinfoList [list compass_client::enter_server_password_check $w]
}

proc compass_client::enter_server_password_check { w name os sysinfo password usb_file } {

    set err [catch { send test_server_password_and_enter $name $os $sysinfo $password $usb_file } ret]
    if { $err } {
	return [list BAD [_ "Could not enter password in server (%s)" $ret]]
    }
    
    set err [catch { send test_server_password } ret_test]
    if { !$err } {
	lassign $ret_test server_pro message
    } else {
	set server_pro VERSION_ID
    }
    $w set_uservar_value server_pro $server_pro
    $w set_uservar_value state [$w give_uservar_value state]
 
    return $ret
}

proc compass_client::update_running_label { w what } {
    
    set label [$w give_uservar_value run_label]
    switch $what {
	"running" {
	    $label configure -image [cu::get_image_or_icon calc-on] -style Toolbutton
	    tooltip::tooltip $label [_ "Program is running"]
	}
	default {
	    $label configure -image [cu::get_image_or_icon calc-off] -style Toolbutton
	    tooltip::tooltip $label [_ "Program is not running"]
	}
    }
    update
}

proc compass_client::check_server_password { w label } {

    switch [$w give_uservar_value state] {
	connected {
	    switch -- [$w give_uservar_value server_pro] {
		"VERSION_ID" {
		    tooltip::tooltip $label [_ "Connected to server '%s' - Evaluation version" [$w give_uservar_value host]]
		    $label configure -image [cu::get_image_or_icon applications-development-16]
		}
		"VERSION_PRO"  - "VERSION_TMPPRO" {
		    tooltip::tooltip $label [_ "Connected to server '%s' Professional version" [$w give_uservar_value host]]
		    $label configure -image [cu::get_image_or_icon applications-development-green-16]
		}
	    }
	}
	unconnected {
	    tooltip::tooltip $label [_ "Not connected to server"]
	    $label configure -image [cu::get_image_or_icon media-record-16]
	}
    }
}

proc compass_client::server_version { w } {
    variable server_id_info
    
    gui_connect $w
    
    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return }
    if { [is_user_type no_users] } {
	snit_messageBox -message [_ "Host does not have users. Create first one user"] \
	    -parent $w
	add_user_gui $w
	return        
    }
    set server_version [send server_version]
    snit_messageBox -message [_ "Server version: %s" $server_version] \
	-parent $w
}

proc compass_client::server_platform {} {
    set err [catch { send server_platform } platform]
    if { $err  } {
	error [_ "Error retrieving server_platform (%s)" $platform]
    }
    return $platform
}

proc compass_client::open_console { w } {
    variable server_id_info

    gui_connect $w

    set server_id [dict_getd $server_id_info current_server ""]
    if { $server_id eq "" } { return }
    if { [is_user_type no_users] } {
	snit_messageBox -message [_ "Host does not have users. Create first one user"] \
	    -parent $w
	add_user_gui $w
	return        
    }
    set tkcon [file join /TclTk/activetcl8.5/bin/ tkcon.tcl]
    if { ![file exists $tkcon] } {
	set tkcon [lindex [auto_execok tkcon] 0]
    }
    if { ![file exists $tkcon] } {
	snit_messageBox -message [_ "Could not find tkcon"]
	return
    }
    set tkconprefs {
	if { $::tcl_platform(platform) == "windows" } {
	    tkcon font "MS Sans Serif" 8
	} else {
	   tkcon font "new century schoolbook" 12
	}
	# Keep 50 commands in history
	set ::tkcon::OPT(history)  50
	set ::tkcon::OPT(buffer) 5000
	
	# Use a pink prompt
	set ::tkcon::COLOR(prompt) red
	# set tkcon(autoload) "Tk"
	set ::tkcon::OPT(cols)  80
	set ::tkcon::OPT(rows) 24
	set ::tkcon::OPT(gc-delay) 0
	#set tkcon(cols) 60
	#set tkcon(rows) 18

	set menubar $::tkcon::PRIV(menubar)
	$menubar insert [$menubar index end] cascade -label [_ Remote] -underline 0 \
	    -menu $menubar.ramm
	set m $menubar.ramm
	menu $m
	$m add check -label [_ "Attach to remote server"] -variable ::tkcon::attachremote \
	-command {
	    if { !$::tkcon::attachremote } {
		::tkcon::Attach {}
		set ::tkcon::PRIV(appname) ""
		::tkcon::Prompt \n [::tkcon::CmdGet $::tkcon::PRIV(console)]
	    } else {
		interp alias {} ::tkcon::EvalAttached {} \
		    my_eval
		set ::tkcon::PRIV(appname) [_ "Remote"]
		::tkcon::Prompt \n [::tkcon::CmdGet $::tkcon::PRIV(console)]
	    }
	}
    }
    append tkconprefs [list puts [_ "Welcome to tkcon client console"]]\n

    if { [interp exists tkcon] } { interp delete tkcon }
    interp create tkcon
    tkcon eval package require Tk
    tkcon eval wm withdraw .
    interp alias tkcon my_eval "" eval compass_client::send
    interp alias tkcon _ "" _

    tkcon eval [list set argv [list -rcfile "" -exec "" -root .tkcon \
		-eval $tkconprefs]]
    tkcon eval [list source $tkcon]
    #tkcon eval ::tkcon::Init $argv
    tkcon eval {
	proc ::tkcon::FinalExit { args } { destroy .tkcon }
    }
}
