
package require comm
package require compass_utils::c
package require msgcat
package require verifp::gui

if { $::tcl_platform(platform) eq "windows" } {
    package require Winico
    package require twapi
}
package require sqlite3

namespace eval compass_server {
    variable version
    variable topdir [file normalize [file dirname [info script]]]

    variable port
    variable portDict ""
    variable interpList ""
    variable internal_maintenance_handler
    variable systemtrayico ""
    variable server_initial_params_dict
    variable myverifp

    variable allow_analysis_users 1    
    variable block_size 200000

#     proc _ { args } {
#         set ret [uplevel 1 ::msgcat::mc $args]
#         regexp {(.*)#C#(.*)} $ret {} ret
#         return $ret
#     }
    #msgcat::mcload [file join $topdir msgs]

}

################################################################################
#    Start & end server
################################################################################

proc compass_server::init_server { args } {
    variable version
    variable interpList
    variable portDict
    
    set optional {
	{ -version version_number NA }
	{ -port port 1375 }
	{ -appdir dir "" }
	{ -portDict dict "" }
	{ -logfile logfile "log.db3" }
	{ -do_allow_analysis_users boolean 1 }
    }
    set compulsory ""
    parse_args $optional $compulsory $args
    
    lassign "" portList interpList
    if { $portDict eq "" } {
	lappend portList $port
	init_server_do $port $appdir $do_allow_analysis_users $logfile
    } else {
	set interpList ""
	dict for "port d" $portDict {
	    lappend portList $port
	    if { [interp exists server$port] } {
		interp delete server$port
	    }
	    set ip [interp create server$port]
	    $ip eval [list lappend auto_path {*}$::auto_path]
	    $ip eval [list package require compass_utils]
	    $ip eval [list compass_server::init_server_do $port [dict get $d appdir] \
		    [dict_getd $d do_allow_analysis_users $do_allow_analysis_users] \
		    [dict_getd $d logfile $logfile]]
	    foreach i [list give_ports_list give_log_data clear_log] {
		interp alias $ip compass_server::${i}G "" compass_server::$i
	    }
	    lappend interpList $ip
	}
	mylog::init [list debug log error]
    }
    set err [catch { manageicontray start $portList } errstring]
    mylog::log [_ "starting server (version %s)" $version]
    if { $err } {
	mylog::log [_ "Could not add icon to icontray (%s)" $errstring]
    }
    check_mail_server_diary
}

proc compass_server::init_server_do { _port _appdir _allow_analysis_users logfile } {
    variable port
    variable appdir
    variable allow_analysis_users
    variable myverifp

    set port $_port
    set appdir [file normalize $_appdir]
    if { $appdir eq "" } {
	error [_ "error: it is necessary to define application directory in -appdir"]
    }
    set allow_analysis_users $_allow_analysis_users

    if { ![file exists $appdir] } {
	file mkdir $appdir
    }
    #cd $appdir
    
    set server_config [file join $appdir server_config]
    if { ![file exists $server_config] } {
	file mkdir $server_config
    }
    if { ![file isdirectory [file join $appdir databases]] } {
	file mkdir [file join $appdir databases]
    }
    if { $logfile ne "" } {
	mylog::init -file [cu::file::jail $server_config $logfile] [list debug log error]
    } else {
	mylog::init ""
    }
    proc ::bgerror { message } {
	mylog::error $::errorInfo
    }
    
    sqlite3 db [file join $server_config conf.db3]

    if { ![table_exists db sessions] } {
	db eval {
	    create table sessions(id integer primary key,username text,type text,
	       connection text,socket text,connected datetime,last_time datetime,priority integer default 2);
	    create index sessions_socket on sessions(socket);

	    create table created_dirs(id integer,dirname text);
	    create index created_dirs_id on created_dirs(id);

	    create table processes(id integer,pid text,cmdline text,dir text);
	    create index processes_id on processes(id);
	}
    }
    set cols [table_columns db dbcommands]
    if { [llength $cols] > 0 && [llength $cols] < 4 } {
	db eval { drop table dbcommands }
	catch { db eval { drop table session_dbcommands } }
    }
    if { ![table_exists db dbcommands] } {
	db eval {
	    create table dbcommands(id integer primary key,session text,
		cmd text,file text);
	    create index dbcommands_session on dbcommands(session);
	}
    }
#     db eval {
#         delete from dbcommands;
#         delete from session_dbcommands;
#     }
    
    if { ![table_exists db mail_server_diary] } {
	db eval {
	    create table mail_server_diary(id integer primary key,mail_from text,mail_to text,db_name text,mytable text,
	    url_pattern text,active boolean);
	}
    }
    sqlite3 db_users [file join $server_config users.db3]

    if { ![table_exists db_users users] } {
	db_users eval {
	    create table users(user text primary key,type text,password text);
	    create index users_user_type on users(user,type);
	}
    }
    if { ![table_exists db_users users_allowed_sqlite_files] } {
	db_users eval {
	    create table users_allowed_sqlite_files(user text,file text);
	    create index users_allowed_sqlite_files_file on users_allowed_sqlite_files(user,file);
	}
    }
    
    restart_check_sessions
    #db eval { delete from sessions }

    comm::comm destroy
    set err [catch { comm::comm new ::comm::comm -listen 1 -local 0 -port $port } errstring]
    if { $err } {
	error [_ "Could not start server at port %s. (%s)" $port $errstring]
    }
    comm::comm hook eval {
	if { [info command db] eq "" } {
	    error [_ "error: Server is stopped"]
	}
	if { [regexp {^session(\d+)\s+(.*)} $buffer {} session buffer] } {
	    set session [db onecolumn { select id from sessions where socket=$fid and id=$session order by priority limit 1 }]
	} elseif { [string match "user_password*" $buffer] } {
	    set session ""
	} else {
	    set session [db onecolumn { select id from sessions where socket=$fid order by priority limit 1 }]
	}
	if { $session ne "" } {
	    return [ip$session eval $buffer]
	} else {
	    set err [catch { compass_server::init_session $fid \
		$id $buffer } ret]
	    #if { $err } { error $::errorInfo }
	    if { $err } { error $ret }
	    return $ret
	}
    }
    comm::comm hook lost {
	compass_server::lost_session $fid
    }
    internal_maintenance
    
    set myverifp [verifp %AUTO% -program compass_server -version 1.0 \
	    -module "compass_server_professional" \
	    -password_dir $server_config -key compass_99_server_1 \
	    ]
    
    set txt [_ "Starting server at port %d. Work dir: %s" $port $appdir]
    mylog::log $txt
    puts $txt
}

proc table_exists { db name } {
    
    set ret [$db eval { select name from sqlite_master where type='table' 
	    and name=$name }]
    if { ![llength $ret] } { return 0 } else { return 1 }
}

proc table_columns { db name } {

    set ret ""
    foreach "- name - - - -" [$db eval "pragma table_info($name)"] {
	lappend ret $name
    }
    return $ret
}

proc compass_server::stop_server {} {
    variable interpList

    mylog::log [_ "Stop server"]
    if { [llength $interpList] == 0 } {
	db close
	db_users close
    } else {
	foreach ip $interpList {
	    $ip eval {
		db close
		db_users close
	    }
	}
    }
}

proc compass_server::end_server {} {
    variable interpList

    manageicontray delete
    mylog::log [_ "Exiting server"]
    if { [llength $interpList] == 0 } {
	db close
	db_users close
    } else {
	foreach ip $interpList {
	    $ip eval {
		db close
		db_users close
	    }
	}
    }
    exit
}

################################################################################
#    Internal maintenance
################################################################################

proc compass_server::internal_maintenance {} {
    variable internal_maintenance_handler

    if { [info exists internal_maintenance_handler] } {
	after cancel $internal_maintenance_handler
    }
    set internal_maintenance_handler [after 5000 \
	    compass_server::internal_maintenance_do]
}

proc compass_server::internal_maintenance_do {} {
    variable internal_maintenance_handler
    
    unset -nocomplain internal_maintenance_handler

    foreach pid [db eval { select pid from processes }] {
	if { ![process_exists $pid] } {
	    db eval { delete from processes where pid=$pid }
	}
    }
    foreach dir [db eval { select dirname from created_dirs }] {
	if { ![file exists $dir] } {
	    db eval { delete from created_dirs where dirname=$dir }
	}
    }
    foreach "session username type connection" [db eval {
	    select id,username,type,connection
	    from sessions where socket is null and
	    julianday('now')-julianday(last_time) > 7 }] {
	set log_text [_ "Auto finished session %s-%s-%s-%s" $session $username \
		$type $connection]
	#mylog::debug "$session $username $type $connection"
	end_session -log_text $log_text $session
    }
    internal_maintenance
}

proc compass_server::restart_check_sessions {} {

    db eval { delete from sessions where socket is null }
    
    foreach "session username type connection socket" [db eval {
	    select id,username,type,connection,socket
	    from sessions where socket is not null}] {

	create_interps_for_session $session $type
	db eval { update sessions set socket=NULL,
	    last_time=strftime("%Y-%m-%d %H:%M:%f",'now')
	    where id=$session }
	
	mylog::log [_ "Auto restarted session %s-%s-%s-%s" $session $username \
		$type $connection]
    }
}

################################################################################
#    Start & end session
################################################################################

proc compass_server::lost_session { args } {

    set optional {
	{ -log_text text "" }
    }
    set compulsory "socket"
    parse_args $optional $compulsory $args

    foreach "session username type connection" [db eval {
	    select id,username,type,connection
	    from sessions where socket=$socket }] {
	db eval { update sessions set socket=NULL,
	    last_time=strftime("%Y-%m-%d %H:%M:%f",'now')
	    where id=$session }

	db eval { select id from dbcommands where session=$session } {
	    set cmd_out ::compass_server::rdb$id
	    catch { $cmd_out close }
	}
	db eval { delete from dbcommands where session=$session }

	if { $log_text eq "" } {
	    set log_text [_ "Lost session %s-%s-%s-%s" $session $username $type \
		    $connection]
	}
	mylog::log $log_text
    }
}

proc compass_server::end_session { args } {

    set optional {
	{ -log_text text "" }
    }
    set compulsory "session"
    parse_args $optional $compulsory $args

    lassign  [db eval { select username,type,connection
	    from sessions where id=$session }]  username type connection

    interp delete ip$session
    db eval { delete from sessions where id=$session }

    db eval { select pid,cmdline from processes where id=$session } {
	catch { end_process $pid -force -wait 1000 }
    }
    db eval { delete from processes where id=$session }
    db eval { select dirname from created_dirs where id=$session } {
	catch { file delete -force $dirname }
    }
    db eval { delete from created_dirs where id=$session }

    db eval { select id from dbcommands where session=$session } {
	# in many cases, this is already done in lost_session
	set cmd_out ::compass_server::rdb$id
	catch { $cmd_out close }
    }
    db eval { delete from dbcommands where session=$session }

    if { $log_text eq "" } {
	set log_text [_ "Ended session %s-%s-%s-%s" $session $username $type \
		$connection]
    }
    mylog::log $log_text
}

proc compass_server::init_session { socket id buffer } {

    set err [catch { lindex $buffer 0 } cmd]
    if { $err || $cmd ne "user_password" } {
	return -code error [_ "rejected connection"]
    }
    set args [lrange $buffer 1 end]

    if { [llength $args] == 3 } {
	# for compatibility
	set args [list -session [lindex $args end]]
	lappend args {*}[lrange $buffer 1 2]
    }
    set optional {
	{ -user_type_strict boolean 0 }
	{ -user_type type "" }
	{ -accept_no_users boolean 1 }
	{ -session session_name "" }
    }
    set compulsory "username key"
    parse_args $optional $compulsory $args

    mylog::log [_ "starting session for user '%s'" $username"]
    lassign  [verify_user -user_type $user_type -user_type_strict \
	    $user_type_strict -accept_no_users $accept_no_users \
	    $username $key] isok user_typeL
    
    if { $user_type ne "database" } {
	# in databases, administrator connects with usertype "administrator" and user with usertype "database"
	# it is important to restrict usertype "database" to only access "databases" subdirectory
	set user_type $user_typeL
    }
    if { $isok == 0 } {
	mylog::log [_ "rejected connection from '%s' user '%s' user_type '%s'. bad username/password" \
		$id $username $user_type]
	return -code error [_ "rejected connection. bad username/password"]
    }
    if { $session ne "" } {
	db eval { update sessions set socket=$socket,last_time=NULL
	    where id=$session }
	if { ![db changes] } {
	    mylog::log [_ "rejected connection. session does not exist"]
	    return -code error [_ "rejected connection. session does not exist"]
	}
	db eval { select id,cmd,file from dbcommands where session=$session } {
	    set err [catch { restricted_sqlite3  databases -cmd_id $id $session $cmd $file } errstring]
	    if { $err } {
		set txt [_ "Error restarting session '%s' (%s)" $session \
		        $errstring] 
		mylog::log $txt
		return -code error $txt
	    }
	}
	mylog::log [_ "reconnected to session '%s'" $session]
	return $session
    }

    db eval { insert into sessions (username,type,connection,socket,connected)
	values ($username,$user_type,$id,$socket,strftime("%Y-%m-%d %H:%M:%f",'now'))
    }
    set session [db last_insert_rowid]
    create_interps_for_session $session $user_type
    
    mylog::log [_ "accepted connection from '%s' user '%s' type '%s'. Created session '%s'"\
	    $id $username $user_type $session]
    return $session
}

proc compass_server::create_interps_for_session { session user_type } {
    variable appdir
    
    switch $user_type {
	database {
	    interp create -safe ip$session
	    foreach f [list sqlite3 glob file] {
		interp alias ip$session $f "" compass_server::restricted_$f databases $session
	    }
	    foreach f [list end_session] {
		interp alias ip$session $f "" compass_server::$f $session
	    }
	}
	no_users {
	    interp create -safe ip$session
	    foreach f [list glob file] {
		interp alias ip$session $f "" compass_server::restricted_$f no_users $session
	    }
	    foreach f [list add_user remove_user manage_user_type \
		    give_existing_users give_allowed_user_types \
		    manage_users_allowed_sqlite_files \
		    test_server_password give_server_sysinfo test_server_password_and_enter] {
		interp alias ip$session $f "" compass_server::$f
	    }
	    foreach f [list give_ports_list give_log_data clear_log] {
		if { [info command ::compass_server::${f}G] ne "" } {
		    interp alias ip$session $f ""  ::compass_server::${f}G
		} else {
		    interp alias ip$session $f ""  ::compass_server::$f
		}
	    }
	    foreach f [list end_session] {
		interp alias ip$session $f "" compass_server::$f $session
	    }
	}
	all - administrator - analysis {
	    interp create ip$session
	    foreach f [list _recieve_file _send_file file_exists file_save_in_cache end_process \
		    process_exists processes_info sessions_list  server_version server_platform] {
		interp alias ip$session $f "" compass_server::$f
	    }
	    if { $user_type in [list all administrator] } {
		foreach f [list  add_user remove_user manage_user_type \
		        give_existing_users \
		        give_allowed_user_types manage_users_allowed_sqlite_files \
		        test_server_password give_server_sysinfo test_server_password_and_enter] {
		    interp alias ip$session $f "" compass_server::$f
		}
		foreach f [list give_ports_list give_log_data clear_log] {
		    if { [info command ::compass_server::${f}G] ne "" } {
		        interp alias ip$session $f ""  ::compass_server::${f}G
		    } else {
		        interp alias ip$session $f ""  ::compass_server::$f
		    }
		}
	    }
	    foreach f [list sqlite3] {
		interp alias ip$session $f "" compass_server::restricted_$f analysis $session
	    }
	    foreach f [list glob file] {
		if { $user_type in [list all administrator] } {
		    interp eval ip$session [list rename $f _$f]
		} else {
		    interp eval ip$session [list rename $f ""]
		}
		interp alias ip$session $f "" compass_server::restricted_$f analysis $session
	    }
	    foreach f [list create_remote_new_dir exec_in_dir end_session] {
		interp alias ip$session $f "" compass_server::$f $session
	    }
	    interp alias ip$session end_another_session "" compass_server::end_session
	    interp alias ip$session ls ip$session glob *
	    ip$session eval [list set auto_path $::auto_path]
	}
    }
    interp eval ip$session [list proc user_password args {}]
    interp eval ip$session [list proc user_type {} [list return $user_type]]
}

################################################################################
#    Server GUI
################################################################################

proc compass_server::server_gui {} {
    variable port
    variable portDict
    variable interpList

    cu::init_tile_styles
    package require dialogwin

    set portList ""
    if { $portDict eq "" } {
	lappend portList $port
    } else {
	set portList [dict keys $portDict]
    }
    wm withdraw .
    set w .gui_server
    destroy $w
    set w [dialogwin_snit $w -title [_ "Compass server"] \
	    -grab 0 \
	    -callback [list compass_server::server_gui_do] -okname [_ "Close"] \
	    -cancelname [_ "Exit"]]
    set f [$w giveframe]
    
    if { [llength $portList] > 1 } {
	set txt [_ "Started server at ports: %s" [join $portList ,]]
    } else {
	set txt [_ "Started server at port: %s" [join $portList ,]]
    }
    ttk::label $f.l -text $txt
    
    if { [llength $portList] > 1 } {
	ttk::label $f.lp -text [_ "Select port"]:
	set portListF [linsert $portList 0 [_ "Global"]]
	ttk::combobox $f.cb1 -textvariable [$w give_uservar port] -values $portListF \
	    -state readonly -width 6
	$w set_uservar_value port [lindex $portList 0]
	set d [dict create [_ "Global"] -$f.b1]
	$w enable_disable_on_key port $d
    }
    ttk::button $f.b1 -text [_ "Add/remove user"] -width 20 -command \
	[list compass_server::server_gui_cmd $w add_user]
    ttk::button $f.b2 -text [_ "Clear log"] -width 20 -command \
	[list compass_server::server_gui_cmd $w clear_log]
    ttk::button $f.b3 -text [_ "View log"] -width 20 -command \
	[list compass_server::server_gui_cmd $w view_log]

    grid $f.l - -sticky w -padx 10 -pady "20 20"
    if { [llength $portList] > 1 } {
	grid $f.lp $f.cb1 -padx 2 -pady 2 -sticky w
	grid configure $f.lp -padx "10 0"
    }
    grid $f.b1 - -sticky w -padx 10 -pady 2
    grid $f.b2 - -sticky w -padx 10 -pady 2
    grid $f.b3 - -sticky w -padx 10 -pady "2 20"
    grid columnconfigure $f 1 -weight 1

    tk::TabToWindow $f.b1
    bind [winfo toplevel $f] <Return> [list $w invokeok]

    $w createwindow
}

proc compass_server::server_gui_do { w } {

    switch -- [$w giveaction] {
	-1 {
	    destroy $w
	}
	0 {
	    end_server
	}
	1 {
	    destroy $w
	}
    }
}

proc compass_server::server_gui_cmd { w what } {

    switch $what {
	add_user {
	    if { [$w exists_uservar port] } {
		set port [$w give_uservar_value port]
		server$port eval package require Tk
		server$port eval wm withdraw .
		server$port eval [list compass_server::add_user_gui ""]
	    } else {
		add_user_gui $w
	    }
	}
	clear_log {
	    if { [$w exists_uservar port] && [$w give_uservar_value port] ne [_ "Global"] } {
		set port [$w give_uservar_value port]
		server$port eval package require Tk
		server$port eval wm withdraw .
		server$port eval [list mylog::clear_log -confirm 1]
	    } else {
		mylog::clear_log -confirm 1 -parent $w
	    }
	}
	view_log {
	    if { [$w exists_uservar port] && [$w give_uservar_value port] ne [_ "Global"] } {
		set port [$w give_uservar_value port]
		server$port eval package require Tk
		server$port eval wm withdraw .
		server$port eval [list mylog::view_log]
	    } else {
		mylog::view_log -parent $w
	    }
	}
    }
}

proc compass_server::view_log { appdir } {
    
    package require Tk
    wm withdraw .
    mylog::_view_log_fulltktree -extlogfile [file join $appdir server_config log.db3] \
	-destroy_handler "compass_server::view_log_exit;#"
}

proc compass_server::view_log_exit {} {
    after idle exit
}

proc compass_server::is_administrative_user_active {} {
    variable administrative_user_active
    
    set ret [db_users eval { select user from users where type in ('administrator','all') }]
    if { $ret eq "" } {
	set administrative_user_active [list "" [clock seconds]]
	return 2
    }
    if { ![info exists administrative_user_active] } {
	return 0
    }
    lassign $administrative_user_active user time
    if { [clock seconds] - $time > 15*60 } {
	return 0
    } else {
	return 1
    }
}

proc compass_server::administrative_user_update {} {
    variable administrative_user_active
    
    if { [is_administrative_user_active] == 0 } {
	return
    }
    lset administrative_user_active 1 [clock seconds]
}

proc compass_server::administrative_user_set { user } {
    variable administrative_user_active

    set administrative_user_active [list $user [clock seconds]]
}

proc compass_server::ask_administrative_user_gui { parent } {

    if { [is_administrative_user_active] } {
	administrative_user_update
	return
    }
    package require dialogwin

    if { $parent eq "." } { set parent "" }
    
    destroy $parent.adduser
    set w [dialogwin_snit $parent.adduser -title [_ "Administrative user and password"] \
	    -entrytext [_ "Enter administrative user and password:"]]
    set f [$w giveframe]

    set users [give_existing_users -for_administrator 1]
    ttk::label $f.l2 -text [_ "User"]:
    ttk::combobox $f.e2 -width 30 -textvariable [$w give_uservar user] \
	-values $users
    $w set_uservar_value user [lindex $users 0]
    
    ttk::label $f.l3 -text [_ "Password"]:
    ttk::entry $f.e3 -textvariable [$w give_uservar password ""] -show *

    grid $f.l2 $f.e2 -sticky w -padx 3 -pady 2
    grid $f.l3 $f.e3 -sticky w -padx 3 -pady 2

    grid configure $f.e2 $f.e3 -sticky ew
    grid columnconfigure $f 1 -weight 1

    if { ![llength $users] } {
	tk::TabToWindow $f.e2
    } else {
	tk::TabToWindow $f.e3
    }
    bind $w <Return> [list $w invokeok]
    set action [$w createwindow]
    while 1 {
	if { $action <= 0 } {
	    destroy $w
	    return
	}
	foreach i [list user password] {
	    set $i [string trim [$w give_uservar_value $i]]
	}
	set key [cu::sha1 [list $user $password]]
	#append key "" ;# this is to convert the bytearray into text
	set ret [db_users onecolumn { select user from users where user=$user
		and password=$key and type in ('administrator','all') }]
      if { $ret eq "" } {
	    snit_messageBox -message [_ "User or password not correct"] -parent $w
	} else {
	    break
	}
	set action [$w waitforwindow]
    }
    administrative_user_set $user    
    destroy $w
}

proc compass_server::add_user_gui { parent } {

    package require dialogwin
    
    ask_administrative_user_gui $parent
    set user_active [is_administrative_user_active]
    if { !$user_active } {
	return
    }
    if { $parent eq "." } { set parent "" }
    destroy $parent.adduser
    set w [dialogwin_snit $parent.adduser -title [_ "Add user"]]
    set f [$w giveframe]
	
    ttk::label $f.l2 -text [_ User:]
    ttk::combobox $f.e2 -width 30 -textvariable [$w give_uservar user ""] \
	-values [give_existing_users]
    
    ttk::label $f.l3 -text [_ Password:]
    ttk::entry $f.e3 -textvariable [$w give_uservar password ""] -show *

    ttk::label $f.l4 -text [_ "Repeat password:"]
    ttk::entry $f.e4 -textvariable [$w give_uservar password2 ""] -show *
	
    ttk::label $f.l5 -text [_ "User type:"]
    set d [dict create administrator [_ Administrator] analysis [_ Analysis] \
	    database [_ Database] all [_ All]]
    lassign [give_allowed_user_types] user_types user_types_dict
    cu::combobox $f.e5 -width 30 -textvariable [$w give_uservar user_type] \
	-values $user_types -dict $user_types_dict -state readonly
    $w set_uservar_value user_type [lindex $user_types end]
    
    ttk::checkbutton $f.cb1 -text [_ "Remove user"] -variable \
	[$w give_uservar removeuser 0]
    
    if { $user_active == 2 } {
	$w set_uservar_value user_type administrator
	#$f.e5 state disabled
	$f.cb1 state disabled
    }

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
	remove_user [$w give_uservar_value user]
    } else {
	add_user [$w give_uservar_value user] [$w give_uservar_value password] \
	    [$w give_uservar_value user_type]
    }
    destroy $w
    administrative_user_update
}

proc compass_server::manageicontray { what args } {
    variable topdir
    variable systemtrayico
    
    switch $what {
	start {
	    lassign $args portList
	    if { $::tcl_platform(platform) ne "windows" } {
		package require img::png
		set ico [file join $topdir images Compasser22.png]
	    } else {
		set ico [file join $topdir images Compasser.ico]
	    }
	    set titleList ""
	    foreach port $portList {
		lappend titleList  [_ "Compass server (port: %s)" $port]
	    }
	    set title [join $titleList "\n"]
	    if { $::tcl_platform(platform) eq "windows" } {
		if { [catch { package require Winico }] } { return }
		set systemtrayico [winico create $ico]
		winico taskbar add $systemtrayico -text $title \
		    -callback [list compass_server::manageicontray callback %m %i %w %l %x %y]
	    } else {
		if { [catch { package present Tk }] } { return }
		package require compass_utils::img
		if { ![cu::icontray_exists] } { return }

		set w [cu::icontray .icontray]
		set img [image create photo -file $ico]
		pack [label $w.b -image $img -bd 0]
		set systemtrayico $w.b
		cu::img::shape set $w  photo $img
		bind $w.b <1> [list compass_server::manageicontray callback WM_LBUTTONUP - - - %X %Y]
		bind $w.b <ButtonRelease-3> [list compass_server::manageicontray callback WM_RBUTTONUP - - - %X %Y]
		cu::tooltip register $systemtrayico $title
	    }
#             mylog::debugoff [_ "starting icon try"]
	}
	callback {
	    lassign $args message ico wparam lparam x y
	    switch $message {
		WM_LBUTTONUP {
		    compass_server::server_gui
		}
		WM_RBUTTONDOWN {
		    #nothing
		}
		WM_RBUTTONUP {
		    catch { destroy .traymenu }
		    set m [menu .traymenu -tearoff 0]
		    $m add command -label [_ "Deiconify"] -command \
		        [list compass_server::server_gui]
		    $m add separator
		    $m add command -label [_ "Exit"] -command \
		        [list compass_server::end_server]
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
    }
}

################################################################################
#    User management
################################################################################

proc compass_server::init_for_add_remove_user { appdir } {
    variable allow_analysis_users
    
    if { $appdir eq "" } {
	error [_ "error: it is necessary to define application directory in -appdir"]
    }
    set allow_analysis_users 1

    if { ![file exists $appdir] } {
	file mkdir $appdir
    }
    set server_config [file join $appdir server_config]
    if { ![file exists $server_config] } {
	file mkdir $server_config
    }
    mylog::init ""
    sqlite3 db_users [file join $server_config users.db3]
}

proc compass_server::add_user { username password { user_type all } } {
    
    if { [db_users eval { select count(user) from users }] == 0 } {
	set has_users 0
    } else {
	set has_users 1
    }
    if { !$has_users && $user_type ni [list "administrator" "all"] } {
	error [_ "Error: First users needs to be administrator or all"]
    }
    lassign [give_allowed_user_types] user_types -
    if { $user_type ni $user_types } {
	error [_ "Error: user type is not correct"]
    }
    if { $password ne "" } {
	set key [cu::sha1 [list $username $password]]
	#append key "" ;# this is to convert the bytearray into text
	db_users eval { replace into users (user,type,password)
	    values($username,$user_type,$key) }
    } else {
	db_users eval { update users set type=$user_type where user=$username }
    }
    set txt [_ "Added or modified user '%s', type='%s'" $username $user_type]
    mylog::log $txt
    return $txt
}

proc compass_server::remove_user { username } {

    db_users eval { delete from users where user=$username }
    if { [db_users changes] } {
	set txt [_ "Removed user '%s'" $username]
    } else {
	set txt [_ "No user '%s' found in database" $username]
    }
    mylog::log $txt
    return $txt
}

proc compass_server::manage_user_type { what username args } {
    switch $what {
	get {
	    return [db_users  onecolumn { select type from users where user=$username }]
	}
	set {
	    lassign $args user_type
	    db_users eval { update users set type=$user_type where user=$username }
	}
    }
}

proc compass_server::verify_user { args } {

    set optional {
	{ -user_type_strict boolean 0 }
	{ -user_type type "" }
	{ -accept_no_users boolean 1 }
    }
    set compulsory "username key"
    parse_args $optional $compulsory $args

    set ret [db_users eval { select user,type from users where user=$username
	    and password=$key and type=$user_type }]
    if { $ret eq "" && !$user_type_strict } {
	if { $user_type eq "" } {
	    set ret [db_users eval { select user,type from users where user=$username
		    and password=$key }]
	} else {
	    set ret [db_users eval { select user,type from users where user=$username
		    and password=$key and type='all' }]
	    #if { [llength $ret] } { lset ret 1 $user_type }
	}
    }
    if { $ret eq "" } {
	if { $accept_no_users && [db_users eval { select count(user) from users }] == 0 } {
	    return [list 1 no_users]
	}
	return [list 0 ""]
    }
    return [list 1 [lindex $ret 1]]
}

proc compass_server::give_existing_users { args } {
    
    set optional {
	{ -for_database boolean 0 }
	{ -for_administrator boolean 0 }
    }
    set compulsory ""
    parse_args $optional $compulsory $args

    if { $for_administrator } {
	return [db_users eval { select distinct user from users where type in ('administrator','all') order by user }]
    } elseif { $for_database } {
	return [db_users eval { select distinct user from users where type in ('all','administrator','database') order by user }]
    } else {
	return [db_users eval { select distinct user from users order by user }]
    }
}

# proc compass_server::give_existing_administrative_users {} {
#   
#     return [db_users eval { select distinct user from users where type='administrator' }]
# }

proc compass_server::give_allowed_user_types {} {
    variable allow_analysis_users
    
    set d [dict create administrator [_ Administrator] analysis [_ Analysis] \
	    database [_ Database] all [_ All]]
    
    set ret [list all administrator]
    if { [give_existing_users] eq "" } { return [list $ret $d] }
    
    lappend ret database
    if { $allow_analysis_users } {
	lappend ret analysis
    }
    return  [list $ret $d] 
}

proc compass_server::manage_users_allowed_sqlite_files { what user args } {
    
    switch $what {
	set {
	    db_users eval { begin }
	    db_users eval { delete from users_allowed_sqlite_files where user=$user }
	    foreach file $args {
		db_users eval { insert into users_allowed_sqlite_files (user,file) values($user,$file) }
	    }
	    db_users eval { commit }
	    mylog::log [_ "Set database files for user '%s' to '%s'" $user $args]
	}
	get {
	    return [db_users eval { select file from users_allowed_sqlite_files where user=$user order by file }]
	}
	clear {
	    db_users eval { delete from users_allowed_sqlite_files where user=$user }
	    mylog::log [_ "Clear database files for user '%s'" $user]
	}
	default {
	    db_users eval { rollback }
	    error "error in compass_server::manage_users_allowed_sqlite_files"
	}
    }
}

################################################################################
#    Support procs for clients
################################################################################

proc compass_server::create_remote_new_dir { session basename } {
    variable appdir
    
    set ic ""
    while 1 {
	set sdir $basename$ic
	set bdir [cu::file::jail $appdir $sdir]
	if { ![file exists $bdir] } { break }
	if { $ic eq "" } { set ic 1 } else { incr ic }
    }
    file mkdir $bdir
    mylog::log [_ "Creating directory '%s'" $sdir]
    db eval { insert into created_dirs values($session,$bdir) }
    return $sdir
}

proc compass_server::exec_in_dir { session dir args } {
    variable appdir
    
    mylog::log [_ "Executing '$args' in '$dir'"]
    set pwd [pwd]
    set dir [cu::file::jail $appdir $dir]
    cd $dir
    set exec [lindex $args 0]
    if { [regexp {^\.[/\\]\w} $exec] } {
	# ok
    } elseif { [regexp {^\w} $exec] } {
	set exec "./$exec"
    } else {
	error "error in exec_in_dir. path to executable must be relative and local"
    }
    if { $::tcl_platform(platform) eq "unix" } {
	if { ![file executable $exec] } {
	    file attributes $exec -permissions +x
	}
    }
    set err [catch { exec {*}$args } ret]
    cd $pwd
    if { $err } {
	error $ret $::errorInfo
    } else {
	db eval { insert into processes values($session,$ret,$args,$dir) }
	return $ret
    }
}

proc compass_server::end_process { args } {
    set optional {
	{ -force boolean 0 }
    }
    set compulsory "pid"
    parse_args $optional $compulsory $args

    if { $::tcl_platform(platform) eq "windows" } {
	if { $force } {
	    set ret [twapi::end_process $pid -force -wait 1000]
	} else {
	    set ret [twapi::end_process $pid]
	}
    } else {
	if { $force } {
	    exec kill -9 $pid
	} else {
	    exec kill $pid
	}
    }
    mylog::log [_ "killed process %s" $pid]
    return $ret
}

proc compass_server::process_exists { pid } {
    if { $::tcl_platform(platform) eq "windows" } {
	set err [catch { twapi::process_exists $pid } ret]
    } else {
	set err [catch { exec ps -p $pid -o comm= } ret]
	set ret 1
    }
    if { $err } { set ret 0 }
    return $ret
}

proc compass_server::_recieve_file { args } {
    variable appdir
    
    set optional {
	{ -binary boolean 0 }
    }
    set compulsory "file data status fout"
    parse_args $optional $compulsory $args

    if { $status eq "start" } {
	mylog::log [_ "recieving file '%s'" $file]
	set file [cu::file::jail $appdir $file]
	set fout [open $file w]
	if { $binary } { fconfigure $fout -translation binary }
    }

    puts -nonewline $fout [cu::inflate $data]

    if { $status eq "end" } {
	close $fout
	mylog::log [_ "recieved file"]
    }
    return $fout
}

proc compass_server::file_exists { file sha1 } {
    variable appdir

    set fileF [cu::file::jail $appdir $file]
    if { [file exists $fileF] } {
	set needs_copy 0
    } else {
	set fileF [file join $appdir files_cache [file tail $file]]
	set needs_copy 1
    }
    set err [catch { cu::file::sha1 $fileF } sha1_alt]
    if { $err || $sha1 ne $sha1_alt } {
	set exists 0
    } else {
	set exists 1
	
	if { $needs_copy } {
	    if { $::tcl_platform(platform) eq "unix" } {
		file link -symbolic [cu::file::jail $appdir $file] $fileF
	    } else {
		file copy -force $fileF [cu::file::jail $appdir $file]
	    }
	}
    }
    mylog::log [_ "file exists '%s': %s" $file $exists]
    return $exists
}

proc compass_server::file_save_in_cache { file } {
    variable appdir

    set fileF [cu::file::jail $appdir $file]
    if { ![file isfile $fileF] } {
	error [_ "File '%s' is not a regular file" $file]
    }
    file mkdir [file join $appdir files_cache]
    file copy -force $fileF [file join $appdir files_cache [file tail $file]]
    mylog::log [_ "file_save_in_cache '%s'" $file]
}

proc compass_server::_send_file { args } {
    variable block_size
    variable appdir

    set optional {
	{ -binary boolean 0 }
	{ -nocomplain boolean 0 }
    }
    set compulsory "file status fin"
    parse_args $optional $compulsory $args

    if { $status eq "start" } {
	mylog::log [_ "sending file '%s'" $file]
	set file [cu::file::jail $appdir $file]
	set err [catch { open $file r } fin]
	if { $err } {
	    if { $nocomplain } {
		mylog::log [_ "failed sending file '%s'" $file]
		return [list "" end ""]
	    }
	    error $fin
	}
	if { $binary } { fconfigure $fin -translation binary }
    }
    set data [cu::deflate [read $fin $block_size]]

    if { [eof $fin] } {
	close $fin
	set status end
    } else {
	set status continue
    }
    return [list $data $status $fin]
}

proc compass_server::server_version {} {
    variable version

    return $version
}

proc compass_server::server_platform {} {
    variable version

    package require platform
    return [platform::identify]
}

proc compass_server::test_server_password {} {
    variable myverifp

    # isok (VERSION_ID) message
    return [$myverifp test_password]
}

proc compass_server::give_server_sysinfo {} {
    variable myverifp

    return [vp_getsysinfo]
}

proc compass_server::test_server_password_and_enter { name os sysinfo password usb_file } {
    variable myverifp

    set ret [$myverifp test_password_and_enter $name $os $sysinfo $password $usb_file]
    mylog::log [_ "Enter server password. Result=%s" $ret]
    return $ret
}

proc compass_server::sessions_list {} {

    mylog::log [_ "Requesting sessions list"]

    set fields [list [_ Id] [_ Username] [_ Type] [_ Connection] [_ Active] [_ Date] \
	    [_ Directories] [_ Processes]]
    set ret [list $fields]
 
    set item ""
    foreach "dirs processes" [list "" ""] break
    db eval { select s.id,s.username,s.type,s.connection,s.socket,s.connected,
	d.dirname,p.pid from sessions as s
	left join created_dirs as d on s.id=d.id
	left join processes as p on s.id=p.id
    } {
	if { $id ne [lindex $item 0] } {
	    if { $item ne "" } { lappend ret $item }
	    foreach "dirs processes" [list "" ""] break
	}
	set dirname [file tail $dirname]
	if { $dirname ne "" && [lsearch -exact $dirs $dirname] == -1 } {
	    lappend dirs $dirname
	}
	if { $pid ne "" && [lsearch -exact $processes $pid] == -1 } {
	    lappend processes $pid
	}
	if { $socket ne "" } { set active 1 } else { set active 0 }
	set item [list $id $username $type $connection $active $connected \
		$dirs $processes]
    }
    if { $item ne "" } { lappend ret $item }
    return $ret
}

proc compass_server::process_info { pid } {

    lassign "" pagefilebytes workingset elapsedtime error
    if { $::tcl_platform(platform) eq "windows" } {
	set err [catch { twapi::get_process_info $pid -pagefilebytes \
		    -workingset -elapsedtime } v]
	if { !$err && $v ne "" } {
	    set pagefilebytes [dict get $v -pagefilebytes]
	    set workingset [dict get $v -workingset]
	    set elapsedtime [dict get $v -elapsedtime]
	} else {
	    set error "error in retrieving info for process $pid ($v)\n"
	}
    } else {
	set err [catch { exec ps -p $pid --no-headers -o rss -o vsz -o etime } ret]
	if { !$err } {
	    lassign $ret workingset pagefilebytes elapsedtime
	    set workingset [expr {$workingset*1024}]
	    set pagefilebytes [expr {$pagefilebytes*1024}]
	} else {
	    set error [_ "error in retrieving info for process %s (%s)" $pid $ret]
	}
    }
    return [list $pagefilebytes $workingset $elapsedtime $error]
}

proc compass_server::processes_info { args } {

    mylog::log [_ "Requesting processes info"]

    set optional {
	{ -pid pid "" }
    }
    set compulsory ""
    parse_args $optional $compulsory $args

    set ret ""
    if { $::tcl_platform(platform) eq "windows" } {
	dict set ret totalphysical [lindex [twapi::get_memory_info \
		    -totalphysical] 1]
    } else {
	lassign [exec grep MemTotal /proc/meminfo]  - mem unit
	switch -- $unit {
	    kB { set mem [expr {1024*$mem}] }
	    mB { set mem [expr {1024*1024*$mem}] }
	}
	dict set ret totalphysical $mem
    }
    dict set ret pagefilebytes_total 0
    dict set ret workingset_total 0
    db eval { select pid as pid_in from processes } {
	lassign [process_info $pid_in] pagefilebytes workingset elapsedtime error
	#mylog::log [_ "pid_in %s pagefilebytes=%s error=%s" $pid_in $pagefilebytes $error]
	if { $error eq "" } {
	    dict incr ret pagefilebytes_total $pagefilebytes
	    dict incr ret workingset_total $workingset
	} else {
	    dict append ret error [_ "error in retrieving info for process %s (%s)\n" $pid_in $error]
	}
    }
    if { $pid ne "" } {
	lassign [process_info $pid] pagefilebytes workingset elapsedtime error
	#mylog::log [_ "pid %s pagefilebytes=%s error=%s" $pid $pagefilebytes $error]
	if { $error eq "" } {
	    dict set ret pagefilebytes $pagefilebytes
	    dict set ret workingset $workingset
	    dict set ret elapsedtime $elapsedtime
	} else {
	    dict append ret error [_ "error in retrieving info for process %s (%s)\n" $pid $error]
	}
    }
    return $ret
}

proc compass_server::restricted_glob { what session args } {
    variable appdir
    
    set optional {
	{ -directory directory "" }
	{ -nocomplain "" 0 }
	{ -types typeList "" }
    }
    set compulsory pattern
    parse_args $optional $compulsory $args
    
    set user [db onecolumn { select username from sessions where id=$session }]
    set allowed_files [db_users eval { select file from users_allowed_sqlite_files where user=$user  }]
    set is_restricted [llength $allowed_files]

    switch $what {
	databases { set jail_dir [file normalize [file join $appdir databases]] }
	default { set jail_dir [file normalize $appdir] }
    }
    
    if { $pattern ne "*" } {
	error "error: pattern for restricted glob can only be '*'"
    }
    set directory [cu::file::jail $jail_dir $directory]
    set ret ""
    foreach i [glob -directory $directory -types $types {*}[expr {($nocomplain)?"-nocomplain":""}] $pattern] {
	set file [cu::file::stripPath $jail_dir $i]
	if { !$is_restricted || [file isdirectory $i] || $file in $allowed_files } {
	    lappend ret $file
	}
    }
    return $ret
}

proc compass_server::restricted_file { what session cmd0 args } {
    variable appdir

    switch $what {
	databases { set jail_dir [file normalize [file join $appdir databases]] }
	default { set jail_dir [file normalize $appdir] }
    }
    
    set user [db onecolumn { select username from sessions where id=$session }]
    set allowed_files [db_users eval { select file from users_allowed_sqlite_files where user=$user  }]
    set is_restricted [llength $allowed_files]

    switch $cmd0 {
	exists {
	    set name [cu::file::jail $jail_dir [lindex $args 0]]
	    if { $is_restricted && [cu::file::stripPath $jail_dir $name] ni $allowed_files } {
		return 0
	    }
	    return [file exists $name]
	}
	mkdir {
	    if { $is_restricted } {
		error [_ "Error a restricted user cannot created directories"]
	    }
	    set cmd [list file mkdir]
	    foreach dir $args {
		lappend cmd [cu::file::jail $jail_dir $dir]
	    }
	    return [eval $cmd]
	}
	rename {
	    set optional {
		{ -force "" 0 }
	    }
	    set compulsory "source target"
	    parse_args $optional $compulsory $args
	    
	    if { $is_restricted } {
		error [_ "Error a restricted user cannot rename databases"]
	    }
	    foreach i [list source target] {
		set $i [cu::file::jail $jail_dir [set $i]]
	    }
	    if { $force } {
		return [file rename -force -- $source $target]
	    } else {
		return [file rename -- $source $target]
	    }
	}
	delete {
	    set optional {
		{ -force "" 0 }
	    }
	    set compulsory ""
	    set ret [parse_args -raise_compulsory_error 0 $optional $compulsory $args]
	    
	    if { $is_restricted } {
		error [_ "Error a restricted user cannot delete databases"]
	    }
	    set cmd [list file delete]
	    if { $force } { lappend cmd -force }
	    lappend cmd --
	    foreach pathname $ret {
		lappend cmd [cu::file::jail $jail_dir $pathname]
	    }
	    return [eval $cmd]
	}
	mtime {
	    set name [cu::file::jail $jail_dir [lindex $args 0]]
	    if { $is_restricted && [cu::file::stripPath $jail_dir $name] ni $allowed_files } {
		return 0
	    }
	    return [file mtime $name]
	}
	default {
	    error "error: restricted_file does not implement '$cmd0' subcommand"
	}
    }
}

proc compass_server::restricted_sqlite3 { what args } {
    variable appdir

    set optional {
	{ -cmd_id id "" }
    }
    set compulsory "session cmd name"
    parse_args $optional $compulsory $args

    if { $name eq ":memory:" } {
	error "error: memory databases not allowed"
    }
    set user [db onecolumn { select username from sessions where id=$session }]
    set allowed_files [db_users eval { select file from users_allowed_sqlite_files where user=$user  }]
    set is_restricted [llength $allowed_files]

    if { $is_restricted && $name ni $allowed_files } {
    error [_ "Error. File '%s' is not allowed for user '%s'" $name $user]
    }
    #mylog::log user=$user--name=$name---allowed_files=$allowed_files
    
    switch $what {
	databases { set jail_dir [file normalize [file join $appdir databases]] }
	default { set jail_dir [file normalize $appdir] }
    }
    set nameF [cu::file::stripPath [pwd] [cu::file::jail $jail_dir $name]]

    db eval { begin }
    if { $cmd_id eq "" } {
	db eval { insert into dbcommands values(null,$session,$cmd,$name) }
	set cmd_id [db last_insert_rowid]
    }
    set cmd_out ::compass_server::rdb$cmd_id
    set err [catch { sqlite3 $cmd_out $nameF } ret]
    if { $err } {
	db eval { rollback }
	error "error opening '$nameF' ($ret)"
    }
    db eval { commit }
    $cmd_out timeout 1000
    # temporally disabled due to a bug in sqlite 3.6.22
    #$cmd_out authorizer compass_server::_restricted_sqlite3_auth
    ip$session alias $cmd compass_server::_restricted_sqlite3_cmd $cmd_out
}

proc compass_server::_restricted_sqlite3_cmd { db cmd args } {
    if { $cmd eq "authorizer" } {
	error "error from restricted sqlite3. authorizer not allowed"
    }
    return [$db $cmd {*}$args]
}

proc compass_server::_restricted_sqlite3_auth { cmd args } {

    switch $cmd {
	SQLITE_ATTACH { return SQLITE_DENY }
	default { return SQLITE_OK }
    }
}

proc compass_server::give_ports_list {} {
    variable port
    variable portDict

    set portList ""
    if { $portDict eq "" } {
	lappend portList $port
    } else {
	set portList [dict keys $portDict]
    }
    return $portList
}

proc compass_server::give_log_data { port } {
    if { $port ne "" } {
	return [server$port eval [list mylog::give_log_data]]
    } else {
	return [mylog::give_log_data]
    }
}

proc compass_server::clear_log { port } {
    if { $port ne "" } {
	server$port eval [list mylog::clear_log]
    } else {
	mylog::clear_log
    }
}

################################################################################
#    mail_diary_server
################################################################################

proc compass_server::give_mail_server_diary {} {
    return [db eval {select * from mail_server_diary order by id}]
}

proc compass_server::check_mail_server_diary {} {
    variable interpList
    variable mail_server_diary
    
    set mail_server_diary ""
    if { [llength $interpList] == 0 } {
	foreach "id mail_from mail_to db_name mytable url_pattern active" [give_mail_server_diary] {
	    if { !$active } { continue }
	    dict lappend mail_server_diary [list $mail_from $mail_to] [list "" $db_name $mytable $url_pattern]
	}
    } else {
	foreach ip $interpList {
	    foreach "id mail_from mail_to db_name mytable url_pattern active" [$ip eval compass_server::give_mail_server_diary] {
		if { !$active } { continue }
		dict lappend mail_server_diary [list $mail_from $mail_to] [list $ip $db_name $mytable $url_pattern]
	    }
	}
    }
    if { [dict size $mail_server_diary] } {
	mylog::log [_ "Starting mail server at port 25"]
	set err [catch { socket -server compass_server::check_mail_server_diary_do 25 } ret]
	if { $err } {
	    mylog::log [_ "Error starting mail server: %s" $ret]
	}
    } else {
	#mylog::log [_ "Mail server not started"]
    }
}

proc compass_server::check_mail_server_diary_do { channel clientaddr clientport } {
    fconfigure $channel -blocking 0 -buffering line
    puts $channel "220 Servidor ESMTP"
    set after [after 5000 [list compass_server::check_mail_server_close $channel ""]]
    dict set state after $after
    fileevent $channel readable [list compass_server::check_mail_server_diary_read $channel EHLO $state]
}

proc compass_server::check_mail_server_close { channel state } {
    
    set after [dict_getd $state after ""]
    if { $after ne "" } {
	after cancel $after
    }
    catch { close $channel }
}

proc compass_server::check_mail_server_check { state } {
    variable mail_server_diary
    
    if { [llength [dict_getd $state rcpt_to ""]] != 1 } {
	error "error: rcpt_to=[dict_getd $state rcpt_to ""]"
    }
    set mail_from [string trim [dict get $state from]]
    set ret [regexp {^\w+}  [lindex [dict get $state rcpt_to] 0] mail_to]
    if { !$ret } {
	error "mail server data not correct for ' $mail_from --- [lindex [dict get $state rcpt_to] 0]"
    }
    if { ![dict exists $mail_server_diary [list $mail_from $mail_to]] } {
	error "could not find mail server data for ' $mail_from $mail_to'"
    }
}

proc compass_server::_parse_email_date { dateM } {
    
    regsub {.*,\s*} $dateM {} dateM
    regsub {\([^\)]*\)\s*$} $dateM {} dateM
    regsub -- {-(-\d+)\s*$} $dateM {\1} dateM
    regsub -- {(\d{2})-(\d{2})\s*$} $dateM {\1\2} dateM
    set err [catch {clock scan $dateM -format "%d %b %Y %H:%M:%S %z"} time]
    if { $err } {
	set err [catch {clock scan $dateM -format "%d %b %Y %H:%M %z"} time]
    }
    if { $err } {
	set err [catch {clock scan $dateM } time]
    }
    #return [clock format $time -format "%Y-%m-%d %H:%M:%S"]
    return [clock format $time -format "%Y-%m-%d"]
}

proc compass_server::_parse_email_contents { data } {
    
    lassign [list "" 0 ""] d idx last_name
    foreach line [string trim [split $data \n]] {
	if { [regexp {Date:\s*(.*)} $line {} date] } {
	    dict set d date [_parse_email_date $date]
	    set last_name ""
	} elseif { [regexp {Content-Type:\s*multipart/(?:related|mixed)(?:.*)boundary=\s*(".*"|\S+)} $line {} boundary] } {
	    dict set d boundary [string trim $boundary {"}]
	    set last_name ""
	} elseif { [regexp {(.*):\s*(.*)} $line {} name value] } {
	    if { [regexp {^\s*=\?([-\w]+)\?Q\?(.*)\?=$} $value {} encoding data] } {
		set value [_quoted-printable_decode $encoding $data]
		regsub -all {_} $value { } value
	    }
	    dict set d $name $value
	    set last_name $name
	} elseif { [regexp {^\s*$} $line] } {
	    break
	} elseif { $last_name ne "" } {
	    set value [string trim $line]
	    if { [regexp {^\s*=\?([-\w]+)\?Q\?(.*)\?=$} $value {} encoding data] } {
		set value [_quoted-printable_decode $encoding $data]
		regsub -all {_} $value { } value
	    }
	    dict append d $last_name $value
	}
	incr idx
    }
    dict set d contents [join [lrange [split $data \n] $idx end] \n]
    return $d
}

proc compass_server::_quoted-printable_decode { mime_encoding data } {
    
    set encoding [cu::mail::reversemapencoding $mime_encoding]
    if { $encoding eq "" } {
	return $data
    }
    set res ""
    set last_idx 0
    foreach "idxs" [regexp -inline -indices -all {=\w\w|=\n} $data] {
	append res [string range $data $last_idx [lindex $idxs 0]-1]
	set t [string range $data {*}$idxs]
	if { $t ne "=\n" } {
	    append res [encoding convertfrom $encoding [binary format c 0x[string range $t 1 2]]]
	}
	set last_idx [expr {[lindex $idxs end]+1}]
    }
    append res [string range $data $last_idx end]
    return $res
}

proc compass_server::check_mail_server_process { state } {
    variable mail_server_diary
    
    set mail_from [string trim [dict get $state from]]
    regexp {^\w+}  [lindex [dict get $state rcpt_to] 0] mail_to
    
    set d [_parse_email_contents [dict get $state data]]
    if { [dict exists $d boundary] } {
	dict set d body ""
	set start 0
	set contents ""
	foreach line [split [dict get $d contents] \n] {
	    if { [string match "*[dict get $d boundary]*" $line] } {
		if { $start == 0 } {
		    set start 1
		} else {
		    set d_in [_parse_email_contents $contents]
		    if { [string match "text/plain*" [dict_getd $d_in Content-Type ""]] } {
		        set body [string trim [dict get $d_in contents]]
		        if {  [dict_getd $d_in Content-Transfer-Encoding ""] eq "quoted-printable" } {
		            if { ![regexp {charset=([\w-]+)} [dict_getd $d_in Content-Type ""] {} enc] } {
		                set enc ISO-8859-1
		            }
		            set body [_quoted-printable_decode $enc $body]
		        }
		        dict set d body $body
		        break
		    }
		    set start 0
		    set contents ""
		}
	    } elseif { $start } {
		append contents "$line\n"
	    }
	}
    } else {
	set body [string trim [dict get $d contents]]
	if {  [dict_getd $d Content-Transfer-Encoding ""] eq "quoted-printable" } {
	    if { ![regexp {charset=([\w-]+)} [dict_getd $d Content-Type ""] {} enc] } {
		set enc ISO-8859-1
	    }
	    set body [_quoted-printable_decode $enc $body]
	}
	dict set d body $body
    }
    if { [string trim [dict_getd $d Subject ""]] eq "" &&  [string trim [dict_getd $d body ""]] eq "" } {
	return
    }
    foreach v [dict get $mail_server_diary [list $mail_from $mail_to]] {
	lassign $v ip db_name mytable url_pattern

	set err [catch {
		if { $ip eq "" } {
		    check_mail_insert $db_name $mytable $url_pattern [dict get $d Message-ID] [dict get $d date] \
		        [dict get $d Subject] [dict get $d body]
		} else {
		    $ip eval [list compass_server::check_mail_insert $db_name $mytable $url_pattern [dict get $d Message-ID] \
		            [dict get $d date] [dict get $d Subject] [dict get $d body]]
		}
	    } ret]
	if { $err } {
	    mylog::log [_ "Error inserting mail from mail_server (%s)" $ret]
	}
    }
}

proc compass_server::check_mail_insert { db_name table url_pattern message_id date subject body } {
    variable appdir
    
    set file [file join $appdir databases $db_name]

    catch { package require lognoter_db }
    
    set lognoter_db [lognoter_db %AUTO% -dbtype sqlite -title compass_server \
	    -savereopenprefs 0 -permit_interactive 0]
    $lognoter_db open -mustexist 1 -raise_error 1 -filetable $file
    
    set fields [list Item Date Concept Status End_date Alarm_state Alarm_date Alarm_time Fields creation_user]
    
    set punct {[-,.+;]}
    set no_punct {[^-,.+;]}
    if { ![regexp "($no_punct+)$punct+(.*)" $subject {} Item Concept] } {
	set Item $subject
	set Concept "" 
    } else {
	set Item [string trim $Item]
	set Concept [string trim $Concept]
    }
    set db $lognoter_db
    set cmd "select Item from [$db sql fs0 $table] where Item=[$db sql vs0 $Item] limit 1"
    set ret [$lognoter_db sql sel $cmd]
    if { $ret eq "" } {
	set Concept "$subject"
	set Item [_ "Mail"]
    }
    regsub {\n[ ]*-{2,6}[ ]*\n(.*)$} $body {} body
    set idx 0
    foreach line [split $body \n] {
	if { [string length $Concept] +[string length $line] > 200 } {
	    break
	}
	append Concept "\n$line"
	incr idx
    }
    set Concept [string trim $Concept]
    set body [string trim [join [lrange [split $body \n] $idx end] \n]]

    set Date $date
    set Status "To complete"
    set End_date ""
    set Alarm_state 0
    set Alarm_date ""
    set Alarm_time ""
    
    set Notes "<wordwidget>"
    if { $body ne "" } {
	append Notes "<para>[xml_map1 $body]</para>"
    }
    set message_id [string trim $message_id <>]
    if { $url_pattern ne "" } {
	set url [format  $url_pattern $message_id]
	append Notes "<para><ulink linktype='url' url='[xml_map $url]'>[xml_map1 [_ "Search mail message"]]</ulink></para>"
    } else {
	append Notes "<para><emphasis role='strong'>[xml_map1 [_ "Message id"]]:</emphasis> [xml_map1$message_id]</para>"
    }
    append Notes "</wordwidget>"
    dict set Fields Notes $Notes
    
    set vs [list $Item $Date $Concept $Status $End_date $Alarm_state $Alarm_date $Alarm_time $Fields  [$db give_user]]
    set cmd "insert into [$db sql fs0 $table] (id,creation_date,modification_date,[$db sql fs $fields])
	values(NULL,now(),now(),[$db sql vs $vs])"
    $db sql exec $cmd
    $db destroy
    mylog::log [_ "Entered new item '%s' for diary in '%s'" $Item $db_name]
}

proc compass_server::check_mail_server_diary_read { channel what state } {
    variable interpList
    variable mail_server_diary
    
    if { [eof $channel] } {
	check_mail_server_close $channel $state
	return
    }
    set data [dict_getd $state data ""]
    set ret [gets $channel line]
    append data $line
    if { $ret != -1 } {
	append data "\n"
    }
    
    #mylog::debug line=$line

    switch $what {
	EHLO {
	    if { ![regexp {^(EHLO|HELO)} $data] } {
		mylog::log "EHLO $data"
		check_mail_server_close $channel $state
		return
	    }
	    fileevent $channel readable  [list compass_server::check_mail_server_diary_read $channel "MAIL FROM" $state]
	    puts $channel "250 Hello, please meet you"
	}
	"MAIL FROM" {
	    if { ![regexp {MAIL FROM:\s*([^<]+|<.*>)} $data {} from] } {
		mylog::log "MAIL FROM $data"
		check_mail_server_close $channel $state
		return
	    }
	    set from [string trim $from <>]
	    dict set state from $from
	    fileevent $channel readable  [list compass_server::check_mail_server_diary_read $channel "DATA" $state]
	    puts $channel "250 Ok"
	}
	"DATA" {
	    if { [regexp {RCPT TO:\s*([^<]+|<.*>)} $data {} rcpt] } {
		set rcpt [string trim $rcpt <>]
		set rcpt_to [dict_getd $state rcpt_to ""]
		lappend rcpt_to $rcpt
		dict set state rcpt_to $rcpt_to
		fileevent $channel readable  [list compass_server::check_mail_server_diary_read $channel "DATA" $state]
		puts $channel "250 Ok"
	    } elseif { [regexp {^DATA\s*$} $data] } {
		set err [catch { check_mail_server_check $state } ret]
		if { $err } {
		    mylog::log "error check DATA: $ret"
		    check_mail_server_close $channel $state
		    return
		}
		fileevent $channel readable  [list compass_server::check_mail_server_diary_read $channel "DATA DO" $state]
		puts $channel "354 End data with <CR><LF>.<CR><LF>"
	    } else {
		mylog::log "error DATA: $data"
		check_mail_server_close $channel $state
		return
	    }
	}
	"DATA DO" {
	    if { [regexp {\n\.\n$} $data] || [string length $data] > 10000 } {
		dict set state data $data
		check_mail_server_process $state
		puts $channel "250 Ok"
		fileevent $channel readable  [list compass_server::check_mail_server_diary_read $channel "QUIT" $state]
	    } else {
		dict set state data $data
		fileevent $channel readable [list compass_server::check_mail_server_diary_read $channel "DATA DO" $state]
	    }
	}
	"QUIT" {
	    puts $channel "221 Bye"
	    check_mail_server_close $channel $state
	}
    }
}

################################################################################
#    Install utilities
################################################################################

proc compass_server::start_server {} {
    variable server_initial_params_dict

    set port [dict_getd $server_initial_params_dict -port ""]
    set portDict [dict_getd  $server_initial_params_dict -portDict ""]
    if { $portDict ne "" } {
	set portList [dict keys $portDict]
    } else {
	set portList [list $port]
    }
    if { [llength $portList] == 0 } {
	error [_ "error: it is necessary to define one or more ports"]
    }
    set dn [dict_getd $server_initial_params_dict -displayname "Compass server"]
    set displayname "$dn (port=[join $portList ,])"

    if { $::tcl_platform(platform) eq "windows" } {
	foreach p $portList {
	    catch { exec netsh firewall add portopening TCP $p $displayname }
	}
    }
    set l ""
    dict for "n v" $server_initial_params_dict {
	if { $n in "-svcname -displayname -user" } { continue }
	lappend l $n $v
    }
    set err [catch { compass_server::init_server {*}$l } errstring]
    if { $err } {
	puts stderr $errstring
	catch {
	    wm withdraw .
	    tk_messageBox -message $errstring
	}
	exit 2
    }
}

proc compass_server::service_handler { control {name ""} {sequence 0} args } {
    variable server_initial_params_dict

    set svcname [dict_getd $server_initial_params_dict -svcname "compass_serverd"]
    
    switch -- $control {
	start - continue {
	    set err [catch { start_server } errstring]
	    if { $err } {
		twapi::eventlog_log "Service $name failed to start: $errstring"
		set state stopped
	    } else {
		set state running
	    }
	    set err [catch { twapi::update_service_status $svcname $sequence $state } errstring]
	    if { $err } { twapi::eventlog_log "Service $name failed to update status: $errstring" }
	}
	stop - all_stopped - pause {
	    set err [catch { stop_server } errstring]
	    if { $err } {
		twapi::eventlog_log "Service $name failed to stop: $errstring"
		set state stopped
	    } else {
		set state stopped
	    }
	    set err [catch { twapi::update_service_status $svcname $sequence $state } errstring]
	    if { $err } { twapi::eventlog_log "Service $name failed to update status: $errstring" }
	}
    }
}

proc compass_server::initial_params { args } {
    variable server_initial_params_dict
    
    set server_initial_params_dict ""
    initial_params_add {*}$args
}

proc compass_server::initial_params_add { args } {
    variable server_initial_params_dict
    
    dict for "n v" $args {
	dict set server_initial_params_dict $n $v
    }
}

proc compass_server::install_service { args } {
    
    set optional {
	{ -prefix prefix "" }
    }
    set compulsory ""
    parse_args $optional $compulsory $args
    
    if { $::tcl_platform(platform) eq "windows" } {
	compass_server::install_service_windows $prefix
    } else {
	compass_server::install_service_unix $prefix
    }
}

proc compass_server::install_service_windows { prefix } {
    variable server_initial_params_dict
    
    if { ![info exists ::starkit::topdir] } {
	set script_name $::argv0
    } else {
	set script_name ""
    }
    set port [dict get $server_initial_params_dict -port]
    set svcname [dict_getd $server_initial_params_dict -svcname "compass_serverd"]
    set dn [dict_getd $server_initial_params_dict -displayname "Compass server"]
    set displayname "$dn (port=$port)"
    
    package require twapi

    catch {
	twapi::stop_service $svcname -wait 2000
	twapi::delete_service $svcname
    }

    set command "\"[info nameofexecutable]\""
    if { ![info exists ::starkit::topdir] } { append command " \"$::argv0\"" }
    if { $prefix ne "" } {
	append command " $prefix"
    }
    
    package require base64
    set l [list -start_service]
    dict for "n v" $server_initial_params_dict {
	if { $n in "-svcname -displayname -user -version" } { continue }
	lappend l $n $v
    }
    set bargs [base64::encode -wrapchar "" [encoding convertto utf-8 $l]]
    append command " -base64args $bargs"

    set err [catch { twapi::create_service $svcname $command \
		-displayname $displayname -interactive 0 \
		-starttype auto_start
	    set ret [twapi::start_service $svcname -wait 8000]
	    if { $ret == 0 } { error "Service did not start" }
	} errstring]
    
    if { !$err } {
	set txt [_ "Service %s started" $svcname]
    } else {
	set txt [_ "Error starting service %s (%s)" $svcname $errstring]
    }
    output_message $txt $err
}

proc compass_server::output_message { txt force } {
    
    if { $force } {
	catch { package require Tk }
    }
    set err [catch {
	    package present Tk
	    wm withdraw .
	    tk_messageBox -message $txt
	}]
    if { $err } {
	if { !$force } {
	    puts $txt
	} else {
	    puts stderr $txt
	}
    }
}

set daemon_script {
#! /bin/sh

DAEMON=%EXE%
DAEMON_OPTS=%OPTS%
# DAEMON_OPTS= %OPTSNE%
SVCNAME=%SVCNAME%
DISPLAYNAME=%NAME%
APPDIR=%APPDIR%

test -x $DAEMON || exit 0

. /lib/lsb/init-functions
. /etc/default/rcS


export PATH="${PATH:+$PATH:}/usr/sbin:/sbin"

start() {
    log_daemon_msg "Starting $DISPLAYNAME" "$SVCNAME"
    if [ -s /var/run/$SVCNAME.pid ] && kill -0 $(cat /var/run/$SVCNAME.pid) >/dev/null 2>&1; then
	log_progress_msg "apparently already running"
	log_end_msg 0
	exit 0
    fi
    if start-stop-daemon --start --quiet --background %CHUID_OPTS% \
	--pidfile /var/run/$SVCNAME.pid --make-pidfile \
	--exec $DAEMON -- $DAEMON_OPTS
    then
	rc=0
	sleep 1
	if ! kill -0 $(cat /var/run/$SVCNAME.pid) >/dev/null 2>&1; then
	    log_failure_msg "$DISPLAYNAME failed to start"
	    rc=1
	fi
    else
	rc=1
    fi
    if [ $rc -eq 0 ]; then
	log_end_msg 0
    else
	log_end_msg 1
	rm -f /var/run/$SVCNAME.pid
    fi
}

stop() {
    log_daemon_msg "Stopping $DISPLAYNAME" "$SVCNAME"
    start-stop-daemon --stop --quiet --oknodo --pidfile /var/run/$SVCNAME.pid
    log_end_msg $?
    rm -f /var/run/$SVCNAME.pid
}

status() {
    status_of_proc -p /var/run/$SVCNAME.pid "$DAEMON" "$DISPLAYNAME"  && exit 0 || exit $?
}
    
viewlog() {
	if [ "$DISPLAY" = "" ] ; then
	    export DISPLAY=:0
	fi
	$DAEMON -appdir $APPDIR -view_log
}

case "$1" in
  start)
	start
	;;
    stop)
	stop
	;;
    
    restart)
	stop
	start
	;;
    viewlog)
	viewlog
	;;
    status)
	status
	;;

  *)
	echo "Usage: /etc/init.d/$SVCNAME {start|stop|restart|status|viewlog}"
	exit 1
esac

exit 0
}

proc compass_server::install_service_unix { prefix } {
    global daemon_script
    variable server_initial_params_dict
	
    if { ![info exists ::starkit::topdir] } {
	set script_name $::argv0
    } else {
	set script_name ""
    }
    set port [dict get $server_initial_params_dict -port]
    set portDict [dict_getd $server_initial_params_dict -portDict ""]
    set portList [join [dict keys $portDict] ,]
    set svcname [dict_getd $server_initial_params_dict -svcname "compass_serverd"]
    set dn [dict_getd $server_initial_params_dict -displayname "Compass server"]
    set appdir [dict_getd $server_initial_params_dict -appdir ""]
    set user [dict_getd $server_initial_params_dict -user ""]

    if { $portDict eq "" } {
	set displayname "$dn (port=$port)"
    } else {
	set displayname "$dn(ports=$portList)"
    }
    catch {
	exec /etc/init.d/$svcname stop
	file delete /etc/init.d/$svcname
	foreach i [glob -nocomplain -dir /etc rc?.d/*$svcname] {
	    file delete $i
	}
    }
    
    if { $appdir eq "" } {
	error [_ "error: -appdir cannot be void"]
    }
    file mkdir $appdir
    if { $user ne "" } {
	catch { exec chown -R $user $appdir >& /dev/null }
    }
    set command [info nameofexecutable]
	
    set args ""
    if { ![info exists ::starkit::topdir] } {
	lappend args [file normalize $::argv0]
    }
    if { $prefix ne "" } {
	lappend args $prefix
    }
    package require base64
    set l [list -start_service]
    dict for "n v" $server_initial_params_dict {
	if { $n in "-svcname -displayname -user -version" } { continue }
	lappend l $n $v
    }
    set bargs [base64::encode -wrapchar "" [encoding convertto utf-8 $l]]
    set args_ne $args
    lappend args -base64args $bargs
    lappend args_ne {*}$l

    set map [list \
	    %EXE% "\"$command\"" \
	    %OPTS% "\"$args\"" \
	    %OPTSNE% "\"$args_ne\"" \
	    %SVCNAME% "\"$svcname\"" \
	    %NAME% "\"$displayname\"" \
	    %APPDIR% "\"$appdir\"" \
	    ]
    if { $user eq "" } {
	lappend map %CHUID_OPTS% ""
    } else {
	lappend map %CHUID_OPTS% "--chuid $user"
    }
    set data [string map $map [string trim $daemon_script]]
    set err [catch { open /etc/init.d/$svcname w } fout]
    if { $err } {
	output_message [_ "Error installing service. It is necessary to be root (%s)" $fout] 1
	return
    }
    puts $fout $data
    close $fout
    file attributes /etc/init.d/$svcname -permissions 00755
    
    for { set i 0 } { $i <= 6 } { incr i } {
	if { [file isdirectory /etc/rc$i.d] } {
	    if { $i in "0 1 6" } {
		set prefix "K09"
	    } else {
		set prefix "S91"
	    }
	    set link [file join /etc/rc$i.d ${prefix}$svcname]
	    file delete $link
	    file link $link ../init.d/$svcname
	}
    }
    set err [catch { exec /etc/init.d/$svcname start } ret]
    
    if { !$err } {
	if { $portDict eq "" } {
	    set txt [_ "Service %s starting at port %s. appdir=%s\n\n%s" $svcname $port $appdir $ret]
	} else {
	    set txt [_ "Service %s starting at ports %s\n\n%s" $svcname $portList $ret]
	}
    } else {
	set txt [_ "Error starting service %s (%s)" $svcname $ret]
    }
    output_message $txt $err
}


proc compass_server::remove_service {} {
    variable server_initial_params_dict

    set svcname [dict_getd $server_initial_params_dict -svcname "compass_serverd"]
    if { $svcname eq "" } {
	error "error in compass_server::remove_service. svcname cannot be void"
    }
    if { $::tcl_platform(platform) eq "windows" } {
	package require twapi
	set err [catch { twapi::get_service_configuration $svcname -displayname } ret]
	if { $err } { return }
	catch {
	    regexp {port=(\S+)} $ret {} portListS
	    foreach p [split $portListS ,] {
		exec netsh firewall delete portopening TCP $p
	    }
	}
	catch { twapi::stop_service $svcname -wait 8000 }
	
	set err [catch { twapi::delete_service $svcname } errstring]
    } else {
	catch { exec /etc/init.d/$svcname stop }
	set err [catch {
		file delete /etc/init.d/$svcname
		foreach i [glob -nocomplain -dir /etc rc?.d/*$svcname] {
		    file delete $i
		}
	    } errstring]
    }
    if { !$err } {
	set txt [_ "Service %s removed" $svcname]
    } else {
	set txt [_ "Error removing service %s (%s)" $svcname $errstring]
    }
    output_message $txt $err
}

proc compass_server::install_service_remote { args } {

    set optional {
	{ -from_dir from_dir "" }
    }
    set compulsory ""
    parse_args $optional $compulsory $args
    
    if { $::tcl_platform(platform) ne "windows" } {
	error [_ "error: options -install_remote is only available for the Windows version"]
    }
    if { ![file isdirectory $from_dir] } {
	error [_ "error: File '%s' is not a directory" $from_dir]
    }
    set exe [file join $from_dir CompassServer.exe]
    if { ![file exists $exe] } {
	error [_ "error: File '%s' does not exist" $exe]
    }
    lassign [install_service_remote_gui] w computers user password to_dir server_user server_password \
	copy_users

    set f [$w giveframe]
    set f1 [ttk::frame $f.f5 -borderwidth 2 -relief raised]
    ttk::label $f1.l -textvariable ::progress_text
    ttk::progressbar $f1.p -orient horizontal -mode determinate \
	-variable ::progress
    grid $f1.l -sticky w -padx 20 -pady "20 3"
    grid $f1.p -sticky new -padx 20 -pady "3 20"
    grid columnconfigure $f1 0 -weight 1

    place $f1 -in $f -x 0 -rely .2 -relwidth 1
    $w configure -cursor watch
    
    set computer_percent [expr {100.0/[llength $computers]}]
    set idx 0
    foreach computer $computers {
	set ::progress_text [_ "Installing on computer '%s'" $computer]
	set ::progress [expr {$idx*$computer_percent}]
	update

	catch { exec net use /delete \\\\$computer }
	set err [catch { exec net use /user:$user \\\\$computer $password } \
		errstring]
	if { $err } {
	    error [_ "Error connecting to computer '%s' with user '%s'" \
		    $computer $user]
	}
	set drive [lindex [file split $to_dir] 0]
	set rest [eval file join [lrange [file split $to_dir] 1 end]]
	if { ![regexp {(?i)^([a-z]):/$} $drive {} drive] } {
	    error "error: destination directory does not contain the drive letter"
	}
	set to_dirF [file nativename [file join $rest Compass [file tail \
		        $from_dir]]]
	set to_dirCF "\\\\$computer\\$drive\$\\$to_dirF"
	
	catch { exec winserv.exe stop \\\\$computer\\compass_serverd }
	catch { exec winserv.exe uninstall \\\\$computer\\compass_serverd }
	
	file delete -force $to_dirCF
	file mkdir [file dirname $to_dirCF]
	file copy -force $from_dir $to_dirCF
	file delete -force [file join $to_dirCF workdir]

	set ::progress_text [_ "Copied files to computer '%s'" $computer]
	set ::progress [expr {($idx+.25)*$computer_percent}]
	update
	
	exec winserv.exe install \\\\$computer\\compass_serverd -displayname \
	    "Compass server" -interactive -start auto \
	    -binary [file nativename [file join $drive:\\$to_dirF winserv.exe]] \
	    [file nativename [file join $drive:\\$to_dirF CompassServer.exe]]
	exec winserv.exe start \\\\$computer\\compass_serverd

	set ::progress_text [_ "Started service on computer '%s'" $computer]
	set ::progress [expr {($idx+.50)*$computer_percent}]
	update
	
	if { $copy_users } {
	    file mkdir [file join $to_dirCF workdir]
	    file copy [file join $from_dir workdir users.db3] \
		[file join $to_dirCF workdir]
	}

	for { set i 0 } { $i < 10 } { incr i } {
	    set ::progress_text [_ "Going to connect (%s) to computer '%s'" \
		    $i $computer]
	    set ::progress [expr {($idx+.75)*$computer_percent}]
	    update
	    after 300
	    
	    set err [catch {
		    if { $copy_users } {
		        compass_client::init_client -raise_error 1 $computer
		        compass_client::end_client
		    } else {
		        compass_client::init_client -raise_error 1 $computer
		        compass_client::send_user_password "" ""
		        compass_client::send add_user $server_user $server_password ""
		        compass_client::end_client
		    }
		}]
	    if { !$err } { break }
	}
	set ::progress_text [_ "Installed on computer '%s'" $computer]
	set ::progress [expr {($idx+.95)*$computer_percent}]
	update

	incr idx
    }
}

proc compass_server::install_service_remote_gui {} {

    package require dialogwin
    package require tooltip

    set hosts [list localhost]
    set users [list compass]

    wm withdraw .
    set w [dialogwin_snit .%AUTO% -title [_ "Install Compass server"] \
	    -cancelname [_ Exit]]
    set f [$w giveframe]

    if { [info exists ::env(ProgramFiles)] } {
	$w set_uservar_value to_dir $::env(ProgramFiles)
    } else {
	$w set_uservar_value to_dir "c:/Program files"
    }
    if { [info exists ::env(USERNAME)] } {
	set user $::env(USERNAME)
    } else { set user "" }

    set p [cu::get_program_preferences compass_server_ir]
    $w set_uservar_value host [dict_getd $p host ""]
    $w set_uservar_value user [dict_getd $p user $user]

    set f1 [ttk::labelframe $f.f1 -text [_ "Computer"]]
    ttk::label $f1.l1 -text [_ "Computer:"]
    ttk::combobox $f1.cb1 -textvariable [$w give_uservar host] \
	-values $hosts
    set txt [_ "A list of one or more computers. Examples:"]
    append txt "\nmycomputer1 mycomputer2 mycomputer3"
    tooltip::tooltip $f1.l1 $txt
    tooltip::tooltip $f1.cb1 $txt

    ttk::label $f1.l2 -text [_ "User:"]
    ttk::combobox $f1.cb2 -textvariable [$w give_uservar user] \
	-values $users -width 20
    ttk::label $f1.l3 -text [_ "Password:"]
    ttk::entry $f1.e1 -textvariable [$w give_uservar password ""] \
	-show *

    ttk::label $f1.l31 -text [_ "Destination base dir:"]
    ttk::entry $f1.e11 -textvariable [$w give_uservar to_dir]

    set txt [_ "This directory must exist"]
    tooltip::tooltip $f1.l31 $txt
    tooltip::tooltip $f1.e11 $txt

    grid $f1.l1 $f1.cb1 - -sticky w -pady 1
    grid $f1.l2 $f1.cb2 - -sticky w -pady 1
    grid $f1.l3 $f1.e1  - -sticky w -pady 1
    grid $f1.l31 $f1.e11  - -sticky w -pady 1
    grid configure $f1.cb1 $f1.cb2 $f1.e1 $f1.e11 -sticky ew
    grid columnconfigure $f1 1 -weight 1
    grid rowconfigure $f1 4 -weight 1

    set f2 [ttk::labelframe $f.f2 -text [_ "Server"]]

    ttk::label $f2.l2 -text [_ "User:"]
    ttk::combobox $f2.cb2 -textvariable [$w give_uservar server_user compass] \
	-values $users -width 40
    ttk::label $f2.l3 -text [_ "Password:"]
    ttk::entry $f2.e1 -textvariable [$w give_uservar server_password ""] \
	-show *

    ttk::checkbutton $f2.cb1 -text [_ "Copy local users"] -variable \
	[$w give_uservar copy_users 0]

    set d [dict create 1 "" 0 "$f2.l2 $f2.cb2 $f2.l3 $f2.e1"]
    $w enable_disable_on_key copy_users $d

    grid $f2.l2 $f2.cb2 - -sticky w -pady 1
    grid $f2.l3 $f2.e1  - -sticky w -pady 1
    grid $f2.cb1 -      - -sticky w -pady 1
    grid configure $f2.cb2 $f2.e1 -sticky ew
    grid columnconfigure $f2 1 -weight 1
    grid rowconfigure $f2 3 -weight 1

    grid $f1 -sticky nsew
    grid $f2 -sticky nsew
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 2 -weight 1

    tk::TabToWindow $f1.cb1
    bind $w <Return> [list $w invokeok]
    set action [$w createwindow]

    if { $action < 1 } { exit }

    
    set ret $w
    foreach i [list host user password to_dir server_user server_password \
	    copy_users] {
	lappend ret [$w give_uservar_value $i]
    }
    set p ""
    foreach i [list host user] {
	dict set p $i [$w give_uservar_value $i]
    }
    cu::store_program_preferences compass_server_ir $p 
    return $ret
}















