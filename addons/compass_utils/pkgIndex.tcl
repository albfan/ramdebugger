# Tcl package index file, version 1.1
# This file is generated by the "pkg_mkIndex" command
# and sourced either when an application starts up or
# by a "package unknown" script.  It invokes the
# "package ifneeded" command to set up package-related
# information so that packages will be loaded automatically
# in response to "package require" commands.  When this
# script is sourced, the variable $dir must contain the
# full path name of this file's directory.

set cmd ""
lappend cmd [list source [file join $dir compass_utils.tcl]]
lappend cmd [list source [file join $dir formulae.tcl]]
lappend cmd [list source [file join $dir html2tdom.tcl]]
lappend cmd [list lappend ::auto_path $dir]

# NOTE: in the near future, auto_path will not be used anymore here
package ifneeded compass_utils 1.12 [join $cmd ";"]

proc load_compass_utils_library { dir basename } {
    if { $::tcl_platform(pointerSize) == 8 } {
	set bits 64
    } else {
	set bits 32
    }
    set library ${basename}_${bits}[info sharedlibextension]
    load [file normalize [file join $dir $library]] $basename
}

package ifneeded compass_utils::c 1.12 [list load_compass_utils_library $dir compass_utils]
package ifneeded compass_utils::math 1.5 [list source [file join $dir math.tcl]]

set cmd ""
lappend cmd [list source [file join $dir tk_utils.tcl]]
lappend cmd [list source [file join $dir draw_graphs.tcl]]
lappend cmd [list source [file join $dir megawidgets.tcl]]
lappend cmd [list load_compass_utils_library $dir compass_utils_tk]

package ifneeded compass_utils::img 1.12 [join $cmd ";"]

set cmd ""
lappend cmd [list source [file join $dir compass_client.tcl]]
lappend cmd [list source [file join $dir compass_server.tcl]]
package ifneeded compass_utils::client_server 1.12 [join $cmd ";"]


package ifneeded compass_utils::fcgi 1.0 {
    package require compass_utils::c
    cu::fcgi_init
}