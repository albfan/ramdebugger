# Tcl package index file, version 1.1
# This file is generated by the "pkg_mkIndex" command
# and sourced either when an application starts up or
# by a "package unknown" script.  It invokes the
# "package ifneeded" command to set up package-related
# information so that packages will be loaded automatically
# in response to "package require" commands.  When this
# script is sourced, the variable $dir must contain the
# full path name of this file's directory.


proc LoadPdffWriter { dir } {
    if { $::tcl_platform(pointerSize) == 8} {
	set bits 64
    } else {
	set bits 32
    }
    set pwd [pwd]
    cd $dir
    load [file join $dir pdfwriter_${bits}[info sharedlibextension]] pdfwriter
    source pdfwriter.tcl
    cd $pwd
}

package ifneeded pdfwriter 2.0 [list LoadPdffWriter $dir]