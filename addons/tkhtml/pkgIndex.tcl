##
## A simple pkgIndex file for the TkHtml widget...
##
if { $::tcl_platform(os) == "Darwin"} {
    package ifneeded Tkhtml 3.0 \
	[list load [file join $dir libTkhtml3.0[info sharedlibextension]] TkHtml]
} else {
    package ifneeded Tkhtml 2.0 \
	[list load [file join $dir libTkhtml2.0[info sharedlibextension]] TkHtml]
}
