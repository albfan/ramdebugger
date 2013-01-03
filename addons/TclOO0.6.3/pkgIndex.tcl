if {[catch {package require Tcl 8.5b1}]} return
package ifneeded TclOO 0.6.3 \
    [list load [file join $dir TclOO063.dll] TclOO]
