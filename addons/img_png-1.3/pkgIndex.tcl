
# @@ Meta Begin
# Package img::png 1.3
# Meta activestatetags ActiveTcl Public Img
# Meta as::origin      http://sourceforge.net/projects/tkimg
# Meta category        Tk Image Format
# Meta description     This package provides support for the png image
# Meta description     format.
# Meta platform        win32-ix86
# Meta require         {img::base 1.3-2}
# Meta require         {Tcl 8.4-9}
# Meta require         {Tk 8.4-9}
# Meta require         pngtcl
# Meta subject         png
# Meta summary         png Image Support
# @@ Meta End


if {![package vsatisfies [package provide Tcl] 8.4]} return

package ifneeded img::png 1.3 [string map [list @ $dir] {
        # ACTIVESTATE TEAPOT-PKG BEGIN REQUIREMENTS

        package require img::base 1.3-2
        package require Tcl 8.4-9
        package require Tk 8.4-9
        package require pngtcl

        # ACTIVESTATE TEAPOT-PKG END REQUIREMENTS

            load [file join {@} tkimgpng13.dll]

        # ACTIVESTATE TEAPOT-PKG BEGIN DECLARE

        package provide img::png 1.3

        # ACTIVESTATE TEAPOT-PKG END DECLARE
    }]
