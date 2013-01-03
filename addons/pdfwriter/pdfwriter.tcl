
package provide pdfwriter 2.0

namespace eval PDFWriter {}

source [file join [file dirname [info script]] pdfwriter_print.tcl]
source [file join [file dirname [info script]] pdfwriter_pdf.tcl]
source [file join [file dirname [info script]] pdfwriter_svg.tcl]

namespace eval PDFWriter {
    namespace import -force PDF::*

    proc choosetype { print_or_pdf } {
	switch [string tolower $print_or_pdf] {
	    print {
		namespace import -force print::*
	    }
	    pdf {
		namespace import -force PDF::*
	    }
	    svg {
		namespace import -force svg::*
	    }
	    default {
		error "error in PDFWriter::choosetype"
	    }
	}
    }
}