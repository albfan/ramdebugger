set papersize A4L
set boxList {
	{mainbox odd {48.4% 9% 90.1% 90%} 1}
	{text odd {68.7% 93.6% 70.2% 96.1%} Helvetica 10 black white nw {$pagenum} 1}
	{text odd {78.9% 4.9% 90.2% 7.0%} Helvetica 10 black white se {$sectiontitle} 1}
	{colorbox odd {48.4% 7.3% 90.2% 7.50%} black}
	{mainbox even {9.7% 9% 51.4% 90%} 1}
	{text even {30% 93.6% 31.5% 96%} Helvetica 10 black white nw {$pagenum} 1}
	{text even {9.7% 4.9% 44.1% 7.0%} Helvetica 10 black white ws {$documenttitle} 1}
	{colorbox even {9.7% 7.3% 51.3% 7.50%} black}
	{mainbox firstsectionpage {48.4% 15.6% 90.1% 90%} 1}
	{text firstsectionpage {48.4% 9% 90.1% 11.6%} Helvetica 14 black #dceae6 nw left {$sectiontitle} 1}
	{text firstsectionpage {68.7% 93.6% 70.2% 96.1%} Helvetica 10 black white nw left {<wordwidget><para>$pagenum</para></wordwidget>} 1}
}
array set state {
    sizedefault 10
    TitleBackgroundColor #dceae6
    printtoc 1
}
