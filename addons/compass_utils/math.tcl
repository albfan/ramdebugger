
package require math::linearalgebra
package require math::constants

package provide compass_utils::math 1.5

namespace eval ::math::linearalgebra { namespace export transpose }
namespace eval m {
    namespace import -force ::math::linearalgebra::*
    ::math::constants::constants pi degtorad radtodeg onethird

    proc vec_angle { vect1 vect2 } {
	set dp [dotproduct $vect1 $vect2]
	set n1 [norm $vect1]
	set n2 [norm $vect2]
	
	if { $n1 == 0.0 || $n2 == 0.0 } {
	    return -code error "Angle not defined for null vector"
	}
	return [expr {acos($dp/$n1/$n2)}]
    }
    proc ellipse_perimeter { rx ry } {
	# Hudson formula (approximate)
	variable pi
	set L [expr {pow($rx-$ry,2)/pow(2.0*($rx+$ry),2)}]
	return [expr {$pi*($rx+$ry)/4.0*(3*(1+$L)+1.0/(1-$L))}]
    }
    proc ellipse_val { c rx ry theta } {
	set x [expr {$rx*cos($theta)}]
	set y [expr {$ry*sin($theta)}]
	return [m::add [list $x $y] $c]
    }
    proc ellipse_normal { rx ry theta } {
	set tx [expr {-1*$rx*sin($theta)}]
	set ty [expr {$ry*cos($theta)}]
	return [m::unitLengthVector [list $ty [expr {-1*$tx}]]]
    }
    proc ellipse_val_inv { c rx ry p } {
	variable pi

	set p [m::sub $p $c]
	set theta $pi
	for { set i 0 } { $i < 100 } { incr i } {
	    set sin [expr {sin($theta)}]
	    set cos [expr {cos($theta)}]
	    set d2 [expr {pow($rx*$cos-[lindex $p 0],2)+pow($ry*$sin-[lindex $p 1],2)}]
	    if { abs($d2) < 1e-7 } { break }
	    set der_d2 [expr {-2*($rx*$cos-[lindex $p 0])*$rx*$sin+
		    2*($ry*$sin-[lindex $p 1])*$ry*$cos}]
	    if { $der_d2 == 0 } { break }
	    set theta [expr {$theta-$d2/$der_d2}]
	}
	while { $theta < 0 } { set theta [expr {$theta+2*$pi}] }
	while { $theta > 2*$pi } { set theta [expr {$theta-2*$pi}] }
	return $theta
    }
    proc ellipseF_val { c rx ry theta phi } {
	
	set sin_phi [expr {sin($phi)}]
	set cos_phi [expr {cos($phi)}]
	
	set x [expr {$rx*cos($theta)}]
	set y [expr {$ry*sin($theta)}]

	set R [m::mkMatrix 2 2 0.0]
	m::setcol R 0 [list $cos_phi [expr {$sin_phi}]]
	m::setcol R 1 [list [expr {-1*$sin_phi}] $cos_phi]
	return [m::add [m::matmul $R [list $x $y]] $c]
    }
    proc ellipse_add_length_to_angle { rx ry theta0 len } {
	# approximating as a circle (very bad)
	set r [expr {.5*($rx+$ry)}]
	set delta_theta [expr {$len/$r}]
	return [expr {$theta0+$delta_theta}]
    }
    proc ellipse_arc_length { rx ry theta1 theta2 { delta .01 } } {
	# from theta1 to theta2 in anti-clockwise orientation
	variable pi

	set len 0.0
	set last_p ""
	while { $theta2 < $theta1 } { set theta2 [expr {$theta2+2*$pi}] }
	for { set t $theta1 } { $t < $theta2 } { set t [expr {$t+$delta}] } {
	    set p [ellipse_val "0 0" $rx $ry $t]
	    if { $last_p ne "" } {
		set len [expr {$len+[m::norm [m::sub $p $last_p]]}]
	    }
	    set last_p $p
	}
	return $len
    }
    proc MSobreN { m n } {
	set num 1.0
	set nfac 1.0
	for { set i $m } { $i >= $m-$n+1 } { incr i -1 } { set num [expr {$num*$i}] }
	for { set i $n } { $i >= 1 } { incr i -1 } { set nfac [expr {$nfac*$i}] }
	return [expr {$num/$nfac}]
    }
    proc eval_cubic_bezier { x0 y0 x1 y1 x2 y2 x3 y3 t } {
	set nsobrei 1
	set s [expr {1.0-$t}]
	set fact 1.0
	set x [expr {$x0*$s}]
	set y [expr {$y0*$s}]
	for { set i 1 } { $i < 3 } { incr i } {
	    set fact [expr {$fact*$t}]
	    set nsobrei [expr {$nsobrei*(4-$i)/$i}] ;# be careful. They are integers
	    set x [expr {$s*$x+$fact*$nsobrei*$s*[set x$i]}]
	    set y [expr {$s*$y+$fact*$nsobrei*$s*[set y$i]}]
	}
	set x [expr {$x+$fact*$t*$x3}]
	set y [expr {$y+$fact*$t*$y3}]
	return [list $x $y]
    }
    proc eval_deriv_cubic_bezier { x0 y0 x1 y1 x2 y2 x3 y3 t } {
	foreach "x y" [list 0.0 0.0] break
	for { set i 0 } { $i < 3 } { incr i } {
	    set BNM1 [expr {[MSobreN 2 $i]*$t**$i*(1.0-$t)**(2-$i)}]
	    set diff_x [expr {[set x[expr {$i+1}]]-[set x$i]}]
	    set diff_y [expr {[set y[expr {$i+1}]]-[set y$i]}]
	    set x [expr {$x+$BNM1*$diff_x}]
	    set y [expr {$y+$BNM1*$diff_y}]
	}
	return [list [expr {3*$x}] [expr {3*$y}]]
    }
    proc eval_cubic_bezier_length { x0 y0 x1 y1 x2 y2 x3 y3 } {
	set GausLegendrePos(0) 0.019855071751232;  set GausLegendreWeight(0) 0.050614268145188;
	set GausLegendrePos(1) 0.101666761293187;  set GausLegendreWeight(1) 0.111190517226687;
	set GausLegendrePos(2) 0.237233795041836;  set GausLegendreWeight(2) 0.156853322938943;
	set GausLegendrePos(3) 0.408282678752175;  set GausLegendreWeight(3) 0.181341891689181;
	set GausLegendrePos(4) 0.591717321247825;  set GausLegendreWeight(4) 0.181341891689181;
	set GausLegendrePos(5) 0.762766204958164;  set GausLegendreWeight(5) 0.156853322938943;
	set GausLegendrePos(6) 0.898333238706813;  set GausLegendreWeight(6) 0.111190517226687;
	set GausLegendrePos(7) 0.980144928248768;  set GausLegendreWeight(7) 0.050614268145188;
     
	set len 0.0
	for { set i 0 } { $i < 8 } { incr i } {
	    foreach "x y" [eval_deriv_cubic_bezier $x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3 \
		    $GausLegendrePos($i)] break
	    set len [expr {$len+$GausLegendreWeight($i)*sqrt($x**2+$y**2)}]
	}
	return $len
    }
    proc eval_cubic_bezier_full { knots pnts t } {

	for { set i 0 } { $i < [llength $knots] } { incr i } {
	    if { $t <= [lindex $knots $i] } { break }
	}
	if { $i == 0 } { set t0 0.0 } else { set t0 [lindex $knots [expr {$i-1}]] }
	set tL [expr {($t-$t0)/double([lindex $knots $i]-$t0)}]
	set coords [lrange $pnts [expr {$i*6}] [expr {$i*6+7}]]
	return [eval_cubic_bezier {*}$coords $tL]
    }
    proc eval_deriv_cubic_bezier_full { knots pnts t } {

	for { set i 0 } { $i < [llength $knots] } { incr i } {
	    if { $t <= [lindex $knots $i] } { break }
	}
	if { $i == 0 } { set t0 0.0 } else { set t0 [lindex $knots [expr {$i-1}]] }
	set tL [expr {($t-$t0)/double([lindex $knots $i]-$t0)}]
	set coords [lrange $pnts [expr {$i*6}] [expr {$i*6+7}]]
	return [eval_deriv_cubic_bezier {*}$coords $tL]
    }
    proc vectorprod3 { vec1 vec2 } {
	return [list [expr {[lindex $vec1 1]*[lindex $vec2 2]-[lindex $vec2 1]*[lindex $vec1 2]}] \
		[expr {[lindex $vec1 2]*[lindex $vec2 0]-[lindex $vec2 2]*[lindex $vec1 0]}] \
		[expr {[lindex $vec1 0]*[lindex $vec2 1]-[lindex $vec2 0]*[lindex $vec1 1]}]]
    }
    proc nice3 { v } {
	return [nice $v 3]
    }
    proc nice { v precision } {
	set ret ""
	foreach i $v {
	    if { $precision == 3 && abs($i) >= 1000 && abs($i) < 10000 } {
		lappend ret [format %.4g $i]
	    } else {
		lappend ret [format %.${precision}g $i]
	    }
	}
	return $ret
    }
    proc linear_interpolate { t tList } {
	
	if { $t <= [lindex $tList 0] } {
	    return [list [lindex $tList 0] [lindex $tList 0] 1.0 0.0 0 0]
	}
	if { $t >= [lindex $tList end] } {
	    set t [lindex $tList end]
	    set idx [expr {[llength $tList]-1}]
	    return [list $t $t 1.0 0.0 $idx $idx]
	}
	lassign [list 0 [expr {[llength $tList]-1}]] i1 i2
	while 1 {
	    set i [expr {round(0.5*($i1+$i2))}]
	    if { $i == $i1 || $i == $i2 } {
		set t1 [lindex $tList $i1]
		set t2 [lindex $tList $i2]
		set alpha [expr {($t2-$t)/double($t2-$t1)}]
		if { $alpha == 0.0 } {
		    return [list $t2 $t2 1.0 0.0 $i2 $i2]
		}
		return [list $t1 $t2 $alpha [expr {1.0-$alpha}] $i1 $i2]
	    }
	    if { $t <= [lindex $tList $i] } {
		set i2 $i
	    } else {
		set i1 $i
	    }
	}
    }
    proc linear_interpolate_value { t tList tValue } {
	lassign [linear_interpolate $t $tList] t1 t2 alpha1 alpha2 idx1 idx2
	return [expr {$alpha1*[lindex $tValue $idx1]+$alpha2*[lindex $tValue $idx2]}]
    }
    proc vector_normal_triangle { p1 p2 p3 } {
	
	set d1 [m::sub $p2 $p1]
	set d2 [m::sub $p3 $p2]
	return [unitLengthVector [vectorprod3 $d1 $d2]]
    }
}








