clear all
set more off

*Directorio de trabajo

use "$bases\neg_emp.dta", clear 

keep if fecha>=tm(`year_update'm`mes_update')

rename TRABAJADORES_pub trab_1
rename TRABAJADORES_priv trab_2
rename REAL_pub real_1
rename REAL_priv real_2

keep fecha trab_* real_*

reshape long real_ trab_ , i(fecha) j(emp)

drop fecha 


graph bar trab_ , over(emp, relabel(1 "Pública" 2 "Privada") label(labsize(5)) axis(lcolor(white))) ///
bar(1, color("111 114 113")) ytitle("") ///
blabel(bar, orientation(horizontal) size(5) format(%7.0fc)) ylab(-45000(20000)180000, nogrid) yline(0, lcolor("111 114 113"))  ///
ysc(off) ysize(2.8) xsize(2.8) legend(off) graphregion(color(white))
graph export "$graphs\negociaciones_empresas\trabajadores/neg_emp_trab_`year_update'm`mes_update'.png", replace


graph bar real_ , over(emp, relabel(1 "Pública" 2 "Privada") label(labsize(5)) axis(lcolor(white))) ///
bar(1, color("188 149 92")) ytitle("") ///
blabel(bar, orientation(horizontal) size(5) format(%2.1f)) ylab(-1(1)5, nogrid) yline(0, lcolor("111 114 113")) ///
ysc(off) ysize(2.8) xsize(2.8) legend(off) graphregion(color(white))
graph export "$graphs\negociaciones_empresas\real/neg_emp_real_`year_update'm`mes_update'.png", replace


