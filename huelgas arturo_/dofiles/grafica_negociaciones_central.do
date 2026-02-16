clear all
set more off

use "$bases\neg_co.dta", clear


rename TRABAJADORES_ctm trab_1
rename TRABAJADORES_croc trab_2
rename TRABAJADORES_crom trab_3
rename TRABAJADORES_sna_asa trab_4
rename TRABAJADORES_otras trab_5
rename TRABAJADORES_ind_ct trab_6
rename TRABAJADORES_no_esp trab_7

rename REAL_ctm real_1
rename REAL_croc real_2
rename REAL_crom real_3
rename REAL_sna_asa real_4
rename REAL_otras real_5
rename REAL_ind_ct real_6
rename REAL_no_esp real_7

keep fecha trab_* real_* 

keep if fecha==tm(`year_update'm`mes_update')

reshape long trab_ real_ , i(fecha) j(cen_obr)


label define cen_obr 1 "CTM" , add
label define cen_obr 2 "CROC" , add
label define cen_obr 3 "CROM" , add
label define cen_obr 4 "SNA y ASA" , add
label define cen_obr 5 "Otras Org. del C.T." , add
label define cen_obr 6 "Indep. del C.T." , add
label define cen_obr 7 "No Esp." , add

label values cen_obr cen_obr

drop if real_==0
drop if cen_obr==7


*ssc install splitvallabels
splitvallabels cen_obr, length(10) recode


graph bar trab_ , over(cen_obr, relabel(`r(relabel)')  label(labsize(2.8)) axis(lcolor(white))) ///
bar(1, color("111 114 113")) ytitle("") ///
blabel(bar, orientation(horizontal) size(3.5) format(%7.0fc)) ylab(, nogrid) yline(0, lcolor("111 114 113"))  ///
ysc(off) ysize(2.8) xsize(2.8) legend(off) graphregion(color(white))

graph export "$graphs\negociaciones_central obrera\trabajadores/neg_central_a_`year_update'm`mes_update'.png", replace


splitvallabels cen_obr, length(10) recode
graph bar real_ , over(cen_obr, relabel(`r(relabel)')  label(labsize(2.8)) axis(lcolor(white))) ///
bar(1, color("188 149 92")) ytitle("") ///
blabel(bar, orientation(horizontal) size(3.5) format(%2.1f)) ylab(, nogrid) yline(0, lcolor("111 114 113")) ///
ysc(off) ysize(2.8) xsize(2.8) legend(off) graphregion(color(white))

graph export "$graphs\negociaciones_central obrera\real/neg_central_b_`year_update'm`mes_update'.png", replace


