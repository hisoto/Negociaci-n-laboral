clear all
set more off

*Directorio de trabajo

use "$bases\huelgas.dta", clear


gen year=fecha


***************************************
*Grafica de huelgas estalladas y vigentes
***************************************
preserve
keep if fecha>=2005

replace huelgas_vigentes=. if huelgas_vigentes==0
replace huelgas=. if huelgas==0 

graph bar huelgas huelgas_vigentes, over(year, label( angle(vertical) labsize(4))) bar(1, color("188 149 92")) ///
bar(2, color("111 114 113")) blabel(bar, orientation(horizontal) size(4) format(%4.0f)) ///
ysc(off) ysize(2.8) xsize(5.9) graphregion(color(white)) legend(order(1 "Huelgas estalladas" 2 "Huelgas vigentes")  ///
size(4) pos(6) rows(1) region(lstyle(none))) plotregion(color(white)) 
	
graph export "$graphs\huelgas/huelgas_vigentes_`year_update'm`mes_update'.png", replace
graph export "$presentaciones/huelgas_vigentes_`year_update'm`mes_update'.png", replace

restore


***************************************
*Grafica de huelgas vigentes por causa 
***************************************
preserve 
keep if huelgas_vigentes>=1

graph bar rev_con_v rev_sal_v firma_con_v viol_con_v viol_con_ley_v, over(year, label( labsize(4))) stack ///
ylabel(, noticks nogrid angle(horizontal) labsize(3)) ///
legend( label(1 "Revisión de contrato") label(2 "Revisión de salario") label(3 "Firma de contrato") ///
label(4 "Violación de contrato") label(5 "Violación de contrato ley") pos(6) rows(2) region(lstyle(none))) ///
plotregion(color(white)) graphregion(color(white)) bgcolor(white)	///
bar(1, fcolor("188 149 92 ") lwidth(none)) bar(2, fcolor("152 152 154") lwidth(none)) bar(3, fcolor("111 114 113") lwidth(none)) bar(4, fcolor("35 91 78") lwidth(none)) bar(5, fcolor("159 34 65") lwidth(none))
	
graph export "$graphs\huelgas/huelgas_causa_`year_update'm`mes_update'.png", replace
graph export "$presentaciones/huelgas_causa_`year_update'm`mes_update'.png", replace
restore
	

