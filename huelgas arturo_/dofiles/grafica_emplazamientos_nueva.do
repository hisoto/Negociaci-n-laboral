***************************************
*Procedimientos a huelga de la TLCA 

clear 
use "$bases\emplaza.dta", clear
drop fecha  
g fecha = ym(year, mes)
format fecha %tm 

label define mes 1 "E", add
label define mes 2 "F", add
label define mes 3 "M", add
label define mes 4 "A", add
label define mes 5 "M", add
label define mes 6 "J", add
label define mes 7 "J", add
label define mes 8 "A", add
label define mes 9 "S", add
label define mes 10 "O", add
label define mes 11 "N", add
label define mes 12 "D", add

label value mes mes 


levelsof fecha, local(times)
foreach time of local times {
    label define nom_fecha `time' `"`= strofreal(`time',"%tmCCYY-m")'"', add
}
label values fecha nom_fecha

keep if fecha>=tm(2022m1)



*Emplazamientos a huelgas

graph bar emp_tlfac , over(mes, gap(40)) over(year, gap(40))  nofill  ///
bar(1, color("188 149 92"))  legend(off) ///
ytitle("")  blabel(bar, orientation(vertical) format(%7.0fc) size(3)) ylabel(, angle(horizontal) labsize (#9) format(%7.0fc) )  ///
graphregion(margin(t+7) color(white)) bgcolor(white) ysize(2.8) xsize(5.9) 

graph export "$graphs\emplazamientos/proc_huelgas_`year_update'm`mes_update'.png", replace

