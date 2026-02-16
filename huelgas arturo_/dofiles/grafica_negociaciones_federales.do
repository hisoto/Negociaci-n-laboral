clear all
set more off

*Directorio de trabajo

use "$bases\neg_fed.dta", clear 

destring _all, replace

format fecha %tm 
tsset fecha

levelsof fecha, local(times)
foreach time of local times {
    label define nom_fecha `time' `"`= strofreal(`time',"%tmCCYY-m")'"', add
}
label values fecha nom_fecha

gen lmil=round(TRABAJADORES/1000,0.1)
format lmil %3.0f
format REAL %3.1f
label var lmil "Trabajadores involucrados (miles)"
label var REAL "Incremento salarial real (%)"


gen lab_real=string(REAL, "%3.2f") 
gen breal="{bf:"+lab_real+"}"


keep if fecha>=tm(`year_firstp'm`mes_firstp')

*Original
twoway scatter lmil fecha, ms(none) mlabc(black) mlabposition(2) ///
mlabsize(3.2) mlabel(lmil) mlabangle(vertical) legend(off) || ///
bar lmil fecha, yaxis(1) xlabel(,format(%tmCCYY)) color("152 152 154") ytitle("Trabajadores involucrados (miles)") ///
ylabel(0(150)750, nogrid angle(horizontal) labsize(5) axis(1)) || ///
scatter REAL fecha if fecha==tm(`year_update'm`mes_update') , yaxis(2)  ms(none) mlabc("35 91 78") mlabposition(6) mlabsize(#9) mlabel(breal)  || ///
line REAL fecha, yaxis(2) yscale(alt) yscale(alt axis(1)) xlabel(,nogrid format(%tmCCYY)) ///
ylabel(-4(3)8, grid angle(horizontal) labsize(3.5) axis(2)) ///
color("35 91 78") ytitle("Incremento salarial real (%)", axis(2)) ///
xtitle("") ylabel(, angle(horizontal) labsize(3.5) axis(1)) ||, ysize(2.8) xsize(5.9) graphregion(color(white)) tlabel(`year_firstp'm`mes_firstp'(12)`year_update'm`mes_update')
graph export "$graphs\negociaciones_federales/neg_federal_`year_update'm`mes_update'.png", replace
graph export "$presentaciones/neg_federal_`year_update'm`mes_update'.png", replace

