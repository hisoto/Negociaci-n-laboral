***************************************
* Bases de NEGOCIACIONES SALARIALES
***************************************

clear all
global dofiles="d:\Users\ldiaz\OneDrive - Comision Nacional de los Salarios Minimos\datos para informe\Informe_api\Negociaciones laborales -Arturo\dofiles"
global graphs="d:\Users\ldiaz\OneDrive - Comision Nacional de los Salarios Minimos\datos para informe\Informe_api\Negociaciones laborales -Arturo\graphs"
global bases="d:\Users\ldiaz\OneDrive - Comision Nacional de los Salarios Minimos\datos para informe\Informe_api\Negociaciones laborales -Arturo\bases"


* Periodo al que se va a actualizar (año y mes):
local year_update=2024
local mes_update=4
* Periodo a partir del cual se quieren las gráficas (año y mes):
local year_firstp=2018
local mes_firstp=1


***************************************
*Base de SCIAN
***************************************
clear

foreach v in real revisiones trabajadores{
	clear
	import excel "$bases\negociaciones_scian.xlsx", sheet("scian_`v'") firstrow
	drop fecha
    g fecha = ym(year, mes)
    format fecha %tm 
    mvencode _all, mv(0) override
	
	forval s=0/19 {
	rename sec_`s' `v'_`s'
}
	save "$bases\scian_`v'.dta", replace 

}


use  "$bases\scian_real.dta", clear 
merge 1:1 fecha using "$bases\scian_revisiones.dta", nogenerate 
merge 1:1 fecha using "$bases\scian_trabajadores.dta", nogenerate 

reshape long real_ revisiones_ trabajadores_ , i(fecha) j(sector)

rename real_ real 
rename trabajadores_ trabajadores
rename revisiones_ revisiones

drop year mes 

keep if fecha==tm(`year_update'm`mes_update')


tostring sector, replace

replace sector="Total"	if sector=="0"	
replace sector="Agricultura, cría y explotación de animales, aprovechamiento forestal, pesca y caza" if sector=="1"	
replace sector="Minería" if sector=="2"	
replace sector="Generación, transmisión y distribución de energía eléctrica, suministro de agua y de gas por ductos al consumidor final" if sector=="3" 
replace sector="Construcción" if sector=="4"	
replace sector="Industrias manufactureras"	if sector=="5"	
replace sector="Comercio" if sector=="6"	
replace sector="Transportes, correos y almacenamiento"	if sector=="7"	
replace sector="Información en medios masivos"	if sector=="8"	
replace sector="Servicios financieros y de seguros" if sector=="9"	
replace sector="Servicios inmobiliarios y de alquiler de bienes muebles e intangibles" if sector=="10" 
replace sector="Servicios profesionales, científicos y técnicos" if sector=="11" 
replace sector="Corporativos" if sector=="12" 
replace sector="Servicios de apoyo a los negocios y manejo de desechos y servicios de remediación" if sector=="13" 
replace sector="Servicios educativos" if sector=="14" 
replace sector="Servicios de salud y de asistencia social" if sector=="15" 
replace sector="Servicios de esparcimiento culturales y deportivos, y otros servicios recreativos" if sector=="16" 
replace sector="Servicios de alojamiento temporal y de preparación de alimentos y bebidas" if sector=="17" 
replace sector="Otros servicios excepto actividades gubernamentales" if sector=="18" 
replace sector="Actividades legislativas, gubernamentales, de impartición de justicia y de organismos internacionales y extraterritoriales" if sector=="19" 


drop if real==0
drop fecha

gen porcen_rev=round((revisiones/revisiones[1])*100,0.01)
gen porcen_trab=round((trabajadores/trabajadores[1])*100,0.01)

order sector real trabajadores porcen_trab revisiones porcen_rev

format real porcen_trab porcen_rev %3.1gc

format trabajadores revisiones  %9.0gc  


gsort -real 

export excel using "$bases\negociaciones_scian.xlsx", sheet("Cuadro") sheetmodify cell(A3) 








