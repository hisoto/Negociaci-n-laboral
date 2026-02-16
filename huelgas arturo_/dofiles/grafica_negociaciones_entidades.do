clear all
set more off

*Directorio de trabajo

use "$bases\neg_ent.dta", clear 

rename 	Aguascalientes	v_1
rename 	BajaCalifornia	v_2
rename 	BajaCaliforniaSur	v_3
rename 	Campeche	v_4
rename 	Coahuila	v_5
rename 	Colima	v_6
rename 	Chiapas	v_7
rename 	Chihuahua	v_8
rename 	CiudaddeMéxico	v_9
rename 	Durango	v_10
rename 	Guanajuato	v_11
rename 	Guerrero	v_12
rename 	Hidalgo	v_13
rename 	Jalisco	v_14
rename 	México	v_15
rename 	Michoacán	v_16
rename 	Morelos	v_17
rename 	Nayarit	v_18
rename 	NuevoLeón	v_19
rename 	Oaxaca	v_20
rename 	Puebla	v_21
rename 	Querétaro	v_22
rename 	QuintanaRoo	v_23
rename 	SanLuisPotosí	v_24
rename 	Sinaloa	v_25
rename 	Sonora	v_26
rename 	Tabasco	v_27
rename 	Tamaulipas	v_28
rename 	Tlaxcala	v_29
rename 	Veracruz	v_30
rename 	Yucatán	v_31
rename 	Zacatecas	v_32


keep if fecha==tm(`year_update'm`mes_update')

reshape long v_ , i(fecha) j(ent)

rename v_ real
drop if real==0

label define	ent	1	"Aguascalientes"	, add
label define	ent	2	"Baja California"	, add
label define    ent 3   "Baja California Sur" , add
label define	ent	4	"Campeche"	, add
label define	ent	5	"Coahuila"	, add
label define	ent	6	"Colima"	, add
label define	ent	7	"Chiapas"	, add
label define	ent	8	"Chihuahua"	, add
label define	ent	9	"Ciudad de México"	, add
label define	ent	10	"Durango"	, add
label define	ent	11	"Guanajuato"	, add
label define	ent	12	"Guerrero"	, add
label define	ent	13	"Hidalgo"	, add
label define	ent	14	"Jalisco"	, add
label define	ent	15	"México"	, add
label define	ent	16	"Michoacán"	, add
label define	ent	17	"Morelos"	, add
label define	ent	18	"Nayarit"	, add
label define	ent	19	"Nuevo León"	, add
label define	ent	20	"Oaxaca"	, add
label define	ent	21	"Puebla"	, add
label define	ent	22	"Querétaro"	, add
label define	ent	23	"Quintana Roo"	, add
label define	ent	24	"San Luis Potosí"	, add
label define	ent	25	"Sinaloa"	, add
label define	ent	26	"Sonora"	, add
label define	ent	27	"Tabasco"	, add
label define	ent	28	"Tamaulipas"	, add
label define	ent	29	"Tlaxcala"	, add
label define	ent	30	"Veracruz"	, add
label define	ent	31	"Yucatán"	, add
label define	ent	32	"Zacatecas"	, add


label value ent ent
graph bar real, over(ent, sort(real) descending lab(angle(vertical) labsize(4)) axis(lcolor(white)) )  ytitle("") ///
bar(1, color("188 149 92")) blabel(bar, orientation(vertical) size(4) format(%2.1f) ) ///
ylab(-3(5)16, nogrid) yline(0, lcolor("111 114 113") )  ///
ysc(off) ysize(2.8) xsize(5.9) legend(off) graphregion(color(white))

graph export "$graphs\negociaciones_entidades/neg_ent_`year_update'm`mes_update'.png", replace

