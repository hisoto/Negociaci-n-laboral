***************************************
*Bases de Negociaciones importadas de excel 
***************************************

***************************************
*Base de Negociaciones por Central obrera mensual
***************************************
clear 
foreach p in ctm croc crom sna_asa otras ind_ct no_esp {
import excel "$bases\negociaciones_central.xlsx", sheet("`p'") firstrow clear
drop fecha
g fecha = ym(year, mes)
format fecha %tm 
save "$bases\neg_`p'.dta", replace	
	
}

use "$bases\neg_ctm.dta", clear
foreach j in croc crom sna_asa otras ind_ct no_esp {
	merge 1:1 fecha using "$bases\neg_`j'.dta"
	drop _merge
}

order fecha mes year REAL_* REVISIONES_*  TRABAJADORES_* NOMINAL_*
save "$bases\neg_co.dta", replace

drop REVISIONES_* NOMINAL_*
drop fecha mes year

foreach k in ctm croc crom sna_asa otras ind_ct no_esp {
	replace TRABAJADORES_`k'=TRABAJADORES_`k'/1000
}

order TRABAJADORES_ctm REAL_ctm TRABAJADORES_croc REAL_croc TRABAJADORES_crom REAL_crom TRABAJADORES_sna_asa REAL_sna_asa TRABAJADORES_otras REAL_otras TRABAJADORES_ind_ct REAL_ind_ct TRABAJADORES_no_esp REAL_no_esp

format REAL_* %3.2f
export excel using "$bases\negociaciones.xlsx", sheet("central_obrera") sheetmodify cell(C3) 


***************************************
*Base de Negociaciones por Central obrera anual
***************************************
clear 

foreach c in ctm croc crom sna_asa otras ind_ct no_esp {
import excel "$bases\negociaciones_central.xlsx", sheet("`c'_anual") firstrow clear

save "$bases\neg_`c'_anual.dta", replace	
	
}

use "$bases\neg_ctm_anual.dta", clear
foreach q in croc crom sna_asa otras ind_ct no_esp {
	merge 1:1 year using "$bases\neg_`q'_anual.dta"
	drop _merge
}

order year REAL_* REVISIONES_*  TRABAJADORES_* NOMINAL_*

save "$bases\neg_co_anual.dta", replace

drop REVISIONES_* NOMINAL_*


order year TRABAJADORES_ctm REAL_ctm TRABAJADORES_croc REAL_croc TRABAJADORES_crom REAL_crom TRABAJADORES_sna_asa REAL_sna_asa TRABAJADORES_otras REAL_otras TRABAJADORES_ind_ct REAL_ind_ct TRABAJADORES_no_esp REAL_no_esp


format REAL_* %3.2f

export excel using "$bases\negociaciones.xlsx", sheet("CO_anual") sheetmodify cell(A3) 
