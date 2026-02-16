***************************************
*Base de Negociaciones Jurisdiccion Federal mensual 
***************************************
clear 
import excel "$bases\negociaciones_stata.xlsx", sheet("j_federal") firstrow clear
drop fecha
g fecha = ym(year, mes)
format fecha %tm 
save "$bases\neg_fed.dta", replace

drop fecha mes year

replace TRABAJADORES=TRABAJADORES/1000

format NOMINAL REAL %3.2f

export excel using "$bases\negociaciones.xlsx", sheet("JF mensual") sheetmodify cell(C1) firstrow(variables)

***************************************
*Base de Negociaciones Jurisdiccion Federal anual
***************************************
clear 
import excel "$bases\negociaciones_stata.xlsx", sheet("j_federal_anual") firstrow
save "$bases\neg_fed_anual.dta", replace

format NOMINAL REAL %3.2f

export excel using "$bases\negociaciones.xlsx", sheet("JF anual") sheetmodify cell(A1) firstrow(variables)


***************************************
*Base de Negociaciones Jurisdiccion Local mensual 
***************************************
clear 
import excel "$bases\negociaciones_stata.xlsx", sheet("j_local") firstrow clear
drop fecha
g fecha = ym(year, mes)
format fecha %tm 
save "$bases\neg_loc.dta", replace


***************************************
*Base de Negociaciones Jurisdiccion local anual
***************************************
clear 
import excel "$bases\negociaciones_stata.xlsx", sheet("j_local_anual") firstrow
save "$bases\neg_loc_anual.dta", replace





***************************************
*Base de Negociaciones por Empresas
***************************************
clear 
import excel "$bases\negociaciones_stata.xlsx", sheet("empresas") firstrow clear
drop fecha
g fecha = ym(year, mes)
format fecha %tm 
save "$bases\neg_emp.dta", replace




***************************************
*Base de entidades
***************************************
clear
import excel "$bases\negociaciones_stata.xlsx", sheet("entidad") firstrow
drop fecha
g fecha = ym(year, mes)
format fecha %tm 
mvencode _all, mv(0) override
save "$bases\neg_ent.dta", replace 


***************************************
*Base de Huelgas
***************************************
clear
import excel "$bases\negociaciones_stata.xlsx", sheet("huelgas") firstrow


save "$bases\huelgas.dta", replace 

***************************************
*Base de Emplazamientos a huelga nueva 
***************************************
clear
import excel "$bases\negociaciones_stata.xlsx", sheet("emplaza") firstrow


save "$bases\emplaza.dta", replace 



