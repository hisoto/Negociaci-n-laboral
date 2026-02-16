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
*Bases que se actualizan con excel 
***************************************
include "$dofiles/bases_excel.do"
include "$dofiles/bases_central.do"
include "$dofiles/bases_scian.do"



