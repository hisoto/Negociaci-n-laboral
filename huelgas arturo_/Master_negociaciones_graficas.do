***************************************
* Graficas de NEGOCIACIONES SALARIALES
***************************************

clear all

global dofiles="d:\Users\ldiaz\OneDrive - Comision Nacional de los Salarios Minimos\datos para informe\Informe_api\Negociaciones laborales -Arturo\dofiles"
global graphs="d:\Users\ldiaz\OneDrive - Comision Nacional de los Salarios Minimos\datos para informe\Informe_api\Negociaciones laborales -Arturo\graphs"
global bases="d:\Users\ldiaz\OneDrive - Comision Nacional de los Salarios Minimos\datos para informe\Informe_api\Negociaciones laborales -Arturo\bases"
global presentaciones="d:\Users\ldiaz\OneDrive - Comision Nacional de los Salarios Minimos\proyectosDT\informes\automatizacion\graphs"


* Periodo al que se va a actualizar megociaciones federale(año y mes):
local year_update=2024
local mes_update=4

*Fecha de las negociaciones locales 
local year_local=2024
local mes_local=2

* Periodo a partir del cual se quieren las gráficas (año y mes):
local year_firstp=2019
local mes_firstp=1

***************************************

*include "$dofiles/grafica_negociaciones_federales.do"
*include "$dofiles/grafica_negociaciones_locales.do"
*include "$dofiles/grafica_negociaciones_central.do"
*include "$dofiles/grafica_negociaciones_empresas.do"
*include "$dofiles/grafica_negociaciones_entidades.do"
include "$dofiles/grafica_emplazamientos_nueva.do"
*include "$dofiles/grafica_huelgas.do"


