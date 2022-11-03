* DID, leads and lags and DID plot
* Profesor: Horacio Larreguy
* Asistente: Manuel Quintero

*** Preamble
* Directorio de trabajo
cd "~/Dropbox/My PC (DESKTOP-D2D98JJ)/Desktop/Tutoriales/Stata/DID"

*** Importamos datos
clear
use "DID_data.dta"

*** Regresión DID
reghdfe outcome tratamiento, a(unidades tiempo) cluster(unidades)

* Creamos variable que especifica el grupo: tratamiento o control
gsort unidades -tratamiento
bysort unidades: gen treat = 1 if tratamiento[1] > 0
replace treat = 0 if treat == .

* Regresamos a la forma original de la base de datos
sort unidades tiempo

* Generamos Leads y lags 
* Llenamos las observacioens de control con 0
bysort unidades: gen lead3 = tratamiento[_n+3]
replace lead3 = 0 if treat == 0
bysort unidades: gen lead2 = tratamiento[_n+2]
replace lead2 = 0 if treat == 0
bysort unidades: gen lead1 = tratamiento[_n+1] 
replace lead1 = 0 if treat == 0
bysort unidades: gen lag1 = tratamiento[_n-1] 
replace lag1 = 0 if treat == 0
bysort unidades: gen lag2 = tratamiento[_n-2] 
replace lag2 = 0 if treat == 0
bysort unidades: gen lag3 = tratamiento[_n-3] 
replace lag3 = 0 if treat == 0

*** Regresión leads y lags
reghdfe outcome lead* tratamiento lag*, a(unidades tiempo) cluster(unidades)

* Exportamos resultados a LaTeX
sum outcome 
return list
estadd local Cluster "unidades" // Var aux
estadd scalar Min = r(min) // valor min
estadd scalar Max = r(max) // valor max
estadd scalar Count = r(N) // obs
estadd scalar Mean = r(mean) // media
estadd scalar SD = r(sd) // std. dev.
est sto est1

label variable lead3   "Tratamiento Lead 3" 
label variable lead2   "Tratamiento Lead 2" 
label variable lead1   "Tratamiento Lead 1" 
label variable tratamiento  "Tratamiento" 
label variable outcome   "Outcome" 

* Tabla de leads and lags, omitimos lags
esttab est1 using "./table_leadsLags_Stata.tex", replace noomitted nobaselevels label se r2 title("Leads and lags") keep(lead3 lead2 lead1 tratamiento) scalars("DF" "Cluster") stats(N r2 Mean SD Min Max Cluster, label(N \(R^{2}\) "Outcome mean" "Outcome std. Dev." "Outcome min" "Outcome max" "Cluster")) star(* 0.10 ** 0.05 *** 0.01) notes addnotes("Lag variables are included but not shown.") nonumbers 

estimates clear // Borramos las regresiones guardadas

* Gráfica de Leads and lags
ssc install coefplot, replace

coefplot, vertical drop(_cons) yline(0) coeflabels(lead3 = "t+3" lead2 = "t+2" lead1 = "t+1" post_treatment = "t" lag1 = "t-1" lag2 = "t+2" lag3 = "t-3") xline(3.5, lp(dash)) ciopts(recast(rcap)) xtitle(Periodo) ytitle(Coeficiente (95% IC)) graphregion(color(white))

* Guardamos la gráfica en formato pdf
graph export "LeadsLags_Stata.pdf", replace

* DID plot (event study) 
* Generamos una variable post:
bysort unidades: gen post = 1 if tratamiento > 0
replace post = 0 if post == .

* Generamos una variable que indica el tiempo del evento
bysort unidades: gen event_time = tiempo if post == 1
sort unidades event_time
bysort unidades: replace event_time = event_time[1]
sort unidades tiempo
replace event_time = 0 if treat == 0  

gen time_to_event = tiempo - event_time // Creamos variable tiempo al tratamiento

replace time_to_event = 0 if treat == 0 // Cambiamos valores NA a 0 del grupo control

summ time_to_event 
g shifted_ttt = time_to_event - r(min) // Stata toma solo factores de valores positivos, hacemos un mapeo de tiempo to treat (ttt) a shifted_ttt para valores positivos. (Sugerencia, vean los datos como se van creando las variables)

* Creamos variable local para usar como base
summ shifted_ttt if time_to_event == -1
local true_neg1 = r(mean) // Variable local que nos da la categoria base, ttt = 0

* Corremos la regresión especificando la categoria base, FE y cluster
reghdfe outcome ib`true_neg1'.shifted_ttt, a(unidades tiempo) vce(cluster unidades)

* Gráficamos el event study
coefplot, keep(*.shifted_ttt) vertical  base ///
rename(0.shifted_ttt="-6" 1.shifted_ttt="-5" 2.shifted_ttt="-4" 3.shifted_ttt="-3" 4.shifted_ttt="-2" 5.shifted_ttt="-1" 6.shifted_ttt="Evento" 7.shifted_ttt = "1" 8.shifted_ttt = "2" 9.shifted_ttt = "3" 10.shifted_ttt = "4" 11.shifted_ttt = "5" 12.shifted_ttt = "6" 13.shifted_ttt = "7") ///
yline(0) xline(7, lp(dash)) ciopts(recast(rcap)) xtitle(Años desde tratamiento) ytitle(Coeficiente (95% IC)) graphregion(color(white))

* Guardamos la gráfica en formato pdf
graph export "DIDplot_Stata.pdf", replace







