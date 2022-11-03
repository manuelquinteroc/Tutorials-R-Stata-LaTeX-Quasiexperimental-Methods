* Tutorial en Stata 
* Profesor: Horacio Larreguy
* Asistente: Manuel Quintero


*** Preamble
clear
ssc install estout, replace // paquete para exportar tablas directamente a un compilador de LaTeX

* Directorio de trabajo
cd "~/Dropbox/My PC (DESKTOP-D2D98JJ)/Desktop/Tutoriales/Stata"

* Simulamos datos
clear
// Semilla aleatoria
set obs 100
generate x1 = rnormal()
generate x2 = rnormal(2)
generate y1	 = 2*x1 + 1 + rnormal()
generate y2 = 2*x1 + 3*x2 + 1 + rnormal()

* Agregamos etiquetas a cada variable
label variable x1   "Var. Independiente 1" 
label variable x2   "Var. Independiente 2" 
label variable y1   "Var. Dependiente 1" 
label variable y2   "Var. Dependiente 2" 

*-------------------------------------------------------
*** De Stata a LaTeX
*-------------------------------------------------------
* Guardamos tabla de estadísticas descriptivas para x1,x2,y1,y2
estpost tabstat x1 x2 y1 y2, c(stat) stat(mean sd min max n) // especificamos 

* Guardamos el codigo TeX
esttab using "./Tables/table1.tex", replace /// 
 cells("mean(fmt(%13.3fc)) sd(fmt(%13.3fc)) min max count") ///
 title("Tabla de estadísticas descriptivas") label ///
 collabels("Sum" "Mean" "SD" "Min" "Max" "N") noobs nonumbers

eststo clear  // borramos todas las estimaciones

*-------------------------------------------------------
*** Regresión lineal 
*-------------------------------------------------------
regress y1 x1  // corremos 
eststo: reghdfe y1 x1, noabsorb // corremos y guardamos regresión
qui testparm* // prueba de hipótesis
estadd scalar p_value = round(r(p),3) // guardamos el valor p de la prueba F

eststo: reghdfe y2 x1 x2 noabsorb   // corremos y guardamos regresión
qui testparm*
estadd scalar p_value = round(r(p),3)

* Creamos tabla 
esttab using "./Tables/table2.tex", replace  ///
title(Regresión Lineal) ///
addnote("This table includes ...") ///
se star(* 0.10 ** 0.05 *** 0.01) label ///
stats(N r2 p_value, labels("Observations" "R-squared" "F-stat (p-value)"))

eststo clear  // borramos todas las estimaciones

*-------------------------------------------------------
*** Manejo de datos 
*-------------------------------------------------------
clear
set seed 32 // Semilla aleatoria
set obs 5 // Número de observaciones 
gen ind = _n // Generamos variable ind de 1 a n = 5
expand 5 // Hacemos de cada ind 5 observaciones
gen x = rnormal() // Generamos x
gen y = rnormal() // Generamos x
gen z = 0 if x < 0.5 // Generamos z con condicional
replace z = 1 if z == . // Remplazamos valores NA de z con 1
keep if y > 0 // Filtramos aquellos renglones donde y > 0
collapse (sum) x y, by(ind) // Colapsamos los datos sumando las variables x e y, agrupando por ind


*-------------------------------------------------------
*** Diferencias en diferencias
*-------------------------------------------------------
* Ejercio con 2 periodos ---------------------------------------------
clear
* Cargar los datos en la base dd_data a la memoria
use "dd_data.dta"
label variable post   "Post" 
label variable wage   "Salario" 
label variable Treatment   "Tratamiento" 

* Calculamos la regresión DID
eststo: reghdfe wage post##Treatment, noabsorb

eststo: diff wage, t(Treatment) p(post)

esttab using "./Tables/table3.tex", replace noomitted nobaselevels label se r2 title("Regresión DID en 2 periodos") ///
mtitles("Modelo reg" "Modelo diff") ///
star(* 0.10 ** 0.05 *** 0.01) ///
nonumbers

eststo clear 

* Ejercio con multiples periodos ---------------------------------------------
* Simulamos datos
clear all
set obs 20
gen ind = _n
expand 10 
bysort ind: egen time = seq(), f(2010) t(2019) // dentro de cada ind
gen treat = 1 if ind <= 10
replace treat = 0 if treat == .

* Generamos la varible donde empieza el evento
set seed 1 // Semilla aleatoria
gen year_treated = runiformint(2013,2017) 
bysort ind: replace year_treated = year_treated[1]
replace year_treated = 0 if treat == 0

* Dummy de treatment con 1 después del tratamiento
bysort ind: gen post_treatment = 1 if time >= year_treated
replace post_treatment = 0 if post_treatment == .
replace post_treatment = 0 if treat == 0

* Generamos Leads y lags 
* Llenamos las observacioens de control con 0
bysort ind: gen lead3 = post_treatment[_n+3]
replace lead3 = 0 if treat == 0
bysort ind: gen lead2 = post_treatment[_n+2]
replace lead2 = 0 if treat == 0
bysort ind: gen lead1 = post_treatment[_n+1] 
replace lead1 = 0 if treat == 0
bysort ind: gen lag1 = post_treatment[_n-1] 
replace lag1 = 0 if treat == 0
bysort ind: gen lag2 = post_treatment[_n-2] 
replace lag2 = 0 if treat == 0
bysort ind: gen lag3 = post_treatment[_n-3] 
replace lag3 = 0 if treat == 0

* Simulamos la variable endógena  
by ind: gen outcome =  runiform(0,1) if time < year_treated  // Pre
by ind: replace outcome = runiform(2,3) if time == year_treated // Evento
by ind: replace outcome = runiform(3.5,4.5) if time > year_treated // Post

* Generamos la variable endógena de control con pre tendencia paralela
replace outcome =  runiform(0,1) if treat == 0

* Two-way fixed effects (2FE) -----------------------------------------
reghdfe outcome post_treatment, a(ind time)

* Especificación Leads and Lags ----------------------------------------
* Usando xtreg
xtset ind time
xtreg outcome lead3 lead2 lead1 post_treatment lag1 lag2 lag3 i.time, fe

* Usando reghdfe
reghdfe outcome lead3 lead2 lead1 post_treatment lag1 lag2 lag3, a(ind time)

sum outcome 
return list
estadd local Cluster "Normal" // Var aux
estadd scalar Min = r(min) // valor min
estadd scalar Max = r(max) // valor max
estadd scalar Count = r(N) // obs
estadd scalar Mean = r(mean) // media
estadd scalar SD = r(sd) // std. dev.
est sto est1

label variable lead3   "Lead 3" 
label variable lead2   "Lead 2" 
label variable lead1   "Lead 1" 
label variable post_treatment   "Periodo 0" 
label variable outcome   "Var. Dependiente" 

* Tabla de leads and lags, omitimos lags
esttab est1 using "./Tables/table4.tex", replace noomitted nobaselevels label se r2 title("Leads and lags") keep(lead3 lead2 lead1 post_treatment) scalars("DF" "Cluster") stats(N r2 Mean SD Min Max Cluster, label(N \(R^{2}\) "DepVar: Mean" "DepVar: Std.Dev." "Scale: min" "Scale: max" "Std. Errors")) star(* 0.10 ** 0.05 *** 0.01) notes addnotes("Lag variables are included but not shown.") nonumbers 

* Gráfica de Leads and lags
ssc install coefplot, replace

coefplot, vertical drop(_cons) yline(0) coeflabels(lead3 = "t+3" lead2 = "t+2" lead1 = "t+1" post_treatment = "t" lag1 = "t-1" lag2 = "t+2" lag3 = "t-3") xline(3.5, lp(dash)) ciopts(recast(rcap)) xtitle(Tiempo) ytitle(Coeficiente: 95% IC) graphregion(color(white))

* Guardamos la gráfica en formato pdf
graph export "LeadsLags.pdf", replace

* DID plot (event study) -----------------------------------------------
gen time_to_treat = time - year_treated // Creamos variable tiempo al tratamiento

replace time_to_treat = 0 if treat == 0 // Cambiamos valores NA a 0 del grupo control

summ time_to_treat 
g shifted_ttt = time_to_treat - r(min) // Stata toma solo factores de valores positivos, hacemos un mapeo de time to treat (ttt) a shifted_ttt para valores positivos. (Sugerencia, vean los datos como se van creando las variables)

summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean) // Variable local que nos da la categoria base, ttt = 0

* Corremos la regresión especificando la categoria base y con 2FE
reghdfe outcome ib`true_neg1'.shifted_ttt, a(ind time) 

* Gráficamos el event study
coefplot, keep(*.shifted_ttt) vertical  base ///
rename(0.shifted_ttt="-6" 1.shifted_ttt="-5" 2.shifted_ttt="-4" 3.shifted_ttt="-3" 4.shifted_ttt="-2" 5.shifted_ttt="-1" 6.shifted_ttt="Evento" 7.shifted_ttt = "1" 8.shifted_ttt = "2" 9.shifted_ttt = "3" 10.shifted_ttt = "4" 11.shifted_ttt = "5" 12.shifted_ttt = "6" 13.shifted_ttt = "7") ///
yline(0) xline(7, lp(dash)) ciopts(recast(rcap)) xtitle(Años desde tratamiento) ytitle(Coeficiente: 95% IC) graphregion(color(white))

* Guardamos la gráfica en formato pdf
graph export "DIDplot.pdf", replace

*----------------------------------------------------
*** Variables Instrumentales  *----------------------------------------------------
ssc install ivreghdfe, replace
clear
* Generamos datos
set seed 32 // Semilla aleatoria
set obs 100
generate x1 = rnormal()
generate x2 = rnormal(2)
generate y1	 = 2*x1 + 1 + rnormal()
generate y2 = 2*x1 + 3*x2 + 1 + rnormal()
set seed 32 // Semilla aleatoria
gen z1 = 0.85*x1 + 0.2*x2^3 + rnormal(0,1)
gen z2 = 0.5*x2 + 0.1*x1^2 + rnormal(0,1)

// IV paso a paso
// First stage
reghdfe x1 z1, noabsorb vce(robust) // fs
estimates store IVfs // Guardamos la regresión
qui testparm* // Prueba de hipotesis
estadd scalar p_value = round(r(p),3) // Guardamos el p value del partial f test

* Corremos la estimación en 2 pasos con un solo comando
ivreghdfe y1 (x1 = z1), robust
estimates store IV2sls // Guardamos la regresión

* Generamos la tabla en .tex
esttab IVfs IV2sls using "./Tables/table5.tex", ///
title(IV: First stage and 2SLS) ///
replace se label ///
star(* 0.10 ** 0.05 *** 0.01) ///
mtitles("First Stage" "2SLS") ///
stats(N r2 p_value, labels("Observations" "R-squared" "F-stat (p-value)"))

eststo clear  // Borramos todas las estimaciones
 
estimates clear // Borramos las regresiones guardadas

// Multiple regresiones IV
eststo: ivreghdfe y1 (x1 = z1)
eststo: ivreghdfe y1 x1 (x2 = z2)
eststo: ivreghdfe y2 (x1 x2 = z1 z2)
eststo: ivreghdfe y2 (x1 x2 = z1 z2), robust

esttab using "./Tables/table6.tex", replace noomitted nobaselevels label se r2 star(* 0.10 ** 0.05 *** 0.01) ///
nonotes postfoot("\hline  \hline \\[-1.8ex]  \multicolumn{5}{l} {\parbox[t]{11cm}{ \textit{Nota:} De esta manera podemos modificar las notas. * denota p$<$0.1, ** denota p$<$0.05, y *** denota p$<$0.01.}}  \\  \end{tabular} ") 

* En LaTeX queremos escribirlo de la sigueinte manera
* \begin{table}[H]
* \centering
* \caption{Multiple IV regressions}
* \input{table6.tex} 
* \end{table}


eststo clear  // borramos todas las estimaciones


*-------------------------------------------------------
*** Rgresion Discontinua
* Paquetes rddensity y rdrobust son requeridos *-------------------------------------------------------
clear all
set seed 1 // Semilla aleatoria
set obs 100
generate x = runiform(-2, 2) 
generate y = 1 + x + 5*(x >= 0) + rnormal()
gen z = 0
replace z = 1 if x > 0

* McCrary Test (bajar el archivo .ado)
DCdensity x, breakpoint(0) generate(Xj Yj r0 fhat se_fhat)
 drop Xj Yj r0 fhat se_fhat
 
* Uando rddensity
rddensity x // Test de manipulation, H0: cutoff manipulado

* Paquete rd
ssc install rd, replace

* Gráfica automática del diseño RD
rdplot y x, p(1) graph_options(legend(off) graphregion(color(white)) xtitle(x) ytitle(y))


* Guardamos la gráfica en formato pdf
graph export "RDD.pdf", replace

* Bandwidth
rdbwselect y x, bwselect(IK) // bw optimo

* Estimación
eststo: rdrobust y x rdbwselect(ik)

esttab using "./Tables/table7.tex", replace noomitted nobaselevels label se r2  title(RDD) 
