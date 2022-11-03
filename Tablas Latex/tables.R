# Ejemplo de una tabla con buen formato

library(stargazer)
library(lfe)
library(matrixStats) # para usar la función colSds

# Simulamos datos
set.seed(32)

code_inegi <- rep(1:10, each = 10)
year <- rep(2010:2019, 10)
datos <- cbind.data.frame(code_inegi, year) %>% group_by(code_inegi) %>% mutate(x = rnorm(10, 2, 0.5)) 
datos <- datos %>% group_by(code_inegi) %>%  mutate(z = 0.5*x + rnorm(10, 2, 0.5)) %>% mutate(y1 = 2*x + z^2 + rnorm(1), 
                                                                                     y2 = 0.5*x*x + 0.5*z + rnorm(1),
                                                                                     y3 = x*z + rnorm(1),
                                                                                     y4 = z + x + rnorm(1),
                                                                                     y5 = 2 + sqrt(z) +z*x + rnorm(1),
                                                                                     w1 = 2*x + z^2 + rnorm(1), 
                                                                                     w2 = 0.5*x + 0.5*z + rnorm(1),
                                                                                     w3 = x*z*x + rnorm(1),
                                                                                     w4 = z*z + x*x + rnorm(1),
                                                                                     w5 = 2 + z +z*x + rnorm(1),)

datos <- datos %>% group_by(code_inegi) %>% mutate(control1 = sqrt(x) + runif(10,0,1), control2 = 0.5*runif(10,0,1)^2,
                                                   lagged_var = runif(10,0,1) + x^3)

# tabla en stargazer --------------------------------------------------------------

aux <- c("y1","y2","y3","y4","y5")
resultados_data <- datos[aux]

res_mean <- round(colMeans(resultados_data), digits = 3)
res_sd <- round(colSds(as.matrix(resultados_data)), digits = 3)


reg1 <- felm(y1 ~ x + z + lagged_var | code_inegi + year | 0 |code_inegi, datos)

reg2 <- felm(y2 ~ x + z + control1 + lagged_var| code_inegi + year | 0 |code_inegi, datos)

reg3 <- felm(y3 ~ x + z + control1 + lagged_var| code_inegi + year | 0 |code_inegi, datos)

reg4 <- felm(y4 ~ x + z + control2 + lagged_var| code_inegi + year | 0 |code_inegi, datos)

reg5 <- felm(y5 ~ x + z + control1 + control2 + lagged_var| code_inegi + year | 0 |code_inegi, datos)

# Todos los nombres fueron inventados para propositos ilustrativos

# Tabla 1 -------------------------------------------------------------------------
dep_var <- c("No education",
             "\\shortstack{Primary \\\\ education}",
             "\\shortstack{Technical or \\\\ Secondary \\\\ education}",
             "\\shortstack{College \\\\ education \\\\ or equivalent}",
             "\\shortstack{Higher \\\\ education}")

tabla1 <- stargazer(reg1, reg2, reg3, reg4, reg5,
                    header=FALSE,
                    font.size="scriptsize",
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    omit = c("Constant","lagged_var"),
                    column.labels = dep_var,
                    covariate.labels = c("Mother education", "Father education", "Total population", "Social gap index"),
                    omit.stat=c("f", "ser","adj.rsq"),
                    add.lines = list(c("Outcome mean", round(res_mean, 3)),
                                     c("Outcome std. dev.", round(res_sd, 3))),
                    title = "Analysis of the different educational levels",
                    type = "latex")


note.latex <- "\\multicolumn{6}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} 
We report estimates from OLS regressions, including municipal and time fixed effects.
All specifications further include the lagged dependent variable as a control. 
Standard errors clustered by municipality are in parentheses. 
* denote p$<$0.1, ** denote p$<$0.05, y *** denote p$<$0.01.}} \\\\"
tabla1[grepl("Note", tabla1)] <- note.latex

cat(tabla1, file = 'tabla1.tex')


# Tabla 2 -------------------------------------------------------------------------
tabla2 <- stargazer(reg1, reg2, reg3, reg4, reg5,
                    header=FALSE,
                    font.size="scriptsize",
                    dep.var.caption = "Variables de educación",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    omit = c("Constant","lagged_var"),
                    column.labels = "",
                    covariate.labels = c("Mother education", "Father education", "Total population", "Social gap index"),
                    omit.stat=c("f", "ser","adj.rsq"),
                    add.lines = list(c("Outcome mean", round(res_mean, 3)),
                                     c("Outcome std. dev.", round(res_sd, 3))),
                    title = "Analysis of the different educational levels",
                    type = "latex")


note.latex <- "\\multicolumn{6}{l} {\\parbox[t]{11cm}{ \\textit{Notes:} 
We report estimates from OLS regressions, including municipal and time fixed effects.
All specifications further include the lagged dependent variable as a control. 
Standard errors clustered by municipality are in parentheses. 
* denote p$<$0.1, ** denote p$<$0.05, y *** denote p$<$0.01.}} \\\\"
tabla2[grepl("Note", tabla2)] <- note.latex

cat(tabla2, file = 'tabla2.tex')


# Tabla 3: tabla ajustada ----------------------------------------------------------------------
aux <- c("y1","y2","y3","y4","y5", "w1", "w2", "w3", "w4", "w5")
resultados_data <- datos[aux]

res_mean <- round(colMeans(resultados_data), digits = 3)
res_sd <- round(colSds(as.matrix(resultados_data)), digits = 3)

# long table
reg6 <- felm(w1 ~ x + z + lagged_var | code_inegi + year | 0 |code_inegi, datos)

reg7 <- felm(w2 ~ x + z + control1 + lagged_var| code_inegi + year | 0 |code_inegi, datos)

reg8 <- felm(w3 ~ x + z + control1 + lagged_var| code_inegi + year | 0 |code_inegi, datos)

reg9 <- felm(w4 ~ x + z + control2 + lagged_var| code_inegi + year | 0 |code_inegi, datos)

reg10 <- felm(w5 ~ x + z + control1 + control2 + lagged_var| code_inegi + year | 0 |code_inegi, datos)


dep_var <- c("\\shortstack{No \\\\ education  \\\\ Mex}",
             "\\shortstack{Primary \\\\ education \\\\  Mex}",
             "\\shortstack{Technical or \\\\ Secondary \\\\ education \\\\  Mex}",
             "\\shortstack{College \\\\ education \\\\ or equivalent \\\\  Mex}",
             "\\shortstack{Higher \\\\ education \\\\  Mex}", 
             "\\shortstack{No  \\\\  education  \\\\ USA}",
             "\\shortstack{Primary \\\\ education  \\\\ USA}",
             "\\shortstack{Technical or \\\\ Secondary \\\\ education  \\\\ USA}",
             "\\shortstack{College \\\\ education \\\\ or equivalent  \\\\ USA}",
             "\\shortstack{Higher \\\\ education  \\\\ USA}")

tabla3 <- stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10,
                    header=FALSE,
                    font.size="tiny", # so that it fits horizontally
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    omit = c("Constant","lagged_var"),
                    column.labels = dep_var,
                    covariate.labels = c("Mother education", "Father education", "Total population", "Social gap index"),
                    omit.stat=c("f", "ser","adj.rsq"),
                    add.lines = list(c("Outcome mean", round(res_mean, 3)),
                                     c("Outcome std. dev.", round(res_sd, 3))),
                    title = "Analysis of the different educational levels",
                    type = "latex")


note.latex <- "\\multicolumn{11}{l} {\\parbox[t]{20cm}{ \\textit{Notes:} 
We report estimates from OLS regressions, including municipal and time fixed effects.
All specifications further include the lagged dependent variable as a control. 
Standard errors clustered by municipality are in parentheses. 
Columns 6 to 10 include the US unemployment index as a control.
* denote p$<$0.1, ** denote p$<$0.05, y *** denote p$<$0.01.}} \\\\"
tabla3[grepl("Note", tabla3)] <- note.latex

cat(tabla3, file = 'tabla3.tex')


# Table 4: sidewysetable ----------------------------------------------
tabla4 <- stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10,
                    header=FALSE,
                    font.size="tiny", # so that it fits horizontally
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    table.placement = "t", # change to top of the page
                    float.env = "sidewaystable", # environment to sidewysetable (add rotating package to preamble in overleaf)
                    omit = c("Constant","lagged_var"),
                    column.labels = dep_var,
                    covariate.labels = c("Mother education", "Father education", "Total population", "Social gap index"),
                    omit.stat=c("f", "ser","adj.rsq"),
                    add.lines = list(c("Outcome mean", round(res_mean, 3)),
                                     c("Outcome std. dev.", round(res_sd, 3))),
                    title = "Analysis of the different educational levels",
                    type = "latex")


note.latex <- "\\multicolumn{11}{l} {\\parbox[t]{23cm}{ \\textit{Notes:} 
We report estimates from OLS regressions, including municipal and time fixed effects.
All specifications further include the lagged dependent variable as a control. 
Standard errors clustered by municipality are in parentheses. 
Columns 6 to 10 include the US unemployment index as a control.
* denote p$<$0.1, ** denote p$<$0.05, y *** denote p$<$0.01.}} \\\\"
tabla4[grepl("Note", tabla4)] <- note.latex

cat(tabla4, file = 'tabla4.tex')

