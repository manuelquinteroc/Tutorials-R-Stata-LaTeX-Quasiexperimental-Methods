library(lfe)
library(dplyr)
library(car)
library(stargazer)

rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Simular datos ----------------------------------------------------------------
set.seed(32)

unidades <- rep(1:10, each = 10)
tiempo <- rep(2010:2019, 10)
datos <- cbind.data.frame(unidades, tiempo) %>% group_by(unidades) %>% mutate(x = rnorm(10, 2, 0.5)) 
datos <- datos %>% group_by(unidades) %>%  mutate(z = 3*x + rnorm(1,0.4)) %>% mutate(y = 2*x + rnorm(1))

datos <- datos %>% group_by(unidades) %>% mutate(control1 = sqrt(x) + runif(10,0,1), control2 = 0.5*runif(10,0,1)^2)

datos <- datos %>% group_by(unidades) %>% mutate(placebo = 0.5*y + z*x + rnorm(1,0.5))

#  -----------------------------------------------------------------------------
# FS - RF - IV 
fs <- felm(x ~ z + control1 + control2 | tiempo + unidades | 0 | unidades, datos)
rf <- felm(y ~ z + control1 + control2 | tiempo + unidades | 0 | unidades, datos)
iv <- felm(y ~ control1 + control2 | tiempo | (x ~ z) | unidades, datos)

summary(fs)
summary(rf)
summary(iv)

# Partial F stat del FS 
fstat <- linearHypothesis(fs, c("z = 0"), test = "F")[4]$`Pr(>F)`[2]

# Tabla
tableIV <-  stargazer(fs, rf, iv,
                      header=FALSE,
                      font.size="scriptsize",
                      dep.var.caption = "",
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      omit = "Constant",
                      column.labels = c("First Stage", "Reduced Form", "IV"),
                      covariate.labels = c("Z", "Control 1", "Control 2", "IV coefficient"),
                      omit.stat=c("f", "ser","adj.rsq"),
                      add.lines = list(c("P-value (F)", round(fstat,3), NA,NA)),
                      title = "Instrumental variables",
                      type = "latex",
                      out = 'tablaIV.tex')


# Placebo
placebo <- felm(placebo ~ z + control1 + control2 | tiempo | 0 | unidades, datos)

# Si el coeficiente de Z es significativo en la regresión de placebo
fs_placebo <- felm(x ~ z + control1 + control2 + placebo | tiempo + unidades | 0 | unidades, datos)
rf_placebo <- felm(y ~ z + control1 + control2 + placebo | tiempo + unidades | 0 | unidades, datos)
iv_placebo <- felm(y ~ control1 + control2 + placebo| tiempo | (x ~ z) | unidades, datos)


# Sino quedarse fon fs, rf, iv sin incluir el placebo

