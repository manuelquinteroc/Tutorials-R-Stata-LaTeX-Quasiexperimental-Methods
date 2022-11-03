# Primer tutorial en R: Regresiones lineales, Variables Instrumentales, Dif and Dif y Regresión discontinua
# Profesor: Horacio Larreguy 
# Asistente: Manuel Quintero
# ITAM, Otoño 2021

# Instalamos los paquete necesarios
list.of.packages <- c("stargazer", "xtable", "AER", "biostat3", "car", "ggplot2", "did", "rddtools", "dplyr", "plm", "haven")

# Verificamos aquellos paquetes que no han sido instalados
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Los instalamos y cargamos todos a la vez
if(length(new.packages)) install.packages(new.packages) # verificamos que exista al menos un paquete en new.packages y lo instalamos
lapply(list.of.packages, library, character.only = TRUE) # Función en R que aplica la función library a todos los elementos en list.of.packages 

# Limpiamos el workspace
rm(list = ls())

# Establecemos el directorio de trabajo al directorio de este documento con el paquete rstudioapi
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# -------------------------------------------------------------------------------
rm(list = ls()) # Limpia todo lo almacenado en el workspace
rm(list = setdiff(ls(), "x")) # Elimina todo excepto la variable x

.rs.restartR() # Reiniciar la sesión: limpia el workspace y paquetes

install.packages("nombre")
# Podemos utilizar un paquete sin instalarlo de la siguiente forma,
nombreDelPaquete::funcion # Donde función es una operación dentro del paquete nombreDelPaquete

# Comando para desinstalar un paquete
remove.packages("nombre") 

# Comando para cargar un paquete y poder utilizarlo 
library(nombre)

# Comando para cambiar directorio de trabajo
setwd("C:/Users/nombre/Desktop/Tutorial") # establece directorio
getwd('') # string que regresa el directorio actual

# Comando para obtener ayuda de una función específica
help(mean)
?mean

# Vectores  ------------------------------------------------------------------------------------
palabras <- c("carro", "barco", "avión")
x <- c(1,2,3,4)
y <- c(2,4,6,8)

# Aritmética vectorial
sqrt(x) # Raiz cuadrada
x + y # Suma
2*x*y # Producto
x/y  # Division entrada a entrada

length(x) # Longitud del vector
sum(x) # Suma las entradas del vector

# Generar sucesiones
x <- c(1:10) # Ascendente del 1 al 10, enteros
y <- c(10:1) # Descendente del 10 al 1, enteros
z <- seq(0, 10, by=.25) # Del 0 al 10, incrementos en 0.25
w <- rep(c(1,2),2) # Dos vectores c(1,2) concatenados

# Operadores lógicos
x > y # TRUE si x es mayor que y
x >= y # TRUE si x es mayor o igual a y
x < y # TRUE si x es menor que y
x <= y # TRUE si x es menor o igual a y
x == y # TRUE si x es igual a y
x != y # TRUE si x diferente de y
is.na(x) # TRUE si x es NA o NaN
!is.na(x) # TRUE si x no es NA y no es NaN
x == 1 & y == 2 # Intersección
x == 1 | y == 2 # Unión


# Lectura de datos  --------------------------------------------------------------------------

# Importación e exportación de archivos .txt
read.table('name.txt', header = F, sep = "") # Importar
write.table(datos, file = '', sep = " ", row.names = F, col.names = T) # Exportar

# Importación e exportación de archivos .xlsx o .xls 
library(xlsx)
read_excel('path', sheet = NULL, col_names = T, ...) # Determina automáticamente si es xls o xlsx
read_xlsx('path.xlsx', sheet = NULL, col_names = T, ...)
read_xls('path.xls', sheet = NULL, col_names = T, ...)

write.xlsx(data, file = '', sheetName = "name") # Exportar

write.xlsx(data, file = '', sheetName = "name") # Exportar


# Importación e exportación de archivos .csv
read.csv('file.csv', header = T, sep = ",", encoding = 'UTF-8', ...) # Importar
write.csv(data, file = '' , row.names = F, col.names = T, fileEncoding = 'UTF-8') # Exportar

# Importación e exportación de archivos .dta
library(haven)
read_dta('file.dta', encoding = '', skip = '')
write_dta(data, path = '')

# readstata13 es mejor para nombres de variables mayores a 32 caracteres
library(readstata13)
readstata13(data, )
save.dta13(data, file = '') 

#  Importación e exportación de archivos .shp o .geojson
library(sf)
st_read(dsn, layer, ...) # Importar
st_write(obj, dsn, layer, ...) # Exportar


# Manejo de datos --------------------------------------------------------------------------------

# Generamos numeros aleatorios de una distribucion exponencial
datos_x <- cbind.data.frame(floor(matrix(rexp(25, rate=.1), ncol=5)), c(NA,NA,1,1,1), c(2,2,NA,NA,NA), c(0:4))
names(datos_x) <- c(paste0("X", c(1:7)), "id")

# Generamos numeros aleatorios de una distribucion normal
datos_y <- cbind.data.frame(matrix(rnorm(50), ncol=5), c(1:10))
names(datos_y) <- c(paste0("Y", c(1:5)), "id")

# Instalamos el paquete y lo cargamos 
install.packages("tidyverse")
library(dplyr)

# Comando para seleccionar columnas 
select(datos_x, -4)
# Comando para crear nueva columna 
mutate(datos_x, X8 = X1 - X2 + X3)
# Comando para eliminar renglones
slice(datos_x, -n()) # Elimina el ultimo renglon

# Comando para fusionar 2 columnas con valores NA 
mutate(datos_x, NUEVA = coalesce(X6,X7))

# Comando para filtrar por ciertos valores
filter(datos_x, X6 == 1)

# Comando para renombrar columnas 
rename(datos_x, EDAD = X2)

# Hacer todo junto
nueva_base_x <- datos_x %>% select(c(-4)) %>% mutate(X8 = X1 - X2 + X3) %>% slice(-n()) %>% 
  mutate(NUEVA = coalesce(X6,X7)) %>% filter(X6 == 1) %>% rename(EDAD = X2)

#  Agrupar por variables de interés
group_by(datos_x, nombre_Variable)

# Colapasar los datos a nivel nombre_Variable
summarise_all(datos_x, sum, na.rm = T) # sum es la función de la forma que queremos se colapsen los datos

# Agregar datos a nivel SECCION y ENTIDAD
Base_de_datos <- Base_de_datos %>% select(c("indices o nombres de las variables que te interesan")) %>% 
  group_by(ENTIDAD, SECCION) %>% summarise_all(sum, na.rm = TRUE)
  
# inner_join(): 
inner_join(datos_x, datos_y, by = "id")

# left_join(): 
left_join(datos_x, datos_y, by = "id") 

# right_join(): 
right_join(datos_x, datos_y, by = "id")

# full_join(): 
full_join(datos_x, datos_y, by = c("id" = "id")) # Si la variable del merge tiene distintos nombres


# Regresion lineal -----------------------------------------------------------------------------------

data(iris) # Cargamos la base de datos iris de R
names(iris) # Vemos el nombre de las variables en la base de datos


# Creamos un modelo de regresión lineal simple
reg_simple <- lm(Sepal.Length ~ Sepal.Width, data = iris) 

# Imprimimos los resultados
summary(reg_simple) 

# Regresión lineal múltiple
reg_multiple1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)
reg_multiple2 <- lm(Petal.Length ~ Sepal.Width + Petal.Width + Sepal.Length, data = iris)


# Para incluir efectos fijos utilizamos factor()
regresion_ef <- lm(Sepal.Length ~ Sepal.Width + factor(Species), data = iris)


# Paquete xtable y stargazer para generar tablas en latex --------------------------------------------
library(xtable) # Cargamos el paquete xtable

print(xtable(reg_simple, caption = 'Resultados de la regresion lineal simple'), caption.placement = 'top')


# stargazer ---
# Tabla sencilla para una regresión lineal simple 
stargazer(reg_simple) # Por default el resultado es código "latex"

# Calculamos el valor p de la prueba F con el paquete car
library(car)
pval_f1 <- linearHypothesis(reg_multiple1, c("Sepal.Width", "Petal.Width"), c(0,0), test = "F")[6]$`Pr(>F)`[2]
pval_f2 <- linearHypothesis(reg_multiple2, c("Sepal.Width", "Petal.Width", "Sepal.Length"), c(0,0), test = "F")[6]$`Pr(>F)`[2]


# Dos regresiones multivariadas en una sola tabla 
tabla2 <- stargazer(reg_multiple1, reg_multiple2, 
                    header = FALSE,
                    font.size = "scriptsize",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = c("Largo del Sépalo", "Largo del pétalo"),
                    covariate.labels = c("Ancho del Sépalo", "Ancho del pétalo",  "Largo del pétalo", "Constante"),
                    omit.stat = c("f", "ser","adj.rsq"),
                    add.lines = list(c("Prueba F: \\textbf{T} $=$ 0 (valor p)", round(pval_f1,4), round(pval_f2))),
                    title = "Ejemplo de múltiples regresiones multivariadas en una sola tabla",
                    type = "latex")

# Un problema con stargazer es la forma forma de agregar las notas, este que se presenta aquí es una forma fácil.
note.latex <- "\\multicolumn{3}{l} {\\parbox[t]{6cm}{ \\textit{Notas:} 
De esta manera podemos modificar las notas. * denota p$<$0.1, ** denota p$<$0.05, y *** denota p$<$0.01.}} \\\\"
tabla2[grepl("Note", tabla2)] <- note.latex

# Imprimimos el código de LaTeX guardado en el objeto tabla2
cat(tabla2)


# Diferencia en diferencias ---------------------------------------------------------------------------

# Generamos datos aleatoriamente
set.seed(32) # Semilla aleatoria

# Un vector de 10 enteros generados aleatoriamente entre 10 y 20
wage1 <- floor(runif(10, min=10, max=20)) 

# Un vector de 10 enteros generados eleatoriamente entre 11 y 21
wage_post_1 <- floor(runif(5, min=11, max=21)) 

# Un vector de 10 enteros generados eleatoriamente entre 20 y 30
wage2_post_2 <- floor(runif(5, min=20, max=30)) 
# Concatemaos el vector de salarios

wage <- c(wage1, wage_post_1, wage2_post_2) 

# Variable indicadora despues de tratamiento
post <- c(rep(0,10), rep(1,10)) 

# Indicadora de tratamiento
treatment <- c(rep(1,5), rep(0,5), rep(1,5), rep(0,5))
dd_data <- cbind.data.frame(wage, post, treatment) # Datos
colnames(dd_data) <- c("wage", "post", "Treatment")

# Corremos la regresión DD
did_reg <- lm(wage ~ post*Treatment + post + Treatment, data = dd_data, singular.ok = TRUE)

stargazer(did_reg, 
          header = FALSE,
          font.size = "scriptsize",
          dep.var.labels.include = FALSE,
          table.placement = "H",
          column.labels = "Salario",
          covariate.labels = c("Post", "Tratamiento (Grupo 2)", "Post x Tratamiento", "Constante") ,
          omit.stat = c("f", "ser","adj.rsq"),
          add.lines = list(c("Outcome range", "[10, 30]")),
          title = "Diferencias en diferencias en el salario de los trabajadores inscritos en el programa",
          type = "latex")

# Obtener coeficientes
did_reg$coefficients[1] # beta_0
did_reg$coefficients[1] + did_reg$coefficients[2] # beta_0 + beta_1
did_reg$coefficients[1] + did_reg$coefficients[3] # beta_0 + beta_2
sum(did_reg$coefficients) # beta_0 + beta_1 + beta_2 + delta


# Apendice --------------------------------------------------------------------
# Obtenemos las estimaciones de las medias e intervalos de confianza al 5%.
# Grupo 1 post = 0 
treat1_before <- lincom(did_reg, c("(Intercept)"))[1:3]
# Grupo 2, post = 0
treat2_before <- lincom(did_reg, c("(Intercept) + Treatment"))[1:3]
# Grupo 1 post = 1
treat1_after <- lincom(did_reg, c("(Intercept) + post"))[1:3]
# Grupo 2 post = 1
treat2_after <- lincom(did_reg, c("(Intercept) + Treatment + post + post:Treatment"))[1:3]

# Generamos un dataframe 
data_est <- rbind.data.frame(treat1_before, treat2_before, treat1_after, treat2_after)

data_est <- cbind.data.frame(data_est, c(rep("Antes del tratamiento",2), 
                                         rep("Después del tratamiento",2)), 
                             c("Grupo 1", "Grupo 2", "Grupo 1", "Grupo 2"))

colnames(data_est) <- c("Estimate", "left", "right", "period", "Treatment")

library(ggplot2) # libreria necesaria

final.text.11 <- element_text(color = "black", size = 11, hjust = 0.5, family="Arial")
text.11 <- element_text(color = "black", size = 10)

# Definimos el tema: el fondo, color, tamaños de letra, etc.
th_ca <-  theme(strip.text.x = final.text.11,
                strip.placement = "inside",
                strip.background = element_rect(colour = "black", fill = "white"),
                axis.text.x = element_blank(), 
                axis.text.y = text.11,
                axis.title.x=element_blank(),
                legend.text = final.text.11,
                legend.title = final.text.11,
                axis.text = final.text.11, 
                panel.border = element_rect(colour = "black", fill = NA),
                legend.position="bottom")

colors_ca <- c("#D55E00","#0072B2")
positions <- c("Grupo 1", "Grupo 2")

# Graficamos
dd_plot <- ggplot(data_est, aes(Treatment, Estimate), color = factor(Treatment)) + 
  geom_point(aes(color = factor(Treatment)), size = 5, fill = "#d3d3d3") +
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ period, switch="both", labeller = labeller(variables = label_wrap_gen(25))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th_ca + 
  scale_color_manual(name = "Tratamiento",  labels = c("Grupo 1", "Grupo 2"),
                     values= colors_ca) +
  ylab("Media") + 
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  geom_hline(yintercept=0, size = 0.8) + 
  ylim(10, 30)


# Guardamos imagen 
ggsave(dd_plot, filename = 'imagen_dd.pdf', device = cairo_pdf, dpi = 300, width = 12, height = 10, units = 'cm')


# Obtener coeficientes
t1_post <- data_est$Estimate[3]
t2_post <- data_est$Estimate[4]
t1_before <- data_est$Estimate[1]
t2_before <- data_est$Estimate[2]
counterfactual <- t2_post+(t1_before-t2_before)

colors <- c("#000000","#D55E00", "#0072B2" )

plot1 <- plot(1, xlab="Tiempo", ylab="Media", xaxt="n", xlim=c(-0.01, 1.01), ylim=c(10, 30))
segments(x0=0, y0=t2_before, x1=1, y1=t2_post, lty = 1, col = colors[1]) # grupo2
segments(x0=0, y0=t1_before, x1=1, y1=t1_post, lty = 2, col = colors[2]) # grupo1
segments(x0=0, y0=t1_before, x1=1, y1=counterfactual, lty = 3, col = colors[3]) # contrafactual 
legend("bottomright", legend = c("Grupo 2", "Grupo 1", "Contrafactual"), lty = c(1,2,3), col = colors)
axis(side =  1, at=c(0,1), labels=NULL)



# Two-way fixed-effect regression ----------------------------------------------------------------
# Simulamos los datos para el grupo de tratamiento
tratados <- cbind.data.frame(c(rep(c(1:10), each=10)), (rep(c(2010:2019), 10)), rep(1,100), c(rep(c(2013,2014,2015,2016,2017),each = 20)))
names(tratados) <- c("state", "year", "treat", "year_treated")

set.seed(13) # Semilla aleatoria para replicacion

tratados <- tratados %>% group_by(state) %>% dplyr::mutate(post_treatment = ifelse(row_number() >= which(year == year_treated), 1, 0))

# Generamos los datos del grupo de control
control <- cbind.data.frame(c(rep(c(11:20), each=10)), rep(c(2010:2019), 10))
names(control) <- c("state",  "year")

# Juntamos las 2 bases
dd <- bind_rows(tratados, control)

# Cambiamos los valores NA por 0 
dd[is.na(dd)] <- 0

# Creamos multiples leads and lags
library(plm)
dd <- pdata.frame(dd,index = c("state","year")) # creamos formato pdata de plm, por facilidad
dd <- cbind(dd, plm::lag(dd$post_treatment,-3:3)) # creamos lags and leads 
dd <- as.data.frame(transform(dd)) # reveritmos el formato de pdata al formato original
names(dd)[(length(dd) - 6):length(dd)] <- c("lead_3", "lead_2", "lead_1", "treat_period", "lag_1", "lag_2", "lag_3") # cambiamos los nombres de las variables

# revertimos las variables que se hicieron factor con pdata, para uso del DID plot
library(varhandle)
dd <- unfactor(dd)

# Reemplazamos valores NA del grupo de control por 0.
dd[100:200, 6:12][is.na(dd[100:200, 6:12])] <- 0


# Funcion auxiliar para crear la variable dependiente 
create_y <- function(x) {
  aux <- match(1, x)
  y <- c(pre_treat[1:(aux-1)], sample(c(1,2,3), prob = c(1/3,1/3,1/3)), post[ (1+aux):10])
  
  return(y)
}

# Generamos los datos sin diferential pre-trend de la variable dependiente
set.seed(5) # Semilla aleatoria
pre_treat <- sample(c(0, 1, 2, 3, 4, 5), size = 100, replace = TRUE, prob = c(0.05,0.05,0.1,0.2,0.3,0.3))
pre_control <- pre_treat + 1
post <- sample(c(0, 1, 2, 3, 4, 5), size = 100, replace = TRUE, prob = c(0.3,0.3,0.2,0.1,0.05,0.05))

dd <- dd %>% group_by(state)  %>%
  mutate(y = ifelse(state %in% c(1:10), create_y(post_treatment), pre_control)) # Posterior del tratamiento

# Two'wat fixed-effect model
# Forma 1 de obtener tau 
forma_1 <- lm(y ~ factor(state) + factor(year) + post_treatment, data = dd)

# Forma 2 de obtener tau 
forma_2 <- plm(y ~ post_treatment , data = dd, model="within",effect = "twoways")

# Forma 3 de obtener tau 
forma_3 <- lfe::felm(y ~ post_treatment | state + year, data = dd)



# Forma 1: utilizando lm 
reg1 <- lm(y ~ lead_3 + lead_2 + lead_1 + treat_period + lag_1 + lag_2 + lag_3 +
             factor(state) + factor(year), data = dd)

# Forma 2: utilizando el paquete plm
reg2 <- plm(y ~ lead_3 + lead_2 + lead_1 + treat_period + lag_1 + lag_2 + lag_3,
            data=dd, model="within", effect="twoways")

# Forma 3: utilizando el paquete lfe
reg3 <- felm(y ~ lead_3 + lead_2 + lead_1 + treat_period + lag_1 + lag_2 + lag_3 | state + year, data = dd)

# Clustered s.e para lm 
se_clustered <- coeftest(reg1, vcov = 'HC1', cluster = 'year')[,2]

# Clustered s.e. para plm 
coeftest(reg2, vcov = vcovHC(reg2, type = "HC1", cluster = "group"))

write_dta(dd, 'data_state_test.dta')

# Clustered s.e. para felm
reg3_cluster <- felm(y ~ lead_3 + lead_2 + lead_1 + treat_period + lag_1 + lag_2 + lag_3 | state + year | 0 | year, data = dd)

summary(reg3_cluster)

summary(reg2)

# Tabla comparativa
tabldedd <- stargazer(reg1, reg2,
                      header = FALSE,
                      font.size = "scriptsize",
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      omit = c("Constant", "year", "state"),
                      column.labels = c("lm", "plm"),
                      covariate.labels = c("Lead 3", "Lead 2", "Lead 1", "Periodo 0", "Lag 1", "Lag 2", "Lag 3"),
                      omit.stat = c("f", "ser","adj.rsq"),
                      title = "Leads and lags: comparacion de metodos",
                      type = "latex")


# Creamos datos para hacer la grafica de leads and lags
coeficientes <- coef(reg2)
se <- sqrt(diag((reg2$vcov)))
time <- 3:-3

# Concatenamos las variables
datos <- data.frame(coeficientes=coeficientes, ses = se, time = time, type = rep(1:3, c(3,1,3)))
datos$time <- factor(datos$time, levels = c(3,2,1,0,-1,-2,-3))

# Grafica de leads and lags
colors <- c("#000000", "#0072B2","#D55E00")
ggplot(data = datos, mapping = aes(y = coeficientes, x = time)) +
  geom_point(aes(colour = factor(type)), size = 2) + 
  geom_errorbar(aes(ymin=(coeficientes-1.833*ses), ymax=(coeficientes+1.833*ses), colour = factor(type)), width=0.2) +
  ylim(c(-2.5,2)) +
  geom_hline(yintercept = 0, linetype="solid", color ="grey", 2) +
  geom_vline(xintercept = 0,linetype="dashed", color ="grey", 2) +
  theme_bw() +
  ylab("Valor estimado (90% IC)") + 
  xlab("Periodo") + 
  scale_x_discrete(labels = 3:-3, breaks = 3:-3) + 
  scale_color_manual(name = "Periodo",  labels = c("Adelantadas","Intervención","Rezagadas"), values= colors)


# Utilizamos la funcion att_gt() para agregar los efectos
out <- att_gt(yname = "y", gname = "year_treated", idname = "state", tname = "year", 
              alp = 0.1, xformla = ~ 1, data = dd, est_method = "reg")

# Utilizamos la funcion aggte() para agregar los efectos entre periodos
es <- aggte(out, type = "dynamic")

# Utilizamos la funcion ggdid() del mismo paquete para graficar es
ggdid(es, title = " ") + 
  geom_hline(yintercept = 0, linetype="solid", color ="grey", 2) +
  geom_vline(xintercept = -0.5,linetype="dashed", color ="black", 2) +
  xlab("Periodo") +
  ylab("Efecto (90% IC)") + 
  geom_line(color="grey") 

# --------------------------------------------------
# Generar los datos sin diferential pre-trend
set.seed(6) # Semilla aleatoria

pre_treat <- sample(c(0, 1, 2, 3, 4, 5), size = 100, replace = TRUE, prob = c(0.05,0.05,0.1,0.2,0.3,0.3))
pre_control <- c(pre_treat[1:7], sample(c(0, 1, 2, 3, 4, 5), size = 3, replace = TRUE, prob = c(0.2,0.2,0.2,0.1,0.1,0.1)))
post <- sample(c(0, 1, 2, 3, 4, 5), size = 100, replace = TRUE, prob = c(0.3,0.3,0.2,0.1,0.05,0.05))

# Creamos la variable tasa de desempleo
dd <- dd %>% group_by(state)  %>%
  mutate(y = ifelse(state %in% c(1:10), 
                         c(pre_treat[1:(match(1,dd$post_treatment) + 1)], # Antes del tratamiento
                           1, # Valor al momento de inciial el tratamiento 
                           post[(match(1,dd$post_treatment) + 1):10]), pre_control)) # Posterior del tratamiento

# Utilizando el paquete plm y 2FE
reg3 <- plm(y ~ lead_3 + lead_2 + lead_1 + treat_period + lag_1 + lag_2 + lag_3,
            data=dd, model="within", effect="twoways")

summary(reg3)

# Tabla comparativa
tabldedd2 <- stargazer(reg3,
                       header = FALSE,
                       font.size = "scriptsize",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       omit = c("Constant", "year", "state", "lag_1", "lag_2", "lag_3"),
                       column.labels = c("Calificación"),
                       covariate.labels = c("Lead 3", "Lead 2", "Lead 1", "Periodo 0"),
                       omit.stat = c("f", "ser","adj.rsq"),
                       title = "Leads and lags: Differential pre-trends",
                       type = "latex")


note.latex <- "\\multicolumn{2}{l} {\\parbox[t]{6cm}{ \\textit{Notas:} 
Las variables rezagadas se incluyen como controles, pero se omiten. * denota p$<$0.1, ** denota p$<$0.05, y *** denota p$<$0.01.}} \\\\"
tabldedd2[grepl("Note", tabldedd2)] <- note.latex

cat(tabldedd2)


# Creamos datos para hacer la grafica de leads and lags
coeficientes <- coef(reg3)
se <- sqrt(diag((reg3$vcov)))
time <- 3:-3
# Concatenamos las variables
datos <- data.frame(coeficientes=coeficientes, ses = se, time = time, type = rep(1:3, c(3,1,3)))
datos$time <- factor(datos$time, levels = c(3,2,1,0,-1,-2,-3))

# Grafica de leads and lags
colors <- c("#000000", "#0072B2","#D55E00")
ggplot(data = datos, mapping = aes(y = coeficientes, x = time)) +
  geom_point(aes(colour = factor(type)), size = 2) + 
  geom_errorbar(aes(ymin=(coeficientes-1.833*ses), ymax=(coeficientes+1.833*ses), colour = factor(type)), width=0.2) +
  ylim(c(-1,2.5)) +
  geom_hline(yintercept = 0, linetype="solid", color ="grey", 2) +
  geom_vline(xintercept = 0,linetype="dashed", color ="grey", 2) +
  theme_bw() +
  ylab("Valor estimado (90% IC)") + 
  xlab("Periodo") + 
  scale_x_discrete(labels = 3:-3, breaks = 3:-3) + 
  scale_color_manual(name = "Periodo",  labels = c("Adelantadas","Intervención","Rezagadas"), values= colors)


# Utilizamos la funcion att_gt() para agregar los efectos
out <- att_gt(yname = "y", 
              gname = "year_treated", 
              idname = "state", 
              tname = "year",
              alp = 0.1,
              xformla = ~ 1, data = dd, est_method = "reg")

# Utilizamos la funcion aggte() para agregar los efectos entre periodos
es <- aggte(out, type = "dynamic")

# Utilizamos la funcion ggdid() del mismo paquete para graficar
ggdid(es, title = " ") + 
  geom_hline(yintercept = 0, linetype="solid", color ="grey", 2) +
  geom_vline(xintercept = -0.5,linetype="dashed", color ="black", 2) +
  xlab("Periodo") +
  ylab("Efecto (90% IC)") + 
  geom_line(color="grey")


# Variables intrumentales ----------------------------------------------------------------------------

# Creamos los datos aleatoriamente
set.seed(32)
salarios <- c(10, 10, 11, 13, 15, 18, 20, 20, 19, 21)
educacion <- c(1,1,1,2,1,2,3,3,3,3)
educacion_padres <- educacion - sample(c(0,1,2), size = 10, replace = T, prob = c(0.7,0.2,0.1))

# Paso a paso 
fs <- lm(educacion ~ educacion_padres) # first stage

# Test weak instrument
linearHypothesis(fs, c("educacion_padres = 0"), test = "F")[6]$`Pr(>F)`[2]

# Otra manera de obtener el mismo resultado es mediante First Stage y Reduced form
fs <- lm(educacion ~ educacion_padres) # first stage
rf <- lm(salarios ~ educacion_padres) # reduced form

# Sacamos el cociente de las estimaciones
b_iv <- rf$coefficients[2]/fs$coefficients[2]

# Hacemos lo mismo utilizando la función ivreg
library(AER)
reg_iv = ivreg(salarios ~ educacion| educacion_padres) 

test <- cbind(salarios, educacion, educacion_padres)

# obtenemos algunos test para ver si tenemos un buen instrumento
summary(reg_iv, df = Inf, diagnostics = TRUE)

tabla_iv <- stargazer(fs, reg_iv,
                      header=FALSE,
                      font.size="scriptsize",
                      dep.var.caption = "",
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      omit = "Constant",
                      column.labels = c("First Stage", "Modelo ivreg"),
                      covariate.labels = c("Educación estimada", "Educación", "Constante"),
                      omit.stat=c("f", "ser","adj.rsq"),
                      title = "Variables instrumentales",
                      type = "latex",
                      out = 'tabla_iv.tex')

cat(tabla_iv)

# Regresion discontinua -------------------------------------------------------------------------------

set.seed(4) # Semilla  aleatoria
# Generamos los datos
x <- runif(100, -2, 2)
y <- 1 +  x + 5*(x >= 0) + rnorm(100,0,1)
z <- ifelse(x > 0, 1, 0) # creamos la variable indicadora

# Concatenamos los vectores
rd_data <- cbind.data.frame(y,x,z)

# Test de McCrary
library(rdd)
DCdensity(rd_data$x , 0, plot = F)


# Forma 1: utilizando la función lm():
rd1 <- lm(y ~ z + z*x , data = rd_data) # el punto de quiebre es 0
rd1$coefficients


# Forma 2: utilizando el paquete rddtools

library(rddtools) # Cargamos el paquete
# Generamos los mismos datos con rdd_data()
rd_data <- rdd_data(x=rd_data$x, y=rd_data$y, z = ifelse(rd_data$x>0, 1,0), cutpoint=0)

# Sin optimal bandwidth
rd2 <- rdd_reg_lm(rdd_object = rd_data, slope = "separate", bw = NULL)
rd2$coefficients

# Graficamos los datos muestra
plot(rd_data, col = c("coral"), xlab = "x", ylab = "y")


# Graficamos el modelo RD ajustado
plot(rd2, col = "coral",  xlab = "x", ylab = "y")


# RD Con optimal bandwidth
rd3_bw <- rdd_reg_lm(rdd_object = rd_data, slope = "separate", bw = rdd_bw_ik(rd_data, kernel = "Triangular"))
rd3_bw$coefficients

# Grafica RD simple
ggplot(rd_data, aes(x, y, color = factor(z))) +
  geom_point() + 
  geom_smooth(size = 1.5) +
  geom_vline(xintercept=0, linetype="longdash") +
  theme_bw() +
  xlab("x") +
  ylab("y") + 
  scale_color_discrete(name = "T")

