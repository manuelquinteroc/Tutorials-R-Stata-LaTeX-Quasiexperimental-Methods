# DID Analysis

# Instalamos los paquete necesarios
list.of.packages <- c("stargazer", "ggplot2", "dplyr", "plm", "haven", "fixest", "varhandle", "lfe")

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

# -----------------------------------------------------------------------------
# Leemos datos
datos <- read_dta('DID_data.dta')

# Regresion DID 
# La función feglm del paquete fixest tiene los mismos cluster S.E. que en STATA.
reg_did <- feols(outcome ~ tratamiento | unidades + tiempo, cluster = "unidades", datos)

# Analysis de Leads and Lags ------------------------------------------------------------
# Creamos indicadora de grupo de tratamiento y control (en caso de que exista un grupo de control). 
datos <- datos %>% group_by(unidades) %>% mutate(treat = ifelse(any(tratamiento != 0) , 1,0))

# Creamos múltiples leads and lags
dd <- pdata.frame(datos,index = c("unidades","tiempo")) # creamos formato pdata de plm, por facilidad
dd <- cbind(dd, plm::lag(dd$tratamiento, c(-3:3)[-4])) # creamos lags and leads 
dd <- as.data.frame(transform(dd)) # reveritmos el formato de pdata al formato original
names(dd)[(length(dd) - 5):length(dd)] <- c("tratamiento_Lead_3", "tratamiento_Lead_2", 
                                            "tratamiento_Lead_1", "tratamiento_Lag_1", 
                                            "tratamiento_Lag_2", "tratamiento_Lag_3") # cambiamos los nombres de las variables

# Revertimos los factores creados por plm
dd <- unfactor(dd)

# Cambiamos los valores creados del del grupo de control a 0
dd[dd$treat == 0, c("tratamiento_Lead_3", "tratamiento_Lead_2", 
                    "tratamiento_Lead_1", "tratamiento_Lag_1", 
                    "tratamiento_Lag_2", "tratamiento_Lag_3")] <- 0

# Regresión Leads and Lags
reg1 <- felm(outcome ~ tratamiento_Lead_3 + tratamiento_Lead_2 + tratamiento_Lead_1 + tratamiento + 
        tratamiento_Lag_1 + tratamiento_Lag_2 + tratamiento_Lag_3 | unidades + tiempo | 0 | unidades, dd)

# Regresión auxiliar para tener mismos s.e. as in STATA
reg1_aux <- feols(outcome ~ tratamiento_Lead_3 + tratamiento_Lead_2 + tratamiento_Lead_1 + tratamiento + 
               tratamiento_Lag_1 + tratamiento_Lag_2 + tratamiento_Lag_3 | unidades + tiempo, cluster = ~ unidades, dd)

table_leads_lags <- stargazer(reg1,
                              header = FALSE,
                              font.size = "tiny",
                              dep.var.caption = "",
                              se = list(se(reg1_aux)),
                              label = "tab:tablaR",
                              dep.var.labels.include = FALSE,
                              float.env = "sidewaystable",
                              table.placement = "H",
                              omit = c("Constant", "year", "unidades", "Lag"),
                              column.labels = "Outcome",
                              covariate.labels = c("Tratamiento Lead 3", "Tratamiento Lead 2", "Tratamiento Lead 1", 
                                                   "Tratamiento"),
                              omit.stat = c("f", "ser","adj.rsq"),
                              add.lines = list(c("Outcome mean", round(mean(dd$outcome),3)),
                                               c("Outcome std. Dev.", round(sd(dd$outcome),3)),
                                               c("Outcome min", round(min(dd$outcome),3)),
                                               c("Outcome max", round(max(dd$outcome),3)),
                                               c("Cluster", "unidades")),
                              title = "Leads and lags",
                              type = "latex")

note.latex <- "\\multicolumn{2}{l} {\\parbox[t]{6cm}{ \\textit{Notas:} 
Lag variables are included but not shown. * denota p$<$0.1, ** denota p$<$0.05, y *** denota p$<$0.01.}} \\\\"
table_leads_lags[grepl("Note", table_leads_lags)] <- note.latex

cat(table_leads_lags, file = 'table_leadsLags_R.tex')


# Gráfica de Leads and lags
# Creamos datos para hacer la grafica de leads and lags
coeficientes <- coef(reg1)
se <- reg1_aux$se # si fuese plm usariamos: se <- sqrt(diag((reg2$vcov)))
time <- 3:-3

# Concatenamos las variables
datos <- data.frame(coeficientes=coeficientes, ses = se, time = time, type = rep(1:3, c(3,1,3)))
datos$time <- factor(datos$time, levels = c(3,2,1,0,-1,-2,-3))

# Grafica de leads and lags
colors <- c("#000000", "#0072B2","#D55E00")
leads_lags <- ggplot(data = datos, mapping = aes(y = coeficientes, x = time)) +
  geom_point(aes(colour = factor(type)), size = 2) + 
  geom_errorbar(aes(ymin=(coeficientes-1.96*ses), ymax=(coeficientes+1.96*ses), colour = factor(type)), width=0.2) +
  ylim(c(-4,4)) +
  geom_hline(yintercept = 0, linetype="solid", color ="grey", 2) +
  geom_vline(xintercept = 3.5,linetype="dashed", color ="red", 2) +
  theme_bw() +
  ylab("Valor estimado (95% IC)") + 
  xlab("Periodo") + 
  scale_x_discrete(labels = c("t+3", "t+2", "t+1", "tratamiento", "t-1", "t-2", "t-3"), breaks = 3:-3) + 
  scale_color_manual(name = "Periodo", values= colors) +
  theme(legend.position = "none") 

# GUardamos figura en formato pdf
ggsave(filename = 'leads_lags_R.pdf', device = cairo_pdf, dpi = 300, width = 12, height = 10, units = 'cm')


# DID plot (event study) ----------------------------------------------------------
# Variable post
dd <- dd %>% group_by(unidades) %>% mutate(post = ifelse(tratamiento > 0 , 1, 0))

# Variable event_time
dd <- dd %>% dplyr::group_by(unidades) %>% dplyr::mutate(event_time = tiempo[post > 0][1])

# Grupo de control a 0
dd$event_time[dd$treat == 0] <- 0

# Creamos la variable tte
dd <- dd %>% group_by(unidades) %>% mutate(time_to_event =  ifelse(treat == 1, tiempo - event_time, 0))

# Grupo de control a 0
dd$event_time[dd$time_to_event == 0] <- 0

# Creamos stte
dd$shifted_tte <- dd$time_to_event - min(dd$time_to_event)

# Valor de categoría base
reference <- mean(dd$shifted_tte[dd$time_to_event == -1])

# Corremos la regresión del event study
dd_plot_reg <- feols(outcome ~ i(shifted_tte, treat, ref = reference) | unidades + tiempo, cluster = "unidades", data = dd)

# Otra forma es con felm y utilizar el plot personalizado de más abajo.
felm(outcome ~ i(shifted_tte, treat, ref = reference) | unidades + tiempo | 0 | unidades, data = dd)

# Plot rápido usando fixest (feols)
iplot(dd_plot_reg, 
      xlab = 'Time to treatment',
      main = '') 


# Plot personalizado (para feols y felm, para plm habría que calcular los se como arriba en el leads and lags)
coefs = c(dd_plot_reg$coefficients[1:reference], 
          0, 
          dd_plot_reg$coefficients[ (reference + 1): length(dd_plot_reg$coefficients)])

se = c(dd_plot_reg$se[1:reference], 
       0, 
       dd_plot_reg$se[(reference + 1):length(dd_plot_reg$coefficients)] ) 
  
# Concatenamos las variables
datos_did <- data.frame(coeficientes = coefs, ses = se, time = -reference:7, type = rep(1:3, c(reference,1,7)))
datos_did$time <- factor(datos_did$time)

# Grafica DID
colors <- c("#000000", "#0072B2","#D55E00")
DID_plot <- ggplot(data = datos_did, mapping = aes(y = coeficientes, x = time)) +
  geom_point(aes(colour = factor(type)), size = 2) + 
  geom_errorbar(aes(ymin=(coeficientes-1.96*ses), ymax=(coeficientes+1.96*ses), colour = factor(type)), width=0.2) +
  geom_hline(yintercept = 0, linetype="solid", color ="grey", 2) +
  geom_vline(xintercept = reference + 1,linetype="dashed", color ="red", 2) +
  theme_bw() +
  ylab("Valor estimado (95% IC)") + 
  xlab("Años desde tratamiento") + 
  scale_color_manual(name = "Periodo", values= colors) +
  theme(legend.position = "none") 
  
# Guardamos la gráfica como pdf
ggsave(filename = 'DID_plot_R.pdf', device = cairo_pdf, dpi = 300, width = 12, height = 10, units = 'cm')


