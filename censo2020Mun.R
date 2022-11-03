list.of.packages <- c("haven", "dplyr", "stringr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---------------------------------------------------------------------------------------------------
# Leamos datos
censo <- read.csv('conjunto_de_datos_iter_00CSV20.csv', encoding = 'UTF-8') %>% select(-c(6:8)) %>% 
  dplyr::rename(CVE_ENT = X.U.FEFF.ENTIDAD)

censo[censo == "*"] <- NA # Missing values

# Convertimos las variables a tipo númerico
censo[,c(5:length(censo))] = apply(censo[c(5:length(censo))], 2, function(x) as.numeric(as.character(x)))

# Auxiliar para cuantas localidades hay por municipio que no son NA para futuras variables
censo_aux_GRAPROES <- censo %>% tidyr::drop_na(GRAPROES) %>% group_by(CVE_ENT, NOM_ENT, MUN, NOM_MUN) %>% summarise(n = n())
censo_aux_PRO_OCUP_C <- censo %>% tidyr::drop_na(PRO_OCUP_C) %>% group_by(CVE_ENT, NOM_ENT, MUN, NOM_MUN) %>% summarise(n = n())
censo_aux_PROM_HNV <- censo %>% tidyr::drop_na(PROM_HNV) %>% group_by(CVE_ENT, NOM_ENT, MUN, NOM_MUN) %>% summarise(n = n())

# create a function to negate %in% operator
`%!in%` = Negate(`%in%`)

censo <- censo %>% filter(LOC %!in% c(0,9998,9999)) 

censo <- censo %>% 
  group_by(CVE_ENT, NOM_ENT, MUN, NOM_MUN) %>% # Agrupamos por municipio
  summarise_all(sum, na.rm = T) %>% select(-LOC) %>% # resumimos sumando los datos
  mutate(code_inegi = CVE_ENT*1000 + MUN) %>%  # creamos código INEGI
  relocate(code_inegi) # Ponemos code_Inegi al inicio de la base de datos


# Create variables or interest
censo$P_18YMAS_log <- log(censo$P_18YMAS) # (log) population aged above 18
censo$illiterate_share <- censo$P15YM_AN / censo$P_15YMAS # share of the population that is illiterate 15 and more
censo$PEA_share <- censo$PEA / censo$P_12YMAS # the working age share of the population
censo$internet_share <- censo$VPH_INTER / censo$TVIVPARHAB# share of households with internet at home
# aprox of the share of the population with electricity, water, and drainage in their home
censo$servicios_share <- (censo$VPH_C_SERV / censo$TVIVPARHAB)*censo$OCUPVIVPAR / censo$POBTOT

# Obtain average years of schooling 
censo$GRAPROES  <- censo$GRAPROES / censo_aux_GRAPROES $n
censo$PRO_OCUP_C  <- censo$PRO_OCUP_C / censo_aux_PRO_OCUP_C$n
censo$PROM_HNV  <- censo$PROM_HNV / censo_aux_PROM_HNV$n

# Creamos dataframe solo con las variables que queremos
vars <- c("code_inegi", "CVE_ENT", "NOM_ENT", "MUN", "P_18YMAS_log", "GRAPROES", "illiterate_share", 
          "PRO_OCUP_C", "PROM_HNV", "PEA_share", "internet_share", "servicios_share")

censo_selected <- censo %>% select(vars)

write_dta(censo_selected, 'Ceneso2020Mun_Variables.dta')




