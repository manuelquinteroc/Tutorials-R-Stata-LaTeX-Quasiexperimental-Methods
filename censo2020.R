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

censo <- censo %>% filter(LOC == 0 & MUN != 0) %>% select(-c("LOC", "ALTITUD")) %>% mutate(code_inegi = CVE_ENT*1000 + MUN) %>%  # creamos código INEGI 
  relocate(code_inegi) 


write_dta(censo, 'Ceneso2020_MUNICIPAL.dta')




