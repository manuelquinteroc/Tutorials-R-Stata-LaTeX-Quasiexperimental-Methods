# Tutorial R 1, merge de bases de datos

# Instalamos los paquete necesarios
list.of.packages <- c("dplyr", "haven")

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


# leemos datos de secciones desde un csv y colapsamos la información a nivel municipal
secciones <- read.csv('Datos/secciones_inegi.csv') %>% group_by(code_INEGI, entidad, municipio) %>% summarise_all(sum, na.rm = T) %>% 
  select(-seccion)

# leemos datos de COVID desde un .dta
covid <- read_dta('Datos/covid_data.dta')

# Hacemos merge de los nombres de los municipios de secciones para la base de covid
covid$municipio <- secciones$municipio[match(covid$code_INEGI, secciones$code_INEGI)]


# si quisieramos hacer merge de diversas variables:

covid <- left_join(covid, secciones[c("code_INEGI", "municipio", "ln")], by = "code_INEGI")

# Eliminamos valores NA de la base covid
covid <- covid %>% filter(!is.na(municipio))


