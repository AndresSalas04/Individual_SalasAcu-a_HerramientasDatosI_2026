# Cargar librería
library(readxl)
library(dplyr)

# Ubicación del archivo Excel
archivo <- "../Data/Listas_Espera.xlsx"

# Importar las hojas
regionce <- read_excel(archivo, sheet = "RegionCE")
regionq <- read_excel(archivo, sheet = "RegionQ")
mensualce <- read_excel(archivo, sheet = "MensualCE")
mensualq <- read_excel(archivo, sheet = "MensualQ")
especialidades <- read_excel(archivo, sheet = "Especialidades ")

region_ce <- regionce %>%  
  dplyr::select(-1)

region_q <- regionq %>% 
  dplyr::select(-1)

mensual_ce <- mensualce %>%  
  dplyr::select(-1)

mensual_q <- mensualq %>% 
  dplyr::select(-1)



