#########################################################
# Proyecto IPM                                          #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())
gc()

#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)
library(sae)
library(lme4)
library(data.table)
library(haven)
library(magrittr)
library(formula.tools)
library(remotes)
#library(StatisticalModels)
library(fastDummies)
library(haven)
library(magrittr)
library(stringr)
library(openxlsx)
select <- dplyr::select

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(250000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################
## Leer encuesta
encuesta_temp <- read_dta("V:/U/DATA/BADEHOG_N-INTERNO/BASES_ESTANDARIZADAS-Usuarios/COL_2018N1.dta")
# encuesta_temp <- read_dta("V:/BC/COL_2018N1.dta")
encuesta <- read_dta("V:/U/DATA/BADEHOG_N-INTERNO/BASES_GRANDES-Usuarios/col18n1/Data/col18n1.dta")
# encuesta <- read_dta("Z:/BG/col18n1/col18n1.dta")
encuesta %<>% mutate(orden_temp = str_pad(
  string = 1:n(),
  width = 7,
  pad = "0"
))

upms <- readRDS(file = "COL/mpio/1.Ingreso/Data/upm_dpto_2018.rds")

encuesta %<>% left_join(upms,
                        by = c("directorio" = "DIRECTORIO",
                               "secuencia_p" = "SECUENCIA_P",
                               "orden"))

encuesta$mpio <- substr(encuesta$segmento,8,12)

encuesta %<>% arrange(orden_temp)
encuesta$condactr_ee <- encuesta_temp$condactr_ee
encuesta$etnia_ee <- encuesta_temp$etnia_ee
encuesta$fep <- encuesta_temp[["_fep"]]
table(encuesta$condact3, useNA = "a")
saveRDS(encuesta, file = "COL/mpio/1.Ingreso/Data/encuesta2018.rds")




