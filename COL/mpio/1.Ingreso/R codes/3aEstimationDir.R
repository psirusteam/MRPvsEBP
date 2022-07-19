#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Modelo Multinivel Bayesiano                           #
# Autor: Stalyn Guerrero  &  Andrés Gutiérrez           #
#########################################################
rm(list = ls())
# Loading required libraries ----------------------------------------------
library(tidyverse)
library(magrittr)
library(patchwork)
library(sp)
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(survey)
library(srvyr)
library(furrr)
source("0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
encuesta_mrp = readRDS("COL/mpio/2.Pobreza/Data/encuesta_mrp.rds")

byAgrega <- c("area",  "sexo","etnia", "anoest","edad")
byAgrega <- t(combn(byAgrega, 2))
byAgrega <- rbind(c("mpio","mpio" ), byAgrega)

diseno <- encuesta_mrp  %>% 
  as_survey_design(weights = fep) %>%
  mutate(pobrezalp = ingreso/lp)

f_temp <- function(ii){
  encuesta_mrp  %>% 
    group_by_at(vars(c("mpio", byAgrega[ii,]))) %>% 
    summarise(nd = n())
}

plan(multisession, workers = 2)

Estimacion_dir <- furrr::future_imap(.x = 1:nrow(byAgrega),~f_temp(.x) )

nom_tabs <- c("mpio", apply(byAgrega[-1,],MARGIN = 1,paste0, collapse = "_"))
names(Estimacion_dir) <- nom_tabs


openxlsx::write.xlsx(Estimacion_dir,
                     file = "COL/mpio/1.Ingreso/Data/sampilng_mpio.xlsx")
    