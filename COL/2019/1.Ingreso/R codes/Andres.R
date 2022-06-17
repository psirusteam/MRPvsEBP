#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())
cat("\f")
# Loading required libraries ----------------------------------------------

library(scales)
library(patchwork)
library(srvyr)
library(survey)
library(haven)
library(sampling)
library(tidyverse)

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
encuesta_mrp <- readRDS("COL/1.Ingreso/Data/encuesta_mrp.rds")
censo_mrp <- readRDS("COL/1.Ingreso/Data/censo_mrp.rds")

##### problemas en los fep
sum(encuesta_mrp$fep)
sum(censo_mrp$n)

tab <- full_join(
  encuesta_mrp %>% group_by(depto) %>% summarise(Nhat = sum(fep)),
  censo_mrp %>% group_by(depto) %>% summarise(N = sum(n)),
  by = "depto"
) %>%
  data.frame() %>%
  mutate(diff = Nhat - N,
          sum = sum(Nhat - N,na.rm = TRUE)) 
  
  tab %>% 
  mutate_at(vars(depto), funs(as.character(.))) %>%
  bind_rows(mutate(., depto = "Total")) %>%
  group_by(depto) %>%
  summarise(Nhat= sum(Nhat, na.rm = TRUE),
            N= sum(N, na.rm = TRUE),
            diff = sum(diff, na.rm = TRUE)) %>% data.frame()


#1339056
censo_mrp %>% group_by(depto) %>% summarise(N = sum(n)) %>% 
  filter(as.numeric(depto)>= 81) %>% summarise(N = sum(N))
