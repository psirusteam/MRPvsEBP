#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list = ls())

# Loading required libraries ----------------------------------------------

library(scales)
library(tidyverse)
library(formatR)
library(patchwork)

source(file = "0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("COL/2019/2.Pobreza/Data/encuesta_mrp.rds")

# Las comparación de proporciones entre la muestra y censo se hacen 
# en la rutina de ingresos.  

# Interaction effects  ----------------------------------------------------

theme_set(theme_bw())
encuesta_mrp %<>% mutate(pobreza = ifelse(ingreso <= lp,1,0))
### AGE x SEX ###

#--- Percentage of people in poverty by AGE x SEX ---#
(p_sex_age <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "sexo",
                   by2 = "edad")
)
### Level of schooling (LoS) x SEX ###
(p_sex_escolar <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "sexo",
                   by2 = "anoest")
)
### State x SEX ###
(p_sex_depto <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "sexo",
                   by2 = "depto")
)

#--- Patchwork in action ---#
(p_sex_age + p_sex_escolar) / p_sex_depto

### Level of schooling (LoS) x AGE ###
(p_escolar_edad <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "anoest",
                   by2 = "edad") +
  theme(legend.position = "bottom") + labs(colour = "anoest")
)
### State x AGE ###
(p_depto_edad <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "edad",
                   by2 = "depto") +
  theme(legend.position = "bottom") + labs(colour = "Edad")
)

p_escolar_edad / p_depto_edad

### Level of schooling (LoS) x State ###
(p_depto_escolar <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "anoest",
                   by2 = "depto") +
  theme(legend.position = "bottom") + labs(colour = "anoest")
)

