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
theme_set(bayesplot::theme_default())
library(tidyverse)

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
encuesta_mrp <- readRDS("COL/2019/2.Pobreza/Data/encuesta_mrp.rds")
censo_mrp <- readRDS("COL/2019/2.Pobreza/Data/censo_mrp.rds")
tasa_desocupados <- readRDS("COL/2019/2.Pobreza/Data/tasa_desocupacion.rds")

fit <- readRDS("COL/2019/2.Pobreza/Data/fit_freq_mrp_logit.rds")


# Poststratification at the National Level --------------------------------

statelevel_predictors_df <- tasa_desocupados


byAgrega <-
  grep(
    pattern =  "^(n|pobreza|ingreso|.groups)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

poststrat_df <- censo_mrp %>%  group_by_at(byAgrega) %>%
  summarise(n = sum(n), .groups = "drop")

# Expand state level predictors to the individual level

poststrat_df <- left_join(poststrat_df, statelevel_predictors_df,
                          by = "depto")

# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.


epred_mat <- predict(fit, newdata = poststrat_df, 
                     type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat))
sum(epred_mat < 0)

poststrat_df$pobreza_mrp <- epred_mat

saveRDS(poststrat_df %>% select(!matches("_\\d{,2}$")), 
        "COL/2019/2.Pobreza/Data/poststrat_df.RDS")