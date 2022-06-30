#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())

# Loading required libraries ----------------------------------------------

library(patchwork)
library(trafo)
library(normtest)
library(nortest)
library(lme4)
library(tidyverse)
library(magrittr)
library(rstanarm)a

# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_mrp <- readRDS("COL/2019/1.Ingreso/Data/encuesta_mrp.rds")
tasa_desocupados <- readRDS("COL/2019/1.Ingreso/Data/tasa_desocupacion.rds")

############################
# Log-shift Transformation #
############################
summary(encuesta_mrp$ingreso)
encuesta_mrp$ingreso <- encuesta_mrp$ingreso + 1

# model = lm(ingreso ~ 1, data = encuesta_mrp)
# logs = logshiftopt(model)$lambdahat
# 
# encuesta_mrp$ingreso2 = log(encuesta_mrp$ingreso + logs)
# 
# summary(encuesta_mrp$ingreso2)
# hist(encuesta_mrp$ingreso2, main = "Transformación Log-shift ingreso",
#      freq = T,
#      xlab = "",
#      ylab = "frecuencia", )

# Bayesian Multilevel Modelling -------------------------------------------

#--- Expand state-level predictors to the individual level ---#

statelevel_predictors_df <- tasa_desocupados

byAgrega <-
  grep(
    pattern =  "^(n|pobreza|ingreso|lp|li|fep)",
    x = names(encuesta_mrp),
    invert = TRUE,
    value = TRUE
  )

encuesta_df_agg <-
  encuesta_mrp %>%
  group_by_at(all_of(byAgrega)) %>%
  summarise(n = n(),
            ingreso = mean(ingreso),
            .groups = "drop") 


encuesta_df_agg <- inner_join(encuesta_df_agg, statelevel_predictors_df, by = "depto")

# summary(encuesta_df$ingreso2)
#--- Fit in stan_glmer ---#

fit <- stan_lmer(
  ingreso ~ (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo  +
    tasa_desocupacion +
    F182013_stable_lights +
    X2016_crops.coverfraction +
    X2016_urban.coverfraction  ,
  data = encuesta_df_agg,
  weights = n
)




#--- Exporting Bayesian Multilevel Model Results ---#

saveRDS(fit, 
        file = "COL/2019/1.Ingreso/Data/fit_bayes_EBP_logshift.rds")

fit_freq <- lmer(
  ingreso ~ (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo  +
    tasa_desocupacion +
    F182013_stable_lights +
    X2016_crops.coverfraction +
    X2016_urban.coverfraction  ,
  data = encuesta_df_agg,
  weights = n
)




#--- Exporting Bayesian Multilevel Model Results ---#

saveRDS(fit_freq, 
        file = "COL/2019/1.Ingreso/Data/fit_freq_EBP_logshift.rds")

