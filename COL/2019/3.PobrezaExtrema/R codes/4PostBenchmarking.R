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
encuesta_mrp <- readRDS("COL/2019/3.PobrezaExtrema/Data/encuesta_mrp.rds")
censo_mrp <- readRDS("COL/2019/3.PobrezaExtrema/Data/censo_mrp.rds")
tasa_desocupados <- readRDS("COL/2019/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

fit <- readRDS("COL/2019/3.PobrezaExtrema/Data/fit_mrp_logit.rds")


# Poststratification at the National Level --------------------------------

statelevel_predictors_df <- tasa_desocupados


byAgrega <-
  grep(
    pattern =  "^(n|pobreza|ingreso)",
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

poststrat_df$epred_mat <- epred_mat

# Calculo de la pobreza Cepal.  -------------------------------------------
encuesta_mrp %<>% mutate(pobreza = ifelse(ingreso <= li, 1, 0))
poststrat_df %<>% mutate(pobreza = epred_mat)
# definiendo diseno muestral

diseno <- encuesta_mrp %>%
  as_survey_design(weights = fep)

###########################################
###########################################
###           Benchmarking              ###
###     (Gutiérrez - Guerrero, 2022)    ###
###########################################
###########################################
names_cov <-
  grep(
    pattern =  "^(n|pobreza|ingreso)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

names_cov <- names_cov[names_cov %in% names(encuesta_mrp)]

num_cat_censo <- apply(poststrat_df[names_cov], MARGIN = 2, function(x)
  length(unique(x)))

num_cat_sample <- apply(encuesta_mrp[names_cov], MARGIN = 2, function(x)
  length(unique(x)))

names_cov <- names_cov[num_cat_censo==num_cat_sample]
#names_cov <- c("area",   "sexo",   "edad",   "anoest", "depto")

poststrat_df %<>%
  fastDummies::dummy_cols(select_columns = names_cov,
                          remove_selected_columns = FALSE)


estimaciones <-
  map(names_cov ,~ poststrat_df %>% group_by_at(all_of(.x)) %>%
        summarise(
          medias = weighted.mean(epred_mat, n),
          Nhat = sum(n),
          t_pobreza = sum(epred_mat *
                            n)
        ))

poststrat_df %<>% 
  mutate_at(vars(matches("\\d$")) ,~.*poststrat_df$epred_mat)


### total
totales <- map(names_cov, ~encuesta_mrp %>% group_by_at(all_of(.x)) %>% 
                 summarise(Nhat = sum(fep),
                           t_pobreza = sum(pobreza*fep),
                           medias = weighted.mean(pobreza,fep)))

paso <- sapply(names_cov, function(byi){
  encuesta_mrp %>% group_by_at(all_of(byi)) %>% 
    summarise(Nhat = sum(fep),
              t_pobreza = sum(pobreza*fep),
              medias = weighted.mean(pobreza,fep))
})

unlist(paso["t_pobreza",])


poststrat_df$gk <- calib(Xs = poststrat_df %>% select(matches("\\d$")), 
                         d = poststrat_df$n,
                         total = unlist(paso["t_pobreza",]),
                         method="logit") 

checkcalibration(Xs = poststrat_df %>% select(matches("\\d$")), 
                 d = poststrat_df$n,
                 total = unlist(paso["t_pobreza",]),
                 g = poststrat_df$gk)

summary(poststrat_df$gk)

map(names_cov ,~ poststrat_df %>% group_by_at(all_of(.x)) %>%
      summarise(
        Nhat = sum(n),
        t_pobreza = sum(epred_mat *n*gk)
      ) %>% 
      mutate(medias = t_pobreza/Nhat))

poststrat_df %<>%
  mutate(pobreza2 = epred_mat *gk)  

temp <- map(names_cov ,~ poststrat_df %>% group_by_at(all_of(.x)) %>%
              summarise(
                Nhat = sum(n),
                t_pobreza = sum(n*pobreza2)
              ) %>% 
              mutate(medias = t_pobreza/Nhat)) 

totales[[1]]
temp[[1]]

totales[[2]]
temp[[2]]

totales[[3]]
temp[[3]]

totales[[4]]
temp[[4]]

totales[[5]]
temp[[5]]

jpeg(file = "COL/2019/3.PobrezaExtrema/Output/Plot_Bench_Pobreza.jpeg")
hist(poststrat_df$gk)
dev.off()

saveRDS(poststrat_df %>% select(!matches("_\\d{,2}$")), 
        "COL/2019/3.PobrezaExtrema/Data/poststrat_df.RDS")
saveRDS(encuesta_mrp, "COL/2019/3.PobrezaExtrema/Data/encuesta_mrp.RDS")

