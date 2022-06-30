#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())

# Loading required libraries ----------------------------------------------

library(lme4)
library(tidyverse)
library(formatR)
library(patchwork)
library(magrittr)
library(rstanarm)
theme_set(bayesplot::theme_default())


# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("COL/mpio/2.Pobreza/Data/encuesta_mrp.rds") %>% 
  mutate(pobreza = ifelse(ingreso <= lp, 1, 0))
tasa_desocupados <- readRDS("COL/mpio/2.Pobreza/Data/tasa_desocupacion.rds")


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
            pobres = sum(pobreza),
            nopobres = n - pobres, .groups = "drop") 

encuesta_df_agg %<>% inner_join(statelevel_predictors_df, 
                         by = "mpio") 


#--- Fit ---#
fit <- stan_glmer(
  cbind(pobres, nopobres) ~  (1 | mpio) +
    edad +
    area +
    anoest +
    etnia +
    sexo  +
    tasa_desocupacion +
    F182013_stable_lights +
    X2016_crops.coverfraction +
    X2016_urban.coverfraction  ,
  family = binomial(link = "logit"),
  data = encuesta_df_agg,
  verbose = TRUE,
  cores = 7
)


sum(predict(fit, type = "response") * encuesta_df_agg$n)
sum(encuesta_df_agg$pobres)

print(fit)
#--- Exporting Bayesian Multilevel Model Results ---#

saveRDS(fit, file = "COL/mpio/2.Pobreza/Data/fit_ebp_logit.rds")

fit_freq2 <- glmer(
  cbind(pobres, nopobres) ~  (1 | mpio) +
    edad +
    area +
    anoest +
    etnia +
    sexo  +
    tasa_desocupacion +
    F182013_stable_lights +
    X2016_crops.coverfraction +
    X2016_urban.coverfraction  ,
  family = binomial(link = "logit"),
  data = encuesta_df_agg
)
saveRDS(fit_freq2, file = "COL/mpio/2.Pobreza/Data/fit_freq_ebp_logit.rds")

#--- Exporting Bayesian Multilevel Model Results ---#

fit <- readRDS(file = "COL/mpio/2.Pobreza/Data/fit_mrp_logit.rds")



# Assessment of the model -------------------------------------------------

# Graphical posterior predictive checks -----------------------------------
## Regresando a la escala original los ingresos

# new_encuesta <- encuesta_mrp %>% inner_join(statelevel_predictors_df, by = "depto")
# y_sam<- new_encuesta$pobreza
# y_pred <- predict(fit, newdata = new_encuesta, type = "response")

# new_encuesta %<>% mutate(y_pred = y_pred,
#                          ingresolp = y_sam)

# names_cov <-
#   grep(
#     pattern =  "^(n|pobreza|ingreso|li|lp|fep|y_pred)",
#     x = names(new_encuesta),
#     invert = TRUE,
#     value = TRUE
#   )


# new_encuesta %>% 
#   summarise(
#     media_ingresolp = mean(ingresolp),
#     media_ingreso_pred = mean(y_pred),
#     dif = (media_ingresolp-media_ingreso_pred)
#   )

# ### total
# map(names_cov, ~new_encuesta %>% group_by_at(all_of(.x)) %>% 
#       summarise(
#         media_ingresolp = mean(ingresolp),
#         media_ingreso_pred = mean(y_pred),
#         dif = (media_ingresolp-media_ingreso_pred)/media_ingresolp
#       ))

# ggplot(data.frame(datos = c(y_sam, y_pred),
#                   repe = gl(2, length(y_sam), 
#                             labels = c("muestra", "predicción"))), 
#        aes(x = datos, fill = repe, alpha = 0.1)) +
#   geom_density() 



