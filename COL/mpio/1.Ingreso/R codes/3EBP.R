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
library(rstanarm)

# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_mrp <- readRDS("COL/mpio/1.Ingreso/Data/encuesta_mrp.rds")
tasa_desocupados <- readRDS("COL/mpio/1.Ingreso/Data/tasa_desocupacion.rds")

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


encuesta_df_agg <- inner_join(encuesta_df_agg, statelevel_predictors_df, 
                              by = "mpio")

# summary(encuesta_df$ingreso2)
#--- Fit in stan_glmer ---#
options(mc.cores = parallel::detectCores())
fit <- stan_lmer(
  ingreso ~ (1 | mpio) +
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
  weights = n,
  iter = 400,
  verbose = FALSE
)



print(fit)
sum(predict(fit, type = "response")<0)
predict(fit, type = "response")[predict(fit, type = "response")<0]

sum(encuesta_df_agg$n,na.rm = TRUE)

sum(predict(fit, type = "response")*encuesta_df_agg$n)/1000000

inner_join(statelevel_predictors_df,encuesta_mrp) %>% ungroup() %>% 
  summarise(sum(ingreso)/1000000)


#--- Exporting Bayesian Multilevel Model Results ---#

saveRDS(fit, 
        file = "COL/mpio/1.Ingreso/Data/fit_EBP_logshift.rds")

fit_freq <- lmer(
  ingreso ~ (1 | mpio) +
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
  weights = n)

saveRDS(fit_freq, 
        file = "COL/mpio/1.Ingreso/Data/fit_freq_EBP_logshift.rds")




# Assessment of the model -------------------------------------------------

# Graphical posterior predictive checks -----------------------------------
## Regresando a la escala original los ingresos

# new_encuesta <- encuesta_mrp %>% inner_join(statelevel_predictors_df, by = "depto")
# y_sam<- new_encuesta$ingreso
# y_pred <- predict(fit, newdata = new_encuesta)

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
#     dif = (media_ingresolp-media_ingreso_pred)/media_ingresolp
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



