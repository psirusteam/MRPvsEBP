#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())


# Loading required libraries ----------------------------------------------

library(rstan)
library(rstanarm)
library(data.table)
library(dplyr)
library(forcats)
library(tidyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(scales)
library(bayesplot)
library(gridExtra)
library(ggalt)
library(usmap)
library(gridExtra)
library(scales)
library(kableExtra)
library(formatR)
library(patchwork)

theme_set(bayesplot::theme_default())
library(tidyverse)


# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_mrp <- readRDS("COL/2019/2.Pobreza/Data/encuesta_mrp.rds")
tasa_desocupados <- readRDS("COL/2019/2.Pobreza/Data/tasa_desocupacion.rds")


# Bayesian Multilevel Modelling -------------------------------------------
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
            pobres = sum(ingreso<lp),
            nopobres = n - pobres, .groups = "drop") 


encuesta_df_agg <- inner_join(encuesta_df_agg, statelevel_predictors_df, by = "depto")


#--- Fit in stan_glmer ---#
fit <- stan_glmer(
  cbind(pobres, nopobres) ~  (1 | depto) +
    (1 | edad) +
    (1 | area) +
    (1 | anoest) +
    (1 | etnia) +
    (1 | depto:area) +
    (1 | depto:etnia) +
    (1 | depto:sexo) +
    (1 | depto:edad) +
    (1 | depto:anoest) +
    (1 | area:etnia) +
    (1 | area:sexo) +
    (1 | area:edad) +
    (1 | area:anoest) +
    (1 | etnia:sexo) +
    (1 | etnia:edad) +
    (1 | etnia:anoest) +
    (1 | sexo:edad) +
    (1 | sexo:anoest) +
    (1 | edad:anoest) +
    #    (1 | discapacidad) +
    sexo  + tasa_desocupacion + 
      F182013_stable_lights + 
  X2016_crops.coverfraction +
  X2016_urban.coverfraction ,
  family = binomial(link = "logit"),
                data = encuesta_df_agg,
                  verbose = TRUE,
                 cores = 7,
                 chains = 4,
                 iter = 200
  )



saveRDS(fit, file = "COL/2019/2.Pobreza/Data/fit_bayes.rds")
# Assessment of the model -------------------------------------------------
#shinystan::launch_shinystan(fit)
############################################################
## Comparando los medelos Freq y Bayes
############################################################
# fit_freq <- readRDS("COL/2019/2.Pobreza/Data/fit_mrp_logit.rds")
# fit_Bayes <- readRDS("COL/2019/2.Pobreza/Data/fit_bayes.rds")

# # Graphical posterior predictive checks -----------------------------------
# new_encuesta <- encuesta_mrp %>% inner_join(statelevel_predictors_df, by = "depto")
# y_sam<- as.numeric(new_encuesta$ingreso < new_encuesta$lp)
# y_pred <- predict(fit_freq, type = "response", newdata = new_encuesta)
# y_pred_B <- posterior_epred(fit_Bayes, newdata = new_encuesta)

# rowsrandom <- sample(nrow(y_pred_B), 100)
# y_pred2 <- y_pred_B[rowsrandom, ]

# color_scheme_set("brightblue")
# #
# dim(y_pred2)
# length(y_sam)
# #ppc_dens_overlay(y = y, y_pred)
# ppc_dens_overlay(y = as.numeric(y_sam), y_pred2)

# plot1 <- ggplot(data.frame(datos = c(y_sam, y_pred, colMeans(y_pred_B)),
#                   repe = gl(3, length(y_sam), 
#                             labels = c("muestra", "Freq", "Bayes"))), 
#        aes(x = datos, fill = repe, alpha = 0.1)) +
#   geom_density() 

# ggsave(filename = "COL/2019/2.Pobreza/Output/Compara_modelos.jpeg", plot1, scale = 3)

# #ppc_hist(y = y, y_pred2)
# ppc_ecdf_overlay(y_sam, y_pred2)
# # Prueba de hipótesis posterior
# prop_gzero <- function(x) mean(x < 0.5)
# prop_gzero(y_sam)
# ppc_stat(y_sam, y_pred2, stat = "prop_gzero")

# prop_gzero <- function(x) mean(x >= 0.5)
# prop_gzero(y_sam)
# ppc_stat(y_sam, y_pred2, stat = "prop_gzero")


# prop_glp <- function(x) mean(x)
# prop_glp(y_sam)
# ppc_stat(y_sam, y_pred_B, stat = "prop_glp")

