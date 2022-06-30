#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())

cat("\f")
# Loading required libraries ----------------------------------------------

library(rstan)
library(rstanarm)
library(data.table)
library(dplyr)
library(magrittr)
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

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("COL/mpio/2.Pobreza/Data/encuesta_mrp.rds")
poststrat_df <- readRDS("COL/mpio/2.Pobreza/Data/poststrat_df.RDS") %>% 
  filter(anoest != "99") %>%  drop_na()


fit_bayes_mrp <- readRDS("COL/mpio/2.Pobreza/Data/fit_mrp_logit.rds")
fit_bayes_ebp <- readRDS("COL/mpio/2.Pobreza/Data/fit_ebp_logit.rds")

# Poststratification at the National Level --------------------------------

byAgrega <-
  grep(
    pattern =  "^(X|F|.groups|n|pobreza|ingreso|tasa_desocupacion|epred_mat|gk|mpio|lp)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )


# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.

epred_mat_mrp <- posterior_epred(fit_bayes_mrp, newdata = poststrat_df, type = "responde")
epred_mat_ebp <- posterior_epred(fit_bayes_ebp, newdata = poststrat_df, type = "responde")

## validacion de los valores posteriores
summary(rowMeans(epred_mat_mrp))
summary(rowMeans(epred_mat_ebp))

length(epred_mat_mrp[which(epred_mat_mrp == 0)])
length(epred_mat_ebp[which(epred_mat_ebp == 0)])

# Resultados nacionales ---------------------------------------------------
(mrp_estimate <-
  Aux_Agregado(poststrat = poststrat_df,
             epredmat = epred_mat_mrp,
             byMap = NULL)
)

(ebp_estimate <-
    Aux_Agregado(poststrat = poststrat_df,
                 epredmat = epred_mat_ebp,
                 byMap = NULL)
)
# Poststratification at the State level -----------------------------------

byAgrega <-
  grep(
    pattern = "mpio",
    x = byAgrega,
    value = TRUE,
    invert = TRUE
  )

byAgrega <- t(combn(byAgrega, 2))
byAgrega <- rbind(c("mpio","mpio" ), byAgrega)
# resultados para ingreso medio

freq_estimate = map(1:nrow(byAgrega),
             ~poststrat_df %>% group_by_at(vars("mpio", byAgrega[.x,])) %>%
               summarise(
                 estimate_mrp_freq = sum(n * pobreza_mrp) / sum(n),
                 estimate_ebp_freq = sum(n * pobreza_ebp, na.rm = TRUE) / sum(n), 
                 .groups = "drop") %>% 
               ungroup())


mrp_ECM <- map(1:nrow(byAgrega), function(i) {
     Aux_Agregado(poststrat_df,
                 epredmat = epred_mat_mrp,
                 byMap = c("mpio", byAgrega[i, ])) %>% 
    rename( estimate_mrp_bayes = mrp_estimate, 
            estimate_mrp_bayes_se = mrp_estimate_se)

  })

ebp_ECM <- map(1:nrow(byAgrega), function(i) {
  Aux_Agregado(poststrat_df,
               epredmat = epred_mat_ebp,
               byMap = c("mpio", byAgrega[i, ])) %>% 
    rename( estimate_ebp_bayes = mrp_estimate, 
            estimate_ebp_bayes_se = mrp_estimate_se)
  
})



tablas <- map2(freq_estimate, ebp_ECM, inner_join) %>% 
            map2(.x = ., .y = mrp_ECM, inner_join)
  
nom_tabs <- c("mpio", apply(byAgrega[-1,],MARGIN = 1,paste0, collapse = "_"))
names(tablas) <- nom_tabs


openxlsx::write.xlsx(tablas, 
                     file = "COL/mpio/2.Pobreza/Output/tablas_estimados.xlsx", 
                     overwrite = TRUE)

saveRDS(tablas, 
        "COL/mpio/2.Pobreza/Data/tablas_estimados.rds")

