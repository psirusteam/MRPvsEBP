#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())

# Librerías

library(tidyverse)
library(patchwork)
library(survey)
library(srvyr)

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
encuesta_mrp <- readRDS("COL/2019/3.PobrezaExtrema/Data/encuesta_mrp.rds")
poststrat_df <- readRDS("COL/2019/3.PobrezaExtrema/Data/poststrat_df.RDS")

# Revisión de NAs ---------------------------------------------------------

View(poststrat_df)
sum(complete.cases(poststrat_df)) == nrow(poststrat_df)

# Calculo de la pobreza Cepal.  -------------------------------------------

poststrat_df %<>% mutate(yk_lmer = pobreza, 
                        yk_bench = pobreza2)

diseno <- encuesta_mrp %>% mutate(yk_dir = pobreza) %>% 
  as_survey_design(weights = fep)

## validación nacional.
diseno %>% summarise(Nacional_dir = survey_mean(yk_dir))
poststrat_df %>% summarise(
  Nacional_lmer = sum(n * yk_lmer) / sum(n/gk),
  Nacional_bench = sum(n * yk_bench) / sum(n),
)


###########################################
###########################################
### Validaciones por subgrupo completo  ###
###########################################
###########################################
bynames <-
  grep(
    pattern =  "^(X|F|.gruops|n|li|pobreza|ingreso|tasa_desocupacion|epred_mat|gk|yk)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )

plot_uni <- map(
  .x = setNames(bynames, bynames),
  ~ plot_compare2_depto(
    sample_diseno = diseno,
    poststrat = poststrat_df,
    by1 = .x
  )
)

plot_uni <- plot_uni$depto$Plot$plot1  / 
  ( plot_uni$sexo$Plot$plot1 + plot_uni$anoest$Plot$plot1 +
      plot_uni$edad$Plot$plot1+ plot_uni$etnia$Plot$plot1 +
      plot_uni$area$Plot$plot1+ plot_uni$discapacitado$Plot$plot1)

ggsave(plot = plot_uni, 
       filename = "COL/2019/3.PobrezaExtrema/Output/plo2t_uni.jpeg",scale = 3)
###########################################
###########################################
### Validaciones por pares de subgrupos ###
###########################################
###########################################

# 1. Dpto-sexo -----------------------------------------------------------------

# bynames2 <- t(combn(bynames, 2))

# map(
#   1:nrow(bynames2),
#   ~ plot_compare2_depto(
#     sample_diseno = diseno,
#     poststrat = poststrat_df,
#     by1 = bynames2[.x, ]
#   )$Plot$plot1 %>%
#     ggsave(
#       paste0(
#         "COL/2019/3.PobrezaExtrema/Output/plot_",
#         paste0(bynames2[.x, ], collapse = "_"),
#         ".jpeg"
#       ),
#       plot = .,
#       scale = 3
#     )
# )



# ###########################################
# ###########################################
# ### Validaciones por triplas            ###
# ###########################################
# ###########################################
# bynames3 <- t(combn(bynames, 3))
# # 1. Dpto -----------------------------------------------------------------
# map(
#   1:nrow(bynames3),
#   ~ plot_compare2_depto(
#     sample_diseno = diseno,
#     poststrat = poststrat_df,
#     by1 = bynames3[.x, ]
#   )$Plot$plot1 %>%
#     ggsave(
#       paste0(
#         "COL/2019/3.PobrezaExtrema/Output/plot_",
#         paste0(bynames3[.x, ], collapse = "_"),
#         ".jpeg"
#       ),
#       plot = .,
#       scale = 3
#     )
# )

