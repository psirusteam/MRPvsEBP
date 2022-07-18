#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Modelo Multinivel Bayesiano                           #
# Autor: Stalyn Guerrero  &  Andrés Gutiérrez           #
#########################################################

rm(list = ls())

# Loading required libraries ----------------------------------------------

library(tidyverse)
library(magrittr)
library(patchwork)
library(sp)
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(survey)
library(srvyr)

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
dat_df = readRDS("COL/mpio/1.Ingreso/Data/tablas_estimados.rds")


## Leer Shape del pais
ShapeSAE <-
  read_sf("COL/mpio/ShapeDeptoCOL/dv_Municipio.shp") %>%
  rename(mpio = COD_DANE) %>%
  mutate(mpio = str_pad(mpio, pad = "0", width = 5),
         nombre = NOM_MUNICI)

###############################################################################
# Colombia
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(dat_df$mpio,  by = "mpio"))

brks_lp <- c(0, 1, 1.5, 2, 3, 5)
tmap_options(check.and.fix = TRUE)
Mapa_lp <-
  P1_ingresolp + 
  tm_polygons(col = c("estimate_mrp_bayes","estimate_ebp_bayes"),
     breaks = brks_lp,
    title = paste0("Income ",c("MRP Bayes", "EBP Bayes")),
    palette = "-YlOrRd",
    colorNA = "white" 
  ) + tm_layout( 
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = TRUE )

tmap_save(
  Mapa_lp,
  "COL/mpio/1.Ingreso/Output/MunicipiosIncome.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
