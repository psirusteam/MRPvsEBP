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


source("0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
dat_df = readRDS("COL/mpio/2.Pobreza/Data/tablas_estimados.rds")



## Leer Shape del pais
ShapeSAE <-
  read_sf("COL/mpio/ShapeDeptoCOL/dv_Municipio.shp") %>%
  rename(mpio = COD_DANE) 

###############################################################################

# Colombia

ShapeSAE %>% data.frame() %>% select(mpio) %>% 
  left_join(dat_df$mpio)

P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(dat_df$mpio,  by = "mpio"))

brks_lp <- c(0, 0.2, 0.4, 0.6,0.8, 1)
tmap_options(check.and.fix = TRUE)
Mapa_lp <-
  P1_ingresolp + 
  tm_polygons(col = c("estimate_mrp_bayes","estimate_ebp_bayes"),
              breaks = brks_lp,
              title = paste0("Pobreza ",c("MRP Bayes", "EBP Bayes")),
              palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_lp,
  "COL/mpio/2.Pobreza/Output/Estados.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
