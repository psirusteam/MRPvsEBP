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
encuesta_mrp = readRDS("COL/mpio/2.Pobreza/Data/encuesta_mrp.rds")

diseno <- encuesta_mrp  %>% 
  as_survey_design(weights = fep)

estimado_dir <- diseno %>% mutate(pobrezalp = ingreso/lp) %>% group_by(mpio) %>% 
  summarise(estimacion_dir = survey_mean(pobrezalp))

## Leer Shape del pais
ShapeSAE <-
  read_sf("COL/mpio/ShapeDeptoCOL/dv_Municipio.shp") %>%
  rename(mpio = COD_DANE) %>%
  mutate(mpio = str_pad(mpio, pad = "0", width = 5),
         nombre = NOM_MUNICI)

###############################################################################

# Colombia

P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(estimado_dir,  by = "mpio"))

brks_lp <- c(0, 1, 1.5, 2, 3, 5)
tmap_options(check.and.fix = TRUE)
Mapa_lp <-
  P1_ingresolp + 
  tm_polygons(col = "estimacion_dir",
     breaks = brks_lp,
    title = "Income",
    palette = "-YlOrRd",
    colorNA = "white" 
  ) +  tm_layout( 
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = TRUE )

tmap_save(
  Mapa_lp,
  "COL/mpio/1.Ingreso/Output/MunicipiosIncomeDirect.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
