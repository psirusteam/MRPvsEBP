#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
memory.limit(500000)

library(tidyverse)
library(sampling)
library(reticulate) # Conexión con Python
library(rgee) # Conexión con Google Earth Engine
library(sf) # Paquete para manejar datos geográficos
library(concaveman)
library(geojsonio)
library(magrittr)

####################################################
### Loading datasets: EH and Population census ###
####################################################
tasa_desocupacion <-
  readRDS("COL/mpio/1.Ingreso/Data/tasa_desocupacion.rds")


#######################################
### configuración inicial de Python ###
#######################################

rgee_environment_dir = "C://Users//sguerrero//Anaconda3//envs//rgee_py//python.exe"
# Configurar python (Algunas veces no es detectado y se debe reiniciar R)
reticulate::use_python(rgee_environment_dir, required = T)
rgee::ee_install_set_pyenv(py_path = rgee_environment_dir, py_env = "rgee_py")
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)
rgee::ee_Initialize(drive = T)
###################################################
### Arreglar la shape                           ###
### Aplicar el CONVEX HULL a los multipolígonos ###
###################################################

## revisando COLivia
COL <-
  read_sf("COL/mpio/ShapeDeptoCOL/dv_Municipio.shp") %>%
  rename(mpio = COD_DANE) %>%
  mutate(mpio = str_pad(mpio, pad = "0", width = 5))
###################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)


mpio <- COL %>% data.frame() %>% distinct(mpio)
COL_luces <- map(mpio$mpio,function(ii){
  ee_extract(
  x = luces,
  y = COL["mpio"] %>% filter(mpio == ii),
  ee$Reducer$mean(),
  sf = FALSE
)})
COL_luces %<>% bind_rows()
#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()
ee_print(tiposuelo)

COL_urbano_cultivo <- map(mpio$mpio,
                          function(ii){
    cat(ii,"\n")
  ee_extract(
                   x = tiposuelo,
                   y = COL["mpio"] %>% filter(mpio == ii ),
                   ee$Reducer$mean(),
                   sf = FALSE
                 )}) 
COL_urbano_cultivo %<>% bind_rows()
###############
### Guardar ###
###############

tasa_desocupacion %<>% 
  full_join(COL_luces) %>% 
  full_join(COL_urbano_cultivo)

tasa_desocupacion %>% filter(is.na(tasa_desocupacion))

cor(tasa_desocupacion[, -1 ])

saveRDS(tasa_desocupacion, "COL/mpio/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "COL/mpio/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "COL/mpio/3.PobrezaExtrema/Data/tasa_desocupacion.rds")
