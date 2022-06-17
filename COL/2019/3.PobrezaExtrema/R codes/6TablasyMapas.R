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
encuesta_mrp <- readRDS("COL/2019/3.PobrezaExtrema/Data/encuesta_mrp.rds")
poststrat_df <- readRDS("COL/2019/3.PobrezaExtrema/Data/poststrat_df.RDS")

bynames <-
  grep(
    pattern =  "^(X|F|n|li|pobreza|ingreso|tasa_desocupacion|epred_mat|gk|depto|lp|.groups)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )

bynames <-   t(combn(bynames, 2)) 


dat_df = map(1:nrow(bynames),
             ~poststrat_df %>% group_by_at(vars("depto", bynames[.x,])) %>%
               summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
               ungroup())

bynames <-   as.tibble(bynames) %>% 
  mutate(dat_df = dat_df)


bynames <- mutate(
  bynames,
  outPaht = paste0("COL/2019/3.PobrezaExtrema/Output/pobreza_", paste0(V1, "_", V2),
                   ".pdf")
)

states_df <- poststrat_df %>% group_by_at("depto") %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
  ungroup() 
## Leer Shape del pais
ShapeSAE <-
  read_sf("COL/2020/ShapeDeptoCOL/depto.shp") %>%
  rename(depto = DPTO) %>%
  mutate(depto = str_pad(depto, pad = "0", width = 2),
         nombre = NOMBRE_DPT)

## Ajustando codigo de departamento

### cod de departamento en la shape
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(depto, nombre)


## El censo y la encuesta fueron conciliados previamente.
cod_encusta <- c(
  "05" = "Antioquia",
  "08" = "Atlántico",
  "11" = "Bogotá, d.C.",
  "13" = "Bolívar",
  "15" = "Boyacá",
  "17" = "Caldas",
  "18" = "Caquetá",
  "19" = "Cauca",
  "20" = "Cesar",
  "23" = "Córdoba",
  "25" = "Cundinamarca",
  "27" = "Chocó",
  "41" = "Huila",
  "44" = "La  guajira",
  "47" = "Magdalena",
  "50" = "Meta",
  "52" = "Nariño",
  "54" = "Norte de santander",
  "63" = "Quindio",
  "66" = "Risaralda",
  "68" = "Santander",
  "70" = "Sucre",
  "73" = "Tolima",
  "76" = "Valle del cauca"
) %>%
  data.frame(nombre = .) %>% rownames_to_column("depto") %>%
  mutate(nombre = toupper(nombre))

cod_censo <-
  c(
    "05" = "ANTIOQUIA",
    "08" = "ATLANTICO",
    "11" = "BOGOTA, D.C.",
    "13" = "BOLIVAR",
    "15" = "BOYACA",
    "17" = "CALDAS",
    "18" = "CAQUETA",
    "19" = "CAUCA",
    "20" = "CESAR",
    "23" = "CORDOBA",
    "25" = "CUNDINAMARCA",
    "27" = "CHOCO",
    "41" = "HUILA",
    "44" = "LA GUAJIRA",
    "47" = "MAGDALENA",
    "50" = "META",
    "52" = "NARIÑO",
    "54" = "NORTE DE SANTANDER",
    "63" = "QUINDIO",
    "66" = "RISARALDA",
    "68" = "SANTANDER",
    "70" = "SUCRE",
    "73" = "TOLIMA",
    "76" = "VALLE DEL CAUCA",
    "81" = "ARAUCA",
    "85" = "CASANARE",
    "86" = "PUTUMAYO",
    "88" = "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA",
    "91" = "AMAZONAS",
    "94" = "GUAINIA",
    "95" = "GUAVIARE",
    "97" = "VAUPES",
    "99" = "VICHADA"
  ) %>%
  data.frame(nombre = .) %>% rownames_to_column("depto") %>%
  mutate(nombre = toupper(nombre))

full_join(cod_encusta, cod_censo, by = "depto") %>%
  full_join(cod_shape, by = "depto")

###############################################################################

# Colombia
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(states_df,  by = "depto"))

brks_li <- c(0, 0.05, 0.1, 0.15, 0.2, 1)
tmap_options(check.and.fix = TRUE)
Mapa_lp <-
  P1_ingresolp + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_li,
    title = "Pobreza extrema",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_lp,
  "COL/2019/3.PobrezaExtrema/Output/Estados2.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

bynames %<>% rename(fnames = V1, cnames = V2)


#########################################################
## Exportando tablas 
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(depto, NOMBRE_DPT) %>% 
  rename(nombre = NOMBRE_DPT)

nom_tabs <- c("depto", paste0(bynames$fnames, "_", bynames$cnames))
dat_df <- c(list(states_df), bynames$dat_df)
names(dat_df) <- nom_tabs

dat_df <-
  map(dat_df, ~ full_join(y = .x, cod_shape, by = "depto")) %>%
  map( ~ .x %>% rename(Benchmarking = Benchmarking_estimate))

openxlsx::write.xlsx(dat_df, 
                     file = "COL/2019/3.PobrezaExtrema/Output/tablas2.xlsx",
                     overwrite = TRUE)

###########################################################
##                      Extra  
####    Validación de pobreza y pobreza extrema        ####
###########################################################
poststrat_Pobreza <- readRDS("COL/2020/2.Pobreza/Data/poststrat_df.RDS")

dat_Pobreza_E <- bynames$dat_df

dat_Pobreza = map(
  1:nrow(bynames),
  ~ poststrat_Pobreza %>% group_by_at(vars("depto",unlist(bynames[.x, 1:2 ]) )) %>%
    summarise(
      Benchmarking_pobreza = sum(n * pobreza2) / sum(n),
      .groups = "drop"
    ) %>%
    ungroup()
)

map2(dat_Pobreza,dat_Pobreza_E,inner_join) %>% 
  map(~.x %>% filter(Benchmarking_pobreza < Benchmarking_estimate))


###########################################################
###########################################################

map2(dat_Pobreza, dat_Pobreza_E, inner_join) %>%
  map( ~ .x %>% mutate(
    Benchmarking = ifelse(
      Benchmarking_pobreza <= Benchmarking_estimate,
      Benchmarking_pobreza,
      Benchmarking_estimate
    )
  )) %>%
  map( ~ .x %>% filter(Benchmarking_pobreza < Benchmarking_estimate))


dat_Pobreza <- map2(dat_Pobreza, dat_Pobreza_E, inner_join) %>%
  map( ~ .x %>% mutate(
    Benchmarking = ifelse(
      Benchmarking_pobreza < Benchmarking_estimate,
      Benchmarking_pobreza,
      Benchmarking_estimate
    ),
    Benchmarking_pobreza = NULL, Benchmarking_estimate=NULL
  )) 
  

dat_Pobreza <- c(list(states_df), dat_Pobreza)
names(dat_Pobreza) <- names(dat_df)

dat_Pobreza <-
  map(dat_Pobreza, ~ full_join(y = .x, cod_shape, by = "depto"))
  

openxlsx::write.xlsx(dat_Pobreza, file = "COL/2019/3.PobrezaExtrema/Output/tablas.xlsx",
                     overwrite = TRUE)


###########################################################
###########################################################

# bynames$Maps = pmap(bynames , function(...) {
#   Aux_Maps(Shape = ShapeSAE,
#            color_p = "YlOrRd" ,
#            brks = brks_li,
#            ...)
# })

