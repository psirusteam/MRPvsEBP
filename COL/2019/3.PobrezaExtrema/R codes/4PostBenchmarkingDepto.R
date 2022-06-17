#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())
cat("\f")
# Loading required libraries ----------------------------------------------

library(scales)
library(patchwork)
library(srvyr)
library(survey)
library(haven)
library(sampling)
library(tidyverse)

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
encuesta_mrp <- readRDS("COL/2019/3.PobrezaExtrema/Data/encuesta_mrp.rds")
censo_mrp <- readRDS("COL/2019/3.PobrezaExtrema/Data/censo_mrp.rds")
tasa_desocupados <- readRDS("COL/2019/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

fit <- readRDS("COL/2019/3.PobrezaExtrema/Data/fit_mrp_logit.rds")


# Poststratification at the National Level --------------------------------

statelevel_predictors_df <- tasa_desocupados


byAgrega <-
  grep(
    pattern =  "^(n|pobreza|ingreso|.groups)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

poststrat_df <- censo_mrp %>%  group_by_at(byAgrega) %>%
  summarise(n = sum(n), .groups = "drop")

# Expand state level predictors to the individual level

poststrat_df <- left_join(poststrat_df, statelevel_predictors_df,
                          by = "depto")

# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.


epred_mat <- predict(fit, newdata = poststrat_df, type = "response",
                     allow.new.levels = TRUE)

length(epred_mat[which(epred_mat < 0)])
poststrat_df[which(epred_mat < 0),]
epred_mat[which(epred_mat < 0)] = 1

summary(epred_mat)
hist(as.numeric(epred_mat))


poststrat_df$epred_mat <- epred_mat

# Resultados nacionales ---------------------------------------------------
## lineas de pobreza por área
## "0" = "Rural", "1" = "Urbana"

(lp <- encuesta_mrp %>% select(area,li) %>% unique())
poststrat_df %<>% inner_join(lp, by = "area")


# Calculo de la pobreza Cepal.  -------------------------------------------
encuesta_mrp %<>% mutate(pobreza = ingreso<li)
poststrat_df %<>% mutate(pobreza = epred_mat)

# definiendo diseno muestral

diseno <- encuesta_mrp %>%
  as_survey_design(weights = fep)

###########################################
###########################################
###           Benchmarking              ###
###     (Gutiérrez - Guerrero, 2022)    ###
###########################################
###########################################
# poststrat_df <- poststrat_df_temp
names_cov <- c("Nacional", "area", "sexo", "edad" , "depto")
names_cov <- setNames(names_cov,names_cov)

R <- map(names_cov, function(byi){
  encuesta_mrp %>% mutate(Nacional = 1) %>% 
    group_by_at(all_of(byi)) %>% 
    summarise(R = sum(pobreza*fep)/sum(fep))
}) 


poststrat_df %<>% mutate(Nacional = 1) %>% 
  fastDummies::dummy_cols(
    select_columns = names_cov,
                    remove_selected_columns = FALSE)  

yk <- poststrat_df %>% 
  mutate_at(vars(matches("_\\d{,2}$")) ,~.*pobreza*n) %>% 
  select(matches("_\\d{,2}$"))

xk <- poststrat_df %>% 
  mutate_at(vars(matches("_\\d{,2}$")) ,~.*n) %>% 
  select(matches("_\\d{,2}$"))

R_temp <- data.frame(
  matrix(c(R$Nacional$R, R$depto$R, R$area$R, R$sexo$R, R$edad$R),
                            nrow = nrow(yk), 
                  ncol = 34, byrow = TRUE))
head(R_temp)
names(R_temp) <- c("Nacional_1",
                   paste0("depto_", unique(R$depto$depto)),
                   paste0("area_", unique(R$area$area)),
                   paste0("sexo_", unique(R$sexo$sexo)),
                   paste0("edad_", unique(R$edad$edad))
                   )

# 
# R_temp <-poststrat_df %>%mutate(id = 1:n()) %>%  
#   select(id, depto, names_depto) %>% 
#   left_join(y = R[[1]]) %>% 
#   mutate_at(vars(matches("_\\d{,2}$")) ,~.*R) 



# R_temp <- map(names_cov,
#               ~poststrat_df %>%mutate(id = 1:n()) %>%  
#                 select(id, matches(.x)) %>% 
#                 left_join(R[[.x]]) %>% 
#             mutate_at(vars(matches("_\\d{,2}$")) ,~.*R) %>% 
#               mutate(R = NULL)) 
# R_temp %>% map(~sum(is.na(.x)) )


zk <- yk[,names(R_temp)] - R_temp*xk[,names(R_temp)]

# poststrat_df %>% mutate(
#   Nacional = 1,
#   yk = pobreza * n,
#   xk = 1 * n,
#   zk = yk - R$R * xk,
#   ##### depto = 05 
#   yk_05 = pobreza * n*(depto == "05"),
#   xk_05 = (depto == "05") * n,
#   zk_05 = yk_05 - R$R_05 * xk_05,
#   ##### sexo = 1 
#   yk_1 = pobreza * n*(sexo == "1"),
#   xk_1 = (sexo == "1") * n,
#   zk_1 = yk_1 - R$R_1 * xk_1,
#   ##### sexo = 2
#   yk_2 = pobreza * n*(sexo == "2"),
#   xk_2 = (sexo == "2") * n,
#   zk_2 = yk_2 - R$R_2 * xk_2,
# )


colSums(yk)/colSums(xk)
R


poststrat_df$gk <- calib(Xs = as.matrix(zk), 
                         d = rep(1, nrow(zk)),
                         total = rep(0, ncol(zk)),
                         method="logit")

checkcalibration(Xs = as.matrix(zk), 
                 d = rep(1, nrow(zk)),
                 total = rep(0, ncol(zk)),
                 g = poststrat_df$gk)

poststrat_df$pobreza2 <- poststrat_df$pobreza
poststrat_df$n <- poststrat_df$gk*poststrat_df$n

temp <- map(names_cov, function(byi){
  poststrat_df %>% mutate(Nacional = 1) %>% 
    group_by_at(all_of(byi)) %>% 
    summarise(R = sum(pobreza2*n)/sum(n))
}) 

temp$Nacional
R$Nacional

temp$area
R$area

temp$sexo
R$sexo

temp$edad
R$edad

inner_join( temp$depto %>% data.frame(),
R$depto %>% data.frame(), by = "depto")
  

hist(poststrat_df$gk)
summary(poststrat_df$gk)
sum(poststrat_df$gk<0)

jpeg(file = "COL/2019/3.PobrezaExtrema/Output/Plot_Bench_Pobreza2.jpeg")
hist(poststrat_df$gk)
dev.off()

saveRDS(poststrat_df %>% select(!matches("_\\d{,2}$"), -Nacional), 
        "COL/2019/3.PobrezaExtrema//Data/poststrat_df.RDS")
saveRDS(encuesta_mrp, "COL/2019/3.PobrezaExtrema/Data/encuesta_mrp.RDS")




