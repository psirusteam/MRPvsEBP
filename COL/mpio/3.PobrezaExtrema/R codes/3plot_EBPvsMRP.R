fit_bayes_EBP <- readRDS("COL/2019/2.Pobreza/Data/fit_ebp_logit.rds")
fit_freq_EBP <-
  readRDS("COL/2019/2.Pobreza/Data/fit_freq_ebp_logit.rds")

fit_bayes_mrp <-
  readRDS("COL/2019/2.Pobreza/Data/fit_mrp_logit.rds")
fit_freq_mrp <-
  readRDS("COL/2019/2.Pobreza/Data/fit_freq_mrp_logit.rds")



new_encuesta <-
  encuesta_mrp %>% inner_join(statelevel_predictors_df, by = "depto")

y_pred_Bayes_EBP <-
  posterior_epred(fit_bayes_EBP,  type = "response")
y_pred_freq_EBP <-
  predict(fit_freq_EBP, type = "response")



gebp  <- ggplot(data.frame(datos = c(colMeans(y_pred_Bayes_EBP))),
                aes(x = datos, fill = "EBP Bayes")) +
  geom_density(aes(weight = encuesta_df_agg$n), alpha = 0.3) +
  geom_density(
    data = data.frame(y_EBP  = y_pred_freq_EBP),
    aes(x = y_EBP, fill = "EBP freq"),
    alpha = 0.3
  )


y_pred_Bayes_MRP <-
  posterior_epred(fit_bayes_mrp,  type = "response")
y_pred_freq_MRP <-
  predict(fit_freq_mrp,  type = "response")

gmrp  <- ggplot(data.frame(datos = c(colMeans(y_pred_Bayes_MRP))),
                aes(x = datos, fill = "MRP Bayes")) +
  geom_density(aes(weight = encuesta_df_agg$n), alpha = 0.3) +
  geom_density(
    data = data.frame(y_mrp  = y_pred_freq_MRP),
    aes(x = y_mrp, fill = "MRP freq"),
    alpha = 0.3
  )

gebp / gmrp


data.frame(
  y_pred_Bayes_EBP = colMeans(y_pred_Bayes_EBP),
  y_pred_freq_EBP = y_pred_freq_EBP,
  y_pred_Bayes_MRP = colMeans(y_pred_Bayes_MRP),
  y_pred_freq_MRP = y_pred_freq_MRP
) %>% plot()



fit_freq_EBP <-
  readRDS("COL/2019/2.Pobreza/Data/fit_freq_ebp_logit.rds")
fit_freq_mrp <-
  readRDS("COL/2019/2.Pobreza/Data/fit_freq_mrp_logit.rds")

y_pred_freq_ebp <-
  predict(fit_freq_EBP,  type = "response")
y_pred_freq_MRP <-
  predict(fit_freq_mrp,  type = "response")

plot(y_pred_freq_ebp,y_pred_freq_MRP)


