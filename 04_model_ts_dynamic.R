
load("data/ts_PCA.RData")
library(tidyverse)

# make ts -----------------------------------------------------------------

ts_wide <- ts_DR_ER %>%
  pivot_wider(id_cols = date,
              names_from = DataEcon,
              values_from = PC)

ts_wide$date <- as.Date(ts_wide$date)
min(ts_wide$date)
max(ts_wide$date)
ts <- ts(data = ts_wide[-1], start = 2004, end = 2022, frequency = 12)
head(ts)
tail(ts)
ts

# baseline dynamic model --------------------------------------------------

library(dynlm)
mod_dynlm <- dynlm(d(Data) ~ L(Data, 1)
                   + L(Data, 12)
                   + L(Data, 24)
                   + L(Data, 36),
                   data = ts)
summary(mod_dynlm) # Adjusted R-squared:  0.239
library(easystats)
mod_dynlm_mp <- parameters(mod_dynlm)
mod_dynlm_mp$Parameter <- c(
  "(Intercept)",
  "1 month lag of Data search volume", 
  "12 months lag of Data search volume",
  "24 months lag of Data search volume",
  "36 months lag of Data search volume")
plot(mod_dynlm_mp) + 
  labs(title = "Dynamic Linear Model | Baseline Model",
       subtitle = "Time Series Regression | Start: January 2004 -- End: January 2022",
       caption = "Adjusted R-squared: 24 %")


# dynamic model with covariate --------------------------------------------

mod_dynlm_cov <- dynlm(d(Data) ~ L(Data, 1)
                       + L(Data, 12)
                       + Economy,
                       data = ts)
summary(mod_dynlm_cov) # Adjusted R-squared:  0.245
mod_dynlm_cov_mp <- parameters(mod_dynlm_cov)
mod_dynlm_cov_mp$Parameter <- c(
  "(Intercept)",
  "1 month lag of Data search volume", 
  "12 months lag of Data search volume",
  "Economy"
)
plot(mod_dynlm_cov_mp) + 
  labs(title = "Dynamic Linear Model with covariate",
       subtitle = "Time Series Regression | Start: January 2004 -- End: January 2022",
       caption = "Adjusted R-squared: 24 %")


# dynamic model with covariate lags ---------------------------------------

mod_dynlm_cov_lag <- dynlm(d(Data) ~ L(Data, 1)
                           + L(Data, 12)
                           + L(Economy, 1)
                           + L(Economy, 12)
                           + L(Economy, 24)
                           + L(Economy, 36),
                           data = ts)
summary(mod_dynlm_cov_lag) # Adjusted R-squared:  0.267
mod_dynlm_cov_lag_mp <- parameters(mod_dynlm_cov_lag)
mod_dynlm_cov_lag_mp$Parameter <- c(
  "(Intercept)",
  "1 month lag of Data search volume", 
  "12 months lag of Data search volume",
  "1 month lag of Economy search volume", 
  "12 months lag of Economy search volume",
  "24 months lag of Economy search volume",
  "36 months lag of Economy search volume"
)
plot(mod_dynlm_cov_lag_mp) + 
  labs(title = "Dynamic Linear Model with covariate lags",
       subtitle = "Time Series Regression | Start: January 2004 -- End: January 2022",
       caption = "Adjusted R-squared: 27 %")


# final dynamic model -----------------------------------------------------

mod_dynlm_final <- dynlm(d(Data) ~ L(Data, 1)
                         + L(Data, 12)
                         + L(Economy, 1),
                         data = ts)
summary(mod_dynlm_final) # Adjusted R-squared:  0.254
mod_dynlm_final_mp <- parameters(mod_dynlm_final)
mod_dynlm_final_mp$Parameter <- c(
  "(Intercept)",
  "1 month lag of Data search volume", 
  "12 months lag of Data search volume",
  "1 month lag of Economy search volume"
)
plot(mod_dynlm_final_mp) + 
  labs(title = "Dynamic Linear Model final",
       subtitle = "Time Series Regression | Start: January 2004 -- End: January 2022",
       caption = "Adjusted R-squared: 25 %")

plot(mod_dynlm_cov_lag_mp, mod_dynlm_final_mp)
mod_dynlm_final_mp # the 1st lag of economy's effect is too small

jtools::plot_summs(mod_dynlm, # baseline
                   mod_dynlm_cov, # with covariate
                   mod_dynlm_cov_lag,  # with covariate lags
                   mod_dynlm_final, # selected model
                   model.names = c("Baseline Model -- only the lags of predicted time series",
                                   "Model including the covariate",
                                   "Model including the covariate's lags",
                                   "Final model"),
                   legend.title = "Models") + 
  labs(title = "Dynamic linear models for time series",
       subtitle = "Time Series Regression | Start: January 2004 -- End: January 2022
       \nData = Search volume of data related queries and topics in Germany
       \nEconomy = Search volume of economy related queries and topics in Germany
       \nL(, #) L stands for lag; # is the lagged number of months of the times series in the models",
       caption = "Adjusted R-squared
       \nBaseline Model 24 %
       \nModel including the covariate 25 %
       \nModel including the covariate's lags 27 %
       \nFinal Model 25 %
       \n
       \nData Source: Google Trends; the 1st principal components of data and economy related search queries & topics are used in analyses")
