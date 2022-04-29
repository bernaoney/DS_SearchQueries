
load("data/ts_PCA.RData")

library(tidymodels)
library(tidyverse)
library(lubridate)
library(modeltime)
library(timetk)
library(modeltime.gluonts)

# prep DFs ----------------------------------------------------------------

ts_DR <- ts_DR_ER %>% filter(DataEcon == "Data") %>% 
  rename(id = DataEcon) %>% 
  mutate(PC = round(PC, 0)) %>%
  select(id, date, PC)
ts_splits <- initial_time_split(ts_DR, prop = 0.9)


## models


# arima -------------------------------------------------------------------

model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(PC ~ date, data = training(ts_splits))

# boosted arima -----------------------------------------------------------

model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(PC ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
      data = training(ts_splits))

# prophet -----------------------------------------------------------------

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(PC ~ date, data = training(ts_splits))


# GluonTS  ----------------------------------------------------------------

## gluonts is a bit harder to handle on Win OS, just like any other dl frameworks
## a unix-based platform is strongly recommended
## make sure to have the relevant environment available
nrow(testing(ts_splits))
horizon <- 22
lookback <- 2*22

model_fit_deepar <- deep_ar(
  id                    = "id",
  freq                  = "M",
  prediction_length     = horizon,
  lookback_length       = lookback,
  epochs                = 5
) %>%
  set_engine("gluonts_deepar") %>%
  fit(PC ~ date + id, data = training(ts_splits))


# model op ----------------------------------------------------------------

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_prophet,
  model_fit_deepar
)
models_tbl

# calibrate ---------------------------------------------------------------

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(ts_splits))
calibration_tbl

# visualize ---------------------------------------------------------------

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(ts_splits),
    actual_data = ts_DR
  ) %>%
  plot_modeltime_forecast(
    .conf_interval_alpha = .1,
    .title = "Forecast plot of 4 models",
    .y_lab = "Search volume: Public interest in data related search queries & topics in Germany",
    .interactive = FALSE
  )

# model performance -------------------------------------------------------

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )



