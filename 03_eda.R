
load("data/ts_PCA.RData")

# bayesian belief network -------------------------------------------------

## prep DF
library(tidyverse)
wide <- ts_DR_ER %>% 
  pivot_wider(id_cols = date,
              names_from = DataEcon,
              values_from = PC)
df <- wide %>%
  mutate(n_months = 1:nrow(wide)) %>%
  select(Data, Economy, n_months) %>%
  as.data.frame() %>% round(0)

## prep black list
bl <- as.matrix(data.frame(from = c("Economy", "Data"),
                           to = c("n_months", "n_months")))

library(bnlearn)
set.seed(2022)
bn_op <- tabu(df, blacklist = bl)
library(qgraph)
qgraph(bn_op, vsize = 9, label.cex = 2)
# no relationship between data & economy
# only time dictates the observed values ??
library(parallel)
cl <- makeCluster(7)

set.seed(2021)
boot_res <- boot.strength(data = df,
                          R = 10000,
                          algorithm = "tabu",
                          algorithm.args = list(blacklist = bl),
                          cluster = cl)
avgnet_threshold <- averaged.network(boot_res, threshold = .99)
qgraph(avgnet_threshold, vsize = 9, label.cex = 2,
       title = "Bayesian belief network revealing whether there is a causal relationship
                \nbetween search volume of data and economy related topics & keywords
                \nThe answer is no, there is no such relationship; only the time determines observed search volumes")

## see if ts ccf and regression agrees

stopCluster(cl)
gdata::keep(ts_DR_ER, wide, sure = TRUE)


# time series -------------------------------------------------------------

library(tidyquant)
library(timetk)
## seasonality
ts_DR_ER %>% 
  group_by(DataEcon) %>%
  plot_stl_diagnostics(
    date, PC,
    .facet_scales = "fixed",
    .frequency = "auto", .trend = "auto",
    .feature_set = c("observed", "season", "trend", "remainder"),
    .interactive = FALSE,
    .title = "Seasonal & trend decomposition for search volumes")

## check acf pacf ccf
wide %>%
  plot_acf_diagnostics(date, 
                       Data,
                       .ccf_vars = Economy,
                       .lags = 36, .interactive = FALSE,
                       .title = "Lag diagnostics for the search volume of Data related queries and topics
                       \nwith Economy as the cross correlation function variables")

## regular correlation
wide %>% select(-date) %>% 
  correlation::correlation() # 0.90*** [0.87, 0.92]
