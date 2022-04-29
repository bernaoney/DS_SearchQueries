
load("data/ts_prepped.RData")
library(tidyverse)

# messy line plot of all --------------------------------------------------

ts %>% ggplot(aes(x = date, y = hits, color = keyword)) + 
  geom_line() +
  scale_y_continuous(expand = expansion(add = c(0,0))) +
  labs(x = "", y = "Normalized search volume with 0 - 100 range",
       title = "Public interest",
       subtitle = "by search TOPIC",
       caption = "Google offers topics that are language agnostic
       \n& account for spelling variations & mistakes,
       \nas well as multiple names for the same thing.
       \nsee https://blog.google/products/search/15-tips-getting-most-out-google-trends/") +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(legend.position = 'top')

# faceted messy line plots ------------------------------------------------

gridExtra::grid.arrange(ts %>% filter(DataEcon == "Data") %>%
                          ggplot(aes(x = date, y = hits, color = keyword)) + 
                          geom_line() +
                          # geom_area(alpha = .1) +
                          scale_y_continuous(expand = expansion(add = c(0,0))) +
                          labs(x = "", y = "",
                               subtitle = "Data Related") +
                          scale_colour_brewer(palette = "Set3") +
                          theme_bw() +
                          theme(legend.position = 'top'),
                        
                        ts %>% filter(DataEcon == "Economy") %>%
                          ggplot(aes(x = date, y = hits, color = keyword)) + 
                          geom_line() +
                          # geom_area(alpha = .1) +
                          scale_y_continuous(expand = expansion(add = c(0,0))) +
                          labs(x = "", y = "",
                               subtitle = "Economy Related") +
                          scale_colour_brewer(palette = "Accent") +
                          theme_bw() +
                          theme(legend.position = 'top'),
                        left = "Normalized search volume with 0 - 100 range")

# faceted line plots with smoothed trendlines -- all categories -----------

library(tidyquant)
library(timetk)
ts %>% group_by(keyword) %>%
  plot_time_series(date, hits,
                   .facet_ncol = 4,
                   .facet_scales = "fixed",
                   .interactive = FALSE,
                   .y_lab = "Normalized search volume with 0 - 100 range",
                   .title = "Public interest in Germany for data & economy related search topics & terms over time")

# summarize multiple trends with PCA --------------------------------------

Data_wide <- ts %>%
  filter(DataEcon == "Data") %>% 
  select(date, hits, keyword) %>%
  pivot_wider(id_cols = date,
              names_from = keyword,
              values_from = hits)

Economy_wide <- ts %>%
  filter(DataEcon == "Economy") %>% 
  select(date, hits, keyword) %>%
  pivot_wider(id_cols = date,
              names_from = keyword,
              values_from = hits)

## get the 1st PCs

library(trendecon)
library(tsbox)
Data_wide_pca <- as.data.frame(ts_prcomp(ts(Data_wide)))
Data_wide$DataRel_PC1 <- Data_wide_pca$PC1

Economy_wide_pca <- as.data.frame(ts_prcomp(ts(Economy_wide)))
Economy_wide$EcoRel_PC1 <- Economy_wide_pca$PC1

gridExtra::grid.arrange(
  Data_wide %>% ggplot(aes(x = date, y = DataRel_PC1)) + 
    geom_line() +
    labs(title = "1st PC of Data related search term & topics") +
    theme_bw(),
  Economy_wide %>% ggplot(aes(x = date, y = EcoRel_PC1)) + 
    geom_line() +
    labs(title = "1st PC of Economy related search term & topics") +
    theme_bw()
)

## build the DF with the 1st PCs
## rescale the PCs back to 0-100 range

ts_DR <- Data_wide %>% select(date, DataRel_PC1) %>%
  mutate(
    DataRel_PC1 = round(scales::rescale(Data_wide$DataRel_PC1,
                                        to = c(0,100),
                                        from = range(Data_wide$DataRel_PC1,
                                                     na.rm = FALSE,
                                                     finite = TRUE)), 2),
    DataEcon = "Data"
  )

ts_ER <- Economy_wide %>% select(date, EcoRel_PC1) %>%
  mutate(
    EcoRel_PC1 = round(scales::rescale(Economy_wide$EcoRel_PC1,
                                       to = c(0,100),
                                       from = range(Economy_wide$EcoRel_PC1,
                                                    na.rm = FALSE,
                                                    finite = TRUE)), 2),
    DataEcon = "Economy"
  )

gridExtra::grid.arrange(
  ts_DR %>% ggplot(aes(x = date, y = DataRel_PC1)) + 
    geom_line() +
    labs(title = "1st PC of Data related search term & topics -- Rescaled 0-100") +
    theme_bw(),
  ts_ER %>% ggplot(aes(x = date, y = EcoRel_PC1)) + 
    geom_line() +
    labs(title = "1st PC of Economy related search term & topics") +
    theme_bw()
)

ts_DR <- ts_DR %>% rename(PC = DataRel_PC1)
ts_ER <- ts_ER %>% rename(PC = EcoRel_PC1)
ts_DR_ER <- bind_rows(ts_DR, ts_ER)

# 1st PC line plots -------------------------------------------------------

## ggplot
ts_DR_ER %>% ggplot(aes(x = date, y = PC, color = DataEcon)) + 
  geom_line() +
  labs(x = "Date", y = "",
       title = "Time series of data & economy related search term & topics summarized by their 1st princial components",
       subtitle = "These time series can be seen as the proxy of public interest to these topics in Germany",
       caption = "Economy related search queries & topics's drop in 2008 is
                  \nnoteworthy. My best guess is that this is the time window
                  \nwhere topics like economic crisis, economic recovery,
                  \nkurzarbiet, unemployement & bankrupcy take over other topics
                  \nlike economic growth & invest.") +
  theme_bw()

## with trend
ts_DR_ER %>% group_by(DataEcon) %>%
  plot_time_series(date, PC,
                   .facet_scales = "fixed",
                   .interactive = FALSE,
                   .y_lab = "1st princial components of search term & topics",
                   .title = "Public interest in data & economy related search topics & terms over time")

## ---------------------------
## save PCs DF
## ---------------------------
save(ts_DR_ER, file = "data/ts_PCA.RData")
