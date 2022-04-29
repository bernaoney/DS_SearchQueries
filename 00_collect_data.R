
## ---------------------------
pacman::p_load(tidyverse, lubridate, gtrendsR, purrr)

# collect data ------------------------------------------------------------

gTrends_bdl <- function (keywords) { 
  
  country <- "DE"
  time <- c("all") 
  channel <- "web" 
  
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time,
                    low_search_volume = TRUE)
  
  results <- trends$interest_over_time
  results$hits <- as.character(results$hits)
  
  return(results)
}

# define search topics & terms --------------------------------------------

topics_and_keywords <- c(
  
  "/m/0jt3_q3",             # Data Science - Field of study
                         
                            # python, data scientist gehalt,
                            # data science / scientist jobs, machine learning,
                            # data analytics, big data, data analyst,
                            # data engineer
                            # ...

  "/m/016jq3",              # Business intelligence - topic
  
                            # power / microsoft / sap / oracle bi
                            # dashboard
                            # ...
  
  "/g/11ckkys_95",          # Tableau Software - topic
  
                            # tableau, tableau dashboard, sql
                            # ...
  
  "/m/0h1fn8h",             # Deepl learning - topic
  
                            # neural network, deep neural network,
                            # tensorflow, ai, github, keras,
                            # pytorch, matlab,
                            # deep learning vs. machine learning,
                            # ...
  
  "data science",           # data science - search term
  "r for data science",     # related search term
  "python for data science",# related search term
  "data engineering",       # data engineering - search term
  "big data",               # big data - search term
  "data mining",            # data mining - search term
  "software engineering",   # software engineering - search term
  
  "Datenwissenschaftler",
  "Datenwissenschaftlerin",
  "Dateningenieur",
  "Dateningenieurin",
  "Geschäftsintelligenz",
  
  
  "/m/012_3l",              # Economic growth - topic
                  
                            # wirtschaftswachstum,
                            # wirtschaftswachstum deutschland
                            # gdp, gdp growth, arbeitslosenquote
                            # inflation, inflationsrate,
                            # ...
  
  "/g/1211cg58",            # Economic crisis - topic
  
                            # wirtschaftskrise, wirtschaftskrise deutschland
                            # finanzkrise, crisis economica
                            # corona wirtschaftskrise, weltwirtschaftskrise
                            # ...
 
  "/m/0gmfp_7",             # Economic recovery - topic
                            # ...
  
  "investieren",            # search query
  
  "/m/0dgpn15",             # Kurzarbeit - topic
  
                            # kurzarbeitergeld / corona / rechner
                            # agentur für arbeit
                            # kündigung kurzarbeit
                            # antrag kurzarbeitergeld
  
  "/m/07s_c",               # Unemployment - topic
  
                            # arbeitslos, arbeitslosigkeit, arbeitslosengeld
                            # deutschland arbeitslosigkeit, hartz 4
                            # ...
  
  "Insolvenz"               # insolvenz - search term || insolvency
)


# pull the data -----------------------------------------------------------

ts_raw <- map_dfr(.x = topics_and_keywords,
                  .f = gTrends_bdl)

length(unique(ts_raw$keyword))


# prep the ts DF with proper labels ---------------------------------------

ts <- ts_raw %>% mutate(
  keyword = case_when(
    keyword == "/m/0jt3_q3" ~ "Data_Science_field_of_study",
    keyword == "/m/016jq3" ~ "Business_intelligence_topic",
    keyword == "/g/11ckkys_95" ~ "Tableau_Software_topic",
    keyword == "/m/0h1fn8h" ~ "Deepl_learning_topic",
    keyword == "data science" ~ "data_science_search_term",
    keyword == "r for data science" ~ "r_for_data_science_search_term",
    keyword == "python for data science" ~ "python_for_data_science_search_term",
    keyword == "data engineering" ~ "data_engineering_search_term",
    keyword == "big data" ~ "big_data_search_term",
    keyword == "data mining" ~ "data_mining_search_term",
    keyword == "software engineering" ~ "software_engineering_search_term",
    keyword == "Datenwissenschaftler" ~ "Datenwissenschaftler_search_term",
    keyword == "Datenwissenschaftlerin" ~ "Datenwissenschaftlerin_search_term",
    keyword == "Dateningenieur" ~ "Dateningenieur_search_term",
    keyword == "Dateningenieurin" ~ "Dateningenieurin_search_term",
    keyword == "Geschäftsintelligenz" ~ "Geschäftsintelligenz_search_term",
    keyword == "/m/012_3l" ~ "Economic_growth_topic",
    keyword == "/g/1211cg58" ~ "Economic_crisis_topic",
    keyword == "/m/0gmfp_7" ~ "Economic_recovery_topic",
    keyword == "investieren" ~ "investieren_search_term",
    keyword == "/m/0dgpn15" ~ "Kurzarbeit(short-time-work)_topic",
    keyword == "/m/07s_c" ~ "Unemployment_topic",
    keyword == "Insolvenz" ~ "Insolvenz(bankruptcy)_search_term",
    TRUE ~ as.character(keyword)
  )
) %>% 
  mutate_at("hits", ~ifelse(. == "<1", 0.5, .)) %>%
  mutate_at("hits", ~as.numeric(.))

length(unique(ts$keyword))

## some things are missing
## check
## what is
## missing

# outersect <- function(x, y) {
#   sort(c(setdiff(x, y),
#          setdiff(y, x)))
# }
# 
# intersect(unique(ts_pulled$keyword), topics_and_keywords)
# outersect(unique(ts_pulled$keyword), topics_and_keywords)

# "Dateningenieur"         "Dateningenieurin"       "Datenwissenschaftler"   "Datenwissenschaftlerin" "Geschäftsintelligenz"

## German search terms 
## don't have enough data
## to show up in the results


# descriptive stats -------------------------------------------------------

ts %>% group_by(keyword) %>%
  summarise(hits_mean_over_time = mean(hits),
            hits_min = min(hits),
            hits_max = max(hits)) %>%
  arrange(desc(hits_mean_over_time))

ts$DataEcon <- ts$keyword

ts <- ts %>% mutate(
  DataEcon = keyword,
  DataEcon = case_when(
    DataEcon == "Data_Science_field_of_study" ~ "Data",
    DataEcon == "Business_intelligence_topic" ~ "Data",
    DataEcon == "Tableau_Software_topic" ~ "Data",
    DataEcon == "Deepl_learning_topic" ~ "Data",
    DataEcon == "data_science_search_term" ~ "Data",
    DataEcon == "r_for_data_science_search_term" ~ "Data",
    DataEcon == "python_for_data_science_search_term" ~ "Data",
    DataEcon == "data_engineering_search_term" ~ "Data",
    DataEcon == "big_data_search_term" ~ "Data",
    DataEcon == "data_mining_search_term" ~ "Data",
    DataEcon == "software_engineering_search_term" ~ "Data",
    DataEcon == "Economic_growth_topic" ~ "Economy",
    DataEcon == "Economic_crisis_topic" ~ "Economy",
    DataEcon == "Economic_recovery_topic" ~ "Economy",
    DataEcon == "investieren_search_term" ~ "Economy",
    DataEcon == "Kurzarbeit(short-time-work)_topic" ~ "Economy",
    DataEcon == "Unemployment_topic" ~ "Economy",
    DataEcon == "Insolvenz(bankruptcy)_search_term" ~ "Economy",
    TRUE ~ as.character(DataEcon)
  )
)

ts %>% group_by(keyword, DataEcon) %>%
  summarise(hits_mean_over_time = mean(hits),
            hits_min = min(hits),
            hits_max = max(hits)) %>%
  arrange(desc(hits_mean_over_time))

ts %>% group_by(DataEcon) %>%
  summarise(hits_mean_over_time = mean(hits),
            hits_min = min(hits),
            hits_max = max(hits)) %>%
  arrange(desc(hits_mean_over_time))


# save DFs ----------------------------------------------------------------
save(ts_raw, file = "data/ts_raw.RData")
save(ts, file = "data/ts_prepped.RData")
