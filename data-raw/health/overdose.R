library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/health/overdose/"

overdose_county <- read_tsv(path %p% "overdose.txt")

overdose_county %<>%
  clean_wonder(method = "weight") %>%
  select(FIPS, year, od_rate = rate) %>%
  mutate(race = "total", sex = "total") %>%
  organize()


# Redo regular OD analysis? maybe

# YPLL calculation

#test <- bind_df(overdose, homicide) %>% filter(!(is.na(od_rate) & is.na(homicide_rate)))

#cor(test$od_rate, test$homicide_rate, na.rm = T)
# analysis...make health index a function that is exported to health index and this alternate analysis.
# Separate alternate OD work from this doc? yes.


# do magic


path <- "data-raw/health/homicide/"

process_mortality <- function(df, race_name){
  #Extract data for 10 most populous counties
  totals <- df %>%
    filter(FIPS ==  "total") %>%
    select(-FIPS) %>%
    rename(
      total_deaths = deaths,
      total_population = population)

  #append data to df and calculate homicide rate
  df %<>%
    filter(FIPS != "total") %>%
    left_join(totals, by = c("year", "age_10")) %>%
    mutate(
      deaths = deaths - total_deaths,
      population = population - total_population) %>%
    select(-total_deaths, -total_population)

  df
}

homicide_county <- wonder_time(path %p% "total")

homicide_county %<>%
  clean_wonder %>%
  process_mortality()

homicide_county %<>% age_adj_rate("deaths", age_var = "age_10")

homicide_county %<>%
  rename(homicide_rate = deaths) %>%
  mutate(race = "total", sex = "total") %>%
  organize()

update_sysdata(overdose_county, homicide_county)

rm(process_mortality, path)
