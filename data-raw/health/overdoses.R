library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/health/overdose/"

overdose <- read_tsv(path %+% "overdose.txt")

overdose %<>%
  clean_wonder(method = "weight") %>%
  select(FIPS, year, od_rate = rate) %>%
  mutate(race = "total", sex = "total")

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

homicide <- wonder_time(path %+% "total")

homicide %<>%
  clean_wonder %>%
  process_mortality

homicide %<>% age_adj_rate("deaths", age_var = "age_10")

homicide %<>%
  rename(homicide_rate = deaths) %>%
  mutate(race = "total", sex = "total")

usethis::use_data(overdose, overwrite = TRUE)
usethis::use_data(homicide, overwrite = TRUE)

rm(homicide, overdose, process_mortality, path)
