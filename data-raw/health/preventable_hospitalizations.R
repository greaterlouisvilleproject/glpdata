library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/health/preventable_hospitalizations/"

process_mmd <- function(df, race = "total", sex = "total"){
  df %>%
    transmute(
      FIPS = str_pad(fips, 5, "left", "0"),
      year,
      preventable_hospitalizations = analysis_value) %>%
    mutate(race = race, sex = sex) %>%
    pull_peers() %>%
    stl_merge(preventable_hospitalizations)
}

all <- any_time(path %p% "all/", starting_year = 2012, col_types = "ncccccccccccccccn")
white <- any_time(path %p% "white/", starting_year = 2012, col_types = "ncccccccccccccccn")
black <- any_time(path %p% "black/", starting_year = 2012, col_types = "ncccccccccccccccn")
male <- any_time(path %p% "male/", starting_year = 2012, col_types = "ncccccccccccccccn")
female <- any_time(path %p% "female/", starting_year = 2012, col_types = "ncccccccccccccccn")

all %<>% process_mmd()
white %<>% process_mmd(race = "white")
black %<>% process_mmd(race = "black")
male %<>% process_mmd(sex = "male")
female %<>% process_mmd(sex = "female")

preventable_hospitalizations_county <- bind_rows(all, white, black, male, female)

update_sysdata(preventable_hospitalizations_county)

rm(all, black, white, male, female, process_mmd, path)
