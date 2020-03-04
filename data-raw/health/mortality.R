library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

source("data-raw/helpers/process_mortality.R")

path <- "data-raw/health/mortality/"

# Total
mort_total <- wonder_time(path %p% "total")

mort_total %<>%
  clean_wonder() %>%
  process_mortality() %>%
  mutate(
    sex = "total",
    race = "total") %>%
  organize()

# Sex
mort_sex <- wonder_time(path %p% "sex")

mort_sex %<>%
  clean_wonder() %>%
  process_mortality() %>%
  mutate(race = "total")

# Race
mort_bw <- wonder_time(path %p% "bw")
mort_h <- wonder_time(path %p% "h")

mort_bw %<>%
  clean_wonder() %>%
  process_mortality()

mort_h %<>%
  clean_wonder() %>%
  mutate(race = "hispanic") %>%
  process_mortality()

mort_race <- bind_rows(mort_bw, mort_h) %>%
  mutate(sex = "total") %>%
  organize()

mortality_county <- bind_rows(mort_total, mort_sex, mort_race)

mortality_county %<>% organize()

update_sysdata(mortality_county)

rm(mort_total, mort_sex, mort_bw, mort_h, mort_race, process_mortality, path)

