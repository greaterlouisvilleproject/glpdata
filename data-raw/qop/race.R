library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/race/"

process_race <- function(df, geog) {
 df %<>%
    select(
      !!geog,
      year,
      total = `Estimate; Total:`,
      white = `Estimate; Not Hispanic or Latino: - White alone`,
      black = `Estimate; Not Hispanic or Latino: - Black or African American alone`,
      hispanic = `Estimate; Hispanic or Latino:`,
      asian = `Estimate; Not Hispanic or Latino: - Asian alone`)
}

race_county <- acs_time(path %p% "B03002")
race_map <- read_csv(path %p% "ACS_17_5YR_B03002_with_ann.csv", skip = 1)

race_county %<>% process_race("FIPS")
race_map %<>%
  mutate(year = 2015) %>%
  process_race("Id")

race_county %<>%
  stl_merge(total:asian, method = "sum") %>%
  mutate_at(vars(white, black, hispanic, asian), ~ . / total * 100) %>%
  select(-total) %>%
  gather(-FIPS, -year, key = "race", value = "pct_race") %>%
  mutate(sex = "total") %>%
  organize()

race_tract <- race_map %>%
  mutate_at(vars(white, black, hispanic, asian), ~ . / total * 100) %>%
  select(-total)

race_nh <- race_map %>%
  left_join(nh_tract, by = c("Id" = "GEO_ID")) %>%
  group_by(neighborhood) %>%
  summarise_at(vars(white, black, hispanic, asian), ~ sum(.) / sum(total) * 100)

update_sysdata(race_county, race_tract, race_nh)

rm(process_race, race_map, path)
