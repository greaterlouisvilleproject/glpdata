library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/commute/"

acs_micro <- feather::read_feather("data-raw/microdata/acs_micro.feather")

map_time       <- read_csv(path %p% "B08012_tract_2018.csv", skip = 1)
map_aggregate  <- read_csv(path %p% "B08013_tract_2018.csv", skip = 1)

acs_micro %<>%
  filter(EMPSTAT == 1) %>%
  mutate(
    MSA = as.character(MSA),
    commute = replace(TRANTIME, TRANTIME == 0, NA))

commute_county  <- svy_race_sex(acs_micro, "commute")
commute_msa_1yr <- svy_race_sex(acs_micro, "commute", geog = "MSA")

map_time %<>%
  rename_all(str_replace_all, "!!", ".") %>%
  transmute(
    tract = id,
    year = 2016,
    total = Estimate.Total,
    commute_0_5   = `Estimate.Total.Less than 5 minutes`,
    commute_5_9   = `Estimate.Total.5 to 9 minutes`,
    commute_10_14 = `Estimate.Total.10 to 14 minutes`,
    commute_15_19 = `Estimate.Total.15 to 19 minutes`,
    commute_20_24 = `Estimate.Total.20 to 24 minutes`,
    commute_25_29 = `Estimate.Total.25 to 29 minutes`,
    commute_30_34 = `Estimate.Total.30 to 34 minutes`,
    commute_35_39 = `Estimate.Total.35 to 39 minutes`,
    commute_40_44 = `Estimate.Total.40 to 44 minutes`,
    commute_45_59 = `Estimate.Total.45 to 59 minutes`,
    commute_60_89 = `Estimate.Total.60 to 89 minutes`,
    commute_90 = `Estimate.Total.90 or more minutes`)

map_aggregate %<>%
  rename_all(str_replace_all, "!!", ".") %>%
  transmute(
    tract= id,
    year = 2016,
    aggregate_time = as.numeric(`Estimate.Aggregate travel time to work (in minutes)`))

commute_map <- bind_df(map_time, map_aggregate)

commute_map %<>%
  transmute(
    tract, year,
    total,
    commute = aggregate_time / total)

process_map(commute_map, "commute", "total", "commute", "mean") %>%
  list2env(.GlobalEnv)

update_sysdata(commute_county, commute_msa_1yr,
               commute_tract, commute_nh, commute_muw)

rm(acs_micro, commute_map, map_aggregate, map_time, path)
