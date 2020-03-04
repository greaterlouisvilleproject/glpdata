library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

source("data-raw/helpers/process_mortality.R")
source("data-raw/helpers/process_health_index.R")

# Overdoses

path <- "data-raw/health/overdose/"

mort_all <- wonder_time(path %p% "all")
mort_non_drug <- wonder_time(path %p% "non_drug")

mort_all %<>%
  clean_wonder() %>%
  rename(deaths_all = deaths)

mort_non_drug %<>%
  clean_wonder() %>%
  rename(deaths_non_drug = deaths)

od_ypll <- full_join(mort_all, mort_non_drug, by = c("year", "age", "FIPS", "population"))

od_ypll %<>%
  mutate(deaths = deaths_all - deaths_non_drug) %>%
  process_mortality() %>%
  rename(ypll_od = ypll) %>%
  mutate(sex = "total", race = "total")


# Homicide

path <- "data-raw/health/homicide/"

mort_ch <- wonder_time(path %p% "cancer_homicide")
mort_c <- wonder_time(path %p% "cancer")

mort_ch %<>%
  clean_wonder() %>%
  rename(deaths_all = deaths)

mort_c %<>%
  clean_wonder() %>%
  rename(deaths_cancer = deaths)

homicide_ypll <- full_join(mort_ch, mort_c, by = c("year", "age", "FIPS", "population"))

homicide_ypll %<>%
  mutate(deaths = deaths_all - deaths_cancer) %>%
  process_mortality() %>%
  rename(ypll_homicide = ypll) %>%
  mutate(sex = "total", race = "total")


# Create new analysis
load("R/sysdata.rda")

ypll_analysis <- bind_df(mortality_county, od_ypll, homicide_ypll) %>%
  mutate(
    ypll_od_adj = ypll - ypll_od,
    ypll_h_adj = ypll - ypll_homicide,
    ypll_od_h_adj = ypll - ypll_od - ypll_homicide) %>%
  select(-ypll)

path <- "data-raw/health/"

if (!exists("natality_county"))   source(path %p% "natality.R")
if (!exists("brfss_msa_1yr"))     source(path %p% "brfss.R")

mortality_adj  <- ypll_analysis %>%
  select(FIPS, year, sex, race, ypll = ypll_od_h_adj) %>%
  filter(race == "total", sex == "total")

health_index_adj_county <- process_health_index(mortality_adj, natality_county, brfss_msa_1yr)

health_index_adj_county %<>%
  select(FIPS, year, sex, race, health_index_adj = health_index) %>%
  bind_df(ypll_analysis) %>%
  filter(!is.na(health_index_adj))

update_sysdata(health_index_adj_county)

