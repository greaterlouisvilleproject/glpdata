library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(survey)
library(feather)

path <- "data-raw/qop/housing/"

acs_micro <- read_feather("data-raw/microdata/acs_micro.feather")

acs_micro %<>%
  group_by(year, SERIAL) %>%
  mutate(hh_members = n()) %>%
  ungroup() %>%
  filter(PERNUM == 1) %>%
  mutate(
    MSA = as.character(MSA),

    OWNCOST  = replace(OWNCOST, OWNCOST == 99999, NA),
    HHINCOME = replace(HHINCOME, HHINCOME == 99999, NA),
    OWNERSHP = replace(OWNERSHP, OWNERSHP == 0, NA),
    RENTGRS  = replace(RENTGRS, RENTGRS == 0 & OWNERSHP == 1, NA),

    homeownership = if_else(OWNERSHP == 1, 1, 0),

    hcost = if_else(homeownership == 1, OWNCOST, RENTGRS),
    cost_burden = if_else(hcost * 12 / HHINCOME > 0.3, 1, 0),
    severe_cost_burden = if_else(hcost * 12 / HHINCOME > 0.5, 1, 0),

    hh_type = case_when(
      homeownership == 1 & cost_burden == 0 ~ "noncb_homeowner",
      homeownership == 1 & cost_burden == 1 ~ "cb_homeowner",
      homeownership == 0 & cost_burden == 0 ~ "noncb_renter",
      homeownership == 0 & cost_burden == 1 ~ "cb_renter",
      TRUE ~ NA_character_),


    KITCHEN  = replace(KITCHEN, KITCHEN == 0, NA),
    ROOMS    = replace(ROOMS, ROOMS == 0, NA),
    PLUMBING = replace(PLUMBING, PLUMBING == 0, NA),

    severe_housing_problems = if_else(
      KITCHEN == 1 | PLUMBING == 10 | hh_members / ROOMS > 1 | severe_cost_burden, 1, 0))

# Homeownership and Cost-burden
housing_county  <- svy_race_sex_cat(acs_micro, "hh_type", "HHWT")
housing_msa_1yr <- svy_race_sex_cat(acs_micro, "hh_type", "HHWT", geog = "MSA")

housing_county %<>%
  mutate(
    homeownership = cb_homeowner + noncb_homeowner,
    cost_burdened = cb_homeowner + cb_renter)

housing_msa_1yr %<>%
  mutate(
    homeownership = cb_homeowner + noncb_homeowner,
    cost_burdened = cb_homeowner + cb_renter)

homeownership_map <- read_csv(path %p% "ACS_17_5YR_B25003_with_ann.csv", skip = 1)

homeownership_map %>%
  transmute(
    tract = Id,
    year = 2015,
    total = `Estimate; Total:`,
    homeownership = `Estimate; Total: - Owner occupied`) %>%
  process_map("homeownership", "total", return_name = "homeownership") %>%
  list2env(.GlobalEnv)

burdened_map <- read_csv(path %p% "ACS_17_5YR_B25106_with_ann.csv", skip = 1)

burdened_map %>%
  transmute(
    tract = Id,
    year = 2015,
    total = `Estimate; Total:`,
    cost_burdened =
      `Estimate; Owner-occupied housing units: - Less than $20,000: - 30 percent or more` +
      `Estimate; Owner-occupied housing units: - $20,000 to $34,999: - 30 percent or more` +
      `Estimate; Owner-occupied housing units: - $35,000 to $49,999: - 30 percent or more` +
      `Estimate; Owner-occupied housing units: - $50,000 to $74,999: - 30 percent or more` +
      `Estimate; Owner-occupied housing units: - $75,000 or more: - 30 percent or more` +
      `Estimate; Owner-occupied housing units: - Zero or negative income` +
      `Estimate; Renter-occupied housing units: - Less than $20,000: - 30 percent or more` +
      `Estimate; Renter-occupied housing units: - $20,000 to $34,999: - 30 percent or more` +
      `Estimate; Renter-occupied housing units: - $35,000 to $49,999: - 30 percent or more` +
      `Estimate; Renter-occupied housing units: - $50,000 to $74,999: - 30 percent or more` +
      `Estimate; Renter-occupied housing units: - $75,000 or more: - 30 percent or more` +
      `Estimate; Renter-occupied housing units: - Zero or negative income`) %>%
  process_map("cost_burdened", "total", return_name = "burdened") %>%
  list2env(.GlobalEnv)

# Severe Housing Problems
housing_problems_county <- svy_race_sex(acs_micro, severe_housing_problems, "HHWT")
housing_problems_msa    <- svy_race_sex(acs_micro, severe_housing_problems, "HHWT", geog = "MSA")

housing_problems_county %<>% mutate(severe_housing_problems = severe_housing_problems * 100)
housing_problems_msa    %<>% mutate(severe_housing_problems = severe_housing_problems * 100)

housing_tract   <- bind_df(homeownership_tract, burdened_tract)
housing_nh      <- bind_df(homeownership_nh,    burdened_nh)
housing_muw     <- bind_df(homeownership_muw,   burdened_muw)
housing_county  %<>% bind_df(housing_problems_county)
housing_msa_1yr %<>% bind_df(housing_problems_msa)

update_sysdata(housing_county, housing_msa_1yr, housing_tract, housing_nh, housing_muw)

rm(acs_micro, housing_problems_county, housing_problems_msa,
   homeownership_map, burdened_map, path)


