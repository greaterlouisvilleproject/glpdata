library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(survey)
library(feather)

path <- "data-raw/education/high_wage/"

acs_micro <- read_feather("data-raw/microdata/acs_micro.feather")

hw_2000 <- c(
  #Management, business, science, and arts occupations
  1:354,
  #Law enforcement supervisors
  370, 371,
  #Law enforcement workers
  380:385,
  #Installation, maintenance, and repair occupations
  700:762)

hw_acs <- c(
  #Management, business, science, and arts occupations
  10:3655, 4465, 3945,
  #Law enforcement supervisors
  3700, 3710,
  #Law enforcement workers
  3800:3850,
  #Installation, maintenance, and repair occupations
  6540, 7000:7630)

acs_micro %<>%
  mutate(
    high_wage = if_else(OCC %in% hw_2000 & year == 2000, 1, 0),
    high_wage = replace(high_wage, OCC %in% hw_acs & year >= 2005, 1),
    high_wage = replace(high_wage, OCC == 0, NA))

high_wage_county  <- svy_race_sex(acs_micro, high_wage)
high_wage_msa_1yr <- svy_race_sex(acs_micro, high_wage, geog = "MSA")

high_wage_msa_1yr %<>% mutate(MSA = as.character(MSA))

# Map
high_wage_map <- read_csv(path %p% "ACS_17_5YR_S2401_with_ann.csv", skip = 1)

high_wage_tract <- high_wage_map %>%
  transmute(
    tract = Id,
    year = 2015,
    high_wage =
      (`Total; Estimate; Management, business, science, and arts occupations:` +
      `Total; Estimate; Service occupations: - Protective service occupations: - Law enforcement workers including supervisors` +
      `Total; Estimate; Natural resources, construction, and maintenance occupations: - Construction and extraction occupations`) /
      `Total; Estimate; Civilian employed population 16 years and over` * 100)

high_wage_nh <- high_wage_map %>%
  left_join(nh_tract, by = c("Id" = "GEO_ID")) %>%
  group_by(neighborhood) %>%
  summarise(
    year = 2015,
    high_wage =
      sum(`Total; Estimate; Management, business, science, and arts occupations:` +
          `Total; Estimate; Service occupations: - Protective service occupations: - Law enforcement workers including supervisors` +
          `Total; Estimate; Natural resources, construction, and maintenance occupations: - Construction and extraction occupations`) /
      sum(`Total; Estimate; Civilian employed population 16 years and over`) * 100)

update_sysdata(high_wage_county, high_wage_tract, high_wage_nh, high_wage_msa_1yr)

rm(hw_2000, hw_acs, acs_micro, path, high_wage_map)
