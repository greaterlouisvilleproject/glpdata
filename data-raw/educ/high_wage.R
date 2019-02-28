library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(survey)
library(feather)

path <- "data-raw/educ/high_wage/"

if("high_wage.feather" %!in% list.files(path)){
  acs <- read_csv(path %p% "usa_00055.csv")
  write_feather(acs, path %p% "high_wage.feather")
} else{
  acs <- read_feather(path %p% "high_wage.feather")
}

acs %<>% process_microdata()

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

acs %<>%
  mutate(
    high_wage = if_else(OCC %in% hw_2000 & year == 2000, 1, 0),
    high_wage = replace(high_wage, OCC %in% hw_acs & year >= 2005, 1),
    high_wage = replace(high_wage, OCC == 0, NA))

survey <- svydesign(ids = ~1, weights = ~PERWT, data = acs)

high_wage <- svy_race_sex(survey, high_wage, cross = FALSE)

usethis::use_data(high_wage, overwrite = TRUE)

rm(hw_2000, hw_acs, acs, survey, path, check)
