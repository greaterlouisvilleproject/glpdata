library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/jobs/hh_income/"

process_00 <- function(df){
  df %<>%
    transmute(
      FIPS = as.numeric(Id2),
      year = 2000,
      
      hh_income = as.numeric(`Median household income in 1999`))
  
  df
}
process_05 <- function(df){
  df %<>%
    transmute(
      FIPS,
      year,
      hh_income = as.numeric(`Estimate; Median household income in the past 12 months (in 2005 inflation-adjusted dollars)`))
  
  df
}

process_income <- function(df, race_name){
  
  df %<>%
    
    #filter to peers
    pull_peers_FIPS(add_info = FALSE) %>%
    
    #gather data, separate sex into column, replace NAs with total, and spread
    
    #add race
    mutate(
      sex = "total",
      race = race_name) %>%
    
    #reorder variables
    select(
      FIPS, year, race, sex, hh_income) %>%
    
    #merge STL counties
    stl_merge(hh_income)
    
  df
}

# Total
hh_inc_00 <- read_csv(path %+% "DEC_00_SF3_P053_with_ann.csv", skip = 1)
hh_inc_05 <- acs_time(path %+% "B19013")
hh_income_map <- read_csv(path %+% "ACS_17_5YR_B19013_with_ann.csv", skip = 1)

hh_inc_00 %<>% process_00
hh_inc_05 %<>% process_05
hh_income_map %<>%
  transmute(
    Id,
    year = 2017,
    hh_inc = `Estimate; Median household income in the past 12 months (in 2017 inflation-adjusted dollars)`)

hh_inc_tot <- bind_rows(hh_inc_00, hh_inc_05)

hh_inc_tot %<>% process_income(race_name = "total")

#White
hh_inc_white_00 <- read_csv(path %+% "DEC_00_SF3_P152I_with_ann.csv", skip = 1)
hh_inc_white_05 <- acs_time(path %+% "B19013H")

hh_inc_white_00 %<>% process_00
hh_inc_white_05 %<>% process_05

hh_inc_white <- bind_rows(hh_inc_white_00, hh_inc_white_05)

hh_inc_white %<>% process_income(race_name = "white")


#Black
hh_inc_black_00 <- read_csv(path %+% "DEC_00_SF3_P152B_with_ann.csv", skip = 1)
hh_inc_black_05 <- acs_time(path %+% "B19013B")

hh_inc_black_00 %<>% process_00
hh_inc_black_05 %<>% process_05

hh_inc_black <- bind_rows(hh_inc_black_00, hh_inc_black_05)

hh_inc_black %<>% process_income(race_name = "black")


#Hispanic
hh_inc_hispanic_00 <- read_csv(path %+% "DEC_00_SF3_P152H_with_ann.csv", skip = 1)
hh_inc_hispanic_05 <- acs_time(path %+% "B19013I")

hh_inc_hispanic_00 %<>% process_00()
hh_inc_hispanic_05 %<>% process_05("FIPS")

hh_inc_hispanic <- bind_rows(hh_inc_hispanic_00, hh_inc_hispanic_05)

hh_inc_hispanic %<>% process_income(race_name = "hispanic")


#combine data frames
hh_income <- bind_rows(hh_inc_tot, hh_inc_white, hh_inc_black, hh_inc_hispanic)

hh_income %<>% COLA(hh_income)

usethis::use_data(hh_income, overwrite = TRUE)
usethis::use_data(hh_income_map, overwrite = TRUE)

rm(hh_inc_00, hh_inc_05, hh_inc_tot, process_income,
   hh_inc_white_00, hh_inc_white_05, hh_inc_white,
   hh_inc_black_00, hh_inc_black_05, hh_inc_black,
   hh_inc_hispanic_00, hh_inc_hispanic_05, hh_inc_hispanic,
   process_00, process_05, path)
