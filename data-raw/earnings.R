library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/jobs/earnings/"

#Functions to process the basic 2000 census and ACS files
process_00 <- function(df){
  df %<>%
    transmute(
      FIPS = as.numeric(Id2),
      year = 2000,

      median_earnings_ft =
        `Worked full-time, year-round in 1999 -- - Total`,
      median_earnings_ft.male =
        `Worked full-time, year-round in 1999 -- - Male`,
      median_earnings_ft.female =
        `Worked full-time, year-round in 1999 -- - Female`) %>%
    mutate_at(vars(median_earnings_ft:median_earnings_ft.female), as.numeric)
  df
}

process_05_total <- function(df) {
  df %<>%
    transmute(
      FIPS, year,
      median_earnings =
        `Estimate; Total (dollars):`,
      median_earnings.male =
        `Estimate; Male -- - Total (dollars)`,
      median_earnings.female =
        `Estimate; Female -- - Total (dollars)`,
      median_earnings_ft.male =
        `Estimate; Male -- - Worked full-time, year-round in the past 12 months (dollars)`,
      median_earnings_ft.female =
        `Estimate; Female -- - Worked full-time, year-round in the past 12 months (dollars)`)
}

process_05_race <- function(df) {
  df %<>%
    transmute(
      FIPS, year,
      median_earnings =
        `Estimate; Total:`,
      median_earnings.male =
        `Estimate; Male -- - Total`,
      median_earnings.female =
        `Estimate; Female -- - Total`,
      median_earnings_ft.male =
        `Estimate; Male -- - Worked full-time, year-round in the past 12 months`,
      median_earnings_ft.female =
        `Estimate; Female -- - Worked full-time, year-round in the past 12 months`) %>%
    mutate_at(vars(median_earnings:median_earnings_ft.female), as.numeric)
}

#Supplemental functions to process additional 2000 tables
process_00_supp <- function(df){
  df %<>%
    transmute(
      FIPS = as.numeric(Id2),
      year = 2000,

      median_earnings = Total,
      median_earnings.male = Male,
      median_earnings.female = Female) %>%
    pull_peers_FIPS(add_info = FALSE)
  df
}

process_05_supp <- function(df){
  df %<>%
    transmute(
      FIPS, year,
      median_earnings_ft =
        as.numeric(`Total; Estimate; Population 16 years and over with earnings - Median earnings (dollars)`))
  df
}

process_17_supp <- function(df){
  df %<>%
    transmute(
      FIPS, year,
      median_earnings_ft =
        `Total; Estimate; Median earnings (dollars)`)
  df
}

#process final data
process_earn <- function(df, race_name) {
  df <- df %>%

    #filter to peers
    pull_peers_FIPS(add_info = FALSE) %>%

    #gather data, separate sex into column, replace NAs with total, and spread
    reshape_sex() %>%

    #add race
    mutate(race = race_name) %>%

    #reorder variables
    select(
      FIPS, year, race, sex, median_earnings, median_earnings_ft) %>%

    #merge STL counties
    stl_merge(median_earnings:median_earnings_ft)

  df
}

#Total

### Read in data
earnings_00_all <- read_csv(path %+% "DEC_00_SF3_P085_with_ann.csv", skip = 1)
earnings_05_ft <- acs_time(path %+% "S2001/05")
earnings_17_ft <- acs_time(path %+% "S2001/17", starting_year = 2017)

earnings_00 <- read_csv(path %+% "DEC_00_SF3_PCT047_with_ann.csv", skip = 1)
earnings_05 <- acs_time(path %+% "B20017")

### Process data
earnings_00_all %<>% process_00_supp()
earnings_05_ft %<>% process_05_supp()
earnings_17_ft %<>% process_17_supp()

earnings_00 %<>% process_00()
earnings_05 %<>% process_05_total()

### Combine datasets
earnings_00 <- left_join(earnings_00_all, earnings_00)

earnings_05_17_ft <- bind_rows(earnings_05_ft, earnings_17_ft)
earnings_05 <- full_join(earnings_05, earnings_05_17_ft)

earnings_total <- bind_rows(earnings_00, earnings_05)

earnings_total %<>% process_earn("total")


#White
earnings_00_white <- read_csv(path %+% "DEC_00_SF3_PCT074H_with_ann.csv", skip = 1)
earnings_05_white <- acs_time(path %+% "B20017I")

earnings_00_white %<>% process_00()
earnings_05_white %<>% process_05_race()

earnings_white <- bind_rows(earnings_00_white, earnings_05_white)

earnings_white %<>% process_earn("white")


#Black
earnings_00_black <- read_csv(path %+% "DEC_00_SF3_PCT074B_with_ann.csv", skip = 1)
earnings_05_black <- acs_time(path %+% "B20017B")

earnings_00_black %<>% process_00()
earnings_05_black %<>% process_05_race()

earnings_black <- bind_rows(earnings_00_black, earnings_05_black)

earnings_black %<>% process_earn("black")


#Hispanic
earnings_00_hispanic <- read_csv(path %+% "DEC_00_SF3_PCT074H_with_ann.csv", skip = 1)
earnings_05_hispanic <- acs_time(path %+% "B20017I")

earnings_00_hispanic %<>% process_00()
earnings_05_hispanic %<>% process_05_race()

earnings_hispanic <- bind_rows(earnings_00_hispanic, earnings_05_hispanic)

earnings_hispanic %<>% process_earn("hispanic")


#Combine data frames
earnings <- bind_rows(earnings_total, earnings_white, earnings_black, earnings_hispanic)

earnings %<>% organize()

earnings %<>% COLA(median_earnings:median_earnings_ft)

usethis::use_data(earnings, overwrite = TRUE)

rm(earnings, earnings_total, earnings_00, earnings_05, earnings_00_all,
   earnings_05_17_ft, earnings_05_ft, earnings_17_ft,
   earnings_00_white, earnings_05_white, earnings_white,
   earnings_00_black, earnings_05_black, earnings_black,
   earnings_00_hispanic, earnings_05_hispanic, earnings_hispanic,
   process_00, process_00_supp, process_05_race, process_05_supp,
   process_05_total, process_17_supp, process_earn, path)
