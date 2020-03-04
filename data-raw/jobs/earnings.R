library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(feather)
library(survey)

path <- "data-raw/jobs/earnings/"

# Functions to process the basic 2000 census and ACS files
process_00_all <- function(df){
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
process_00_ft <- function(df){
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
    mutate_at(vars(median_earnings_ft:median_earnings_ft.female), as.numeric) %>%
    pull_peers_FIPS(add_info = FALSE)
  df
}

process_05_all <- function(df, geog) {

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "MSA", "tract" = "Id")

  df %>%
    transmute(
      !!geog := .data[[col_name]],
      year,
      median_earnings =
        `Estimate; Total (dollars):`,
      median_earnings.male =
        `Estimate; Male -- - Total (dollars)`,
      median_earnings.female =
        `Estimate; Female -- - Total (dollars)`,
      median_earnings_ft.male =
        `Estimate; Male -- - Worked full-time, year-round in the past 12 months (dollars)`,
      median_earnings_ft.female =
        `Estimate; Female -- - Worked full-time, year-round in the past 12 months (dollars)`) %>%
    mutate_at(vars(median_earnings:median_earnings_ft.female), as.numeric)
}
process_18_all <- function(df, geog) {

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "MSA", "tract" = "Id")

  df %>%
    transmute(
      !!geog := .data[[col_name]],
      year,
      median_earnings =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Total (dollars)`,
      median_earnings.male =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Male --..Total (dollars)`,
      median_earnings.female =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Female --..Total (dollars)`,
      median_earnings_ft.male =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Male --..Worked full-time, year-round in the past 12 months (dollars)`,
      median_earnings_ft.female =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Female --..Worked full-time, year-round in the past 12 months (dollars)`) %>%
    mutate_at(vars(median_earnings:median_earnings_ft.female), as.numeric)
}
process_05_ft <- function(df){
  df %<>%
    transmute(
      FIPS, year,
      total = as.numeric(`Total; Estimate; Population 16 years and over with earnings`),
      median_earnings_ft =
        as.numeric(`Total; Estimate; Population 16 years and over with earnings - Median earnings (dollars)`))
  df
}
process_17_ft <- function(df){
  df %<>%
    transmute(
      FIPS, year,
      total = as.numeric(`Total; Estimate; Population 16 years and over with earnings`),
      median_earnings_ft =
        `Total; Estimate; Median earnings (dollars)`)
  df
}
process_msa <- function(df) {
  df %<>%
    rename(
      `Estimate; Total (dollars):` =
        `Estimate; Median earnings in the past 12 months (in 2010 inflation-adjusted dollars) -- - Total (dollars):`,
      `Estimate; Male -- - Total (dollars)` =
        `Estimate; Median earnings in the past 12 months (in 2010 inflation-adjusted dollars) -- - Male -- - Total (dollars)`,
      `Estimate; Female -- - Total (dollars)` =
        `Estimate; Median earnings in the past 12 months (in 2010 inflation-adjusted dollars) -- - Female -- - Total (dollars)`,
      `Estimate; Male -- - Worked full-time, year-round in the past 12 months (dollars)` =
        `Estimate; Median earnings in the past 12 months (in 2010 inflation-adjusted dollars) -- - Male -- - Worked full-time, year-round in the past 12 months (dollars)`,
      `Estimate; Female -- - Worked full-time, year-round in the past 12 months (dollars)` =
        `Estimate; Median earnings in the past 12 months (in 2010 inflation-adjusted dollars) -- - Female -- - Worked full-time, year-round in the past 12 months (dollars)`)
}

process_05_all_race <- function(df) {
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
process_18_all_race <- function(df) {
  df %<>%
    transmute(
      FIPS, year,
      median_earnings =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Total`,
      median_earnings.male =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Male --..Total`,
      median_earnings.female =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Female --..Total`,
      median_earnings_ft.male =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Male --..Worked full-time, year-round in the past 12 months`,
      median_earnings_ft.female =
        `Estimate..Median earnings in the past 12 months (in 2018 inflation-adjusted dollars) --..Female --..Worked full-time, year-round in the past 12 months`) %>%
    mutate_at(vars(median_earnings:median_earnings_ft.female), as.numeric)
}

# Supplemental functions to process additional tables

# Process final data
process_earn <- function(df, race_name) {

  if (df_type(df) == "MSA") geog <- "MSA" else geog <- "FIPS"

  df %>%
    #filter to peers
    pull_peers(add_info = FALSE) %>%

    #gather data, separate sex into column, replace NAs with total, and spread
    reshape_sex() %>%

    #add race
    mutate(race = race_name) %>%

    #reorder variables
    select(!!geog, year, race, sex, median_earnings, median_earnings_ft) %>%

    #merge STL counties
    {if (geog == "FIPS") stl_merge(., median_earnings:median_earnings_ft) else .}
}

# Total

earnings_00_total <- build_census_var_df("sf3", "P85")
earnings_05_total <- build_census_var_df("acs1", "B20017")
  filter(str_detect(label, "Total"))

earnings_05_total <- get_census(earnings_05_total, "FIPS", T)

test <- ranking_data(earnings_05_total, var, years = 2005:2018)

### Read in data
earnings_00_all <- read_csv(path %p% "DEC_00_SF3_P085_with_ann.csv", skip = 1)
earnings_00_ft <- read_csv(path %p% "DEC_00_SF3_PCT047_with_ann.csv", skip = 1)

earnings_05_all <- acs_time(path %p% "B20017/05")
earnings_18_all <- acs_time(path %p% "B20017/18", starting_year = 2018)
earnings_05_ft <- acs_time(path %p% "S2001/05")
earnings_17_ft <- acs_time(path %p% "S2001/17", starting_year = 2017)

earnings_msa_1yr <- acs_time(path %p% "MSA/B20017", geog = "MSA", starting_year = 2010)

### Process data
earnings_00_all %<>% process_00_all()
earnings_00_ft  %<>% process_00_ft()

earnings_05_all %<>% process_05_all("FIPS")
earnings_18_all %<>% process_18_all("FIPS")
earnings_05_ft  %<>% process_05_ft()
earnings_17_ft  %<>% process_17_ft()

earnings_msa_1yr %<>% process_msa() %>% process_05_all("MSA")

### Combine data frames
earnings_00 <- bind_df(earnings_00_all, earnings_00_ft)

earnings_05_17_ft <- bind_rows(earnings_05_ft, earnings_17_ft)
earnings_05 <- bind_rows(earnings_05_all, earnings_18_all) %>% bind_df(earnings_05_17_ft)

earnings_total <- bind_rows(earnings_00, earnings_05) %>% select(-total)

earnings_total   %<>% process_earn("total")
earnings_msa_1yr %<>% process_earn("total")

# Race

### White
earnings_00_white <- read_csv(path %p% "DEC_00_SF3_PCT074H_with_ann.csv", skip = 1)
earnings_05_white <- acs_time(path %p% "B20017H/05")
earnings_18_white <- acs_time(path %p% "B20017H/18", starting_year = 2018)

earnings_00_white %<>% process_00_ft()
earnings_05_white %<>% process_05_all_race()
earnings_18_white %<>% process_18_all_race()

earnings_white <- bind_rows(earnings_00_white, earnings_05_white, earnings_18_white)

earnings_white %<>% process_earn("white")

### Black
earnings_00_black <- read_csv(path %p% "DEC_00_SF3_PCT074B_with_ann.csv", skip = 1)
earnings_05_black <- acs_time(path %p% "B20017B/05")
earnings_18_black <- acs_time(path %p% "B20017B/18", starting_year = 2018)

earnings_00_black %<>% process_00_ft()
earnings_05_black %<>% process_05_all_race()
earnings_18_black %<>% process_18_all_race()

earnings_black <- bind_rows(earnings_00_black, earnings_05_black, earnings_18_black)

earnings_black %<>% process_earn("black")

### Hispanic
earnings_00_hispanic <- read_csv(path %p% "DEC_00_SF3_PCT074H_with_ann.csv", skip = 1)
earnings_05_hispanic <- acs_time(path %p% "B20017I/05")
earnings_18_hispanic <- acs_time(path %p% "B20017I/18", starting_year = 2018)

earnings_00_hispanic %<>% process_00_ft()
earnings_05_hispanic %<>% process_05_all_race()
earnings_18_hispanic %<>% process_18_all_race()

earnings_hispanic <- bind_rows(earnings_00_hispanic, earnings_05_hispanic, earnings_18_hispanic)

earnings_hispanic %<>% process_earn("hispanic")

earnings_race <- bind_rows(earnings_white, earnings_black, earnings_hispanic)

# Combine and process data frames

earnings_county <- bind_rows(earnings_total, earnings_race)

# Process data frame
earnings_county %<>% organize()

earnings_county  %<>% COLA(median_earnings:median_earnings_ft)
earnings_msa_1yr %<>% COLA(median_earnings:median_earnings_ft)

earnings_gap <- earnings_county %>%
  filter(sex == "total") %>%
  select(-median_earnings_ft) %>%
  spread(key = race, value = median_earnings) %>%
  transmute(
    FIPS, year, sex,
    race = "total",
    earnings_gap_wb = white - black)

earnings_county %<>% bind_df(earnings_gap)

# Map
earnings_map <- read_csv(path %p% "ACS_17_5YR_S2001_with_ann.csv", skip = 1)

earnings_tract <- earnings_map %>%
  transmute(
    tract = Id,
    year = 2015,
    median_earnings = as.numeric(`Total; Estimate; Median earnings (dollars)`))

earnings_nh <- earnings_map %>%
  left_join(nh_tract, by = c("Id" = "GEO_ID")) %>%
  group_by(neighborhood) %>%
  mutate(
    total = `Total; Estimate; Population 16 years and over with earnings`,
    median_earnings = as.numeric(`Total; Estimate; Median earnings (dollars)`)) %>%
  filter(neighborhood != "Airport") %>%
  summarise(
    year = 2015,
    median_earnings = Hmisc::wtd.quantile(median_earnings, total, na.rm = T, probs = .5)) %>%
  ungroup()

earnings_nh[nrow(earnings_nh) + 1,] <- list("Airport", 2015, NA)

update_sysdata(earnings_county, earnings_msa_1yr, earnings_tract, earnings_nh)

rm(earnings_total, process_msa,
   earnings_00, earnings_00_all, earnings_00_ft,
   earnings_05, earnings_05_all, earnings_05_ft,
   earnings_17_ft, earnings_05_17_ft, earnings_18_all,
   earnings_00_white, earnings_05_white, earnings_18_white, earnings_white,
   earnings_00_black, earnings_05_black, earnings_18_black, earnings_black,
   earnings_00_hispanic, earnings_05_hispanic, earnings_18_hispanic, earnings_hispanic,
   earnings_race, earnings_gap, earnings_map,
   process_00_ft, process_00_all, process_05_all_race, process_05_ft, process_18_all,
   process_18_all_race, process_05_all, process_17_ft, process_earn, path)
