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
      FIPS = as.character(as.numeric(Id2)),
      year = 2000,

      hh_income = as.numeric(`Median household income in 1999`))

  df
}
process_05 <- function(df,geog){

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "MSA", "tract" = "Id")

  df %<>%
    transmute(
      !!geog := .data[[col_name]],
      year,
      hh_income = as.numeric(`Estimate; Median household income in the past 12 months (in 2005 inflation-adjusted dollars)`))

  df
}
process_income <- function(df, race_name){

  if (df_type(df) == "MSA") geog <- "MSA" else geog <- "FIPS"

  df %>%

    #filter to peers
    pull_peers(add_info = FALSE) %>%

    #gather data, separate sex into column, replace NAs with total, and spread

    #add race
    mutate(
      sex = "total",
      race = race_name) %>%

    #reorder variables
    select(
      !!geog, year, race, sex, hh_income) %>%

    #merge STL counties
    {if (geog == "FIPS") stl_merge(., hh_income) else .}
}
process_msa <- function(df) {
  df %>%
    rename(
      `Estimate; Median household income in the past 12 months (in 2005 inflation-adjusted dollars)` =
        `Estimate; Median household income in the past 12 months (in 2010 inflation-adjusted dollars)`)
}

# Total
hh_inc_00         <- read_csv(path %p% "DEC_00_SF3_P053_with_ann.csv", skip = 1)
hh_inc_05         <- acs_time(path %p% "B19013")
hh_income_map     <- read_csv(path %p% "ACS_17_5YR_S1901_with_ann.csv", skip = 1)
hh_income_msa_1yr <- acs_time(path %p% "MSA/B19013", geography = "MSA", starting_year = 2010)

hh_inc_00         %<>% process_00()
hh_inc_05         %<>% process_05("FIPS")
hh_income_msa_1yr %<>% process_msa() %>% process_05("MSA")

hh_inc_tot <- bind_rows(hh_inc_00, hh_inc_05)

hh_inc_tot        %<>% process_income(race_name = "total")
hh_income_msa_1yr %<>% process_income(race_name = "total")

hh_income_tract <- hh_income_map %>%
  transmute(
    tract = Id,
    year = 2015,
    hh_income = as.numeric(`Households; Estimate; Median income (dollars)`))

hh_income_nh <- hh_income_map %>%
  left_join(nh_tract, by = c("Id" = "GEO_ID")) %>%
  group_by(neighborhood) %>%
  mutate(
    total = `Households; Estimate; Total`,
    hh_income = as.numeric(`Households; Estimate; Median income (dollars)`)) %>%
  filter(neighborhood != "Airport") %>%
  summarise(
    year = 2015,
    hh_income = Hmisc::wtd.quantile(hh_income, total, na.rm = T, probs = .5)) %>%
  ungroup()

hh_income_nh[nrow(hh_income_nh) + 1,] <- list("Airport", 2015, NA)

# White
hh_inc_white_00 <- read_csv(path %p% "DEC_00_SF3_P152I_with_ann.csv", skip = 1)
hh_inc_white_05 <- acs_time(path %p% "B19013H")

hh_inc_white_00 %<>% process_00()
hh_inc_white_05 %<>% process_05("FIPS")

hh_inc_white <- bind_rows(hh_inc_white_00, hh_inc_white_05)

hh_inc_white %<>% process_income(race_name = "white")


# Black
hh_inc_black_00 <- read_csv(path %p% "DEC_00_SF3_P152B_with_ann.csv", skip = 1)
hh_inc_black_05 <- acs_time(path %p% "B19013B")

hh_inc_black_00 %<>% process_00()
hh_inc_black_05 %<>% process_05("FIPS")

hh_inc_black <- bind_rows(hh_inc_black_00, hh_inc_black_05)

hh_inc_black %<>% process_income(race_name = "black")


# Hispanic
hh_inc_hispanic_00 <- read_csv(path %p% "DEC_00_SF3_P152H_with_ann.csv", skip = 1)
hh_inc_hispanic_05 <- acs_time(path %p% "B19013I")

hh_inc_hispanic_00 %<>% process_00()
hh_inc_hispanic_05 %<>% process_05("FIPS")

hh_inc_hispanic <- bind_rows(hh_inc_hispanic_00, hh_inc_hispanic_05)

hh_inc_hispanic %<>% process_income(race_name = "hispanic")


# Combine data frames
hh_income_county <- bind_rows(hh_inc_tot, hh_inc_white, hh_inc_black, hh_inc_hispanic)

hh_income_county  %<>% COLA(hh_income)
hh_income_msa_1yr %<>% COLA(hh_income)

update_sysdata(hh_income_county, hh_income_tract, hh_income_nh, hh_income_msa_1yr)

rm(hh_inc_00, hh_inc_05, hh_inc_tot, process_income, process_msa,
   hh_inc_white_00, hh_inc_white_05, hh_inc_white,
   hh_inc_black_00, hh_inc_black_05, hh_inc_black,
   hh_inc_hispanic_00, hh_inc_hispanic_05, hh_inc_hispanic,
   process_00, process_05, hh_income_map, path)
