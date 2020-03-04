library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/health/insurance/"

process_08        <- function(df){
  df %>%
    transmute(
      FIPS,
      year,

      total = `Estimate; Total:`,
      insured =
        `Estimate; Under 65 years: - With health insurance coverage` +
        `Estimate; 65 years or over: - With health insurance coverage`)
}
process_09        <- function(df){
  df %>%
    transmute(
      FIPS,
      year,

      total.male = `Estimate; Male:`,
      total.female = `Estimate; Female:`,
      total = total.male + total.female,
      insured.male =
        `Estimate; Male: - Under 18 years: - With health insurance coverage` +
        `Estimate; Male: - 18 to 64 years: - With health insurance coverage` +
        `Estimate; Male: - 65 years and over: - With health insurance coverage`,
      insured.female =
        `Estimate; Female: - Under 18 years: - With health insurance coverage` +
        `Estimate; Female: - 18 to 64 years: - With health insurance coverage` +
        `Estimate; Female: - 65 years and over: - With health insurance coverage`,
      insured =
        insured.male + insured.female)
}
process_race      <- function(df) {
  df %>%
    transmute(
      FIPS,
      year,

      total =
        `Estimate; Total:`,
      insured =
        `Estimate; Under 18 years: - With health insurance coverage` +
        `Estimate; 18 to 64 years: - With health insurance coverage` +
        `Estimate; 65 years and over: - With health insurance coverage`)
}
process_map       <- function(df) {
  df %>%
    transmute(
      tract = Id,
      year,
      total = `Estimate; Total:`,
      insured =
        `Estimate; Male: - Under 6 years: - With health insurance coverage` +
        `Estimate; Male: - 6 to 18 years: - With health insurance coverage` +
        `Estimate; Male: - 19 to 25 years: - With health insurance coverage` +
        `Estimate; Male: - 26 to 34 years: - With health insurance coverage` +
        `Estimate; Male: - 35 to 44 years: - With health insurance coverage` +
        `Estimate; Male: - 45 to 54 years: - With health insurance coverage` +
        `Estimate; Male: - 55 to 64 years: - With health insurance coverage` +
        `Estimate; Male: - 65 to 74 years: - With health insurance coverage` +
        `Estimate; Male: - 75 years and over: - With health insurance coverage` +
        `Estimate; Female: - Under 6 years: - With health insurance coverage` +
        `Estimate; Female: - 6 to 18 years: - With health insurance coverage` +
        `Estimate; Female: - 19 to 25 years: - With health insurance coverage` +
        `Estimate; Female: - 26 to 34 years: - With health insurance coverage` +
        `Estimate; Female: - 35 to 44 years: - With health insurance coverage` +
        `Estimate; Female: - 45 to 54 years: - With health insurance coverage` +
        `Estimate; Female: - 55 to 64 years: - With health insurance coverage` +
        `Estimate; Female: - 65 to 74 years: - With health insurance coverage` +
        `Estimate; Female: - 75 years and over: - With health insurance coverage`)
}
process_msa       <- function(df) {
  df %>%
    transmute(
      MSA = as.character(Id2),
      year,

      total.male = `Estimate; Male:`,
      total.female = `Estimate; Female:`,
      total = total.male + total.female,
      insured.male =
        `Estimate; Male: - Under 19 years: - With health insurance coverage` +
        `Estimate; Male: - 19 to 64 years: - With health insurance coverage` +
        `Estimate; Male: - 65 years and over: - With health insurance coverage`,
      insured.female =
        `Estimate; Female: - Under 19 years: - With health insurance coverage` +
        `Estimate; Female: - 19 to 64 years: - With health insurance coverage` +
        `Estimate; Female: - 65 years and over: - With health insurance coverage`,
      insured =
        insured.male + insured.female)
}
process_insurance <- function(df, race_name){

  if (df_type(df) == "MSA") geog <- "MSA" else geog <- "FIPS"

  df %<>%

    #filter to peers
    pull_peers(add_info = FALSE) %>%

    #create sex column
    reshape_sex() %>%

    #add race
    mutate(race = race_name) %>%

    #reorder variables
    select(geog, year, race, sex, total, insured) %>%
    # merge STL counties
    {if (df_type(df) == "county") stl_merge(., total:insured, method = "sum") else .} %>%

    #calculate percentages
    mutate(insured = insured / total * 100) %>%

    select(-total)
}

insurance_08     <- acs_time(path %p% "C27001/08", starting_year = 2008)
insurance_09     <- acs_time(path %p% "C27001/09", starting_year = 2009)
insurance_map    <- read_csv(path %p% "B27001_tract/ACS_17_5YR_B27001_with_ann.csv", skip = 1)
insurance_msa    <- read_csv(path %p% "C27001_msa/ACS_17_1YR_C27001_with_ann.csv", skip = 1)

insurance_08  %<>% process_08()
insurance_09  %<>% process_09()
insurance_map %<>% mutate(year = 2015) %>% process_map()
insurance_msa %<>% mutate(year = 2017) %>% process_msa()

insurance_08  %<>% process_insurance("total")
insurance_09  %<>% process_insurance("total")
insurance_msa %<>% process_insurance("total")

insurance_white    <- acs_time(path %p% "C27001H", starting_year = 2009)
insurance_black    <- acs_time(path %p% "C27001B", starting_year = 2009)
insurance_hispanic <- acs_time(path %p% "C27001I", starting_year = 2009)

insurance_white    %<>% process_race() %>% process_insurance("white")
insurance_black    %<>% process_race() %>% process_insurance("black")
insurance_hispanic %<>% process_race() %>% process_insurance("hispanic")

insurance_tract <- insurance_map %>%
  transmute(
    tract,
    year,
    insured = insured / total * 100)

insurance_nh <- insurance_map %>%
  left_join(nh_tract, by = c("tract" = "GEO_ID")) %>%
  group_by(neighborhood, year) %>%
  summarise(
    insured = sum(insured) / sum(total) * 100) %>%
  ungroup()

insurance_county <- bind_rows(insurance_08, insurance_09,
                              insurance_white, insurance_black, insurance_hispanic)

update_sysdata(insurance_county, insurance_msa, insurance_tract, insurance_nh)

rm(process_08, process_09, process_race, process_insurance,
   insurance_map, process_map, process_msa,
   insurance_08, insurance_09, insurance_white, insurance_black, insurance_hispanic,
   path)
