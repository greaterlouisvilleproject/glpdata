library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/jobs/unemployment/"

process_00 <- function(df){
  df %<>%
    transmute(
      FIPS = as.character(as.numeric(Id2)),
      year = 2000,

      total.male =
        `Male: - 16 to 19 years: - In labor force: - Civilian:` +
        `Male: - 20 to 21 years: - In labor force: - Civilian:` +
        `Male: - 22 to 24 years: - In labor force: - Civilian:` +
        `Male: - 25 to 29 years: - In labor force: - Civilian:` +
        `Male: - 30 to 34 years: - In labor force: - Civilian:` +
        `Male: - 35 to 44 years: - In labor force: - Civilian:` +
        `Male: - 45 to 54 years: - In labor force: - Civilian:` +
        `Male: - 55 to 59 years: - In labor force: - Civilian:` +
        `Male: - 60 to 61 years: - In labor force: - Civilian:` +
        `Male: - 62 to 64 years: - In labor force: - Civilian:` +
        `Male: - 65 to 69 years: - In labor force: - Civilian:` +
        `Male: - 70 to 74 years: - In labor force: - Civilian:` +
        `Male: - 75 years and over: - In labor force: - Civilian:`,
      total.female =
        `Female: - 16 to 19 years: - In labor force: - Civilian:` +
        `Female: - 20 to 21 years: - In labor force: - Civilian:` +
        `Female: - 22 to 24 years: - In labor force: - Civilian:` +
        `Female: - 25 to 29 years: - In labor force: - Civilian:` +
        `Female: - 30 to 34 years: - In labor force: - Civilian:` +
        `Female: - 35 to 44 years: - In labor force: - Civilian:` +
        `Female: - 45 to 54 years: - In labor force: - Civilian:` +
        `Female: - 55 to 59 years: - In labor force: - Civilian:` +
        `Female: - 60 to 61 years: - In labor force: - Civilian:` +
        `Female: - 62 to 64 years: - In labor force: - Civilian:` +
        `Female: - 65 to 69 years: - In labor force: - Civilian:` +
        `Female: - 70 to 74 years: - In labor force: - Civilian:` +
        `Female: - 75 years and over: - In labor force: - Civilian:`,
      total = total.male + total.female,

      unemployment.male =
        `Male: - 16 to 19 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 20 to 21 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 22 to 24 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 25 to 29 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 30 to 34 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 35 to 44 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 45 to 54 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 55 to 59 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 60 to 61 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 62 to 64 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 65 to 69 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 70 to 74 years: - In labor force: - Civilian: - Unemployed` +
        `Male: - 75 years and over: - In labor force: - Civilian: - Unemployed`,
      unemployment.female =
        `Female: - 16 to 19 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 20 to 21 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 22 to 24 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 25 to 29 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 30 to 34 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 35 to 44 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 45 to 54 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 55 to 59 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 60 to 61 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 62 to 64 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 65 to 69 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 70 to 74 years: - In labor force: - Civilian: - Unemployed` +
        `Female: - 75 years and over: - In labor force: - Civilian: - Unemployed`,
      unemployment = unemployment.male + unemployment.female)
}
process_05 <- function(df, geog){

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "Id2", "tract" = "Id")

  df %>%
    transmute(
      !!geog := as.character(.data[[col_name]]),
      year,

      total.male =
        `Estimate; Male: - 16 to 19 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 20 and 21 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 22 to 24 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 25 to 29 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 30 to 34 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 35 to 44 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 45 to 54 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 55 to 59 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 60 and 61 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 62 to 64 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 65 to 69 years: - In labor force:` +
        `Estimate; Male: - 70 to 74 years: - In labor force:` +
        `Estimate; Male: - 75 years and over: - In labor force:`,
      total.female =
        `Estimate; Female: - 16 to 19 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 20 and 21 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 22 to 24 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 25 to 29 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 30 to 34 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 35 to 44 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 45 to 54 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 55 to 59 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 60 and 61 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 62 to 64 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 65 to 69 years: - In labor force:` +
        `Estimate; Female: - 70 to 74 years: - In labor force:` +
        `Estimate; Female: - 75 years and over: - In labor force:`,
      total = total.male + total.female,

      unemployment.male =
        `Estimate; Male: - 16 to 19 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 20 and 21 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 22 to 24 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 25 to 29 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 30 to 34 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 35 to 44 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 45 to 54 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 55 to 59 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 60 and 61 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 62 to 64 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 65 to 69 years: - In labor force: - Unemployed` +
        `Estimate; Male: - 70 to 74 years: - In labor force: - Unemployed` +
        `Estimate; Male: - 75 years and over: - In labor force: - Unemployed`,
      unemployment.female =
        `Estimate; Female: - 16 to 19 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 20 and 21 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 22 to 24 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 25 to 29 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 30 to 34 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 35 to 44 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 45 to 54 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 55 to 59 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 60 and 61 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 62 to 64 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 65 to 69 years: - In labor force: - Unemployed` +
        `Estimate; Female: - 70 to 74 years: - In labor force: - Unemployed` +
        `Estimate; Female: - 75 years and over: - In labor force: - Unemployed`,
      unemployment = unemployment.male + unemployment.female)
}
process_unemployment <- function(df, race_name){
  df %>%

    #filter to peers
    pull_peers(add_info = FALSE) %>%

    #create sex column
    reshape_sex() %>%

    #add race
    mutate(race = race_name) %>%

    #merge STL counties
    {if (df_type(df) == "FIPS") stl_merge(., total:unemployment, method = "sum") else .} %>%

    #calculate percentages
    mutate(unemployment = unemployment / total * 100) %>%

    #remove unnecessary variables
    select(-total)
}

process_00_race <- function(df){
  df %<>%
    transmute(
      FIPS = as.character(as.numeric(Id2)),
      year = 2000,

      total.male   = `Male: - In labor force: - Civilian:`,
      total.female = `Female: - In labor force: - Civilian:`,
      total        = total.male + total.female,

      unemployment.male   = `Male: - In labor force: - Civilian: - Unemployed`,
      unemployment.female = `Female: - In labor force: - Civilian: - Unemployed`,
      unemployment        = unemployment.male + unemployment.female)
}
process_05_race <- function(df){
  df %>%
    transmute(
      FIPS,
      year,
      total.male =
        `Estimate; Male: - 16 to 19 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 20 to 24 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 25 to 54 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 55 to 64 years: - In labor force: - Civilian:` +
        `Estimate; Male: - 65 to 69 years: - In labor force:` +
        `Estimate; Male: - 70 years and over: - In labor force:`,
      total.female =
        `Estimate; Female: - 16 to 19 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 20 to 24 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 25 to 54 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 55 to 64 years: - In labor force: - Civilian:` +
        `Estimate; Female: - 65 to 69 years: - In labor force:` +
        `Estimate; Female: - 70 years and over: - In labor force:`,
      total = total.male + total.female,

      unemployment.male =
        `Estimate; Male: - 16 to 19 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 20 to 24 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 25 to 54 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 55 to 64 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Male: - 65 to 69 years: - In labor force: - Unemployed` +
        `Estimate; Male: - 70 years and over: - In labor force: - Unemployed`,
      unemployment.female =
        `Estimate; Female: - 16 to 19 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 20 to 24 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 25 to 54 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 55 to 64 years: - In labor force: - Civilian: - Unemployed` +
        `Estimate; Female: - 65 to 69 years: - In labor force: - Unemployed` +
        `Estimate; Female: - 70 years and over: - In labor force: - Unemployed`,
      unemployment = unemployment.male + unemployment.female)
}

# Total
unemployment_00      <- read_csv(path %p% "DEC_00_SF3_PCT035_with_ann.csv", skip = 1)
unemployment_05      <- acs_time(path %p% "B23001")
unemployment_msa_5yr <- acs_time(path %p% "B23001_5yr", geography = "MSA", starting_year = 2007)
unemployment_msa_1yr <- read_csv(path %p% "MSA/ACS_17_1YR_B23001_with_ann.csv", skip = 1)

unemployment_00      %<>% process_00()
unemployment_05      %<>% process_05("FIPS")
unemployment_00_msa  <- unemployment_00 %>% sum_FIPS_to_MSA()
unemployment_msa_5yr %<>% process_05("FIPS") %>% sum_FIPS_to_MSA()
unemployment_msa_1yr %<>% mutate(year = 2017) %>% process_05("MSA")

unemployment_tot     <- bind_rows(unemployment_00, unemployment_05)
unemployment_msa_1yr %<>% bind_rows(unemployment_00_msa, .)
unemployment_msa_5yr %<>% bind_rows(unemployment_00_msa, .)

unemployment_tot     %<>% process_unemployment("total")
unemployment_msa_1yr %<>% process_unemployment("total")
unemployment_msa_5yr %<>% process_unemployment("total")


# White
unemployment_white_00 <- read_csv(path %p% "DEC_00_SF3_P150I_with_ann.csv", skip = 1)
unemployment_white_05 <- acs_time(path %p% "B23002H")

unemployment_white_00 %<>% process_00_race()
unemployment_white_05 %<>% process_05_race()

unemployment_white <- bind_rows(unemployment_white_00, unemployment_white_05)
unemployment_white %<>% process_unemployment("white")


# Black
unemployment_black_00 <- read_csv(path %p% "DEC_00_SF3_P150B_with_ann.csv", skip = 1)
unemployment_black_05 <- acs_time(path %p% "B23002B")

unemployment_black_00 %<>% process_00_race()
unemployment_black_05 %<>% process_05_race()

unemployment_black <- bind_rows(unemployment_black_00, unemployment_black_05)
unemployment_black %<>% process_unemployment("black")


# Hispanic
unemployment_hisp_00 <- read_csv(path %p% "DEC_00_SF3_P150H_with_ann.csv", skip = 1)
unemployment_hisp_05 <- acs_time(path %p% "B23002I")

unemployment_hisp_00 %<>% process_00_race()
unemployment_hisp_05 %<>% process_05_race()

unemployment_hisp <- bind_rows(unemployment_hisp_00, unemployment_hisp_05)
unemployment_hisp %<>% process_unemployment("hispanic")

unemployment_county <- bind_rows(unemployment_tot, unemployment_white,
                                 unemployment_black, unemployment_hisp)

unemployment_county %<>% organize()

# Map
unemployment_map <- read_csv(path %p% "ACS_17_5YR_B23001_with_ann.csv", skip = 1)

unemployment_map %<>%
  mutate(year = 2015) %>%
  process_05("tract")

unemployment_tract <- unemployment_map %>%
  transmute(
    tract,
    year,
    unemployment = unemployment / total * 100)

unemployment_nh <- unemployment_map %>%
  left_join(nh_tract, by = c("tract" = "GEO_ID")) %>%
  group_by(neighborhood, year) %>%
  summarise(
    unemployment = sum(unemployment) / sum(total) * 100) %>%
  ungroup()

update_sysdata(unemployment_county, unemployment_tract, unemployment_nh,
               unemployment_msa_1yr, unemployment_msa_5yr)

rm(path, unemployment_map, unemployment_tot, unemployment_00, unemployment_05,
   unemployment_white, unemployment_white_00, unemployment_white_05,
   unemployment_black, unemployment_black_00, unemployment_black_05,
   unemployment_hisp, unemployment_hisp_00, unemployment_hisp_05,
   process_00, process_00_race, process_05, process_05_race, process_unemployment,
   unemployment_00_msa)


