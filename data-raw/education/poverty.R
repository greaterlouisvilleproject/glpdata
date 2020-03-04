library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/education/poverty/"

process_00  <- function(df){
  df %<>%
    transmute(
      FIPS = as.character(as.numeric(Id2)),
      year = 2000,

      num_pov_under_5.male =
        `Income in 1999 below poverty level: - Male: - Under 5 years`,
      num_pov_under_5.female =
        `Income in 1999 below poverty level: - Female: - Under 5 years`,
      num_pov_under_5 =
        num_pov_under_5.male + num_pov_under_5.female,

      num_pov_5_17.male =
        `Income in 1999 below poverty level: - Male: - 5 years` +
        `Income in 1999 below poverty level: - Male: - 6 to 11 years` +
        `Income in 1999 below poverty level: - Male: - 12 to 14 years` +
        `Income in 1999 below poverty level: - Male: - 15 years` +
        `Income in 1999 below poverty level: - Male: - 16 and 17 years`,
      num_pov_5_17.female =
        `Income in 1999 below poverty level: - Female: - 5 years` +
        `Income in 1999 below poverty level: - Female: - 6 to 11 years` +
        `Income in 1999 below poverty level: - Female: - 12 to 14 years` +
        `Income in 1999 below poverty level: - Female: - 15 years` +
        `Income in 1999 below poverty level: - Female: - 16 and 17 years`,
      num_pov_5_17 =
        num_pov_5_17.male + num_pov_5_17.female,

      num_pov_child.male =
        num_pov_under_5.male + num_pov_5_17.male,
      num_pov_child.female =
        num_pov_under_5.female + num_pov_5_17.female,
      num_pov_child =
        num_pov_under_5 + num_pov_5_17,

      num_pov.male =
        `Income in 1999 below poverty level: - Male:`,
      num_pov.female =
        `Income in 1999 below poverty level: - Female:`,
      num_pov =
        num_pov.male + num_pov.female,

      total_under_5.male =
        num_pov_under_5.male +
        `Income in 1999 at or above poverty level: - Male: - Under 5 years`,
      total_under_5.female =
        num_pov_under_5.female +
        `Income in 1999 at or above poverty level: - Female: - Under 5 years`,
      total_under_5 =
        total_under_5.male + total_under_5.female,

      total_5_17.male =
        num_pov_5_17.male +
        `Income in 1999 at or above poverty level: - Male: - 5 years` +
        `Income in 1999 at or above poverty level: - Male: - 6 to 11 years` +
        `Income in 1999 at or above poverty level: - Male: - 12 to 14 years` +
        `Income in 1999 at or above poverty level: - Male: - 15 years` +
        `Income in 1999 at or above poverty level: - Male: - 16 and 17 years`,
      total_5_17.female =
        num_pov_5_17.female +
        `Income in 1999 at or above poverty level: - Female: - 5 years` +
        `Income in 1999 at or above poverty level: - Female: - 6 to 11 years` +
        `Income in 1999 at or above poverty level: - Female: - 12 to 14 years` +
        `Income in 1999 at or above poverty level: - Female: - 15 years` +
        `Income in 1999 at or above poverty level: - Female: - 16 and 17 years`,
      total_5_17 =
        total_5_17.male + total_5_17.female,

      total_child.male =
        total_under_5.male + total_5_17.male,
      total_child.female =
        total_under_5.female + total_5_17.female,
      total_child =
        total_child.male + total_child.female,

      total.male =
        num_pov.male +
        `Income in 1999 at or above poverty level: - Male:`,
      total.female =
        num_pov.female +
        `Income in 1999 at or above poverty level: - Female:`,
      total =
        total.male + total.female)

  df
}
process_05  <- function(df, geog){

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "Id2", "tract" = "Id")

  df %>%
    transmute(
      !!geog := as.character(.data[[col_name]]),
      year,

      num_pov_under_5.male =
        `Estimate; Income in the past 12 months below poverty level: - Male: - Under 5 years`,
      num_pov_under_5.female =
        `Estimate; Income in the past 12 months below poverty level: - Female: - Under 5 years`,
      num_pov_under_5 =
        num_pov_under_5.male + num_pov_under_5.female,

      num_pov_5_17.male =
        `Estimate; Income in the past 12 months below poverty level: - Male: - 5 years` +
        `Estimate; Income in the past 12 months below poverty level: - Male: - 6 to 11 years` +
        `Estimate; Income in the past 12 months below poverty level: - Male: - 12 to 14 years` +
        `Estimate; Income in the past 12 months below poverty level: - Male: - 15 years` +
        `Estimate; Income in the past 12 months below poverty level: - Male: - 16 and 17 years`,
      num_pov_5_17.female =
        `Estimate; Income in the past 12 months below poverty level: - Female: - 5 years` +
        `Estimate; Income in the past 12 months below poverty level: - Female: - 6 to 11 years` +
        `Estimate; Income in the past 12 months below poverty level: - Female: - 12 to 14 years` +
        `Estimate; Income in the past 12 months below poverty level: - Female: - 15 years` +
        `Estimate; Income in the past 12 months below poverty level: - Female: - 16 and 17 years`,
      num_pov_5_17 =
        num_pov_5_17.male + num_pov_5_17.female,

      num_pov_child.male =
        num_pov_under_5.male + num_pov_5_17.male,
      num_pov_child.female =
        num_pov_under_5.female + num_pov_5_17.female,
      num_pov_child =
        num_pov_child.male + num_pov_child.female,

      num_pov.male =
        `Estimate; Income in the past 12 months below poverty level: - Male:`,
      num_pov.female =
        `Estimate; Income in the past 12 months below poverty level: - Female:`,
      num_pov =
        num_pov.male + num_pov.female,

      total_under_5.male =
        num_pov_under_5.male +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - Under 5 years`,
      total_under_5.female =
        num_pov_under_5.female +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - Under 5 years`,
      total_under_5 =
        total_under_5.male + total_under_5.female,

      total_5_17.male =
        num_pov_5_17.male +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 5 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 6 to 11 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 12 to 14 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 15 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 16 and 17 years`,
      total_5_17.female =
        num_pov_5_17.female +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 5 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 6 to 11 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 12 to 14 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 15 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 16 and 17 years`,
      total_5_17 =
        total_5_17.male + total_5_17.female,

      total_child.male =
        total_under_5.male + total_5_17.male,
      total_child.female =
        total_under_5.female + total_5_17.female,
      total_child =
        total_child.male + total_child.female,

      total.male =
        num_pov.male +
        `Estimate; Income in the past 12 months at or above poverty level: - Male:`,
      total.female =
        num_pov.female +
        `Estimate; Income in the past 12 months at or above poverty level: - Female:`,
      total =
        total.male + total.female)
}
process_pov <- function(df, race_name){

  if (df_type(df) == "MSA") geog <- "MSA" else geog <- "FIPS"

  df %>%

    #filter to peers
    pull_peers(add_info = FALSE) %>%

    #gather data, separate sex into column, replace NAs with total, and spread
    reshape_sex() %>%

    #add race
    mutate(race = race_name) %>%

    #reorder variables
    select(
      !!geog, year, race, sex,
      total, total_child, total_under_5, total_5_17,
      num_pov, num_pov_child, num_pov_under_5, num_pov_5_17) %>%

    #merge STL counties
    {if (geog == "FIPS") stl_merge(., total:num_pov_5_17, method = "sum") else .} %>%

    #calculate percentages
    mutate(
      poverty = num_pov / total * 100,
      child_poverty = num_pov_child / total_child * 100,
      child_poverty_under_5 = num_pov_under_5 / total_under_5 * 100,
      child_poverty_5_17 = num_pov_5_17 / total_5_17 * 100) %>%

    #remove unnecessary variables
    select(-contains("num"), -contains("total"))
}

#Total
poverty_00      <- read_csv(path %p% "DEC_00_SF3_PCT049_with_ann.csv", skip = 1)
poverty_05      <- acs_time(path %p% "B17001")
poverty_map     <- read_csv(path %p% "ACS_17_5YR_B17001_with_ann.csv", skip = 1)
poverty_msa_5yr <- acs_time(path %p% "B17001_5yr", geography = "MSA", starting_year = 2007)
poverty_msa_1yr <- read_csv(path %p% "B17001_msa/ACS_17_1YR_B17001_with_ann.csv", skip = 1)

poverty_00      %<>% process_00()
poverty_05      %<>% process_05("FIPS")
poverty_map     %<>% mutate(year = 2015) %>% process_05("tract")
poverty_00_msa  <- poverty_00 %>% sum_FIPS_to_MSA()
poverty_msa_5yr %<>% process_05("FIPS") %>% sum_FIPS_to_MSA()
poverty_msa_1yr %<>% mutate(year = 2017) %>% process_05("MSA")

poverty_tot <- bind_rows(poverty_00, poverty_05)

poverty_tot     %<>% process_pov("total")
poverty_msa_5yr %<>% bind_rows(poverty_00_msa, .) %>% process_pov("total")
poverty_msa_1yr %<>% bind_rows(poverty_00_msa, .) %>% process_pov("total")

poverty_tract <- poverty_map %>%
  transmute(
    tract,
    year,
    poverty = num_pov / total * 100,
    child_poverty = num_pov_child / total_child * 100,
    child_poverty_under_5 = num_pov_under_5 / total_under_5 * 100,
    child_poverty_5_17 = num_pov_5_17 / total_5_17 * 100)

poverty_nh <- poverty_map %>%
  left_join(nh_tract, by = c("tract" = "GEO_ID")) %>%
  group_by(neighborhood, year) %>%
  summarise(
    poverty = sum(num_pov) / sum(total) * 100,
    child_poverty = sum(num_pov_child) / sum(total_child) * 100,
    child_poverty_under_5 = sum(num_pov_under_5) / sum(total_under_5) * 100,
    child_poverty_5_17 = sum(num_pov_5_17) / sum(total_5_17) * 100) %>%
  ungroup()

#White
poverty_00_white <- read_csv(path %p% "DEC_00_SF3_PCT075I_with_ann.csv", skip = 1)
poverty_05_white <- acs_time(path %p% "B17001H")

poverty_00_white %<>% process_00()
poverty_05_white %<>% process_05("FIPS")

poverty_white <- bind_rows(poverty_00_white, poverty_05_white)

poverty_white %<>% process_pov(race_name = "white")


#Black
poverty_00_black <- read_csv(path %p% "DEC_00_SF3_PCT075B_with_ann.csv", skip = 1)
poverty_05_black <- acs_time(path %p% "B17001B")

poverty_00_black %<>% process_00()
poverty_05_black %<>% process_05("FIPS")

poverty_black <- bind_rows(poverty_00_black, poverty_05_black)

poverty_black %<>% process_pov(race_name = "black")


#Hispanic
poverty_00_hispanic <- read_csv(path %p% "DEC_00_SF3_PCT075H_with_ann.csv", skip = 1)
poverty_05_hispanic <- acs_time(path %p% "B17001I")

poverty_00_hispanic %<>% process_00()
poverty_05_hispanic %<>% process_05("FIPS")

poverty_hispanic <- bind_rows(poverty_00_hispanic, poverty_05_hispanic)

poverty_hispanic %<>% process_pov(race_name = "hispanic")

#combine data frames
poverty_county <- bind_rows(poverty_tot, poverty_white, poverty_black, poverty_hispanic)

update_sysdata(poverty_county, poverty_msa_1yr, poverty_msa_5yr, poverty_tract, poverty_nh)

rm(path, process_00, process_05, process_pov, poverty_map,
   poverty_00, poverty_05, poverty_tot,
   poverty_00_white, poverty_05_white, poverty_white,
   poverty_00_black, poverty_05_black, poverty_black,
   poverty_00_hispanic, poverty_05_hispanic, poverty_hispanic)
