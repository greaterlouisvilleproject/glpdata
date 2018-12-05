library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/educ/child_pov/"

process_00 <- function(df){
  df %<>%
    transmute(
      FIPS = as.numeric(Id2),
      year = 2000,

      num_pov_under_5.male =
        `Income in 1999 below poverty level: - Male: - Under 5 years`,
      num_pov_under_5.female =
        `Income in 1999 below poverty level: - Female: - Under 5 years`,
      num_pov_under_5 =
        num_pov_under_5.male + num_pov_under_5.female,

      num_pov.male =
        num_pov_under_5.male +
        `Income in 1999 below poverty level: - Male: - 5 years` +
        `Income in 1999 below poverty level: - Male: - 6 to 11 years` +
        `Income in 1999 below poverty level: - Male: - 12 to 14 years` +
        `Income in 1999 below poverty level: - Male: - 15 years` +
        `Income in 1999 below poverty level: - Male: - 16 and 17 years`,
      num_pov.female =
        num_pov_under_5.female +
        `Income in 1999 below poverty level: - Female: - 5 years` +
        `Income in 1999 below poverty level: - Female: - 6 to 11 years` +
        `Income in 1999 below poverty level: - Female: - 12 to 14 years` +
        `Income in 1999 below poverty level: - Female: - 15 years` +
        `Income in 1999 below poverty level: - Female: - 16 and 17 years`,
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

      total.male =
        num_pov.male +
        `Income in 1999 at or above poverty level: - Male: - Under 5 years` +
        `Income in 1999 at or above poverty level: - Male: - 5 years` +
        `Income in 1999 at or above poverty level: - Male: - 6 to 11 years` +
        `Income in 1999 at or above poverty level: - Male: - 12 to 14 years` +
        `Income in 1999 at or above poverty level: - Male: - 15 years` +
        `Income in 1999 at or above poverty level: - Male: - 16 and 17 years`,
      total.female =
        num_pov.female +
        `Income in 1999 at or above poverty level: - Female: - Under 5 years` +
        `Income in 1999 at or above poverty level: - Female: - 5 years` +
        `Income in 1999 at or above poverty level: - Female: - 6 to 11 years` +
        `Income in 1999 at or above poverty level: - Female: - 12 to 14 years` +
        `Income in 1999 at or above poverty level: - Female: - 15 years` +
        `Income in 1999 at or above poverty level: - Female: - 16 and 17 years`,
      total =
        total.male + total.female)

  df
}

process_05 <- function(df){
  df %<>%
    transmute(
      FIPS,
      year,

      num_pov_under_5.male =
        `Estimate; Income in the past 12 months below poverty level: - Male: - Under 5 years`,
      num_pov_under_5.female =
        `Estimate; Income in the past 12 months below poverty level: - Female: - Under 5 years`,
      num_pov_under_5 =
        num_pov_under_5.male + num_pov_under_5.female,

      num_pov.male =
        num_pov_under_5.male +
        `Estimate; Income in the past 12 months below poverty level: - Male: - 5 years` +
        `Estimate; Income in the past 12 months below poverty level: - Male: - 6 to 11 years` +
        `Estimate; Income in the past 12 months below poverty level: - Male: - 12 to 14 years` +
        `Estimate; Income in the past 12 months below poverty level: - Male: - 15 years` +
        `Estimate; Income in the past 12 months below poverty level: - Male: - 16 and 17 years`,
      num_pov.female =
        num_pov_under_5.female +
        `Estimate; Income in the past 12 months below poverty level: - Female: - 5 years` +
        `Estimate; Income in the past 12 months below poverty level: - Female: - 6 to 11 years` +
        `Estimate; Income in the past 12 months below poverty level: - Female: - 12 to 14 years` +
        `Estimate; Income in the past 12 months below poverty level: - Female: - 15 years` +
        `Estimate; Income in the past 12 months below poverty level: - Female: - 16 and 17 years`,
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

      total.male =
        num_pov.male +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - Under 5 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 5 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 6 to 11 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 12 to 14 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 15 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Male: - 16 and 17 years`,
      total.female =
        num_pov.female +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - Under 5 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 5 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 6 to 11 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 12 to 14 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 15 years` +
        `Estimate; Income in the past 12 months at or above poverty level: - Female: - 16 and 17 years`,
      total =
        total.male + total.female)

  df
}

process_pov <- function(df, race_name){
  df <- df %>%

    #filter to peers
    pull_peers_FIPS(add_info = FALSE) %>%

    #gather data, separate sex into column, replace NAs with total, and spread
    reshape_sex() %>%

    #add race
    mutate(race = race_name) %>%

    #reorder variables
    select(
      FIPS, year, race, sex, contains("total"), num_pov, num_pov_under_5) %>%

    #merge STL counties
    stl_merge(total:num_pov_under_5, method = "sum") %>%

    #calculate percentages
    mutate(num_pov = num_pov / total * 100) %>%
    mutate(num_pov_under_5 = num_pov_under_5 / total_under_5 * 100) %>%

    #remove unnecessary variables
    select(-total, -total_under_5) %>%

    #rename variables to "per"
    rename(
      child_pov = num_pov,
      child_pov_under_5 = num_pov_under_5)

  df
}


#Total
pov_00 <- read_csv(path %+% "DEC_00_SF3_PCT049_with_ann.csv", skip = 1)
pov_05 <- acs_time(path %+% "B17001")

pov_00 %<>% process_00()
pov_05 %<>% process_05()

pov_tot <- bind_rows(pov_00, pov_05)

pov_tot %<>% process_pov(race_name = "total")


#White
pov_00_white <- read_csv(path %+% "DEC_00_SF3_PCT075I_with_ann.csv", skip = 1)
pov_05_white <- acs_time(path %+% "B17001H")

pov_00_white %<>% process_00()
pov_05_white %<>% process_05()

pov_white <- bind_rows(pov_00_white, pov_05_white)

pov_white %<>% process_pov(race_name = "white")


#Black
pov_00_black <- read_csv(path %+% "DEC_00_SF3_PCT075B_with_ann.csv", skip = 1)
pov_05_black <- acs_time(path %+% "B17001B")

pov_00_black %<>% process_00()
pov_05_black %<>% process_05()

pov_black <- bind_rows(pov_00_black, pov_05_black)

pov_black %<>% process_pov(race_name = "black")


#Hispanic
pov_00_hispanic <- read_csv(path %+% "DEC_00_SF3_PCT075H_with_ann.csv", skip = 1)
pov_05_hispanic <- acs_time(path %+% "B17001I")

pov_00_hispanic %<>% process_00()
pov_05_hispanic %<>% process_05()

pov_hispanic <- bind_rows(pov_00_hispanic, pov_05_hispanic)

pov_hispanic %<>% process_pov(race_name = "hispanic")

#combine data frames
poverty <- bind_rows(pov_tot, pov_white, pov_black, pov_hispanic)

usethis::use_data(poverty, overwrite = TRUE)

rm(pov_00, pov_05, pov_tot, poverty,
   pov_00_white, pov_05_white, pov_white,
   pov_00_black, pov_05_black, pov_black,
   pov_00_hispanic, pov_05_hispanic, pov_hispanic,
   path, process_00, process_05, process_pov)
