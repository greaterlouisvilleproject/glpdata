library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/educ/poverty/"

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
process_05 <- function(df, geog){

  df$id <- df[[geog]]

  df %<>%
    transmute(
      id,
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

  df[[geog]] <- df$id

  df %<>% select(-id)

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
      FIPS, year, race, sex,
      total, total_child, total_under_5, total_5_17,
      num_pov, num_pov_child, num_pov_under_5, num_pov_5_17) %>%

    #merge STL counties
    stl_merge(total:num_pov_5_17, method = "sum") %>%

    #calculate percentages
    mutate(
      pov = num_pov / total * 100,
      child_pov = num_pov_child / total_child * 100,
      child_pov_under_5 = num_pov_under_5 / total_under_5 * 100,
      child_pov_5_17 = num_pov_5_17 / total_5_17 * 100) %>%

    #remove unnecessary variables
    select(-contains("num"), -contains("total"))

  df
}


#Total
pov_00 <- read_csv(path %p% "DEC_00_SF3_PCT049_with_ann.csv", skip = 1)
pov_05 <- acs_time(path %p% "B17001")
poverty_map <- read_csv(path %p% "ACS_17_5YR_B17001_with_ann.csv", skip = 1)

pov_00 %<>% process_00()
pov_05 %<>% process_05("FIPS")
poverty_map %<>%
  mutate(year = 2017) %>%
  process_05("Id")

pov_tot <- bind_rows(pov_00, pov_05)

pov_tot %<>% process_pov(race_name = "total")

poverty_map %<>%
  transmute(
    Id,
    child_pov = num_pov_child / total_child * 100,
    pov = num_pov / total * 100)


#White
pov_00_white <- read_csv(path %p% "DEC_00_SF3_PCT075I_with_ann.csv", skip = 1)
pov_05_white <- acs_time(path %p% "B17001H")

pov_00_white %<>% process_00()
pov_05_white %<>% process_05("FIPS")

pov_white <- bind_rows(pov_00_white, pov_05_white)

pov_white %<>% process_pov(race_name = "white")


#Black
pov_00_black <- read_csv(path %p% "DEC_00_SF3_PCT075B_with_ann.csv", skip = 1)
pov_05_black <- acs_time(path %p% "B17001B")

pov_00_black %<>% process_00()
pov_05_black %<>% process_05("FIPS")

pov_black <- bind_rows(pov_00_black, pov_05_black)

pov_black %<>% process_pov(race_name = "black")


#Hispanic
pov_00_hispanic <- read_csv(path %p% "DEC_00_SF3_PCT075H_with_ann.csv", skip = 1)
pov_05_hispanic <- acs_time(path %p% "B17001I")

pov_00_hispanic %<>% process_00()
pov_05_hispanic %<>% process_05("FIPS")

pov_hispanic <- bind_rows(pov_00_hispanic, pov_05_hispanic)

pov_hispanic %<>% process_pov(race_name = "hispanic")

#combine data frames
poverty <- bind_rows(pov_tot, pov_white, pov_black, pov_hispanic)

usethis::use_data(poverty, overwrite = TRUE)
usethis::use_data(poverty_map, overwrite = TRUE)

rm(path, process_00, process_05, process_pov,
   pov_00, pov_05, pov_tot,
   pov_00_white, pov_05_white, pov_white,
   pov_00_black, pov_05_black, pov_black,
   pov_00_hispanic, pov_05_hispanic, pov_hispanic)
