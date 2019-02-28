library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/educ/preschool/"

process_00 <- function(df){
  df %<>%
    transmute(
      FIPS = as.numeric(Id2),
      year = 2000,

      enrolled_3_4.male =
        `Male: - Enrolled in school: - 3 and 4 years`,
      enrolled_3_4.female =
        `Female: - Enrolled in school: - 3 and 4 years`,
      enrolled_3_4 =
        enrolled_3_4.male + enrolled_3_4.female,

      total.male =
        enrolled_3_4.male +
        `Male: - Not enrolled in school: - 3 and 4 years`,
      total.female =
        enrolled_3_4.female +
        `Female: - Not enrolled in school: - 3 and 4 years`,
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

      enrolled_3_4.male =
        `Estimate; Male: - Enrolled in public school: - 3 and 4 years` +
        `Estimate; Male: - Enrolled in private school: - 3 and 4 years`,
      enrolled_3_4.female =
        `Estimate; Female: - Enrolled in public school: - 3 and 4 years` +
        `Estimate; Female: - Enrolled in private school: - 3 and 4 years`,
      enrolled_3_4 =
        enrolled_3_4.male + enrolled_3_4.female,

      total.male =
        enrolled_3_4.male +
        `Estimate; Male: - Not enrolled in school: - 3 and 4 years`,
      total.female =
        enrolled_3_4.female +
        `Estimate; Female: - Not enrolled in school: - 3 and 4 years`,
      total =
        total.male + total.female)

  df[[geog]] <- df$id

  df %<>% select(-id)

  df
}

process_preschool <- function(df){
  df %<>%
    pull_peers_FIPS(add_info = FALSE) %>%
    reshape_sex() %>%
    mutate(race = "total") %>%
    select(FIPS, year, race, sex, total, enrolled_3_4) %>%
    stl_merge(total:enrolled_3_4, method = "sum") %>%
    mutate(enrolled_3_4 = enrolled_3_4 / total * 100) %>%
    select(-total)

  df
}

preschool_00 <- read_csv(path %p% "DEC_00_SF3_PCT023_with_ann.csv", skip = 1)
preschool_05 <- acs_time(path %p% "B14003")
preschool_map <- read_csv(path %p% "ACS_17_5YR_B14003_with_ann.csv", skip = 1)

preschool_00 %<>% process_00()
preschool_05 %<>% process_05("FIPS")
preschool_map %<>%
  mutate(year = 2017) %>%
  process_05("Id")

preschool <- bind_rows(preschool_00, preschool_05)

preschool %<>% process_preschool()
preschool_map %<>%
  transmute(
    Id,
    enrolled_3_4 = enrolled_3_4 / total * 100)

usethis::use_data(preschool, overwrite = TRUE)
usethis::use_data(preschool_map, overwrite = TRUE)

rm(process_00, process_05, process_preschool, preschool_00, preschool_05, path)
