library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/educ/attainment/"

process_00 <- function(df) {
  df <- df %>%
    transmute(
      FIPS = as.numeric(Id2),
      year = 2000,

      #total
      total.male = `Male:`,
      total.female = `Female:`,
      total = total.male + total.female,

      #no high school
      num_no_hs.male =
        `Male: - Less than 9th grade` +
        `Male: - 9th to 12th grade, no diploma`,
      num_no_hs.female =
        `Female: - Less than 9th grade` +
        `Female: - 9th to 12th grade, no diploma`,
      num_no_hs =
        num_no_hs.male + num_no_hs.female,

      #high school
      num_hs.male =
        `Male: - High school graduate (includes equivalency)`,
      num_hs.female =
        `Female: - High school graduate (includes equivalency)`,
      num_hs =
        num_hs.male + num_hs.female,

      #some college
      num_some_col.male =
        `Male: - Some college, no degree`,
      num_some_col.female =
        `Female: - Some college, no degree`,
      num_some_col =
        num_some_col.male + num_some_col.female,

      #associate
      num_assoc.male =
        `Male: - Associate degree`,
      num_assoc.female =
        `Female: - Associate degree`,
      num_assoc =
        num_assoc.male + num_assoc.female,

      #bachelor's
      num_bach.male =
        `Male: - Bachelor's degree`,
      num_bach.female =
        `Female: - Bachelor's degree`,
      num_bach =
        num_bach.male + num_bach.female,

      #grad
      num_grad.male =
        `Male: - Graduate or professional degree`,
      num_grad.female =
        `Female: - Graduate or professional degree`,
      num_grad =
        num_grad.male + num_grad.female,

      num_assoc_plus.male = num_assoc.male + num_bach.male + num_grad.male,
      num_assoc_plus.female = num_assoc.female + num_bach.female + num_grad.female,
      num_assoc_plus = num_assoc_plus.male + num_assoc_plus.female,

      num_bach_plus.male = num_bach.male + num_grad.male,
      num_bach_plus.female = num_bach.female + num_grad.female,
      num_bach_plus = num_bach_plus.male + num_bach_plus.female)

  df
}

process_05 <- function(df) {
  df <- df %>%
    transmute(
      FIPS, year,

      total.male = `Estimate; Male:`,
      total.female = `Estimate; Female:`,
      total = total.male + total.female,

      #no high school
      num_no_hs.male =
        `Estimate; Male: - Less than 9th grade` +
        `Estimate; Male: - 9th to 12th grade, no diploma`,
      num_no_hs.female =
        `Estimate; Female: - Less than 9th grade` +
        `Estimate; Female: - 9th to 12th grade, no diploma`,
      num_no_hs =
        num_no_hs.male + num_no_hs.female,

      #high school
      num_hs.male =
        `Estimate; Male: - High school graduate (includes equivalency)`,
      num_hs.female =
        `Estimate; Female: - High school graduate (includes equivalency)`,
      num_hs =
        num_hs.male + num_hs.female,

      #some college
      num_some_col.male =
        `Estimate; Male: - Some college, no degree`,
      num_some_col.female =
        `Estimate; Female: - Some college, no degree`,
      num_some_col =
        num_some_col.male + num_some_col.female,

      #associate
      num_assoc.male =
        `Estimate; Male: - Associate's degree`,
      num_assoc.female =
        `Estimate; Female: - Associate's degree`,
      num_assoc =
        num_assoc.male + num_assoc.female,

      #bachelor's
      num_bach.male =
        `Estimate; Male: - Bachelor's degree`,
      num_bach.female =
        `Estimate; Female: - Bachelor's degree`,
      num_bach =
        num_bach.male + num_bach.female,

      #grad
      num_grad.male =
        `Estimate; Male: - Graduate degree`,
      num_grad.female =
        `Estimate; Female: - Graduate degree`,
      num_grad =
        num_grad.male + num_grad.female,

      num_assoc_plus.male = num_assoc.male + num_bach.male + num_grad.male,
      num_assoc_plus.female = num_assoc.female + num_bach.female + num_grad.female,
      num_assoc_plus = num_assoc_plus.male + num_assoc_plus.female,

      num_bach_plus.male = num_bach.male + num_grad.male,
      num_bach_plus.female = num_bach.female + num_grad.female,
      num_bach_plus = num_bach_plus.male + num_bach_plus.female)

  df
}

process_08 <- function(df) {
  df <- df %>%
    transmute(
      FIPS, year,

      total.male = `Estimate; Male:`,
      total.female = `Estimate; Female:`,
      total = `Estimate; Total:`,

      #no high school
      num_no_hs.male =
        `Estimate; Male: - Less than 9th grade` +
        `Estimate; Male: - 9th to 12th grade, no diploma`,
      num_no_hs.female =
        `Estimate; Female: - Less than 9th grade` +
        `Estimate; Female: - 9th to 12th grade, no diploma`,
      num_no_hs =
        num_no_hs.male + num_no_hs.female,

      #high school
      num_hs.male =
        `Estimate; Male: - Regular high school diploma` +
        `Estimate; Male: - GED or alternative credential`,
      num_hs.female =
        `Estimate; Female: - Regular high school diploma` +
        `Estimate; Female: - GED or alternative credential`,
      num_hs =
        num_hs.male + num_hs.female,

      num_some_col.male =
        `Estimate; Male: - Some college, no degree`,
      num_some_col.female =
        `Estimate; Female: - Some college, no degree`,
      num_some_col =
        num_some_col.male + num_some_col.female,

      num_assoc.male =
        `Estimate; Male: - Associate's degree`,
      num_assoc.female =
        `Estimate; Female: - Associate's degree`,
      num_assoc =
        num_assoc.male + num_assoc.female,

      num_bach.male =
        `Estimate; Male: - Bachelor's degree`,
      num_bach.female =
        `Estimate; Female: - Bachelor's degree`,
      num_bach =
        num_bach.male + num_bach.female,

      num_grad.male =
        `Estimate; Male: - Graduate degree`,
      num_grad.female =
        `Estimate; Female: - Graduate degree`,
      num_grad =
        num_grad.male + num_grad.female,

      num_assoc_plus.male = num_assoc.male + num_bach.male + num_grad.male,
      num_assoc_plus.female = num_assoc.female + num_bach.female + num_grad.female,
      num_assoc_plus = num_assoc_plus.male + num_assoc_plus.female,

      num_bach_plus.male = num_bach.male + num_grad.male,
      num_bach_plus.female = num_bach.female + num_grad.female,
      num_bach_plus = num_bach_plus.male + num_bach_plus.female)

  df
}

process_attainment <- function(df, race_name){

  df <- df %>%

    #filter to peers
    pull_peers_FIPS(add_info = FALSE) %>%

    #gather data, separate sex into column, replace NAs with total, and spread
    reshape_sex() %>%

    #add race
    mutate(race = race_name) %>%

    #reorder variables
    select(
      FIPS, year, race, sex, total,
      num_no_hs, num_hs, num_some_col, num_assoc, num_bach, num_grad,
      num_assoc_plus, num_bach_plus) %>%

    #merge STL counties
    stl_merge(total:num_bach_plus, method = "sum") %>%

    #calculate percentages
    mutate_at(vars(num_no_hs:num_bach_plus), funs(. / total * 100)) %>%

    #remove unnecessary variables
    select(-total) %>%

    #rename variables to "per"
    rename_at(vars(contains("num")), funs(sub("num_", "", .)))

  df
}

#White
degree_white_00 <- read_csv(path %+% "DEC_00_SF3_P148I_with_ann.csv", skip = 1)
degree_white_05 <- acs_time(path %+% "B15002H/Y05", starting_year = 2005)
degree_white_08 <- acs_time(path %+% "B15002H/Y08", starting_year = 2008)

degree_white_00 %<>% process_00()
degree_white_05 %<>% process_05()
degree_white_08 %<>% process_08()

degree_white <- bind_rows(degree_white_00, degree_white_05, degree_white_08)

degree_white %<>% process_attainment(race_name = "white")


#Black
degree_black_00 <- read_csv(path %+% "DEC_00_SF3_P148B_with_ann.csv", skip = 1)
degree_black_05 <- acs_time(path %+% "B15002B/Y05", starting_year = 2005)
degree_black_08 <- acs_time(path %+% "B15002B/Y08", starting_year = 2008)

degree_black_00 %<>% process_00()
degree_black_05 %<>% process_05()
degree_black_08 %<>% process_08()

degree_black <- bind_rows(degree_black_00, degree_black_05, degree_black_08)

degree_black %<>% process_attainment(race_name = "black")


#Hispanic
degree_hispanic_00 <- read_csv(path %+% "DEC_00_SF3_P148H_with_ann.csv", skip = 1)
degree_hispanic_05 <- acs_time(path %+% "B15002I/Y05", starting_year = 2005)
degree_hispanic_08 <- acs_time(path %+% "B15002I/Y08", starting_year = 2008)

degree_hispanic_00 %<>% process_00()
degree_hispanic_05 %<>% process_05()
degree_hispanic_08 %<>% process_08()

degree_hispanic = bind_rows(degree_hispanic_00, degree_hispanic_05, degree_hispanic_08)

degree_hispanic %<>% process_attainment(race_name = "hispanic")


#combine data frames
degree_race <- bind_rows(degree_white, degree_black, degree_hispanic)

degree_race %<>% arrange()

usethis::use_data(degree_race, overwrite = TRUE)

rm(degree_white_00, degree_white_05, degree_white_08, degree_white,
   degree_black_00, degree_black_05, degree_black_08, degree_black,
   degree_hispanic_00, degree_hispanic_05, degree_hispanic_08, degree_hispanic,
   degree_race, process_00, process_05, process_08, process_attainment, path)
