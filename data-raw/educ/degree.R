library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(survey)
library(feather)

path <- "data-raw/educ/degree/"

# Total

process_00 <- function(df){
  df %<>%
    transmute(
      FIPS = as.numeric(Id2),
      year = 2000,

      total_young.male = `Male: - 25 to 34 years:`,
      total_young.female = `Female: - 25 to 34 years:`,
      total_young = total_young.male + total_young.female,

      total.male =
        `Male: - 25 to 34 years:` +
        `Male: - 35 to 44 years:` +
        `Male: - 45 to 64 years:`,
      total.female =
        `Female: - 25 to 34 years:` +
        `Female: - 35 to 44 years:` +
        `Female: - 45 to 64 years:`,
      total =
        total.male + total.female,

      num_no_hs_young.male =
        `Male: - 25 to 34 years: - Less than 9th grade` +
        `Male: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      num_no_hs_young.female =
        `Female: - 25 to 34 years: - Less than 9th grade` +
        `Female: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      num_no_hs_young =
        num_no_hs_young.male + num_no_hs_young.female,

      num_no_hs.male =
        num_no_hs_young.male +
        `Male: - 35 to 44 years: - Less than 9th grade` +
        `Male: - 45 to 64 years: - Less than 9th grade` +
        `Male: - 35 to 44 years: - 9th to 12th grade, no diploma` +
        `Male: - 45 to 64 years: - 9th to 12th grade, no diploma`,
      num_no_hs.female =
        num_no_hs_young.female +
        `Female: - 35 to 44 years: - Less than 9th grade` +
        `Female: - 45 to 64 years: - Less than 9th grade` +
        `Female: - 35 to 44 years: - 9th to 12th grade, no diploma` +
        `Female: - 45 to 64 years: - 9th to 12th grade, no diploma`,
      num_no_hs =
        num_no_hs.male + num_no_hs.female,

      num_hs_young.male =
        `Male: - 25 to 34 years: - High school graduate (includes equivalency)`,
      num_hs_young.female =
        `Female: - 25 to 34 years: - High school graduate (includes equivalency)`,
      num_hs_young = num_hs_young.male + num_hs_young.female,

      num_hs.male =
        num_hs_young.male +
        `Male: - 35 to 44 years: - High school graduate (includes equivalency)` +
        `Male: - 45 to 64 years: - High school graduate (includes equivalency)`,
      num_hs.female =
        num_hs_young.female +
        `Female: - 35 to 44 years: - High school graduate (includes equivalency)` +
        `Female: - 45 to 64 years: - High school graduate (includes equivalency)`,
      num_hs = num_hs.male + num_hs.female,

      num_some_col_young.male =
        `Male: - 25 to 34 years: - Some college, no degree`,
      num_some_col_young.female =
        `Female: - 25 to 34 years: - Some college, no degree`,
      num_some_col_young =
        num_some_col_young.male + num_some_col_young.female,

      num_some_col.male =
        num_some_col_young.male +
        `Male: - 35 to 44 years: - Some college, no degree` +
        `Male: - 45 to 64 years: - Some college, no degree`,
      num_some_col.female =
        `Female: - 35 to 44 years: - Some college, no degree` +
        `Female: - 45 to 64 years: - Some college, no degree`,
      num_some_col = num_some_col.male + num_some_col.female,

      num_assoc_young.male =
        `Male: - 25 to 34 years: - Associate degree`,
      num_assoc_young.female =
        `Female: - 25 to 34 years: - Associate degree`,
      num_assoc_young = num_assoc_young.male + num_assoc_young.female,

      num_assoc.male =
        num_assoc_young.male +
        `Male: - 35 to 44 years: - Associate degree` +
        `Male: - 45 to 64 years: - Associate degree`,
      num_assoc.female =
        num_assoc_young.female +
        `Female: - 35 to 44 years: - Associate degree` +
        `Female: - 45 to 64 years: - Associate degree`,
      num_assoc = num_assoc.male + num_assoc.female,

      num_bach_young.male =
        `Male: - 25 to 34 years: - Bachelor's degree`,
      num_bach_young.female =
        `Female: - 25 to 34 years: - Bachelor's degree`,
      num_bach_young = num_bach_young.male + num_bach_young.female,

      num_bach.male =
        num_bach_young.male +
        `Male: - 35 to 44 years: - Bachelor's degree` +
        `Male: - 45 to 64 years: - Bachelor's degree`,
      num_bach.female =
        num_bach_young.female +
        `Female: - 35 to 44 years: - Bachelor's degree` +
        `Female: - 45 to 64 years: - Bachelor's degree`,
      num_bach = num_bach.male + num_bach.female,

      num_grad_young.male =
        `Male: - 25 to 34 years: - Graduate or professional degree`,
      num_grad_young.female =
        `Female: - 25 to 34 years: - Graduate or professional degree`,
      num_grad_young =
        num_grad_young.male + num_grad_young.female,

      num_grad.male =
        num_grad_young.male +
        `Male: - 35 to 44 years: - Graduate or professional degree` +
        `Male: - 45 to 64 years: - Graduate or professional degree`,
      num_grad.female =
        num_grad_young.female +
        `Female: - 35 to 44 years: - Graduate or professional degree` +
        `Female: - 45 to 64 years: - Graduate or professional degree`,
      num_grad =
        num_grad.male + num_grad.female,

      num_assoc_plus_young.male = num_assoc_young.male + num_bach_young.male + num_grad_young.male,
      num_assoc_plus_young.female = num_assoc_young.female + num_bach_young.female + num_grad_young.female,
      num_assoc_plus_young = num_assoc_plus_young.male + num_assoc_plus_young.female,

      num_assoc_plus.male = num_assoc.male + num_bach.male + num_grad.male,
      num_assoc_plus.female = num_assoc.female + num_bach.female + num_grad.female,
      num_assoc_plus = num_assoc_plus.male +  num_assoc_plus.female,

      num_bach_plus_young.male = num_bach_young.male + num_grad_young.male,
      num_bach_plus_young.female = num_bach_young.female + num_grad_young.female,
      num_bach_plus_young = num_bach_young.male + num_bach_plus_young.female,

      num_bach_plus.male = num_bach.male + num_grad.male,
      num_bach_plus.female = num_bach.female + num_grad.female,
      num_bach_plus = num_bach_plus.male + num_bach_plus.female)

  df
}
process_05 <- function(df, geog){

  df$id <- df[[geog]]

  df %<>%
    transmute(
      id,
      year,

      total_young.male =
        `Estimate; Male: - 25 to 34 years:`,
      total_young.female =
        `Estimate; Female: - 25 to 34 years:`,
      total_young =
        total_young.male + total_young.female,

      total.male =
        total_young.male +
        `Estimate; Male: - 35 to 44 years:` +
        `Estimate; Male: - 45 to 64 years:`,
      total.female =
        total_young.female +
        `Estimate; Female: - 35 to 44 years:` +
        `Estimate; Female: - 45 to 64 years:`,
      total =
        total.male + total.female,

      num_no_hs_young.male =
        `Estimate; Male: - 25 to 34 years: - Less than 9th grade` +
        `Estimate; Male: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      num_no_hs_young.female =
        `Estimate; Female: - 25 to 34 years: - Less than 9th grade` +
        `Estimate; Female: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      num_no_hs_young =
        num_no_hs_young.male + num_no_hs_young.female,

      num_no_hs.male =
        num_no_hs_young.male +
        `Estimate; Male: - 35 to 44 years: - Less than 9th grade` +
        `Estimate; Male: - 45 to 64 years: - Less than 9th grade` +
        `Estimate; Male: - 35 to 44 years: - 9th to 12th grade, no diploma` +
        `Estimate; Male: - 45 to 64 years: - 9th to 12th grade, no diploma`,
      num_no_hs.female =
        num_no_hs_young.female +
        `Estimate; Female: - 35 to 44 years: - Less than 9th grade` +
        `Estimate; Female: - 45 to 64 years: - Less than 9th grade` +
        `Estimate; Female: - 35 to 44 years: - 9th to 12th grade, no diploma` +
        `Estimate; Female: - 45 to 64 years: - 9th to 12th grade, no diploma`,
      num_no_hs =
        num_no_hs.male + num_no_hs.female,

      num_hs_young.male =
        `Estimate; Male: - 25 to 34 years: - High school graduate (includes equivalency)`,
      num_hs_young.female =
        `Estimate; Female: - 25 to 34 years: - High school graduate (includes equivalency)`,
      num_hs_young =
        num_hs_young.male + num_hs_young.female,

      num_hs.male =
        num_hs_young.male +
        `Estimate; Male: - 35 to 44 years: - High school graduate (includes equivalency)` +
        `Estimate; Male: - 45 to 64 years: - High school graduate (includes equivalency)`,
      num_hs.female =
        num_hs_young.female +
        `Estimate; Female: - 35 to 44 years: - High school graduate (includes equivalency)` +
        `Estimate; Female: - 45 to 64 years: - High school graduate (includes equivalency)`,
      num_hs =
        num_hs.male + num_hs.female,

      num_some_col_young.male =
        `Estimate; Male: - 25 to 34 years: - Some college, no degree`,
      num_some_col_young.female =
        `Estimate; Female: - 25 to 34 years: - Some college, no degree`,
      num_some_col_young =
        num_some_col_young.male + num_some_col_young.female,

      num_some_col.male =
        num_some_col_young.male +
        `Estimate; Male: - 35 to 44 years: - Some college, no degree` +
        `Estimate; Male: - 45 to 64 years: - Some college, no degree`,
      num_some_col.female =
        num_some_col_young.female +
        `Estimate; Female: - 35 to 44 years: - Some college, no degree` +
        `Estimate; Female: - 45 to 64 years: - Some college, no degree`,
      num_some_col =
        num_some_col.male + num_some_col.female,

      num_assoc_young.male =
        `Estimate; Male: - 25 to 34 years: - Associate's degree`,
      num_assoc_young.female =
        `Estimate; Female: - 25 to 34 years: - Associate's degree`,
      num_assoc_young =
        num_assoc_young.male + num_assoc_young.female,

      num_assoc.male =
        num_assoc_young.male +
        `Estimate; Male: - 35 to 44 years: - Associate's degree` +
        `Estimate; Male: - 45 to 64 years: - Associate's degree`,
      num_assoc.female =
        num_assoc_young.female +
        `Estimate; Female: - 35 to 44 years: - Associate's degree` +
        `Estimate; Female: - 45 to 64 years: - Associate's degree`,
      num_assoc =
        num_assoc.male + num_assoc.female,

      num_bach_young.male =
        `Estimate; Male: - 25 to 34 years: - Bachelor's degree`,
      num_bach_young.female =
        `Estimate; Female: - 25 to 34 years: - Bachelor's degree`,
      num_bach_young =
        num_bach_young.male + num_bach_young.female,

      num_bach.male =
        num_bach_young.male +
        `Estimate; Male: - 35 to 44 years: - Bachelor's degree` +
        `Estimate; Male: - 45 to 64 years: - Bachelor's degree`,
      num_bach.female =
        num_bach_young.female +
        `Estimate; Female: - 35 to 44 years: - Bachelor's degree` +
        `Estimate; Female: - 45 to 64 years: - Bachelor's degree`,
      num_bach =
        num_bach.male + num_bach.female,

      num_grad_young.male =
        `Estimate; Male: - 25 to 34 years: - Graduate or professional degree`,
      num_grad_young.female =
        `Estimate; Female: - 25 to 34 years: - Graduate or professional degree`,
      num_grad_young =
        num_grad_young.male + num_grad_young.female,

      num_grad.male =
        num_grad_young.male +
        `Estimate; Male: - 35 to 44 years: - Graduate or professional degree` +
        `Estimate; Male: - 45 to 64 years: - Graduate or professional degree`,
      num_grad.female =
        num_grad_young.female +
        `Estimate; Female: - 35 to 44 years: - Graduate or professional degree` +
        `Estimate; Female: - 45 to 64 years: - Graduate or professional degree`,
      num_grad =
        num_grad.male + num_grad.female,

      num_assoc_plus_young.male = num_assoc_young.male + num_bach_young.male + num_grad_young.male,
      num_assoc_plus_young.female = num_assoc_young.female + num_bach_young.female + num_grad_young.female,
      num_assoc_plus_young = num_assoc_plus_young.male + num_assoc_plus_young.female,

      num_assoc_plus.male = num_assoc.male + num_bach.male + num_grad.male,
      num_assoc_plus.female = num_assoc.female + num_bach.female + num_grad.female,
      num_assoc_plus = num_assoc_plus.male +  num_assoc_plus.female,

      num_bach_plus_young.male = num_bach_young.male + num_grad_young.male,
      num_bach_plus_young.female = num_bach_young.female + num_grad_young.female,
      num_bach_plus_young = num_bach_young.male + num_bach_plus_young.female,

      num_bach_plus.male = num_bach.male + num_grad.male,
      num_bach_plus.female = num_bach.female + num_grad.female,
      num_bach_plus = num_bach_plus.male + num_bach_plus.female)

  df[[geog]] <- df$id

  df %<>% select(-id)

  df
}

process_degree <- function(df){
  df %<>%

    #filter to peers
    pull_peers_FIPS(add_info = FALSE) %>%

    #create sex column
    reshape_sex() %>%

    #add race
    mutate(race = "total") %>%

    #reorder variables
    select(
      FIPS, year, race, sex, total, total_young,
      num_no_hs, num_hs, num_some_col, num_assoc, num_bach, num_grad,
      num_assoc_plus, num_bach_plus,
      num_no_hs_young, num_hs_young, num_some_col_young, num_assoc_young,
      num_bach_young, num_grad_young, num_assoc_plus_young, num_bach_plus_young) %>%

    #merge STL counties
    stl_merge(total:num_bach_plus_young, method = "sum") %>%

    #calculate percentages
    mutate_at(vars(num_no_hs:num_bach_plus), funs(. / total * 100)) %>%
    mutate_at(vars(num_no_hs_young:num_bach_plus_young),
              funs(. / total_young * 100)) %>%

    #remove unnecessary variables
    select(-total, -total_young) %>%

    #rename variables to "per"
    rename_at(vars(contains("num")), funs(sub("num_", "", .)))

  df
}

degree_00 <- read_csv(path %p% "DEC_00_SF3_PCT025_with_ann.csv", skip = 1)
degree_05 <- acs_time(path %p% "B15001")
degree_map <- read_csv(path %p% "ACS_17_5YR_B15001_with_ann.csv", skip = 1)

degree_00 %<>% process_00()
degree_05 %<>% process_05("FIPS")
degree_map %<>%
  mutate(year = 2017) %>%
  process_05("Id")

degree_tot <- bind_rows(degree_00, degree_05)

degree_tot %<>% process_degree()
degree_map %<>%
  transmute(
    Id,
    assoc_plus = num_assoc_plus / total * 100,
    assoc_plus_young = num_assoc_plus_young / total_young * 100,
    bach_plus = num_bach_plus / total * 100,
    bach_plus_young = num_bach_plus_young / total_young * 100,
    grad = num_grad / total * 100,
    grad_plus_young = num_grad_young / total_young * 100)


# By Race

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

process_degree_race <- function(df, race_name){

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
degree_white_00 <- read_csv(path %p% "DEC_00_SF3_P148I_with_ann.csv", skip = 1)
degree_white_05 <- acs_time(path %p% "B15002H/Y05", starting_year = 2005)
degree_white_08 <- acs_time(path %p% "B15002H/Y08", starting_year = 2008)

degree_white_00 %<>% process_00()
degree_white_05 %<>% process_05()
degree_white_08 %<>% process_08()

degree_white <- bind_rows(degree_white_00, degree_white_05, degree_white_08)

degree_white %<>% process_degree_race(race_name = "white")


#Black
degree_black_00 <- read_csv(path %p% "DEC_00_SF3_P148B_with_ann.csv", skip = 1)
degree_black_05 <- acs_time(path %p% "B15002B/Y05", starting_year = 2005)
degree_black_08 <- acs_time(path %p% "B15002B/Y08", starting_year = 2008)

degree_black_00 %<>% process_00()
degree_black_05 %<>% process_05()
degree_black_08 %<>% process_08()

degree_black <- bind_rows(degree_black_00, degree_black_05, degree_black_08)

degree_black %<>% process_degree_race(race_name = "black")


#Hispanic
degree_hispanic_00 <- read_csv(path %p% "DEC_00_SF3_P148H_with_ann.csv", skip = 1)
degree_hispanic_05 <- acs_time(path %p% "B15002I/Y05", starting_year = 2005)
degree_hispanic_08 <- acs_time(path %p% "B15002I/Y08", starting_year = 2008)

degree_hispanic_00 %<>% process_00()
degree_hispanic_05 %<>% process_05()
degree_hispanic_08 %<>% process_08()

degree_hispanic = bind_rows(degree_hispanic_00, degree_hispanic_05, degree_hispanic_08)

degree_hispanic %<>% process_degree_race(race_name = "hispanic")


# Young Adults
if("educ_micro.feather" %!in% list.files(path %p% "..")){
  acs <- read_csv(path %p% "../educ_micro.csv")
  write_feather(acs, path %p% "../educ_micro.feather")
} else{
  acs <- read_feather(path %p% "../educ_micro.feather")
}

acs %<>% process_microdata()

acs %<>%
  filter(age %in% 25:34) %>%
  mutate(
    no_hs      = if_else(educ == "no_hs", 1, 0),
    hs         = if_else(educ == "hs", 1, 0),
    some_col   = if_else(educ == "some_col", 1, 0),
    assoc      = if_else(educ == "assoc", 1, 0),
    bach       = if_else(educ == "bach", 1, 0),
    grad       = if_else(educ == "grad", 1, 0),
    assoc_plus = if_else(educ == "assoc_plus", 1, 0),
    bach_plus  = if_else(educ == "bach_plus", 1, 0))

survey_young <- svydesign(ids = ~1, weights = ~PERWT, data = acs)

educ_vars <- c("no_hs", "hs", "some_col", "assoc", "bach",
               "grad", "assoc_plus", "bach_plus")

for(i in 1:length(educ_vars)){

  svy_results <- svy_race_sex(survey_young, educ_vars[i], cross = FALSE)

  if(i == 1) degree_young <- svy_results
  else degree_young %<>% bind_df(svy_results)
}

svy_race_sex(survey_young, "no_hs", cross = FALSE)

#combine data frames
degree <- bind_rows(degree_tot, degree_white, degree_black,
                    degree_hispanic, degree_young)

usethis::use_data(degree, overwrite = TRUE)
usethis::use_data(degree_map, overwrite = TRUE)

rm(degree_00, degree_05, degree_tot, process_degree, process_degree_race,
   degree_white_00, degree_white_05, degree_white_08, degree_white,
   degree_black_00, degree_black_05, degree_black_08, degree_black,
   degree_hispanic_00, degree_hispanic_05, degree_hispanic_08, degree_hispanic,
   process_00, process_05, process_08, acs, survey, path)
