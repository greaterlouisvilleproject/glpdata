library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/educ/attainment/"

degree_00 <- read_csv(path %+% "DEC_00_SF3_PCT025_with_ann.csv", skip = 1)
degree_05 <- acs_time(path %+% "B15001")

degree_00 %<>%
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

degree_05 %<>%
  transmute(
    FIPS,
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

degree <- bind_rows(degree_00, degree_05)

degree %<>%

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

usethis::use_data(degree, overwrite = TRUE)

rm(path, degree_00, degree_05, degree)
