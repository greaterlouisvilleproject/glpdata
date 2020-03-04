library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/education/degree/"

# Total
process_00 <- function(df){
  df %<>%
    transmute(
      FIPS = as.character(as.numeric(Id2)),
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

      no_hs_young.male =
        `Male: - 25 to 34 years: - Less than 9th grade` +
        `Male: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      no_hs_young.female =
        `Female: - 25 to 34 years: - Less than 9th grade` +
        `Female: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      no_hs_young =
        no_hs_young.male + no_hs_young.female,

      no_hs.male =
        no_hs_young.male +
        `Male: - 35 to 44 years: - Less than 9th grade` +
        `Male: - 45 to 64 years: - Less than 9th grade` +
        `Male: - 35 to 44 years: - 9th to 12th grade, no diploma` +
        `Male: - 45 to 64 years: - 9th to 12th grade, no diploma`,
      no_hs.female =
        no_hs_young.female +
        `Female: - 35 to 44 years: - Less than 9th grade` +
        `Female: - 45 to 64 years: - Less than 9th grade` +
        `Female: - 35 to 44 years: - 9th to 12th grade, no diploma` +
        `Female: - 45 to 64 years: - 9th to 12th grade, no diploma`,
      no_hs =
        no_hs.male + no_hs.female,

      hs_young.male =
        `Male: - 25 to 34 years: - High school graduate (includes equivalency)`,
      hs_young.female =
        `Female: - 25 to 34 years: - High school graduate (includes equivalency)`,
      hs_young = hs_young.male + hs_young.female,

      hs.male =
        hs_young.male +
        `Male: - 35 to 44 years: - High school graduate (includes equivalency)` +
        `Male: - 45 to 64 years: - High school graduate (includes equivalency)`,
      hs.female =
        hs_young.female +
        `Female: - 35 to 44 years: - High school graduate (includes equivalency)` +
        `Female: - 45 to 64 years: - High school graduate (includes equivalency)`,
      hs = hs.male + hs.female,

      some_col_young.male =
        `Male: - 25 to 34 years: - Some college, no degree`,
      some_col_young.female =
        `Female: - 25 to 34 years: - Some college, no degree`,
      some_col_young =
        some_col_young.male + some_col_young.female,

      some_col.male =
        some_col_young.male +
        `Male: - 35 to 44 years: - Some college, no degree` +
        `Male: - 45 to 64 years: - Some college, no degree`,
      some_col.female =
        `Female: - 35 to 44 years: - Some college, no degree` +
        `Female: - 45 to 64 years: - Some college, no degree`,
      some_col = some_col.male + some_col.female,

      assoc_young.male =
        `Male: - 25 to 34 years: - Associate degree`,
      assoc_young.female =
        `Female: - 25 to 34 years: - Associate degree`,
      assoc_young = assoc_young.male + assoc_young.female,

      assoc.male =
        assoc_young.male +
        `Male: - 35 to 44 years: - Associate degree` +
        `Male: - 45 to 64 years: - Associate degree`,
      assoc.female =
        assoc_young.female +
        `Female: - 35 to 44 years: - Associate degree` +
        `Female: - 45 to 64 years: - Associate degree`,
      assoc = assoc.male + assoc.female,

      bach_young.male =
        `Male: - 25 to 34 years: - Bachelor's degree`,
      bach_young.female =
        `Female: - 25 to 34 years: - Bachelor's degree`,
      bach_young = bach_young.male + bach_young.female,

      bach.male =
        bach_young.male +
        `Male: - 35 to 44 years: - Bachelor's degree` +
        `Male: - 45 to 64 years: - Bachelor's degree`,
      bach.female =
        bach_young.female +
        `Female: - 35 to 44 years: - Bachelor's degree` +
        `Female: - 45 to 64 years: - Bachelor's degree`,
      bach = bach.male + bach.female,

      grad_young.male =
        `Male: - 25 to 34 years: - Graduate or professional degree`,
      grad_young.female =
        `Female: - 25 to 34 years: - Graduate or professional degree`,
      grad_young =
        grad_young.male + grad_young.female,

      grad.male =
        grad_young.male +
        `Male: - 35 to 44 years: - Graduate or professional degree` +
        `Male: - 45 to 64 years: - Graduate or professional degree`,
      grad.female =
        grad_young.female +
        `Female: - 35 to 44 years: - Graduate or professional degree` +
        `Female: - 45 to 64 years: - Graduate or professional degree`,
      grad =
        grad.male + grad.female,

      assoc_plus_young.male = assoc_young.male + bach_young.male + grad_young.male,
      assoc_plus_young.female = assoc_young.female + bach_young.female + grad_young.female,
      assoc_plus_young = assoc_plus_young.male + assoc_plus_young.female,

      assoc_plus.male = assoc.male + bach.male + grad.male,
      assoc_plus.female = assoc.female + bach.female + grad.female,
      assoc_plus = assoc_plus.male +  assoc_plus.female,

      bach_plus_young.male = bach_young.male + grad_young.male,
      bach_plus_young.female = bach_young.female + grad_young.female,
      bach_plus_young = bach_young.male + bach_plus_young.female,

      bach_plus.male = bach.male + grad.male,
      bach_plus.female = bach.female + grad.female,
      bach_plus = bach_plus.male + bach_plus.female)

  df
}
process_05 <- function(df, geog){

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "Id2", "tract" = "Id")

  df %>%
    transmute(
      !!geog := as.character(.data[[col_name]]),
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

      no_hs_young.male =
        `Estimate; Male: - 25 to 34 years: - Less than 9th grade` +
        `Estimate; Male: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      no_hs_young.female =
        `Estimate; Female: - 25 to 34 years: - Less than 9th grade` +
        `Estimate; Female: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      no_hs_young =
        no_hs_young.male + no_hs_young.female,

      no_hs.male =
        no_hs_young.male +
        `Estimate; Male: - 35 to 44 years: - Less than 9th grade` +
        `Estimate; Male: - 45 to 64 years: - Less than 9th grade` +
        `Estimate; Male: - 35 to 44 years: - 9th to 12th grade, no diploma` +
        `Estimate; Male: - 45 to 64 years: - 9th to 12th grade, no diploma`,
      no_hs.female =
        no_hs_young.female +
        `Estimate; Female: - 35 to 44 years: - Less than 9th grade` +
        `Estimate; Female: - 45 to 64 years: - Less than 9th grade` +
        `Estimate; Female: - 35 to 44 years: - 9th to 12th grade, no diploma` +
        `Estimate; Female: - 45 to 64 years: - 9th to 12th grade, no diploma`,
      no_hs =
        no_hs.male + no_hs.female,

      hs_young.male =
        `Estimate; Male: - 25 to 34 years: - High school graduate (includes equivalency)`,
      hs_young.female =
        `Estimate; Female: - 25 to 34 years: - High school graduate (includes equivalency)`,
      hs_young =
        hs_young.male + hs_young.female,

      hs.male =
        hs_young.male +
        `Estimate; Male: - 35 to 44 years: - High school graduate (includes equivalency)` +
        `Estimate; Male: - 45 to 64 years: - High school graduate (includes equivalency)`,
      hs.female =
        hs_young.female +
        `Estimate; Female: - 35 to 44 years: - High school graduate (includes equivalency)` +
        `Estimate; Female: - 45 to 64 years: - High school graduate (includes equivalency)`,
      hs =
        hs.male + hs.female,

      some_col_young.male =
        `Estimate; Male: - 25 to 34 years: - Some college, no degree`,
      some_col_young.female =
        `Estimate; Female: - 25 to 34 years: - Some college, no degree`,
      some_col_young =
        some_col_young.male + some_col_young.female,

      some_col.male =
        some_col_young.male +
        `Estimate; Male: - 35 to 44 years: - Some college, no degree` +
        `Estimate; Male: - 45 to 64 years: - Some college, no degree`,
      some_col.female =
        some_col_young.female +
        `Estimate; Female: - 35 to 44 years: - Some college, no degree` +
        `Estimate; Female: - 45 to 64 years: - Some college, no degree`,
      some_col =
        some_col.male + some_col.female,

      assoc_young.male =
        `Estimate; Male: - 25 to 34 years: - Associate's degree`,
      assoc_young.female =
        `Estimate; Female: - 25 to 34 years: - Associate's degree`,
      assoc_young =
        assoc_young.male + assoc_young.female,

      assoc.male =
        assoc_young.male +
        `Estimate; Male: - 35 to 44 years: - Associate's degree` +
        `Estimate; Male: - 45 to 64 years: - Associate's degree`,
      assoc.female =
        assoc_young.female +
        `Estimate; Female: - 35 to 44 years: - Associate's degree` +
        `Estimate; Female: - 45 to 64 years: - Associate's degree`,
      assoc =
        assoc.male + assoc.female,

      bach_young.male =
        `Estimate; Male: - 25 to 34 years: - Bachelor's degree`,
      bach_young.female =
        `Estimate; Female: - 25 to 34 years: - Bachelor's degree`,
      bach_young =
        bach_young.male + bach_young.female,

      bach.male =
        bach_young.male +
        `Estimate; Male: - 35 to 44 years: - Bachelor's degree` +
        `Estimate; Male: - 45 to 64 years: - Bachelor's degree`,
      bach.female =
        bach_young.female +
        `Estimate; Female: - 35 to 44 years: - Bachelor's degree` +
        `Estimate; Female: - 45 to 64 years: - Bachelor's degree`,
      bach =
        bach.male + bach.female,

      grad_young.male =
        `Estimate; Male: - 25 to 34 years: - Graduate or professional degree`,
      grad_young.female =
        `Estimate; Female: - 25 to 34 years: - Graduate or professional degree`,
      grad_young =
        grad_young.male + grad_young.female,

      grad.male =
        grad_young.male +
        `Estimate; Male: - 35 to 44 years: - Graduate or professional degree` +
        `Estimate; Male: - 45 to 64 years: - Graduate or professional degree`,
      grad.female =
        grad_young.female +
        `Estimate; Female: - 35 to 44 years: - Graduate or professional degree` +
        `Estimate; Female: - 45 to 64 years: - Graduate or professional degree`,
      grad =
        grad.male + grad.female,

      assoc_plus_young.male = assoc_young.male + bach_young.male + grad_young.male,
      assoc_plus_young.female = assoc_young.female + bach_young.female + grad_young.female,
      assoc_plus_young = assoc_plus_young.male + assoc_plus_young.female,

      assoc_plus.male = assoc.male + bach.male + grad.male,
      assoc_plus.female = assoc.female + bach.female + grad.female,
      assoc_plus = assoc_plus.male +  assoc_plus.female,

      bach_plus_young.male = bach_young.male + grad_young.male,
      bach_plus_young.female = bach_young.female + grad_young.female,
      bach_plus_young = bach_plus_young.male + bach_plus_young.female,

      bach_plus.male = bach.male + grad.male,
      bach_plus.female = bach.female + grad.female,
      bach_plus = bach_plus.male + bach_plus.female)
}
process_18 <- function(df, geog){

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "Id2", "tract" = "Id")

  df %>%
    transmute(
      !!geog := as.character(.data[[col_name]]),
      year,

      total_young.male =
        `Estimate..Total..Male..25 to 34 years`,
      total_young.female =
        `Estimate..Total..Female..25 to 34 years`,
      total_young =
        total_young.male + total_young.female,

      total.male =
        total_young.male +
        `Estimate..Total..Male..35 to 44 years` +
        `Estimate..Total..Male..45 to 64 years`,
      total.female =
        total_young.female +
        `Estimate..Total..Female..35 to 44 years` +
        `Estimate..Total..Female..45 to 64 years`,
      total =
        total.male + total.female,

      no_hs_young.male =
        `Estimate..Total..Male..25 to 34 years..Less than 9th grade` +
        `Estimate..Total..Male..25 to 34 years..9th to 12th grade, no diploma`,
      no_hs_young.female =
        `Estimate..Total..Female..25 to 34 years..Less than 9th grade` +
        `Estimate..Total..Female..25 to 34 years..9th to 12th grade, no diploma`,
      no_hs_young =
        no_hs_young.male + no_hs_young.female,

      no_hs.male =
        no_hs_young.male +
        `Estimate..Total..Male..35 to 44 years..Less than 9th grade` +
        `Estimate..Total..Male..45 to 64 years..Less than 9th grade` +
        `Estimate..Total..Male..35 to 44 years..9th to 12th grade, no diploma` +
        `Estimate..Total..Male..45 to 64 years..9th to 12th grade, no diploma`,
      no_hs.female =
        no_hs_young.female +
        `Estimate..Total..Female..35 to 44 years..Less than 9th grade` +
        `Estimate..Total..Female..45 to 64 years..Less than 9th grade` +
        `Estimate..Total..Female..35 to 44 years..9th to 12th grade, no diploma` +
        `Estimate..Total..Female..45 to 64 years..9th to 12th grade, no diploma`,
      no_hs =
        no_hs.male + no_hs.female,

      hs_young.male =
        `Estimate..Total..Male..25 to 34 years..High school graduate (includes equivalency)`,
      hs_young.female =
        `Estimate..Total..Female..25 to 34 years..High school graduate (includes equivalency)`,
      hs_young =
        hs_young.male + hs_young.female,

      hs.male =
        hs_young.male +
        `Estimate..Total..Male..35 to 44 years..High school graduate (includes equivalency)` +
        `Estimate..Total..Male..45 to 64 years..High school graduate (includes equivalency)`,
      hs.female =
        hs_young.female +
        `Estimate..Total..Female..35 to 44 years..High school graduate (includes equivalency)` +
        `Estimate..Total..Female..45 to 64 years..High school graduate (includes equivalency)`,
      hs =
        hs.male + hs.female,

      some_col_young.male =
        `Estimate..Total..Male..25 to 34 years..Some college, no degree`,
      some_col_young.female =
        `Estimate..Total..Female..25 to 34 years..Some college, no degree`,
      some_col_young =
        some_col_young.male + some_col_young.female,

      some_col.male =
        some_col_young.male +
        `Estimate..Total..Male..35 to 44 years..Some college, no degree` +
        `Estimate..Total..Male..45 to 64 years..Some college, no degree`,
      some_col.female =
        some_col_young.female +
        `Estimate..Total..Female..35 to 44 years..Some college, no degree` +
        `Estimate..Total..Female..45 to 64 years..Some college, no degree`,
      some_col =
        some_col.male + some_col.female,

      assoc_young.male =
        `Estimate..Total..Male..25 to 34 years..Associate's degree`,
      assoc_young.female =
        `Estimate..Total..Female..25 to 34 years..Associate's degree`,
      assoc_young =
        assoc_young.male + assoc_young.female,

      assoc.male =
        assoc_young.male +
        `Estimate..Total..Male..35 to 44 years..Associate's degree` +
        `Estimate..Total..Male..45 to 64 years..Associate's degree`,
      assoc.female =
        assoc_young.female +
        `Estimate..Total..Female..35 to 44 years..Associate's degree` +
        `Estimate..Total..Female..45 to 64 years..Associate's degree`,
      assoc =
        assoc.male + assoc.female,

      bach_young.male =
        `Estimate..Total..Male..25 to 34 years..Bachelor's degree`,
      bach_young.female =
        `Estimate..Total..Female..25 to 34 years..Bachelor's degree`,
      bach_young =
        bach_young.male + bach_young.female,

      bach.male =
        bach_young.male +
        `Estimate..Total..Male..35 to 44 years..Bachelor's degree` +
        `Estimate..Total..Male..45 to 64 years..Bachelor's degree`,
      bach.female =
        bach_young.female +
        `Estimate..Total..Female..35 to 44 years..Bachelor's degree` +
        `Estimate..Total..Female..45 to 64 years..Bachelor's degree`,
      bach =
        bach.male + bach.female,

      grad_young.male =
        `Estimate..Total..Male..25 to 34 years..Graduate or professional degree`,
      grad_young.female =
        `Estimate..Total..Female..25 to 34 years..Graduate or professional degree`,
      grad_young =
        grad_young.male + grad_young.female,

      grad.male =
        grad_young.male +
        `Estimate..Total..Male..35 to 44 years..Graduate or professional degree` +
        `Estimate..Total..Male..45 to 64 years..Graduate or professional degree`,
      grad.female =
        grad_young.female +
        `Estimate..Total..Female..35 to 44 years..Graduate or professional degree` +
        `Estimate..Total..Female..45 to 64 years..Graduate or professional degree`,
      grad =
        grad.male + grad.female,

      assoc_plus_young.male = assoc_young.male + bach_young.male + grad_young.male,
      assoc_plus_young.female = assoc_young.female + bach_young.female + grad_young.female,
      assoc_plus_young = assoc_plus_young.male + assoc_plus_young.female,

      assoc_plus.male = assoc.male + bach.male + grad.male,
      assoc_plus.female = assoc.female + bach.female + grad.female,
      assoc_plus = assoc_plus.male +  assoc_plus.female,

      bach_plus_young.male = bach_young.male + grad_young.male,
      bach_plus_young.female = bach_young.female + grad_young.female,
      bach_plus_young = bach_plus_young.male + bach_plus_young.female,

      bach_plus.male = bach.male + grad.male,
      bach_plus.female = bach.female + grad.female,
      bach_plus = bach_plus.male + bach_plus.female)
}
process_5yr <- function(df) {
  df %>%
    rename(
      `Estimate; Male: - 25 to 34 years: - High school graduate (includes equivalency)` =
        `Estimate; Male: - 25 to 34 years: - High school graduate, GED, or alternative`,
      `Estimate; Male: - 35 to 44 years: - High school graduate (includes equivalency)` =
        `Estimate; Male: - 35 to 44 years: - High school graduate, GED, or alternative`,
      `Estimate; Male: - 45 to 64 years: - High school graduate (includes equivalency)` =
        `Estimate; Male: - 45 to 64 years: - High school graduate, GED, or alternative`,
      `Estimate; Male: - 65 years and over: - High school graduate (includes equivalency)` =
        `Estimate; Male: - 65 years and over: - High school graduate, GED, or alternative`,
      `Estimate; Female: - 25 to 34 years: - High school graduate (includes equivalency)` =
        `Estimate; Female: - 25 to 34 years: - High school graduate, GED, or alternative`,
      `Estimate; Female: - 35 to 44 years: - High school graduate (includes equivalency)` =
        `Estimate; Female: - 35 to 44 years: - High school graduate, GED, or alternative`,
      `Estimate; Female: - 45 to 64 years: - High school graduate (includes equivalency)` =
        `Estimate; Female: - 45 to 64 years: - High school graduate, GED, or alternative`,
      `Estimate; Female: - 65 years and over: - High school graduate (includes equivalency)` =
        `Estimate; Female: - 65 years and over: - High school graduate, GED, or alternative`)
}
process_degree <- function(df){

  if (df_type(df) == "MSA") geog <- "MSA" else geog <- "FIPS"

  df %<>%

    #filter to peers
    pull_peers(add_info = FALSE) %>%

    #create sex column
    reshape_sex() %>%

    #add race
    mutate(race = "total") %>%

    #reorder variables
    select(
      !!geog, year, race, sex, total, total_young,
      no_hs, hs, some_col, assoc, bach, grad,
      assoc_plus, bach_plus,
      no_hs_young, hs_young, some_col_young, assoc_young,
      bach_young, grad_young, assoc_plus_young, bach_plus_young) %>%

  # merge STL counties
    {if (df_type(df) == "FIPS") stl_merge(., total:bach_plus_young, method = "sum") else .} %>%

    #calculate percentages
    mutate_at(vars(no_hs:bach_plus), ~ . / total * 100) %>%
    mutate_at(vars(no_hs_young:bach_plus_young), ~ . / total_young * 100) %>%

    #remove unnecessary variables
    select(-total, -total_young) %>%

    #rename variables to "per"
    rename_at(vars(contains("num")), funs(sub("", "", .)))

  df
}

process_05_55k <- function(df, geog){

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "Id2", "tract" = "Id")

  df %>%
    transmute(
      !!geog := as.character(.data[[col_name]]),
      year,

      total.25_34.male = `Estimate; Male: - 25 to 34 years:`,
      total.35_44.male = `Estimate; Male: - 35 to 44 years:`,
      total.45_64.male =   `Estimate; Male: - 45 to 64 years:`,

      total.25_34.female = `Estimate; Female: - 25 to 34 years:`,
      total.35_44.female = `Estimate; Female: - 35 to 44 years:`,
      total.45_64.female = `Estimate; Female: - 45 to 64 years:`,

      no_hs.25_34.male =
        `Estimate; Male: - 25 to 34 years: - Less than 9th grade` +
        `Estimate; Male: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      no_hs.35_44.male =
        `Estimate; Male: - 35 to 44 years: - Less than 9th grade` +
        `Estimate; Male: - 35 to 44 years: - 9th to 12th grade, no diploma`,
      no_hs.45_64.male =
        `Estimate; Male: - 45 to 64 years: - Less than 9th grade` +
        `Estimate; Male: - 45 to 64 years: - 9th to 12th grade, no diploma`,

      no_hs.25_34.female =
        `Estimate; Female: - 25 to 34 years: - Less than 9th grade` +
        `Estimate; Female: - 25 to 34 years: - 9th to 12th grade, no diploma`,
      no_hs.35_44.female =
        `Estimate; Female: - 35 to 44 years: - Less than 9th grade` +
        `Estimate; Female: - 35 to 44 years: - 9th to 12th grade, no diploma`,
      no_hs.45_64.female =
        `Estimate; Female: - 45 to 64 years: - Less than 9th grade` +
        `Estimate; Female: - 45 to 64 years: - 9th to 12th grade, no diploma`,

      hs.25_34.male = `Estimate; Male: - 25 to 34 years: - High school graduate (includes equivalency)`,
      hs.35_44.male = `Estimate; Male: - 35 to 44 years: - High school graduate (includes equivalency)`,
      hs.45_64.male = `Estimate; Male: - 45 to 64 years: - High school graduate (includes equivalency)`,

      hs.25_34.female = `Estimate; Female: - 25 to 34 years: - High school graduate (includes equivalency)`,
      hs.35_44.female = `Estimate; Female: - 35 to 44 years: - High school graduate (includes equivalency)`,
      hs.45_64.female = `Estimate; Female: - 45 to 64 years: - High school graduate (includes equivalency)`,

      some_col.25_34.male = `Estimate; Male: - 25 to 34 years: - Some college, no degree`,
      some_col.35_44.male = `Estimate; Male: - 35 to 44 years: - Some college, no degree`,
      some_col.45_64.male = `Estimate; Male: - 45 to 64 years: - Some college, no degree`,

      some_col.25_34.female = `Estimate; Female: - 25 to 34 years: - Some college, no degree`,
      some_col.35_44.female = `Estimate; Female: - 35 to 44 years: - Some college, no degree`,
      some_col.45_64.female = `Estimate; Female: - 45 to 64 years: - Some college, no degree`,

      assoc.25_34.male = `Estimate; Male: - 25 to 34 years: - Associate's degree`,
      assoc.35_44.male = `Estimate; Male: - 35 to 44 years: - Associate's degree`,
      assoc.45_64.male = `Estimate; Male: - 45 to 64 years: - Associate's degree`,

      assoc.25_34.female = `Estimate; Female: - 25 to 34 years: - Associate's degree`,
      assoc.35_44.female = `Estimate; Female: - 35 to 44 years: - Associate's degree`,
      assoc.45_64.female = `Estimate; Female: - 45 to 64 years: - Associate's degree`,

      bach.25_34.male = `Estimate; Male: - 25 to 34 years: - Bachelor's degree`,
      bach.35_44.male = `Estimate; Male: - 35 to 44 years: - Bachelor's degree`,
      bach.45_64.male = `Estimate; Male: - 45 to 64 years: - Bachelor's degree`,

      bach.25_34.female = `Estimate; Female: - 25 to 34 years: - Bachelor's degree`,
      bach.35_44.female = `Estimate; Female: - 35 to 44 years: - Bachelor's degree`,
      bach.45_64.female = `Estimate; Female: - 45 to 64 years: - Bachelor's degree`,

      grad.25_34.male = `Estimate; Male: - 25 to 34 years: - Graduate or professional degree`,
      grad.35_44.male = `Estimate; Male: - 35 to 44 years: - Graduate or professional degree`,
      grad.45_64.male = `Estimate; Male: - 45 to 64 years: - Graduate or professional degree`,

      grad.25_34.female = `Estimate; Female: - 25 to 34 years: - Graduate or professional degree`,
      grad.35_44.female = `Estimate; Female: - 35 to 44 years: - Graduate or professional degree`,
      grad.45_64.female = `Estimate; Female: - 45 to 64 years: - Graduate or professional degree`)
}
process_18_55k <- function(df, geog){

  df %>%
    transmute(
      !!geog := as.character(.data[[geog]]),
      year,

      total.25_34.male = `Estimate..Total..Male..25 to 34 years`,
      total.35_44.male = `Estimate..Total..Male..35 to 44 years`,
      total.45_64.male = `Estimate..Total..Male..45 to 64 years`,
      total.25_34.female = `Estimate..Total..Female..25 to 34 years`,
      total.35_44.female = `Estimate..Total..Female..35 to 44 years`,
      total.45_64.female = `Estimate..Total..Female..45 to 64 years`,

      no_hs.25_34.male =
        `Estimate..Total..Male..25 to 34 years..Less than 9th grade` +
        `Estimate..Total..Male..25 to 34 years..9th to 12th grade, no diploma`,
      no_hs.35_44.male =
        `Estimate..Total..Male..35 to 44 years..Less than 9th grade` +
        `Estimate..Total..Male..35 to 44 years..9th to 12th grade, no diploma`,
      no_hs.45_64.male =
        `Estimate..Total..Male..45 to 64 years..Less than 9th grade` +
        `Estimate..Total..Male..45 to 64 years..9th to 12th grade, no diploma`,
      no_hs.25_34.female =
        `Estimate..Total..Female..25 to 34 years..Less than 9th grade` +
        `Estimate..Total..Female..25 to 34 years..9th to 12th grade, no diploma`,
      no_hs.35_44.female =
        `Estimate..Total..Female..35 to 44 years..Less than 9th grade` +
        `Estimate..Total..Female..35 to 44 years..9th to 12th grade, no diploma`,
      no_hs.45_64.female =
        `Estimate..Total..Female..45 to 64 years..Less than 9th grade` +
        `Estimate..Total..Female..45 to 64 years..9th to 12th grade, no diploma`,

      hs.25_34.male = `Estimate..Total..Male..25 to 34 years..High school graduate (includes equivalency)`,
      hs.35_44.male = `Estimate..Total..Male..35 to 44 years..High school graduate (includes equivalency)`,
      hs.45_64.male = `Estimate..Total..Male..45 to 64 years..High school graduate (includes equivalency)`,
      hs.25_34.female = `Estimate..Total..Female..25 to 34 years..High school graduate (includes equivalency)`,
      hs.35_44.female = `Estimate..Total..Female..35 to 44 years..High school graduate (includes equivalency)`,
      hs.45_64.female = `Estimate..Total..Female..45 to 64 years..High school graduate (includes equivalency)`,

      some_col.25_34.male = `Estimate..Total..Male..25 to 34 years..Some college, no degree`,
      some_col.35_44.male = `Estimate..Total..Male..35 to 44 years..Some college, no degree`,
      some_col.45_64.male = `Estimate..Total..Male..45 to 64 years..Some college, no degree`,
      some_col.25_34.female = `Estimate..Total..Female..25 to 34 years..Some college, no degree`,
      some_col.35_44.female = `Estimate..Total..Female..35 to 44 years..Some college, no degree`,
      some_col.45_64.female = `Estimate..Total..Female..45 to 64 years..Some college, no degree`,

      assoc.25_34.male = `Estimate..Total..Male..25 to 34 years..Associate's degree`,
      assoc.35_44.male = `Estimate..Total..Male..35 to 44 years..Associate's degree`,
      assoc.45_64.male = `Estimate..Total..Male..45 to 64 years..Associate's degree`,
      assoc.25_34.female = `Estimate..Total..Female..25 to 34 years..Associate's degree`,
      assoc.35_44.female = `Estimate..Total..Female..35 to 44 years..Associate's degree`,
      assoc.45_64.female = `Estimate..Total..Female..45 to 64 years..Associate's degree`,

      bach.25_34.male = `Estimate..Total..Male..25 to 34 years..Bachelor's degree`,
      bach.35_44.male = `Estimate..Total..Male..35 to 44 years..Bachelor's degree`,
      bach.45_64.male = `Estimate..Total..Male..45 to 64 years..Bachelor's degree`,
      bach.25_34.female = `Estimate..Total..Female..25 to 34 years..Bachelor's degree`,
      bach.35_44.female = `Estimate..Total..Female..35 to 44 years..Bachelor's degree`,
      bach.45_64.female = `Estimate..Total..Female..45 to 64 years..Bachelor's degree`,

      grad.25_34.male = `Estimate..Total..Male..25 to 34 years..Graduate or professional degree`,
      grad.35_44.male = `Estimate..Total..Male..35 to 44 years..Graduate or professional degree`,
      grad.45_64.male = `Estimate..Total..Male..45 to 64 years..Graduate or professional degree`,
      grad.25_34.female = `Estimate..Total..Female..25 to 34 years..Graduate or professional degree`,
      grad.35_44.female = `Estimate..Total..Female..35 to 44 years..Graduate or professional degree`,
      grad.45_64.female = `Estimate..Total..Female..45 to 64 years..Graduate or professional degree`)
}
process_degree_55k <- function(df){

  df %>%

    #gather columns
    gather(-FIPS, -year, key = "variable", value = "value") %>%

    #divide columns at "."
    separate(variable, c("variable", "age", "sex"), "\\.", extra = "drop", fill = "right")

}

# read in data
degree_00      <- read_csv(path %p% "DEC_00_SF3_PCT025_with_ann.csv", skip = 1)
degree_05      <- acs_time(path %p% "B15001/05", additional_counties = "21067")
degree_18      <- acs_time(path %p% "B15001/18", starting_year = 2018, additional_counties = "21067")
degree_map     <- read_csv(path %p% "ACS_17_5YR_B15001_with_ann.csv", skip = 1)
degree_05_5yr  <- acs_time(path %p% "B15001_5yr", geog = "MSA", starting_year = 2007)
degree_msa_1yr <- read_csv(path %p% "MSA/ACS_17_1YR_B15001_with_ann.csv", skip = 1)
degree_national <- any_time(path %p% "national/B15001", starting_year = 2010, skip = 1)

# process data
degree_05_55k <- degree_05 %>% process_05_55k("FIPS")
degree_18_55k <- degree_18 %>% process_18_55k("FIPS")
degree_national %<>% rename_all( ~ str_replace_all(., "!", ".")) %>% process_18_55k("id")

degree_00      %<>% process_00()
degree_05      %<>% process_05("FIPS")
degree_18      %<>% process_18("FIPS")
degree_map     %<>% mutate(year = 2015) %>% process_05("tract")
degree_00_msa  <- degree_00 %>% sum_FIPS_to_MSA()
degree_05_5yr  %<>% process_5yr() %>% process_05("FIPS") %>% sum_FIPS_to_MSA()
degree_msa_1yr %<>% mutate(year = 2017) %>% process_05("MSA")

# create final data frames
degree_tot     <- bind_rows(degree_00, degree_05, degree_18)
degree_msa_1yr <- bind_rows(degree_00_msa, degree_msa_1yr)
degree_msa_5yr <- bind_rows(degree_00_msa, degree_05_5yr)
degree_55k     <- bind_rows(degree_05_55k, degree_18_55k)

degree_tot     %<>% process_degree()
degree_msa_1yr %<>% process_degree()
degree_msa_5yr %<>% process_degree()
degree_55k     %<>%
  process_degree_55k() %>%
  pull_peers(add_info = TRUE, additional_counties = "21067") %>%
  mutate(
    city = replace(city, FIPS == "21067", "Lexington"),
    baseline = replace(baseline, FIPS == "21067", 1)) %>%
  select(-FIPS, -current, -baseline) %>%
  filter(year >= 2008)

degree_map %>%
  process_map(c("assoc_plus", "bach_plus", "grad"),
              "total", return_name = "degree_young") %>%
  list2env(.GlobalEnv)

degree_map %>%
  process_map(c("assoc_plus_young", "bach_plus_young", "grad_young"),
              "total_young", return_name = "degree") %>%
  list2env(.GlobalEnv)

degree_tract <- bind_df(degree_tract, degree_young$df_tract)
degree_nh    <- bind_df(degree_nh,    degree_young$df_nh)
degree_muw   <- bind_df(degree_muw,   degree_young$df_muw)

# By Race
process_00 <- function(df) {
  df <- df %>%
    transmute(
      FIPS = as.character(as.numeric(Id2)),
      year = 2000,

      #total
      total.male = `Male:`,
      total.female = `Female:`,
      total = total.male + total.female,

      #no high school
      no_hs.male =
        `Male: - Less than 9th grade` +
        `Male: - 9th to 12th grade, no diploma`,
      no_hs.female =
        `Female: - Less than 9th grade` +
        `Female: - 9th to 12th grade, no diploma`,
      no_hs =
        no_hs.male + no_hs.female,

      #high school
      hs.male =
        `Male: - High school graduate (includes equivalency)`,
      hs.female =
        `Female: - High school graduate (includes equivalency)`,
      hs =
        hs.male + hs.female,

      #some college
      some_col.male =
        `Male: - Some college, no degree`,
      some_col.female =
        `Female: - Some college, no degree`,
      some_col =
        some_col.male + some_col.female,

      #associate
      assoc.male =
        `Male: - Associate degree`,
      assoc.female =
        `Female: - Associate degree`,
      assoc =
        assoc.male + assoc.female,

      #bachelor's
      bach.male =
        `Male: - Bachelor's degree`,
      bach.female =
        `Female: - Bachelor's degree`,
      bach =
        bach.male + bach.female,

      #grad
      grad.male =
        `Male: - Graduate or professional degree`,
      grad.female =
        `Female: - Graduate or professional degree`,
      grad =
        grad.male + grad.female,

      assoc_plus.male = assoc.male + bach.male + grad.male,
      assoc_plus.female = assoc.female + bach.female + grad.female,
      assoc_plus = assoc_plus.male + assoc_plus.female,

      bach_plus.male = bach.male + grad.male,
      bach_plus.female = bach.female + grad.female,
      bach_plus = bach_plus.male + bach_plus.female)

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
      no_hs.male =
        `Estimate; Male: - Less than 9th grade` +
        `Estimate; Male: - 9th to 12th grade, no diploma`,
      no_hs.female =
        `Estimate; Female: - Less than 9th grade` +
        `Estimate; Female: - 9th to 12th grade, no diploma`,
      no_hs =
        no_hs.male + no_hs.female,

      #high school
      hs.male =
        `Estimate; Male: - High school graduate (includes equivalency)`,
      hs.female =
        `Estimate; Female: - High school graduate (includes equivalency)`,
      hs =
        hs.male + hs.female,

      #some college
      some_col.male =
        `Estimate; Male: - Some college, no degree`,
      some_col.female =
        `Estimate; Female: - Some college, no degree`,
      some_col =
        some_col.male + some_col.female,

      #associate
      assoc.male =
        `Estimate; Male: - Associate's degree`,
      assoc.female =
        `Estimate; Female: - Associate's degree`,
      assoc =
        assoc.male + assoc.female,

      #bachelor's
      bach.male =
        `Estimate; Male: - Bachelor's degree`,
      bach.female =
        `Estimate; Female: - Bachelor's degree`,
      bach =
        bach.male + bach.female,

      #grad
      grad.male =
        `Estimate; Male: - Graduate degree`,
      grad.female =
        `Estimate; Female: - Graduate degree`,
      grad =
        grad.male + grad.female,

      assoc_plus.male = assoc.male + bach.male + grad.male,
      assoc_plus.female = assoc.female + bach.female + grad.female,
      assoc_plus = assoc_plus.male + assoc_plus.female,

      bach_plus.male = bach.male + grad.male,
      bach_plus.female = bach.female + grad.female,
      bach_plus = bach_plus.male + bach_plus.female)

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
      no_hs.male =
        `Estimate; Male: - Less than 9th grade` +
        `Estimate; Male: - 9th to 12th grade, no diploma`,
      no_hs.female =
        `Estimate; Female: - Less than 9th grade` +
        `Estimate; Female: - 9th to 12th grade, no diploma`,
      no_hs =
        no_hs.male + no_hs.female,

      #high school
      hs.male =
        `Estimate; Male: - Regular high school diploma` +
        `Estimate; Male: - GED or alternative credential`,
      hs.female =
        `Estimate; Female: - Regular high school diploma` +
        `Estimate; Female: - GED or alternative credential`,
      hs =
        hs.male + hs.female,

      some_col.male =
        `Estimate; Male: - Some college, no degree`,
      some_col.female =
        `Estimate; Female: - Some college, no degree`,
      some_col =
        some_col.male + some_col.female,

      assoc.male =
        `Estimate; Male: - Associate's degree`,
      assoc.female =
        `Estimate; Female: - Associate's degree`,
      assoc =
        assoc.male + assoc.female,

      bach.male =
        `Estimate; Male: - Bachelor's degree`,
      bach.female =
        `Estimate; Female: - Bachelor's degree`,
      bach =
        bach.male + bach.female,

      grad.male =
        `Estimate; Male: - Graduate degree`,
      grad.female =
        `Estimate; Female: - Graduate degree`,
      grad =
        grad.male + grad.female,

      assoc_plus.male = assoc.male + bach.male + grad.male,
      assoc_plus.female = assoc.female + bach.female + grad.female,
      assoc_plus = assoc_plus.male + assoc_plus.female,

      bach_plus.male = bach.male + grad.male,
      bach_plus.female = bach.female + grad.female,
      bach_plus = bach_plus.male + bach_plus.female)

  df
}
process_18 <- function(df) {
  df <- df %>%
    mutate_at(vars(-FIPS), as.numeric) %>%
    transmute(
      FIPS, year,

      total.male = `Estimate..Total..Male`,
      total.female = `Estimate..Total..Female`,
      total = `Estimate..Total`,

      #no high school
      no_hs.male =
        `Estimate..Total..Male..Less than 9th grade` +
        `Estimate..Total..Male..9th to 12th grade, no diploma`,
      no_hs.female =
        `Estimate..Total..Female..Less than 9th grade` +
        `Estimate..Total..Female..9th to 12th grade, no diploma`,
      no_hs =
        no_hs.male + no_hs.female,

      #high school
      hs.male =
        `Estimate..Total..Male..Regular high school diploma` +
        `Estimate..Total..Male..GED or alternative credential`,
      hs.female =
        `Estimate..Total..Female..Regular high school diploma` +
        `Estimate..Total..Female..GED or alternative credential`,
      hs =
        hs.male + hs.female,

      some_col.male =
        `Estimate..Total..Male..Some college, no degree`,
      some_col.female =
        `Estimate..Total..Female..Some college, no degree`,
      some_col =
        some_col.male + some_col.female,

      assoc.male =
        `Estimate..Total..Male..Associate's degree`,
      assoc.female =
        `Estimate..Total..Female..Associate's degree`,
      assoc =
        assoc.male + assoc.female,

      bach.male =
        `Estimate..Total..Male..Bachelor's degree`,
      bach.female =
        `Estimate..Total..Female..Bachelor's degree`,
      bach =
        bach.male + bach.female,

      grad.male =
        `Estimate..Total..Male..Graduate or professional degree`,
      grad.female =
        `Estimate..Total..Female..Graduate or professional degree`,
      grad =
        grad.male + grad.female,

      assoc_plus.male = assoc.male + bach.male + grad.male,
      assoc_plus.female = assoc.female + bach.female + grad.female,
      assoc_plus = assoc_plus.male + assoc_plus.female,

      bach_plus.male = bach.male + grad.male,
      bach_plus.female = bach.female + grad.female,
      bach_plus = bach_plus.male + bach_plus.female)

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
      no_hs, hs, some_col, assoc, bach, grad,
      assoc_plus, bach_plus) %>%

    #merge STL counties
    stl_merge(total:bach_plus, method = "sum") %>%

    #calculate percentages
    mutate_at(vars(no_hs:bach_plus), funs(. / total * 100)) %>%

    #remove unnecessary variables
    select(-total) %>%

    #rename variables to "per"
    rename_at(vars(contains("num")), funs(sub("", "", .)))

  df
}

#White
degree_white_00 <- read_csv(path %p% "DEC_00_SF3_P148I_with_ann.csv", skip = 1)
degree_white_05 <- acs_time(path %p% "B15002H/05", starting_year = 2005)
degree_white_08 <- acs_time(path %p% "B15002H/08", starting_year = 2008)
degree_white_18 <- acs_time(path %p% "B15002H/18", starting_year = 2018)

degree_white_00 %<>% process_00()
degree_white_05 %<>% process_05()
degree_white_08 %<>% process_08()
degree_white_18 %<>% process_18()

degree_white <- bind_rows(degree_white_00, degree_white_05, degree_white_08, degree_white_18)
degree_white %<>% process_degree_race(race_name = "white")


#Black
degree_black_00 <- read_csv(path %p% "DEC_00_SF3_P148B_with_ann.csv", skip = 1)
degree_black_05 <- acs_time(path %p% "B15002B/05", starting_year = 2005)
degree_black_08 <- acs_time(path %p% "B15002B/08", starting_year = 2008)
degree_black_18 <- acs_time(path %p% "B15002B/18", starting_year = 2018)

degree_black_00 %<>% process_00()
degree_black_05 %<>% process_05()
degree_black_08 %<>% process_08()
degree_black_18 %<>% process_18()

degree_black <- bind_rows(degree_black_00, degree_black_05, degree_black_08, degree_black_18)
degree_black %<>% process_degree_race(race_name = "black")


#Hispanic
degree_hispanic_00 <- read_csv(path %p% "DEC_00_SF3_P148H_with_ann.csv", skip = 1)
degree_hispanic_05 <- acs_time(path %p% "B15002I/05", starting_year = 2005)
degree_hispanic_08 <- acs_time(path %p% "B15002I/08", starting_year = 2008)
degree_hispanic_18 <- acs_time(path %p% "B15002I/18", starting_year = 2018)


degree_hispanic_00 %<>% process_00()
degree_hispanic_05 %<>% process_05()
degree_hispanic_08 %<>% process_08()
degree_hispanic_18 %<>% process_18()

degree_hispanic = bind_rows(degree_hispanic_00, degree_hispanic_05, degree_hispanic_08, degree_hispanic_18)
degree_hispanic %<>% process_degree_race(race_name = "hispanic")


# Young Adults by Race

# Read in microdata and process
acs_micro <- feather::read_feather("data-raw/microdata/acs_micro.feather")

acs_micro %<>% filter(age %in% 25:34)

degree_young_race <- svy_race_sex_cat(acs_micro, educ)

degree_young_race %<>%
  filter(race != "total") %>%
  mutate(
    assoc_plus = assoc + bach + grad,
    bach_plus = bach + grad) %>%
  rename_at(vars(assoc:bach_plus), funs(paste0), "_young") %>%
  select(FIPS, year, sex, race, no_hs_young, hs_young, some_col_young,
         assoc_young, bach_young, grad_young, assoc_plus_young, bach_plus_young)

degree_race <- bind_rows(degree_white, degree_black, degree_hispanic)

degree_race %<>% bind_df(degree_young_race)

#combine data frames
degree_county <- bind_rows(degree_tot, degree_race)

update_sysdata(degree_county, degree_tract, degree_nh, degree_msa_1yr, degree_msa_5yr, degree_55k)

rm(degree_00, degree_05, degree_tot, process_degree, process_degree_race, degree_18,
   degree_00_msa, degree_05_5yr, process_5yr, process_18,
   degree_white_00, degree_white_05, degree_white_08, degree_white_18, degree_white,
   degree_black_00, degree_black_05, degree_black_08, degree_black_18, degree_black,
   degree_hispanic_00, degree_hispanic_05, degree_hispanic_08, degree_hispanic_18, degree_hispanic,
   process_00, process_05, process_08, acs_micro, degree_race, degree_young_race,
   degree_map, process_05_55k, process_18_55k, process_degree_55k, degree_05_55k, degree_18_55k, path)
