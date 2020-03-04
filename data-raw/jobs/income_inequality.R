library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/jobs/income_inequality/"

# 2020 Ratio
process_05 <- function(df, geog) {

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "MSA", "tract" = "Id")

  df %<>%
    transmute(
      !!geog := .data[[col_name]],
      year,
      sex = "total",
      race = "total",
      income_bottom_quintile = as.numeric(`Estimate; Quintile Means: - Highest Quintile`),
      income_top_quintile = as.numeric(`Estimate; Quintile Means: - Lowest Quintile`),
      income_inequality = income_top_quintile / income_bottom_quintile) %>%
    { if (geog == "FIPS") stl_merge(., income_bottom_quintile:income_inequality) else .} %>%
    COLA(income_bottom_quintile:income_top_quintile)

  df
}

income_inequality_county  <- acs_time(path %p% "B19081", starting_year = 2006)
income_inequality_msa_1yr <- acs_time(path %p% "MSA/B19081", geography = "MSA", starting_year = 2010)

income_inequality_county  %<>% process_05("FIPS")
income_inequality_msa_1yr %<>% process_05("MSA")

# Gini Index
gini <- acs_time(path %p% "B19083", starting_year = 2006)

process_05 <- function(df) {
  df %>%
    transmute(
      FIPS, year,
      sex = "total",
      race = "total",
      gini_index = `Estimate; Gini Index`) %>%
    stl_merge(gini_index)
}

gini %<>% process_05()

income_inequality_county %<>% bind_df(gini)

update_sysdata(income_inequality_county, income_inequality_msa_1yr)

rm(path, process_05, gini)
