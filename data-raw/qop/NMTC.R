library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/NMTC/"

NMTC <- readxl::read_excel(path %p% "FY 2018 NMTC Public Data Release.xlsx", sheet = "Financial Notes 1")

NMTC %<>%
  group_by(`Community Development Entity (CDE) Name`) %>%
  fill(`Origination Year`) %>%
  ungroup() %>%
  transmute(
    FIPS =
      str_pad(`2010 Census Tract`, 11, "left", "0") %>%
      str_sub(1, 5),
    tract = "1400000US" %p% `2010 Census Tract`,
    year = `Origination Year`,
    NMTC = `QLICI Amount`) %>%
  pull_peers(add_info = FALSE, geography = "MSA")

process_NMTC <- function(df, geog) {
  df %<>%
    group_by_at(c(geog, "year")) %>%
    summarise(NMTC = sum(NMTC)) %>%
    ungroup()

  df_inflation <- df %>%
    COLA(NMTC, rpp = FALSE) %>%
    rename(NMTC_inflation = NMTC)

  bind_df(df, df_inflation)
}

NMTC_county <- NMTC %>%
  process_NMTC("FIPS") %>%
  pull_peers(add_info = FALSE)

all_FIPS <- data.frame(
  FIPS = rep(unique(NMTC_county$FIPS), each = length(years_in_df(NMTC_county, NMTC))),
  year = years_in_df(NMTC_county, NMTC),
  stringsAsFactors = FALSE)

NMTC_county %<>%
  right_join(all_FIPS, by = c("FIPS", "year")) %>%
  mutate(
    sex = "total", race = "total",
    NMTC = replace_na(NMTC, 0),
    NMTC_inflation = replace_na(NMTC_inflation, 0))%>%
  organize() %>%
  pull_peers(add_info = FALSE) %>%
  stl_merge(NMTC:NMTC_inflation, method = "sum") %>%
  left_join(population_df_merged, by = c("FIPS", "year")) %>%
  mutate(
    NMTC_pp = NMTC / population,
    NMTC_pp_inflation = NMTC_inflation / population)

if (FALSE) {
  NMTC_msa <- NMTC %>%
    process_NMTC("FIPS") %>%
    left_join(MSA_FIPS, by = "FIPS") %>%
    group_by(MSA, year) %>%
    summarise_at(vars(NMTC, NMTC_inflation), sum) %>%
    ungroup() %>%
    mutate(
      sex = "total",
      race = "total") %>%
    organize()

  NMTC_tract <- NMTC %>%
    filter(FIPS == 21111) %>%
    process_NMTC("tract")

  NMTC_nh <- NMTC_tract %>%
    left_join(nh_tract, by = c("tract" = "GEO_ID")) %>%
    group_by(neighborhood, year) %>%
    summarise_at(vars(NMTC, NMTC_inflation), sum) %>%
    ungroup()
}

update_sysdata(NMTC_county)

rm(path, all_FIPS, NMTC, process_NMTC)

