library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/LIHTC/"

LIHTC <- read_csv(path %p% "LIHTCPUB.CSV")

LIHTC %<>%
  transmute(
    FIPS =
      str_pad(fips2010, 11, "left", "0") %>%
      str_sub(1, 5),
    tract = "1400000US" %p% fips2010,
    year = yr_pis,
    LIHTC = allocamt) %>%
  filter(year %in% 2005:2017)

process_LIHTC <- function(df, geog) {
  df %<>%
    group_by_at(c(geog, "year")) %>%
    summarise(LIHTC = sum(LIHTC, na.rm = TRUE)) %>%
    ungroup()

  df_inflation <- df %>%
    COLA(LIHTC, rpp = FALSE) %>%
    rename(LIHTC_inflation = LIHTC)

  bind_df(df, df_inflation)
}

LIHTC_county <- LIHTC %>%
  process_LIHTC("FIPS") %>%
  pull_peers(add_info = FALSE) %>%
  stl_merge(LIHTC:LIHTC_inflation, method = "sum")

LIHTC_msa <- LIHTC %>%
  process_LIHTC("FIPS") %>%
  left_join(MSA_FIPS, by = "FIPS") %>%
  group_by(MSA, year) %>%
  summarise_at(vars(LIHTC, LIHTC_inflation), sum) %>%
  ungroup()

LIHTC_tract <- LIHTC %>%
  filter(FIPS == 21111) %>%
  process_LIHTC("tract")

LIHTC_nh <- LIHTC_tract %>%
  left_join(nh_tract, by = c("tract" = "GEO_ID")) %>%
  group_by(neighborhood, year) %>%
  summarise_at(vars(LIHTC, LIHTC_inflation), sum) %>%
  ungroup()

update_sysdata(LIHTC_county, LIHTC_msa, LIHTC_tract, LIHTC_nh)

rm(path, LIHTC, process_LIHTC)
