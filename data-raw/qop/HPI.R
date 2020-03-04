library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/HPI/"

HPI_county <- readxl::read_xlsx(path %p% "HPI_AT_BDL_county.xlsx", skip = 6)

HPI_county %<>%
  transmute(
    FIPS = `FIPS code`,
    year = as.numeric(Year),
    HPI = as.numeric(`HPI with 2000 base`)) %>%
  filter(year >= 2000) %>%
  pull_peers(add_info = FALSE) %>%
  stl_merge(HPI) %>%
  mutate(sex = "total", race = "total") %>%
  organize()

HPI_msa_1yr <- readxl::read_xlsx(path %p% "HPI_AT_BDL_cbsa.xlsx", skip = 6)

HPI_msa_1yr %<>%
  transmute(
    MSA = CBSA,
    year = as.numeric(Year),
    HPI = as.numeric(`HPI with 2000 base`)) %>%
  filter(year >= 2000) %>%
  pull_peers(add_info = FALSE) %>%
  mutate(sex = "total", race = "total") %>%
  organize()

HPI_zip <- readxl::read_xlsx(path %p% "HPI_AT_BDL_ZIP5.xlsx", skip = 6)

HPI_zip %<>%
  transmute(
    zip  = `Five-Digit ZIP Code`,
    year = as.numeric(Year),
    HPI_2000  = as.numeric(`HPI with 2000 base`)) %>%
  left_join(FIPS_zip, by = "zip") %>%
  filter(
    FIPS == "21111",
    year >= 2000) %>%
  group_by(zip) %>%
  mutate(
    HPI_2013 = if_else(year <= 2013, NA_real_,
                       HPI_2000 / HPI_2000[year == 2013] * 100 - 100),
    HPI_2017 = if_else(year <= 2017, NA_real_,
                       HPI_2000 / HPI_2000[year == 2017] * 100 - 100),
    HPI_2000 = HPI_2000 - 100) %>%
  select(zip, year, HPI_2000, HPI_2013, HPI_2017) %>%
  ungroup()

update_sysdata(HPI_county, HPI_msa_1yr, HPI_zip)

rm(path)
