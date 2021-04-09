library(glptools)
glp_load_packages()

path <- "data-raw/qop/HPI/"

HPI_county <- readxl::read_xlsx(path %p% "HPI_AT_BDL_county.xlsx", skip = 6)

HPI_county %<>%
  transmute(
    FIPS = `FIPS code`,
    year = as.numeric(Year),
    HPI = as.numeric(`HPI with 2000 base`)) %>%
  filter(year >= 2000) %>%
  pull_peers() %>%
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
  pull_peers() %>%
  mutate(sex = "total", race = "total") %>%
  organize()

HPI_zip <- readxl::read_xlsx(path %p% "HPI_AT_BDL_ZIP5.xlsx", skip = 6)

HPI_zip %<>%
  transmute(
    zip  = `Five-Digit ZIP Code`,
    year = as.numeric(Year),
    HPI  = as.numeric(`HPI with 2000 base`)) %>%
  left_join(FIPS_zip, by = "zip") %>%
  filter(
    FIPS == "21111",
    year >= 2000) %>%
  group_by(zip) %>%
  mutate(
    HPI_2015 = if_else(year <= 2015, NA_real_,
                       HPI / HPI[year == 2015] * 100 - 100),
    HPI_2018 = if_else(year <= 2018, NA_real_,
                       HPI / HPI[year == 2018] * 100 - 100)) %>%
  select(zip, year, HPI, HPI_2015, HPI_2018) %>%
  ungroup()

usethis::use_data(HPI_county, HPI_msa_1yr, HPI_zip, overwrite = TRUE)

rm(path)
