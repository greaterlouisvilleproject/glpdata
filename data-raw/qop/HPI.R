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
    HPI5 = (HPI - lag(HPI, 5)) / lag(HPI, 5) * 100,
    HPI1 = (HPI - lag(HPI, 1)) / lag(HPI, 1) * 100) %>%
  select(zip, year, HPI, HPI5, HPI1) %>%
  ungroup()

HPI_tract <- read_csv(path %p% "HPI_AT_BDL_tract.csv")

HPI_tract %<>%
  filter(
    tract %in% tract10_tract_00$tract10,
    str_sub(tract, 1, 5) == "21111",
    year >= 2000) %>%
  transmute(
    tract,
    year,
    HPI  = as.numeric(hpi)) %>%
  complete(tract, year) %>%
  group_by(tract) %>%
  mutate(
    HPI_2000 = HPI / HPI[year == 2000] * 100,
    HPI_2010 = HPI / HPI[year == 2010] * 100,
    HPI5 = (HPI - lag(HPI, 5)) / lag(HPI, 5) * 100,
    HPI1 = (HPI - lag(HPI, 1)) / lag(HPI, 1) * 100)

usethis::use_data(HPI_county, HPI_msa_1yr, HPI_zip, HPI_tract, overwrite = TRUE)

rm(path)
