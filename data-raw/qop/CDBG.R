library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/CDBG/"

CDBG <- readxl::read_excel(path %p% "Grantee_Awards_09042019_09301783.xlsx", skip = 3,
                           col_type = c("numeric", "text", "text", "text", "text", "text", "numeric"))

CoC_crosswalk <- tibble(
  city = FIPS_df_two_stl$city,
  CoC_name = c("AL-500", "FL-510", "IN-503", "KY-501", "MI-506", "MO-604", "MO-500", "MO-501",
               "NE-501", "NC-504", "NC-505", "NC-507", "OH-503", "OH-500", "OH-505",
               "OK-502", "OK-501", "SC-501", "TN-504", "TN-502", "TN-501", "VA-500"))


CDBG %<>%
  transmute(
    org = `Org Name`,
    CoC_name = str_sub(`CoC Name`, 1, 6),
    year = Year,
    program = `Program Name`,
    funding = `Award Amount`,
    state = State)

CoC <- CDBG %>%
  filter(program == "CoC") %>%
  left_join(CoC_crosswalk, by = "CoC_name") %>%
  select(city, year, program, funding)

CDBG_county <- CDBG %>%
  filter(program %in% c("CDBG", "HOME")) %>%
  mutate(
    geog =
      if_else(str_detect(org, ",", negate = T), "state",    # If there is no comma, state
              if_else(str_detect(org, "County"), "county",  # If name contains "County", county
                      "city")),                            # else, city

    state,

    city   =  if_else(geog == "city", str_extract(org, "^.*(?=,)") %p% "." %p% state,
                      NA_character_),

    county = if_else(geog == "county", str_extract(org, "^.*(?= County)") %p% "." %p% state,
                     NA_character_)
  ) %>%
  filter(city   %in% c(paste0(FIPS_info$city,   ".", FIPS_info$state), "High Point.NC") |
         county %in% c(paste0(FIPS_info$county, ".", FIPS_info$state),
                       "Jacksonville-Duval.FL", "St. Louis.MO", "Nashville-Davidson.TN", "Louisville-Jefferson.KY")) %>%
  mutate(
    city   = str_sub(city,   1, -4) %>%
      replace(. == "High Point", "Greensboro"),
    county = str_sub(county, 1, -4) %>%
      replace(. == "Jacksonville-Duval",   "Duval") %>%
      replace(. == "St. Louis",            "St. Louis County") %>%
      replace(. == "Nashville-Davidson",   "Davidson") %>%
      replace(. == "Louisville-Jefferson", "Jefferson")) %>%
  {
    x <- .
    bind_rows(
      x %>% filter(geog == "city"),
      x %>% filter(geog == "county") %>%
        select(-city) %>%
        left_join(select(FIPS_info, county, state, city), by = c("state", "county")))
  } %>%
  select(city, year, program, funding) %>%
  bind_rows(CoC) %>%
  left_join(FIPS_df_one_stl, by = "city") %>%
  group_by(FIPS, year, program) %>%
  summarise(funding = sum(funding)) %>%
  ungroup() %>%
  spread(key = program, value = funding) %>%
  left_join(population_df_merged, by = c("FIPS", "year")) %>%
  mutate(
    CoC = replace(CoC, year %in% 2013:2018 & is.na(CoC), 0),
    CDBG_pp  = CDBG / population,
    CoC_pp  = CoC  / population,
    HOME_pp = HOME / population) %>%
  select(-population) %>%
  mutate(sex = "total", race = "total") %>%
  organize()

CDBG_inflation <- CDBG_county %>%
  COLA(CDBG:HOME_pp, base_year = 2019, rpp = FALSE) %>%
  rename_at(vars(CDBG:HOME_pp), ~paste0(., "_inflation"))

CDBG_county %<>% bind_df(CDBG_inflation)

update_sysdata(CDBG_county)

rm(CDBG, CDBG_inflation, CoC, CoC_crosswalk, path)
