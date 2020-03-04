library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/health/asthma/"

asthma <- readxl::read_excel(path %p% "health_department_request.xlsx", sheet = "Hospitalization", skip = 2)

demographics <- c("Total Admissions" = "total",
                  "Female" = "female", "Male" = "male",
                  "Black or African Americcan" = "black", "White" = "white", "Other" = "other")

asthma %<>%
  mutate(category = recode(`...1`, !!!demographics)) %>%
  pivot_longer(`2009`:`2017`, names_to = "year", values_to = "admissions") %>%
  filter(!is.na(admissions)) %>%
  select(-`...1`)

asthma_total <- asthma %>%
  filter(category == "total") %>%
  mutate(sex = "total", race = "total")

asthma_sex <- asthma %>%
  filter(category %in% c("male", "female")) %>%
  mutate(sex = category, race = "total")

asthma_race <- asthma %>%
  filter(category %in% c("white", "black")) %>%
  mutate(sex = "total", race = category)

asthma_county <- bind_rows(asthma_total, asthma_sex, asthma_race) %>%
  transmute(
    FIPS = "21111",
    year = as.numeric(year),
    race, sex,
    admissions = as.numeric(admissions)) %>%
  left_join(glpdata:::population_county, by = c("FIPS", "year", "sex", "race")) %>%
  transmute(
    FIPS, year, race, sex,
    admissions,
    admissions_rate = admissions / population * 100000)

asthma_zip <- asthma %>%
  transmute(
    zip = category %>% as.numeric() %>% as.character(),
    year,
    admissions) %>%
  filter(!is.na(zip)) %>%
  left_join(FIPS_zip, by = "zip") %>%
  filter(population_in_FIPS > 0) %>%
  transmute(
    zip = as.numeric(zip),
    year,
    admissions = as.numeric(admissions) %>% replace_na(5),
    admissions_rate = admissions / population_total * 100000) %>%
  filter(year == 2017)

all_mort <- read_tsv(path %p% "all_cause_mortality.txt")
non_asthma_mort <- read_tsv(path %p% "non_asthma_mortality.txt")

all_mort %<>%
  clean_wonder(method = "weight") %>%
  rename(all_rate = rate)

non_asthma_mort %<>%
  clean_wonder(method = "weight") %>%
  rename(non_asthma_rate = rate)

asthma_mortality <- bind_df(all_mort, non_asthma_mort)

asthma_mortality %<>%
  transmute(
    FIPS, year,
    race = "total", sex = "total",
    asthma_mortality = all_rate - non_asthma_rate) %>%
  organize()

asthma_county %<>% bind_df(asthma_mortality)

update_sysdata(asthma_county)









