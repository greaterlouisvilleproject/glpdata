library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/population/"

process_00 <- function(df, geog) {
  df %>%
    transmute(
      !!geog := .data[[geog]],
      year = 2000,

      total.male = `Male:`,
      total.female = `Female:`,
      total = total.male + total.female) %>%
    {if (geog != "tract") pull_peers(., geog = "MSA", add_info = FALSE) else .}
}
process_05 <- function(df, geog) {
  df %>%
    transmute(
      !!geog := .data[[geog]], year,

      total.male = `Estimate; Male:`,
      total.female = `Estimate; Female:`,
      total = total.male + total.female)
}
process_18 <- function(df, geog) {
  df %>%
    transmute(
      !!geog := .data[[geog]], year,

      total.male = `Estimate..Total..Male`,
      total.female = `Estimate..Total..Female`,
      total = total.male + total.female)
}

process_pop <- function(df_00, df_05_1yr, df_05_5yr, race_name) {

  # Process data frames and create 1-year and 5-year data frames
  df_00     %<>% rename(FIPS = Id2) %>% process_00("FIPS") %>% reshape_sex()
  df_05_1yr %<>% process_05("FIPS") %>% reshape_sex()
  df_05_5yr %<>% process_05("FIPS") %>% reshape_sex()

  df_1yr <- bind_rows(df_00, df_05_1yr)
  df_5yr <- bind_rows(df_00, df_05_5yr)

  # Create data frame of all combinations of ID variables
  all_FIPS = unique(df_5yr$FIPS)
  all_year = 2000:2019
  all_sex = unique(df_5yr$sex)

  n_FIPS = length(all_FIPS)
  n_year = length(all_year)
  n_sex  = length(all_sex)

  results <- data.frame(
    FIPS = rep(all_FIPS, each = n_year * n_sex),
    year = rep(all_year, each = n_sex),
    sex  = rep(all_sex, n = n_FIPS * n_sex),
    stringsAsFactors = FALSE)

  # Use 1-year data where there are more observations than are available for 5-year data
  FIPS_1yr <- pop_05 %>%
    group_by(FIPS) %>%
    summarise(n = n()) %>%
    filter(n >= length(unique(df_5yr$year))) %>%
    pull(FIPS)

  df_1yr %<>% filter(FIPS %in% FIPS_1yr)
  df_5yr %<>% anti_join(df_1yr, by = c("FIPS", "year", "sex"))

  df <- bind_rows(df_1yr, df_5yr)

  results %>%
    left_join(df, by = c("FIPS", "year", "sex")) %>%
    organize() %>%
    group_by(FIPS, sex) %>%
    mutate(
      population = Hmisc::approxExtrap(x = 2000:2017, y = total, xout = 2000:2019, na.rm = T)$y %>%
        replace(. < 0, 0),
      race = race_name) %>%
    ungroup() %>%
    select(-total) %>%
    organize()
}

# Total
pop_00     <- read_csv(path %p% "DEC_00_SF3_P008_with_ann.csv", skip = 1)
pop_05     <- acs_time(path %p% "B01001/05", geog = "MSA")
pop_18     <- acs_time(path %p% "B01001/18", geog = "MSA", starting_year = 2018)
pop_05_5yr <- acs_time(path %p% "B01001_5yr", geog = "MSA", starting_year = 2007)

pop_05 <- bind_rows(pop_05, pop_18)

pop_total <- process_pop(pop_00, pop_05, pop_05_5yr, "total")

# White
pop_00_white     <- read_csv(path %p% "DEC_00_SF3_P145I_with_ann.csv", skip = 1)
pop_05_white     <- acs_time(path %p% "B01001H/05", geog = "MSA")
pop_18_white     <- acs_time(path %p% "B01001H/18", geog = "MSA", starting_year = 2018)
pop_05_5yr_white <- acs_time(path %p% "B01001H_5yr", geog = "MSA", starting_year = 2007)

pop_05_white <-bind_rows(pop_05_white, pop_18_white)
pop_white <- process_pop(pop_00_white, pop_05_white, pop_05_5yr_white, "white")

# Black
pop_00_black     <- read_csv(path %p% "DEC_00_SF3_P145B_with_ann.csv", skip = 1)
pop_05_black     <- acs_time(path %p% "B01001B/05", geog = "MSA")
pop_18_black     <- acs_time(path %p% "B01001B/18", geog = "MSA", starting_year = 2018)
pop_05_5yr_black <- acs_time(path %p% "B01001B_5yr", geog = "MSA", starting_year = 2007)

pop_05_black <-bind_rows(pop_05_black, pop_18_black)
pop_black <- process_pop(pop_00_black, pop_05_black, pop_05_5yr_black, "black")

# Hispanic
pop_00_hispanic     <- read_csv(path %p% "DEC_00_SF3_P145H_with_ann.csv", skip = 1)
pop_05_hispanic     <- acs_time(path %p% "B01001I/05", geog = "MSA")
pop_18_hispanic     <- acs_time(path %p% "B01001I/18", geog = "MSA", starting_year = 2018)
pop_05_5yr_hispanic <- acs_time(path %p% "B01001I_5yr", geog = "MSA", starting_year = 2007)

pop_05_hispanic <-bind_rows(pop_05_hispanic, pop_18_hispanic)
pop_hispanic <- process_pop(pop_00_hispanic, pop_05_hispanic, pop_05_5yr_hispanic, "hispanic")

population <- bind_rows(pop_total, pop_white, pop_black, pop_hispanic)

population_county <- population %>%
  pull_peers(add_info = F, geog = "FIPS") %>%
  stl_merge(population, method = "sum")

population_msa_1yr <- population %>%
  left_join(MSA_FIPS, by = "FIPS") %>%
  group_by(MSA, year, race, sex) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  organize()

population_msa_counties <- population %>%
  organize()

population_county %<>%
  bind_df(population_msa_1yr %>%
            rename(msa_population = population) %>%
            add_FIPS_to_MSA()) %>%
  mutate(core_county = population / msa_population * 100) %>%
  select(-MSA, -msa_population) %>%
  organize()

# pop_00_tract_total  <- build_census_var_df("sf3", "P8")
# pop_00_tract_race   <- build_census_var_df("sf3", "P145")
# pop_05_tract        <- build_census_var_df("acs5", "B01001")
#
# pop_tract <- bind_rows(pop_00_tract_total, pop_00_tract_race, pop_05_tract)
#
# pop_tract <- get_census(pop_tract, "tract", T)
#
# pop_tract2 <- pop_tract %>%
#  tract_00_to_10(2000:2007, var)

process_map <- function(df, geog) {

  geogs <- unique(df[[geog]])
  years  <- 2000:2019

  results <- data.frame(
    geog = rep(geogs, each = length(years)),
    year = rep(years),
    stringsAsFactors = FALSE) %>%
    filter(geog %not_in% c("1400000US21111980100", "Airport")) %>%
    rename(!!geog := geog)

  results %<>%
    left_join(df, by = c(geog, "year")) %>%
    organize() %>%
    group_by_at(geog) %>%
    mutate(
      population = Hmisc::approxExtrap(x = 2000:2017, y = population,
                                       xout = 2000:2019, na.rm = T)$y) %>%
    ungroup()

  results %<>%
    group_by(year) %>%
    mutate(total_pop = sum(population, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(year) %>%
    group_by_at(geog)  %>%
    mutate(
      #population_change is % change
      population_change = (population - first(population)) / first(population) * 100,

      #population_change_adj is % change - county-wide % change
      population_change_adj =
        population_change -
        ((total_pop - first(total_pop))/first(total_pop) * 100)) %>%
    ungroup() %>%
    select(!!geog, year, population, population_change, population_change_adj)

  airport <- data.frame(geog = if_else(geog == "tract", "1400000US21111980100", "Airport"),
                        year = years, population = NA, population_change = NA, population_change_adj = NA) %>%
    rename(!!geog := geog)

  results %>%
    bind_rows(airport) %>%
    organize()
}

pop_00_tract <- read_csv(path %p% "tract/DEC_00_SF3_P008_with_ann.csv", skip = 1)
pop_05_tract <- acs_time(path %p% "B01001_tract", starting_year = 2007)

pop_00_tract %<>% mutate(tract = Id) %>% process_00("tract")
pop_05_tract %<>% process_05("tract")

pop_map <- bind_rows(pop_00_tract, pop_05_tract)

pop_map %<>%
  select(tract, year, population = total) %>%
  tract_00_to_10(2000:2007, population) %>%
  mutate(population = replace(population, tract == "1400000US21111980100", NA))

population_tract <- pop_map %>% process_map("tract")

population_nh <- pop_map %>%
  left_join(nh_tract, by = c("tract" = "GEO_ID")) %>%
  group_by(neighborhood, year) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  process_map("neighborhood")

population_muw <- pop_map %>%
  left_join(muw_tract, by = c("tract" = "GEO_ID")) %>%
  group_by(neighborhood, year) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  process_map("neighborhood")

update_sysdata(population_county, population_msa_1yr, population_msa_counties, population_tract, population_nh, population_muw)

rm(pop_00, pop_00_black, pop_00_hispanic, pop_00_tract, pop_00_white,
   pop_05, pop_05_5yr, pop_05_5yr_black, pop_05_5yr_hispanic, pop_05_5yr_white,
   pop_05_black, pop_05_hispanic, pop_05_tract, pop_05_white,
   pop_black, pop_hispanic, pop_map, pop_total, pop_white, population,
   process_00, process_05, process_map, process_pop, path)

if (FALSE) {
process_pop <- function(df_00, df_05_1yr, df_05_5yr, race_name) {
  df_00     %<>% process_00() %>% reshape_sex()
  df_05_1yr %<>% process_05() %>% reshape_sex()
  df_05_5yr %<>% process_05() %>% reshape_sex()

  df_1yr <- bind_rows(df_00, df_05_1yr)
  df_5yr <- bind_rows(df_00, df_05_5yr)

  model <- df_5yr %>%
    nest(-FIPS, -sex) %>%
    mutate(
      reg = map(data, ~ lm(.x$total ~ .x$year)),
      model = map(reg, tidy)) %>%
    unnest(model) %>%
    select(FIPS, sex, term, estimate) %>%
    spread(key = term, value = estimate) %>%
    rename(intercept = `(Intercept)`, slope = `.x$year`)

  all_FIPS = unique(df_5yr$FIPS)
  all_year = 2000:2017
  all_sex = unique(df_5yr$sex)

  n_FIPS = length(all_FIPS)
  n_year = length(all_year)
  n_sex  = length(all_sex)

  results <- data.frame(
    FIPS = rep(all_FIPS, each = n_year * n_sex),
    year = rep(all_year, each = n_sex),
    sex  = rep(all_sex, n = n_FIPS * n_sex),
    stringsAsFactors = FALSE)

  results %<>%
    left_join(model, by = c("FIPS", "sex")) %>%
    mutate(total = intercept + year * slope) %>%
    select(FIPS, year, sex, total) %>%
    anti_join(df_1yr, by = c("FIPS", "year", "sex"))

  bind_rows(df_1yr, results) %>%
    mutate(race = race_name) %>%
    organize()
}
}
