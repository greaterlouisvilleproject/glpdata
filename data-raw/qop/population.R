library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(broom)
library(purrr)

path <- "data-raw/qop/population/"

pop_00 <- read_csv(path %+% "DEC_00_SF3_P001_with_ann.csv", skip = 1)
pop_05 <- acs_time(path %+% "B01003", county_filter = "MSA_counties")
pop_05_5yr <- acs_time(path %+% "B01003_5", starting_year = 2007, county_filter = "MSA_counties")

# 2000 data frame
pop_00 %<>%
  transmute(
    FIPS = as.numeric(Id2),
    year = 2000,
    total = Total) %>%
  pull_peers_FIPS(county_filter = "MSA_counties", add_info = FALSE)

# 2005 single-year data frame
pop_05 %<>%
  transmute(
    FIPS,
    year,
    total = `Estimate; Total`)

# 2005 5-year data frame used to model population for missing years
pop_05_5yr %<>%
  transmute(
    FIPS,
    year,
    total = `Estimate; Total`)

# Create model
pop_05_model <- pop_05_5yr %>%
  nest(-FIPS) %>%
  mutate(
    reg = map(data, ~ lm(.x$total ~ .x$year)),
    model = map(reg, tidy)) %>%
  unnest(model) %>%
  select(FIPS, term, estimate) %>%
  spread(key = term, value = estimate) %>%
  rename(intercept = `(Intercept)`, slope = `.x$year`)

# Evaluate model for the years 2005 to 2017
pop_05_est <- data.frame(
  FIPS = rep(pop_05_model$FIPS, each = length(2005:2017)),
  year = rep(2005:2017, length(pop_5_model$FIPS)))

pop_05_est %<>%
  left_join(pop_05_model, by = "FIPS") %>%
  mutate(total = intercept + year * slope) %>%
  select(FIPS, year, total)

# Remove years that are in the single=-yeaqr data frame
pop_05_est %<>% anti_join(pop_05, by = c("FIPS", "year"))

# Bind all poulation data frames
population <- bind_rows(pop_00, pop_05) %>% bind_rows(pop_05_est)

population %<>%
  left_join(MSA_FIPS %>% mutate(FIPS = as.numeric(FIPS)), by = "FIPS") %>%
  group_by(MSA, year) %>%
  mutate(pct_county = total / sum(total) * 100) %>%
  ungroup() %>%
  stl_merge(pct_county, method = "sum") %>%
  pull_peers_FIPS()






