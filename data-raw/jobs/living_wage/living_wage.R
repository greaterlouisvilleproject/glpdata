library(tidyverse)
library(magrittr)
library(glptools)
library(survey)
library(feather)

path <- "data-raw/jobs/living_wage/"

#Microdata
#acs <- read_csv(path %p% "usa_00061.csv")
#acs %<>% process_microdata() %>% pull_peers_FIPS(add_info = FALSE) %>% organize()
#write_feather(acs, path %p% "living_wage.feather")

#Microdata

lw <- read_feather(path %p% "living_wage.feather")

lw %<>% filter(
  year > 2014,
  FIPS == 21111)

#lw %<>% select(FIPS, year, city, SERIAL, HHWT, FAMUNIT, PERNUM, SEX, AGE, LABFORCE, HHINCOME, INCEARN, INCWAGE, FTOTINC)

lw %<>% mutate(
  HHINCOME = replace(HHINCOME, HHINCOME == 9999999, NA),
  income = HHINCOME)

lw %<>%
  group_by(SERIAL, year) %>%
  mutate(
    income = sum(INCEARN),
    working = if_else(any(LABFORCE == 2), 1, 0)) %>%
  filter(working == 1) %>%
  ungroup()

#lw %<>%
#  group_by(SERIAL, FAMUNIT) %>%
#  mutate(PERNUM = row_number()) %>%
#  ungroup() %>%
#  mutate(SERIAL = paste0(SERIAL, "-", FAMUNIT))

#Calculate number of children, adults, and size of household
lw %<>%
  mutate(adult = if_else(age > 17, 1, 0)) %>%
  group_by(SERIAL, year, adult) %>%
  mutate(group_size = n()) %>%
  ungroup() %>%
  {
    x <- .
    bind_rows(
      x %>%
        filter(adult == 1) %>%
        mutate(hh_adults = group_size,
               hh_children = 0),
      x %>%
        filter(adult == 0) %>%
        mutate(hh_adults = 0,
               hh_children = group_size))
  } %>%
  group_by(SERIAL, year) %>%
  mutate(
    hh_adults = max(hh_adults),
    hh_children = max(hh_children),
    hh_size = hh_adults + hh_children) %>%
  ungroup() %>%
  select(-group_size, -adult) %>%
  arrange(FIPS, SERIAL, PERNUM)

#Determine if there is a stay at home parent
lw %<>%
  group_by(SERIAL, year) %>%
  mutate(
    stay_at_home = if_else(any(age > 17 & SCHOOL == 1 & LABFORCE == 1 & NCHILD > 0), 1, 0)) %>%
  ungroup() %>%
  filter(stay_at_home == 0)

#Create living wage data frame and adjust for inflation to 2017
basic_lw <- data.frame(
  hh_adults = c(rep(1, 4), rep(2, 8)),
  hh_children = rep(0:3, 3),
  stay_at_home = c(rep(0, 8), rep(1, 4)),
  total_costs = c(10.72, 22.89, 27.74, 36.07,
                  8.85 * 2, 12.31 * 2, 15.42 * 2, 18.36 * 2,
                  17.70, 21.03, 24.02, 26.67) * 2080)

basic_lw %<>%
  mutate(
    FIPS = "21111",
    year = 2015) %>%
  COLA(total_costs) %>%
  select(-FIPS, -year)

lw %<>% COLA(income)

lw %<>%
  left_join(basic_lw, by = c("hh_adults", "hh_children", "stay_at_home")) %>%
  filter(!is.na(total_costs)) %>%
  mutate(living_wage = if_else(total_costs < income, 1, 0))

lw %<>%
  group_by(FIPS, SERIAL, year, hh_adults, hh_children) %>%
  summarise(
    HHWT = mean(HHWT),
    living_wage = mean(living_wage),
    income = mean(income)) %>%
  ungroup()

lw_svy <- svydesign(ids = ~0, weights = ~HHWT, data = lw)

lw_df_pct <- svyby(~living_wage, by = ~hh_adults+hh_children, design = lw_svy, svymean)

lw_df_pct %<>% mutate(living_wage = living_wage * 100)

lw_df_med <- svyby(~income, by = ~hh_adults+hh_children, design = lw_svy, svyquantile, quantiles = c(0.5), na.rm = TRUE, ci = TRUE)

table(lw$hh_adults, lw$hh_children)
