library(tidyverse)
library(magrittr)
library(glptools)
library(feather)
library(survey)

path <- "data-raw/qop/disconnected/"

acs <- read_csv(path %+% "usa_00054.csv")

acs %<>%
  process_microdata(gq = TRUE) %>%
  pull_peers_FIPS()

acs %<>%
  filter(AGE >= 16 & AGE <= 24) %>%
  filter(FIPS == 21111)

acs$race <- "other"
acs$race[acs$RACE == 1 & acs$HISPAN == 0] <- "white"
acs$race[acs$RACE == 2 & acs$HISPAN == 0] <- "black"
acs$race[acs$HISPAN == 1] <- "hispanic"

acs %<>%
  mutate(
    not_employed = if_else(EMPSTAT == 2 | EMPSTAT == 3, 1, 0),
    not_employed = replace(not_employed, EMPSTAT == 0, NA),

    not_in_school = if_else(SCHOOL == 1, 1, 0),
    not_in_school = replace(not_in_school, SCHOOL == 0, NA),

    disconnected = if_else(not_employed & not_in_school, 1, 0),

    parent = if_else(NCHILD >= 1, 1, 0))

acs %<>%
  transmute(
    PERWT,
    year,
    age = AGE,
    race,
    parent,
    not_employed,
    not_in_school,
    disconnected)

#Create three year data frame
for(i in 2005:2017){
  this <- acs %>% filter(year %in% c(i - 1, i, i + 1))
  this$year <- i
  if(i == 2005) acs_3 <- this
  else          acs_3 %<>% bind_rows(this)
}

acs_3 %<>% filter(year > 2005, year < 2016)

#export results and sample size
survey <- svydesign(ids = ~1, weights = ~PERWT, data = acs)
survey_3 <- svydesign(ids = ~1, weights = ~PERWT, data = acs_3)

#calculate disconnected youth
results_tot <- svyby(~disconnected, ~year+parent,
                     design = survey, svymean, na.rm = TRUE)
results_race <- svyby(~disconnected, ~year+race+parent,
                      design = survey, svymean, na.rm = TRUE)

results_tot$race <- "total"

results <- bind_rows(results_tot, results_race)


results_tot_3 <- svyby(~disconnected, ~year,
                       design = survey_3, svymean, na.rm = TRUE)
results_race_3 <- svyby(~disconnected, ~year+race,
                        design = survey_3, svymean, na.rm = TRUE)

results_tot_3$race <- "total"

results_3 <- bind_rows(results_tot_3, results_race_3)

results_3 %<>%
  rename(
    disconnected_3 = disconnected,
    se_3 = se)

#sample size
acs_total <- acs %>%
  group_by(year, parent) %>%
  summarise(sample = n()) %>%
  mutate(race = "total")

acs_race  <- acs %>%
  group_by(year, race) %>%
  summarise(sample = n())

acs_total_3 <- acs_3 %>%
  group_by(year) %>%
  summarise(sample_3 = n()) %>%
  mutate(race = "total")

acs_race_3  <- acs_3 %>%
  group_by(year, race) %>%
  summarise(sample_3 = n())


output <- bind_rows(acs_total, acs_race) %>%
  mutate(race = factor(race,
                       levels = c("total", "black", "hispanic", "white", "other"),
                       ordered = TRUE)) %>%
  arrange(race, year)

output_3 <- bind_rows(acs_total_3, acs_race_3) %>%
  mutate(race = factor(race,
                       levels = c("total", "black", "hispanic", "white", "other"),
                       ordered = TRUE)) %>%
  arrange(race, year)


output %<>% bind_df(results, output_3, results_3)

output %<>%
  mutate_at(vars(disconnected, disconnected_3, se, se_3),
            funs(. * 100)) %>%
  select(year, race, everything())

write_csv(acs, "disconnected_data.csv")
write_csv(output, "disconnected_results.csv")










