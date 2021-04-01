library(glptools)
glp_load_packages()

for (y in 2000:2018) {
  acc <- read_csv(paste0("data-raw/health/driving_fatalities/FARS/FARS", y, "NationalCSV/ACCIDENT.csv"))
  per <- read_csv(paste0("data-raw/health/driving_fatalities/FARS/FARS", y, "NationalCSV/PERSON.csv"))
  veh <- read_csv(paste0("data-raw/health/driving_fatalities/FARS/FARS", y, "NationalCSV/VEHICLE.csv"))

  acc %<>%
    transmute(
      case = ST_CASE,
      year = YEAR,
      FIPS = str_pad(STATE, 2, "left", "0") %p% str_pad(COUNTY, 3, "left", "0"))

  veh %<>%
    transmute(
      case = ST_CASE,
      year = y,
      drinking = DR_DRINK)

  per %<>%
    transmute(
      case = ST_CASE,
      year = y,
      #vehicle = VEH_NO,
      person = PER_NO,
      sex = case_when(SEX == 1 ~ "male",
                      SEX == 2 ~ "female",
                      TRUE ~ NA_character_),
      race = case_when(HISPANIC %in% 1:6 ~ "hispanic",
                       RACE == 1 ~ "white",
                       RACE == 2 ~ "black",
                       TRUE ~ "other"),
      fatality = if_else(INJ_SEV == 4, 1, 0))

  accident <- assign_row_join(accident, acc)
  person   <- assign_row_join(person, per)
  vehicle <- assign_row_join(vehicle, veh)
}

vehicle %<>%
  group_by(year, case) %>%
  summarise(drinking = if_else(any(drinking == 1), 1, 0))

# Person describes demographics and whether a person died
# Vehicle describes if a driver was drinking
# Accident describes which county the accident occurred in

# Join data frames, subset to peers, and summarize fatalities by drunk driver status
drunk_driving_county <- left_join(person, vehicle, by = c("case", "year")) %>%
  left_join(accident, by = c("case", "year")) %>%
  pull_peers() %>%
  group_by(FIPS, year, drinking, sex, race) %>%
  summarise(fatality = sum(fatality), .groups = "drop") %>%

  # Aggregate data to create totals
  total_demographics(fatality,
                     total_race = T, include_na = T,
                     other_grouping_vars = "drinking") %>%
  filter(
    sex %in% c("total", "male", "female"),
    race %in% c("total", "white", "black", "hispanic")) %>%

  # Calculate percent of deaths including a drinking driver
  mutate(fatality = replace_na(fatality, 0)) %>%
  pivot_wider(names_from = drinking, values_from = fatality) %>%
  mutate() %>%
  transmute(
    FIPS, year, sex, race,
    drunk_driving_deaths = `1`,
    crash_deaths = `0` + `1`,
    drunk_driving_deaths_pct = drunk_driving_deaths / crash_deaths * 100) %>%
  per_capita_adj(drunk_driving_deaths, crash_deaths, keep_vars = F, scale = 100000)

usethis::use_data(drunk_driving_county)

rm(acc, accident, veh, vehicle, per, person, output, y)
