library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/educ/preschool/"

preschool_00 <- read_csv(path %+% "DEC_00_SF3_PCT023_with_ann.csv", skip = 1)
preschool_05 <- acs_time(path %+% "B14003")

preschool_00 %<>%
  transmute(
    FIPS = as.numeric(Id2),
    year = 2000,

    enrolled_3_4.male =
      `Male: - Enrolled in school: - 3 and 4 years`,
    enrolled_3_4.female =
      `Female: - Enrolled in school: - 3 and 4 years`,
    enrolled_3_4 =
      enrolled_3_4.male + enrolled_3_4.female,

    total.male =
      enrolled_3_4.male +
      `Male: - Not enrolled in school: - 3 and 4 years`,
    total.female =
      enrolled_3_4.female +
      `Female: - Not enrolled in school: - 3 and 4 years`,
    total =
      total.male + total.female)

preschool_05 %<>%
  transmute(
    FIPS,
    year,

    enrolled_3_4.male =
      `Estimate; Male: - Enrolled in public school: - 3 and 4 years` +
      `Estimate; Male: - Enrolled in private school: - 3 and 4 years`,
    enrolled_3_4.female =
      `Estimate; Female: - Enrolled in public school: - 3 and 4 years` +
      `Estimate; Female: - Enrolled in private school: - 3 and 4 years`,
    enrolled_3_4 =
      enrolled_3_4.male + enrolled_3_4.female,

    total.male =
      enrolled_3_4.male +
      `Estimate; Male: - Not enrolled in school: - 3 and 4 years`,
    total.female =
      enrolled_3_4.female +
      `Estimate; Female: - Not enrolled in school: - 3 and 4 years`,
    total =
      total.male + total.female)

preschool <- bind_rows(preschool_00, preschool_05)

preschool %<>%
  pull_peers_FIPS(add_info = FALSE) %>%
  reshape_sex() %>%
  mutate(race = "total") %>%
  select(FIPS, year, race, sex, total, enrolled_3_4) %>%
  stl_merge(total:enrolled_3_4, method = "sum") %>%
  mutate(enrolled_3_4 = enrolled_3_4 / total * 100) %>%
  select(-total)

usethis::use_data(preschool, overwrite = TRUE)

rm(preschool_00, preschool_05, preschool, path)
