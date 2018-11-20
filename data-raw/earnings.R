library(tidyverse)
library(glptools)

path <- "data-raw/jobs/earnings/"

earn_data_2000 <- read_csv(path %+% "DEC_00_SF3_P085_with_ann.csv", skip = 1)
earn_data_05_16 <- acs_time(path %+% "S2001/05/")
earn_data_17 <- acs_time(path %+% "S2001/17/", starting_year = 2017)

earn_data_2000 <- earn_data_2000 %>%
  transmute(
    FIPS = as.numeric(Id2),
    year = 2000,
    median_earnings = Total,
    median_earnings_male = Male,
    median_earnings_female = Female) %>%
  pull_peers_FIPS(add_info = FALSE) %>%
  weight_stl(c("median_earnings", "median_earnings_male", "median_earnings_female"))


earn_data_05_16 %<>%
  select(FIPS,
         year,
         pop = `Total; Estimate; Population 16 years and over with earnings`,
         pop_male =`Male; Estimate; Population 16 years and over with earnings`,
         pop_female = `Female; Estimate; Population 16 years and over with earnings`,
         pop_ft = `Total; Estimate; Population 16 years and over with earnings - Full-time, year-round workers with earnings`,
         pop_ft_male = `Male; Estimate; Population 16 years and over with earnings - Full-time, year-round workers with earnings`,
         pop_ft_female = `Female; Estimate; Population 16 years and over with earnings - Full-time, year-round workers with earnings`,

         median_earnings = `Total; Estimate; Population 16 years and over with earnings - Median earnings (dollars)`,
         median_earnings_male = `Male; Estimate; Population 16 years and over with earnings - Median earnings (dollars)`,
         median_earnings_female = `Female; Estimate; Population 16 years and over with earnings - Median earnings (dollars)`,
         median_earnings_ft = `Total; Estimate; Population 16 years and over with earnings - Full-time, year-round workers with earnings - Mean earnings (dollars)`,
         median_earnings_ft_male = `Male; Estimate; Population 16 years and over with earnings - Full-time, year-round workers with earnings - Median earnings (dollars)`,
         median_earnings_ft_female = `Female; Estimate; Population 16 years and over with earnings - Full-time, year-round workers with earnings - Median earnings (dollars)`) %>%
  mutate_at(vars(pop, pop_male, pop_female, pop_ft, pop_ft_male, pop_ft_female,
                 median_earnings, median_earnings_male, median_earnings_female,
                 median_earnings_ft, median_earnings_ft_male, median_earnings_ft_female),
            as.numeric)

earn_data_17 %<>%
  select(FIPS,
         year,
         pop = `Total; Estimate; Population 16 years and over with earnings`,
         pop_male =`Male; Estimate; Population 16 years and over with earnings`,
         pop_female = `Female; Estimate; Population 16 years and over with earnings`,
         pop_ft = `Total; Estimate; FULL-TIME, YEAR-ROUND WORKERS WITH EARNINGS`,
         pop_ft_male = `Male; Estimate; FULL-TIME, YEAR-ROUND WORKERS WITH EARNINGS`,
         pop_ft_female = `Female; Estimate; FULL-TIME, YEAR-ROUND WORKERS WITH EARNINGS`,

         median_earnings = `Total; Estimate; Median earnings (dollars)`,
         median_earnings_male = `Male; Estimate; Median earnings (dollars)`,
         median_earnings_female = `Female; Estimate; Median earnings (dollars)`,
         median_earnings_ft = `Total; Estimate; Median earnings (dollars) for full-time, year-round workers with earnings`,
         median_earnings_ft_male = `Male; Estimate; Mean earnings (dollars) for full-time, year-round workers with earnings`,
         median_earnings_ft_female = `Female; Estimate; Mean earnings (dollars) for full-time, year-round workers with earnings`) %>%
  mutate_at(vars(pop, pop_male, pop_female, pop_ft, pop_ft_male, pop_ft_female,
                 median_earnings, median_earnings_male, median_earnings_female,
                 median_earnings_ft, median_earnings_ft_male, median_earnings_ft_female),
            as.numeric)

earn_data_05_17 <- bind_rows(earn_data_05_16, earn_data_17)

earn_data_05_17 <- bind_df(
  earn_data_05_17 %>% weight_stl("median_earnings", "pop"),
  earn_data_05_17 %>% weight_stl("median_earnings_male", "pop_male"),
  earn_data_05_17 %>% weight_stl("median_earnings_female", "pop_female"),
  earn_data_05_17 %>% weight_stl("median_earnings_ft", "pop_ft"),
  earn_data_05_17 %>% weight_stl("median_earnings_ft_male", "pop_ft_male"),
  earn_data_05_17 %>% weight_stl("median_earnings_ft_female", "pop_ft_female"))

earn_data <- earn_data_05_17

earn_data %<>% COLA(median_earnings:median_earnings_ft_female)

devtools::use_data(earn_data, overwrite = TRUE)

rm(earn_data_2000, earn_data_05_16, earn_data_17, earn_data_05_17, earn_data, path)
