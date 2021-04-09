library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/mpi/"

read_tract <- function(folder, starting_year = 2007){
  wd <- getwd()
  directory <- paste0(wd, "/", folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- starting_year
  for (i in 1:n){
    file_path <- paste0(wd, "/", folder, "/", file_names[i])
    df <- read_csv(file_path, skip = 1)

    df %<>% rename(tract := Id2)

    df$year <- y
    y <- y + 1

    if(i == 1){
      output <- df
    }
    else{
      names(df) <- names(output)
      output <- rbind(output, df)
    }
  }
  output
}

# Create list of states to download tract data
state_tracts <- MSA_FIPS %>%
  mutate(state_FIPS = FIPS %>%
           str_pad(5, "left", "0") %>%
           str_sub(1, 2)) %>%
  pull(state_FIPS) %>%
  unique()

state_names <- state_df %<>%
  mutate(state_FIPS = str_pad(FIPS, 2, "left", "0")) %>%
  filter(state_FIPS %in% state_tracts) %>%
  pull(state)

# Subset list of tract codes
degree <- read_tract(path %p% "B15001")

degree %<>%
  transmute(
    county_FIPS = tract %>% str_sub(1, 5) %>% as.numeric %>% as.character,
    tract,
    year,
    total_degree =
      `Estimate; Male: - 25 to 34 years:` +
      `Estimate; Male: - 35 to 44 years:` +
      `Estimate; Male: - 45 to 64 years:` +
      `Estimate; Female: - 25 to 34 years:` +
      `Estimate; Female: - 35 to 44 years:` +
      `Estimate; Female: - 45 to 64 years:`,
    no_hs =
      `Estimate; Male: - 25 to 34 years: - Less than 9th grade` +
      `Estimate; Male: - 35 to 44 years: - Less than 9th grade` +
      `Estimate; Male: - 45 to 64 years: - Less than 9th grade` +
      `Estimate; Male: - 25 to 34 years: - 9th to 12th grade, no diploma` +
      `Estimate; Male: - 35 to 44 years: - 9th to 12th grade, no diploma` +
      `Estimate; Male: - 45 to 64 years: - 9th to 12th grade, no diploma` +
      `Estimate; Female: - 25 to 34 years: - Less than 9th grade` +
      `Estimate; Female: - 35 to 44 years: - Less than 9th grade` +
      `Estimate; Female: - 45 to 64 years: - Less than 9th grade` +
      `Estimate; Female: - 25 to 34 years: - 9th to 12th grade, no diploma` +
      `Estimate; Female: - 35 to 44 years: - 9th to 12th grade, no diploma` +
      `Estimate; Female: - 45 to 64 years: - 9th to 12th grade, no diploma`) %>%
  filter(county_FIPS %in% MSA_FIPS$FIPS) %>%
  left_join(MSA_FIPS, by = c("county_FIPS" = "FIPS"))

insurance <- read_tract(path %p% "B27001", starting_year = 2010)

insurance %<>%
  transmute(
    county_FIPS = tract %>% str_sub(1, 5) %>% as.numeric %>% as.character,
    tract,
    year,
    total_insurance = `Estimate; Total:`,
    uninsured =
      `Estimate; Male: - Under 6 years: - No health insurance coverage` +
      `Estimate; Male: - 6 to 17 years: - No health insurance coverage` +
      `Estimate; Male: - 18 to 24 years: - No health insurance coverage` +
      `Estimate; Male: - 25 to 34 years: - No health insurance coverage` +
      `Estimate; Male: - 35 to 44 years: - No health insurance coverage` +
      `Estimate; Male: - 45 to 54 years: - No health insurance coverage` +
      `Estimate; Male: - 55 to 64 years: - No health insurance coverage` +
      `Estimate; Male: - 65 to 74 years: - No health insurance coverage` +
      `Estimate; Male: - 75 years and over: - No health insurance coverage` +
      `Estimate; Female: - Under 6 years: - No health insurance coverage` +
      `Estimate; Female: - 6 to 17 years: - No health insurance coverage` +
      `Estimate; Female: - 18 to 24 years: - No health insurance coverage` +
      `Estimate; Female: - 25 to 34 years: - No health insurance coverage` +
      `Estimate; Female: - 35 to 44 years: - No health insurance coverage` +
      `Estimate; Female: - 45 to 54 years: - No health insurance coverage` +
      `Estimate; Female: - 55 to 64 years: - No health insurance coverage` +
      `Estimate; Female: - 65 to 74 years: - No health insurance coverage` +
      `Estimate; Female: - 75 years and over: - No health insurance coverage`) %>%
  filter(county_FIPS %in% MSA_FIPS$FIPS) %>%
  left_join(MSA_FIPS, by = c("county_FIPS" = "FIPS"))

low_income <- read_tract(path %p% "C17002")

low_income %<>%
  transmute(
    county_FIPS = tract %>% str_sub(1, 5) %>% as.numeric %>% as.character,
    tract,
    year,
    total_low_income = `Estimate; Total:`,
    low_income =
      `Estimate; Under .50` +
      `Estimate; .50 to .99` +
      `Estimate; 1.00 to 1.24` +
      `Estimate; 1.25 to 1.49`) %>%
  filter(county_FIPS %in% MSA_FIPS$FIPS) %>%
  left_join(MSA_FIPS, by = c("county_FIPS" = "FIPS"))

unemployment_07  <- read_tract(path %p% "S23001/07")
unemployment_13  <- read_tract(path %p% "S23001/13", starting_year = 2013)

unemployment_07 %<>%
  transmute(
    county_FIPS = tract %>% str_sub(1, 5) %>% as.numeric %>% as.character,
    tract,
    year,
    total_unemployment =
      `Total; Estimate; Population 16 years and over` *
      as.numeric(`In labor force; Estimate; Population 16 years and over`) / 100,
    unemployment =
      as.numeric(`Unemployment rate; Estimate; Population 16 years and over`))

unemployment_13 %<>%
  transmute(
    county_FIPS = tract %>% str_sub(1, 5) %>% as.numeric %>% as.character,
    tract,
    year,
    total_unemployment =
      `Total; Estimate; Population 16 years and over` *
      as.numeric(`Labor Force Participation Rate; Estimate; Population 16 years and over`) / 100,
    unemployment =
      as.numeric(`Unemployment rate; Estimate; Population 16 years and over`))

unemployment <- bind_rows(unemployment_07, unemployment_13)

unemployment %<>%
  filter(county_FIPS %in% MSA_FIPS$FIPS) %>%
  left_join(MSA_FIPS, by = c("county_FIPS" = "FIPS"))

mpi <- bind_df(degree, insurance, low_income, unemployment,
                  by = c("MSA", "county_FIPS", "tract", "year"))

mpi %<>%
  transmute(
    MSA,
    FIPS = county_FIPS,
    tract,
    year,
    pop_degree = total_degree,
    no_hs = no_hs / total_degree * 100,
    pop_insurance = total_insurance,
    uninsured = uninsured /total_insurance * 100,
    pop_low_income = total_low_income,
    low_income = low_income / total_low_income * 100,
    pop_unemployment = total_unemployment,
    unemployment) %>%
  filter(pop_degree > 0, pop_insurance > 0,
         pop_low_income > 0, pop_unemployment > 0)

mpi_msa_5yr <- mpi %>%
    group_by(MSA, year) %>%
    mutate(
      no_hs_z        = norm_z(no_hs),
      uninsured_z    = norm_z(uninsured),
      low_income_z   = norm_z(low_income),
      unemployment_z = norm_z(unemployment),
      MPI = (no_hs_z + uninsured_z + low_income_z + unemployment_z) / 4,
      population = pop_insurance,
      mpi_tract         = if_else(MPI > 1, 1, 0),
      extreme_mpi_tract = if_else(MPI > 2.5, 1, 0)) %>%
    summarise(
      pop_mpi = sum(population[mpi_tract == 1]),
      pop_extreme_mpi = sum(population[extreme_mpi_tract == 1]),
      population = sum(population)) %>%
    ungroup() %>%
    transmute(
      MSA,
      year,
      sex = "total",
      race = "total",
      MPI = pop_mpi / population * 100,
      MPI_extreme = pop_extreme_mpi / population * 100) %>%
  organize()

mpi_county <- mpi %>%
  filter(FIPS %in% FIPS_df$FIPS) %>%
  mutate(FIPS = replace(FIPS, FIPS %in% c("29189", "29510"), "MERGED")) %>%
  group_by(FIPS, year) %>%
  mutate(
    no_hs_z        = norm_z(no_hs),
    uninsured_z    = norm_z(uninsured),
    low_income_z   = norm_z(low_income),
    unemployment_z = norm_z(unemployment),
    MPI = (no_hs_z + uninsured_z + low_income_z + unemployment_z) / 4,
    population = pop_insurance,
    mpi_tract         = if_else(MPI > 1, 1, 0),
    extreme_mpi_tract = if_else(MPI > 2.5, 1, 0)) %>%
  summarise(
    pop_mpi = sum(population[mpi_tract == 1]),
    pop_extreme_mpi = sum(population[extreme_mpi_tract == 1]),
    population = sum(population)) %>%
  ungroup() %>%
  transmute(
    FIPS,
    year,
    sex = "total",
    race = "total",
    MPI = pop_mpi / population * 100,
    MPI_extreme = pop_extreme_mpi / population * 100) %>%
  organize()

mpi_tract <- mpi %>%
  filter(FIPS == 21111) %>%
  group_by(year) %>%
  mutate(
    tract = "1400000US" %p% tract,
    no_hs_z        = norm_z(no_hs),
    uninsured_z    = norm_z(uninsured),
    low_income_z   = norm_z(low_income),
    unemployment_z = norm_z(unemployment),
    MPI = (no_hs_z + uninsured_z + low_income_z + unemployment_z) / 4,
    population = pop_insurance,
    mpi_tract         = if_else(MPI > 1, 1, 0),
    extreme_mpi_tract = if_else(MPI > 2.5, 1, 0)) %>%
  ungroup()

mpi_nh <- mpi %>%
  filter(FIPS == 21111) %>%
  mutate(tract = "1400000US" %p% tract) %>%
  left_join(nh_tract, by = c("tract" = "GEO_ID")) %>%
  group_by(neighborhood, year) %>%
  summarise(
    no_hs        = weighted.mean(no_hs, pop_degree),
    uninsured    = weighted.mean(uninsured, pop_insurance),
    low_income   = weighted.mean(low_income, pop_low_income),
    unemployment = weighted.mean(unemployment, pop_unemployment),
    pop_insurance = sum(pop_insurance)) %>%
  group_by(year) %>%
  mutate(
    no_hs_z        = norm_z(no_hs),
    uninsured_z    = norm_z(uninsured),
    low_income_z   = norm_z(low_income),
    unemployment_z = norm_z(unemployment),
    MPI = (no_hs_z + uninsured_z + low_income_z + unemployment_z) / 4,
    population = pop_insurance,
    mpi_tract         = if_else(MPI > 1, 1, 0),
    extreme_mpi_tract = if_else(MPI > 2.5, 1, 0)) %>%
  ungroup()

update_sysdata(mpi_county, mpi_msa_5yr, mpi_tract, mpi_nh)
