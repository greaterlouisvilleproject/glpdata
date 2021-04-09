library(glptools)
glp_load_packages()

# Create a population data frame for Louisville and peer cities broken down by demographic.
# Because many data points rely on population-adjusted data, the data is approximated to cover
#   2000-2019, the full span of GLP data points.

# Fetch data from census API. Create county-level data frame for 1-year and 5-year data.

population_00_total  <- build_census_var_df("sf3", "P8", age_groups = "all")
population_00_race   <- build_census_var_df("sf3", "P145", age_groups = "all")
population_00_all    <- build_census_var_df("sf1", "P12", age_groups = "all")   # Needed for ZCTA estimates
population_05_1yr    <- build_census_var_df("acs1", "B01001", age_groups = "all")
population_05_5yr    <- build_census_var_df("acs5", "B01001", age_groups = "all")

population_vars_1yr <- bind_rows(population_00_total, population_00_race, population_05_1yr)
population_vars_5yr <- bind_rows(population_00_total, population_00_race, population_05_5yr)
population_vars_zip <- bind_rows(population_00_all,   population_05_5yr)

population_all_counties_1yr <- get_census(population_vars_1yr, "MSA",   var_name = "population", parallel = T)
population_all_counties_5yr <- get_census(population_vars_5yr, "MSA",   var_name = "population", parallel = T)
population_map              <- get_census(population_vars_5yr, "tract", var_name = "population", parallel = T)
population_tract_all        <- get_census(population_vars_5yr, "tract_all", var_name = "population", parallel = T)

for (r in unique(population_vars_zip$race)) {
  temp <- get_census(filter(population_vars_zip, race == r), "zip",   var_name = "population", parallel = T)

  population_zip <- assign_row_join(population_zip, temp)
}

# Create data frame of population for each county, race, sex, and year

# Keep one year population if span of 2007 to 2016 is complete
population_all_counties_1yr %<>%
  group_by(FIPS, race, sex) %>%
  mutate(complete_flag = if_else(any(year %in% 2007:2016 & is.na(population)), F, T)) %>%
  ungroup() %>%
  filter(complete_flag) %>%
  select(-complete_flag)

# Subset five-year data to counties not included in one year data
population_all_counties_5yr %<>%
  anti_join(population_all_counties_1yr, by = c("FIPS", "sex", "race"))

population_all_counties <- bind_rows(population_all_counties_1yr, population_all_counties_5yr)

# Create data frame of every combinations of county, race, sex, and year
population_all_counties %<>%
  complete(nesting(FIPS, race, sex), year = 2000:2018)

# approx interpolates linearly from 2000 to 2005/2007
# To avoid errors due to extrapolation, population for most recent year  is simply filled forward.
population_all_counties %<>%
  group_by(FIPS, race, sex) %>%
  nest() %>%
  mutate(
    year =  purrr::map(data, ~ approx(x = .$year, y = .$population,
                                        xout = 2000:2019, na.rm = T)$x),
    population = purrr::map(data, ~ approx(x = .$year, y = .$population,
                                           xout = 2000:2019, na.rm = T)$y)) %>%
  select(-data) %>%
  unnest(cols = c(year, population)) %>%
  fill(population, .direction = "down") %>%
  ungroup() %>%
  organize()


# Create MSA, MSA county, county, and map data frames
population_msa_1yr <- population_all_counties %>%
  sum_FIPS_to_MSA(population)

population_msa_counties <- population_all_counties

population_county <- population_all_counties %>%
  pull_peers() %>%
  stl_merge(population, method = "sum", keep_counties = T)

# Calculate population in the core county
population_county %<>%
  left_join(MSA_FIPS, by = "FIPS") %>%
  bind_df(population_msa_1yr %>% rename(msa_population = population),
          by = c("MSA", "year", "race", "sex")) %>%
  mutate(core_county = population / msa_population * 100) %>%
  select(-MSA, -msa_population) %>%
  organize()

# Create data frames of population for different years of census tracts
population_tract_all %<>%
  select(tract, year, race, sex, population) %>%
  complete_vector_arg(c("tract", "race", "sex"), years = c("2000:2007", "2008:2019")) %>%
  group_by(tract, year >= 2008, race, sex) %>%
  nest() %>%
  mutate(
    year =  purrr::map(data, ~ approx(x = .$year, y = .$population, xout = .$year, na.rm = T)$x),
    population = purrr::map(data, ~ approx(x = .$year, y = .$population, xout = .$year, na.rm = T)$y)) %>%
  select(-data) %>%
  unnest(cols = c(year, population)) %>%
  fill(population, .direction = "down") %>%
  ungroup() %>%
  select(-`year >= 2008`) %>%
  organize() %>%
  mutate(population = replace(population, tract == "21111980100", NA))

# 2000 census tracts for 2000 - 2011
population_tract_00 <- population_tract_all %>%
  filter(year <= 2007) %>%
  complete(nesting(tract, race, sex), year = 2000:2011) %>%
  fill(population)

# 2010 census tracts for 2012 - 2019
population_tract_10 <- population_tract_all %>%
  filter(year >= 2012)

# 2010 census tracts for 2000 - 2019
population_tract_00_10 <- population_tract_all %>%
  tract_00_to_10(2000:2007, population)

population_tract_00_10_lou <- population_tract_00_10 %>% filter(str_sub(tract, 1, 5) == "21111")

# Calculate population change
population_tract_00_10_lou %<>%
  group_by(year, race, sex) %>%
  mutate(total_pop = sum(population, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(tract, race, sex)  %>%
  mutate(
    #population_change is % change
    population_change = (population - first(population)) / first(population) * 100,

    #population_change_adj is % change - county-wide % change
    population_change_adj =
      population_change -
      ((total_pop - first(total_pop))/first(total_pop) * 100)) %>%
  ungroup() %>%
  select(-total_pop) %>%
  mutate(
    population_change = if_else(population_change == Inf | is.nan(population_change), NA_real_, population_change),
    population_change_adj = if_else(population_change_adj == Inf| is.nan(population_change_adj), NA_real_, population_change_adj))

process_map(population_tract_00_10_lou, population, pop = population, method = "sum", return_name = "population_num") %>%
  list2env(.GlobalEnv)

process_map(population_tract_00_10_lou, population_change:population_change_adj,
            method = "mean", pop = population, return_name = "population_change") %>%
  list2env(.GlobalEnv)

population_tract <- bind_df(population_num_tract, population_change_tract)
population_nh    <- bind_df(population_num_nh,    population_change_nh)
population_muw   <- bind_df(population_num_muw,   population_change_muw)


approx_catch <- function(x, y, xout, na.rm) {

  if(sum(!is.na(y)) == 1) {
    return(list(x = xout,
                y = rep(y[!is.na(y)], times = length(xout))))
  } else {
    approx(x = x, y = y, xout = xout, na.rm = na.rm)
  }
}

population_zip_full_MSA <- population_zip %>%
  filter(age_group == "all") %>%
  left_join(glptools:::FIPS_zip_full_MSA, by = "zip") %>%
  pull_peers(geog = "MSA") %>%
  transmute(
    FIPS, zip, year, sex, race,
    population_in_FIPS = population * pct_pop_in_county / 100,
    population_total = population) %>%
  complete(nesting(FIPS, zip, sex, race), year = 2000:2019) %>%
  group_by(FIPS, zip, sex, race) %>%
  nest() %>%
  mutate(
    year = purrr::map(data, ~ approx_catch(x = .$year, y = .$population_in_FIPS, xout = 2000:2019, na.rm = T)$x),
    population_in_FIPS = purrr::map(data, ~ approx_catch(x = .$year, y = .$population_in_FIPS, xout = 2000:2019, na.rm = T)$y),
    population_total = purrr::map(data, ~ approx_catch(x = .$year, y = .$population_total, xout = 2000:2019, na.rm = T)$y)) %>%
  select(-data) %>%
  unnest(cols = c(year, population_in_FIPS, population_total)) %>%
  fill(population_in_FIPS, population_total, .direction = "downup") %>%
  ungroup() %>%
  organize()

population_zip %<>%
  pull_peers()

usethis::use_data(population_county, population_msa_1yr, population_msa_counties,
                  population_tract, population_nh, population_muw, population_zip, overwrite = TRUE)

usethis::use_data(population_tract_all, population_tract_00, population_tract_10,
                  population_zip_full_MSA, overwrite = TRUE, internal = TRUE)

rm(population_00_total, population_00_race, population_00_all, population_05_1yr, population_05_5yr,
   population_vars_1yr, population_vars_5yr, population_vars_zip, population_all_counties,
   population_all_counties_1yr, population_all_counties_5yr, population_map,
   population_tract_00_10, population_tract_00_10_lou,
   population_change_tract, population_change_nh, population_change_muw,
   population_num_tract, population_num_nh, population_num_muw,
   temp, r, approx_catch)

if(FALSE){
  get_census_12 <- function(var_df, geog, var_name, parallel = F, label = F, var = F) {

    if (geog %in% c("MSA", "FIPS")) {
      fxn <- function(survey, year, geography, data, ...) {

        output <- tryCatch({
          api <- censusapi::getCensus(
            name = survey,
            vintage = year,
            vars = data$variable,
            regionin = "state:" %p% str_sub(geography, 1, 2),
            region = "county:" %p% str_sub(geography, 3, 5),
            key = Sys.getenv("CENSUS_API_KEY"))

          api %<>%
            pivot_longer(cols = data$variable) %>%
            left_join(data, by = c("name" = "variable")) %>%
            transmute(
              FIPS = geography,
              year = if_else(str_detect(survey, "acs5"), year - 2, year),
              race,
              sex,
              value,
              label,
              variable = name)
        },
        error = function(cond){
          data.frame(
            FIPS = geography,
            year = if_else(str_detect(survey, "acs5"), year - 2, year),
            race = data$race,
            sex  = data$sex,
            value = rep(NA_real_, nrow(data)),
            label = data$label,
            variable = data$variable,
            stringsAsFactors = F)
        })

        output
      }
    } else if (geog == "tract") {
      fxn <- function(survey, year, data, ...) {

        api <- censusapi::getCensus(
          name = survey,
          vintage = year,
          vars = data$variable,
          regionin = "state:21&county:111",
          region = "tract:*",
          key = Sys.getenv("CENSUS_API_KEY"))

        api %<>%
          pivot_longer(cols = data$variable) %>%
          left_join(data, by = c("name" = "variable")) %>%
          transmute(
            tract = paste0("21111", tract),
            year  = if_else(str_detect(survey, "acs5"), year - 2, year),
            race,
            sex,
            value,
            label,
            variable = name)
      }
    } else if (geog == "zip") {
      fxn <- function(survey, year, data, ...) {

        api <- censusapi::getCensus(
          name = survey,
          vintage = year,
          vars = data$variable,
          region = "zip code tabulation area:*",
          key = Sys.getenv("CENSUS_API_KEY"))

        api %<>%
          pivot_longer(cols = data$variable) %>%
          left_join(data, by = c("name" = "variable")) %>%
          transmute(
            zip = zip_code_tabulation_area,
            if_else(str_detect(survey, "acs5"), year - 2, year),
            race,
            sex,
            value,
            label,
            variable = name)
      }
    }

    if (geog %in% c("MSA", "FIPS")) {
      if (geog == "FIPS") geography <- FIPS_df_two_stl$FIPS
      if (geog == "MSA")  geography <- MSA_FIPS_2012 %>% filter(FIPS != "MERGED") %>% pull(FIPS)

      var_df <- tidyr::crossing(geography, var_df)
      grouping_vars <- c("survey", "geography", "year")
    } else {
      grouping_vars <- c("survey", "year")
    }

    output <- var_df %>%
      group_by_at(grouping_vars) %>%
      nest()

    if (parallel) {
      future::plan(future::multiprocess)
      output %<>% furrr::future_pmap_dfr(fxn)
    } else {
      output %<>% purrr::pmap_dfr(fxn)
    }

    if (!missing(var_name)) output %<>% rename(!!var_name := value)

    output
  }

  sum_FIPS_to_MSA_12 <- function(df, ..., other_grouping_vars = "") {

    variables <- dplyr:::tbl_at_vars(df, vars(...))

    grouping_vars <- c("MSA", "year", "sex", "race", other_grouping_vars)

    df %<>%
      left_join(MSA_FIPS_2012, by = "FIPS") %>%
      select(-FIPS) %>%
      group_by_at(. %cols_in% grouping_vars) %>%
      summarise_at(variables, sum) %>%
      ungroup() %>%
      filter(!is.na(MSA))
  }

  population_all_counties_1yr_12 <- get_census_12(population_vars_1yr, "MSA", var_name = "population", parallel = T)
  population_all_counties_5yr_12 <- get_census_12(population_vars_5yr, "MSA", var_name = "population", parallel = T)

  population_all_counties_1yr_12 %<>%
    group_by(FIPS, race, sex) %>%
    mutate(complete_flag = if_else(any(year %in% 2007:2016 & is.na(population)), F, T)) %>%
    ungroup() %>%
    filter(complete_flag) %>%
    select(-complete_flag)

  # Subset five-year data to counties not included in one year data
  population_all_counties_5yr_12 %<>%
    anti_join(population_all_counties_1yr_12, by = c("FIPS", "sex", "race"))

  population_all_counties_12 <- bind_rows(population_all_counties_1yr_12, population_all_counties_5yr_12)

  # Full set of combinations of county, race, sex, and year
  population_all_counties_12 %<>%
    complete(nesting(FIPS, race, sex), year = 2000:2018)

  # approxExtrap interpolates linearly from 2000 to 2005/2007
  # to avoid errors due to extrapolation, population for 2018 is copied to 2019
  population_all_counties_12 %<>%
    group_by(FIPS, race, sex) %>%
    mutate(
      population =
        Hmisc::approxExtrap(x = 2000:2018, y = population,
                            xout = 2000:2018, na.rm = T)$y %>%
        replace(. < 0, 0)) %>%
    ungroup() %>%
    complete(nesting(FIPS, race, sex), year = 2000:2019) %>%
    fill(population)

  population_msa_1yr_12 <- population_all_counties_12 %>%
    sum_FIPS_to_MSA_12(population)

  population_county_12 <- population_all_counties_12 %>%
    pull_peers(add_info = F) %>%
    stl_merge(population, method = "sum")

  population_county_12 %<>%
    left_join(MSA_FIPS_2012, by = "FIPS") %>%
    bind_df(population_msa_1yr_12 %>% rename(msa_population = population),
            by = c("MSA", "year", "race", "sex")) %>%
    mutate(core_county = population / msa_population * 100) %>%
    select(-MSA, -msa_population) %>%
    organize()
}
