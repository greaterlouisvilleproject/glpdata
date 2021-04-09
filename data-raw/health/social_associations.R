library(glptools)
glp_load_packages()

# Social Associations comes from the U.S. Census Bureau County Business Patterns API.
# https://www.census.gov/programs-surveys/cbp.html

# Define calls to censusapi::get_census used to obtain data
get_sa_fxn <- function(FIPS, year, NAICS, naics_var_name) {
  if (year >= 2017){
    output <- tryCatch({
      censusapi::getCensus(
        name = "cbp",
        vintage = year,
        vars = "ESTAB",
        NAICS2017 = NAICS,
        regionin = "state:" %p% str_sub(FIPS, 1, 2),
        region = "county:" %p% str_sub(FIPS, 3, 5),
        key = Sys.getenv("CENSUS_API_KEY"))
    },
    error = function(cond){
      data.frame(
        state = str_sub(FIPS, 1, 2),
        county = str_sub(FIPS, 3, 5),
        ESTAB = "0",
        NAICS2017 = NAICS)
    })

    output %<>% rename(NAICS = NAICS2017)

  } else if (year >= 2012){
    output <- tryCatch({
      censusapi::getCensus(
        name = "cbp",
        vintage = year,
        vars = "ESTAB",
        NAICS2012 = NAICS,
        regionin = "state:" %p% str_sub(FIPS, 1, 2),
        region = "county:" %p% str_sub(FIPS, 3, 5),
        key = Sys.getenv("CENSUS_API_KEY"))
    },
    error = function(cond){
      data.frame(
        state = str_sub(FIPS, 1, 2),
        county = str_sub(FIPS, 3, 5),
        ESTAB = "0",
        NAICS2012 = NAICS)
    })

    output %<>% rename(NAICS = NAICS2012)

  } else if (year >= 2008){
    output <- tryCatch({
      censusapi::getCensus(
        name = "cbp",
        vintage = year,
        vars = "ESTAB",
        NAICS2007 = NAICS,
        regionin = "state:" %p% str_sub(FIPS, 1, 2),
        region = "county:" %p% str_sub(FIPS, 3, 5),
        key = Sys.getenv("CENSUS_API_KEY"))
    },
    error = function(cond){
      data.frame(
        state = str_sub(FIPS, 1, 2),
        county = str_sub(FIPS, 3, 5),
        ESTAB = "0",
        NAICS2007 = NAICS)
    })

    output %<>% rename(NAICS = NAICS2007)

  } else if (year >= 2003){
    output <- tryCatch({
      censusapi::getCensus(
        name = "cbp",
        vintage = year,
        vars = "ESTAB",
        NAICS2002 = NAICS,
        regionin = "state:" %p% str_sub(FIPS, 1, 2),
        region = "county:" %p% str_sub(FIPS, 3, 5),
        key = Sys.getenv("CENSUS_API_KEY"))
    },
    error = function(cond){
      data.frame(
        state = str_sub(FIPS, 1, 2),
        county = str_sub(FIPS, 3, 5),
        ESTAB = "0",
        NAICS2002 = NAICS)
    })

    output %<>% rename(NAICS = NAICS2002)

  } else {
    output <- tryCatch({
      censusapi::getCensus(
        name = "cbp",
        vintage = year,
        vars = "ESTAB",
        NAICS1997 = NAICS,
        regionin = "state:" %p% str_sub(FIPS, 1, 2),
        region = "county:" %p% str_sub(FIPS, 3, 5),
        key = Sys.getenv("CENSUS_API_KEY"))
    },
    error = function(cond){
      data.frame(
        state = str_sub(FIPS, 1, 2),
        county = str_sub(FIPS, 3, 5),
        ESTAB = "0",
        NAICS1997 = NAICS)
    })

    output %<>% rename(NAICS = NAICS1997)
  }

  output %<>%
    transmute(
      FIPS = paste0(state, county),
      year,
      NAICS = as.character(NAICS),
      ESTAB)
}

# Create a data frame of FIPS codes, years, and NAICS codes
social_associations <- crossing(
  FIPS = FIPS_df_two_stl$FIPS,
  year = 2000:2018,
  NAICS = c(813410, 713950, 713910, 713940, 711211, 813110, 813940, 813930, 813910, 813920))

# Fetch data
future::plan(future::multiprocess)
social_associations %<>% furrr::future_pmap_dfr(get_sa_fxn)

# Process data
social_associations %<>%
  group_by(FIPS, year) %>%
  summarise(ESTAB = sum(as.numeric(ESTAB)), .groups = "drop") %>%
  stl_merge(ESTAB, method = "sum") %>%
  per_capita_adj(ESTAB) %>%
  transmute(
    FIPS, year,
    social_associations = ESTAB_pp * 10000)

# Error in 2006 data, so replace values with NA.
social_associations %<>%
  mutate(social_associations = if_else(year == 2006, NA_real_, social_associations))

usethis::use_data(social_associations, overwrite = TRUE)

rm(get_sa_fxn)
