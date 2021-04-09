library(glptools)
glp_load_packages()

path <- "data-raw/qop/crime/"

test <- any_time(path)

test1 <- test %>%
  mutate(year_occured = lubridate::year(DATE_OCCURED))

library(tidygeocoder)

test1 %<>%
  mutate(
    street = BLOCK_ADDRESS,
    city = CITY,
    state = "KY",
    zip = ZIP_CODE)

test2 <- test1[1:100,] %>%
  geocode(
    street = street,
    city = city,
    state = state,
    postalcode = zip,
    full_results = TRUE,
    return_type = "geographies")

test3 <- test2 %>%
  select(INCIDENT_NUMBER:zip) %>%
  geocode(
    street = street,
    city = city,
    state = state,
    postalcode = zip,
    full_results = TRUE)
