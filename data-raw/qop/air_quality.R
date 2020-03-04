library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/air_quality/"

if("air_quality.csv" %not_in% list.files(path)) {

  criteria_pollutants <- data.frame(
    pollutant = c("CO",    "SO2",   "NO2",   "ozone", "PM10",  "PM2.5"),
    code      = c("42101", "42401", "42602", "44201", "81102", "88101"),
    stringsAsFactors = FALSE)

  # replace Birmingham FIPS with 5-digit version
  # replace Kansas City FIPS with Kansas City, KA to capture air quality meter
  urls <- crossing(years = 2000:2018,
                   FIPS = FIPS_df_two_stl$FIPS %>%
                     replace(. %in% c("1073", "29095"), c("01073", "20209")))

  urls <- crossing(years = 2009,
                   FIPS = "21111")

  urls %<>%
    mutate(
      url = paste0(
        "https://aqs.epa.gov/data/api/annualData/byCounty?email=harrison@greaterlouisvilleproject.org&key=goldfrog32",
        "&param=", paste0(criteria_pollutants$code, collapse = ","),
        "&bdate=", years, "0101", "&edate=", years, "0101",
        "&state=", str_sub(FIPS, 1, 2), "&county=", county = str_sub(FIPS, 3, 5)))

  for(url in urls$url){

    print(str_trunc(url, 22, "left"))

    response <- RCurl::getURLContent(url)

    response <- RCurl::getURLContent(urls$url)

    parsed_response <- jsonlite::fromJSON(response)
    response_data <- parsed_response$Data

    output <- assign_row_join(output, response_data)

    Sys.sleep(5)
  }

  write_csv(output, path %p% "air_quality.csv")
}

air_quality <- read_csv(path %p% "air_quality.csv")
naaqs <-       read_csv(path %p% "NAAQS.csv")

pollutant_names <- c("Nitrogen dioxide (NO2)"   = "Nitrogen dioxide",
                     "PM10 Total 0-10um STP"    = "PM10",
                     "PM2.5 - Local Conditions" = "PM2.5")

standard_names <- c("CO 1-hour 1971"    = "1 hour",
                    "CO 8-hour 1971"    = "8 hour",
                    "NO2 Annual 1971"   = "annual",
                    "NO2 1-hour"        = "1 hour",
                    "Ozone 8-hour 2015" = "8 hour",
                    "PM25 Annual 2012"  = "annual",
                    "PM25 24-hour 2012" = "24 hour",
                    "PM10 24-hour 2006" = "24 hour",
                    "SO2 1-hour 2010"   = "1 hour",
                    "SO2 3-hour 1971"   = "3 hour")

# Reshape air quality data frame
air_quality %<>%
  mutate(
    FIPS = state_code %p% county_code %>%
      as.numeric() %>% as.character() %>%
      replace(. %in% c("29189", "29510"), "MERGED") %>%
      replace(. == "20209", "29095")) %>%
  mutate(
    pollutant = parameter          %>% recode(!!!pollutant_names),
    standard =  pollutant_standard %>% recode(!!!standard_names, .default = NA_character_)) %>%
  filter(
    !is.na(standard),
    validity_indicator == "Y") %>%
  select(FIPS, year, pollutant, standard,
         arithmetic_mean, primary_exceedance_count, second_max_value,
         ninety_ninth_percentile, ninety_eighth_percentile, fourth_max_value) %>%
  group_by(FIPS, year, pollutant, standard) %>%
  summarise_all(~max(., na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(primary_exceedance_count = primary_exceedance_count %>% replace(. == -Inf, NA)) %>%
  pivot_longer(arithmetic_mean:fourth_max_value, names_to = "variable", values_to = "value")

air_quality_means <- air_quality %>%
  filter(variable == "arithmetic_mean") %>%
  transmute(
    FIPS, year,
    sex = "total", race = "total",
    pollutant = pollutant %p% " - " %p% standard,
    value) %>%
  pivot_wider(names_from = pollutant, values_from = value)

air_quality_naaqs <- air_quality %>%
  left_join(naaqs, by = c("pollutant", "standard", "variable")) %>%
  filter(!is.na(limit)) %>%
  mutate(naaqs_pct = value / limit * 100) %>%
  transmute(
    FIPS, year,
    sex = "total", race = "total",
    pollutant = pollutant %p% " - " %p% standard,
    value) %>%
  pivot_wider(names_from = pollutant, values_from = value)

aqi <- glptools:::any_time(path %p% "AQI/", starting_year = 2000)

aqi %<>%
  mutate(
    County = County %>% replace(. == "Saint Louis", "St. Louis County")) %>%
  left_join(FIPS_info, by = c("State" = "state", "County" = "county")) %>%
  pull_peers(add_info = FALSE)
  transmute(
    FIPS, year,
    aqi_days =
      `Unhealthy for Sensitive Groups Days` +
      `Unhealthy Days` +
      `Very Unhealthy Days` +
      `Hazardous Days`,
    median_aqi = `Median AQI`,
    ninetieth_aqi = `90th Percentile AQI`,
    good_days = `Good Days`,
    moderate_days = `Moderate Days`,
    unhealthy_sensitive_days = `Unhealthy for Sensitive Groups Days`,
    unhealthy_days = `Unhealthy Days`,
    very_unhealthy_days = `Very Unhealthy Days`,
    hazardous_days = `Hazardous Days`) %>%
  stl_merge(aqi_days:hazardous_days, method = "max")

png("PM25_98trend.png", 3000, 2400, res = 200)
r <- trend(air_quality_naaqs,
           "PM2.5 - 24 hour",
           plot_title = "98th percentile PM2.5")
print(r)
dev.off()

png("PM25_98rank.png", 3000, 2400, res = 200)
r <- ranking(air_quality_naaqs,
             "PM2.5 - 24 hour",
             plot_title = "98th percentile PM2.5, 2018",
             y_title = "micrograms / cubic meter",
             order = "Ascending")
print(r)
dev.off()


png("PM25_meantrend.png", 3000, 2400, res = 200)
r <- trend(air_quality_means,
           "PM2.5 - 24 hour",
           plot_title = "Average Daily PM2.5")
print(r)
dev.off()

png("PM25_meanrank.png", 3000, 2400, res = 200)
r <- ranking(air_quality_means,
             "PM2.5 - 24 hour",
             plot_title = "Average Daily PM2.5, 2018",
             y_title = "micrograms / cubic meter",
             order = "Ascending")
print(r)
dev.off()


png("aqi_trend.png", 3000, 2400, res = 200)
r <- trend(aqi,
           "aqi_days",
           plot_title = "Air Quality Alert Days",
           rollmean = 3)
print(r)
dev.off()

png("aqi_rank.png", 3000, 2400, res = 200)
r <- ranking(aqi,
             "aqi_days",
             plot_title = "Air Quality Alert Days, 2018",
             y_title = "Days",
             order = "Ascending",
             accuracy = 1)
print(r)
dev.off()



rm(naaqs, path, pollutant_names, standard_names)
