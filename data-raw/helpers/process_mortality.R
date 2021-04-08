process_mortality <- function(df){
  # Extract total data for populous counties
  totals <- df %>%
    filter(FIPS ==  "total") %>%
    select(-FIPS) %>%
    pivot_vartype_wider("deaths") %>%
    rename(
      total_deaths = estimate,
      total_population = population)

  # Append data to df and calculate ypll per 100,000 residents
  df %>%
    filter(FIPS != "total") %>%
    pivot_vartype_wider("deaths") %>%
    left_join(totals, by = df %cols_in% c("year", "age", "race", "sex")) %>%
    mutate(
      deaths = estimate - total_deaths,
      population = population - total_population) %>%
    select(-total_deaths, -total_population) %>%
    filter(age < 75) %>%
    mutate(ypll = deaths * (75 - age)) %>%
    age_adj_rate(var = "ypll") %>%
    organize()
}
