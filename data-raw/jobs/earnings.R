library(glptools)
glp_load_packages()

earnings_00_total <- build_census_var_df("sf3", "P085")
earnings_05_total_1yr <- build_census_var_df("acs1", "B20017")
earnings_05_total_5yr <- build_census_var_df("acs5", "B20017")

earnings_total <- bind_rows(earnings_00_total, earnings_05_total_1yr)

earnings_county <- get_census(earnings_total, "FIPS")
earnings_map    <- get_census(filter(earnings_05_total_5yr, year >= 2010), "tract")

earnings_county %<>%
  filter(str_detect(label, "Other", negate = TRUE)) %>%
  mutate(var_name = if_else(str_detect(label, "full-time"), "median_earnings_ft", "median_earnings")) %>%
  pivot_wider(id_cols = FIPS:var_type, names_from = var_name, values_from = value) %>%
  stl_merge(median_earnings:median_earnings_ft) %>%
  COLA(median_earnings:median_earnings_ft)

earnings_gap <- earnings_county %>%
  filter(sex == "total") %>%
  select(-median_earnings_ft) %>%
  pivot_wider(names_from = race, values_from = median_earnings) %>%
  transmute(
    FIPS, year, sex,
    race = "total",
    earnings_gap_wb = white - black)

earnings_county %<>% bind_df(earnings_gap)

earnings_map %<>%
  filter(str_detect(label, "Other", negate = TRUE)) %>%
  mutate(var_name = if_else(str_detect(label, "full-time"), "median_earnings_ft", "median_earnings")) %>%
  pivot_wider(id_cols = tract:sex, names_from = var_name, values_from = earnings) %>%
  mutate_at(vars(median_earnings:median_earnings_ft), ~ if_else(. == -666666666, NA_real_, .))

process_map(earnings_map, median_earnings, return_name = "earnings", method = "mean") %>%
  list2env(.GlobalEnv)

usethis::use_data(earnings_county, earnings_tract, earnings_nh, earnings_muw, overwrite = TRUE)

rm(earnings_00_total, earnings_05_total_1yr, earnings_05_total_5yr, earnings_total, earnings_gap)
