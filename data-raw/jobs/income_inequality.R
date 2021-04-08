library(glptools)
glp_load_packages()

income_inequality_vars_05_1yr <- build_census_var_df("acs1", "B19081")
gini_vars_05_1yr <- build_census_var_df("acs1", "B19083")

income_inequality_county <- get_census(income_inequality_vars_05_1yr, "FIPS", parallel = T)
gini_county <- get_census(gini_vars_05_1yr, "FIPS", parallel = T)

income_inequality_county %<>%
  mutate(
    label = case_when(
      str_detect(label, "Lowest") ~ "income_bottom_quintile",
      str_detect(label, "Highest") ~ "income_top_quintile",
      TRUE ~ NA_character_)) %>%
  filter(!is.na(label)) %>%
  pivot_wider(id_cols = c(FIPS, year, sex, race), names_from = label, values_from = value) %>%
  mutate(income_inequality = income_top_quintile / income_bottom_quintile) %>%
  stl_merge(income_bottom_quintile:income_inequality) %>%
  COLA(income_bottom_quintile:income_top_quintile)

gini_county %<>%
  select(FIPS, year, sex, race, gini_index = value) %>%
  stl_merge(gini_index)

income_inequality_county %<>% bind_df(gini_county)

usethis::use_data(income_inequality_county, overwrite = TRUE)

rm(income_inequality_vars_05_1yr, gini_vars_05_1yr, gini_county)
