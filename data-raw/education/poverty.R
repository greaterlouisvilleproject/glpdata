library(glptools)
glp_load_packages()

poverty_vars_00_total <- build_census_var_df("sf3", "PCT049", additional_filters = "below|above")
poverty_vars_00_race  <- build_census_var_df("sf3", "PCT075", additional_filters = "below|above")
poverty_vars_05_1yr   <- build_census_var_df("acs1", "B17001", additional_filters = "below|above")
poverty_vars_05_5yr   <- build_census_var_df("acs5", "B17001", additional_filters = "below|above")

poverty_vars_1yr <- bind_rows(poverty_vars_00_total, poverty_vars_00_race, poverty_vars_05_1yr)
poverty_vars_5yr <- bind_rows(poverty_vars_00_total, poverty_vars_00_race, poverty_vars_05_5yr)

poverty_county            <- get_census(poverty_vars_1yr, "FIPS")
#poverty_all_counties_5yr <- get_census(poverty_vars_5yr, "FIPS")
poverty_map               <- get_census(poverty_vars_5yr, "tract")
#poverty_zip               <- get_census(filter(poverty_vars_5yr, year >= 2010), "zip")

poverty_county %<>% mutate(poverty_status = if_else(str_detect(label, "below"), T, F))
poverty_map    %<>% mutate(poverty_status = if_else(str_detect(label, "below"), T, F))
#poverty_zip    %<>% mutate(poverty_status = if_else(str_detect(label, "below"), T, F))

poverty_county %<>% process_census(cat_var = "poverty_status",
                                   output_name = "poverty",
                                   age_groups = c("all", "0_4", "0_17"))

poverty_map %<>% process_census(cat_var = "poverty_status",
                                output_name = "poverty",
                                age_groups = c("all", "0_4", "0_17"))

#Create zip-code poverty data?
if(FALSE){
   test1 <- glpdata:::population_zip_full_MSA %>%
      group_by(zip, year, sex, race, population_total) %>%
      summarise(
         population_in_MSA = sum(population_in_FIPS), .groups = "drop")

   test2 <- poverty_zip %>%
      process_census(cat_var = "poverty_status",
                     output_name = "poverty")

   test4 <- glptools:::FIPS_zip_full_MSA %>% filter(pct_pop_in_county >= 50)

   test3 <- test2 %>%
      left_join(MSA_zip, by = "zip") %>%
      filter(MSA == "31140", sex == "total", race == "total") %>%
      left_join(test1, by = c("zip", "year", "race", "sex")) %>%
      transmute(zip, poverty, year,
                population_in_Louisville = population_in_MSA,
                population_total,
                percent_in_Louisville = population_in_Louisville / population_total * 100) %>%
      left_join(test4) %>%
      left_join(MSA_FIPS_info) %>%
      filter(percent_in_Louisville > 50) %>%
      select(zip, year, poverty, county, population_in_Louisville, population_total, percent_in_Louisville)

   write_csv(test3, "poverty_in_Louisville_2019.csv")

}

process_map(poverty_map, poverty:poverty_under_18, return_name = "poverty") %>% list2env(.GlobalEnv)

# Let's look at the # of kids under each % of poverty
if(FALSE) {
  acs_micro <- feather::read_feather("data-raw/microdata/acs_micro_repwts.feather")

  acs_micro %<>%
     filter(age == 4, FIPS == "21111")

  temp <- acs_micro %>%
     filter(FIPS == "21111") %>%
     mutate(poverty = POVERTY < 150) %>%
     survey_by_demog("poverty",
                     type = "categorical",
                     breakdowns = c("total", "race"))

  temp2 <- acs_micro %>%
     filter(FIPS == "21111", year %in% 2019) %>%
     mutate(poverty = POVERTY < 150) %>%
     svy_bootstrap(var = "poverty",
                   weight_var = "PERWT",
                   type = "categorical",
                   grouping_vars = "FIPS")

  temp %<>%
     filter(race == "total")

  write_csv(temp, "child_poverty.csv")

  pov_threshold <- function(pct) {
     acs_micro %>%
        filter(FIPS == "21111", year == 2019) %>%
        mutate(poverty = POVERTY <= pct) %>%
        survey_by_demog("poverty", method="bootstrap", breakdowns = c("total", "race")) %>%
        mutate(threshold = pct)
  }

  library(future)
  plan("multisession")

  test <- map_dfr(seq(5, 500, by=1),
                                ~pov_threshold(.))

  graph_df <- test %>%
     filter(race %in% c("black", "total"), var_type %in% c("percent.estimate", "percent.lower", "percent.upper")) %>%
     pivot_wider(values_from = poverty, names_from = "var_type")

  ggplot(graph_df, aes(x=threshold, y=percent.estimate, group=race, color=race)) +
      geom_line() +
      geom_point()+
      geom_errorbar(aes(ymin=percent.lower, ymax=percent.upper))
}


usethis::use_data(poverty_county, poverty_tract, poverty_nh, poverty_muw, overwrite = TRUE)

rm(poverty_vars_00_total, poverty_vars_00_race, poverty_vars_05_1yr, poverty_vars_05_5yr,
   poverty_vars_1yr, poverty_vars_5yr,
   poverty_map)
