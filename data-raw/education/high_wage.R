library(glptools)
glp_load_packages()

acs_micro <- feather::read_feather("data-raw/microdata/acs_micro_repwts.feather")

hw_2000 <- c(
  #Management, business, science, and arts occupations
  1:354,
  #Law enforcement supervisors
  370, 371,
  #Law enforcement workers
  380:385,
  #Installation, maintenance, and repair occupations
  700:762)

hw_acs <- c(
  #Management, business, science, and arts occupations
  10:3655, 4465, 3945,
  #Law enforcement supervisors
  3700, 3710,
  #Law enforcement workers
  3800:3850,
  #Installation, maintenance, and repair occupations
  6540, 7000:7630)

acs_micro %<>%
  mutate(
    high_wage = case_when(
      year == 2000 & OCC %in% hw_2000 ~ TRUE,
      year >= 2005 & OCC %in% hw_acs  ~ TRUE,
      OCC == 0 ~ NA,
      TRUE ~ FALSE))

high_wage_county  <- survey_by_demog(acs_micro, high_wage)
high_wage_msa_1yr <- survey_by_demog(acs_micro, high_wage, geog = "MSA")

high_wage_05_5yr <- build_census_var_df("acs5", "C24010") %>% filter(year >= 2014, race == "total")

high_wage_recode = data.frame(
  variable = c("C24010_002E", "C24010_003E", "C24010_023E", "C24010_032E", "C24010_038E", "C24010_039E", "C24010_059E", "C24010_068E"),
  label = c("total", "high_wage", "high_wage", "high_wage"),
  sex   = rep(c("male", "female"), each = 4),
  group = c("total",
            "Management business science and arts occupations",
            "Service occupations..Protective service occupations..Law enforcement workers including supervisors",
            "Natural resources construction and maintenance occupations..Construction and extraction occupations"),
  stringsAsFactors = F)

high_wage_05_5yr %<>% filter(variable %in% high_wage_recode$variable)

high_wage_map    <- get_census(high_wage_05_5yr, geog = "tract", parallel =  T)

high_wage_map %<>%
  transmute(tract, year, race, sex, value, variable) %>%
  left_join(high_wage_recode, by = c("sex", "variable")) %>%
  group_by(tract, year, race, sex, label) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = label, values_from = value) %>%
  total_demographics(high_wage:total) %>%
  organize()

high_wage_map %>%
  process_map("high_wage", pop = "total", method = "percent", return_name = "high_wage") %>%
  list2env(.GlobalEnv)

usethis::use_data(high_wage_msa_1yr, high_wage_county,
                  high_wage_tract, high_wage_nh, high_wage_muw, overwrite = TRUE)

rm(hw_2000, hw_acs, acs_micro, high_wage_map, high_wage_05_5yr, high_wage_recode)
