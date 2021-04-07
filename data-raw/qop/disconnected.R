library(glptools)
glp_load_packages()

acs_micro <- feather::read_feather("data-raw/microdata/acs_micro_repwts.feather")

acs_micro %<>%
  filter(age >= 16 & age <= 24) %>%
  mutate(
    MSA = as.character(MSA),
    not_employed = if_else(EMPSTAT == 2 | EMPSTAT == 3, 1, 0),
    not_employed = replace(not_employed, EMPSTAT == 0, NA),

    not_in_school = if_else(SCHOOL == 1, 1, 0),
    not_in_school = replace(not_in_school, SCHOOL == 0, NA),

    disconnected = if_else(not_employed & not_in_school, T, F),
    disconnected = replace(disconnected,
                           is.na(not_employed) | is.na(not_in_school),
                           NA))

disconnected_county  <- survey_by_demog(acs_micro, "disconnected")
disconnected_msa_1yr <- survey_by_demog(acs_micro, "disconnected", geog = "MSA")

#Map making from ACS5
disconnected_vars_05_5yr   <- build_census_var_df("acs5", "B14005")

#total columns = "B14005_001E", "B14005_002E", "B14005_016E"
#identify specific vars we want
keep_all <- c("B14005_003E", "B14005_009E", "B14005_013E", "B14005_017E",
                    "B14005_023E", "B14005_027E", "B14005_010E", "B14005_011E", "B14005_014E", "B14005_015E",
                    "B14005_024E", "B14005_025E", "B14005_028E", "B14005_029E",
                    "B14005_003M", "B14005_009M", "B14005_013M", "B14005_017M",
                    "B14005_023M", "B14005_027M", "B14005_010M", "B14005_011M", "B14005_014M", "B14005_015M",
                    "B14005_024M", "B14005_025M", "B14005_028M", "B14005_029M")
disconnected <- c("B14005_010E" ,"B14005_011E", "B14005_014E", "B14005_015E",
                  "B14005_024E", "B14005_025E", "B14005_028E", "B14005_029E", "B14005_010M", "B14005_011M", "B14005_014M", "B14005_015M",
                  "B14005_024M", "B14005_025M", "B14005_028M", "B14005_029M")

justgood <- disconnected_vars_05_5yr %>%
  subset(variable %in% keep_all)

#get data
youth <- get_census(justgood, "tract")

#make categorical variable
youth_2 <- youth  %>%
  mutate(disconnect = if_else(variable %in% disconnected, T, F))

#process census
clean_disconnected <- youth_2 %>%
  process_census(cat_var = "disconnect",
                 output_name = "disconnected")

#process map
map_disconnected <- clean_disconnected%>%
  process_map(disconnected, return_name = "disconnected") %>%
  list2env(.GlobalEnv)

usethis::use_data(disconnected_county, disconnected_msa_1yr,disconnected_nh, disconnected_muw, disconnected_tract, overwrite = TRUE)

rm(acs_micro, clean_disconnected, justgood, youth, youth_2, disconnected_vars_05_5yr)



