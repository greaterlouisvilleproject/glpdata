library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/education/preschool/"

process_00 <- function(df){
  df %<>%
    transmute(
      FIPS = as.character(as.numeric(Id2)),
      year = 2000,

      enrolled_3_4.male =
        `Male: - Enrolled in school: - 3 and 4 years`,
      enrolled_3_4.female =
        `Female: - Enrolled in school: - 3 and 4 years`,
      enrolled_3_4 =
        enrolled_3_4.male + enrolled_3_4.female,

      total.male =
        enrolled_3_4.male +
        `Male: - Not enrolled in school: - 3 and 4 years`,
      total.female =
        enrolled_3_4.female +
        `Female: - Not enrolled in school: - 3 and 4 years`,
      total =
        total.male + total.female)

  df

}
process_05 <- function(df, geog){

  col_name <- switch(geog, "FIPS" = "FIPS", "MSA" = "Id2", "tract" = "Id")

  df %>%
    transmute(
      !!geog := as.character(.data[[col_name]]),
      year,

      enrolled_3_4.male =
        `Estimate; Male: - Enrolled in public school: - 3 and 4 years` +
        `Estimate; Male: - Enrolled in private school: - 3 and 4 years`,
      enrolled_3_4.female =
        `Estimate; Female: - Enrolled in public school: - 3 and 4 years` +
        `Estimate; Female: - Enrolled in private school: - 3 and 4 years`,
      enrolled_3_4 =
        enrolled_3_4.male + enrolled_3_4.female,

      total.male =
        enrolled_3_4.male +
        `Estimate; Male: - Not enrolled in school: - 3 and 4 years`,
      total.female =
        enrolled_3_4.female +
        `Estimate; Female: - Not enrolled in school: - 3 and 4 years`,
      total =
        total.male + total.female)
}
process_preschool <- function(df){

  if (df_type(df) == "MSA") geog <- "MSA" else geog <- "FIPS"

  df %<>%
    pull_peers(add_info = FALSE) %>%
    reshape_sex() %>%
    mutate(race = "total") %>%
    select(!!geog, year, race, sex, total, enrolled_3_4) %>%
    {if(df_type(df) == "FIPS") stl_merge(., total:enrolled_3_4, method = "sum") else .} %>%
    mutate(enrolled_3_4 = enrolled_3_4 / total * 100) %>%
    select(-total)

  df
}

preschool_00      <- read_csv(path %p% "DEC_00_SF3_PCT023_with_ann.csv", skip = 1)
preschool_05      <- acs_time(path %p% "B14003")
preschool_map     <- read_csv(path %p% "ACS_17_5YR_B14003_with_ann.csv", skip = 1)
preschool_msa_5yr <- acs_time(path %p% "B14003_5yr", geog = "MSA", starting_year = 2007)
preschool_msa_1yr <- read_csv(path %p% "MSA/ACS_17_1YR_B14003_with_ann.csv", skip = 1)

preschool_00      %<>% process_00()
preschool_05      %<>% process_05("FIPS")
preschool_map     %<>% mutate(year = 2015) %>% process_05("tract")
preschool_00_msa  <- preschool_00 %>% sum_FIPS_to_MSA()
preschool_msa_5yr %<>% process_05("FIPS") %>% sum_FIPS_to_MSA()
preschool_msa_1yr %<>% mutate(year = 2017) %>% process_05("MSA")

preschool_county  <- bind_rows(preschool_00, preschool_05)
preschool_msa_1yr <- bind_rows(preschool_00_msa, preschool_msa_1yr)
preschool_msa_5yr <- bind_rows(preschool_00_msa, preschool_msa_5yr)

preschool_county  %<>% process_preschool()
preschool_msa_1yr %<>% process_preschool()
preschool_msa_5yr %<>% process_preschool()

preschool_map %>%
  process_map("enrolled_3_4", "total", maps = c("nh", "muw"), return_name = "preschool") %>%
  list2env(.GlobalEnv)

update_sysdata(preschool_county, preschool_msa_1yr, preschool_msa_5yr,
               preschool_nh, preschool_muw)

rm(process_00, process_05, process_preschool, preschool_00, preschool_05,
   preschool_map, preschool_00_msa,path)
