library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(feather)
library(Hmisc)
library(labelled)
library(googledrive)
library(purrr)

path <- "data-raw/microdata/"

# American Community Survey
acs_micro <- read_csv(path %p% "acs_micro.csv")
acs_micro %<>% process_acs()
write_feather(acs_micro, path %p% "acs_micro.feather")
#drive_upload(path %p% "acs_micro.feather", "microdata/acs_microdata.feather")

# Current Population Survey
cps_micro <- read_csv(path %p% "cps_micro.csv")
cps_micro %<>% process_cps()
write_feather(cps_micro, path %p% "cps_micro.feather")

# Behavioral Risk Factor Surveillance System
brfss_micro <- brfss_time(path %p% "brfss")
write_feather(brfss_micro, path %p% "brfss_micro.feather")

library(glpdata)
library(readr)

write_csv(education_msa_1yr, "education_1yr.csv")
write_csv(education_msa_5yr, "education_5yr.csv")
write_csv(jobs_msa_1yr, "jobs_1yr.csv")
write_csv(jobs_msa_5yr, "jobs_5yr.csv")
write_csv(health_msa_1yr, "health_1yr.csv")
write_csv(qop_msa_1yr, "qop_1yr.csv")
write_csv(MSA_df, "MSAs.csv")

process_acs <- function(df, gq = T, pull_peers = T, remove_vars = T){

  # Rename some columns
  suppressWarnings(df %<>% rename_at(df %cols_in% c("YEAR", "AGE", "SEX"), funs(str_to_lower)))

  # Remove group quarters residents if gq = FALSE
  if (!gq) df %<>% filter(GQ == 1 | GQ == 2)

  # Rename the MSA column and label the Tulsa MSA
  if ("MET2013" %in% names(df)) {

    df %<>% rename(MSA = MET2013)

    df$MSA[df$STATEFIP == 40 & df$year <= 2011 & df$PUMA %in% c(1000, 1100, 1200)] <- 46140
    df$MSA[df$STATEFIP == 40 & df$year >= 2012 & df$PUMA %in% c(1201, 1202, 1203, 1204, 1301)] <- 46140
  }

  # Add FIPS codes to data and change St. Louis FIPS codes to MERGED
  df %<>% left_join(FIPS_PUMA, by = c("STATEFIP", "PUMA", "year"))

  df$FIPS[df$FIPS == 29189] = "MERGED"
  df$FIPS[df$FIPS == 29510] = "MERGED"

  # df %<>% mutate(replace(FIPS, STATEFIP == 21 & PUMA %in% c(1901, 1902), "21067")) # Adds in Lexington

  # Subset data to peers at the MSA or county level
  if ("MSA" %in% names(df) & pull_peers) {
    df %<>% pull_peers(add_info = FALSE, geog = "MSA")
  } else if (pull_peers) {
    df %<>% pull_peers(add_info = FALSE)
  }

  # Recode race
  if ("RACE" %in% names(df)) {
    df$race <- "other"
    df$race[df$RACE == 1 & df$HISPAN == 0] <- "white"
    df$race[df$RACE == 2 & df$HISPAN == 0] <- "black"
    df$race[df$HISPAN %in% 1:4] <- "hispanic"

    df %<>% select(-RACE, -RACED, -HISPAN, -HISPAND)
  }

  # Recode sex
  if ("sex" %in% names(df)) {
    df$sex <- if_else(df$sex == 1, "male", "female")
  }

  # Recode education
  if ("EDUCD" %in% names(df)) {
    df %<>%
      mutate(
        educ = "no_hs",
        educ = replace(educ, EDUCD %in% c(62, 63, 64), "hs"),
        educ = replace(educ, EDUCD %in% c(65, 71), "some_col"),
        educ = replace(educ, EDUCD == 81, "assoc"),
        educ = replace(educ, EDUCD == 101, "bach"),
        educ = replace(educ, EDUCD %in% c(114, 115, 116), "grad"),
        educ = replace(educ, EDUCD == 1, NA)) %>%
      select(-EDUC, -EDUCD)
  }

  # Remove some variables that are no longer needed

  if (remove_vars) df %<>% select(df %cols_not_in% c("GQ", "STATEFIP", "PUMA", "DATANUM", "CBSERIAL" ))

  df
}

pull_peers <- function(df, add_info = T, subset_to_peers = T, geog = "", additional_geogs = ""){

  # If no geography provided, use MSA column. If no MSA column, use FIPS column.
  if (geog == ""){
    if ("MSA" %in% names(df)) geog <- "MSA"
    else if ("FIPS" %in% names(df)) geog <- "FIPS"
  }
  if(geog == "") stop("MSA and FIPS columns are missing from the data frame.")

  # Ensure the Brimingham FIPS code is four digits and that the MSA column is of type character
  if ("MSA" %in% names(df))  df %<>% mutate(MSA = MSA %>% as.character())
  if ("FIPS" %in% names(df)) df %<>% mutate(FIPS = FIPS %>% as.character %>% replace(FIPS == "01073", "1073"))

  # Add information columns
  if (add_info) {
    if      ("MSA" %in% names(df))  df %<>% left_join(MSA_df,  by = "MSA")
    else if ("FIPS" %in% names(df)) df %<>% left_join(FIPS_df, by = "FIPS")
  }

  # subset to peers based on geog
  if(subset_to_peers) {
    if (geog == "FIPS") df %<>% filter(FIPS %in% c(FIPS_df$FIPS, additional_geogs))
    if (geog == "MSA") {
      if ("MSA" %in% names(df)) {
        df %<>% filter(MSA %in% c(MSA_FIPS$MSA, additional_geogs))
      } else {
        df %<>% filter(FIPS %in% c(MSA_FIPS$FIPS, additional_geogs))
      }
    }
  }
  df %<>% organize()

  df
}
