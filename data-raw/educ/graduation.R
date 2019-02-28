library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

source("data-raw/helpers/clean_ky_ed.R")

path <- "data-raw/educ/graduation/"

grad_read <- function(folder) {

  files <- list.files(getwd() %p% "/" %p% folder)
  y <- 2013

  var_names <- c("COHORT_RATE", "COHORT_2014", "COHORT_2015",
                "REPORTYEAR_2016", "REPORTYEAR_2017", "GRADRATE4YR")

  for (f in files){
    df <- read_csv(folder %p% f)

    df %<>%
      filter(
        grepl("District Total", SCH_NAME),
        !is.na(DIST_NAME))

    if(y %in% 2013){
      df %<>%
        transmute(
          district = DIST_NAME,
          year = y,
          demographic = DISAGG_LABEL,
          num_students = COHORT_DENOMINATOR,
          grad_rate = COHORT_RATE)
    }
    if(y %in% 2014:2017){

      df$value <- df[[var_names[y - 2012]]]

      df %<>%
        filter(
          COHORT_TYPE == "FOUR YEAR",
          TARGET_LABEL %in% c("Actual Score", "Denominator Count")) %>%
        transmute(
          district = DIST_NAME,
          year = y,
          demographic = DISAGG_LABEL,
          TARGET_LABEL,
          value = as.numeric(value))

      score_df <- df %>%
        filter(TARGET_LABEL == "Actual Score") %>%
        select(-TARGET_LABEL) %>%
        rename(grad_rate = value)

      denom_df <- df %>%
        filter(TARGET_LABEL == "Denominator Count") %>%
        select(-TARGET_LABEL) %>%
        rename(num_students = value)

      df <- full_join(score_df, denom_df,
                      by = c("district", "year", "demographic"))


    }
    if(y %in% 2018){
      df %<>%
        transmute(
          district = DIST_NAME,
          year = y,
          demographic = DEMOGRAPHIC,
          num_students = COHORT4YR,
          grad_rate = GRADRATE4YR)
    }

    df %<>%
      mutate(
        num_students = gsub(",", "", num_students, fixed = TRUE),
        num_students = as.numeric(num_students),
        grad_rate = as.numeric(grad_rate))

    if(y == 2013){
      output <- df
    }
    else{
      output %<>% rbind(df)
    }

    y <- y + 1
  }
  output
}

graduation <- grad_read(path)

graduation %<>% clean_ky_ed(grad_rate)

usethis::use_data(graduation, overwrite = TRUE)

rm(path, clean_ky_ed, grad_read)
