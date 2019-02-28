library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

source("data-raw/helpers/clean_ky_ed.R")

path <- "data-raw/educ/act/"

act_read <- function(folder) {

  files <- list.files(getwd() %p% "/" %p% folder)
  y <- 2012

  for (f in files) {

    df <- read_csv(folder %p% f)

    df %<>%
      filter(
        grepl("District Total", SCH_NAME),
        !is.na(DIST_NAME))

    if(y %in% 2012:2017){
      df %<>%
        transmute(
          district = DIST_NAME,
          year = y,
          demographic = DISAGG_LABEL,
          num_students = STDNT_TESTED_CNT,
          act_english = ENGLISH_MEAN_SCORE,
          act_math = MATHEMATICS_MEAN_SCORE,
          act_reading = READING_MEAN_SCORE,
          act_science = SCIENCE_MEAN_SCORE,
          act_composite = COMPOSITE_MEAN_SCORE)
    }
    if(y %in% 2018){
      df %<>%
        transmute(
          district = DIST_NAME,
          year = y,
          demographic = DEMOGRAPHIC,
          num_students = TESTED,
          act_english = AVG_ENG,
          act_math = AVG_MA,
          act_reading = AVG_RD,
          act_science = AVG_SC,
          act_composite = AVG_COMP)
    }

    df %<>%
      mutate(
        num_students = gsub(",", "", num_students, fixed = TRUE),
        num_students = as.numeric(num_students))

    if (y == 2012) {
      output <- df
    }
    else{
      output %<>% bind_rows(df)
    }

    y <- y + 1
  }
  output
}

act <- act_read(path)

act %<>% clean_ky_ed(act_english:act_composite)

usethis::use_data(act, overwrite = TRUE)

rm(act_read, clean_ky_ed, path)
