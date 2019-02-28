library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

source("data-raw/helpers/clean_ky_ed.R")

path <- "data-raw/educ/ccr/"

ccr_read <- function(folder) {

  files <- list.files(getwd() %p% "/" %p% folder)
  y <- 2012

  for(f in files){
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
          num_students = NBR_GRADUATES_WITH_DIPLOMA,
          ccr = PCT_CCR_NO_BONUS)
    }
    if(y %in% 2018){
      df %<>%
        transmute(
          district = DIST_NAME,
          year = y,
          demographic = DEMOGRAPHIC,
          num_students = GRADS,
          ccr = TRANSITIONRATE)
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

ccr <- ccr_read(path)

ccr %<>% clean_ky_ed(ccr)

usethis::use_data(ccr, overwrite = TRUE)

rm(path, ccr_read, clean_ky_ed)
