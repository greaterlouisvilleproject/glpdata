library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

source("data-raw/helpers/clean_ky_ed.R")

path <- "data-raw/educ/naep/"

read_naep <- function(folder, score_type){

  files <- list.files(getwd() %p% "/" %p% folder %p% score_type)

  for (f in files){
    df <- read_csv(folder %p% score_type %p% "/" %p% f,
                   skip = 9,
                   col_names = c("year", "geography",
                                 "demographic", "naep_" %p% score_type),
                   col_types = "icci")

    if(!exists("output")){
      output <- df
    }
    else{
      output %<>% bind_rows(df)
    }
  }
  output
}

naep_math <- read_naep(path, score_type = "math")
naep_reading <- read_naep(path, score_type = "reading")

naep <- full_join(naep_math, naep_reading,
                  by = c("year", "geography", "demographic"))

naep %<>%
  clean_ky_ed(naep_math:naep_reading, calc_nonfrl = FALSE) %>%
  select(geography, year, demographic, naep_math, naep_reading) %>%
  mutate(geography = replace(geography, geography == "Jefferson County (KY)", "Jefferson County")) %>%
  arrange(geography, year, demographic)

usethis::use_data(naep, overwrite = TRUE)

rm(path, read_naep, naep_math, naep_reading)
