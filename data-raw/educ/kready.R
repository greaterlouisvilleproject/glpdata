library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(readxl)
source("data-raw/helpers/clean_ky_ed.R")

path <- "data-raw/educ/kscreen/"

read_kscreen <- function(folder){

  files <- list.files(getwd() %p% "/" %p% folder)
  y <- 2013

  for(f in files){
    df <- read_xlsx(folder %p% f, skip = 1)

    df %<>%
      filter(
        grepl("District Total", `School Name`),
        `Prior Setting` == "All Students") %>%
      transmute(
        district = `District Name`,
        year = y,
        demographic = Demographic,
        num_students = `Number Tested`,
        kready = `Kindergarten Ready`) %>%
      mutate(
        num_students = gsub(",", "", num_students, fixed = TRUE),
        num_students = as.numeric(num_students))

    if(y == 2013){
      output <- df
    } else {
      output %<>% bind_rows(df)
    }

    y = y + 1
  }
  output
}

kready <- read_kscreen(path)

kready %<>% clean_ky_ed(kready)

usethis::use_data(kready, overwrite = TRUE)

rm(path, read_kscreen)
