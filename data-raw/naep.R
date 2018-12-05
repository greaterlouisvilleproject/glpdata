library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/educ/naep/"

naep_time <- function(folder, score_type){
  wd <- getwd()
  directory <- paste0(wd, "/", folder, "/", score_type)
  file_names <- list.files(directory)
  n <- length(file_names)

  for (i in 1:n){
    file_path <- paste0(directory, "/", file_names[i])
    df <- read_csv(file_path, progress = FALSE, skip = 9,
                   col_names = c("year", "geography", "category", score_type),
                   col_types = "icci")

    if(i == 1){
      output <- df
    }
    else{
      names(df) <- names(output)
      output %<>% bind_rows(df)
    }
  }
  output
}

process_naep <- function(df){
  df %<>%
    filter(!is.na(category)) %>%
    filter(category %!in% c("American Indian/Alaska Native", "Asian/Pacific Islander",
                            "Information not available", "Two or more races")) %>%
    mutate(
      category = replace(category, category == "All students", "all"),
      category = replace(category, category == "Black", "black"),
      category = replace(category, category == "Eligible", "frl"),
      category = replace(category, category == "Female", "female"),
      category = replace(category, category == "Hispanic", "hispanic"),
      category = replace(category, category == "Male", "male"),
      category = replace(category, category == "Not eligible", "nonfrl"),
      category = replace(category, category == "White", "white"))

  df
}

naep_math <- naep_time(path, score_type = "math")
naep_reading <- naep_time(path, score_type = "reading")

naep_math %<>% process_naep()
naep_reading %<>% process_naep()

naep <- full_join(naep_math, naep_reading) %>%
  arrange(year, geography, category)

usethis::use_data(naep, overwrite = TRUE)

rm(naep_math, naep_reading, naep,
   naep_time, process_naep, path)
