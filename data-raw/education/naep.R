library(glptools)
glp_load_packages()

source("data-raw/helpers/process_ky_ed.R")

path <- "data-raw/education/naep/"

read_naep <- function(folder, score_type){

  files <- list.files(getwd() %p% "/" %p% folder %p% score_type)

  for (f in files){
    df <- read_csv(folder %p% score_type %p% "/" %p% f,
                   skip = 9,
                   col_names = c("year", "geography",
                                 "demographic", "naep_" %p% score_type),
                   col_types = "icci")

    output <- assign_row_join(output, df)
  }
  output
}

naep_math <- read_naep(path, score_type = "math")
naep_reading <- read_naep(path, score_type = "reading")

naep_ky <- full_join(naep_math, naep_reading,
                  by = c("year", "geography", "demographic"))

naep_ky %<>%
  clean_ky_ed(naep_math:naep_reading, calc_nonfrl = FALSE) %>%
  select(year, demographic, variable = geography, naep_math, naep_reading) %>%
  mutate(variable = if_else(variable == "Kentucky", "mean", "lou")) %>%
  spread_ky_ed()

usethis::use_data(naep_ky, overwrite = TRUE)

rm(path, read_naep, naep_math, naep_reading, clean_ky_ed, process_ky_ed, spread_ky_ed, clean_55k)
