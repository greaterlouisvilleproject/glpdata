library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/educ/act/"

act_read <- function(folder) {
  initial_wd <- getwd()
  directory <- paste(initial_wd, folder, sep = "/")
  file_names <- list.files(directory)
  n <- length(file_names)
  for (i in 1:n) {
    data <-read_csv(
      paste0(directory, file_names[i]),
      progress = FALSE,
      col_types = cols(STDNT_TESTED_CNT = col_double(),
                       ENGLISH_MEAN_SCORE = col_double(),
                       MATHEMATICS_MEAN_SCORE = col_double(),
                       READING_MEAN_SCORE = col_double(),
                       SCIENCE_MEAN_SCORE = col_double(),
                       COMPOSITE_MEAN_SCORE = col_double()))

    data %<>%
      filter(SCH_NAME %in% c("---District Total---", "--- District Total ---")) %>%
      select(district = DIST_NAME,
             year = SCH_YEAR,
             category = DISAGG_LABEL,
             num_students = STDNT_TESTED_CNT,
             english_score = ENGLISH_MEAN_SCORE,
             math_score = MATHEMATICS_MEAN_SCORE,
             reading_score = READING_MEAN_SCORE,
             science_score = SCIENCE_MEAN_SCORE,
             composite_score = COMPOSITE_MEAN_SCORE) %>%
      filter(category %in% c("All Students", "Male", "Female", "White (Non-Hispanic)",
                             "African American", "Hispanic", "Asian",
                             "Free/Reduced-Price Meals")) %>%
      mutate(year = as.numeric(substr(year, 5, 8)))

    if (i == 1) {
      df <- data
    }
    else{

      df <- bind_rows(df, data)
    }
  }
  df
}

act <- act_read(path)

act %<>%
  mutate(
    category = replace(category, category == "African American", "black"),
    category = replace(category, category == "All Students", "all"),
    category = replace(category, category == "Asian", "asian"),
    category = replace(category, category == "Female", "female"),
    category = replace(category, category == "Free/Reduced-Price Meals", "frl"),
    category = replace(category, category == "Hispanic", "hispanic"),
    category = replace(category, category == "Male", "male"),
    category = replace(category, category == "White (Non-Hispanic)", "white"))

#Non-frl calculation:
# frl_students * frl_score + non_frl_students * non_frl_score = total_students * total_score
# non_frl_score = (total_students * total_score + frl_students * frl_score) / non_frl_students
# non_frl_score = (total_students * total_score /  non_frl_students) + (frl_students * frl_score / non_frl_students)

#Find the number of non-frl students and add it as a column
act %<>%
  group_by(year, district) %>%
  filter(category %in% c("all", "frl")) %>%
  arrange(desc(category)) %>%
  select(-category) %>%
  summarise_all(diff) %>%
  mutate(category = "nonfrl_temp") %>%
  bind_rows(act, .) %>%
  group_by(year, district) %>%
  mutate(nonfrl_students = last(num_students)) %>%
  ungroup() %>%
  filter(category != "nonfrl_temp")

#Perform the calculation above and add the results to the data frame
act %<>%
  group_by(year, district) %>%
  filter(category %in% c("all", "frl")) %>%
  summarise_at(vars(english_score:composite_score), funs(diff(. * num_students / nonfrl_students))) %>%
  ungroup() %>%
  mutate(category = "nonfrl") %>%
  bind_rows(act, .) %>%
  select(-num_students, -nonfrl_students)

act %<>%
  arrange(year, district, category)

use_data(act, overwrite = TRUE)

rm(act, act_read, path)
