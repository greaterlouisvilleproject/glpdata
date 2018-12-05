library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/educ/graduation/"

grad_read <- function(folder, grad_var) {

  wd <- getwd()
  directory <- paste0(wd, "/", folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- 2013

  for (i in 1:n){
    file_path <- paste0(wd, "/", folder, file_names[i])
    df <- read_csv(file_path, progress = FALSE)

    var_name <- c("COHORT_RATE", "COHORT_2014", "COHORT_2015",
                   "REPORTYEAR_2016", "REPORTYEAR_2017")[i]

    df$var <- df[[var_name]]

    df %<>%
      filter(SCH_NAME %in% c("--District Total--", "---District Total---"))

    if(y > 2013){
      df %<>%
        filter(
          COHORT_TYPE == "FOUR YEAR" &
          TARGET_LABEL %in% c("Numerator Count", "Denominator Count")) %>%
        select(
          district = DIST_NAME,
          category = DISAGG_LABEL,
          TARGET_LABEL,
          var) %>%
        mutate(var = str_remove(var, ",")) %>%
        spread(TARGET_LABEL, var) %>%
        rename(
          grads = `Numerator Count`,
          total = `Denominator Count`)
    } else {
      df %<>%
        select(
          district = DIST_NAME,
          category = DISAGG_LABEL,
          grads = COHORT_NUMERATOR,
          total = COHORT_DENOMINATOR)
    }

    df %<>%
      filter(
        category %in% c("All Students", "Male", "Female", "White (Non-Hispanic)",
                            "African American", "Hispanic", "Asian",
                            "Free/Reduced-Price Meals")) %>%
      mutate(
        grads = as.numeric(grads),
        total = as.numeric(total),
        year = y) %>%
      select(district, year, category, total, grads)

    y <- y + 1

    if(i == 1){
      output <- df
    }
    else{
      names(df) <- names(output)
      output %<>% rbind(df)
    }
  }
  output
}


graduation <- grad_read(path)

graduation %<>%
  mutate(
    category = replace(category, category == "African American", "black"),
    category = replace(category, category == "All Students", "all"),
    category = replace(category, category == "Asian", "asian"),
    category = replace(category, category == "Female", "female"),
    category = replace(category, category == "Free/Reduced-Price Meals", "frl"),
    category = replace(category, category == "Hispanic", "hispanic"),
    category = replace(category, category == "Male", "male"),
    category = replace(category, category == "White (Non-Hispanic)", "white"))

graduation_frl <- graduation %>%
  filter(category == "frl") %>%
  rename(
    total_frl = total,
    grads_frl = grads) %>%
  select(-category)

graduation_total <- graduation %>%
  filter(category == "all") %>%
  rename(
    total_all = total,
    grads_all = grads) %>%
  select(-category)

graduation_non_frl <- full_join(graduation_frl, graduation_total,
                                by = c("district", "year"))

graduation_non_frl %<>%
  transmute(
    district, year,
    total = total_all - total_frl,
    grads = grads_all - grads_frl,
    category = "nonfrl")

graduation %<>% bind_rows(graduation_non_frl) %>%
  mutate(grad_rate = grads / total * 100) %>%
  select(-total, -grads) %>%
  arrange(year, district, category)

usethis::use_data(graduation, overwrite = TRUE)

rm(graduation, graduation_frl, graduation_non_frl, graduation_total,
   path, grad_read)
