clean_ky_ed <- function(df_original, ..., pop_var = "num_students", calc_nonfrl = TRUE){

  variables <- dplyr:::tbl_at_vars(df_original, vars(...))
  pop_var <- as.character(substitute(pop_var))

  # KSCREEN 2013-2018
  dem_lab_1 <- c("African American" = "black", "All Students" = "total",
                 "Asian" = "asian", "Female" = "female",
                 "Free Reduced Price Meals" = "frl", "Hispanic" = "hispanic",
                 "Male" = "male", "White (Non-Hispanic)" = "white")

  # ACT 2012-2017
  dem_lab_2 <- c("Free/Reduced-Price Meals" = "frl")

  # ACT 2018
  dem_lab_3 <- c("ETB" = "black", "TST" = "total",
                 "ETA" = "asian", "SXF" = "female",
                 "LUP" = "frl", "ETH" = "hispanic",
                 "SXM" = "male", "ETW" = "white")

  # NAEP
  dem_lab_4 <- c("Black" = "black", "All students" = "total",
                 "Asian/Pacific Islander" = "asian", "White" = "white",
                 "Eligible" = "frl", "Not eligible" = "nonfrl")

  demog_labels = c(dem_lab_1, dem_lab_2, dem_lab_3, dem_lab_4)

  if(!calc_nonfrl){
    output <- df_original %>%
      mutate(
        demographic = recode(demographic, !!!demog_labels, .default = "other")) %>%
      filter(demographic != "other")

    return(output)
  }

  df_original %<>%
    mutate(demographic = recode(demographic, !!!demog_labels, .default = "other")) %>%
    filter(demographic != "other")

  for(var in variables){

    df <- df_original %>% select(district, year, demographic, !!var, !!pop_var)

    # Non-frl calculation:
    # frl_students * frl_score + non_frl_students * non_frl_score = total_students * total_score
    # non_frl_score * non_frl_students = total_students * total_score - frl_students * frl_score
    # non_frl_score = (total_students * total_score - frl_students * frl_score) / non_frl_students

    df_frl <- df %>%
      filter(demographic == "frl") %>%
      select(-demographic) %>%
      rename(
        var_frl = !!var,
        pop_frl = !!pop_var)

    df_all <- df %>%
      filter(demographic == "total") %>%
      select(-demographic) %>%
      rename(
        var_all = !!var,
        pop_all = !!pop_var)

    df_nonfrl <- full_join(df_all, df_frl, by = c("district", "year"))

    df_nonfrl %<>%
      mutate(
        pop_nonfrl = pop_all - pop_frl,
        var_nonfrl = (pop_all * var_all -
                      pop_frl * var_frl) / pop_nonfrl,
        demographic = "nonfrl") %>%
      select(district, year, demographic, !!var := var_nonfrl, !!pop_var := pop_nonfrl)

    df %<>%
      bind_rows(df_nonfrl) %>%
      mutate(!!var := round(!!sym(var), 1))

    df %<>%
      select(district, year, demographic, !!pop_var, !!var) %>%
      arrange(year, district, demographic)

    output <- assign_col_join(output, df, by = c("district", "year", "demographic", pop_var))
  }
  output
}

process_ky_ed <- function(df_original, ..., pop_var = "num_students", calc_nonfrl = TRUE){

  variables <- dplyr:::tbl_at_vars(df_original, vars(...))
  pop_var <- as.character(substitute(pop_var))

  for(var in variables){
    df <- df_original %>% select("district", "year", "demographic", !!var, !!pop_var)

    #df$var <- df[[v]]
    #df$pop <- df[[pop_var]]

    # Mean
    #   jc_students * jc_score + non_jc_students * non_jc_score = total_students * total_score
    #   non_jc_score * non_jc_students = total_students * total_score - jc_students * jc_score
    #   non_jc_score = (total_students * total_score - jc_students * jc_score) / non_jc_students

    df_jc <- df %>%
      filter(str_detect(district, "Jefferson")) %>%
      rename(
        var_jc = !!var,
        pop_jc = !!pop_var)

    df_state <- df %>%
      filter(str_detect(district, "State")) %>%
      rename(
        var_state = !!var,
        pop_state = !!pop_var)

    df_nonjc <- full_join(df_state, df_jc, by = c("year", "demographic"))

    df_nonjc %<>%
      mutate(
        pop_nonjc = pop_state - pop_jc,
        mean = (pop_state * var_state -
                pop_jc * var_jc) / pop_nonjc) %>%
      select(year, demographic, mean)

    # Q1 and Q3
    #   Do not process for race, as most non-white population data is missing

    pctiles <- df %>%
      filter(
        str_detect(district, "Jefferson|State", negate = T),
        demographic %not_in% c("white", "black", "hispanic", "asian")) %>%
      group_by(year, demographic) %>%
      summarise(
        q1 = Hmisc::wtd.quantile(!!sym(var), !!sym(pop_var), probs = 0.25, na.rm = T),
        q3 = Hmisc::wtd.quantile(!!sym(var), !!sym(pop_var), probs = 0.75, na.rm = T)) %>%
      ungroup()

    # Louisville
    df_jc %<>% select(year, demographic, lou = var_jc)

    df <- full_join(df_jc, df_nonjc, by = c("year", "demographic"))
    df %<>% full_join(pctiles, by = c("year", "demographic"))

    df %<>%
      gather(lou, q1, mean, q3, key = "variable", value = !!var) %>%
      select(year, demographic, variable, !!var)

    output <- assign_col_join(output, df)
  }

  output
}

spread_ky_ed <- function(df) {
  df_total <- df %>%
    filter(demographic == "total") %>%
    mutate(
      sex = "total",
      race = "total",
      frl_status = "total")

  df_sex <- df %>%
    filter(demographic %in% c("male", "female")) %>%
    mutate(
      sex = demographic,
      race = "total",
      frl_status = "total")

  df_race <- df %>%
    filter(demographic %in% c("white", "black", "hispanic", "asian")) %>%
    mutate(
      sex = "total",
      race = demographic,
      frl_status = "total")

  df_frl <- df %>%
    filter(demographic %in% c("frl", "nonfrl")) %>%
    mutate(
      sex = "total",
      race = "total",
      frl_status = demographic)

  df <- bind_rows(df_total, df_sex, df_race, df_frl)

  df %<>%
    select(-demographic) %>%
    organize()
}

clean_55k <- function(df) {
  recode <- c("Total" = "All Students", "White" = "White (Non-Hispanic)",
              "Black" = "African American", "Frl" = "Free/Reduced-Price Meals")

  df %>%
    mutate(
      demographic =
        str_to_title(demographic) %>%
        recode(!!!recode),

      school =
        str_replace_all(school, "-", "") %>%
        trimws() %>%
        str_replace("Dupont", "duPont") %>%
        str_replace(" MCA", "") %>%
        str_replace(" Traditional High", " High School") %>%
        str_replace(" School", "") %>%
        str_replace(" J Graham", "") %>%
        str_replace(" Magnet Career Academy", "") %>%
        str_replace("Louisville ", "") %>%
        str_replace("Marion C Moore", "Moore Traditional"),

      demographic = replace(demographic, demographic == "total", "")) %>%
    filter(
      school %in% c("Atherton High",
                    "Ballard High",
                    "Brown",
                    "Butler High",
                    "Central High",
                    "District Total",
                    "Doss High",
                    "duPont Manual High",
                    "Eastern High",
                    "Fairdale High",
                    "Fern Creek High",
                    "Iroquois High",
                    "Jeffersontown High",
                    "Male High",
                    "Moore Traditional",
                    "Pleasure Ridge Park High",
                    "Seneca High",
                    "Southern High",
                    "State Total",
                    "The Academy @ Shawnee",
                    "Valley High",
                    "Waggener High",
                    "Western High")) %>%
    select(-district)
}


