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

  for(v in 1:length(variables)){

    df <- df_original[,c("district", "year", "demographic", variables[v], pop_var)]

    df$var <- df[[variables[v]]]
    df$pop <- df[[pop_var]]

    df %<>%
      mutate(
        var = as.numeric(var),
        pop = as.numeric(pop),
        demographic = recode(demographic, !!!demog_labels, .default = "other")) %>%
      filter(demographic != "other")

    # Non-frl calculation:
    # frl_students * frl_score + non_frl_students * non_frl_score = total_students * total_score
    # non_frl_score * non_frl_students = total_students * total_score - frl_students * frl_score
    # non_frl_score = (total_students * total_score - frl_students * frl_score) / non_frl_students

    df_frl <- df %>%
      filter(demographic == "frl") %>%
      select(-demographic) %>%
      rename(
        var_frl = var,
        pop_frl = pop)

    df_all <- df %>%
      filter(demographic == "total") %>%
      select(-demographic) %>%
      rename(
        var_all = var,
        pop_all = pop)

    df_nonfrl <- full_join(df_all, df_frl, by = c("district", "year"))

    df_nonfrl %<>%
      mutate(
        pop_nonfrl = pop_all - pop_frl,
        var =
          (pop_all * var_all -
             pop_frl * var_frl) / pop_nonfrl,
        demographic = "nonfrl") %>%
      select(district, year, demographic, var)

    df %<>%
      bind_rows(df_nonfrl) %>%
      mutate(var = round(var, 1))

    df[[variables[v]]] <- df$var

    df <- df[,c("district", "year", "demographic", variables[v])]

    df %<>%
      arrange(year, district, demographic)


    if(v == 1){
      output <- df
    } else {
      output %<>% full_join(df, by = c("district", "year", "demographic"))
    }
    v <- v + 1
  }
  output
}
