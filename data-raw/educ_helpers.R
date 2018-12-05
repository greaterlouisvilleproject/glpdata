reshape_ky_ed <- function(df, var_name) {
  df %<>%
    mutate(
      category = paste0(var_name, "_", category),
      category = replace(category, category == var_name %+% "_all", var_name)) %>%
    spread(key = category, value = !!var_name) %>%
    select(
      district, year, !!var_name,
      ends_with("_white"), ends_with("_black"), ends_with("_hispanic"), ends_with("_asian"),
      ends_with("_male"), ends_with("_female"), ends_with("_frl"), ends_with("nonfrl"))
  
  df
}