process_health_index <- function(mortality, natality, brfss){

  brfss %<>%
    left_join(MSA_FIPS_core_county, by = "MSA") %>%
    select(-MSA)

  health_index <- bind_df(mortality, natality, brfss) %>%
    filter(year >= 2003) %>%
    pull_peers(add_info = T) %>%
    filter(current == 1) %>%
    select(FIPS, year, sex, race, ypll, underweight_births, poor_or_fair, physdays, mentdays)

  z_scores <- health_index %>%
    filter(race == "total", sex == "total") %>%
    select(year, ypll:mentdays) %>%
    pivot_longer(ypll:mentdays, names_to = "variable", values_to = "value") %>%
    group_by(year, variable) %>%
    summarise_all(
      list(~mean(., na.rm = T), ~sd(., na.rm = T))) %>%
    ungroup()

  browser()

  health_z <- health_index %>%
    pivot_longer(ypll:mentdays, names_to = "variable", values_to = "value") %>%
    left_join(z_scores, by = c("year", "variable")) %>%
    mutate(
      value_z = (value - mean) / sd * -1,
      variable = variable %p% "_index") %>%
    select(-mean, -sd, -value) %>%
    pivot_wider(names_from = variable, values_from = value_z) %>%
    mutate(
      health_index =
        ypll_index * .5 +
        underweight_births_index * .2 +
        poor_or_fair_index * .1 +
        physdays_index * .1 +
        mentdays_index * .1)

  health_index %<>%
    bind_df(health_z) %>%
    select(-ypll, -underweight_births, -poor_or_fair, -physdays, -mentdays) %>%
    organize()
}
