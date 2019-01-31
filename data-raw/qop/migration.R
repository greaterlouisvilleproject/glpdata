library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/qop/migration/"

#Age

in_mig <- acs_time(path %+% "B07001", starting_year = 2006)
out_mig <- acs_time(path %+% "B07401", starting_year = 2007)

in_mig %<>%
  mutate(
    total =
      `Estimate; Total:`,

    total_young =
      `Estimate; 20 to 24 years` +
      `Estimate; 30 to 34 years`,

    in_mig =
      `Estimate; Moved from different county within same state:` +
      `Estimate; Moved from different state:` +
      `Estimate; Moved from abroad:`,

    in_mig_young =
      `Estimate; Moved from different county within same state: - 25 to 29 years` +
      `Estimate; Moved from different county within same state: - 30 to 34 years` +
      `Estimate; Moved from different state: - 25 to 29 years` +
      `Estimate; Moved from different state: - 30 to 34 years` +
      `Estimate; Moved from abroad: - 25 to 29 years` +
      `Estimate; Moved from abroad: - 30 to 34 years`)

out_mig %<>%
  mutate(
    out_mig =
      `Estimate; Moved to different county within same state:` +
      `Estimate; Moved to different state:`,

    out_mig_young =
      `Estimate; Moved to different county within same state: - 25 to 29 years` +
      `Estimate; Moved to different county within same state: - 30 to 34 years` +
      `Estimate; Moved to different state: - 25 to 29 years` +
      `Estimate; Moved to different state: - 30 to 34 years`)

mig_age <- bind_df(in_mig, out_mig)

mig_age %<>%
  mutate(
    net_mig = in_mig - out_mig,
    net_mig_young = in_mig_young - out_mig_young)

mig_total <- mig_age %>%
  stl_merge(total, in_mig, out_mig, net_mig, method = "sum") %>%
  group_by(year) %>%
  mutate(
    lou_pop = total[FIPS == "21111"],
    scale_lou = lou_pop / total,
    in_mig_adj = in_mig * scale_lou,
    out_mig_adj = out_mig * scale_lou,
    net_mig_adj = net_mig * scale_lou) %>%
  ungroup() %>%
  select(-total, -lou_pop, -scale_lou) %>%
  organize()

mig_age %<>%
  stl_merge(total_young, in_mig_young, out_mig_young, net_mig_young, method = "sum") %>%
  select(-total_young) %>%
  organize()


#Education
in_mig <- acs_time(path %+% "B07009")
out_mig <- acs_time(path %+% "B07409", starting_year = 2007)

in_mig %<>%
  mutate(
    in_mig_bach =
      `Estimate; Moved from different county within same state: - Bachelor's degree` +
      `Estimate; Moved from different state: - Bachelor's degree` +
      `Estimate; Moved from abroad: - Bachelor's degree`,

    in_mig_grad =
      `Estimate; Moved from different county within same state: - Graduate or professional degree` +
      `Estimate; Moved from different state: - Graduate or professional degree` +
      `Estimate; Moved from abroad: - Graduate or professional degree`,

    in_mig_bach_plus =
      in_mig_bach + in_mig_grad)

out_mig %<>%
  mutate(
    out_mig_bach =
      `Estimate; Moved to different county within same state: - Bachelor's degree` +
      `Estimate; Moved to different state: - Bachelor's degree`,

    out_mig_grad =
      `Estimate; Moved to different county within same state: - Graduate or professional degree` +
      `Estimate; Moved to different state: - Graduate or professional degree`,

    out_mig_bach_plus =
      out_mig_bach + out_mig_grad)

mig_edu <- bind_df(in_mig, out_mig)

mig_edu %<>%
  select(FIPS, year, contains("mig")) %>%
  stl_merge(in_mig_bach:out_mig_bach_plus, method = "sum") %>%
  mutate(
    net_mig_bach = in_mig_bach - out_mig_bach,
    net_mig_grad = in_mig_grad - out_mig_grad,
    net_mig_bach_plus = in_mig_bach_plus - out_mig_bach_plus)

mig <- bind_df(mig_total, mig_age, mig_edu)

usethis::use_data(mig, overwrite = TRUE)

rm(in_mig, out_mig, mig, mig_age, mig_edu, mig_total, path)

