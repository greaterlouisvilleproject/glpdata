library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/jobs/SBA/"

sba1 <- readxl::read_excel(path %p% "FOIA - 7(a)(FY2010-Present).xlsx")
sba2 <- readxl::read_excel(path %p% "FOIA - 504 (FY1991-Present).xlsx")

FIPS_join <- FIPS_info %>%
  mutate(
    state = state_abbr,
    county = str_to_upper(county) %>%
      str_replace("ST.", "SAINT"))

sba <- sba1 %>%
  transmute(
    zip = BorrZip,
    year = str_sub(ApprovalDate, 1, 4) %>% as.numeric(),
    sba_loan = SBAGuaranteedApproval,
    total_loan = GrossApproval,
    county = ProjectCounty,
    state = ProjectState) %>%
  left_join(FIPS_join, by = c("state", "county")) %>%
  pull_peers(add_info = F) %>%
  select(FIPS, zip, year, sba_loan, total_loan)

sba_county <- sba %>%
  group_by(FIPS, year) %>%
  summarise(
    sba_loan = sum(sba_loan),
    total_loan = sum(total_loan)) %>%
  ungroup() %>%
  COLA(sba_loan:total_loan, base_year = 2019, rpp = F) %>%
  per_capita_adj(sba_loan:total_loan, geog = "FIPS")

sba_zip <- sba %>%
  filter(FIPS == "21111") %>%
  group_by(FIPS, zip, year) %>%
  summarise(
    sba_loan = sum(sba_loan),
    total_loan = sum(total_loan)) %>%
  ungroup() %>%
  COLA(sba_loan:total_loan, base_year = 2019, rpp = F) %>%
  select(-FIPS) %>%
  left_join(FIPS_zip, by = "zip") %>%
  mutate(
    sba_loan_pp = sba_loan / population_total,
    total_loan_pp = total_loan / population_total) %>%
  select(-population_total, -population_in_FIPS, -FIPS)


sba <- sba2 %>%
  transmute(
    zip = BorrZip,
    year = str_sub(ApprovalDate, 1, 4) %>% as.numeric(),
    total_loan2 = GrossApproval,
    county = ProjectCounty,
    state = ProjectState) %>%
  left_join(FIPS_join, by = c("state", "county")) %>%
  pull_peers(add_info = F) %>%
  select(FIPS, zip, year, total_loan2)

sba_county2 <- sba %>%
  group_by(FIPS, year) %>%
  summarise(
    total_loan2 = sum(total_loan2)) %>%
  ungroup() %>%
  COLA(total_loan2, base_year = 2019, rpp = F) %>%
  per_capita_adj(total_loan2, geog = "FIPS")

sba_zip2 <- sba %>%
  filter(FIPS == "21111") %>%
  group_by(FIPS, zip, year) %>%
  summarise(
    total_loan2 = sum(total_loan2)) %>%
  ungroup() %>%
  COLA(total_loan2, base_year = 2019, rpp = F) %>%
  select(-FIPS) %>%
  left_join(FIPS_zip, by = "zip") %>%
  mutate(
    total_loan2_pp = total_loan2 / population_total) %>%
  select(-population_total, -population_in_FIPS, -FIPS)

sba_county %<>% left_join(sba_county2, by = c("FIPS", "year"))
sba_zip %<>% left_join(sba_zip2, by = c("zip", "year"))

update_sysdata(sba_county, sba_zip)



# per_capita_adj <- function(df, ..., geog, keep_vars = T, keep_pop = F) {
#
#   # Create list of variables from ... argument
#   variables <- dplyr:::tbl_at_vars(df, vars(...))
#
#   # Determine geography and other variables to join by
#   if(missing(geog)) {
#     geog <- df_type(df)
#   }
#
#   if(length(geog) > 1) {
#     stop("Too many geography columns. Provide geog argument.")
#   }
#
#   join_vars <- c(geog, df %cols_in% c("year", "sex", "race"))
#
#   # Create a clean, minimal population data frame
#   tryCatch({
#     pop_df <- switch(geog,
#                      "MSA"   = glpdata:::population_msa_1yr,
#                      "FIPS"  = glpdata:::population_county,
#                      "tract" = glpdata:::population_tract,
#                      "neighborhood"    = glpdata:::population_nh,
#                      "muw"   = glpdata:::population_muw)
#   },
#   error = function(e){
#     stop("Geography not MSA, FIPS, or tract")
#   })
#
#   if("year" %not_in% join_vars) pop_df %<>% filter(year == 2018)
#   if("sex"  %not_in% join_vars & geog %in% c("FIPS", "MSA")) pop_df %<>% filter(sex == "total")
#   if("race" %not_in% join_vars & geog %in% c("FIPS", "MSA")) pop_df %<>% filter(race == "total")
#
#   pop_df %<>% select_at(c(join_vars, "population"))
#
#   # Join df to population df and divide by population.
#   # If keep_vars == TRUE, retain original variables.
#   if (keep_vars) {
#     new_df <- df %>%
#       left_join(pop_df, by = join_vars) %>%
#       mutate_at(variables, ~ . / population) %>%
#       rename_at(variables, ~ paste0(., "_pp")) %>%
#       select_at(c(join_vars, paste0(variables, "_pp"), "population"))
#
#     df %<>% bind_df(new_df)
#   } else {
#     df %<>%
#       left_join(pop_df, by = join_vars) %>%
#       mutate_at(variables, ~ . / population)
#   }
#
#   # If keep_pop == FALSE, remove population variable
#   if (!keep_pop) df %<>% select(-population)
#
#   df
# }
