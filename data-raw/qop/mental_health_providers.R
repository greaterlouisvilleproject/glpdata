library(glptools)
glp_load_packages()
library(feather)
library(lubridate)

path <- "data-raw/health/mental_health_providers/"

# READ DATA
# Data from http://data.nber.org/data/nppes/
if ("provider_subset.feather" %not_in% list.files(path)) {
  # Data is in a very long format saved as a Stata (dta) file
  # Read in by chunks using haven::read_dta

  # Other function to read dta. Not used because it is slower.
  # library(readstata13)
  # test <- readstata13::read.dta13(path %p% "npicore dta/npicore.dta", select.rows	= 1)
  remove_attr <-  function(x) {
    attr(x, "format.stata") <- NULL
    return(x)
  }

  for(i in seq(0, 15000000, by = 1000000)) {
    print(i)

    temp <- haven::read_dta(path %p% "npicore dta/npicore.dta",
                            skip = i + 1,
                            n_max	= i + 1000000)

    temp %<>%
      map_dfc(remove_attr) %>%
      mutate_all( ~ replace(., . ==  "", values = NA_character_)) %>%
      mutate(zip = str_sub(ploczip, 1, 5)) %>%
      filter(zip %in% FIPS_zip$zip)

    provider_subset <- assign_row_join(provider_subset, temp)
  }

  write_feather(provider_subset, path %p% "provider_subset.feather")
} else {
  provider_subset <- read_feather(path %p% "provider_subset.feather")
}

# PROCESS DATA

# Filter provider taxonomies to any containing mental health-related codes
mental_health_codes <- c("2084P0800X", # Psychiatry Physician)
                         "103T.....X", # Psychologist
                         "1041.....X", # Social Workers
                         "101Y.....X", # Counselors
                         "106H00000X", # Marriage & Family Therapist,
                         "364SP08..X") # Nurses

mental_health_codes <- c("Psychiatrist"  = "2084P0800X", # Psychiatry Physician)
                         "Psycologist"   = "103T.....X", # Psychologist
                         "Social Worker" = "1041.....X", # Social Workers
                         "Counselor"     = "101Y.....X", # Counselors
                         "Marriage and Family Therapist" = "106H00000X", # Marriage & Family Therapist,
                         "Nurse"         = "364SP08..X") # Nurses

taxonomy_codes  <- read_csv(path %p% "ptaxcode.csv", col_types = "cccc")

# To speed up processing, taxonomies to providers and providers to taxonomies
taxonomy_codes %<>%
  filter(str_detect(ptaxcode, paste0(mental_health_codes, collapse = "|"))) %>%
  semi_join(provider_subset, by = "npi") %>%
  select(-seq) %>%
  mutate(
    Psychiatrist    = str_detect(ptaxcode, mental_health_codes[1]),
    Psycologist     = str_detect(ptaxcode, mental_health_codes[2]),
    `Social Worker` = str_detect(ptaxcode, mental_health_codes[3]),
    Counselor       = str_detect(ptaxcode, mental_health_codes[4]),
    `Marriage and Family Therapist` = str_detect(ptaxcode, mental_health_codes[5]),
    Nurse           = str_detect(ptaxcode, mental_health_codes[6]))

provider_subset %<>%
  semi_join(taxonomy_codes, by = "npi")

# Create all combinations of providers and years
all_combinations <- provider_subset %>%
  select(npi) %>%
  distinct()

# Create start dates
provider_start_date <- provider_subset %>%
  mutate(start_date = mdy(penumdatestr), .groups = "drop") %>%
  group_by(npi) %>%
  summarise(start_date = min(start_date), .groups = "drop")

# Create deactivation dates
deactivations <- readxl::read_xlsx(path %p% "NPPES Deactivated NPI Report 20201215.xlsx", skip = 1)

deactivations %<>%
  transmute(
    npi = NPI,
    deactivation_date = mdy(`NPPES Deactivation Date`))

interim_deactivations <- provider_subset %>%
  filter(!is.na(npideactdate)) %>%
  mutate(interim_deactivation = mdy(npideactdate),
         interim_reactivation = mdy(npireactdate)) %>%
  select(npi, interim_deactivation, interim_reactivation) %>%
  distinct(npi, .keep_all = T)

all_combinations %<>%
  left_join(provider_start_date, by = "npi") %>%
  left_join(interim_deactivations, by = "npi") %>%
  left_join(deactivations, by = "npi")

# Check to see if a provider was open for most of the year
# Uses enumeration date and deactivation date
check_year_fxn <- function(this_year, df) {

  first_day <- dmy("1/1/" %p% this_year)
  last_day <- dmy("1/1/" %p% (this_year + 1))

  df$seconds <- duration(1, "year")

  df %<>%
    mutate(

      # subtract start date
      seconds = if_else(start_date > first_day,
                        seconds - (start_date - first_day),
                        seconds),

      # subtract interim deactivation
      deact_max = if_else(interim_deactivation < first_day, first_day, interim_deactivation),
      react_min = if_else(interim_reactivation > last_day, last_day, interim_reactivation),

      seconds =
        if_else(!is.na(interim_deactivation),
                if_else(this_year >= year(interim_deactivation) &
                          this_year <= year(interim_reactivation),
                        seconds - (react_min - deact_max),
                        #seconds - 10000,
                        seconds),
                seconds),

  # subtract deactivation
      seconds =
        if_else(!is.na(deactivation_date),
                if_else(deactivation_date < last_day,
                        seconds - (last_day - deactivation_date),
                        seconds),
                seconds))

  df$seconds / duration(1, "year") * 100
}

temp <- map_dfc(2007:2020, ~check_year_fxn(., all_combinations))
names(temp) <- as.character(2007:2020)

open_providers <- bind_cols(all_combinations, temp)

open_providers %<>%
  mutate(across(as.character(2007:2020), ~ if_else(. >= 50, T, F))) %>%
  pivot_longer(as.character(2007:2020), names_to = "year") %>%
  filter(value) %>%
  select(npi, year)

provider_data <- provider_subset %>%
  group_by(npi, year) %>%
  summarise(
    zip = last(zip),
    entity = last(entity),
    .groups = "drop")

open_providers %<>%
  left_join(provider_data, by = c("npi", "year")) %>%
  group_by(npi) %>%
  fill(zip, entity, .direction = "downup") %>%
  ungroup() %>%
  left_join(FIPS_zip, by = "zip")

# GEOCODE DATA
if (TRUE) {

} else {

}

# mental_health_providers_subset <- mental_health_providers %>%
#   select(FIPS, zip, NPI,
#          type = `Entity Type Code`,
#          name = `Provider Organization Name (Legal Business Name)`,
#          any_of(paste0("Healthcare Provider Taxonomy Code_", 1:15))) %>%
#   select(FIPS, zip, NPI, type, name,
#          taxonomy_code = `Healthcare Provider Taxonomy Code_1`)



mental_health_providers_subset <- mental_health_providers %>%
  filter(str_detect(`Healthcare Provider Taxonomy Code_1`, paste(mental_health_codes, collapse = "|")),
         FIPS == "21111",
         is.na(`Provider Organization Name (Legal Business Name)`),
         `Provider Business Practice Location Address City Name` == "LOUISVILLE",
         NPI %not_in% as.numeric(deactivation$NPI))

c(556,  1415, 1798, 1952, 2035, 2118, 2204, 2345)
c(2010, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
