library(glptools)
glp_load_packages()

# Subtract non-injury deaths from total deaths

path <- "data-raw/health/injury_deaths/"

# Read and clean data
all_race        <- read_tsv(path %p% "total_deaths_race.txt")
all_sex         <- read_tsv(path %p% "total_deaths_sex.txt")
all             <- read_tsv(path %p% "total_deaths.txt")
non_injury_race <- read_tsv(path %p% "non_injury_deaths_race.txt")
non_injury_sex  <- read_tsv(path %p% "non_injury_deaths_sex.txt")
non_injury      <- read_tsv(path %p% "non_injury_deaths.txt")

all_race        %<>% clean_wonder(method = "weight")
all_sex         %<>% clean_wonder(method = "weight")
all             %<>% clean_wonder(method = "weight")
non_injury_race %<>% clean_wonder(method = "weight")
non_injury_sex  %<>% clean_wonder(method = "weight")
non_injury      %<>% clean_wonder(method = "weight")

# Calculate the injury death rate
non_injury_race %<>% rename(non_injury_rate = rate)
non_injury_sex  %<>% rename(non_injury_rate = rate)
non_injury      %<>% rename(non_injury_rate = rate)

injury_deaths_race <- bind_df(all_race, non_injury_race)
injury_deaths_sex  <- bind_df(all_sex,  non_injury_sex)
injury_deaths_all  <- bind_df(all,      non_injury)

injury_deaths_county <- bind_rows(injury_deaths_race, injury_deaths_sex, injury_deaths_all)

injury_deaths_county %<>%
  transmute(
    FIPS, year, sex, race,
    var_type = "estimate",
    injury_deaths = rate - non_injury_rate)

usethis::use_data(injury_deaths_county, overwrite = TRUE)

rm(all_race, all_sex, all, non_injury_race, non_injury_sex, non_injury,
   injury_deaths_race, injury_deaths_sex, injury_deaths_all,
   path)
