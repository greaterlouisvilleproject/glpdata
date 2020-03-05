library(stringr)
library(dplyr)
library(readr)
library(magrittr)
library(glptools)

load("R/sysdata.rda")

drivers <- c("education", "jobs","health", "qop")

refresh_data <- F

for(d in drivers){

  # List the scripts in the driver directory
  script_dir <- paste(getwd(), "data-raw", d, sep = "/")
  scripts <- list.files(script_dir)
  scripts <- scripts[grepl(".R", scripts)]

  for(s in scripts){
    df_name <- str_remove(s, ".R")
    print(df_name)

    if (refresh_data) source(paste0(script_dir, "/", s))

    # Check to see is map file exists for this data frame
    county_exists  <- exists(df_name %p% "_county")
    tract_exists   <- exists(df_name %p% "_tract")
    nh_exists      <- exists(df_name %p% "_nh")
    muw_exists     <- exists(df_name %p% "_muw")
    zip_exists     <- exists(df_name %p% "_zip")
    msa_1yr_exists <- exists(df_name %p% "_msa_1yr")
    msa_5yr_exists <- exists(df_name %p% "_msa_5yr")
    ky_exists      <- exists(df_name %p% "_ky")

    # Assign output files to df and df_map -- SHOULD JUST LOAD SYSDATA
    if (county_exists)  df_county  <- eval(as.name(df_name %p% "_county"))
    if (msa_1yr_exists) df_msa_1yr <- eval(as.name(df_name %p% "_msa_1yr")) %>% mutate(MSA = as.character(MSA))
    if (msa_5yr_exists) df_msa_5yr <- eval(as.name(df_name %p% "_msa_5yr")) %>% mutate(MSA = as.character(MSA))
    if (tract_exists)   df_tract   <- eval(as.name(df_name %p% "_tract"))
    if (nh_exists)      df_nh      <- eval(as.name(df_name %p% "_nh"))
    if (muw_exists)     df_muw     <- eval(as.name(df_name %p% "_muw"))
    if (zip_exists)     df_zip     <- eval(as.name(df_name %p% "_zip"))
    if (ky_exists)      df_ky      <- eval(as.name(df_name %p% "_ky"))

    if (tract_exists) if("Id" %in% names(df_tract)) df_tract %<>% rename(tract = Id) #correct ASAP

    # Create or bind the data frame to the output data frame and maps
    if (county_exists)  output_df_county  <- assign_col_join(output_df_county,  df_county)
    if (msa_1yr_exists) output_df_msa_1yr <- assign_col_join(output_df_msa_1yr, df_msa_1yr)
    if (msa_5yr_exists) output_df_msa_5yr <- assign_col_join(output_df_msa_5yr, df_msa_5yr)
    if (ky_exists)      output_df_ky      <- assign_col_join(output_df_ky,      df_ky)
    if (tract_exists)   output_df_tract   <- assign_col_join(output_df_tract,   df_tract, by = df_tract %cols_in% c("tract", "year"))
    if (nh_exists)      output_df_nh      <- assign_col_join(output_df_nh,      df_nh,    by = df_nh %cols_in% c("neighborhood", "year"))
    if (muw_exists)     output_df_muw     <- assign_col_join(output_df_muw,     df_muw,   by = df_muw %cols_in% c("neighborhood", "year"))
    if (zip_exists)     output_df_zip     <- assign_col_join(output_df_zip,     df_zip,   by = df_zip %cols_in% c("zip", "year"))
  }

  assign(d %p% "_county", output_df_county)
  rm(output_df_county)

  assign(d %p% "_msa_1yr", output_df_msa_1yr)
  rm(output_df_msa_1yr)

  # EDIT OUT AFTER ADDING MSA DATA FOR ALL DRIVERS

  if(exists("output_df_msa_5yr")) {
    assign(d %p% "_msa_5yr", output_df_msa_5yr)
    rm(output_df_msa_5yr)
  }
  if(exists("output_df_ky")) {
    assign(d %p% "_ky", output_df_ky)
    rm(output_df_ky)
  }

  assign(d %p% "_tract", output_df_tract)
  final_tract <- assign_col_join(final_tract, output_df_tract, by = c("tract", "year"))
  rm(output_df_tract)

  assign(d %p% "_nh", output_df_nh)
  final_nh <- assign_col_join(final_nh, output_df_nh, by = c("neighborhood", "year"))
  rm(output_df_nh)

  if(exists("output_df_muw")) {
    assign(d %p% "_muw", output_df_muw)
    final_muw <- assign_col_join(final_muw, output_df_muw, by = c("neighborhood", "year"))
    rm(output_df_muw)
  }

  if(exists("output_df_zip")) {
    assign(d %p% "_zip", output_df_zip)
    final_zip <- assign_col_join(final_zip, output_df_zip, by = c("zip", "year"))
    rm(output_df_zip)
  }
}

all_df <- paste0(rep(drivers, each = 8), "_",
                 rep(c("county", "msa_1yr", "msa_5yr", "ky", "tract", "nh", "muw", "zip"), 4))

for(d in all_df){
  if(exists(d)){
    print(d)
    assign(d, get(d) %>% organize())
    save(list = c(d), file = paste0("data/", d, ".rda"))
  }
}

map_tract <- glptools:::map_tract
map_nh    <- glptools:::map_nh
map_muw   <- glptools:::map_muw

final_tract %<>% filter(year == 2016)
final_nh    %<>% filter(year == 2016)
final_muw   %<>% filter(year == 2016)

map_tract@data %<>% left_join(final_tract, by = c(GEO_ID = "tract"))
map_nh@data    %<>% left_join(final_nh, by = "neighborhood")
map_muw@data   %<>% left_join(final_muw, by = "neighborhood")

save(map_tract, file = "data/map_tract.rda")
save(map_nh,    file = "data/map_nh.rda")
save(map_muw,   file = "data/map_muw.rda")

