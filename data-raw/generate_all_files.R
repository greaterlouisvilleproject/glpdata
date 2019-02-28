library(stringr)

drivers <- c("educ", "jobs","health", "qop")

for(d in drivers[1]){

  # List the scripts in the driver directory
  script_dir <- paste(getwd(), "data-raw", d, sep = "/")

  scripts <- list.files(script_dir)

  scripts <- scripts[grepl(".R", scripts)]

  for(s in scripts){

    df_name <- str_remove(s, ".R")

    print(df_name)

    # Run script to generate data frames
    source(paste0(script_dir, "/", s))

    # Check to see is map file exists for this data frame
    if (exists(df_name %p% "_map")) this_map <- TRUE
    else this_map <- FALSE

    # Assign output files to df and df_map
    df <- eval(as.name(df_name))

    if(this_map) df_map <- eval(as.name(df_name %p% "_map"))

    # Determine the name of the output data frames
    driver_df <- paste0(d, "_", df_type(df))

    driver_map_df <- d %p% "_map"

    # Create or bind the data frame to the output data frame
    if(!exists(driver_df)){
      assign(driver_df, df)
    } else {
      assign(driver_df, bind_df(get(driver_df), df))
    }

    if(this_map & !exists(driver_map_df)){
      assign(driver_map_df, df_map)
    } else if (this_map){
      assign(driver_map_df, bind_df(get(driver_map_df), df_map))
    }
  }
}

all_df <- paste0(rep(drivers, each = 5), "_",
                 rep(c("county", "MSA", "ky_ed", "naep", "map"), 4))

for(d in all_df){
  if(exists(d)){
    assign(d, get(d))
    save(list = c(d), file = paste0("data/", d, ".rda"))
  }
}

usethis::use_data(educ_county, overwrite = TRUE)
usethis::use_data(educ_map, overwrite = TRUE)
usethis::use_data(educ_ky_ed, overwrite = TRUE)
usethis::use_data(educ_naep, overwrite = TRUE)







