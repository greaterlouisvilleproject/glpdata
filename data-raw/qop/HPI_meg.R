library(glptools)
glp_load_packages()

path <- "data-raw/qop/HPI/"


zip_map <- glptools::map_zip

#HPI_zip <- readxl::read_xlsx("/Users/mraisle/Documents/OneDrive - University of North Carolina at Chapel Hill/glpdata/data-raw/qop/HPI/HPI_AT_BDL_ZIP5.xlsx", skip = 6)

#HPI_zip_forapp <- HPI_zip %>%
 # transmute(
 #   zip  = as.numeric(`Five-Digit ZIP Code`),
  #  year = as.numeric(Year),
  #  HPI_2000  = as.numeric(`HPI with 2000 base`)) %>%
  #filter(zip %in% zip_map$zip, zip !="40067") %>%
 # group_by(zip)

# ^all of this done in glpdata::HPI_zip_forapp



#ok now we've got the column down now we need to make it display just the two values we want

HPI_zip <- glpdata::HPI_zip_forapp
year1 <- 1990
year2 <- 2017
homevalue <- 100000
zipneeded <- HPI_zip %>% filter(zip=="40299", !is.na(year == year1)) %>%
  mutate(
    index_year1 = round((HPI_2000 / HPI_2000[year == year1] * 100), 2)) %>%
  filter(year == year2)

year2_index <- as.numeric(zipneeded[1,4])
year2_multiplier <- year2_index/100
newprice <- (homevalue*year2_multiplier)

#ok so i think i've got all the numbers figured out, what i need to do know if make sure tht the dataframe
#will update with every change of value . . .i think this will happen if i do it in reactive



#usethis::use_data(HPI_zip_forapp, overwrite = TRUE)

