# Function to filter by species & years and create the VAST input data
## ARGUMENTS
## initial_data = a dataframe survey data that's already had CPUE calculated. Required columns 
##                are SPECIES_CODE, YEAR, HAULJOIN, START_LATITUDE, START_LONGITUDE, EFFORT, 
##                WEIGHT
## stock_id = a vector of numeric IDs for each stock needed (the input my_stock_ids is created 
##                 in initial_data_formatting_script.R)
## species = a vector with 1 element, with the latin name of the stock (from names(my_stock_ids))
## raw_haul = a dataframe of raw survey haul data, containing the columns HAULJOIN, START_LATITUDE,
##            START_LONGITUDE
#
## RETURNS
## Data_Geostat = A dataframe with survey data for one species, including columns for year, catch by weight, 
## start latitude and longitude (location of start of haul), and the area covered in the haul in km^2

filter_create_VAST_input <- function(initial_data, 
                                     stock_id, 
                                     species, 
                                     starting_year, 
                                     haul_data){
  
  # Separating out one species
  species_data <- initial_data[initial_data$SPECIES_CODE == stock_id[species],]
  
  # filter by starting year
  species_data <- species_data[species_data$YEAR >= starting_year, ] 
  
  # Adding in start longitude and latitude from the raw_haul data frame into the 
  # CPUE_data data frame
  species_data <- merge(species_data, haul_data[, c("HAULJOIN", "START_LATITUDE",
                                                    "START_LONGITUDE")], 
                        by = "HAULJOIN")
  
  # Create the VAST input dataframe, with column required by VAST functions
  Data_Geostat <- species_data[, c("WEIGHT", "YEAR", "EFFORT", "START_LATITUDE", 
                                   "START_LONGITUDE")]
  
  # Renaming column names to the ones required by VAST functions
  colnames(Data_Geostat) <- c("Catch_KG", "Year", "AreaSwept_km2", "Lat", "Lon")
  
  return(Data_Geostat)
}


# Function to set Rhoconfig beta and obsmodel settings based on user choices
## ARGUMENTS
## dist_setting = distribution used for observation model; Gamma in this analysis
## beta_setting = the 2 settings dictating the temporal coefficient form for encounter 
##                probability and positive catch 
## epsilon_setting = the 2 settings dictating the spatiotemporal coefficient form for
##                    encounter probability and positive catch
## stock = one of the species in stock_folder_names list
##
## RETURNS
## list of 2:
## RhoConfig = a list with a vector of 4 named elements, 2 for temporal settings (betas) and 2 for spatiotemporal 
## settings (epsilons)
## obsmodel = a vector of the 2 settings for the positive catch rate distribution

dist_beta_setting_fun <- function(dist_setting, 
                                  beta_setting,
                                  epsilon_setting,
                                  stock) {
  # Setting the catch rate distribution model settings
  if(dist_setting == "Gamma") {
    obsmodel <- c(2, 1) 
  } else {
    stop("Missing or invalid distribution setting")
  }
  # Setting up the betas for RhoConfig based on the species
  betas <- beta_setting[[stock]]
  # Creating RhoConfig VAST input object
  RhoConfig  <- c("Beta1" = betas[1], "Beta2" = betas[2], 
                  "Epsilon1" = epsilon_setting[1], "Epsilon2" = epsilon_setting[2])
  
  return(list(RhoConfig = RhoConfig, obsmodel = obsmodel))
}


# Function to set up names of results folders for all data results
## ARGUMENTS
## starting_point = here("results")
## stock = the species, from stock_folder_names
## 
## RETURNS
## results_folder = A single string with the name for the results folder for that species

results_folder_fun <- function(starting_point, 
                               stock) {
  results_folder <- paste0(starting_point, "/", stock, "/")
  return(results_folder)
}
