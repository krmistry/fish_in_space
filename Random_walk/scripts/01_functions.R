### Function to filter and arrange data by species and subregion & create CV values
## ARGUMENTS
## data = raw survey data in dataframe format
## species = one of the species in stock_folder_names list
## start_year = first year of survey data for that species, from start_year vector in 
##              02_data_setup.R
## subregion = vector of subregion names, i.e. subregion object in 02_data_setup.R
#
## RETURNS
## subregion_data = dataframe of survey data for one species & subregion

initial_set_up_fun <- function(data, 
                               species, 
                               start_year, 
                               subregion) {
  # filtering by species, species-specific start year and subregion, then ordering the dataframe by year
  subregion_data <- data %>% subset(SPECIES_CODE == my_stock_ids[species]
                                    & YEAR >= start_year
                                    & grepl(subregion, REGULATORY_AREA_NAME)) %>% arrange(YEAR)
  
  # check for 0s in the biomass data
  if(sum(isZero(subregion_data$MEAN_WGT_CPUE)) > 0){
    # if there are zeros, identify the indices for the zeros
    zero_ind <- which(isZero(subregion_data$MEAN_WGT_CPUE))
    # replace biomass mean 0s with 0.1 (NOT adding 0.1 to all values, just the zero)
    subregion_data$MEAN_WGT_CPUE[zero_ind] = subregion_data$MEAN_WGT_CPUE[zero_ind] + 0.1
    subregion_data$AREA_BIOMASS[zero_ind] = subregion_data$AREA_BIOMASS[zero_ind] + 0.1
    # replace biomass variance 0s with 0.0001
    subregion_data$VAR_WGT_CPUE[zero_ind] = subregion_data$VAR_WGT_CPUE[zero_ind] + 1e-4
  } # the above prevents the CV from being NA, but since the VAR_WGT_CPUE value is 0, it means CV = 0; the model threw an error with the matrix inverse, so does this mean that the CV can't be 0? Should I artificially give it a value? Or give VAR_WGT_CPUE a value?
  
  # calculating CV values
  subregion_data$BIOMASS_CV <-  (sqrt(subregion_data$VAR_WGT_CPUE)/subregion_data$MEAN_WGT_CPUE) # might be wrong right now, I need to ask Cecilia which columns this should be
  return(subregion_data)
}


### Function to create and organize the data that goes into the re.dat file, the input 
### for the ADMB model
## ARGUMENTS
## data = use dataframe created by the inital_set_up function
## start_year = first year of survey data for a species, from start_year vector in 
##              02_data_setup.R
## end_year = end year of survey data for a species, from end_year vector in 02_data_setup.R
#
## RETURNS
## list of 6:
## styr = first survey year
## endyr = last survey year 
## nobs = number of survey years
## yrs_srv = survey years
## srv_est = estimated subregional biomass from survey 
## srv_cv = estimated CV of subregional biomass from survey
re.dat_inputs_fun <- function(data, 
                              start_year, 
                              end_year) {
  input.yrs <- data$YEAR
  input.idx <- data$AREA_BIOMASS 
  input.cv <- data$BIOMASS_CV
  #DateFile <- re.dat_directory
  styr <- start_year  #Start year
  endyr <- end_year  #End year
  nobs <- length(input.yrs)  #Number of surveys
  yrs_srv <- input.yrs  #Survey years
  srv_est <- input.idx  #Survey Biomass
  srv_cv <- input.cv  #Survey Biomass CV
  return(list(styr, 
              endyr, 
              nobs, 
              yrs_srv, 
              srv_est, 
              srv_cv))
}




