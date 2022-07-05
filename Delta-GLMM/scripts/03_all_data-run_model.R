

## Load packages and set working directory
# library(TMB) - I want to use version 1.7.18               
library(VAST)
library(here)
library(dplyr)

# Source user inputs, initial data formatting and function scripts
source(here("scripts/00_user-inputs.R"))
source(here("scripts/01_functions.R"))
source(here("scripts/02_all_data-formatting.R"))

### Separate one species' data and rename it VAST required name:
for(stock in 1:N_stock) {
  Data_Geostat <- Data_Geostat_list[[stock]]

  ## Run model
  fit <- fit_model( "settings"= settings[[stock]], 
                    "Lat_i"= Data_Geostat[,'Lat'], #latitude of observation
                    "Lon_i"= Data_Geostat[,'Lon'],  #longitude of observation
                    "t_i"= Data_Geostat[,'Year'], #time for each observation
                    "c_i"= rep(0,nrow(Data_Geostat)), #categories for multivariate analyses; don't actually use this, could comment it out if it doesn't have a fit
                    "b_i"= Data_Geostat[,'Catch_KG'], #in kg, raw catch or in CPUE per tow
                    "a_i"= Data_Geostat[,'AreaSwept_km2'], #sampled area for each observation
                    #                 "v_i"= Data_Geostat[,'Vessel'], #ok to leave in because it's all "missing" in data, so no vessel effects
                    "input_grid"= input_grid, #only needed if you have a user input extrapolation grid (which I do)
                    "optimize_args" =list("lower"=-Inf,"upper"=Inf), #TMB argument (?fit_tmb)
                    "working_dir" = initial_results_folders[stock],
                    "run_model" = TRUE)

  ## Plot results, save in plots folder
  plot(fit, working_dir = initial_plots_folders[stock])

  ## Save the VAST model in the results folder
  saveRDS(fit, file =  paste0(initial_results_folders[stock],"VASTfit.RDS"))
}



#### Doing a separate model run with dummy data for missing years in the survey and 
#### predicting out 3 years (2020 - 2022) to trick VAST into estimating for those years, 
#### using predtf_i argument (for figure 2, and table 2 in results)

for(stock in 1:N_stock) {
  # Selecting species data
  Data_Geostat <- Data_Geostat_list[[stock]]
  # Separating out interior years from survey years
  int_years <- c(subset(c(start_years[stock]:end_years[stock]), !(c(start_years[stock]:end_years[stock]) %in% survey_years[[stock]])),
                 2020:2022)
  n_int_years <- length(int_years)
  # Creating some dummy data by sampling within existing data
  dummy_df <- as.data.frame(matrix(NA, nrow = 50*n_int_years, 
                                   ncol = ncol(Data_Geostat)))
  colnames(dummy_df) <- colnames(Data_Geostat)
  # dummy_df[, 1] <- sample(Data_Geostat$Catch_KG, 1*n_int_years)
  dummy_df[, 1] <- rep(1, 50*n_int_years)
  dummy_df[, 2] <- rep(int_years, 50)
  dummy_df[, 3] <- sample(Data_Geostat$AreaSwept_km2, 50*n_int_years)
  dummy_df[, 4] <- sample(Data_Geostat$Lat, 50*n_int_years)
  dummy_df[, 5] <- sample(Data_Geostat$Lon, 50*n_int_years) 
  
  # Adding dummy data to Data_Geostat object
  Data_Geostat <- rbind(Data_Geostat, dummy_df)
  # Setting up vector object for predtf_i argument
  predtf_i <- rep(NA, length = nrow(Data_Geostat))
  for(ind in 1:nrow(Data_Geostat)) {
    if(Data_Geostat$Year[ind] %in% int_years) {
      predtf_i[ind] <- 1
    } else {
      predtf_i[ind] <- 0 
    }
  }
  
  # Run VAST
  fit <- fit_model( "settings"= settings[[stock]], #all of the settings we set up above
                    "Lat_i"= Data_Geostat[,'Lat'], #latitude of observation
                    "Lon_i"= Data_Geostat[,'Lon'],  #longitude of observation
                    "t_i"= Data_Geostat[,'Year'], #time for each observation
                    "c_i"= rep(0,nrow(Data_Geostat)), #categories for multivariate analyses; don't actually use this, could comment it out if it doesn't have a fit
                    "b_i"= Data_Geostat[,'Catch_KG'], #in kg, raw catch or in CPUE per tow
                    "a_i"= Data_Geostat[,'AreaSwept_km2'], #sampled area for each observation
                    #                 "v_i"= Data_Geostat[,'Vessel'], #ok to leave in because it's all "missing" in data, so no vessel effects
                    "input_grid"= input_grid, #only needed if you have a user input extrapolation grid (which I do)
                    "optimize_args" = list("lower"=-Inf,"upper"=Inf), #TMB argument (?fit_tmb)
                    "working_dir" = all_years_results_folders[stock],
                    "run_model" = TRUE,
                    "PredTF_i" = predtf_i)
  
  plot(fit, working_dir = all_years_plots_folders[stock])
  saveRDS(fit,file =  paste0(all_years_results_folders[stock], "VASTfit.RDS"))
}


