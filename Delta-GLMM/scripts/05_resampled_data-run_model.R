#### Run model with resampled data sets

## Load packages and set working directory
# library(TMB) - I want to use version 1.7.18               
library(VAST)
library(here)
library(dplyr)

# Source user inputs, initial data formatting and function scripts
source(here("scripts/00_user-inputs.R"))
source(here("scripts/01_functions.R"))
source(here("scripts/02_all_data-formatting.R"))
source(here("scripts/04_resampled_data-formatting.R"))


# Jackknife resampled model runs, using PredTF_i to exclude one year at a time
for(stock in 1:N_stock) {
  for(year in 1:length(survey_years[[stock]])) {
    Data_Geostat <- Data_Geostat_list[[stock]]
    # Using predtf_i, exclude 1 year of data so VAST will not use it but will predict 
    # for that year
    predtf_i <- rep(NA, length = nrow(Data_Geostat))
    for(ind in 1:nrow(Data_Geostat)) {
      if(Data_Geostat$Year[ind] == survey_years[[stock]][year]) {
        predtf_i[ind] <- 1
      } else {
        predtf_i[ind] <- 0 
      }
    }
    ## Run model
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
                      "working_dir" = resampled_results_folders[[stock]][year],
                      "run_model" = TRUE,
                      "PredTF_i" = predtf_i)  
    
    ## Plot results, save in plots folder
    plot(fit, working_dir = resampled_plots_folders[[stock]][year])
    
    ## ##
    ## Save the VAST model in the results folder
    saveRDS(fit,file =  paste0(resampled_results_folders[[stock]][year], "/","VASTfit.RDS"))
  }
}
