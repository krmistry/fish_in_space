

## Load packages and set working directory
# library(TMB) - I want to use version 1.7.18               
library(VAST)
library(here)
library(dplyr)

# Source user inputs, initial data formatting and function scripts
source(here("scripts/00_user-inputs.R"))
source(here("scripts/01_functions.R"))
source(here("scripts/02_all_data-formatting.R"))

## Saving data object to send to Jim with script for troubleshooting
# saveRDS(object = Data_Geostat_list, file = "Data_Geostat_list.rds")

# Separate one species' data and rename it VAST required name:
### Currently set to run one species only (can reactivate loop to run both)
#settings$RhoConfig[c(1,2)] <- c(2, 1)
for(stock in 1:N_stock) {
  Data_Geostat <- Data_Geostat_list[[stock]]

#   
#   ## Run model
  fit <- fit_model( "settings"= settings, #all of the settings we set up above
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
                    #"PredTF_i" = predtf_i)
                    #"years_to_plot" = c(min(survey_years[[stock]]):max(survey_years[[stock]])))
                    #"year_labels" = c(min(survey_years[[stock]]):max(survey_years[[stock]])))

  ## Plot results, save in plots folder
  plot(fit, working_dir = initial_plots_folders[stock])

  ## ##
  ## Save the VAST model in the results folder
  saveRDS(fit, file =  paste0(initial_results_folders[stock],"VASTfit.RDS"))
}
#### Parameters for norther rockfish with all data
## for betas = 4, epsilons = 0, fit$tmb_list$Obj$par = 
  # ln_H_input   ln_H_input   L_omega1_z L_epsilon1_z    L_beta1_z    logkappa1 
  # 0.0000000    0.0000000    1.0000000    1.0000000    1.0000000   -0.1053605 
  # Beta_mean1_c  Beta_rho1_f   L_omega2_z L_epsilon2_z    L_beta2_z    logkappa2 
  # 0.0000000    0.0100000    1.0000000    1.0000000    1.0000000   -0.1053605 
  # Beta_mean2_c  Beta_rho2_f    logSigmaM 
  # 0.0000000    0.0100000    1.6094379 
## for betas = 0, epsilons = 0, fit$tmb_list$Obj$par =
  # ln_H_input   ln_H_input     beta1_ft     beta1_ft     beta1_ft     beta1_ft 
  # 0.0000000    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000 
  # beta1_ft     beta1_ft     beta1_ft     beta1_ft     beta1_ft     beta1_ft 
  # 0.0000000    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000 
  # beta1_ft     beta1_ft     beta1_ft     beta1_ft     beta1_ft     beta1_ft 
  # 0.0000000    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000 
  # L_omega1_z L_epsilon1_z    logkappa1     beta2_ft     beta2_ft     beta2_ft 
  # 1.0000000    1.0000000   -0.1053605    0.0000000    0.0000000    0.0000000 
  # beta2_ft     beta2_ft     beta2_ft     beta2_ft     beta2_ft     beta2_ft 
  # 0.0000000    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000 
  # beta2_ft     beta2_ft     beta2_ft     beta2_ft     beta2_ft     beta2_ft 
  # 0.0000000    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000 
  # beta2_ft   L_omega2_z L_epsilon2_z    logkappa2    logSigmaM 
  # 0.0000000    1.0000000    1.0000000   -0.1053605    1.6094379 


# Changing RhoConfig because its breaking for NR; L_beta2_z is approaching 
# zero (tried with both 1984 and 1987 excluded, and the same issue occurred); 
# the solution given for this issue is to make Beta2 = 3 (it's currently 4)
## since trying the above, L_beta1_z approached 0 (when omitting 1987), and its
# recommending Beta1 = 3 is the solution

temporal_dist_settings$RhoConfig[1:4] <- 0

settings = make_settings(Version = "VAST_v12_0_0", #.cpp version, not software #e.g., "VAST_v12_0_0"
                         n_x = 500, #knots aka spatial resolution of our estimates
                         Region = "User", #Region = "gulf_of_alaska" , go to ?make_settings for other built in extrapolation grids
                         purpose = "index2", #changes default settings
                         ObsModel= temporal_dist_settings$obsmodel,
                         ## everything after this is default if you use purpose = "index2"##
                         FieldConfig = FieldConfig, #spatial & spatiotemp random effects 
                         RhoConfig = temporal_dist_settings$RhoConfig, #temporal settings; default is all 0s, but if I specify this it will be changed here
                         strata.limits = strata.limits, #define area that you're producing index for
                         "knot_method" = "grid", #knots in proportion to spatial domain #other option is knot_method="samples"
                         fine_scale = TRUE, #changes the type of interpolation in your extrapolation area
                         bias.correct = TRUE, #corrects the index for nonlinear transformation; I want this for the final version, but I can turn it off while I'm messing with the model so it will run faster
                         use_anisotropy = TRUE) ##correlations decline depend on direction if this argument is TRUE


### After 1/27/22 meeting with Jim, Paul, Mark & Cecilia, Jim suggested trying
# running the model with betas = 2, for an autoregressive random walk rather
# than an AR1 process
settings$RhoConfig[c(1,2)] <- c(2, 1)
Data_Geostat <- Data_Geostat_list[[1]]
betas_2_results_folder <- gsub("beta_4s", "beta_2s", 
                               initial_results_folders)
betas_2_plot_folder <- gsub("beta_4s", "beta_2s", 
                            initial_plots_folders)

fit <- fit_model( "settings"= settings, #all of the settings we set up above
                  "Lat_i"= Data_Geostat[,'Lat'], #latitude of observation
                  "Lon_i"= Data_Geostat[,'Lon'],  #longitude of observation
                  "t_i"= Data_Geostat[,'Year'], #time for each observation
                  "c_i"= rep(0,nrow(Data_Geostat)), #categories for multivariate analyses; don't actually use this, could comment it out if it doesn't have a fit
                  "b_i"= Data_Geostat[,'Catch_KG'], #in kg, raw catch or in CPUE per tow
                  "a_i"= Data_Geostat[,'AreaSwept_km2'], #sampled area for each observation
                  #                 "v_i"= Data_Geostat[,'Vessel'], #ok to leave in because it's all "missing" in data, so no vessel effects
                  "input_grid"= input_grid, #only needed if you have a user input extrapolation grid (which I do)
                  "optimize_args" =list("lower"=-Inf,"upper"=Inf), #TMB argument (?fit_tmb)
                  "working_dir" = betas_2_results_folder[1],
                  "run_model" = TRUE)
# Plot results, save in plots folder
plot(fit, working_dir = betas_2_plot_folder[1])

## ##
## Save the VAST model in the results folder
saveRDS(fit, file =  paste0(betas_2_results_folder[1],"VASTfit.RDS"))



#### Trying out adding dummy data for missing years in the survey to trick
#### VAST into estimating for those years, using predtf_i

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
  # Setting up predtf_i object
  predtf_i <- rep(NA, length = nrow(Data_Geostat))
  for(ind in 1:nrow(Data_Geostat)) {
    if(Data_Geostat$Year[ind] %in% int_years) {
      predtf_i[ind] <- 1
    } else {
      predtf_i[ind] <- 0 
    }
  }
  
  # Setting RhoConfig based on species
  if(stock_names[stock] == stock_names[1]) {
    settings$RhoConfig[c(1:2)] <- c(2, 2)
  } else if (stock_names[stock] == stock_names[2]) {
    settings$RhoConfig[c(1:2)] <- c(3, 2)
  }
  
  # Run VAST
  fit <- fit_model( "settings"= settings, #all of the settings we set up above
                    "Lat_i"= Data_Geostat[,'Lat'], #latitude of observation
                    "Lon_i"= Data_Geostat[,'Lon'],  #longitude of observation
                    "t_i"= Data_Geostat[,'Year'], #time for each observation
                    "c_i"= rep(0,nrow(Data_Geostat)), #categories for multivariate analyses; don't actually use this, could comment it out if it doesn't have a fit
                    "b_i"= Data_Geostat[,'Catch_KG'], #in kg, raw catch or in CPUE per tow
                    "a_i"= Data_Geostat[,'AreaSwept_km2'], #sampled area for each observation
                    #                 "v_i"= Data_Geostat[,'Vessel'], #ok to leave in because it's all "missing" in data, so no vessel effects
                    "input_grid"= input_grid, #only needed if you have a user input extrapolation grid (which I do)
                    "optimize_args" = list("lower"=-Inf,"upper"=Inf), #TMB argument (?fit_tmb)
                    "working_dir" = paste0(initial_results_folders[[stock]], "all_years"),
                    "run_model" = TRUE,
                    "PredTF_i" = predtf_i)
  
  plot(fit, working_dir = paste0(initial_results_folders[[stock]], "all_years/Plots/"))
  saveRDS(fit,file =  paste0(initial_results_folders[[stock]], "all_years/","VASTfit.RDS"))
  
}

# It worked with adding dummy data for just 1 year!!!! (1985) Now to try adding
# dummy data for all missing years...
## It seems to have worked, but it produced slightly different results than
## running the model without the dummy data, so that doesn't seem right...


#### Running VAST with different beta settings while excluding the last 5 
#### years in order to compare them to the version with all data to evaluate
#### how well they predict - only do with POP (since NR doesn't run with 2 
#### of those settigns)

# Excluding betas = 0, so just using betas = 2, betas = 2,1 and betas = 4,
beta_options <- c("beta_2s", "beta_2_1", "beta_4s")
# Setting data to POP
Data_Geostat <- Data_Geostat_list[[1]]
# Setting years to exclude 
excl_years <- c(1996, 1999, 2005, 2009)
# ^excl_years = 1996, 1999, 2005, 2009

# Setting up predtf_i object to exclude those years data
predtf_i <- rep(NA, length = nrow(Data_Geostat))
for(ind in 1:nrow(Data_Geostat)) {
  if(Data_Geostat$Year[ind] %in% excl_years) {
    predtf_i[ind] <- 1
  } else {
    predtf_i[ind] <- 0 
  }
}
# Loop to run above data through VAST with 3 different temporal settings
for(betas in 2:2) {
  # Setting up beta setting
  beta_setting <- beta_options[betas]
  # Setting results folders
  initial_results_folders <- results_folder_fun(here("results"), 
                                                stock_names[1], 
                                                dist_setting, 
                                                beta_setting)
  initial_plots_folders <- paste0(initial_results_folders, "Plots/")
  # Updating settings$RhoConfig to correct
  if(beta_setting == "beta_2s") {
    settings$RhoConfig[1:2] <- c(2, 2)
  } else if (beta_setting == "beta_2_1") {
    settings$RhoConfig[1:2] <- c(2, 1)
  } else if (beta_setting == "beta_4s") {
    settings$RhoConfig[1:2] <- c(4, 4)
  }
  # Run VAST
  fit <- fit_model( "settings"= settings, #all of the settings we set up above
                    "Lat_i"= Data_Geostat[,'Lat'], #latitude of observation
                    "Lon_i"= Data_Geostat[,'Lon'],  #longitude of observation
                    "t_i"= Data_Geostat[,'Year'], #time for each observation
                    "c_i"= rep(0,nrow(Data_Geostat)), #categories for multivariate analyses; don't actually use this, could comment it out if it doesn't have a fit
                    "b_i"= Data_Geostat[,'Catch_KG'], #in kg, raw catch or in CPUE per tow
                    "a_i"= Data_Geostat[,'AreaSwept_km2'], #sampled area for each observation
                    #                 "v_i"= Data_Geostat[,'Vessel'], #ok to leave in because it's all "missing" in data, so no vessel effects
                    "input_grid"= input_grid, #only needed if you have a user input extrapolation grid (which I do)
                    "optimize_args" = list("lower"=-Inf,"upper"=Inf), #TMB argument (?fit_tmb)
                    "working_dir" = initial_results_folders,
                    "run_model" = TRUE,
                    "PredTF_i" = predtf_i)
                    #"test_fit" = FALSE) # Used when parameters are going to 0
  # Save plots & data file
  plot(fit, working_dir = initial_plots_folders)
  saveRDS(fit,file =  paste0(initial_results_folders, "/","VASTfit.RDS"))
}

## Both betas = 2 and betas = 4 had issues with parameters going to 0, so 
## I re-ran the above with test_fit = FALSE




#### Adding dummy data for 2020 - 2022 to get predictions to compare to RE model

for (stock in 1:N_stock) {
  # Selecting species data
  Data_Geostat <- Data_Geostat_list[[stock]]
  print(nrow(Data_Geostat))
  # Separating out interior years from survey years
  pred_years <- c(2020:2022)
  n_pred_years <- length(pred_years)
  # Creating some dummy data by sampling within existing data
  dummy_df <- as.data.frame(matrix(NA, nrow = 50*n_pred_years, 
                                   ncol = ncol(Data_Geostat)))
  colnames(dummy_df) <- colnames(Data_Geostat)
  dummy_df[, 1] <- rep(1, 50*n_pred_years)
  dummy_df[, 2] <- rep(pred_years, 50)
  dummy_df[, 3] <- sample(Data_Geostat$AreaSwept_km2, 50*n_pred_years)
  dummy_df[, 4] <- sample(Data_Geostat$Lat, 50*n_pred_years)
  dummy_df[, 5] <- sample(Data_Geostat$Lon, 50*n_pred_years) 
  
  # Adding dummy data to Data_Geostat object
  Data_Geostat <- rbind(Data_Geostat, dummy_df)
  # Setting up predtf_i object
  predtf_i <- rep(NA, length = nrow(Data_Geostat))
  for(ind in 1:nrow(Data_Geostat)) {
    if(Data_Geostat$Year[ind] %in% pred_years) {
      predtf_i[ind] <- 1
    } else {
      predtf_i[ind] <- 0 
    }
  }
  print(nrow(Data_Geostat))
  print(sum(predtf_i))
  # Run VAST
  fit <- fit_model( "settings"= settings, #all of the settings we set up above
                    "Lat_i"= Data_Geostat[,'Lat'], #latitude of observation
                    "Lon_i"= Data_Geostat[,'Lon'],  #longitude of observation
                    "t_i"= Data_Geostat[,'Year'], #time for each observation
                    "c_i"= rep(0,nrow(Data_Geostat)), #categories for multivariate analyses; don't actually use this, could comment it out if it doesn't have a fit
                    "b_i"= Data_Geostat[,'Catch_KG'], #in kg, raw catch or in CPUE per tow
                    "a_i"= Data_Geostat[,'AreaSwept_km2'], #sampled area for each observation
                    #                 "v_i"= Data_Geostat[,'Vessel'], #ok to leave in because it's all "missing" in data, so no vessel effects
                    "input_grid"= input_grid, #only needed if you have a user input extrapolation grid (which I do)
                    "optimize_args" = list("lower"=-Inf,"upper"=Inf), #TMB argument (?fit_tmb)
                    "working_dir" = initial_results_folders[[stock]],
                    "run_model" = TRUE,
                    "PredTF_i" = predtf_i,
                    "test_fit" = FALSE)
  
  plot(fit, working_dir = initial_plots_folders[[stock]])
  saveRDS(fit,file =  paste0(initial_results_folders[[stock]], "/","VASTfit.RDS"))
  
}



