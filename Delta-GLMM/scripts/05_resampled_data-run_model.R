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

# Changing settings$RhoConfig to (2,1)
settings$RhoConfig[c(1,2)] <- c(2, 1)
length(survey_years[[stock]])
# Resampled runs, using PredTF_i to exclude one year at a time
for(stock in 1:1) {
  for(year in 11:length(survey_years[[stock]])) {
    Data_Geostat <- Data_Geostat_list[[stock]]
    predtf_i <- rep(NA, length = nrow(Data_Geostat))
    for(ind in 1:nrow(Data_Geostat)) {
      if(Data_Geostat$Year[ind] == survey_years[[stock]][year]) {
        predtf_i[ind] <- 1
      } else {
        predtf_i[ind] <- 0 
      }
    }
    ## Run model
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


# Got errors with RhoConfig = 0 for both betas and epsilons for Northern Rockfish
# The errors were for parameters with gradient of zero at starting values,
# so experimenting with starting values below

# Separating out one year
Data_Geostat <- Data_Geostat_list[[2]]
crappy_years <- c(2, 6, 7)
for(year in crappy_years) {
  predtf_i <- rep(NA, length = nrow(Data_Geostat))
  for(ind in 1:nrow(Data_Geostat)) {
    if(Data_Geostat$Year[ind] == survey_years[[2]][year]) {
      predtf_i[ind] <- 1
    } else {
      predtf_i[ind] <- 0 
    }
  }
  
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
                    "working_dir" = resampled_results_folders[[2]][year],
                    "run_model" = TRUE,
                    "PredTF_i" = predtf_i,
                    "test_fit" = FALSE) # used when parameters are going to 0
  
  plot(fit, working_dir = resampled_plots_folders[[2]][year])
  saveRDS(fit,file =  paste0(resampled_results_folders[[2]][year], "/","VASTfit.RDS"))
  
}
 

## Using all subregions, with 1990 missing, betas = 0 and epsilons = 0, this
## is the error message I got:
# The following parameters appear to be approaching zero:
#   Param starting_value Lower           MLE Upper final_gradient
# 11 L_beta2_z              1  -Inf -1.032588e-07   Inf   -0.006021448

# Trying out excluding eastern subregion, both from the data set and from the
# settings$strata.limit object

# excluding eastern subregion from both strata.limits object and data set
settings$strata.limits <- settings$strata.limits[-3,]
settings$strata.limits[2,3] <- "Inf"
Data_Geostat <- Data_Geostat[Data_Geostat$Lon <= -147,]
# setting up predft_i object to exclude one year of data
predtf_i <- rep(NA, length = nrow(Data_Geostat))
for(ind in 1:nrow(Data_Geostat)) {
  if(Data_Geostat$Year[ind] == survey_years[[2]][1]) {
    predtf_i[ind] <- 1
  } else {
    predtf_i[ind] <- 0 
  }
}
# setting betas to 0 instead of 4 (the default)
settings$RhoConfig[c(1,2)] <- c(2, 1)
Data_Geostat <- Data_Geostat_list[[2]]
# setting up predft_i object to exclude one year of data
predtf_i <- rep(NA, length = nrow(Data_Geostat))
for(ind in 1:nrow(Data_Geostat)) {
  if(Data_Geostat$Year[ind] == survey_years[[2]][1]) {
    predtf_i[ind] <- 1
  } else {
    predtf_i[ind] <- 0 
  }
}

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
                  "working_dir" = resampled_results_folders[[2]][1],
                  "run_model" = TRUE,
                  "PredTF_i" = predtf_i)
plot(fit, working_dir = resampled_plots_folders[[2]][1])
saveRDS(fit,file =  paste0(resampled_results_folders[[2]][1], "/","VASTfit.RDS"))

## Wouldn't run and got this error, not sure what its saying:
# Calculating range shift for stratum #1:
# Error in (function (b_i, a_i, t_i, c_iz = rep(0, length(b_i)), e_i = c_iz[,  : 
# b_i, a_i, c_i, s_i, v_i, or tprime_i doesn't have length n_i
### predtf_i was the wrong length for the above run, tha was probably the problem

# ## with 1990 missing, correct predtf_i length, betas = 4, epsilons = 0, got this error:
# The following parameters appear to be approaching zero:
#   Param starting_value Lower          MLE Upper final_gradient
# 14 Beta_rho2_f           0.01  -Inf 2.257316e-05   Inf   0.0001343746

## with 1990 missing, correct predft_i length, betas = 0, epsilons = 0:
# Error: Please check model structure; some parameter has a gradient of zero at 
# starting values
# fit$tmb_list$Obj$par = 
#   ln_H_input   ln_H_input     beta1_ft     beta1_ft     beta1_ft     beta1_ft 
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

original_parameters <- fit$tmb_list$Obj$par
zero_pars <- which(original_parameters == 0)
non_zero_pars <- which(original_parameters != 0)
# length(zero_pars) = 34

# Run model with original parameters starting values:
fit$tmb_list$Obj$par <- original_parameters
# fit model without running
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
                  "working_dir" = resampled_results_folders[[2]][1],
                  "run_model" = FALSE,
                  "PredTF_i" = predtf_i)

# Set to original parameter starting values:
fit$tmb_list$Obj$par <- original_parameters
# Adjusting one parameter at a time by 0.5:
fit$tmb_list$Obj$par[zero_pars[11]] <- 0.5
# Re-fit model and try running it
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
                  "working_dir" = resampled_results_folders[[2]][1],
                  "run_model" = TRUE,
                  "PredTF_i" = predtf_i)

# For loop to check if each parameter value one at a time works and move on if 
# the model breaks
for (parameter in 32:length(zero_pars)) {
  
  skip_to_next <- FALSE
  tryCatch({
    # Set to original parameter starting values:
    fit$tmb_list$Obj$par <- original_parameters
    # Adjusting one parameter at a time that originally had 0 values to 0.5:
    fit$tmb_list$Obj$par[zero_pars[parameter]] <- 0.5
    # Re-fit model and try running it
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
                      "working_dir" = resampled_results_folders[[2]][1],
                      "run_model" = TRUE,
                      "PredTF_i" = predtf_i)}, 
           error = function(e) { 
             print(paste0(zero_pars[parameter], " - error "))
             skip_to_next <<- TRUE
             })
  
  if(skip_to_next == TRUE) {
    next
    } else if(skip_to_next == FALSE) {
    print(paste0(zero_pars[parameter], " - sucess ")) 
    break
    }
}
  
# For loop to check if 2 parameters values being changed works and move on if 
# the model breaks
for(parameter_1 in 29:length(zero_pars)) {
  for (parameter_2 in parameter_1:length(zero_pars)) {
    skip_to_next <- FALSE
    tryCatch({
      # Set to original parameter starting values:
      fit$tmb_list$Obj$par <- original_parameters
      # Adjusting one parameter at a time that originally had 0 values to 0.5:
      fit$tmb_list$Obj$par[zero_pars[parameter_1]] <- 0.5
      fit$tmb_list$Obj$par[zero_pars[parameter_2]] <- 0.5
      # Re-fit model and try running it
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
                        "working_dir" = resampled_results_folders[[2]][1],
                        "run_model" = TRUE,
                        "PredTF_i" = predtf_i)}, 
      error = function(e) { 
        print(paste0(zero_pars[parameter_1], " combined with ",
                     zero_pars[parameter_2], " - error "))
        skip_to_next <<- TRUE
      })
    
    if(skip_to_next == TRUE) {
      next
    } else if(skip_to_next == FALSE) {
      print(paste0(zero_pars[parameter_1], " combined with ",
                   zero_pars[parameter_2], " - sucess ")) 
      break
    }
  }
}

# Stopped the above after completing 28

# Trying all beta1_ft set to 0.5
fit$tmb_list$Obj$par <- original_parameters
fit$tmb_list$Obj$par[zero_pars] <- 0.5
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
                  "working_dir" = resampled_results_folders[[2]][1],
                  "run_model" = TRUE,
                  "PredTF_i" = predtf_i)

# Changing non-zero parameters to 0.5 one at a time, just for the hell of it
for(parameter in 1:length(non_zero_pars)) {
  skip_to_next <- FALSE
  tryCatch({
    fit$tmb_list$Obj$par <- original_parameters
    fit$tmb_list$Obj$par[non_zero_pars[parameter]] <- 0.5
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
                      "working_dir" = resampled_results_folders[[2]][1],
                      "run_model" = TRUE,
                      "PredTF_i" = predtf_i)},
    error = function(e) { 
      print(paste0(non_zero_pars[parameter], " - error" ))
      skip_to_next <<- TRUE
    })
  if(skip_to_next == TRUE) {
    next
  } else if(skip_to_next == FALSE) {
    print(paste0(non_zero_pars[parameter], " - success" )) 
    break
  }
}


############# Experimenting with parallel R loops (trying to figure out why
############# I got errors last time) 
library(doParallel)
library(foreach)

# Setting epsilons to 0 in order to get it to run quickly (hopefully in just 2 hours)
settings$RhoConfig[3:4] <- c(0, 0)

# Set up parallel backend
parallel::detectCores()
n.cores <- 3
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()

# Setting up a fit object to save to (so its not overwriting in each iteration)
#fit <- list()

# Running first 3 replicates for POP as a test (should take a couple of hours)
x <- foreach (
  year = 1:3,
  .combine = 'c'
) %dopar% {
  Data_Geostat <- Data_Geostat_list[[1]]
  predtf_i <- rep(NA, length = nrow(Data_Geostat))
  for(ind in 1:nrow(Data_Geostat)) {
    if(Data_Geostat$Year[ind] == survey_years[[1]][year]) {
      predtf_i[ind] <- 1
    } else {
      predtf_i[ind] <- 0 
    }
  }
  ## Run model
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
                    "working_dir" = resampled_results_folders[[1]][year],
                    "run_model" = TRUE,
                    "PredTF_i" = predtf_i)  
  return(fit)
  # ## Plot results, save in plots folder
  # plot(fit[year], working_dir = resampled_plots_folders[[1]][year])
  # 
  # ## ##
  # ## Save the VAST model in the results folder
  # saveRDS(fit[year], file =  paste0(resampled_results_folders[[1]][year], "/","VASTfit.RDS"))
}

# stop cluster when done
parallel::stopCluster(cl = my.cluster)
