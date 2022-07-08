#### Run models

# Call all libraries needed for all scripts
library(here)
library(R2admb)
library(PBSmodelling)
library(dplyr)
#library(ggplot2)
library(filesstrings)
library(R.utils)
library(abind)
#library(RColorBrewer)
library(reshape2)

# source script with custom functions and initial data set up
source(here("scripts/functions.R"))
##### Run both models with all years and models with jackknife resampled data sets 
##### with ADMB random walk model

source(here("scripts/01_functions.R"))
source(here("scripts/02_data_setup.R")) # edit here to change stocks & starting years


# Either source the whole .R file or copy and paste the following compile_admb and 
# run_admb lines into the console (lines 27 - 43) to run, rather than using command-enter 
# to run them from the script editing window. 

### Compile and run ADMB-RE model ###
for(sub in 1:N_sub) {
  for(stock in 1:N_stock) {
    ### Note: can't use results_folders object inside the below functions (doesn't run for some reason)
    # Creating file path for re.tpl file
    re_file <- paste0("results/", stock_folder_names[stock], "/", subregion[sub], "/re")
    # Compile re.tpl file
    compile_admb(fn = paste0("results/", stock_folder_names[stock], "/", 
                             subregion[sub], "/re"), 
                 re = TRUE, verbose=TRUE) 
    # Run model
    run_admb(fn = paste0("results/", stock_folder_names[stock], "/", 
                         subregion[sub], "/re"), 
             verbose = FALSE)
    # Several of the results files are going into the project directory rather than the 
    # appropriate subregion/species folder, so this moves them into the appropriate 
    # directories
    for(file in 1:length(result_files)) {
      file.rename(here(result_files[file]), paste0(results_folders[stock, sub], 
                                                   "/", result_files[file]))
    }
  }
} 


# Creating new folders for the resampled results
for(stock in 1:N_stock) {
  for (sub in 1:N_sub) {
    for (year in 1:length(survey_years[[stock]][[sub]])) {
      dir.create(paste0(here("results/Resampling_results/"), stock_folder_names[stock], "/", subregion[sub],"/",  survey_years[[stock]][[sub]][year]), recursive = TRUE)
    }
  }
}


resampled_results_folders <- list()
for (stock in 1:N_stock) {
  stock_folders <- list()
  for (sub in 1:N_sub) {
    species_years <- survey_years[[stock]]
    stock_folders[[sub]] <- lapply(species_years[[sub]], function(x) {
      paste0(here("results/Resampling_results/"), stock_folder_names[stock], "/", subregion[sub], "/", x)
    })
    stock_folders[[sub]] <- unlist(stock_folders[[sub]])
  }
  resampled_results_folders[[stock]] <- stock_folders
  names(stock_folders) <- subregion
}
names(resampled_results_folders) <- stock_folder_names 


# Create jackknife resampled data sets, saving into appropriate folders
for (stock in 1:N_stock) {
  for (sub in 1:N_sub) {
    for (year in 1:length(survey_years[[stock]][sub])) {
      # Exclude one year of data from the dataset
      jackknifed_data <- separated_stock_data[[stock]][[sub]][-year,]
      # Formatting the data into the re.dat required format
      re.dat <- re.dat_inputs_fun(jackknifed_data, unname(start_years[stock]), unname(end_years[stock]))
      # Writing re.dat into the appropraite folder
      write_dat(paste0(resampled_results_folders[[stock]][[sub]][[year]], "/re"),
                L = re.dat)
      # Copy re.tpl file into folder
      file.copy(from = here("re.tpl"),
                to = resampled_results_folders[[stock]][[sub]][[year]],
                overwrite = TRUE)
      
    }
  }
}



# Run resampled data sets in the ADMB model, saving results into appropriate folders
for (stock in 1:N_stock) {
  for (sub in 1:N_sub) {
    for (year in 1:length(survey_years[[stock]][[sub]])) {
      # Creating partial (rather than full) file path, as required by the below ADMB functions
      resample_folder <- sub(paste0(here(), "/"), "", resampled_results_folders[[stock]][[sub]][[year]])
      # Compile ADMB model
      compile_admb(fn = paste0(resample_folder, "/re"), re = TRUE, verbose=TRUE)
      # Run ADMB model
      run_admb(fn = paste0(resample_folder, "/re"), verbose = FALSE)
      # Move output files that end up in the main project folder into appropriate results folder
      for(file in 1:length(result_files)) {
        file.rename(here(result_files[file]), paste0(resampled_results_folders[[stock]][[sub]][[year]], "/", result_files[file]))
      }
    }
  }
}


