#### Run models with resampled data sets

# Call all libraries needed for all scripts
library(here)
library(R2admb)
library(PBSmodelling)
library(dplyr)
library(filesstrings)
library(R.utils)
library(abind)
library(reshape2)

# source script with custom functions and initial data set up
source(here("scripts/01_functions.R"))
source(here("scripts/02_data_setup.R")) # edit here to change stocks & starting years
source(here("scripts/04_resampled_data_setup.R"))



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
