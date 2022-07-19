#### Run models

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


# If you have issues running the code below, either source the whole .R file or
# copy and paste the following compile_admb and run_admb lines into the console
# (lines 27 - 43) to run, rather than using command-enter to run them from the
# script editing window.

### Compile and run ADMB-RE model ###
for(sub in 1:N_sub) {
  for(stock in 1:N_stock) {
    ### Note: can't use results_folders object inside the below functions
    ### (doesn't run for some reason)
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






