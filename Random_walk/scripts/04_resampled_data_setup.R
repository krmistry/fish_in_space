## Set up data and results folders for resampled data sets

# Create lists of results folder names
resampled_results_folders <- list()
for (stock in 1:N_stock) {
  stock_folders <- list()
  for (sub in 1:N_sub) {
    species_years <- survey_years[[stock]]
    stock_folders[[sub]] <- lapply(species_years[[sub]], function(x) {
      paste0(here("results/Resampling_results/"), stock_folder_names[stock], "/", 
             subregion[sub], "/", x)
    })
    stock_folders[[sub]] <- unlist(stock_folders[[sub]])
  }
  resampled_results_folders[[stock]] <- stock_folders
  names(stock_folders) <- subregion
}
names(resampled_results_folders) <- stock_folder_names

# Check to see if resampled results folders exist, create if not
for(stock in 1:N_stock) {
  for (sub in 1:N_sub) {
    for (year in 1:length(survey_years[[stock]][[sub]])) {
      if(!dir.exists(resampled_results_folders[[stock]][[sub]][year])) {
        dir.create(resampled_results_folders[[stock]][[sub]][year], recursive = TRUE)
      } else {
        print("Directories exist")
      }
    }
  }
}



# Create jackknife resampled data sets, saving into appropriate folders
for (stock in 1:N_stock) {
  for (sub in 1:N_sub) {
    for (year in 1:length(survey_years[[stock]][[sub]])) {
      # Exclude one year of data from the dataset
      jackknifed_data <- separated_stock_data[[stock]][[sub]][-year,]
      # Formatting the data into the re.dat required format
      re.dat <- re.dat_inputs_fun(jackknifed_data, unname(start_years[stock]), unname(end_years[stock]))
      # Writing re.dat into the appropriate folder
      write_dat(paste0(resampled_results_folders[[stock]][[sub]][year], "/re"),
                L = re.dat)
      # Copy re.tpl file into folder
      file.copy(from = here("re.tpl"),
                to = resampled_results_folders[[stock]][[sub]][year],
                overwrite = TRUE)
      
    }
  }
}

