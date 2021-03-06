### Script to set up folders for jackknifed replicate results

# Set up vectors of results & result plots for resampled data
resampled_results_folders <- list()
resampled_plots_folders <- list()
for(stock in stock_folder_names) {
  resampled_results_folders[[stock]] <- vector()
  resampled_plots_folders[[stock]] <- vector()
  for(year in 1:length(survey_years[[stock]])) {
    resampled_results_folders[[stock]][year] <- paste0(results_folder_fun(here("results/Resampled_results"), 
                                                                          stock),
                                                       survey_years[[stock]][year])
    resampled_plots_folders[[stock]][year] <- paste0(resampled_results_folders[[stock]][year],
                                                     "/Plots/")
  }
}

# Check that the directories for the resampled results exist
for (stock in 1:N_stock) {
  for(folder in 1:length(resampled_plots_folders[[stock]])) {
    if(!dir.exists(resampled_plots_folders[[stock]][folder])) {
      dir.create(resampled_plots_folders[[stock]][folder], recursive = TRUE)
    } else {
      print("Directories exist")
    }
  }
}

