### Calculating absolute difference between proportion of area from model 
# result estimates and survey data 

# Calculate absolute difference between RE resampled results and the survey data
RE_abs_diff <- list()
for (stock in 1:N_stock) {
  RE_abs_diff[[stock]] <- abs_diff_fun(RE_POA_resampled[[stock]], 
                              survey_data_POA[[stock]], 
                              survey_years[[stock]][[3]], 
                              subregion)
}
names(RE_abs_diff) <- stock_folder_names

# Calculate absolute difference between VAST resampled results and the survey data
VAST_abs_diff <- list()
for (stock in 1:N_stock) {
  VAST_abs_diff[[stock]] <- abs_diff_fun(VAST_POA_resampled[[stock]], 
                                survey_data_POA[[stock]], 
                                survey_years[[stock]][[3]], 
                                subregion)
}
names(VAST_abs_diff) <- stock_folder_names


##### Mean absolute difference calculation #####

# RE 
RE_mean_abs_diff <- as.data.frame(matrix(NA, nrow = N_stock, ncol = N_sub,
                                         dimnames = list(stock_folder_names, subregion)))
for(stock in 1:N_stock) {
  for(sub in 1:N_sub) {
    results <- RE_abs_diff[[stock]]$abs_diff[RE_abs_diff[[stock]]$Subregion == subregion[sub]]
    RE_mean_abs_diff[stock, sub] <- sum(results)/length(results)
  }
}

# VAST 
VAST_mean_abs_diff <- as.data.frame(matrix(NA, nrow = N_stock, ncol = N_sub,
                                           dimnames = list(stock_folder_names, subregion)))
for(stock in 1:N_stock) {
  for(sub in 1:N_sub) {
    results <- VAST_abs_diff[[stock]]$abs_diff[VAST_abs_diff[[stock]]$Subregion == subregion[sub]]
    VAST_mean_abs_diff[stock, sub] <- sum(results)/length(results)
  }
}


# Combining results for each species in order to plot them together
abs_diff_all <- list()
for (stock in 1:N_stock) {
  abs_diff_all[[stock]] <- rbind(VAST_abs_diff[[stock]], RE_abs_diff[[stock]])
  abs_diff_all[[stock]]$model <- c(rep("delta-GLMM results", nrow(VAST_abs_diff[[stock]])), 
                                   rep("RW results", nrow(RE_abs_diff[[stock]])))
}
names(abs_diff_all) <- stock_folder_names
# Melting species in order to plot in the same grid
abs_diff_all_melt <- melt(abs_diff_all, id.vars = colnames(abs_diff_all[[1]]))
colnames(abs_diff_all_melt)[7] <- "Species"


# Formatting mean absolute difference dataframe, with the same column names as
# abs_diff_all_melt, in order to plot them together
RE_mean_abs_diff$Species <- stock_folder_names
RE_mean_abs_diff_melt <- melt(RE_mean_abs_diff)
colnames(RE_mean_abs_diff_melt)[2:3] <- c("Subregion", "abs_diff")
RE_mean_abs_diff_melt$model <- "RW results"

VAST_mean_abs_diff$Species <- stock_folder_names
VAST_mean_abs_diff_melt <- melt(VAST_mean_abs_diff)
colnames(VAST_mean_abs_diff_melt)[2:3] <- c("Subregion", "abs_diff")
VAST_mean_abs_diff_melt$model <- "delta-GLMM results"

mean_abs_diff_all <- rbind(RE_mean_abs_diff_melt, VAST_mean_abs_diff_melt)




