#### Calculate coefficient of variation for model estimates


# Calculate CV for full data set results
# RE
RE_CV <- list()
for(stock in 1:N_stock) {
  CV_df <- as.data.frame(matrix(NA, nrow = nrow(RE_POA[[stock]]), ncol = 3))
  # Copying year and subregion columns from POA dataframe
  CV_df[, c(1:2)] <- RE_POA[[stock]][, c(1:2)]
  # Creating CVs
  CV_df[, 3] <- CV_fun(RE_POA[[stock]]$POA, RE_POA[[stock]]$SE)
  colnames(CV_df) <- c("Year", "Subregion", "CV")
  RE_CV[[stock]] <- CV_df
}
names(RE_CV) <- stock_folder_names

# VAST
VAST_CV <- list()
for(stock in 1:N_stock) {
  CV_df <- as.data.frame(matrix(NA, nrow = nrow(VAST_POA[[stock]]), ncol = 3))
  # Copying year and subregion columns from POA dataframe
  CV_df[, c(1:2)] <- VAST_POA[[stock]][, c(1:2)]
  # Creating CVs
  CV_df[, 3] <- CV_fun(VAST_POA[[stock]]$POA, VAST_POA[[stock]]$SE)
  colnames(CV_df) <- c("Year", "Subregion", "CV")
  VAST_CV[[stock]] <- CV_df
}
names(VAST_CV) <- stock_folder_names


### Calculating CV for RE resampled results - just excluded year in each replicate
RE_resampled_CV <- list()
for(stock in 1:N_stock) {
  # Isolate survey years as possible missing years
  years <- survey_years[[stock]][[3]]
  CV_df <- as.data.frame(matrix(NA,
                                nrow = (length(years)*3),
                                ncol = 3))
  colnames(CV_df) <- c("Year", "Subregion", "CV")
  # Adding subregions in the order they'll appear when the missing years are isolated
  CV_df[, 2] <- rep(subregion, length(years))
  # Empty vector to store CVs for each loop
  CV_col <- vector()
  for(year in 1:length(years)) {
    # Separating out the missing year for each replicate (3 rows in each year)
    missing_year <- RE_POA_resampled[[stock]][[year]][RE_POA_resampled[[stock]][[year]]$Year == years[year], ]
    # Calculate CVs for missing year
    CV_col <- c(CV_col, CV_fun(missing_year$POA, missing_year$SE))
    # Adding years in the order they'll appear when isolating missing year
    CV_df[c((year*3-2):(year*3)), 1] <- missing_year[, 1]
  }
  CV_df[, 3] <- CV_col
  # Adding model type column for plotting
  CV_df$model_type <- "RW results"
  RE_resampled_CV[[stock]] <- CV_df
}
names(RE_resampled_CV) <- stock_folder_names

### Calculating CV for VAST resampled results
VAST_resampled_CV <- list()
for(stock in 1:N_stock) {
  # Isolate survey years as possible missing years
  years <- survey_years[[stock]][[3]]
  CV_df <- as.data.frame(matrix(NA,
                                nrow = (length(years)*3),
                                ncol = 3))
  colnames(CV_df) <- c("Year", "Subregion", "CV")
  # Adding subregions in the order they'll appear when the missing years are isolated
  CV_df[, 2] <- rep(subregion, length(years))
  # Empty vector to store CVs for each loop
  CV_col <- vector()
  for(year in 1:length(years)) {
    # Separating out the missing year for each replicate (3 rows in each year)
    missing_year <- VAST_POA_resampled[[stock]][[year]][VAST_POA_resampled[[stock]][[year]]$Year == years[year], ]
    # Calculate CVs for missing year
    CV_col <- c(CV_col, CV_fun(missing_year$POA, missing_year$SE))
    # Adding years in the order they'll appear when isolating missing year
    CV_df[c((year*3-2):(year*3)), 1] <- missing_year[, 1]
  }
  CV_df[, 3] <- CV_col
  # Adding model type column for plotting
  CV_df$model_type <- "delta-GLMM results"
  VAST_resampled_CV[[stock]] <- CV_df
}
names(VAST_resampled_CV) <- stock_folder_names


### Calculating difference between CV with all data and CV with excluded 
### year

## resampled CV - all data CV; positive value means resampled CV is bigger
# For RE
RE_CV_diff <- list()
for(stock in 1:N_stock) {
  # Create dataframe to hold difference results
  CV_abs_diff <- as.data.frame(matrix(NA, nrow = nrow(RE_resampled_CV[[stock]]), 
                                      ncol = 3))
  colnames(CV_abs_diff) <- c("Year", "Subregion", "diff")
  # Year and subregion columns match resampled CV (so only survey years)
  CV_abs_diff[,c(1:2)] <- RE_resampled_CV[[stock]][, c(1:2)]
  # Reorder all data CV dataframe to have the years clustered, same as resampled CV dataframe
  all_data_CV <- RE_CV[[stock]][order(RE_CV[[stock]]$Year),]
  # Calculate difference: resampled CV - all data CV (just survey years w/out 2001)
  CV_abs_diff$diff <- RE_resampled_CV[[stock]]$CV - all_data_CV$CV[all_data_CV$Year %in% survey_years[[stock]][[3]]]
  # Setting model_type for later melting
  CV_abs_diff$model_type <- "RW results"
  # Saving to species list
  RE_CV_diff[[stock]] <- CV_abs_diff
}
names(RE_CV_diff) <- stock_folder_names

# For VAST:
VAST_CV_diff <- list()
for(stock in 1:N_stock) {
  # Create dataframe to hold difference results
  CV_abs_diff <- as.data.frame(matrix(NA, nrow = nrow(VAST_resampled_CV[[stock]]), 
                                      ncol = 3))
  colnames(CV_abs_diff) <- c("Year", "Subregion", "diff")
  # Year and subregion columns match resampled CV (so only survey years)
  CV_abs_diff[,c(1:2)] <- VAST_resampled_CV[[stock]][, c(1:2)]
  # Reorder all data CV dataframe to have the years clustered, same as resampled CV dataframe
  all_data_CV <- VAST_CV[[stock]][order(VAST_CV[[stock]]$Year),]
  # Calculate difference: resampled CV - all data CV (just survey years w/out 2001)
  CV_abs_diff$diff <- VAST_resampled_CV[[stock]]$CV - all_data_CV$CV[all_data_CV$Year %in% survey_years[[stock]][[3]]]
  # Setting model_type for later melting
  CV_abs_diff$model_type <- "delta-GLMM results"
  # Saving to species list
  VAST_CV_diff[[stock]] <- CV_abs_diff
}
names(VAST_CV_diff) <- stock_folder_names

### Calculating ratio between CV with all data and CV with excluded 
### year - will likely replace CV difference as a metric

## resampled CV - all data CV; positive value means resampled CV is bigger
# For RE
RE_CV_ratio <- list()
for(stock in 1:N_stock) {
  # Create dataframe to hold difference results
  CV_ratio <- as.data.frame(matrix(NA, nrow = nrow(RE_resampled_CV[[stock]]), 
                                      ncol = 3))
  colnames(CV_ratio) <- c("Year", "Subregion", "ratio")
  # Year and subregion columns match resampled CV (so only survey years)
  CV_ratio[,c(1:2)] <- RE_resampled_CV[[stock]][, c(1:2)]
  # Reorder all data CV dataframe to have the years clustered, same as resampled CV dataframe
  all_data_CV <- RE_CV[[stock]][order(RE_CV[[stock]]$Year),]
  # Calculate difference: resampled CV - all data CV (just survey years w/out 2001)
  CV_ratio$ratio <- RE_resampled_CV[[stock]]$CV/all_data_CV$CV[all_data_CV$Year %in% survey_years[[stock]][[3]]]
  # Setting model_type for later melting
  CV_ratio$model_type <- "RW results"
  # Saving to species list
  RE_CV_ratio[[stock]] <- CV_ratio
}
names(RE_CV_ratio) <- stock_folder_names

# For VAST:
VAST_CV_ratio <- list()
for(stock in 1:N_stock) {
  # Create dataframe to hold difference results
  CV_ratio <- as.data.frame(matrix(NA, nrow = nrow(VAST_resampled_CV[[stock]]), 
                                      ncol = 3))
  colnames(CV_ratio) <- c("Year", "Subregion", "ratio")
  # Year and subregion columns match resampled CV (so only survey years)
  CV_ratio[,c(1:2)] <- VAST_resampled_CV[[stock]][, c(1:2)]
  # Reorder all data CV dataframe to have the years clustered, same as resampled CV dataframe
  all_data_CV <- VAST_CV[[stock]][order(VAST_CV[[stock]]$Year),]
  # Calculate difference: resampled CV - all data CV (just survey years w/out 2001)
  CV_ratio$ratio <- VAST_resampled_CV[[stock]]$CV/all_data_CV$CV[all_data_CV$Year %in% survey_years[[stock]][[3]]]
  # Setting model_type for later melting
  CV_ratio$model_type <- "delta-GLMM results"
  # Saving to species list
  VAST_CV_ratio[[stock]] <- CV_ratio
}
names(VAST_CV_ratio) <- stock_folder_names

## Plotting the distributions of CVs 

# Combining RE and VAST CVs for plotting
Combined_CVs_resampled <- rbind(melt(RE_resampled_CV, id.vars = colnames(RE_resampled_CV[[1]])),
                                melt(VAST_resampled_CV, id.vars = colnames(VAST_resampled_CV[[1]])))
colnames(Combined_CVs_resampled)[5] <- "Species"
Combined_CVs_resampled$Subregion <- factor(Combined_CVs_resampled$Subregion,
                                           levels = subregion)

# Combining RE and VAST CV difference dataframes for plotting
Combined_CV_diff <- rbind(melt(RE_CV_diff, id.vars = colnames(RE_CV_diff[[1]])),
                          melt(VAST_CV_diff, id.vars = colnames(VAST_CV_diff[[1]])))
colnames(Combined_CV_diff)[5] <- "Species"
Combined_CV_diff$Subregion <- factor(Combined_CV_diff$Subregion,
                                           levels = subregion)

# Combining RE and VAST CV ratio dataframes for plotting
Combined_CV_ratio <- rbind(melt(RE_CV_ratio, id.vars = colnames(RE_CV_ratio[[1]])),
                          melt(VAST_CV_ratio, id.vars = colnames(VAST_CV_ratio[[1]])))
colnames(Combined_CV_ratio)[5] <- "Species"
Combined_CV_ratio$Subregion <- factor(Combined_CV_ratio$Subregion,
                                     levels = subregion)

### Calculate mean CV for POP in each subregion for RE & VAST
## RE
RE_mean_CV <- as.data.frame(matrix(NA, nrow = N_stock, ncol = N_sub,
                                   dimnames = list(stock_folder_names, subregion)))
for(stock in 1:N_stock) {
  for(sub in 1:N_sub) {
    data <- RE_resampled_CV[[stock]]$CV[RE_resampled_CV[[stock]]$Subregion == subregion[sub]]
    RE_mean_CV[stock, sub] <- sum(data)/length(data)
  }
}

## VAST
VAST_mean_CV <- as.data.frame(matrix(NA, nrow = N_stock, ncol = N_sub,
                                     dimnames = list(stock_folder_names, subregion)))
for(stock in 1:N_stock) {
  for(sub in 1:N_sub) {
    data <- VAST_resampled_CV[[stock]]$CV[VAST_resampled_CV[[stock]]$Subregion == subregion[sub]]
    VAST_mean_CV[stock, sub] <- sum(data)/length(data)
  }
}

# Formatting mean CV dataframe, with the same column names as
# CV_all_melt, in order to plot them together
RE_mean_CV$Species <- stock_folder_names
RE_mean_CV_melt <- melt(RE_mean_CV)
colnames(RE_mean_CV_melt)[2:3] <- c("Subregion", "CV")
RE_mean_CV_melt$model_type <- "RW"

VAST_mean_CV$Species <- stock_folder_names
VAST_mean_CV_melt <- melt(VAST_mean_CV)
colnames(VAST_mean_CV_melt)[2:3] <- c("Subregion", "CV")
VAST_mean_CV_melt$model_type <- "delta-GLMM"

mean_CV_all <- rbind(RE_mean_CV_melt, VAST_mean_CV_melt)


### Contextualizing CV values: quantify error to SE i.e. absolute difference/SE:

# Calculating abs_diff/SE from excluded year for RE
RE_error <- list()
RE_excluded_SEs <- list()
for(stock in stock_folder_names) {
  excluded_SEs <- list()
  for(sub in subregion) {
    rep_year <- survey_years[[stock]][[3]]
    # Create vector to hold SEs from excluded year in each replicate
    excluded_SEs[[sub]] <- vector()
    for(years in 1:length(rep_year)) {
      excluded_SEs[[sub]][years] <- RE_POA_resampled[[stock]][[years]]$SE[RE_POA_resampled[[stock]][[years]]$Year == rep_year[years] & RE_POA_resampled[[stock]][[years]]$Subregion == sub]
    }
    excluded_SEs[[sub]] <- as.data.frame(cbind(rep_year, excluded_SEs[[sub]]))
    colnames(excluded_SEs[[sub]]) <- c("Year", "excluded_SE")
  }
  excluded_SEs_all <- melt(excluded_SEs, id.vars = colnames(excluded_SEs[[1]]))
  colnames(excluded_SEs_all)[3] <- "Subregion"
  RE_excluded_SEs[[stock]] <- excluded_SEs_all 
  
  RE_error[[stock]] <- as.data.frame(matrix(NA, nrow = 3*length(rep_year), ncol = 4))
  colnames(RE_error[[stock]]) <- c("Year", "Subregion", "Error", "model_type")
  RE_error[[stock]][, 1:2] <- RE_abs_diff[[stock]][, 1:2]
  RE_error[[stock]][, 3] <- RE_abs_diff[[stock]]$abs_diff/excluded_SEs_all$excluded_SE
  RE_error[[stock]]$model_type <- "RW results"
}


# Calculating abs_diff/SE from excluded year for RE
VAST_error <- list()
VAST_excluded_SEs <- list()
for(stock in stock_folder_names) {
  excluded_SEs <- list()
  for(sub in subregion) {
    rep_year <- survey_years[[stock]][[3]]
    # Create vector to hold SEs from excluded year in each replicate
    excluded_SEs[[sub]] <- vector()
    for(years in 1:length(rep_year)) {
      excluded_SEs[[sub]][years] <- VAST_POA_resampled[[stock]][[years]]$SE[VAST_POA_resampled[[stock]][[years]]$Year == rep_year[years] & VAST_POA_resampled[[stock]][[years]]$Subregion == sub]
    }
    excluded_SEs[[sub]] <- as.data.frame(cbind(rep_year, excluded_SEs[[sub]]))
    colnames(excluded_SEs[[sub]]) <- c("Year", "excluded_SE")
  }
  excluded_SEs_all <- melt(excluded_SEs, id.vars = colnames(excluded_SEs[[1]]))
  colnames(excluded_SEs_all)[3] <- "Subregion"
  VAST_excluded_SEs[[stock]] <- excluded_SEs_all 

  VAST_error[[stock]] <- as.data.frame(matrix(NA, nrow = 3*length(rep_year), ncol = 4))
  colnames(VAST_error[[stock]]) <- c("Year", "Subregion", "Error", "model_type")
  VAST_error[[stock]][, 1:2] <- VAST_abs_diff[[stock]][, 1:2]
  VAST_error[[stock]][, 3] <- VAST_abs_diff[[stock]]$abs_diff/excluded_SEs_all$excluded_SE
  VAST_error[[stock]]$model_type <- "delta-GLMM results"
}

Combined_error <- list()
for(stock in stock_folder_names) {
  Combined_error[[stock]] <- rbind(RE_error[[stock]], VAST_error[[stock]])
}

All_error <- melt(Combined_error, id.vars = colnames(Combined_error[[1]]))
colnames(All_error)[5] <- "Species"


### Experimenting with coverage metric
# diff/SE
diff_div_SE <- list()
for(stock in stock_folder_names) {
  diff_div_SE[[stock]] <- list()
  RE <- RE_abs_diff[[stock]]$diff/RE_excluded_SEs[[stock]]$excluded_SE
  VAST <- VAST_abs_diff[[stock]]$diff/VAST_excluded_SEs[[stock]]$excluded_SE
  diff_div_SE[[stock]]$diff_div_SE <- c(RE, VAST)
  diff_div_SE[[stock]]$pnorm <- pnorm(diff_div_SE[[stock]]$diff_div_SE)
  diff_div_SE[[stock]]$model_type <- c(rep("RW results", length(RE)), rep("delta-GLMM results", length(VAST)))
  diff_div_SE[[stock]] <- as.data.frame(diff_div_SE[[stock]])
  diff_div_SE[[stock]]$Year <- rep(RE_excluded_SEs[[stock]]$Year, 2)
  diff_div_SE[[stock]]$Subregion <- rep(RE_excluded_SEs[[stock]]$Subregion, 2)
}
all_diff_div_SE <- melt(diff_div_SE, id.vars = colnames(diff_div_SE[[1]]))
colnames(all_diff_div_SE)[6] <- "Species"

  
  

