### Calculating proportion of error and the associated standard errors for 
# both model results and just proportion of area for survey data

# RE with all data
RE_POA <- list()
for(stock in 1:N_stock) {
  RE_POA[[stock]] <- POA_and_SE_fun(RE_results_all[[stock]], model_type = "RE")
}
names(RE_POA) <- stock_folder_names

# RE with resampled data
RE_POA_resampled <- list()
for (stock in 1:N_stock) {
  years <- names(RE_results_resampled[[stock]])
  POA_list <- list()
  for(year in 1:length(years)) {
    POA_list[[year]] <- POA_and_SE_fun(RE_results_resampled[[stock]][[year]], 
                                model_type = "RE")
  }
  RE_POA_resampled[[stock]] <- POA_list
  names(RE_POA_resampled[[stock]]) <- years
}
names(RE_POA_resampled) <- stock_folder_names

# VAST with all data & only survey years
VAST_POA <- list()
for (stock in 1:N_stock) {
  VAST_POA[[stock]] <- POA_and_SE_fun(VAST_results_all[[stock]], 
                               model_type = "VAST")
}
names(VAST_POA) <- stock_folder_names


# VAST with all data & all years
VAST_POA_all_yrs <- list()
for (stock in stock_folder_names) {
  VAST_POA_all_yrs[[stock]] <- POA_and_SE_fun(VAST_all_yrs[[stock]], 
                                      model_type = "VAST")
}
# names(VAST_POA) <- stock_folder_names

# VAST with resampled data
VAST_POA_resampled <- list()
for(stock in 1:N_stock) {
  year_names <- names(VAST_results_resampled[[stock]])
  POA_list <- list()
  for(year in 1:length(year_names)) {
    POA_list[[year]] <- POA_and_SE_fun(VAST_results_resampled[[stock]][[year_names[year]]], 
                                model_type = "VAST")
  }
  names(POA_list) <- year_names
  VAST_POA_resampled[[stock]] <- POA_list
}
names(VAST_POA_resampled) <- stock_folder_names


# Survey data, excluding 2001 (eastern survey didn't happen that year)
survey_data_POA <- list()
for (stock in 1:N_stock) {
  # Melting subregion dataframes to create a single results dataframe
  id_variables <- colnames(separated_stock_data[[stock]][[1]])
  alt_survey_data <- melt(separated_stock_data[[stock]], 
                          id.vars = id_variables)
  colnames(alt_survey_data)[20] <- "Subregion"
  # Excluding 2001, because the 2001 survey missed the eastern subregion
  alt_survey_data <- alt_survey_data[alt_survey_data$YEAR != 2001,]
  # Calculating POA
  survey_data_POA[[stock]] <- POA_and_SE_fun(alt_survey_data, 
                                      model_type = "survey")
}
names(survey_data_POA) <- stock_folder_names


### Combining RE, VAST and survey POA dataframes for plotting purposes 

# Version with all years (can exclude non-survey years later if needed)
Combined_POA <- list()
for(stock in 1:N_stock) {
  # # Excluding non-survey years from RE POA before combining & adding data type 
  # # column
  # RE_POA_survey_yrs <- RE_POA[[stock]][RE_POA[[stock]]$Year %in% survey_years[[stock]][[3]], ]
  # RE_POA_survey_yrs$data_type <- "RW results"
  # # Excluding 2001 from VAST POA before combining, since its missing from survey
  # # and adding data type column
  # VAST_POA_survey_yrs <- VAST_POA[[stock]][VAST_POA[[stock]]$Year %in% survey_years[[stock]][[3]],  ]
  # VAST_POA_survey_yrs$data_type <- "delta-GLMM results"
  
  # Adding data type to both model results
  RE_POA_all <- RE_POA[[stock]]
  RE_POA_all$data_type <- "RW results"
  VAST_POA_all <- VAST_POA_all_yrs[[stock]]
  VAST_POA_all$data_type <- "delta-GLMM results"
  # Excluding 2020 - 2022 estimates from VAST
  VAST_POA_all <- VAST_POA_all[VAST_POA_all$Year != 2020:2022,]

  # Adding data type and SE columns to survey data POA
  survey_POA <- survey_data_POA[[stock]]
  survey_POA$SE <- 0
  survey_POA$data_type <- "survey data"
  
  
  ## Combine RE, VAST and survey into one long dataframe for plotting
  POA_long_df <- rbind(RE_POA_all, VAST_POA_all, survey_POA)
  
  # Setting up upper and lower confidence intervals, with 0 and 1 set as the 
  # upper and lower limits to keep the plot from going outside those bounds and
  # otherwise its POA +/- 2*SE
  for(i in 1:nrow(POA_long_df)) {
    POA_long_df$POA_LCI[i] <- max((POA_long_df$POA - 2*POA_long_df$SE)[i], 0)
    POA_long_df$POA_UCI[i] <- min((POA_long_df$POA + 2*POA_long_df$SE)[i], 1)
  }
  Combined_POA[[stock]] <- POA_long_df
}
names(Combined_POA) <- stock_folder_names



### Separating out POA estimates from excluded years from resampled results into
### its own dataframe (for bias plots)

# Both RE & VAST in one for loop
Excl_year_POA <- list()
for (stock in 1:N_stock) {
  RE_excl_year_estimates <- as.data.frame(matrix(NA, 
                                                 nrow = length(survey_years[[stock]][[3]]), 
                                                 ncol = 4))
  colnames(RE_excl_year_estimates) <- c(subregion, "Year")
  RE_excl_year_estimates$Year <- survey_years[[stock]][[3]]
  VAST_excl_year_estimates <- as.data.frame(matrix(NA, 
                                                   nrow = length(survey_years[[stock]][[3]]), 
                                                   ncol = 4))
  colnames(VAST_excl_year_estimates) <- c(subregion, "Year")
  VAST_excl_year_estimates$Year <- survey_years[[stock]][[3]]
  # Extracting excluded year POA estimates
  for(year in 1:length(survey_years[[stock]][[3]])) {
    RE_year_results <- RE_POA_resampled[[stock]][[year]]
    VAST_year_results <- VAST_POA_resampled[[stock]][[year]]
    for(sub in 1:N_sub) {
      RE_excl_year_estimates[year, sub] <- RE_year_results$POA[RE_year_results$Year == survey_years[[stock]][[3]][year] & RE_year_results$Subregion == subregion[sub]]
      VAST_excl_year_estimates[year, sub] <- VAST_year_results$POA[VAST_year_results$Year == survey_years[[stock]][[3]][year] & VAST_year_results$Subregion == subregion[sub]]
    }
  }
  RE_excl_year_estimates$model_type <- "RW results"
  RE_excl_year_estimates <- melt(RE_excl_year_estimates, id.vars = c("Year", "model_type"))
  colnames(RE_excl_year_estimates)[3:4] <- c("Subregion", "estimated_POA")
  RE_excl_year_estimates$survey_POA <- survey_data_POA[[stock]]$POA
  
  VAST_excl_year_estimates$model_type <- "delta-GLMM results"
  VAST_excl_year_estimates <- melt(VAST_excl_year_estimates, id.vars = c("Year", "model_type"))
  colnames(VAST_excl_year_estimates)[3:4] <- c("Subregion", "estimated_POA")
  VAST_excl_year_estimates$survey_POA <- survey_data_POA[[stock]]$POA
  
  model_excl_year_estimates <- rbind(RE_excl_year_estimates, VAST_excl_year_estimates)
  Excl_year_POA[[stock]] <- model_excl_year_estimates
}
names(Excl_year_POA) <- stock_folder_names


## Calculating POA estimates for 2020-2022 predictions for VAST
pred_POA <- list()
for(stock in 1:N_stock) {
  # Extracting 2020 - 2022 predictions
  pred_VAST <- VAST_all_yrs[[stock]][VAST_all_yrs[[stock]]$Year %in% c(2020:2022),]
  # Calculate POA for these predictions
  pred_POA[[stock]] <- POA_and_SE_fun(pred_VAST, "VAST")
}
names(pred_POA) <- stock_folder_names


