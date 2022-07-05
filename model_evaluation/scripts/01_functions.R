#### Functions for model comparison analysis


## Function to import and format VAST outputs
VAST_import_fun <- function(results_folder, 
                            output_file_name, 
                            subregion_names) {
  VAST_output <- read.csv(paste0(results_folder, "/Plots/", output_file_name)) 
  # Change the automatic column name for subregions
  colnames(VAST_output)[3] <- "Subregion"
  # Change the automatic column name for time
  if(sum(colnames(VAST_output) %in% "Time") > 0) {
    colnames(VAST_output)[which(names(VAST_output) == "Time")] <- "Year"
  }
  # Change the automatic column names for biomass & SE estimates (for either new or old VAST outputs)
  colnames(VAST_output)[which(names(VAST_output) == "Estimate_metric_tons" | names(VAST_output) == "Estimate")] <- "Biomass"
  colnames(VAST_output)[which(names(VAST_output) == "SD_mt" | names(VAST_output) == "Std..Error.for.Estimate")] <- "Biomass_SE"
  # Change automatic numeric values to subregion names
  for(sub in 1:N_sub) {
    VAST_output$Subregion[VAST_output$Subregion == subregion_names[sub]] <- subregion[sub]
  }
  # Setting specific order of subregion factor levels (for plotting)
  VAST_output$Subregion <- factor(VAST_output$Subregion, levels = subregion)
  
  return(VAST_output)
}




## Function to calculate proportion of area and standard error of proportion 
# of area for both RE & VAST results and proportion of area for survey data

POA_and_SE_fun <- function(results_data, 
                    model_type = c("RE", "VAST", "survey")) {
  # Standardizing column names 
  if (model_type == "RE") {
    # colnames(results_data)[which(names(results_data) == "biomA")] <- "Biomass"
    # colnames(results_data)[which(names(results_data) == "yrs")] <- "Year"
    # Calculate SE from the log SE and log biomass columns with lognormal variance equation
    results_data$Biomass_SE <- sqrt((exp(results_data$biomsd.sd^2) - 1)*exp(2*results_data$biomsd + results_data$biomsd.sd^2))
  } else if (model_type == "VAST") {
    # colnames(results_data)[which(names(results_data) == "Estimate_metric_tons")] <- "Biomass"
    # colnames(results_data)[which(names(results_data) == "SD_mt")] <- "Biomass_SE"
  } else {
    colnames(results_data)[which(names(results_data) == "AREA_BIOMASS")] <- "Biomass"
    colnames(results_data)[which(names(results_data) == "YEAR")] <- "Year"
  }

  # Set up year vector & POA dataframe
  years <- sort(unique(results_data$Year))
  POA <- as.data.frame(matrix(NA, nrow = length(years), ncol = 4))
  colnames(POA) <- c(subregion, "Year")
  # Separating out each subregions biomass results
  I_t_subregions <- as.data.frame(matrix(NA, nrow = length(years), ncol = 3))
  colnames(I_t_subregions) <- subregion
  for(sub in 1:N_sub) {
    I_t_subregions[, sub] <- results_data$Biomass[results_data$Subregion == subregion[sub]]
  }
  # Creating vector of biomass sums for each year
  I_t <- vector()
  # Calculating POA for each subregion
  for(year in 1:length(years)) {
    I_t[year] <- sum(I_t_subregions[year, ])
    POA[year, 1] <- I_t_subregions[year, 1]/I_t[year]
    POA[year, 2] <- I_t_subregions[year, 2]/I_t[year]
    POA[year, 3] <- I_t_subregions[year, 3]/I_t[year]
  }
  POA[, 4] <- years
  # Melt dataframe to longer format, with 3 columns (year, subregion, POA)
  POA <- melt(POA, id.vars = "Year")
  colnames(POA)[2:3] <- c("Subregion", "POA")
  # For survey data, stop here and return existing 4 column dataframe
  if (model_type == "survey") {
    return(POA = POA)
  } else { # For RE & VAST results, add SEs to dataframe
    # Separate out biomass standard errors 
    SE_I_t <- as.data.frame(matrix(NA, ncol = 3, nrow = length(years)))
    colnames(SE_I_t) <- subregion
    for(sub in 1:N_sub) {
      SE_I_t[, sub] <- results_data$Biomass_SE[results_data$Subregion == subregion[sub]]
    }
    # Calculate standard errors for proportions for each subregion
    SE_P_t_subregions <- as.data.frame(matrix(NA, nrow = length(years), ncol = 3))
    colnames(SE_P_t_subregions) <- subregion
    # Calculating fourth part of equation first because it requires a different for loop than the others
    part_4 <- vector()
    for(year in 1:length(years)) {
      part_4[year] <- sum(SE_I_t[year, 1]^2, SE_I_t[year, 2]^2, SE_I_t[year, 3]^2)/I_t[year]^2
    }
    # Calculing other 3 parts of equation, then using them to calculate POA SE
    for(sub in 1:N_sub) {
      part_1 <- I_t_subregions[, sub]^2/I_t^2
      part_2 <- SE_I_t[, sub]^2/I_t_subregions[, sub]^2
      part_3 <- 2*(SE_I_t[, sub]^2/(I_t*I_t_subregions[, sub]))
      # Putting all parts together for each subregion
      SE_P_t_subregions[, sub] <- part_1*(part_2 - part_3 + part_4)
    }
    SE_P_t_subregions <- sqrt(SE_P_t_subregions)
    # Melting SE dataframe to long version (year, subregion, SE)
    SE <- melt(SE_P_t_subregions)
    colnames(SE) <- c("Subregion", "SE")
    # Combining POA dataframe with SE dataframe
    POA_and_SE <- cbind(POA, SE[, 2])
    colnames(POA_and_SE)[4] <- "SE"
    
    return(POA_and_SE = POA_and_SE)
  }
}


## Function to calculate absolute difference between resmapled results and 
# survey data

abs_diff_fun <- function(resampled_POA, 
                         survey_POA, 
                         survey_year, 
                         subregions) {
    # Setting up dataframe & year vector
    abs_diff <- as.data.frame(matrix(NA, nrow = nrow(survey_POA), 
                                     ncol = 3))
    colnames(abs_diff) <- c("Year", "Subregion", "diff")
    years <- survey_year
    # Setting year values to numeric 
    #survey_POA$Year <- as.numeric(survey_POA$Year)
    # Vectors to hold POA values for excluded years for each subregion
    west_excl_POA <- vector()
    central_excl_POA <- vector()
    east_excl_POA <- vector()
    # Calculating absolute difference between model estimated & survey POA
    for (year in 1:length(years)) {
      # Isolating each year's results dataset
      year_POA <- resampled_POA[[paste0("X", years[year])]]
      year_POA <- year_POA[year_POA$Year %in% years, ]
      # Extract POA value for excluded year from each corresponding resampled
      # result dataset 
      west_excl_POA[year] <- year_POA$POA[year_POA$Year == years[year] & year_POA$Subregion == subregions[1]]
      central_excl_POA[year] <- year_POA$POA[year_POA$Year == years[year] & year_POA$Subregion == subregions[2]]
      east_excl_POA[year] <- year_POA$POA[year_POA$Year == years[year] & year_POA$Subregion == subregions[3]]
    }
    # Calculating difference between excluded year POAs and survey POA
    abs_diff$diff <- c(west_excl_POA, central_excl_POA, east_excl_POA) - survey_POA$POA
    # Copying over year & subregion columns
    abs_diff[, 1:2] <- year_POA[, 1:2]
    # Creating absolute difference
    abs_diff$abs_diff <- abs(abs_diff$diff)
    # Creating column indicating if difference is positive or negative
    abs_diff$direction <- NA
    for(row in 1:nrow(abs_diff)) {
      if(abs_diff$diff[row] > 0) {
        abs_diff$direction[row] <- "Greater than"
      } else {
        abs_diff$direction[row] <- "Less than"
      }
    }
  return(abs_diff)
}


### Coefficient of variation function
# Using the proportion of area as the mean, and the proportion standard error
# as the standard deviation

CV_fun <- function(mean, sd) {
  CV <- sd/mean
  return(CV)
}

