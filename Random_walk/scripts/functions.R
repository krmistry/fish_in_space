# Function to filter and arrange data by species and subregion & create CV values
initial_set_up_fun <- function(data, species, start_year, subregion) {
  # filtering by species, species-specific start year and subregion, then ordering the dataframe by year
  subregion_data <- data %>% subset(SPECIES_CODE == my_stock_ids[species]
                                    & YEAR >= start_year
                                    & grepl(subregion, REGULATORY_AREA_NAME)) %>% arrange(YEAR)
  
  # check for 0s in the biomass data
  if(sum(isZero(subregion_data$MEAN_WGT_CPUE)) > 0){
    # if there are zeros, identify the indices for the zeros
    zero_ind <- which(isZero(subregion_data$MEAN_WGT_CPUE))
    # replace biomass mean 0s with 0.1 (NOT adding 0.1 to all values, just the zero)
    subregion_data$MEAN_WGT_CPUE[zero_ind] = subregion_data$MEAN_WGT_CPUE[zero_ind] + 0.1
    subregion_data$AREA_BIOMASS[zero_ind] = subregion_data$AREA_BIOMASS[zero_ind] + 0.1
    # replace biomass variance 0s with 0.0001
    subregion_data$VAR_WGT_CPUE[zero_ind] = subregion_data$VAR_WGT_CPUE[zero_ind] + 1e-4
  } # the above prevents the CV from being NA, but since the VAR_WGT_CPUE value is 0, it means CV = 0; the model threw an error with the matrix inverse, so does this mean that the CV can't be 0? Should I artificially give it a value? Or give VAR_WGT_CPUE a value?
  
  # calculating CV values
  subregion_data$BIOMASS_CV <-  (sqrt(subregion_data$VAR_WGT_CPUE)/subregion_data$MEAN_WGT_CPUE) # might be wrong right now, I need to ask Cecilia which columns this should be
  return(subregion_data)
}


# Function to create and organize the data that goes into the re.dat file
re.dat_inputs_fun <- function(data, start_year, end_year) {
  input.yrs <- data$YEAR
  input.idx <- data$AREA_BIOMASS 
  input.cv <- data$BIOMASS_CV
  #DateFile <- re.dat_directory
  styr <- start_year  #Start year
  endyr <- end_year  #End year
  nobs <- length(input.yrs)  #Number of surveys
  yrs_srv <- input.yrs  #Survey years
  srv_est <- input.idx  #Survey Biomass
  srv_cv <- input.cv  #Survey Biomass CV
  return(list(styr, endyr, nobs, yrs_srv, srv_est, srv_cv))
}


# Function to read the RE output object rwout.rep and format it into a usable
# dataframe; any value other than "0" for year returns the results when that 
# year was excluded
format_RE_output_fun <- function(rwout_path, 
                                 stock_name) {
  # Import raw results
  raw_results <- read.delim(rwout_path)
  # Creating dataframe for results, using column names from rwout.rep file
  col_names <- c(unlist(filter(raw_results, row_number() %% 2 == 0)), 
                 use.names = FALSE)[-c(1,2)]
  RE_results <- as.data.frame(matrix(NA, 
                                     nrow = length(seq(start_years[stock_name], 
                                                       end_years[stock_name])), 
                                     ncol = length(col_names)))
  names(RE_results) <- col_names
  # Populate dataframe with results data
  for(i in 1:length(col_names)) {
    row <- seq(7, nrow(raw_results), 2)[i]
    RE_results[, i] <-  as.numeric(unlist(strsplit(raw_results[row, 1], 
                                                   split = " "))[-1])
  }
  return(RE_results)
}



# Function to plot results from all data (red line) vs all of the resampled 
# results (black)
basic_plot_fun <- function(all_data, 
                           resampled_data, 
                           survey_data, 
                           survey_years, 
                           stock, 
                           subregion) {
  # Reforming resampled data list into a long dataframe, and adding in the 
  # all_data dataset
  result_data <- melt(resampled_data)
  colnames(result_data)[9] <- "missing_year"
  result_data$missing_year <- sub("X", "", result_data$missing_year)
  all_data$missing_year <- "all_data"
  result_data <- rbind(result_data, all_data)
  
  # Creating ymin and ymax to ensure the plot will be big enough
  y_max <- ceiling(max(as.numeric(result_data$biomA),
                       survey_data$AREA_BIOMASS))
  y_min <- floor(min(as.numeric(result_data$biomA),
                     survey_data$AREA_BIOMASS))
  
  # Creating custom color palette
  myColors <- c(colorRampPalette(brewer.pal(8, "Set2"))(length(survey_years)),
                "black")
  names(myColors) <- c(survey_years, "all_data")
  
  # Creating plot
  plot <- ggplot() +
    ylim(y_min, y_max) +
    labs(title = paste0(stock, " - ", subregion), 
         x = "Years", y = "Estimated biomass") +  
    geom_line(data = result_data, 
              aes(x = yrs, 
                  y = biomA,
                  color = missing_year)) +
    scale_color_manual(name = "Missing year\n in resampled data", 
                       values = myColors) +
    geom_point(data = survey_data, aes(x = YEAR, y = AREA_BIOMASS, 
                                       fill = "Survey data"), 
               shape = 20,  color = "blue", size = 2) +
    scale_fill_manual(name = "", values = c("Survey data" = "blue")) +
    geom_line(data = all_data, 
              aes(x = yrs, y = biomA), 
              color = "black") 
  return(plot)
}

# test <- melt(resampled_data)
# colnames(test)[9] <- "missing_year"
# test$missing_year <- sub("X", "", test$missing_year)
# 
# all_data$missing_year <- "all_data"
# 
# test <- rbind(test, all_data)
# 
# myColors <- c(colorRampPalette(brewer.pal(8, "Set2"))(length(survey_years)),
#               "black")
# names(myColors) <- c(survey_years, "all_data")
# 
# 
# plot <- ggplot() +
#   ylim(y_min, y_max) +
#   labs(title = paste0(stock, " - ", subregion), 
#        x = "Years", y = "Estimated biomass") +  
#   geom_line(data = test, 
#             aes(x = as.numeric(yrs), 
#                 y = as.numeric(biomA),
#                 color = missing_year)) +
#   scale_color_manual(name = "Missing year\n in resampled data", 
#                      values = myColors) +
#   geom_point(data = survey_data, aes(x = YEAR, y = AREA_BIOMASS, 
#                                      fill = "Survey data"), 
#              shape = 20,  color = "blue", size = 2) +
#   scale_fill_manual(name = "", values = c("Survey data" = "blue")) +
#   geom_line(data = all_data, 
#            aes(x = as.numeric(yrs), y = as.numeric(biomA)), 
#            color = "black") 



