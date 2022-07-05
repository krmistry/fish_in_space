##### Importing results and setting up data objects

# Importing functions from VAST and RE model projects 
source("/Users/kellymistry/Desktop/Graduate Work/RE_model_v2/scripts/functions.R")
source("/Users/kellymistry/Desktop/Graduate Work/groundfish_VAST/scripts/01_functions.R")

## Setting up species, year and subregion objects
# species
stock_names <- c("Pacific Ocean Perch" = "Sebastes alutus",
                 "Northern Rockfish" = "Sebastes polyspinis")
#   "Atka Mackerel" = "Pleurogrammus monopterygius")
my_stock_ids <- c("30060",
                  "30420")
#"21921")
names(my_stock_ids) <- stock_names
stock_folder_names <- sub(" ", "_", stock_names)

# Species-specific start and end years
start_years <- c(1990, 
                 1984) 
# 1984)
names(start_years) = stock_names
end_years <- c(2019,
               2019)
# 2019)
names(end_years) = stock_names

# subregions
subregion <- c("WESTERN", "CENTRAL", "EASTERN")

# Number of stocks and subregions
N_stock <- length(stock_names)
N_sub <- length(subregion)

## Import original data sets
# Data set with summarized estimates for each subregion in each year (used with RE model)
GOA_indices_data <- read.csv("/Users/kellymistry/Desktop/Graduate Work/RE_model_v2/data/BIOMASS_AREA_DATA_TABLE.csv")
# Raw data (used in VAST model), with all of the hauls in each year
GOA_haul_data <- read.csv("/Users/kellymistry/Desktop/Graduate Work/groundfish_VAST/data/haul.csv")
GOA_CPUE_data <- read.csv("/Users/kellymistry/Desktop/Graduate Work/groundfish_VAST/data/CPUE.csv")
# Filtering raw data by species and years
GOA_raw_data_list <- list()
for (stock in 1:N_stock) {
  GOA_raw_data_list[[stock]] <- filter_create_VAST_input(GOA_CPUE_data, 
                                                         my_stock_ids, 
                                                         stock_names[stock],
                                                         start_years[stock],
                                                         GOA_haul_data)
  # Checking for NAs in the AreaSwept_km2 column (variable necessary for
  # calculating CPUE, so NA values will cause code to fail)
  if(sum(is.na(GOA_raw_data_list[[stock]][, "AreaSwept_km2"])) > 0) {
    deleted_rows <- which(is.na(GOA_raw_data_list[[stock]][, "AreaSwept_km2"]) == TRUE)
    if (length(deleted_rows) < 10 && length(deleted_rows) > 0) {
      GOA_raw_data_list[[stock]] <- GOA_raw_data_list[[stock]][-c(deleted_rows),]
    } else if (length(deleted_rows) > 10) {
      stop("More than 10 NAs in AreaSwept_km2 column in Data_Geostat") #it might be important to look at why there might be a lot of NAs before deciding to get rid of the rows
    }
  }
  # Adding a column with the subregion for each haul location
  for (i in 1:nrow(GOA_raw_data_list[[stock]])) {
    if (GOA_raw_data_list[[stock]]$Lon[i] <= -157) {
      GOA_raw_data_list[[stock]]$Subregion[i] <-  subregion[1] 
    } else if (GOA_raw_data_list[[stock]]$Lon[i] > -157 && GOA_raw_data_list[[stock]]$Lon[i] <= -147) {
      GOA_raw_data_list[[stock]]$Subregion[i] <-  subregion[2] 
    } else {
      GOA_raw_data_list[[stock]]$Subregion[i] <- subregion[3]
    }
  }
}
names(GOA_raw_data_list) <- stock_folder_names


# Creating list of data frames for each subregion for each species (6 total)
separated_stock_data <- list()
for (stock in 1:N_stock) {
  separated_stock_data[[stock]] <- lapply(subregion, function(sub) {
    initial_set_up_fun(GOA_indices_data, 
                       stock_names[stock], 
                       start_years[stock], 
                       sub)
  })
  names(separated_stock_data[[stock]]) <- subregion
}
names(separated_stock_data) <- stock_folder_names

# Recording survey years for each species
survey_years <- list()
for(stock in stock_folder_names) {
  survey_years[[stock]] <- lapply(subregion, function(sub) {
    separated_stock_data[[stock]][[sub]]$YEAR
  })
  names(survey_years[[stock]]) <- subregion
}

# Creating lists for all years, and then for non-survey years
all_years <- list()
non_survey_yrs <- list()
for(stock in 1:N_stock) {
  all_years[[stock]] <- start_years[stock]:end_years[stock]
  survey_yrs_ind <- which(all_years[[stock]] %in% survey_years[[stock]][[1]])
  non_survey_yrs[[stock]] <- all_years[[stock]][-survey_yrs_ind]
}
names(all_years) <- names(non_survey_yrs) <- stock_folder_names



### Import VAST results

# Beta & epsilon settings
beta_list <- c("beta_2s", "beta_2_1","beta_4s")
beta_num <- beta_list[1]
epsilon_list <- c("epsilons_0", "epsilons_2")
epsilon_num <- epsilon_list[2]

# New VAST subregion names
VAST_sub_names <- paste0("Stratum_", c(1:3))

# New VAST column names: Category, Time, Stratum, Units, Estimate, 
# Std..Error.for.Estimate, Std..Error.for.ln.Estimate.
# Old VAST column names: Year, Estimate_metric_tons, SD_mt

# Old and new names for VAST bias-corrected output file
new_VAST_output_file <- "Index.csv"
old_VAST_output_file <- "Table_for_SS3.csv"

# Create list to hold species VAST results dataframes
# ***** Note: new VAST results (epsilons = 2) have different name for bias-corrected
# .csv file - now its Index.csv, before it was Table_for_SS3.csv
VAST_results_all <- list()
for (stock in 1:N_stock) { 
  # Importing and formatting VAST output file
  VAST_output <- VAST_import_fun(paste0("/Users/kellymistry/Desktop/Graduate Work/groundfish_VAST/results/", 
                                        stock_folder_names[stock], 
                                        "/Gamma/",
                                        beta_num, "/",
                                        epsilon_num, "/just_survey_yrs"), 
                                 new_VAST_output_file, 
                                 VAST_sub_names)
  # Manually excluding non-survey years (since the new version returned 0s for 
  # that instead of NAs) and saving results to list
  VAST_results_all[[stock]] <- VAST_output[VAST_output$Year %in% survey_years[[stock]][[1]],]
}
names(VAST_results_all) <- stock_folder_names


# Import RE results with all data
# Set results folders dataframe up:
RE_results_all <- list()
for (stock in 1:N_stock) {
  sub_results <- lapply(subregion, function(x) {
    # Import raw rwout.rep file
    rwout_path <- paste0("/Users/kellymistry/Desktop/Graduate Work/RE_model_v2/results/", 
                         x, "/", stock_folder_names[stock], "/rwout.rep")
    # Transform it into a dataframe with custom function (see functions.R for details)
    format_RE_output_fun(rwout_path, stock_names[stock])
  })
  names(sub_results) <- subregion
  RE_results_all[[stock]] <- sub_results
  # Reformat above to melt subregion data into a single dataframe for each species in order to plot in the same way as the VAST data
  id_variables <- colnames(RE_results_all[[stock]][[1]])
  RE_results_all[[stock]] <- melt(RE_results_all[[stock]], id.vars = id_variables)
  # Rename yrs, biomA and newly created subregion column names
  colnames(RE_results_all[[stock]])[c(1, 3, 9)] <- c("Year", "Biomass", 
                                                     "Subregion")
}
names(RE_results_all) <- stock_folder_names


# Import resampled RE results
RE_results_resampled <- list()
for (stock in 1:N_stock) {
    year_list <- list()
    years <- survey_years[[stock]][[3]] 
    for(year in 1:length(years)) {
      year_list[[year]] <- lapply(subregion, function(x) {
        # Import raw rwout.rep file
        rwout_path <- paste0("/Users/kellymistry/Desktop/Graduate Work/RE_model_v2/results/Resampling_results/", 
                             stock_folder_names[stock], "/", x, "/", years[year], "/rwout.rep")
        
        # Transform it into a dataframe with custom function (see functions.R for details)
        format_RE_output_fun(rwout_path, stock_names[stock])
      })
      names(year_list[[year]]) <- subregion
      # Reformat above to melt subregion data into a single dataframe for each species in order to plot in the same way as the VAST data
      id_variables <- colnames(year_list[[year]][[1]])
      year_list[[year]] <- melt(year_list[[year]], id.vars = id_variables)
      # Rename yrs, biomA and newly created subregion column names
      colnames(year_list[[year]])[c(1, 3, 9)] <- c("Year", "Biomass", 
                                                         "Subregion")
    }
  names(year_list) <- paste0("X", years)
  RE_results_resampled[[stock]] <- year_list
}
names(RE_results_resampled) <- stock_folder_names



# Creating list of folders to store the names of the VAST resampled folders
VAST_resampled_results_folders <- list()
for (stock in 1:N_stock) {
  species_years <- survey_years[[stock]][[1]]
  VAST_resampled_results_folders[[stock]] <- lapply(species_years, function(x) {
    paste0("/Users/kellymistry/Desktop/Graduate Work/groundfish_VAST/results/Resampled_results/", 
           stock_folder_names[stock], "/Gamma/", beta_num, "/", epsilon_num, "/", x)
  })
  VAST_resampled_results_folders[[stock]] <- unlist(VAST_resampled_results_folders[[stock]])
}
names(VAST_resampled_results_folders) <- stock_folder_names

# Import resampled VAST results
VAST_results_resampled <- list()
for (stock in 1:N_stock) {
  years <- survey_years[[stock]][[1]]
  year_list <- list()
  for (year in 1:length(years)) {
    # Importing and formatting VAST output file
    VAST_output <- VAST_import_fun(paste0("/Users/kellymistry/Desktop/Graduate Work/groundfish_VAST/results/Resampled_results/", 
                                          stock_folder_names[stock], 
                                          "/Gamma/",
                                          beta_num, "/",
                                          epsilon_num, "/",
                                          years[year]), 
                                   new_VAST_output_file, 
                                   VAST_sub_names)
    # Manually excluding non-survey years (since the new version returned 0s for 
    # that instead of NAs) and saving results to list
    year_list[[year]] <- VAST_output[VAST_output$Year %in% survey_years[[stock]][[1]],]
  }
  VAST_results_resampled[[stock]] <- year_list
  names(VAST_results_resampled[[stock]]) <- paste0("X", survey_years[[stock]][[1]])
}
names(VAST_results_resampled) <- stock_folder_names


## Import VAST results with intermediate years & predictions for 2020-2022
###### Until epsilons = 2 results are available, the below is just importing
###### a version with predictions for 2020-2022, not intermediate years
VAST_all_yrs <- list()
for (stock in 1:N_stock) { 
  # Importing and formatting VAST output file
  VAST_all_yrs[[stock]] <- VAST_import_fun(paste0("/Users/kellymistry/Desktop/Graduate Work/groundfish_VAST/results/", 
                                        stock_folder_names[stock], 
                                        "/Gamma/",
                                        beta_num, "/",
                                        epsilon_num, "/all_years"), 
                                 new_VAST_output_file, 
                                 VAST_sub_names)
}
names(VAST_all_yrs) <- stock_folder_names

# Custom colors for plots
subregion_colors <- brewer.pal(3, "Dark2")
names(subregion_colors) <- subregion

model_colors <- c("blue", "red", "black")
names(model_colors) <- c("RW results", "delta-GLMM results", "survey data")
