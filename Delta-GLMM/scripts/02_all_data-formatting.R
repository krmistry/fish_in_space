########## Importing data and creating VAST settings

library(here)
# Setting up main folder path and importing user inputs script
main_folder <- sub("Delta-GLMM", "", here())
source(paste0(main_folder, "00_user_inputs.R"))


# Set up vectors of results & result plots folder names for runs with survey years
initial_results_folders <- results_folder_fun(here("results"), 
                                              stock_folder_names)
initial_plots_folders <- paste0(initial_results_folders, "Plots/")

# Check for the existence of results folders and create if necessary
for(folder in 1:length(initial_plots_folders)) {
  if(!dir.exists(initial_plots_folders[folder])) {
    dir.create(initial_plots_folders[folder], recursive = TRUE)
  } else {
    print("Directories exist")
  }
}

# Set up vectors of results & result plots folder names for runs with survey years &
# dummy data (non-survey interior years and 3 years after last survey year)
all_years_results_folders <- paste0(results_folder_fun(here("results"), 
                                              stock_names), "all_years/")
all_years_plots_folders <- paste0(all_years_results_folders, "Plots/")

# Check for the existence of results folders and create if necessary
for(folder in 1:length(all_years_plots_folders)) {
  if(!dir.exists(all_years_plots_folders[folder])) {
    dir.create(all_years_plots_folders[folder], recursive = TRUE)
  } else {
    print("Directories exist")
  }
}

## VAST input data initial formatting

# Import raw survey haul data
raw_haul <- read.csv(here("data/haul.csv"))
# Metadata for above file:
# $ CRUISEJOIN         : int 
# $ HAULJOIN           : int  
# $ REGION             : chr  "GOA" "GOA" "GOA" "GOA" ...
# $ VESSEL             : int  143 143 143 143 143 143 143 143 143 143 ...
# $ CRUISE             : int  200501 200501 200501 200501 200501 200501 200501 200501 200501 200501 ...
# $ HAUL               : int  1 2 3 4 5 6 7 8 9 10 ...
# $ HAUL_TYPE          : int  3 3 3 3 3 3 3 3 3 3 ...
# $ PERFORMANCE        : num  0 1.11 0 0 0 0 0 0 0 0 ...
# $ START_TIME         : chr  "05/21/05" "05/21/05" "05/22/05" "05/23/05" ...
# $ DURATION           : num  0.281 0.252 0.255 0.266 0.255 0.252 0.256 0.253 0.259 0.253 ...
# $ DISTANCE_FISHED    : num  1.59 1.33 1.42 1.5 1.45 ...
# $ NET_WIDTH          : num  17.5 15.1 16.9 13.7 13.5 ...
# $ NET_MEASURED       : chr  "Y" "Y" "Y" "Y" ...
# $ NET_HEIGHT         : num  7.3 7.29 6.81 8.02 8.01 ...
# $ STRATUM            : int  210 10 111 10 10 111 10 10 310 111 ...
# $ START_LATITUDE     : num  52.6 52.6 52.7 53.2 53.2 ...
# $ END_LATITUDE       : num  52.6 52.6 52.7 53.2 53.2 ...
# $ START_LONGITUDE    : num  -170 -170 -169 -168 -168 ...
# $ END_LONGITUDE      : num  -170 -170 -169 -168 -168 ...
# $ STATIONID          : chr  "6-Mar" "8-Mar" "9-Jul" "23-23" ...
# $ GEAR_DEPTH         : int  212 75 176 25 64 98 71 59 344 118 ...
# $ BOTTOM_DEPTH       : int  219 82 183 33 72 105 79 67 351 125 ...
# $ BOTTOM_TYPE        : int  NA 6 NA 3 3 4 3 3 1 3 ...
# $ SURFACE_TEMPERATURE: num  6.1 5.3 5.3 5.4 5.8 5.7 5.8 5.7 5.2 5.6 ...
# $ GEAR_TEMPERATURE   : num  NA 4.9 4.7 5.2 5.1 4.9 4.9 4.9 4.2 5.1 ...
# $ WIRE_LENGTH        : int  549 320 503 183 274 366 274 274 869 411 ...
# $ GEAR               : int  172 172 172 172 172 172 172 172 172 172 ...
# $ ACCESSORIES        : int  129 129 129 129 129 129 129 129 129 129 ...
# $ SUBSAMPLE          : int  1 1 1 1 1 1 1 1 1 2 ...
# $ ABUNDANCE_HAUL     : chr  "Y" "Y" "Y" "Y" ...
# $ AUDITJOIN          : int 

# Import pre-processed data provided by Cecilia
CPUE_data <- read.csv(here("data/CPUE.csv"))
# Metadata for above file:
# 'data.frame':	1378798 obs. of  16 variables:
#   $ SURVEY         : chr  "GOA" "GOA" "GOA" "GOA" ...
# $ YEAR           : int  2005 2005 2005 2005 2005 2005 2005 2005 2005 2005 ...
# $ CATCHJOIN      : int  NA NA NA NA NA NA NA NA NA NA ...
# $ HAULJOIN       : int  -13266 -13267 -13268 -13269 -13270 -13271 -13272 -13274 -13275 -13276 ...
# $ VESSEL         : int  147 147 147 147 147 147 147 147 147 147 ...
# $ CRUISE         : int  200501 200501 200501 200501 200501 200501 200501 200501 200501 200501 ...
# $ HAUL           : int  68 69 70 71 72 73 74 76 77 78 ...
# $ STRATUM        : int  112 13 13 13 13 12 12 12 12 12 ...
# $ DISTANCE_FISHED: num  1.62 1.59 1.58 1.6 1.55 ...
# $ NET_WIDTH      : num  17.1 14.7 15 15.8 15.6 ...
# $ SPECIES_CODE   : int  30210 30210 30210 30210 30210 30210 30210 30210 30210 30210 ...
# $ WEIGHT         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ NUMBER_FISH    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ EFFORT         : num  0.0277 0.0235 0.0237 0.0253 0.0242 ...
# $ WGTCPUE        : num  0 0 0 0 0 0 0 0 0 0 ...
# $ NUMCPUE        : num  0 0 0 0 0 0 0 0 0 0 ...

#### Spatial & Spatio-temporal VAST settings

# Setting RhoConfig betas: (2,2) for random walk process for both encounter probability 
# & positive catch equations for Pacific Ocean Perch and(2,1) for random for encounter 
# probability and fixed effect for positive catch for Northern Rockfish
beta_setting <- list(c(2, 2), c(2, 1)) 
names(beta_setting) <- stock_folder_names

# Setting RhoConfig epsilons: 2,2 for random walk process for spatiotemporal random effect
# for both encounter probability & positive catch equations
epsilon_setting <- c(2, 2)

# Set up gamma for positive catch, delta model for encounter
dist_setting <- "Gamma"


# Creating list of temporal setting and the distribution settings for VAST input for each species
# with custom function (see 01_functions.R)
temporal_dist_settings <- list()
for(stock in stock_folder_names) {
  temporal_dist_settings[[stock]] <- dist_beta_setting_fun(dist_setting, 
                                                         beta_setting,
                                                         epsilon_setting,
                                                         stock)
}


## Spatial & spatiotemp random effects settings 
FieldConfig <- matrix( c("IID","IID",
                         "IID","IID",
                         "IID","IID"), #"IID" = independent identical distribution
                       ncol=2, nrow=3, 
                       dimnames=list(c("Omega","Epsilon","Beta"), 
                                     c("Component_1","Component_2")) ) 
#omega = spatial factor for encounter[1] & positive catch[2]
#epsilon = spatiotemp factor for encounter[1] & positive catch[2]
#beta = number of factors for intercepts (relevant for multivariate models - factor analysis for years)


# Creating input data files for VAST, filtered by species and species-specific start years 
# (custom function described in 01_function.R script)
Data_Geostat_list <- list()
for (stock in 1:N_stock) {
  Data_Geostat_list[[stock]] <- filter_create_VAST_input(CPUE_data, 
                                           my_stock_ids, 
                                           stock_names[stock],
                                           start_years[stock],
                                           raw_haul)
  # Checking for NAs in the AreaSwept_km2 column (variable necessary for
  # calculating CPUE, so NA values will cause code to fail)
  if(sum(is.na(Data_Geostat_list[[stock]][, "AreaSwept_km2"])) > 0) {
    deleted_rows <- which(is.na(Data_Geostat_list[[stock]][, "AreaSwept_km2"]) == TRUE)
    if (length(deleted_rows) < 10 && length(deleted_rows) > 0) {
      Data_Geostat_list[[stock]] <- Data_Geostat_list[[stock]][-c(deleted_rows),]
    } else if (length(deleted_rows) > 10) {
      stop("More than 10 NAs in AreaSwept_km2 column in Data_Geostat") #it might be important to look at why there might be a lot of NAs before deciding to get rid of the rows
    }
  }
}
names(Data_Geostat_list) <- sub(" ", "_", stock_names)

# Creating list of survey years for each species
survey_years <- list()
for(stock in 1:N_stock) {
  survey_years[[stock]] <- sort(unique(Data_Geostat_list[[stock]]$Year))
}
names(survey_years) <- stock_folder_names


# VAST settings objects for each species
settings <- list()
for(stock in stock_folder_names) {
  settings[[stock]] <- make_settings(Version = "VAST_v12_0_0", #.cpp version, not software #e.g., "VAST_v12_0_0"
                           n_x = 500, #knots aka spatial resolution of our estimates
                           Region = "User", #Region = "gulf_of_alaska" , go to ?make_settings for other built in extrapolation grids
                           purpose = "index2", #changes default settings
                           ObsModel= temporal_dist_settings[[stock]]$obsmodel,
                           ## everything after this is default if you use purpose = "index2"##
                           FieldConfig = FieldConfig, #spatial & spatiotemp random effects 
                           RhoConfig = temporal_dist_settings[[stock]]$RhoConfig, #temporal settings; default is all 0s, but if I specify this it will be changed here
                           strata.limits = strata.limits, #define area that you're producing index for
                           "knot_method" = "grid", #knots in proportion to spatial domain #other option is knot_method="samples"
                           fine_scale = TRUE, #changes the type of interpolation in your extrapolation area
                           bias.correct = TRUE, #corrects the index for nonlinear transformation; I want this for the final version, but I can turn it off while I'm messing with the model so it will run faster
                           use_anisotropy = TRUE) ##correlations decline depend on direction if this argument is TRUE
  
}

## Import extrapolation grid from data folder:
GOAgrid <- read.csv(file = here("data/Extrapolation_Grids", "GOAThorsonGrid_Less700m.csv"))

input_grid <- cbind(Lat = GOAgrid$Lat,
                    Lon = GOAgrid$Lon,
                    Area_km2 = GOAgrid$Shape_Area/1000000)  # Extrapolation grid area is in 
# m^2 & is converted to km^2
gc() 



