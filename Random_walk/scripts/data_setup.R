# Importing data
GOA_indices_data <- read.csv(here("data/BIOMASS_AREA_DATA_TABLE.csv"))
# species 
stock_names <- c("Pacific Ocean Perch" = "Sebastes alutus", 
                 "Northern Rockfish" = "Sebastes polyspinis") 
                 #"Atka Mackerel" = "Pleurogrammus monopterygius")
my_stock_ids <- c("30060", 
                  "30420") 
                  #"21921")
names(my_stock_ids) <- stock_names
stock_folder_names <- sub(" ", "_", stock_names)

# Species-specific start years & most recent survey year
start_years <- c(1990, 
                 1984) 
                 #1984)
names(start_years) = stock_names
end_years <- c(2019,
               2019)
               #2019)
names(end_years) = stock_names

# # Creating a vector with the number of years each species is estimated for 
# # (this will be useful later on, when formatting the result outputs)
# stock_years <- sapply(c(1,2), function(x) {
#   end_years[x] - start_years[x] + 1
# })
# names(stock_years) <- stock_names

# subregions
subregion <- c("WESTERN", "CENTRAL", "EASTERN")

# Number of stocks and number of subregions (for looping) 
N_sub <- length(subregion)
N_stock <- length(stock_names)

# results folder names (formatted like this in order to make nested for loops work)
results_folders <- matrix(NA, nrow = 2, ncol = 3, 
                          dimnames = list(stock_folder_names, subregion))

# Rows are for each stock, columns for subregions
for (sub in 1:N_sub) {
  for (stock in 1:N_stock) {
    results_folders[stock, sub] <- paste0(here("results/"), subregion[sub], "/", stock_folder_names[stock])
  }
}


# # Creating separate data frames for each subregion for each species (6 total)
# for(stock in 1:length(stock_names)) {
#   for(sub in 1:length(subregion)) {
#     assign(paste0(subregion[sub], "_", stock_folder_names[stock],"_data"),
#            initial_set_up_fun(GOA_indices_data, stock_names[stock], start_years[stock], subregion[sub]))
#   }
# }

# alternative method to the above nested for loops; results in a list of lists, with the 
# upper level being by subregion and the second list by stock
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

# Creating list of survey years for each species and subregion; in my data set
# the eastern subregion wasn't surveyed in 2001 for either species, so there
# is one less survey year
survey_years <- list()
for(stock in 1:N_stock) {
  survey_years[[stock]] <- lapply(subregion, function(sub) {
      separated_stock_data[[stock]][[sub]]$YEAR
  })
  names(survey_years[[stock]]) <- subregion
}

names(survey_years) <- stock_folder_names


# Creating and saving re.dat files (input for RE model) for each subregion and each 
# species in the appropriate results folder; for description of content of re.dat, see notes 
# for re.dat_inputs_fun() in functions.R
for (sub in 1:N_sub) {
  for (stock in 1:N_stock) {
    # Creating re.dat object
    re.dat <- re.dat_inputs_fun(separated_stock_data[[stock]][[sub]], 
                                start_years[stock_names[stock]], 
                                end_years[stock_names[stock]])
    # Saving as re.dat into appropriate results folder
    write_dat(paste0(results_folders[stock, sub], "/re"), 
              L = re.dat)
  }
}


### COPY IN .tpl to each of the subregion/species folders (from the master copy in the project folder) ###
for (i in 1:(N_sub*N_stock)) {
  file.copy(from = here("re.tpl"), 
            to = as.list(results_folders)[[i]], 
            overwrite = TRUE)
}


# ADMB output objects that are getting saved into the main project folder instead of the 
# appropriate results folder and therefore need to be moved
result_files <- c("rwout.rep", 
                  "admodel.cov", 
                  "admodel.dep", 
                  "admodel.hes", 
                  "hesscheck", 
                  "hessian.bin", 
                  "fmin.log")

