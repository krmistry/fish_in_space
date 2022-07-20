################# Set user inputs ######################33
# 
# # Set stock names & stock ID number
# stock_names <- c("Pacific Ocean Perch" = "Sebastes alutus", 
#                  "Northern Rockfish" = "Sebastes polyspinis") 
# my_stock_ids <- c("30060", 
#                   "30420") 
# names(my_stock_ids) <- stock_names
# # Setting up stock folder names
# stock_folder_names <- sub(" ", "_", stock_names)
# # Number of stocks (for looping) 
# N_stock <- length(stock_names)
# 
# # Species-specific start years & most recent survey year
# start_years <- c(1990, 
#                  1984) 
# names(start_years) <- stock_names
# end_years <- c(2019,
#                2019)
# names(end_years) <- stock_names
# 
# # Defining the 3 GOA subregions using longitude
# strata.limits <- data.frame(STRATA = as.factor(c("Western", "Central", "Eastern")),
#                             west_border = c(-Inf, -159, -147),
#                             east_border = c(-159, -147, Inf))


## Set RhoConfig betas (both 0 or both 4) which specifies how to treat the temporal variable and observation error distribution (gamma or tweedie)



