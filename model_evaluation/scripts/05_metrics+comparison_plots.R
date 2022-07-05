#### Plots of metrics & comparisons between RE and VAST estimates

# Library setup
library(here)
library(dplyr)
library(tidyr)
library(R.utils)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(tibble)

## Running all of previous scripts 
source(here("scripts/01_functions.R"))
source(here("scripts/00_results_importing.R"))
source(here("scripts/02_POA_and_POA_SE.R"))
source(here("scripts/03_absolute_difference.R"))
source(here("scripts/04_CV.R"))

# Setting up species labels for plot titles 
species_names <- names(stock_names)
names(species_names) <- stock_folder_names
plot_color_labels <- c("RW results" = "Random walk", 
                       "delta-GLMM results" = "Delta-GLMM",
                       "survey data" = "Design-based")

################# FIGURES #################

### Plotting proportion of area for RE, VAST and the survey data all together 
# as a dotted line plot, with proportion standard error estimates included for
# RE and VAST

# Combining both species in Combined_POA list into a single dataframe to plot 
# them stacked on top of each other
Combined_POA_long <- melt(Combined_POA, id.vars = colnames(Combined_POA[[1]]))
colnames(Combined_POA_long)[8] <- "Species"
# Excluding NR eastern subregion  
modified_Combined_POA_long <- Combined_POA_long[Combined_POA_long$Subregion != subregion[3] | Combined_POA_long$Species != stock_folder_names[2],]
# Creating data set that excludes survey data for the error ribbon
delta_rw_data <- modified_Combined_POA_long[modified_Combined_POA_long$data_type != "survey data",]

plot_1 <- ggplot(modified_Combined_POA_long, aes(x = Year, y = POA)) +
  geom_point(aes(color = data_type), position = position_dodge(width = 0.3)) +
  geom_line(aes(color = data_type), position = position_dodge(width = 0.3)) +
  geom_ribbon(data = delta_rw_data, aes(ymin = POA_LCI, ymax = POA_UCI, fill = data_type), alpha = 0.2) +
  facet_wrap(Species ~ Subregion, scales = "free_x", ncol = 3,
             labeller = labeller(Species = species_names)) +
  scale_colour_manual(values = model_colors, name = "",
                      labels = plot_color_labels) +
  scale_fill_manual(values = model_colors, name = "",
                    labels = plot_color_labels) +
  labs(title = "", y = "Proportion of Total Biomass") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0,1), expand = expansion(mult = c(0, 0)))

# ggsave(filename = "All_POA_comparison_plot_no_NReast_freey.png",
#        plot = plot_1, path = here("Plots/betas_2"))


### Bias plot comparing POA estimate from both models for the excluded year
### to the survey data POA

# Melting species list to plot both species in the same plot
Combined_excl_year_POA <- melt(Excl_year_POA, id.vars = colnames(Excl_year_POA[[1]]))
colnames(Combined_excl_year_POA)[6] <- "Species"
# Excluding eastern NR 
modified_Combined_excl_year_POA <- Combined_excl_year_POA[Combined_excl_year_POA$Subregion != subregion[3] | Combined_excl_year_POA$Species != stock_folder_names[2],]

plot_2 <- ggplot(modified_Combined_excl_year_POA, 
                 aes(y = estimated_POA, x = survey_POA, color = model_type)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values = model_colors[1:2],
                     labels = plot_color_labels) +
  facet_wrap(Species ~ Subregion, ncol = 3,
             labeller = labeller(Species = species_names)) +
  labs(title = "", y = "Model Estimated Proportion", 
       x = "Design-based Proportion", color = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.spacing = unit(1, "cm", data = NULL),
        plot.margin = unit(c(0,1,0,0), "cm")) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0)))

# ggsave(filename ="POA_bias_comparison_v2.png",
#        plot = plot_2, path = here("Plots/betas_2"))

### Plotting histograms of absolute differences for replicate results, with
# both RE and VAST in the same plot, and including mean absolute difference
# values as vertical lines in the histogram

# Excluding eastern NR 
modified_abs_diff_all_melt <- abs_diff_all_melt[abs_diff_all_melt$Subregion != subregion[3] | abs_diff_all_melt$Species != stock_folder_names[2],]
modified_mean_abs_diff_all <- mean_abs_diff_all[mean_abs_diff_all$Subregion != subregion[3] | mean_abs_diff_all$Species != stock_folder_names[2],]

plot_3 <- ggplot(data = modified_abs_diff_all_melt, 
                 aes(x = abs_diff, fill = model)) + 
  geom_histogram(alpha = 0.4, position = "identity", bins = 20, 
                 color = "black", size=0.3) +
  geom_vline(data = modified_mean_abs_diff_all[modified_mean_abs_diff_all$model == "RW results",], 
             aes(xintercept = abs_diff), color = "blue") +
  geom_vline(data = modified_mean_abs_diff_all[modified_mean_abs_diff_all$model == "delta-GLMM results",], 
             aes(xintercept = abs_diff), color = "red") +
  scale_fill_manual(values = model_colors[1:2], name = "",
                    labels = plot_color_labels) +
  scale_color_manual(values = model_colors[1:2], name = "",
                     labels = plot_color_labels) +
  facet_wrap(Species ~ Subregion, ncol = 3,
             labeller = labeller(Species = species_names)) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(title = "", x = "Absolute difference", y = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))


# ggsave(filename = "RE_v_VAST_abs_diff_hist.png", plot = plot_3,
#        path = here("Plots/betas_2"))


### Plot histograms of CVs for excluded year, including vertical lines with 
### mean CV

# Taking out eastern NR (as suggested)
modified_Combined_CVs_resampled <- Combined_CVs_resampled[Combined_CVs_resampled$Subregion != subregion[3] | Combined_CVs_resampled$Species != stock_folder_names[2],]
modified_mean_CV_all <- mean_CV_all[mean_CV_all$Subregion != subregion[3] | mean_CV_all$Species != stock_folder_names[2],]


plot_4 <- ggplot(data = modified_Combined_CVs_resampled, 
                 aes(x = CV, fill = model_type)) + 
  geom_histogram(alpha = 0.4, position = "identity", bins = 20,
                 color = "black", size = 0.3) +
  geom_vline(data = modified_mean_CV_all[modified_mean_CV_all$model_type == "RW",], 
             aes(xintercept = CV), color = "blue") +
  geom_vline(data = modified_mean_CV_all[modified_mean_CV_all$model_type == "delta-GLMM",], 
             aes(xintercept = CV), color = "red") +
  facet_wrap(Species ~ Subregion, ncol = 3,
             labeller = labeller(Species = species_names)) +
  scale_fill_manual(values = model_colors[1:2], name = "",
                    labels = plot_color_labels[1:2]) +
  scale_color_manual(values = model_colors[1:2], name = "",
                     labels = plot_color_labels) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Coefficient of Variation", y = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))

# ggsave(filename = "CV_comparison_hist.png",
#        plot = plot_4, path = here("Plots/betas_2"))


### Scatter plot for ratio in CV values between all data and excluded years

# Excluding eastern NR 
modified_Combined_CV_ratio <- Combined_CV_ratio[Combined_CV_ratio$Subregion != subregion[3] | Combined_CV_ratio$Species != stock_folder_names[2],]

plot_5 <- ggplot(modified_Combined_CV_ratio) +
  geom_point(aes(x = Year, y = log(ratio), color = model_type)) +
  facet_wrap(Species ~ Subregion, ncol = 3,
             labeller = labeller(Species = species_names)) +
  scale_color_manual(values = model_colors[1:2], name = "",
                     labels = plot_color_labels[1:2]) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Log(ratio of CVs)", 
       x = "Year") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))

# ggsave(filename = paste0("CV_ratio_points.png"),
#        plot = plot_5, path = here("Plots/betas_2"))



### Plot of absolute difference/SE normality evaluation:


# #Excluding eastern NR
# modified_all_error <- All_error[All_error$Subregion != subregion[3] | All_error$Species != stock_folder_names[2],]
# 
# plot_6 <- ggplot(data = modified_all_error, aes(x = Error, fill = model_type)) + 
#   geom_histogram(alpha = 0.4, position = "identity", bins = 20) +
#   facet_wrap(Species ~ Subregion, ncol = 3,
#              labeller = labeller(Species = species_names)) +
#   scale_fill_manual(values = model_colors[1:2]) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "", x = "Absolute difference of proportion/SE of proportion", y = "", 
#        fill = "Model")
# 
# # ggsave(filename ="abs_diff_div_SE_histogram.png",
# #        plot = plot_6, path = here("Plots/betas_2"))
# 
# ## Version of above histogram that includes a vertical line at 2 (showing 
# ## where 2 SEs is)
# 
# plot_6a <- ggplot(data = modified_all_error, aes(x = Error, fill = model_type)) + 
#   geom_histogram(alpha = 0.4, position = "identity", bins = 20) +
#   geom_vline(xintercept = 2) +
#   facet_wrap(Species ~ Subregion, ncol = 3,
#              labeller = labeller(Species = species_names)) +
#   scale_fill_manual(values = model_colors[1:2]) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "", x = "Absolute difference of proportion/SE of proportion", y = "", 
#        fill = "Model")
# 
# # ggsave(filename ="abs_diff_div_SE_histogram_v2.png",
# #        plot = plot_6a, path = here("Plots/betas_2"))
# 
# plot_alt_6 <- ggplot(modified_all_error) +
#   geom_point(aes(x = Year, y = Error, color = model_type)) +
#   geom_hline(yintercept = 2) +
#   facet_wrap(Species ~ Subregion, ncol = 3,
#              labeller = labeller(Species = species_names),
#              scales = "free_x") +
#   scale_color_manual(values = model_colors[1:2]) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "", 
#        y = "abs_diff/SE", x = "Year", 
#        color = "Model")
# 
# # ggsave(filename ="abs_diff_div_SE_points.png",
# #        plot = plot_alt_6, path = here("Plots/betas_2"))

#### Trying out plotting pnorm(abs_diff/SE) - this should be normally distributed,
#### I guess?

# Excluding Eastern NR & ordering subregions in correct plotting order:
modified_diff_div_SE <- all_diff_div_SE[all_diff_div_SE$Subregion != subregion[3] | all_diff_div_SE$Species != stock_folder_names[2],]
modified_diff_div_SE$Subregion <- factor(modified_diff_div_SE$Subregion,
                                         levels = subregion)

# Histogram of pnorm values
plot_6 <- ggplot(modified_diff_div_SE, aes(x = pnorm, fill = model_type)) +
  geom_histogram(alpha = 0.4, position = "identity", bins = 15,
                 color = "black", size = 0.3) +
  stat_function(fun = dnorm, args = list(mean = 0.5, sd = 0.16), linetype = "dashed") +
  facet_wrap(Species ~ Subregion, ncol = 3,
             labeller = labeller(Species = species_names)) +
  scale_fill_manual(values = model_colors[1:2], name = "",
                    labels = plot_color_labels) +
  scale_color_manual(values = model_colors[1:2], name = "",
                     labels = plot_color_labels) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "pnorm(difference/SE)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))

# ggsave(filename ="pnorm_hist.png",
#        plot = plot_6, path = here("Plots/betas_2"))


## QQ plot of pnorm values

plot_7 <- ggplot(data = modified_diff_div_SE, aes(sample = diff_div_SE,
                                                   color = model_type)) +
  stat_qq()+
  stat_qq_line() +
  facet_wrap(Species ~ Subregion, ncol = 3,
             labeller = labeller(Species = species_names)) +
  scale_color_manual(values = model_colors[1:2], name = "",
                     labels = plot_color_labels) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "", x = "Theoratical Normal Quantiles",
       y = "Difference/SE Quantiles") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))

ggsave(filename ="diff_div_SE_qqplot.png",
       plot = plot_7, path = here("Plots/betas_2"))

# # Cummulative points
# plot_7b <- ggplot(modified_diff_div_SE) +
#   geom_point(aes(x = diff_div_SE, y = pnorm, color = model_type)) +
#   facet_wrap(Species ~ Subregion, ncol = 3,
#              labeller = labeller(Species = species_names)) +
#   scale_color_manual(values = model_colors[1:2]) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "", 
#        y = "pnorm(diff/SE)", x = "diff/SE", 
#        color = "Model")
# 
# # ggsave(filename ="pnorm_vs_diff_div_SE_points.png",
# #        plot = plot_7b, path = here("Plots/betas_2"))
# # 
# # qqnorm(diff_div_SE$Sebastes_alutus$pnorm)
# # qqline(diff_div_SE$Sebastes_alutus$pnorm)
# 
# plot_7c <- ggplot(data = modified_diff_div_SE, aes(sample = diff_div_SE, 
#                                                    color = model_type)) +
#   stat_qq()+
#   stat_qq_line() +
#   facet_wrap(Species ~ Subregion, ncol = 3,
#              labeller = labeller(Species = species_names),
#              scales = "free_y") +
#   scale_color_manual(values = model_colors[1:2]) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "", x = "Theoratical Normal Quantiles",
#        y = "Difference/SE Quantiles", 
#        color = "Model")
# 
# # ggsave(filename ="diff_div_SE_qqplot.png",
# #        plot = plot_7c, path = here("Plots/betas_2"))
# 
# ## Version of the above plot which separates the two models into different plots
# # Made subregion levels correct for order of plotting:
# for(stock in stock_folder_names) {
#   diff_div_SE[[stock]]$Subregion <- factor(diff_div_SE[[stock]]$Subregion,
#                                        levels = subregion)
# }
# 
# plot_7d_1 <- ggplot(data = diff_div_SE[[1]], aes(sample = diff_div_SE, 
#                                                  color = model_type)) +
#   stat_qq()+
#   stat_qq_line() +
#   facet_wrap(Subregion ~ model_type, ncol = 2,
#              labeller = labeller(Subregion = sub_names)) +
#   scale_color_manual(values = model_colors[1:2]) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = species_names[1], 
#        y = "", x = "diff/SE", 
#        color = "Model")
# 
# # ggsave(filename ="diff_div_SE_qqplot_POP.png",
# #        plot = plot_7d_1, path = here("Plots/betas_2"))
# 
# plot_7d_2 <- ggplot(data = diff_div_SE[[2]], aes(sample = diff_div_SE, 
#                                                  color = model_type)) +
#   stat_qq()+
#   stat_qq_line() +
#   facet_wrap(Subregion ~ model_type, ncol = 2,
#              labeller = labeller(Subregion = sub_names)) +
#   scale_color_manual(values = model_colors[1:2]) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = species_names[2], 
#        y = "", x = "diff/SE", 
#        color = "Model")

# ggsave(filename ="diff_div_SE_qqplot_NR.png",
#        plot = plot_7d_2, path = here("Plots/betas_2"))

#   
# library(ggpubr)
# ggqqplot(diff_div_SE[[1]], x = "diff_div_SE",
#          color = "model_type", 
#          palette = model_colors[1:2],
#          ggtheme = theme_pubclean())


################# TABLES #################

###### **Not using the equation table anymore** #####
## Creating linear models of the survey proportion ~ model estimated proportion
## for each species, subregion and model type to put in a table that corresponds
## with the regression lines in the bias plots

# # May not be using this after all, but keep in for now.
# lm_eqn_all <- list()
# for(stock in 1:N_stock) {
#   lm_eqn <- as.data.frame(matrix(NA, nrow = 3, ncol = 3))
#   colnames(lm_eqn) <- c("RW", "delta-GLMM", "Subregion")
#   lm_eqn[, 3] <- subregion
#   for (model in 1:2) {
#     for (sub in 1:N_sub) {
#       results <- Excl_year_POA[[stock]][Excl_year_POA[[stock]]$Subregion == subregion[sub] & 
#                                       Excl_year_POA[[stock]]$model_type == names(model_colors)[model],]
#       lm_fit <- lm(results$survey_POA ~ results$estimated_POA)
#       lm_eqn[sub, model] <- paste0("y = ", round(lm_fit$coefficients[2], 3), "x + ", 
#                                    round(lm_fit$coefficients[1], 3))
#       print(paste(stock_names[stock], subregion[sub], names(model_colors)[model]))
#       print(summary(lm_fit)) # checking for significance  
#     }
#   }
#   lm_eqn_all[[stock]] <- lm_eqn
# }
# names(lm_eqn_all) <- stock_folder_names
# 
# # With new VAST results, significance is: only NR central intercept (0.0137),
# # and only very minorly. So yeah, probably no need to include the above table
# 
# Combined_lm_eqn_all <- melt(lm_eqn_all)
# colnames(Combined_lm_eqn_all)[4] <- "Species"
# 
# Combined_lm_eqn_all

### Formatting mean absolute difference and mean CV dataframe to be made into a
### table in thesis .Rmd

Combined_mean_abs_diff_CV <- add_column(modified_mean_abs_diff_all, .after = 3, CV = modified_mean_CV_all$CV)
Combined_mean_abs_diff_CV$Species <- gsub("_", " ", Combined_mean_abs_diff_CV$Species)
Combined_mean_abs_diff_CV$model <- gsub(" results", "", Combined_mean_abs_diff_CV$model)


### Creating table with predicted biomass for 2020-2022 based on stock 
### assessments

### Creating dataframes for biomass info from stock assessments
stock_assessment_biomass <- list()
## POP data
# Setting up total biomasses dataframe
stock_assessment_biomass[[1]] <- list()
stock_assessment_biomass[[1]]$total_biomass <- as.data.frame(cbind(c(2020:2022), 
                                                                      c(31238, 36177, 34602)))
colnames(stock_assessment_biomass[[1]]$total_biomass) <- c("Year", "total_biomass_mt")
# Setting up stock assessment predicted proportions based on RE model
stock_assessment_biomass[[1]]$preds <- as.data.frame(matrix(NA, nrow = 3,  ncol = 4))
colnames(stock_assessment_biomass[[1]]$preds) <- c("Year", subregion)
stock_assessment_biomass[[1]]$preds$Year <- c(2020:2022)
stock_assessment_biomass[[1]]$preds$WESTERN <- c(1437, 1643, 1572)
stock_assessment_biomass[[1]]$preds$CENTRAL <- c(23678, 27429, 26234)
stock_assessment_biomass[[1]]$preds$EASTERN <- c(6123, 7105, 6796)
stock_assessment_biomass[[1]]$preds$source <- "stock assessment"
## NR data
# Setting up total biomasses dataframe
stock_assessment_biomass[[2]] <- list()
stock_assessment_biomass[[2]]$total_biomass <- as.data.frame(cbind(c(2020:2022), 
                                                                   c(4311, 5358, 5100)))
colnames(stock_assessment_biomass[[2]]$total_biomass) <- c("Year", "total_biomass_mt")
# Setting up stock assessment predicted proportions based on RE model
stock_assessment_biomass[[2]]$preds <- as.data.frame(matrix(NA, nrow = 3,  ncol = 4))
colnames(stock_assessment_biomass[[2]]$preds) <- c("Year", subregion)
stock_assessment_biomass[[2]]$preds$Year <- c(2020:2022)
stock_assessment_biomass[[2]]$preds$WESTERN <- c(1133, 2023, 1926)
stock_assessment_biomass[[2]]$preds$CENTRAL <- c(3178, 3334, 3173)
stock_assessment_biomass[[2]]$preds$EASTERN <- c(1, 1, 1)
stock_assessment_biomass[[2]]$preds$source <- "stock assessment"

names(stock_assessment_biomass) <- stock_folder_names

# Creating dataframe with stock assessment estimates & VAST predicted proportions
VAST_preds <- list()
for(stock in 1:N_stock) {
  pred_df <- as.data.frame(matrix(NA, nrow = 3, ncol = 4))
  colnames(pred_df) <- c("Year", subregion)
  pred_df$Year <- c(2020:2022)
  pred_df$WESTERN <- round(stock_assessment_biomass[[stock]]$total_biomass$total_biomass_mt*pred_POA[[stock]]$POA[pred_POA[[stock]]$Subregion == subregion[1]]) 
  pred_df$CENTRAL <- round(stock_assessment_biomass[[stock]]$total_biomass$total_biomass_mt*pred_POA[[stock]]$POA[pred_POA[[stock]]$Subregion == subregion[2]]) 
  pred_df$EASTERN <- round(stock_assessment_biomass[[stock]]$total_biomass$total_biomass_mt*pred_POA[[stock]]$POA[pred_POA[[stock]]$Subregion == subregion[3]]) 
  pred_df$source <- "delta-GLMM"
  VAST_preds[[stock]] <- pred_df
}
names(VAST_preds) <- stock_folder_names

# combining stock assessment and VAST predictions
Combined_preds <- list()
for(stock in 1:N_stock) {
  Combined_preds[[stock]] <- rbind(stock_assessment_biomass[[stock]]$preds, VAST_preds[[stock]])
}
names(Combined_preds) <- stock_folder_names

All_preds <- melt(Combined_preds, id.vars = colnames(Combined_preds[[1]]))
colnames(All_preds)[6] <- "Species"

# Formatting table
Preds_table <- All_preds[order(All_preds$Species, All_preds$Year),]




################# SAVE ALL #################
plot_list <- list()
## Create list to store plots (to create .rds file at the end and save)
plot_list$Figures <- list(POA = plot_1, 
                          bias = plot_2,
                          abs_diff = plot_3, 
                          CV = plot_4, 
                          CV_ratio = plot_5,
                          diff_div_SE_pnorm = plot_6,
                          diff_div_SE_qqplot = plot_7)
plot_list$Tables <- list(mean_absdiff_CV = Combined_mean_abs_diff_CV, 
                       # lm_eqns = Combined_lm_eqn_all,
                       pred_20_22 = Preds_table)


### Make an .rds file with plot list object to import into other projects
saveRDS(plot_list, file = here("all_figs_tables_obj.rds"))

####### Saving results objects used in plots (to redo plots for presentation)
data_objs <- list()
data_objs$results <- list(species_names_obj = species_names,
                          combined_POA = modified_Combined_POA_long,
                          abs_diff = modified_abs_diff_all_melt,
                          mean_abs_diff = modified_mean_abs_diff_all,
                          CVs_resampled = modified_Combined_CVs_resampled,
                          mean_CVs_resampled = modified_mean_CV_all,
                          CV_ratios = modified_Combined_CV_ratio,
                          excl_yr_POA = modified_Combined_excl_year_POA,
                          diff_div_SE = modified_diff_div_SE)
data_objs$survey_data <- list(raw_survey_data = GOA_raw_data_list,
                              subregion_survey_data = separated_stock_data)
saveRDS(data_objs, file = here("results+survey_data.rds"))
