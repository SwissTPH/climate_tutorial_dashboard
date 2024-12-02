###############################
# code for generating the models calibrated for each 
# district using the malclimsim package
#
# 13.11.2024
#############################

library(malclimsim)
library(dplyr)

source("~/gitRepos/climate_tutorial_dashboard/processing_scripts/adjust_by_seasonality.R")

## Model parameters
model_params = read.csv("~/gitRepos/code_climate/province_sites_parameters.csv", sep=";")
# saveRDS(model_params, file = "~/gitRepos/malclimsim/ClimateTutorialDashboard/data/province_sites_parameters.RDS")

## Climate data preparation
#climate_data1 = readRDS("~/gitRepos/code_climate/province_level_past_future_climate_data.RDS")
climate_data = readRDS("~/gitRepos/code_climate/version7_province_level_past_future_climate_data.RDS")
#climate_data_no_seasonality = climate_data1 %>% select(province, date, none_seasonal_ppt)
#climate_data_no_seasonality = climate_data_no_seasonality %>% rename(no_seasonality = none_seasonal_ppt)

# Fix the no seasonal entries in the new dataset to be constant
# Update values in df1 with those from df2 where dates match
# climate_data = climate_data2 %>%
#   left_join(climate_data_no_seasonality, by = c("province", "date")) %>% 
#   mutate(none_seasonal_ppt = no_seasonality) %>% 
#   select(-no_seasonality)  # Remove the temporary new_value column

# Remove the seasonal_ppt column which is not needed
# climate_data = climate_data %>% select(-seasonal_ppt)

# Rename columns to fit with scenario encoding
all_climate_data = climate_data %>% rename(normal_ppt = norm_ppt,
                                           drier_ppt = dry_ppt,
                                           extreme_dry_ppt = drier_ppt,
                                           more_wet_ppt = wet_ppt,
                                           extreme_wet_ppt = wetter_ppt,
                                           normal_temp = norm_temp,
                                           warmer_temp = warm_temp,
                                           colder_temp = cool_temp,
                                           extreme_warm_temp = warmer_temp,
                                           extreme_cold_temp = cooler_temp,
                                           normal_seasonal_amplified_ppt = norm_ppt_amplified,   
                                           normal_seasonal_longer_ppt = norm_ppt_longer,      
                                           normal_seasonal_shorter_ppt = norm_ppt_shorter,     
                                           extreme_dry_seasonal_amplified_ppt = drier_ppt_amplified, 
                                           extreme_dry_seasonal_longer_ppt = drier_ppt_longer,     
                                           extreme_dry_seasonal_shorter_ppt = drier_ppt_shorter,    
                                           drier_seasonal_amplified_ppt = dry_ppt_amplified,    
                                           drier_seasonal_longer_ppt = dry_ppt_longer,       
                                           drier_seasonal_shorter_ppt = dry_ppt_shorter,     
                                           more_wet_seasonal_amplified_ppt = wet_ppt_amplified,    
                                           more_wet_seasonal_longer_ppt = wet_ppt_longer,       
                                           more_wet_seasonal_shorter_ppt = wet_ppt_shorter,      
                                           extreme_wet_seasonal_amplified_ppt = wetter_ppt_amplified, 
                                           extreme_wet_seasonal_longer_ppt = wetter_ppt_longer, 
                                           extreme_wet_seasonal_shorter_ppt = wetter_ppt_shorter)

# Generate the rainfall under different seasonality scenarios
# for (location in unique(model_params$Province)) {
#   for (seas_scenario in c("amplified", "longer", "shorter")) {
#     for (rain_scenario in c("normal", "drier", "extreme_dry", "more_wet", "extreme_wet")) {
#       column_name = paste0(rain_scenario, "_seasonal_", seas_scenario, "_ppt")
#       print(column_name)
#       
#       rain_scenario_col = paste0(rain_scenario, "_ppt")
#       print(rain_scenario_col)
#       
#       idx_location = which(all_climate_data$province == location)
#       # First initialize the new column with no seasonality change
#       all_climate_data[idx_location, column_name] = all_climate_data[idx_location, rain_scenario_col]
#       # Change the seasonality for the future
#       idx_future = which(all_climate_data$province == location & all_climate_data$yr >= 2024)
#       # if (rain_scenario == drier) {
#       #   print("here")
#       # }
#       all_climate_data[idx_future, column_name] = seasonality_fun(all_climate_data[idx_future, rain_scenario_col], seas_scenario)
#     }
#   }
# }
saveRDS(all_climate_data, file = "~/gitRepos/climate_tutorial_dashboard/processing_scripts/all_climate_data_province_v7.RDS")

na_counts <- colSums(is.na(all_climate_data))

# Only upload the necessary data on the dashboard
filtered_climate_data = all_climate_data %>% filter(yr >= 2022 & yr <= 2039)
saveRDS(filtered_climate_data, file = "~/gitRepos/climate_tutorial_dashboard/dashboardApp_FR/data/climate_data_province_v7.RDS")

#   
# # The first step to simulating from the model is to specify the region 
# # (latitude and longitude) in which you want your model to simulate from. 
# # Additionally, the years used in the analysis must be specified.
# 
# # Years used for the analysis
# years <- 2010:2023
# 
# 
# 
# # Processing climate data to be used in the model
# # `D' controls the number of previous days used for the rainfall rolling average
# # calculation and `months_30_days` determines if years are 360 or 365 days
# met_360 <- process_climate_data(lon, lat, years, D1 = 60, D2 = 60, temp_path = temp_path,
#                                 rain_path = rain_path, path_to_data, months_30_days = TRUE)
# colnames(met_360)[1] <- "date"
# 
# # Extract rainfall and temperature data
# rain <- met_360$anom  # Standardized rolling mean of rainfall
# temp <- met_360$temp  # Temperature data
# 
# # One now needs to define the inputs related to seasonal malaria 
# # chemoprevention (SMC). There are three vectors that are needed which should 
# # all the same length as the rainfall and temperature vectors. One vector 
# # consists of 1s and 0s that define the start of a round of SMC. The second 
# # vector is the coverage of SMC for that round. And the last vector is the 
# # result of a decay function applied to the coverage over time. The total 
# # effect of SMC at a given time point is defined as the product 
# # (component-wise multiplication) of these vectors at this time.
# 
# # Defining the start and end date of the simulation
# start_date <- as.Date(paste0(as.character(years[1]), "-01-01"))
# end_date <- as.Date(paste0(as.character(years[length(years)]), "-12-31"))
# 
# # Create a matrix indicating which months are active for SMC in each year.
# # Each row represents a year, and each column (1-12) represents a month (1 = active, 0 = inactive).
# months_active <- matrix(data = 0, nrow = length(years), ncol = 12)  # Initialize all months as inactive
# months_active[, c(7, 8, 9, 10)] <- 1  # Set July through October as active months for SMC deployment
# 
# # Generate the SMC schedule using the defined start date, end date, active months, and coverage.
# # The argument 'months_30_days = TRUE' simulates a 360-day calendar (12 months with 30 days each).
# # Coverage is set to 75%.
# smc_schedule <- gen_smc_schedule(start_date, end_date, years, months_active = months_active, months_30_days = TRUE, coverage = 0.75)
# 
# # Viewing the contents of the resulting data frame
# print(head(smc_schedule[240:300,]))
# 
# # Extract key SMC schedule information
# SMC <- smc_schedule$SMC  # Indicator for days when an SMC round started (1s and 0s)
# decay <- smc_schedule$decay  # Efficacy decay of SMC over time
# cov <- smc_schedule$cov  # SMC coverage over time
# 
# # Now, the climate model is loaded using "load_model" which takes as an input the name of the model. 
# # Here it is called "model_det_1". In principle, one could adjust the model or add new 
# # models which could then be loaded and simulated from. The model itself is written in 'odin', 
# # a domain-specific language (DSL) created for facilitating the writing of efficient state-space models. 
# # More information can be found in the originally paper by FitzJohn et al (10.12688/wellcomeopenres.16466.2).
# climate_model <- load_model("model_det_1")  # Load the deterministic climate model
# 
# # Save model and data to list
# model_obj = list(clim_model = climate_model, 
#                  rain_data = rain, 
#                  temp_data = temp,
#                  smc_data = smc_schedule)
# save(model_obj, file = "~/gitRepos/malclimsim/ClimateTutorialDashboard/data/model.Rdata")
# 
# # One last object to define before running the model itself. This 
# # input is a named list where each name corresponds to a parameter within the 
# # model and the values can be specified based on prior knowledge or by fitting 
# # to some observed data. Many of the parameter values here come from the paper 
# # by Ukawuba and Shaman where the model was fit to district-level data in Rwanda. 
# # Found below is a table with information about each parameter 
# # (taken directly from Ukawuba), as well as the corresponding name in the 
# # paper by Ukawuba, as the naming convention differs slightly for some parameters.
# 
# # Define parameter inputs for the malaria model simulation
# param_inputs <- list(
#   mu_TS = 1/30, mu_IR = 1/5, mu_RS_C = 1/130,
#   mu_EI = 1/8, delta_b = 1/(21*365), delta_d = 1/(21*365),
#   delta_a = 1/(5 * 365), p_HM = 0.125, p_MH_C = 0.5, 
#   fT_C = 0.27, qR = 0.17, a_R = 0.4, b_R = 3, N = 5e5, 
#   s = 0.9, p_surv = 0.934, percAdult = 0.81, 
#   eff_SMC = 0, decay = decay, SMC = SMC, cov_SMC = cov,
#   c_R_D = rain, temp = temp, eta = 1, phi = 1, rho = 1  # Inputs for rainfall and temperature
# )
# 
# param_inputs <- list(
#   mu_TS = 1/30, mu_IR = 1/5, eta = 1, mu_RS_C = 1/130, size = 1,
#   mu_EI = 1/8, delta_b = 1/(21*365), delta_d = 1/(21*365),
#   delta_a = 1/(5 * 365), phi = 1, p_HM = 0.125, p_MH_C = 0.5, 
#   rho = 1, fT_C = 0.27, z = 1, qR = 0.17, a_R = 0.4, b_R = 3, N = 5e5, 
#   s = 0.9, p_surv = 0.934, percAdult = 0.81, steps_per_day = 1, SC0 = 0.301, 
#   EC0 = 0.071, IC0 = 0.042, TC0 = 0.054, RC0 = 0.533, SA0 = 0.281, 
#   EA0 = 0.067, IA0 = 0.040, TA0 = 0.051, RA0 = 0.562, size_inv = 0,
#   eff_SMC = 0, decay = decay, SMC = SMC, cov_SMC = cov,
#   c_R_D = rain, temp = temp  # Inputs for rainfall and temperature
# )
# 
# # The "date_sim" function outputs a data frame whic has five columns: the date, 
# # the month number, the incidence in those aged 5 and older, the incidence in 
# # those under 5 years old, and the total incidence.
# start_date <- ymd("2008-01-01")  # Start date of the simulation
# end_date <- ymd("2016-12-31")  # End date of the simulation
# results <- data_sim(
#   climate_model, param_inputs = param_inputs, start_date, end_date, return_EIR = TRUE,
#   month = TRUE, round = FALSE, save = FALSE, month_unequal_days = FALSE,
# )
# 
# 
# # The "plot_time_series" function allows for one to visualize the incidence 
# # over time as well as the rainfall (standardized rolling average) over time. 
# # For all options see ?plot_time_series. 
# 
# # Example 1: Plotting only malaria incidence data with only "total" incidence type
# plot_time_series(results = results, plot_title = "Malaria Incidence",
#                  select_incidence = "<5")
# 
# # Example 2: Plotting both malaria incidence (only "<5" and ">=5") and climate data (only "temp")
# plot_time_series(results = results, met = met_360, plot_title = "Malaria Incidence and Climate Data",
#                  select_incidence = c("<5", ">=5"), select_climate = "temp", climate_facet = TRUE)
# 
# # Example 3: Plotting both malaria incidence (only "total") and climate data (both "temp" and "rollmean")
# plot_time_series(results = results, met = met_360, plot_title = "Malaria Incidence and Climate Data",
#                  select_incidence = "total", select_climate = c("temp", "rollmean"), climate_facet = TRUE)
