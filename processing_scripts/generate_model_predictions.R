########################
# generate database with model predictions
# monica.golumbeanu@swistph.ch
# 21.11.2024
########################
library(dplyr)

source("~/gitRepos/malclimsim/dashboard_processing_scripts/rainfall_anomaly_calculation.R")

predict_model = function(location, model_params, met_df, rain_scen, temp_scen, year1, year2, climate_model) {
  # Select only the province
  met_df = met_df %>% filter(province == location)
  # Extract parameters
  prov_param = model_params %>% filter(Province == location)
  
  # Calculate rainfall anomaly
  counter_rain = met_df %>% select("date", "normal_ppt") %>% rename(avg_rain = normal_ppt)
  observed_rain = met_df %>% select("date", rain_scen) %>% rename(avg_rain = rain_scen)

  observed_rollavg_rain = rolling_average_D_days(avg_rain = observed_rain, D = prov_param$D)
  counter_rollavg_rain = rolling_average_D_days(avg_rain = counter_rain, D = prov_param$D)
  
  anom = updated_standardize_rainfall(avg_rain = counter_rollavg_rain, save = F, file = "", 
                                      observed_rollavg_rain = observed_rollavg_rain)
  
  rain = anom %>% filter(year >= year1 & year <= year2) %>% select(anom)
  
  # Calculate rolling average of temperature and convert to C from K
  temp_data = met_df %>% select("date", temp_scen)

  rolling_temp = rolling_average_temp_D_days(temp_data, prov_param$D, save = FALSE)
  temp = rolling_temp %>% filter(year(date) >= year1 & year(date) <= year2)
  
  # SMC
  years <- 2019:2039
  start_date <- ymd("2019-01-01")  # Start date of the simulation
  end_date <- ymd("2039-12-31")  # End date of the simulation
  # Create a matrix indicating which months are active for SMC in each year.
  # Each row represents a year, and each column (1-12) represents a month (1 = active, 0 = inactive).
  months_active = matrix(data = 0, nrow = length(years), ncol = 12)  # Initialize all months as inactive
  months_active[, c(7)] = 1  # Set July as active month for SMC deployment
  smc_schedule = gen_smc_schedule(start_date, end_date, years, months_active = months_active, months_30_days = TRUE, coverage = 0)
  # Extract key SMC schedule information
  SMC = smc_schedule$SMC  # Indicator for days when an SMC round started (1s and 0s)
  decay = smc_schedule$decay  # Efficacy decay of SMC over time
  cov = smc_schedule$cov  # SMC coverage over time
  
  # param_inputs = list(
  #   mu_TS = 1/30, mu_IR = 1/5, eta = 1, mu_RS_C = prov_param$mu_RS, size = 1,
  #   mu_EI = prov_param$mu_EI, delta_b = 1/(21*365), delta_d = 1/(21*365),
  #   delta_a = 1/(5 * 365), phi = 1, p_HM = 0.125, p_MH_C = 0.5, 
  #   rho = 1, fT_C = 0.27, z = 1, qR = prov_param$qR, a_R = prov_param$a_R, b_R = prov_param$b_R, N = 5e5, 
  #   s = prov_param$s, p_surv = 0.934, percAdult = 0.81, steps_per_day = 1, SC0 = 0.301, 
  #   EC0 = 0.071, IC0 = 0.042, TC0 = 0.054, RC0 = 0.533, SA0 = 0.281, 
  #   EA0 = 0.067, IA0 = 0.040, TA0 = 0.051, RA0 = 0.562, size_inv = 0,
  #   eff_SMC = 0, decay = decay, SMC = SMC, cov_SMC = cov,
  #   c_R_D = rain$anom, temp = temp$rolltemp - 273.15)  # Inputs for rainfall and temperature
  
  param_inputs <- list(
    mu_TS = 1/30, mu_IR = 1/5, eta = 1, mu_RS_C = 1/130, size = 1,
    mu_EI = 1/8, delta_b = 1/(21*365), delta_d = 1/(21*365),
    delta_a = 1/(5 * 365), phi = 1, p_HM = 0.125, p_MH_C = 0.5, 
    rho = 1, fT_C = 0.27, z = 1, qR = 0.17, a_R = 0.4, b_R = 3, N = 5e5, 
    s = 0.9, p_surv = 0.934, percAdult = 0.81, steps_per_day = 1, SC0 = 0.301, 
    EC0 = 0.071, IC0 = 0.042, TC0 = 0.054, RC0 = 0.533, SA0 = 0.281, 
    EA0 = 0.067, IA0 = 0.040, TA0 = 0.051, RA0 = 0.562, size_inv = 0,
    eff_SMC = 0, decay = decay, SMC = SMC, cov_SMC = cov,
    c_R_D = rain$anom, temp = temp$rolltemp - 273.15  # Inputs for rainfall and temperature
  )
  
  results <- data_sim(
    climate_model, param_inputs = param_inputs, start_date, end_date, 
    return_EIR = TRUE, month = TRUE, round = FALSE, 
    save = FALSE, month_unequal_days = FALSE,
  )
  results$inc_A = NULL
  results$month_no = NULL
  results$province = location
  results$temp_scen = temp_scen
  results$rain_scen = rain_scen
  results = results %>% rename(inc_5 = inc_C, inc_all = inc)
  
  return(results)
}

model_params = read.csv("~/gitRepos/code_climate/province_sites_parameters.csv", sep=";")
climate_data_df = readRDS("~/gitRepos/code_climate/all_climate_data_province.RDS")
#load(file = "~/gitRepos/malclimsim/ClimateTutorialDashboard/data/model.Rdata")

temp_scenarios = c("normal", "warmer", "extreme warm", "colder", "extreme cold")
rainfall_scenarios = c("normal", "drier", "extreme dry", "wetter", "extreme wet")
seasonality_scenarios = c("no change", "none", "amplified", "longer", "shorter")

# Time frame
year1 = 2019
year2 = 2039

all_model_pred = NULL
climate_model = load_model("model_det_1")

# For each place
for (location in unique(model_params$Province)){
  print(location)
  # Varying temperature
  for(i_temp in temp_scenarios) {
    print(i_temp)
    temp_scen = paste0(gsub(" ", "_", i_temp), "_temp")
    
    # Varying rainfall
    for (i_rain in rainfall_scenarios) {
      print(i_rain)
      rain_scen = paste0(gsub(" ", "_", i_rain), "_ppt")
      if (i_rain == "normal") {
        # Varying seasonality
        for (i_seas in seasonality_scenarios) {
          print(i_seas)
          rain_scen = paste0(gsub(" ", "_", i_seas), "_seasonal_ppt")
          res_i = predict_model(location, model_params, climate_data_df, 
                                rain_scen, temp_scen, year1, year2, climate_model)
          all_model_pred = rbind.data.frame(all_model_pred, res_i)
        }
      } else {
        res_i = predict_model(location, model_params, climate_data_df, 
                              rain_scen, temp_scen, year1, year2, climate_model)
        all_model_pred = rbind.data.frame(all_model_pred, res_i)
      }
      
    }
  }
}

saveRDS(all_model_pred, file = "~/gitRepos/malclimsim/ClimateTutorialDashboard/data/model_predictions_province.RDS")
