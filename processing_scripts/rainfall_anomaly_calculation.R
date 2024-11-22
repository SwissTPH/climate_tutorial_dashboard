########################
# Functions for calculating the anomaly
########################

library(zoo)
library(malclimsim)

# Calculate Rolling Average
# rainfall Numeric vector of rainfall values.
# D - Integer specifying the window size (in days).
# return - A numeric vector of rolling averages with `NA` for positions without enough data.
calculate_rolling_average <- function(rainfall, D) {
  return(zoo::rollmean(rainfall, D, fill = NA, align = "right"))
}

# Standardize Values
# values -  Numeric vector to standardize.
# return - A numeric vector of z-scores.
standardize_values <- function(values) {
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  return((values - mean_val) / sd_val)
}

# Standardize Rainfall with Rolling Average
# rainfall - Numeric vector of daily rainfall values.
# D - Integer specifying the window size (in days) for the rolling average.
# return - A data frame with columns for original rainfall, rolling average, and standardized values.
standardize_rainfall <- function(rainfall, D) {
  # Calculate rolling average
  roll_avg <- calculate_rolling_average(rainfall, D)
  
  # Standardize the rolling average
  standardized <- standardize_values(roll_avg)
  
  # Return results as a data frame
  return(data.frame(
    Rainfall = rainfall,
    RollingAverage = roll_avg,
    Standardized = standardized
  ))
}

rolling_average_temp_D_days <- function(avg_temp, D, save = FALSE, file = "") {
  
  # Extract date and temperature columns
  c1 <- as.Date(as.matrix(avg_temp[1]))  # Date column
  c2 <- as.numeric(as.matrix(avg_temp[2]))  # Temperature column
  
  # Create a time-series object using zoo
  x <- zoo::zoo(c2, c1)
  
  # Calculate rolling average with a window of D days
  avg_temp$rolltemp <- zoo::rollmean(x, D, fill = NA, align = "right")
  
  # Optionally save the updated data with rolling average
  if (save) {
    saveRDS(avg_temp, file = file)
  }
  
  return(avg_temp)  # Return the updated data frame
}

# augmented the function to take observed_rollavg_rain argurment for climate counterfactual experiments.
# observed_rollavg_rain is rolling average rainfall for the original rainfall data to which the model was fitted
# observed_rollavg_rain is a dataframe with date in column 1 and rolling average rainfall in column 2
#**Note: observed_rollavg_rain default is NULL. If observed_rollavg_rain is provided counterfactual anomalies will be returned*
updated_standardize_rainfall <- function(avg_rain, save = TRUE, file = "", observed_rollavg_rain = NULL) {
  # Calculate z-scores for rainfall anomalies.
  if(!is.null(observed_rollavg_rain)){
    avg_rain$anom <- (avg_rain$rollrain - mean(observed_rollavg_rain$rollrain, na.rm = TRUE)) /
      sd(observed_rollavg_rain$rollrain, na.rm = TRUE)
  }else{
    avg_rain$anom <- (avg_rain$rollrain - mean(avg_rain$rollrain, na.rm = TRUE)) /
      sd(avg_rain$rollrain, na.rm = TRUE)
  }
  # Extract date components (month, week, day) for further analysis
  avg_rain$month <- month(avg_rain$date)
  avg_rain$week <- week(avg_rain$date)
  avg_rain$day <- day(avg_rain$date)
  avg_rain$year <- year(avg_rain$date)
  # Optionally save the updated data
  if (save) {
    saveRDS(avg_rain, file = paste(dir, file, sep = ""))
  }
  return(avg_rain)  # Return the updated data frame
}

rolling_average_D_days <- function(avg_rain, D, save = FALSE, file = "", rain = TRUE) {
  
  # Extract date and rainfall columns
  c1 <- as.Date(as.matrix(avg_rain[1]))  # Date column
  c2 <- as.numeric(as.matrix(avg_rain[2]))  # Rainfall column
  
  # Create a time-series object using zoo
  x <- zoo::zoo(c2, c1)
  
  # Calculate rolling average with a window of D days
  avg_rain$rollrain <- zoo::rollmean(x, D, fill = NA, align = "right")
  
  # Optionally save the updated data with rolling average
  if (save) {
    saveRDS(avg_rain, file = paste(dir, file, sep = ""))
  }
  
  return(avg_rain)  # Return the updated data frame
}

# met_df = readRDS("~/gitRepos/code_climate/province_level_past_future_climate_data.RDS")
# observed_rain_dataset = met_df %>% select("date","ppt") %>% rename(avg_rain = ppt)
# counter_rain_dataset = met_df %>% select("date","wetter_ppt") %>% rename(avg_rain=wetter_ppt)
# D=50
#  
# observed_rollavg_rain = rolling_average_D_days(avg_rain = observed_rain_dataset, D=D)
# counter_rollavg_rain = rolling_average_D_days(avg_rain = counter_rain_dataset, D=D)
# 
# a = updated_standardize_rainfall(avg_rain = counter_rollavg_rain, save = F, file = "", 
#                                   observed_rollavg_rain = observed_rollavg_rain)

