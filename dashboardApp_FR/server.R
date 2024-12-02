#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyr)
library(stringr)
library(lubridate)
library(dplyr)
library(zoo)

function(input, output, session) {
  
  # Load model and  data
  climate_data = readRDS("data/climate_data_province_v5.RDS")
  province_params = readRDS("data/province_sites_parameters.RDS")
  model_pred = readRDS("data/model_predictions_province_v6.RDS")
  start_year = 2023
  
  source("rainfall_anomaly_calculation.R")
  
  # Track button state
  content_visible = reactiveVal(TRUE)
  
  observeEvent(input$hide_climate_btn, {
    toggle("climate_div")  # Toggle the div visibility
    
    # Change button label based on visibility
    if (content_visible()) {
      updateActionButton(session, "hide_climate_btn", label = "\u25BC")  # Change to down-triangle
      content_visible(FALSE)  # Update state
    } else {
      updateActionButton(session, "hide_climate_btn", label = "\u25B2")  # Change to up-triangle
      content_visible(TRUE)  # Update state
    }
  })
  
  # Define a reactive variable that stores the selected temperature scenario
  selected_temp = reactiveVal(NULL)
  SMC_coverage = reactiveVal(NULL)
  
  # Deactivate/activate the temperature type only for Kigali city
  observe({
    # If "A" is selected in the first selectInput, disable the second selectInput
    if (input$province_choice != "Kigali City") {
      updateSelectInput(session, "temperature_type", selected = "temporal evolution")
      shinyjs::disable("temperature_type")
    } else {
      shinyjs::enable("temperature_type")
    }
  })
  
  # Activate/deactivate parts of the inputs based on temperature type
  observe({
    # If "A" is selected in the first selectInput, disable the second selectInput
    if (input$temperature_type == "baseline constant") {
      selected_temp(paste0("temp_", input$discrete_temperature, "C"))
      SMC_coverage(0)
      
      # Reset values to default for disabled inputs
      # updateSelectInput(session, "rainfall_scenario", selected = "normal")
      updateSelectInput(session, "seasonality_scenario", selected = "no change")
      updateSliderTextInput(session, "SMC_cov", selected = "0")
      
      #shinyjs::disable("rainfall_scenario")  
      shinyjs::disable("seasonality_scenario")
      shinyjs::disable("SMC_cov")
    } else {
      selected_temp(paste0(gsub(" ", "_", input$temperature_scenario), "_temp"))
      SMC_coverage(input$SMC_cov)
      shinyjs::enable("rainfall_scenario")  
      shinyjs::enable("seasonality_scenario") 
      shinyjs::enable("SMC_cov")
    }
  })
  
  # Define a reactive variable based on the listbox with the locations
  selected_site = reactive({
    province_params %>% filter(Province == input$province_choice) %>% select(Site)
  })
  
  # Define a reactive variable based on the listbox with the rollong average window
  site_D = reactive({
    province_params %>% filter(Province == input$province_choice) %>% pull(D)
  })
  
  # Dynamically update site name based on selection
  output$selected_site_name <- renderText({
    paste("Site:", selected_site())
  })
  
  # Define a reactive variable based on the scenario choices
  selected_climate_data = reactive({
    # Identify the selected time frame to forecast
    selected_year = input$yearForecast
    
    # Identify the scenarios
    # Temperature
    # selected_temp = paste0(gsub(" ", "_", selected_temp()), "_temp")
    # Rain depends on the seasonality scenario
    if (input$seasonality_scenario == "no change") {
      selected_rain = paste0(gsub(" ", "_", input$rainfall_scenario), "_ppt")
    } else {
      selected_rain = paste0(gsub(" ", "_", input$rainfall_scenario), 
                             "_seasonal_", gsub(" ", "_", input$seasonality_scenario), "_ppt")
    }
    
    # Define new columns with the scenario values
    climate_data$temperature_scenario = climate_data[[selected_temp()]]
    climate_data$rainfall_scenario = climate_data[[selected_rain]]
    
    # Define list of columns to select
    all_columns = c(colnames(climate_data)[1:6], 
                    "normal_temp", "normal_ppt",
                    "rainfall_scenario", "temperature_scenario")
    
    # Select corresponding data
    climate_data %>% filter(province == input$province_choice &
                              yr >= start_year & yr <= input$year_forecast) %>% 
      select(all_columns)
  })
  
  output$rain_plot = renderPlotly({
    # Select the rain data only and change to long format
    rain_data = selected_climate_data() %>% select(1:6, "normal_ppt", "rainfall_scenario")
    rain_data = rain_data %>% rename("Selected scenario" = "rainfall_scenario",
                                     "Counterfactual" = "normal_ppt")
    rain_data = rain_data %>% pivot_longer(cols = c("Selected scenario", "Counterfactual"),
                                           names_to = "rain_scenario",
                                           values_to = "rain_value")
    
    # Define a custom color palette (you can choose colors or use a predefined scale)
    rain_colors <- c("Selected scenario" = "#2b8cbe", "Counterfactual" = "grey")
    # Create the plotly plot
    plot <- plot_ly(data = rain_data, x = ~date, y = ~rain_value, 
                    color = ~rain_scenario, 
                    type = 'scatter', mode = 'lines',
                    line = list(width = 2),
                    colors = rain_colors
    ) %>%
      layout(
        title = " ",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Rainfall (mm per day)"),
        legend = list(
          title = list(text = ""),
          orientation = 'h',  # Horizontal legend
          x = 0.5,  # Center the legend horizontally
          xanchor = 'center',  # Anchor the x position at the center
          y = -0.2  # Position the legend below the plot
        )
      )
    plot
  })
  
  output$rain_plot_anomaly = renderPlotly({
    
    # Select the rain data only and change to long format
    rain_data = selected_climate_data() %>% select(1:6, "normal_ppt", "rainfall_scenario")
    # rain_data = rain_data %>% rename("Selected scenario" = "rainfall_scenario",
    #                                  "Counterfactual" = "normal_ppt")
    # rain_data = rain_data %>% pivot_longer(cols = c("Selected scenario", "Counterfactual"),
    #                                        names_to = "rain_scenario",
    #                                        values_to = "rain_value")
    
    # Calculate rainfall anomaly
    counter_rain = rain_data %>% select("date", "rainfall_scenario") %>% rename(avg_rain = rainfall_scenario)
    observed_rain = rain_data %>% select("date", "normal_ppt") %>% rename(avg_rain = normal_ppt)
    
    observed_rollavg_rain = rolling_average_D_days(avg_rain = observed_rain, D = site_D())
    counter_rollavg_rain = rolling_average_D_days(avg_rain = counter_rain, D = site_D())
    
    anom = updated_standardize_rainfall(avg_rain = counter_rollavg_rain,
                                        save = F, file = "",
                                        observed_rollavg_rain = observed_rollavg_rain)
    
    filtered_anom = anom %>% filter(year >= start_year & year <= input$year_forecast) %>% select(anom, date, year)
    
    # # Create the plotly plot
    plot <- plot_ly(data = filtered_anom, x = ~date, y = ~anom,
                    type = 'scatter', mode = 'lines',
                    line = list(width = 2)
    ) %>%
      layout(
        title = " ",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Anomaly"),
        legend = list(
          title = list(text = ""),
          orientation = 'h',  # Horizontal legend
          x = 0.5,  # Center the legend horizontally
          xanchor = 'center',  # Anchor the x position at the center
          y = -0.2  # Position the legend below the plot
        )
      )
    plot
    
    
  })
  
  output$rain_plot_rollmean = renderPlotly({
    # Select the rain data only and change to long format
    rain_data = selected_climate_data() %>%
      select(1:6, "normal_ppt", "rainfall_scenario") %>%
      rename("Selected scenario" = "rainfall_scenario",
             "Counterfactual" = "normal_ppt") %>%
      pivot_longer(cols = c("Selected scenario", "Counterfactual"),
                   names_to = "rain_scenario",
                   values_to = "rain_value")
    
    # Apply rolling mean with window size D (define your desired window size)
    D = 7  # Example window size for a weekly rolling average
    rain_data = rain_data %>%
      group_by(rain_scenario) %>%
      mutate(rolling_avg = rollmean(rain_value, k = round(site_D()), fill = NA, align = "center")) %>%  # Calculate rolling average
      ungroup()
    
    # Define a custom color palette
    rain_colors = c("Selected scenario" = "#2b8cbe", "Counterfactual" = "grey")
    
    # Create the plotly plot with rolling averages
    plot = plot_ly(
      data = rain_data,
      x = ~date,
      y = ~rolling_avg,  # Use rolling average instead of raw values
      color = ~rain_scenario,
      type = 'scatter',
      mode = 'lines',
      line = list(width = 2),
      colors = rain_colors
    ) %>%
      layout(
        title = "Rolling Average Rainfall",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Rainfall (mm per day)"),
        legend = list(
          title = list(text = ""),
          orientation = 'h',
          x = 0.5,
          xanchor = 'center',
          y = -0.2
        )
      )
    
    plot
  })
  
  output$temp_plot = renderPlotly({
    # Select the rain data only and change to long format
    temp_data = selected_climate_data() %>% select(1:6, "normal_temp", "temperature_scenario")
    temp_data = temp_data %>% rename("Selected scenario" = "temperature_scenario",
                                     "Counterfactual" = "normal_temp")
    temp_data = temp_data %>% pivot_longer(cols = c("Selected scenario", "Counterfactual"),
                                           names_to = "temp_scenario",
                                           values_to = "temp_value")
    
    # Define a custom color palette (you can choose colors or use a predefined scale)
    rain_colors <- c("Selected scenario" = "#fd8d3c", "Counterfactual" = "grey")
    # Create the plotly plot
    plot <- plot_ly(data = temp_data, x = ~date, y = ~temp_value - 273.15, 
                    color = ~temp_scenario, 
                    type = 'scatter', mode = 'lines',
                    line = list(width = 2),
                    colors = rain_colors
    ) %>%
      layout(
        title = " ",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Temperature (\u00b0C)"),
        legend = list(
          title = list(text = ""),
          orientation = 'h',  # Horizontal legend
          x = 0.5,  # Center the legend horizontally
          xanchor = 'center',  # Anchor the x position at the center
          y = -0.2  # Position the legend below the plot
        )
      )
    plot
  })
  
  output$temp_plot_rollmean = renderPlotly({
    # Select the temperature data only and change to long format
    temp_data = selected_climate_data() %>%
      select(1:6, "normal_temp", "temperature_scenario") %>%
      rename("Selected scenario" = "temperature_scenario",
             "Counterfactual" = "normal_temp") %>%
      pivot_longer(cols = c("Selected scenario", "Counterfactual"),
                   names_to = "temp_scenario",
                   values_to = "temp_value")
    
    # Apply rolling mean with window size D corresponding to the site
    temp_data = temp_data %>%
      group_by(temp_scenario) %>%
      mutate(rolling_avg = rollmean(temp_value - 273.15, k = round(site_D()), fill = NA, align = "center")) %>%  # Apply rolling mean
      ungroup()
    
    # Define a custom color palette
    rain_colors = c("Selected scenario" = "#fd8d3c", "Counterfactual" = "grey")
    
    # Create the plotly plot with rolling averages
    plot = plot_ly(
      data = temp_data,
      x = ~date,
      y = ~rolling_avg,
      color = ~temp_scenario,
      type = 'scatter',
      mode = 'lines',
      line = list(width = 2),
      colors = rain_colors
    ) %>%
      layout(
        title = "Rolling Average Temperature",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Temperature (\u00b0C)"),
        legend = list(
          title = list(text = ""),
          orientation = 'h',
          x = 0.5,
          xanchor = 'center',
          y = -0.2
        )
      )
    
    plot
  })
  
  # Create the reactive plotly plot
  output$model_plot_monthly <- renderPlotly({
    # Identify the scenarios
    # Temperature
    # selected_temp = paste0(gsub(" ", "_", input$temperature_scenario), "_temp")
    # Rain depends on the seasonality scenario
    if (input$seasonality_scenario == "no change") {
      selected_rain = paste0(gsub(" ", "_", input$rainfall_scenario), "_ppt")
    } else {
      selected_rain = paste0(gsub(" ", "_", input$rainfall_scenario), 
                             "_seasonal_", gsub(" ", "_", input$seasonality_scenario), "_ppt")
    }
    
    # Select the results for the combination of scenarios
    model_pred_scen = model_pred %>% filter(year(date_ymd) >= start_year & 
                                              year(date_ymd) <= input$year_forecast &
                                              province == input$province_choice)
    model_pred_scen_1 = model_pred_scen %>% filter(temp_scen == selected_temp() &
                                                     rain_scen == selected_rain &
                                                     SMC_cov == SMC_coverage())
    
    model_pred_scen_1$scenario = "Selected climate and SMC scenario"
    
    model_pred_scen_2 = model_pred_scen %>% filter(temp_scen == "normal_temp" &
                                                     rain_scen == "normal_ppt" &
                                                     SMC_cov == 0)
    model_pred_scen_2$scenario = "Counterfactual\n(Normal climate, no SMC)"
    
    model_pred_scen_3 = model_pred_scen %>% filter(temp_scen == "normal_temp" &
                                                     rain_scen == "normal_ppt" &
                                                     SMC_cov == SMC_coverage())
    model_pred_scen_3$scenario = "Normal climate, selected SMC scenario"
    
    model_pred_scen_4 = model_pred_scen %>% filter(temp_scen == selected_temp() &
                                                     rain_scen == selected_rain &
                                                     SMC_cov == 0)
    model_pred_scen_4$scenario = "Selected climate scenario, no SMC"
    
    model_pred_scen = rbind.data.frame(model_pred_scen_1, model_pred_scen_2, model_pred_scen_3, model_pred_scen_4)
    model_pred_scen = as.data.frame(model_pred_scen)
    model_pred_scen$scenario = factor(model_pred_scen$scenario,
                                      levels = c("Selected climate scenario, no SMC",
                                                 "Normal climate, selected SMC scenario",
                                                 "Counterfactual\n(Normal climate, no SMC)", 
                                                 "Selected climate and SMC scenario"))
    model_colors <- c("Selected climate scenario, no SMC" = "#56B4E9",
                      "Normal climate, selected SMC scenario" = "#009E73",
                      "Counterfactual\n(Normal climate, no SMC)" = "grey",
                      "Selected climate and SMC scenario" = "#c51b8a")
    
    if (input$output_type == "inc") {
      if(input$age_groups == "moins de 5 ans") {
        model_out = "inc_5"
        plot_title = "Incidence per 10000 (<5yo)"
      } else {
        model_out = "inc_all"
        plot_title = "Incidence per 10000 (all ages)"
      }
    } else {
      model_out = "EIR"
      plot_title = "Entomological inoculation rate"
    }
    
    # Define hidden scenarios
    hidden_scenarios = c("Selected climate scenario, no SMC",
                         "Normal climate, selected SMC scenario")
    # Filter out the data for the hidden scenarios
    visible_data = model_pred_scen[!(model_pred_scen$scenario %in% hidden_scenarios), ]
    
    # Create the plotly plot
    plot = plot_ly(
      data = as.data.frame(visible_data), 
      x = ~date_ymd, 
      y = ~get(model_out), 
      color = ~scenario,
      type = 'scatter', 
      mode = 'lines+markers',
      line = list(width = 2),
      colors = model_colors
    ) 
    
    # Add the hidden scenarios but with `legendonly` visibility
    for (hidden_scenario in hidden_scenarios) {
      plot = plot %>%
        add_trace(
          data = as.data.frame(model_pred_scen[model_pred_scen$scenario == hidden_scenario, ]),
          x = ~date_ymd,
          y = ~get(model_out),
          type = 'scatter',
          mode = 'lines+markers',
          color = ~scenario,
          colors = model_colors,
          visible = 'legendonly'  # Hide these from the plot but show in legend
        )
    }
    
    # Adjust layout as necessary
    plot = plot %>%
      layout(
        title = "",
        xaxis = list(title = "Time (months)"),
        yaxis = list(title = plot_title),
        legend = list(
          title = list(text = ""),
          orientation = 'h',
          x = 0.5,
          xanchor = 'center',
          y = -0.2
        )
      )
    
    if (SMC_coverage() > 0) {
      # Find the first date where SMC is 1 for each year
      # first_smc_dates <- model_pred_scen %>%
      #   filter(SMC == 1) %>%
      #   mutate(year = format(date_ymd, "%Y")) %>%
      #   group_by(year) %>%
      #   slice(1) %>%
      #   ungroup() %>%
      #   select(date_ymd, year)
      smc_dates = model_pred_scen %>%
        filter(SMC == 1) %>%
        select(date_ymd) 
      
      # # Add arrows for the first month with SMC == 1 for each year
      # for (i in 1:nrow(first_smc_dates)) {
      #   plot <- plot %>%
      #     add_annotations(
      #       x = first_smc_dates$date_ymd[i], 
      #       y = max(model_pred_scen[[model_out]]),  # Adjust if needed
      #       text = "SMC",
      #       showarrow = TRUE, 
      #       arrowhead = 2,  # Customize arrow appearance
      #       ax = 0, 
      #       ay = -30,  # Adjust the vertical position
      #       arrowsize = 1, 
      #       arrowwidth = 2, 
      #       arrowcolor = "orange", 
      #       font = list(color = "black", size = 12)
      #     )
      # }
      
      # Add arrows for each month with SMC == 1
      for (i in 1:nrow(smc_dates)) {
        plot = plot %>%
          add_annotations(
            x = smc_dates$date_ymd[i], 
            y = max(model_pred_scen[[model_out]], na.rm = TRUE),  # Adjust if needed
            text = "",
            showarrow = TRUE, 
            arrowhead = 2,  # Customize arrow appearance
            ax = 0, 
            ay = -30,  # Adjust the vertical position
            arrowsize = 1, 
            arrowwidth = 2, 
            arrowcolor = "orange", 
            font = list(color = "black", size = 12)
          )
      }
      
      # smc_dates = model_pred_scen %>% filter(SMC == 1) %>% pull(date_ymd)
      # for (date in smc_dates) {
      #   plot = plot %>%
      #     add_segments(x = as.Date(date), xend = as.Date(date), y = min(model_pred_scen[[model_out]]), 
      #                  yend = max(model_pred_scen[[model_out]]), 
      #                  line = list(color = "orange", dash = "dashdot", width = 0.5),
      #                  showlegend = FALSE)
      # }
    }
    
    plot
  })
  
  output$model_plot_yearly <- renderPlotly({
    # Identify the scenarios
    # Temperature
    # selected_temp = paste0(gsub(" ", "_", input$temperature_scenario), "_temp")
    # Rain depends on the seasonality scenario
    if (input$seasonality_scenario == "no change") {
      selected_rain = paste0(gsub(" ", "_", input$rainfall_scenario), "_ppt")
    } else {
      selected_rain = paste0(gsub(" ", "_", input$rainfall_scenario), 
                             "_seasonal_", gsub(" ", "_", input$seasonality_scenario), "_ppt")
    }
    
    # Select the results for the combination of scenarios
    model_pred_scen = model_pred %>% filter(year(date_ymd) >= start_year & 
                                              year(date_ymd) <= input$year_forecast &
                                              province == input$province_choice)
    
    model_pred_scen_1 = model_pred_scen %>% filter(temp_scen == selected_temp() &
                                                     rain_scen == selected_rain &
                                                     SMC_cov == SMC_coverage())
    
    model_pred_scen_1$scenario = "Selected climate and SMC scenario"
    
    model_pred_scen_2 = model_pred_scen %>% filter(temp_scen == "normal_temp" &
                                                     rain_scen == "normal_ppt" &
                                                     SMC_cov == 0)
    model_pred_scen_2$scenario = "Counterfactual\n(Normal climate, no SMC)"
    
    model_pred_scen_3 = model_pred_scen %>% filter(temp_scen == "normal_temp" &
                                                     rain_scen == "normal_ppt" &
                                                     SMC_cov == SMC_coverage())
    model_pred_scen_3$scenario = "Normal climate, selected SMC scenario"
    
    model_pred_scen_4 = model_pred_scen %>% filter(temp_scen == selected_temp() &
                                                     rain_scen == selected_rain &
                                                     SMC_cov == 0)
    model_pred_scen_4$scenario = "Selected climate scenario, no SMC"
    
    model_pred_scen = rbind.data.frame(model_pred_scen_1, model_pred_scen_2, model_pred_scen_3, model_pred_scen_4)
    model_pred_scen = as.data.frame(model_pred_scen)
    model_pred_scen$scenario = factor(model_pred_scen$scenario,
                                      levels = c("Selected climate scenario, no SMC",
                                                 "Normal climate, selected SMC scenario",
                                                 "Counterfactual\n(Normal climate, no SMC)", 
                                                 "Selected climate and SMC scenario"))
    model_colors <- c("Selected climate scenario, no SMC" = "#56B4E9",
                      "Normal climate, selected SMC scenario" = "#009E73",
                      "Counterfactual\n(Normal climate, no SMC)" = "grey",
                      "Selected climate and SMC scenario" = "#c51b8a")
    
    model_pred_scen$year = year(model_pred_scen$date_ymd)
    model_pred_scen = model_pred_scen %>% group_by(year, scenario) %>% 
      summarise(EIR_year = sum(EIR),
                inc_5_year = sum(inc_5),
                inc_all_year = sum(inc_all))
    
    if (input$output_type == "inc") {
      if(input$age_groups == "moins de 5 ans") {
        model_out = "inc_5_year"
        plot_title = "Incidence per 10000 (<5yo)"
      } else {
        model_out = "inc_all_year"
        plot_title = "Incidence per 10000 (all ages)"
      }
      
    } else {
      model_out = "EIR_year"
      plot_title = "Entomological inoculation rate"
    }
    
    # Define hidden scenarios
    hidden_scenarios = c("Selected climate scenario, no SMC",
                         "Normal climate, selected SMC scenario")
    # Filter out the data for the hidden scenarios
    visible_data = model_pred_scen[!(model_pred_scen$scenario %in% hidden_scenarios), ]

    # Create the plotly plot
    plot = plot_ly(
      data = as.data.frame(visible_data), 
      x = ~year, 
      y = ~get(model_out), 
      color = ~scenario,
      type = 'scatter', 
      mode = 'lines+markers',
      line = list(width = 2),
      colors = model_colors
    ) 
    
    # Add the hidden scenarios but with `legendonly` visibility
    for (hidden_scenario in hidden_scenarios) {
      plot = plot %>%
        add_trace(
          data = as.data.frame(model_pred_scen[model_pred_scen$scenario == hidden_scenario, ]),
          x = ~year,
          y = ~get(model_out),
          type = 'scatter',
          mode = 'lines+markers',
          color = ~scenario,
          colors = model_colors,
          visible = 'legendonly'  # Hide these from the plot but show in legend
        )
    }
    
    # Adjust layout as necessary
    plot = plot %>%
      layout(
        title = "",
        xaxis = list(title = "Year"),
        yaxis = list(title = plot_title),
        legend = list(
          title = list(text = ""),
          orientation = 'h',
          x = 0.5,
          xanchor = 'center',
          y = -0.2
        )
      )
    
    plot
  })
  
}


