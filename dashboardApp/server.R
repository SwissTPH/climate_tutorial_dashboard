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
#library(malclimsim)

function(input, output, session) {
  
  # Load model and  data
  climate_data = readRDS("data/climate_data_province.RDS")
  province_params = readRDS("data/province_sites_parameters.RDS")
  model_pred = readRDS("data/model_predictions_province.RDS")
  start_year = 2020
  
  # observeEvent(input$reset_button, {
  #   # Toggle visibility of the div when the title is clicked
  #   toggle("climate_div")
  #   print("here")
  # })
  
  # runjs('
  #   $("#climate_title").click(function() {
  #     $("#climate_div").toggle();
  #   });
  # ')
  
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
  
  # Define a reactive variable based on the slider input
  selected_site = reactive({
    province_params %>% filter(Province == input$province_choice) %>% select(Site)
  })
  
  # Dynamically update site name based on selection
  output$selected_site_name <- renderText({
    paste("Site name:", selected_site())
  })
  
  # Define a reactive variable based on the slider input
  selected_climate_data = reactive({
    # Identify the selected time frame to forecast
    selected_year = input$yearForecast
    
    # Identify the scenarios
    selected_rain = paste0(gsub(" ", "_", input$rainfall_scenario), "_ppt")
    selected_temp = paste0(gsub(" ", "_", input$temperature_scenario), "_temp")
    selected_seasonality = paste0(gsub(" ", "_", input$seasonality_scenario), "_seasonal_ppt")
    scenarios = c(selected_rain, selected_temp, selected_seasonality)
    
    # Identify scenario columns for the future projections
    future_columns = climate_data %>% select(where(~ any(str_detect(names(climate_data), 
                                                                    str_c(scenarios, collapse = "|")))))
    
    # Define new columns with the scenario values
    climate_data$temperature_scenario = climate_data[[selected_temp]]
    
    if (selected_rain != "normal_ppt") {
      climate_data$rainfall_scenario = climate_data[[selected_rain]]
    } else {
      climate_data$rainfall_scenario = climate_data[[selected_seasonality]]
    }

    # Define list of columns to select
    all_columns = c(colnames(climate_data)[1:7], 
                    "normal_temp", "no_change_seasonal_ppt",
                    "rainfall_scenario", "temperature_scenario")
    
    # Select corresponding data
    climate_data %>% filter(province == input$province_choice &
                            yr >= start_year & yr <= input$year_forecast) %>% 
                     select(all_columns)
    
  })
  
  output$rain_plot = renderPlotly({
    # Select the rain data only and change to long format
    rain_data = selected_climate_data() %>% select(1:7, "no_change_seasonal_ppt", "rainfall_scenario")
    rain_data = rain_data %>% rename("Selected scenario" = "rainfall_scenario",
                                     "Counterfactual" = "no_change_seasonal_ppt")
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
  
  output$temp_plot = renderPlotly({
    # Select the rain data only and change to long format
    temp_data = selected_climate_data() %>% select(1:7, "normal_temp", "temperature_scenario")
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
  
  # Create the reactive plotly plot
  output$model_plot <- renderPlotly({
    # Identify the scenarios
    selected_rain = paste0(gsub(" ", "_", input$rainfall_scenario), "_ppt")
    selected_temp = paste0(gsub(" ", "_", input$temperature_scenario), "_temp")
    selected_seasonality = paste0(gsub(" ", "_", input$seasonality_scenario), "_seasonal_ppt")
    if (selected_rain == "normal_ppt") {
      selected_rain = selected_seasonality
    }
    
    # Select the results for the combination of scenarios
    model_pred_scen = model_pred %>% filter(year(date_ymd) >= start_year & 
                                            year(date_ymd) <= input$year_forecast &
                                            province == input$province_choice)
    model_pred_scen_1 = model_pred_scen %>% filter(temp_scen == selected_temp &
                                                  rain_scen == selected_rain)
    model_pred_scen_1$scenario = "Selected scenario"
    model_pred_scen_2 = model_pred_scen %>% filter(temp_scen == "normal_temp" &
                                                     rain_scen == "no_change_seasonal_ppt")
    model_pred_scen_2$scenario = "Counterfactual"
    model_pred_scen = rbind.data.frame(model_pred_scen_1, model_pred_scen_2)
    
    if (input$output_type == "inc") {
      if(input$age_groups == "under 5 years") {
        model_out = "inc_5"
        plot_title = "Incidence (<5yo)"
      } else {
        model_out = "inc_all"
        plot_title = "Incidence (all ages)"
      }
      
    } else {
      model_out = "EIR"
      plot_title = "Entomological inoculation rate"
    }
    
    model_colors <- c("Selected scenario" = "#c51b8a", "Counterfactual" = "grey")
    # Create the plotly plot
    plot <- plot_ly(
      data = model_pred_scen, 
      x = ~date_ymd, 
      y = ~get(model_out), 
      color = ~scenario,
      type = 'scatter', 
      mode = 'lines',
      line = list(width = 2),
      colors = model_colors
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Year"),
        yaxis = list(title = plot_title),
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
    
}


