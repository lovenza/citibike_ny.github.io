# NYC Citi Bike Dashboard
# Fixed version for actual data columns

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(lubridate)
library(scales)

# Data loading with error handling
tryCatch({
  # Load bike data
  bike_data <- read_csv("bike_data_light.csv", show_col_types = FALSE) %>%
    mutate(
      # Parse dates
      started_at = ymd_hms(started_at, quiet = TRUE),
      ended_at = ymd_hms(ended_at, quiet = TRUE),
      start_date = as.Date(started_at),
      hour = hour(started_at),
      day_of_week = wday(started_at, label = TRUE),
      is_weekend = day_of_week %in% c("Sat", "Sun"),
      
      # Duration - use riding_time column (already in hours)
      ride_duration_min = riding_time * 60,  # Convert hours to minutes
      
      # Calculate distance using haversine formula (no external package needed)
      distance_km = {
        # Haversine formula
        R <- 6371  # Earth's radius in km
        lat1 <- start_lat * pi / 180
        lat2 <- end_lat * pi / 180
        dlat <- (end_lat - start_lat) * pi / 180
        dlon <- (end_lng - start_lng) * pi / 180
        
        a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
        c <- 2 * atan2(sqrt(a), sqrt(1-a))
        R * c
      },
      
      # Ensure member_casual is factor
      member_casual = factor(member_casual, levels = c("member", "casual"))
    ) %>%
    filter(!is.na(ride_duration_min), ride_duration_min > 0, ride_duration_min < 120,
           !is.na(distance_km), distance_km > 0, distance_km < 50)
  
  # Load weather data
  weather_data <- read_csv("noaa_central_park_2025.csv", show_col_types = FALSE) %>%
    # FIX: Rename the actual uppercase columns to lowercase to match the script's expectations.
    rename(date = DATE, 
           tmax = TMAX, 
           tmin = TMIN, 
           prcp = PRCP) %>%
    select(date, tmax, tmin, prcp)
  
  # Join weather data
  bike_data <- bike_data %>%
    left_join(weather_data, by = c("start_date" = "date"))
  
  cat("✓ Data loaded successfully!\n")
  cat("  Rows:", nrow(bike_data), "\n")
  cat("  Date range:", as.character(min(bike_data$start_date)), "to", 
      as.character(max(bike_data$start_date)), "\n")
  
}, error = function(e) {
  cat("✗ ERROR loading data:\n")
  cat(" ", conditionMessage(e), "\n")
  stop("Failed to load data files")
})

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "🚴 NYC Citi Bike", titleWidth = 250),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Temporal", tabName = "temporal", icon = icon("clock")),
      menuItem("Spatial", tabName = "spatial", icon = icon("map")),
      menuItem("Weather", tabName = "weather", icon = icon("cloud")),
      menuItem("Users", tabName = "users", icon = icon("users")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    ),
    hr(),
    selectInput("user_type", "User Type:", 
                c("All" = "all", "Member" = "member", "Casual" = "casual"),
                selected = "all"),
    sliderInput("date_range", "Date:", 
                min(bike_data$start_date, na.rm = TRUE), 
                max(bike_data$start_date, na.rm = TRUE),
                value = c(min(bike_data$start_date, na.rm = TRUE), 
                          max(bike_data$start_date, na.rm = TRUE))),
    sliderInput("hour_range", "Hours:", 0, 23, c(0, 23)),
    checkboxInput("weekend_only", "Weekends Only", value = FALSE),
    actionButton("reset", "Reset", icon = icon("refresh")),
    hr(),
    textOutput("filter_info")
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem("overview",
              h2("Overview"),
              fluidRow(
                valueBoxOutput("total_rides", 3),
                valueBoxOutput("avg_duration", 3),
                valueBoxOutput("avg_distance", 3),
                valueBoxOutput("unique_stations", 3)
              ),
              fluidRow(
                box(title = "Rides Over Time", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("rides_time", height = 250))
              ),
              fluidRow(
                box(title = "Hourly Distribution", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("hourly", height = 250)),
                box(title = "User Type Split", status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("user_pie", height = 250))
              )
      ),
      
      # Temporal Tab
      tabItem("temporal",
              h2("Temporal Patterns"),
              fluidRow(
                box(title = "Hourly Pattern", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("hourly_pattern", height = 300)),
                box(title = "By Day of Week", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("daily_pattern", height = 300))
              ),
              fluidRow(
                box(title = "Weekend vs Weekday", status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("weekend_comp", height = 300)),
                box(title = "Duration Distribution", status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("duration_hist", height = 300))
              )
      ),
      
      # Spatial Tab
      tabItem("spatial",
              h2("Spatial Analysis"),
              fluidRow(
                box(title = "Station Map", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("map", height = 400))
              ),
              fluidRow(
                box(title = "Top 10 Start Stations", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("top_start", height = 300)),
                box(title = "Top 10 End Stations", status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("top_end", height = 300))
              )
      ),
      
      # Weather Tab
      tabItem("weather",
              h2("Weather Impact"),
              fluidRow(
                box(title = "Rides vs Temperature", status = "danger", solidHeader = TRUE, width = 6,
                    plotlyOutput("rides_temp", height = 300)),
                box(title = "Rides vs Precipitation", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("rides_precip", height = 300))
              ),
              fluidRow(
                box(title = "Temperature Distribution", status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("temp_hist", height = 300)),
                box(title = "Weather Summary", status = "info", solidHeader = TRUE, width = 6,
                    DTOutput("weather_table"))
              )
      ),
      
      # Users Tab
      tabItem("users",
              h2("Member vs Casual Comparison"),
              fluidRow(
                box(title = "Duration Comparison", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("duration_box", height = 300)),
                box(title = "Hourly Usage Pattern", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("hourly_user", height = 300))
              ),
              fluidRow(
                box(title = "Bike Type Preference", status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("bike_type", height = 300)),
                box(title = "Weekend vs Weekday Usage", status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("weekend_user", height = 300))
              )
      ),
      
      # Data Tab
      tabItem("data",
              h2("Data Explorer"),
              fluidRow(
                box(title = "Filtered Data Table", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("data_table"),
                    downloadButton("download", "Download Filtered Data"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtering
  filtered <- reactive({
    d <- bike_data
    
    if (input$user_type != "all") {
      d <- d %>% filter(member_casual == input$user_type)
    }
    
    d <- d %>% filter(
      start_date >= input$date_range[1], 
      start_date <= input$date_range[2],
      hour >= input$hour_range[1], 
      hour <= input$hour_range[2]
    )
    
    if (input$weekend_only) {
      d <- d %>% filter(is_weekend == TRUE)
    }
    
    # Explicitly check if the result is empty or not a data frame, and return NULL if so
    if (nrow(d) == 0) {
      return(NULL)
    }
    
    d
  })
  
  # Reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "user_type", selected = "all")
    updateSliderInput(session, "date_range", 
                      value = c(min(bike_data$start_date), max(bike_data$start_date)))
    updateSliderInput(session, "hour_range", value = c(0, 23))
    updateCheckboxInput(session, "weekend_only", value = FALSE)
  })
  
  # Filter info
  output$filter_info <- renderText({
    # Use validate here too, to prevent comma(NULL) error
    validate(
      need(!is.null(filtered()), "No data matches the current filters.")
    )
    sprintf("%s rides", comma(nrow(filtered())))
  })
  
  # Value boxes (No need for validate if using safe functions like mean(NA.rm=T) on a reactive)
  output$total_rides <- renderValueBox({
    validate(
      need(!is.null(filtered()), "0")
    )
    valueBox(comma(nrow(filtered())), "Total Rides", icon("bicycle"), "purple")
  })
  
  output$avg_duration <- renderValueBox({
    validate(
      need(!is.null(filtered()), "0.0 min")
    )
    valueBox(
      sprintf("%.1f min", mean(filtered()$ride_duration_min, na.rm = TRUE)), 
      "Avg Duration", icon("clock"), "blue"
    )
  })
  
  output$avg_distance <- renderValueBox({
    validate(
      need(!is.null(filtered()), "0.00 km")
    )
    valueBox(
      sprintf("%.2f km", mean(filtered()$distance_km, na.rm = TRUE)), 
      "Avg Distance", icon("road"), "green"
    )
  })
  
  output$unique_stations <- renderValueBox({
    validate(
      need(!is.null(filtered()), "0")
    )
    n_stations <- length(unique(c(filtered()$start_station_name, filtered()$end_station_name)))
    valueBox(n_stations, "Unique Stations", icon("map-marker"), "orange")
  })
  
  # Plots (validate() added to all plot and table outputs)
  output$rides_time <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    daily <- filtered() %>% count(start_date)
    plot_ly(daily, x = ~start_date, y = ~n, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#0066cc')) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Number of Rides"))
  })
  
  output$hourly <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    hourly <- filtered() %>% count(hour)
    plot_ly(hourly, x = ~hour, y = ~n, type = 'bar', marker = list(color = '#667eea')) %>%
      layout(xaxis = list(title = "Hour of Day"), yaxis = list(title = "Number of Rides"))
  })
  
  output$user_pie <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    user_counts <- filtered() %>% count(member_casual)
    plot_ly(user_counts, labels = ~member_casual, values = ~n, type = 'pie',
            marker = list(colors = c('#0066cc', '#764ba2')))
  })
  
  output$hourly_pattern <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    d <- filtered() %>% 
      mutate(type = ifelse(is_weekend, "Weekend", "Weekday")) %>%
      group_by(hour, type) %>% 
      summarise(n = n(), .groups = "drop")
    
    plot_ly(d, x = ~hour, y = ~n, color = ~type, type = 'scatter', mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Hour"), yaxis = list(title = "Rides"))
  })
  
  output$daily_pattern <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    daily <- filtered() %>% count(day_of_week)
    plot_ly(daily, x = ~day_of_week, y = ~n, type = 'bar', marker = list(color = '#667eea')) %>%
      layout(xaxis = list(title = "Day"), yaxis = list(title = "Rides"))
  })
  
  output$weekend_comp <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    d <- filtered() %>% 
      mutate(type = ifelse(is_weekend, "Weekend", "Weekday")) %>%
      count(type, member_casual)
    
    plot_ly(d, x = ~type, y = ~n, color = ~member_casual, type = 'bar') %>%
      layout(barmode = 'group')
  })
  
  output$duration_hist <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    plot_ly(filtered(), x = ~ride_duration_min, type = 'histogram', nbinsx = 50,
            marker = list(color = '#667eea')) %>%
      layout(xaxis = list(title = "Duration (minutes)"), yaxis = list(title = "Count"))
  })
  
  output$map <- renderLeaflet({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    top <- filtered() %>% 
      group_by(start_station_name, start_lat, start_lng) %>%
      summarise(n = n(), .groups = "drop") %>% 
      filter(!is.na(start_lat), !is.na(start_lng)) %>% 
      arrange(desc(n)) %>% 
      head(100)
    
    leaflet(top) %>% 
      addTiles() %>%
      addCircleMarkers(
        lng = ~start_lng, lat = ~start_lat, 
        radius = ~sqrt(n) / 5,
        color = "#0066cc",
        fillColor = "#667eea",
        fillOpacity = 0.6,
        popup = ~paste0("<b>", start_station_name, "</b><br>", comma(n), " rides")
      )
  })
  
  output$top_start <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    d <- filtered() %>% 
      count(start_station_name, sort = TRUE) %>% 
      head(10) %>%
      mutate(start_station_name = fct_reorder(start_station_name, n))
    
    plot_ly(d, x = ~n, y = ~start_station_name, type = 'bar', orientation = 'h',
            marker = list(color = '#0066cc')) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Number of Rides"),
             margin = list(l = 150))
  })
  
  output$top_end <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    d <- filtered() %>% 
      count(end_station_name, sort = TRUE) %>% 
      head(10) %>%
      mutate(end_station_name = fct_reorder(end_station_name, n))
    
    plot_ly(d, x = ~n, y = ~end_station_name, type = 'bar', orientation = 'h',
            marker = list(color = '#764ba2')) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Number of Rides"),
             margin = list(l = 150))
  })
  
  output$rides_temp <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    d <- filtered() %>% 
      group_by(start_date, tmax) %>% 
      summarise(n = n(), .groups = "drop")
    
    plot_ly(d, x = ~tmax, y = ~n, type = 'scatter', mode = 'markers',
            marker = list(color = '#667eea', size = 10)) %>%
      layout(xaxis = list(title = "Max Temperature (°F)"), 
             yaxis = list(title = "Number of Rides"))
  })
  
  output$rides_precip <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    d <- filtered() %>% 
      group_by(start_date, prcp) %>% 
      summarise(n = n(), .groups = "drop")
    
    plot_ly(d, x = ~prcp, y = ~n, type = 'scatter', mode = 'markers',
            marker = list(color = '#0066cc', size = 10)) %>%
      layout(xaxis = list(title = "Precipitation (inches)"), 
             yaxis = list(title = "Number of Rides"))
  })
  
  output$temp_hist <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    temp_data <- filtered() %>% distinct(start_date, .keep_all = TRUE)
    plot_ly(temp_data, x = ~tmax, type = 'histogram', marker = list(color = '#764ba2')) %>%
      layout(xaxis = list(title = "Max Temperature (°F)"), yaxis = list(title = "Count"))
  })
  
  output$weather_table <- renderDT({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    filtered() %>% 
      group_by(start_date) %>%
      summarise(
        Date = first(start_date), 
        `Max Temp (°F)` = first(tmax),
        `Min Temp (°F)` = first(tmin), 
        `Precip (in)` = first(prcp), 
        Rides = n(), 
        .groups = "drop"
      ) %>%
      select(-start_date) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$duration_box <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    plot_ly(filtered(), x = ~member_casual, y = ~ride_duration_min, 
            type = 'box', color = ~member_casual,
            colors = c('#0066cc', '#764ba2')) %>%
      layout(showlegend = FALSE, xaxis = list(title = "User Type"),
             yaxis = list(title = "Ride Duration (minutes)"))
  })
  
  output$hourly_user <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    d <- filtered() %>% 
      group_by(hour, member_casual) %>% 
      summarise(n = n(), .groups = "drop")
    
    plot_ly(d, x = ~hour, y = ~n, color = ~member_casual, 
            type = 'scatter', mode = 'lines+markers',
            colors = c('#0066cc', '#764ba2')) %>%
      layout(xaxis = list(title = "Hour of Day"), 
             yaxis = list(title = "Number of Rides"))
  })
  
  output$bike_type <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    d <- filtered() %>% 
      count(member_casual, rideable_type) %>%
      group_by(member_casual) %>% 
      mutate(pct = n / sum(n) * 100)
    
    plot_ly(d, x = ~member_casual, y = ~pct, color = ~rideable_type, type = 'bar') %>%
      layout(barmode = 'stack', yaxis = list(title = "Percentage (%)"),
             xaxis = list(title = "User Type"))
  })
  
  output$weekend_user <- renderPlotly({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    d <- filtered() %>% 
      mutate(type = ifelse(is_weekend, "Weekend", "Weekday")) %>%
      count(member_casual, type) %>% 
      group_by(member_casual) %>% 
      mutate(pct = n / sum(n) * 100)
    
    plot_ly(d, x = ~member_casual, y = ~pct, color = ~type, type = 'bar',
            colors = c('#0066cc', '#764ba2')) %>%
      layout(barmode = 'group', yaxis = list(title = "Percentage (%)"),
             xaxis = list(title = "User Type"))
  })
  
  output$data_table <- renderDT({
    validate(
      need(!is.null(filtered()), "Please select filters to display data.")
    )
    filtered() %>% 
      select(started_at, start_station_name, end_station_name,
             member_casual, rideable_type, distance_km, ride_duration_min, 
             tmax, prcp) %>%
      head(1000) %>%
      datatable(options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("citibike_filtered_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)