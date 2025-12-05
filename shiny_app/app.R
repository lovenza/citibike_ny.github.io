library(shiny)
library(bs4Dash)
library(tidyverse)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(DT)
library(fresh)
library(shinycssloaders)
library(shinyWidgets)

my_theme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#2c3e50",
    sidebar_light_bg = "#ffffff",
    main_bg = "#f4f6f9",
    primary = "#2C3E50",
    secondary = "#95a5a6",
    success = "#18BC9C",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C"
  ),
  bs4dash_sidebar_light(
    bg = "#ffffff",
    color = "#2c3e50",
    hover_color = "#3498DB"
  ),
  bs4dash_status(
    primary = "#2C3E50",
    danger = "#E74C3C",
    info = "#3498DB",
    success = "#18BC9C",
    warning = "#F39C12"
  )
)

bike_data <- tryCatch({
  possible_paths <- c(
    "data/final_bike_weather_categorized.csv",
    "final_bike_weather_categorized.csv"
  )
  path <- possible_paths[file.exists(possible_paths)][1]
  
  if (is.na(path)) stop("Data file not found")
  
  read_csv(path, show_col_types = FALSE) %>%
    mutate(
      start_date = as.Date(start_date),
      start_lat = as.numeric(start_lat),
      start_lng = as.numeric(start_lng),
      end_lat = as.numeric(end_lat),
      end_lng = as.numeric(end_lng),
      member_casual = factor(member_casual, levels = c("member", "casual")),
      rain_category = factor(rain_category, levels = c("No Rain", "Light Rain", "Medium Rain", "Heavy Rain")),
      ride_duration_min = ride_duration_hours * 60,
      hour = hour(started_at)
    ) %>%
    filter(!is.na(start_lat), !is.na(start_lng), !is.na(member_casual))
}, error = function(e) {
  data.frame(
    start_date = Sys.Date(),
    start_lat = 40.7, start_lng = -74.0,
    end_lat = 40.7, end_lng = -74.0,
    member_casual = factor("member", levels = c("member", "casual")),
    rain_category = factor("No Rain", levels = c("No Rain", "Light Rain", "Medium Rain", "Heavy Rain")),
    ride_duration_min = 10,
    ride_duration_hours = 0.16,
    hour = 12,
    weekend_and_holiday = "weekday",
    start_station_name = "Test Station",
    end_station_name = "Test Station",
    tmax = 70, prcp = 0, rideable_type = "classic_bike"
  )[0, ]
})

color_palette <- c("member" = "#2C3E50", "casual" = "#E74C3C")

ui <- bs4DashPage(
  title = "NYC Citi Bike 2025",
  freshTheme = my_theme,
  dark = NULL,
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "🚲 Citi Bike NYC",
      color = "primary",
      href = "#",
      image = NULL
    ),
    fixed = TRUE
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    elevation = 3,
    sidebarMenu(
      menuItem("Executive Summary", tabName = "overview", icon = icon("chart-line")),
      menuItem("Spatial Analysis", tabName = "spatial", icon = icon("map-marked-alt")),
      menuItem("Weather Impact", tabName = "weather", icon = icon("cloud-sun-rain")),
      menuItem("User Behavior", tabName = "users", icon = icon("users")),
      menuItem("Model Insights", tabName = "models", icon = icon("brain")),
      menuItem("Raw Data", tabName = "data", icon = icon("database"))
    ),
    
    div(style = "padding: 15px;",
        h5("Global Filters", style = "color: #7f8c8d; font-size: 0.8rem; text-transform: uppercase; letter-spacing: 1px; font-weight: bold;"),
        hr(),
        
        pickerInput("user_type", "User Type:",
                    choices = c("Annual Member" = "member", "Casual Rider" = "casual"),
                    selected = c("member", "casual"),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        
        sliderInput("date_range", "Date Range:",
                    min = if(nrow(bike_data) > 0) min(bike_data$start_date) else Sys.Date(),
                    max = if(nrow(bike_data) > 0) max(bike_data$start_date) else Sys.Date(),
                    value = if(nrow(bike_data) > 0) c(min(bike_data$start_date), max(bike_data$start_date)) else c(Sys.Date(), Sys.Date()),
                    timeFormat = "%b %d", ticks = FALSE),
        
        sliderInput("hour_range", "Time of Day:", min = 0, max = 23, value = c(0, 23), step = 1, ticks = FALSE),
        
        prettyCheckbox("weekend_only", "Weekends Only", value = FALSE, icon = icon("check"), status = "success", animation = "smooth"),
        
        actionButton("reset", "Reset Filters", icon = icon("undo"), width = "100%", class = "btn-outline-secondary")
    )
  ),
  
  body = dashboardBody(
    tags$head(tags$style(HTML("
      .small-box .inner h3 { font-size: 2rem; }
      .card { border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
      .content-wrapper { background-color: #f4f6f9; }
    "))),
    
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_rides", width = 3),
                valueBoxOutput("avg_duration", width = 3),
                valueBoxOutput("avg_distance", width = 3),
                valueBoxOutput("total_rain_impact", width = 3)
              ),
              fluidRow(
                box(
                  title = "Daily Ridership Trend", width = 12, status = "primary", solidHeader = TRUE,
                  maximizable = TRUE, collapsible = TRUE,
                  plotlyOutput("trend_plot", height = "350px") %>% withSpinner(type = 6, color = "#2C3E50")
                )
              ),
              fluidRow(
                box(
                  title = "Hourly Demand Heatmap", width = 6, status = "info", solidHeader = TRUE,
                  plotlyOutput("hourly_heatmap", height = "300px")
                ),
                box(
                  title = "User Split", width = 6, status = "info", solidHeader = TRUE,
                  plotlyOutput("user_pie", height = "300px")
                )
              )
      ),
      
      tabItem(tabName = "spatial",
              fluidRow(
                box(
                  title = "Ridership Routes & Top Stations", width = 12, height = "650px",
                  status = "primary", solidHeader = TRUE, maximizable = TRUE,
                  leafletOutput("main_map", height = "600px") %>% withSpinner(type = 6, color = "#2C3E50")
                )
              ),
              fluidRow(
                box(title = "Top 10 Start Stations", width = 6, status = "warning", plotlyOutput("top_start_plot", height = "300px")),
                box(title = "Top 10 End Stations", width = 6, status = "success", plotlyOutput("top_end_plot", height = "300px"))
              )
      ),
      
      tabItem(tabName = "weather",
              fluidRow(
                column(12,
                       bs4Callout(
                         title = "Key Insight", status = "info", width = 12, elevation = 2,
                         "Casual riders are significantly more sensitive to rain than Members. Commuters ride rain or shine!"
                       )
                )
              ),
              fluidRow(
                box(
                  title = "Temperature vs. Volume", width = 6, status = "warning", solidHeader = TRUE,
                  plotlyOutput("temp_scatter", height = "350px")
                ),
                box(
                  title = "Rain Impact Comparison", width = 6, status = "primary", solidHeader = TRUE,
                  plotlyOutput("rain_box", height = "350px")
                )
              )
      ),
      
      tabItem(tabName = "users",
              fluidRow(
                box(
                  title = "Ride Duration Distribution (Member vs Casual)", width = 12, status = "info", solidHeader = TRUE,
                  plotlyOutput("duration_density", height = "400px") %>% withSpinner(color = "#3498DB")
                )
              ),
              fluidRow(
                box(title = "Weekend Preference", width = 6, status = "primary", plotlyOutput("weekend_prop_plot")),
                box(title = "Bike Type Preference", width = 6, status = "success", plotlyOutput("bike_type_plot"))
              )
      ),
      
      tabItem(tabName = "models",
              h3("Statistical Analysis Results", style = "margin-bottom: 20px;"),
              fluidRow(
                box(
                  title = "What drives Ridership Volume?", width = 6, status = "success", solidHeader = TRUE,
                  plotlyOutput("coef_plot_volume", height = "400px"),
                  footer = "Positive values increase ridership, negative values decrease it."
                ),
                box(
                  title = "What drives Ride Duration?", width = 6, status = "warning", solidHeader = TRUE,
                  plotlyOutput("coef_plot_duration", height = "400px"),
                  footer = "Distance is the dominant factor. Weather has minimal impact on duration."
                )
              )
      ),
      
      tabItem(tabName = "data",
              box(
                title = "Filtered Dataset", width = 12, status = "primary", solidHeader = TRUE,
                DTOutput("raw_table"),
                downloadButton("download_csv", "Download CSV", class = "btn-success")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  if (nrow(bike_data) == 0) {
    showNotification("No data loaded. Please check file paths.", type = "error", duration = NULL)
  }
  
  filtered_raw <- reactive({
    req(input$date_range, input$user_type)
    
    d <- bike_data %>%
      filter(
        start_date >= input$date_range[1],
        start_date <= input$date_range[2],
        hour >= input$hour_range[1],
        hour <= input$hour_range[2],
        member_casual %in% input$user_type
      )
    
    if(input$weekend_only) {
      d <- d %>% filter(weekend_and_holiday == "weekend and holiday")
    }
    
    d
  })
  
  filtered_data <- filtered_raw %>% debounce(800)
  
  observeEvent(input$reset, {
    updatePickerInput(session, "user_type", selected = c("member", "casual"))
    updateSliderInput(session, "date_range", value = c(min(bike_data$start_date), max(bike_data$start_date)))
    updateSliderInput(session, "hour_range", value = c(0, 23))
    updatePrettyCheckbox(session, "weekend_only", value = FALSE)
  })
  
  output$total_rides <- renderValueBox({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    valueBox(
      value = format(nrow(filtered_data()), big.mark = ","),
      subtitle = "Total Rides", icon = icon("bicycle"), color = "primary"
    )
  })
  
  output$avg_duration <- renderValueBox({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    valueBox(
      value = sprintf("%.1f min", mean(filtered_data()$ride_duration_min, na.rm = TRUE)),
      subtitle = "Avg Duration", icon = icon("stopwatch"), color = "info"
    )
  })
  
  output$avg_distance <- renderValueBox({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    valueBox(
      value = sprintf("%.2f km", mean(filtered_data()$distance_km, na.rm = TRUE)),
      subtitle = "Avg Distance", icon = icon("ruler-horizontal"), color = "success"
    )
  })
  
  output$total_rain_impact <- renderValueBox({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    rainy_rides <- sum(filtered_data()$prcp > 0, na.rm = TRUE)
    total <- nrow(filtered_data())
    pct <- ifelse(total > 0, (rainy_rides/total)*100, 0)
    valueBox(
      value = sprintf("%.1f%%", pct),
      subtitle = "Rides in Rain", icon = icon("cloud-showers-heavy"), color = "danger"
    )
  })
  
  output$trend_plot <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No data matching filters"))
    daily <- filtered_data() %>% count(start_date, member_casual)
    
    p <- ggplot(daily, aes(x = start_date, y = n, color = member_casual)) +
      geom_line(size = 1) + geom_point(size = 1.5, alpha = 0.8) +
      scale_color_manual(values = color_palette) +
      theme_minimal() + 
      labs(x = "", y = "Daily Rides", color = "User Type") +
      theme(legend.position = "top")
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.1, y = 1.1))
  })
  
  output$user_pie <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    df <- filtered_data() %>% count(member_casual)
    plot_ly(df, labels = ~member_casual, values = ~n, type = 'pie',
            marker = list(colors = unname(color_palette[as.character(df$member_casual)])),
            textinfo = 'label+percent',
            hole = 0.4) %>%
      layout(showlegend = TRUE, legend = list(orientation = "h"))
  })
  
  output$hourly_heatmap <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    df <- filtered_data() %>% count(hour, member_casual)
    p <- ggplot(df, aes(x = hour, y = n, fill = member_casual)) +
      geom_col(position = "dodge", alpha = 0.9) +
      scale_fill_manual(values = color_palette) +
      theme_minimal() + labs(x = "Hour of Day", y = "Ride Volume")
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.1, y = 1.1))
  })
  
  output$main_map <- renderLeaflet({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    
    data_sample <- filtered_data()
    
    stations <- data_sample %>%
      group_by(start_station_name, start_lat, start_lng) %>%
      summarise(rides = n(), .groups = "drop") %>%
      arrange(desc(rides)) %>%
      head(200)
    
    heat_data <- data_sample %>%
      select(start_lat, start_lng) %>%
      slice_sample(n = min(nrow(.), 5000))
    
    route_data <- data_sample %>%
      filter(!is.na(end_lat), !is.na(end_lng)) %>%
      slice_sample(n = min(nrow(.), 100))
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addHeatmap(data = heat_data, lng = ~start_lng, lat = ~start_lat,
                 intensity = 0.6, blur = 25, max = 0.05, radius = 15)
    
    if(nrow(route_data) > 0) {
      for(i in 1:nrow(route_data)) {
        map <- map %>%
          addPolylines(
            lat = c(route_data$start_lat[i], route_data$end_lat[i]),
            lng = c(route_data$start_lng[i], route_data$end_lng[i]),
            weight = 1, color = "#3498DB", opacity = 0.4
          )
      }
    }
    
    map %>%
      addCircleMarkers(
        data = stations, lng = ~start_lng, lat = ~start_lat,
        radius = ~sqrt(rides)/2 + 2,
        color = "#E67E22", weight = 1, opacity = 1,
        fillColor = "#2C3E50", fillOpacity = 0.8,
        popup = ~paste("<b>", start_station_name, "</b><br>Rides:", format(rides, big.mark = ","))
      )
  })
  
  output$top_start_plot <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    df <- filtered_data() %>% count(start_station_name, sort = TRUE) %>% head(10)
    plot_ly(df, x = ~n, y = ~reorder(start_station_name, n), type = 'bar', orientation = 'h',
            marker = list(color = "#34495E")) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Rides"))
  })
  
  output$top_end_plot <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    df <- filtered_data() %>% count(end_station_name, sort = TRUE) %>% head(10)
    plot_ly(df, x = ~n, y = ~reorder(end_station_name, n), type = 'bar', orientation = 'h',
            marker = list(color = "#18BC9C")) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Rides"))
  })
  
  output$temp_scatter <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    daily <- filtered_data() %>% 
      group_by(start_date, tmax, member_casual) %>% 
      summarise(rides = n(), .groups = "drop")
    
    p <- ggplot(daily, aes(x = tmax, y = rides, color = member_casual)) +
      geom_point(alpha = 0.6) + 
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
      scale_color_manual(values = color_palette) +
      theme_minimal() + labs(x = "Max Temp (°F)", y = "Daily Rides")
    ggplotly(p)
  })
  
  output$rain_box <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    daily <- filtered_data() %>%
      group_by(start_date, rain_category, member_casual) %>%
      summarise(rides = n(), .groups = "drop") %>%
      filter(!is.na(rain_category))
    
    p <- ggplot(daily, aes(x = rain_category, y = rides, fill = member_casual)) +
      geom_boxplot(outlier.shape = NA) + 
      scale_fill_manual(values = color_palette) +
      theme_minimal() + labs(x = "Rain Condition", y = "Daily Rides")
    ggplotly(p) %>% layout(boxmode = "group")
  })
  
  output$duration_density <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    df <- filtered_data() %>% filter(ride_duration_min < 60)
    p <- ggplot(df, aes(x = ride_duration_min, fill = member_casual)) +
      geom_density(alpha = 0.5, adjust = 2) + 
      scale_fill_manual(values = color_palette) +
      theme_minimal() + labs(x = "Duration (min)", y = "Density")
    ggplotly(p)
  })
  
  output$weekend_prop_plot <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    df <- filtered_data() %>% 
      count(member_casual, weekend_and_holiday) %>% 
      group_by(member_casual) %>% 
      mutate(prop = n/sum(n))
    
    p <- ggplot(df, aes(x = member_casual, y = prop, fill = weekend_and_holiday)) +
      geom_col() + scale_fill_brewer(palette = "Blues") +
      scale_y_continuous(labels = scales::percent) + 
      theme_minimal() + labs(x = "", y = "Proportion", fill = "Day Type")
    ggplotly(p)
  })
  
  output$bike_type_plot <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    df <- filtered_data() %>% count(member_casual, rideable_type)
    plot_ly(df, x = ~member_casual, y = ~n, color = ~rideable_type, type = 'bar') %>%
      layout(barmode = 'stack')
  })
  
  output$coef_plot_volume <- renderPlotly({
    coef_data <- data.frame(
      Term = c("Temp (+10°F)", "Rain (+1 inch)", "Weekend"),
      Estimate = c(7.5, -27.0, -12.0)
    )
    plot_ly(coef_data, x = ~Estimate, y = ~reorder(Term, Estimate), type = 'bar', orientation = 'h',
            marker = list(color = ifelse(coef_data$Estimate > 0, "#18BC9C", "#E74C3C"))) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "% Change in Volume"))
  })
  
  output$coef_plot_duration <- renderPlotly({
    coef_data <- data.frame(
      Term = c("Dist (+1 km)", "Temp (+10°F)", "Rain (+1 inch)", "Casual User"),
      Estimate = c(4.3, 0.2, -0.5, 6.5)
    )
    plot_ly(coef_data, x = ~Estimate, y = ~reorder(Term, Estimate), type = 'bar', orientation = 'h',
            marker = list(color = "#3498DB")) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Minutes Change"))
  })
  
  output$raw_table <- renderDT({
    validate(need(nrow(filtered_data()) > 0, "No Data"))
    filtered_data() %>%
      select(Date=start_date, Station=start_station_name, User=member_casual,
             Dur=ride_duration_min, Dist=distance_km, Temp=tmax, Rain=prcp) %>%
      head(1000) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10, dom = 'Bfrtip'),
                selection = "none")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { paste("citibike_analysis_", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
}

shinyApp(ui, server)