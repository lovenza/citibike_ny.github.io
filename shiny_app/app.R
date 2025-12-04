library(shiny)
library(bs4Dash)
library(tidyverse)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(DT)
library(fresh)
library(shinycssloaders)

my_theme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#2c3e50",
    sidebar_light_bg = "#ffffff",
    main_bg = "#f4f6f9"
  ),
  bs4dash_status(
    primary = "#2C3E50",
    danger = "#E74C3C",
    info = "#3498DB",
    success = "#18BC9C",
    warning = "#F39C12"
  )
)

possible_paths <- c(
  "data/final_bike_weather_categorized.csv",
  "final_bike_weather_categorized.csv"
)
data_path <- possible_paths[file.exists(possible_paths)][1]

if (!is.na(data_path)) {
  bike_data <- read_csv(data_path, show_col_types = FALSE) %>%
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
    filter(!is.na(start_lat), !is.na(start_lng))
} else {
  stop("Error: 'final_bike_weather_categorized.csv' not found.")
}

color_palette <- c("member" = "#2C3E50", "casual" = "#E74C3C")

ui <- bs4DashPage(
  title = "NYC Citi Bike 2025",
  freshTheme = my_theme,
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Citi Bike NYC",
      color = "primary",
      href = "#",
      image = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d4/Citi_Bike_logo.svg/1200px-Citi_Bike_logo.svg.png"
    )
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    sidebarMenu(
      menuItem("Executive Summary", tabName = "overview", icon = icon("chart-line")),
      menuItem("Spatial Analysis", tabName = "spatial", icon = icon("map-marked-alt")),
      menuItem("Weather Impact", tabName = "weather", icon = icon("cloud-sun-rain")),
      menuItem("User Behavior", tabName = "users", icon = icon("users")),
      menuItem("Model Insights", tabName = "models", icon = icon("brain")), 
      menuItem("Raw Data", tabName = "data", icon = icon("database"))
    ),
    
    div(style = "padding: 15px;",
        h5("Global Filters", style = "color: #7f8c8d; font-size: 0.9rem; text-transform: uppercase; letter-spacing: 1px;"),
        hr(),
        selectInput("user_type", "User Type:", 
                    choices = c("All Types" = "all", "Annual Member" = "member", "Casual Rider" = "casual"), 
                    selected = "all"),
        
        sliderInput("date_range", "Date Range:",
                    min = min(bike_data$start_date),
                    max = max(bike_data$start_date),
                    value = c(min(bike_data$start_date), max(bike_data$start_date)),
                    timeFormat = "%b %d"),
        
        sliderInput("hour_range", "Time of Day:", min = 0, max = 23, value = c(0, 23), step = 1),
        
        checkboxInput("weekend_only", "Weekends Only", value = FALSE),
        
        actionButton("reset", "Reset Filters", icon = icon("undo"), width = "100%", class = "btn-outline-secondary")
    )
  ),
  
  body = dashboardBody(
    tags$head(tags$style(HTML(".small-box .inner h3 { font-size: 2rem; }"))),
    
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
                  plotlyOutput("trend_plot", height = "350px") %>% withSpinner(type = 6)
                )
              ),
              fluidRow(
                box(
                  title = "Hourly Demand Heatmap", width = 6, status = "info",
                  plotlyOutput("hourly_heatmap", height = "300px")
                ),
                box(
                  title = "User Split", width = 6, status = "info",
                  plotlyOutput("user_pie", height = "300px")
                )
              )
      ),
      
      tabItem(tabName = "spatial",
              fluidRow(
                box(
                  title = "Ridership Routes & Top Stations", width = 12, height = "600px",
                  status = "primary", maximize = TRUE,
                  leafletOutput("main_map", height = "550px") %>% withSpinner(type = 6)
                )
              ),
              fluidRow(
                box(title = "Top 10 Start Stations", width = 6, plotlyOutput("top_start_plot", height = "300px")),
                box(title = "Top 10 End Stations", width = 6, plotlyOutput("top_end_plot", height = "300px"))
              )
      ),
      
      tabItem(tabName = "weather",
              fluidRow(
                column(12,
                       callout(
                         title = "Key Insight", status = "info", width = 12,
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
                  title = "Ride Duration Distribution (Member vs Casual)", width = 12,
                  plotlyOutput("duration_density", height = "400px") %>% withSpinner()
                )
              ),
              fluidRow(
                box(title = "Weekend Preference", width = 6, plotlyOutput("weekend_prop_plot")),
                box(title = "Bike Type Preference", width = 6, plotlyOutput("bike_type_plot"))
              )
      ),
      
      tabItem(tabName = "models",
              h3("Statistical Analysis Results"),
              p("Visualizing the coefficients from the linear regression models performed in the analysis phase."),
              fluidRow(
                box(
                  title = "What drives Ridership Volume?", width = 6, status = "success",
                  plotlyOutput("coef_plot_volume", height = "400px"),
                  footer = "Positive values increase ridership, negative values decrease it."
                ),
                box(
                  title = "What drives Ride Duration?", width = 6, status = "warning",
                  plotlyOutput("coef_plot_duration", height = "400px"),
                  footer = "Distance is the dominant factor. Weather has minimal impact on duration."
                )
              )
      ),
      
      tabItem(tabName = "data",
              box(
                title = "Filtered Dataset", width = 12,
                DTOutput("raw_table"),
                downloadButton("download_csv", "Download CSV", class = "btn-success")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$date_range) 
    
    d <- bike_data %>%
      filter(start_date >= input$date_range[1],
             start_date <= input$date_range[2],
             hour >= input$hour_range[1],
             hour <= input$hour_range[2])
    
    if(input$user_type != "all") {
      d <- d %>% filter(member_casual == input$user_type)
    }
    
    if(input$weekend_only) {
      d <- d %>% filter(weekend_and_holiday == "weekend and holiday")
    }
    
    d
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "user_type", selected = "all")
    updateSliderInput(session, "date_range", value = c(min(bike_data$start_date), max(bike_data$start_date)))
    updateSliderInput(session, "hour_range", value = c(0, 23))
    updateCheckboxInput(session, "weekend_only", value = FALSE)
  })
  
  output$total_rides <- renderValueBox({
    n <- nrow(filtered_data())
    valueBox(
      value = format(n, big.mark = ","),
      subtitle = "Total Rides Selected",
      icon = icon("bicycle"),
      color = "primary"
    )
  })
  
  output$avg_duration <- renderValueBox({
    val <- mean(filtered_data()$ride_duration_min, na.rm = TRUE)
    valueBox(
      value = sprintf("%.1f min", val),
      subtitle = "Avg Duration",
      icon = icon("stopwatch"),
      color = "info"
    )
  })
  
  output$avg_distance <- renderValueBox({
    val <- mean(filtered_data()$distance_km, na.rm = TRUE)
    valueBox(
      value = sprintf("%.2f km", val),
      subtitle = "Avg Distance",
      icon = icon("ruler-horizontal"),
      color = "success"
    )
  })
  
  output$total_rain_impact <- renderValueBox({
    rainy_rides <- filtered_data() %>% filter(prcp > 0) %>% nrow()
    total <- nrow(filtered_data())
    pct <- ifelse(total > 0, (rainy_rides/total)*100, 0)
    
    valueBox(
      value = sprintf("%.1f%%", pct),
      subtitle = "Rides occurring in Rain",
      icon = icon("cloud-showers-heavy"),
      color = "danger"
    )
  })
  
  output$trend_plot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    daily <- filtered_data() %>% count(start_date, member_casual)
    
    p <- ggplot(daily, aes(x = start_date, y = n, color = member_casual)) +
      geom_line(size = 1) + geom_point(size = 1.5) +
      scale_color_manual(values = color_palette) +
      theme_minimal() + labs(x = "", y = "Daily Rides", color = "User Type")
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = 1.1))
  })
  
  output$user_pie <- renderPlotly({
    df <- filtered_data() %>% count(member_casual)
    plot_ly(df, labels = ~member_casual, values = ~n, type = 'pie',
            marker = list(colors = unname(color_palette[as.character(df$member_casual)])),
            textinfo = 'label+percent') %>%
      layout(showlegend = FALSE)
  })
  
  output$hourly_heatmap <- renderPlotly({
    df <- filtered_data() %>% count(hour, member_casual)
    p <- ggplot(df, aes(x = hour, y = n, fill = member_casual)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = color_palette) +
      theme_minimal() + labs(x = "Hour of Day", y = "Ride Volume")
    ggplotly(p)
  })
  
  output$main_map <- renderLeaflet({
    # 1. 站点数据
    stations <- filtered_data() %>%
      filter(!is.na(start_lat), !is.na(start_lng)) %>%
      group_by(start_station_name, start_lat, start_lng) %>%
      summarise(rides = n(), .groups = "drop") %>%
      arrange(desc(rides)) %>% 
      head(300)
    
    # 2. 热力图数据
    heat_data <- filtered_data() %>% 
      filter(!is.na(start_lat), !is.na(start_lng)) %>%
      select(start_lat, start_lng) %>% 
      sample_n(min(nrow(.), 5000))
    
    # 3. 路线数据 (随机抽取150条)
    route_data <- filtered_data() %>%
      filter(!is.na(start_lat), !is.na(start_lng), !is.na(end_lat), !is.na(end_lng)) %>%
      sample_n(min(nrow(.), 150))
    
    # 4. 构建地图
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addHeatmap(data = heat_data, lng = ~start_lng, lat = ~start_lat,
                 intensity = 0.6, blur = 25, max = 0.05, radius = 18)
    
    # 5. 循环添加路线 (使用细蓝线)
    for(i in seq_len(nrow(route_data))) {
      map <- map %>% 
        addPolylines(
          lat = c(route_data$start_lat[i], route_data$end_lat[i]),
          lng = c(route_data$start_lng[i], route_data$end_lng[i]),
          weight = 1, 
          color = "#3498DB", 
          opacity = 0.5
        )
    }
    
    # 6. 添加站点标记 (最上层)
    map %>%
      addCircleMarkers(
        data = stations, 
        lng = ~start_lng, lat = ~start_lat,
        radius = ~ifelse(rides < 50, 4, 4 + sqrt(rides)/4),
        color = "#E67E22",
        weight = 2,
        opacity = 1,
        fillColor = "#2C3E50",
        fillOpacity = 0.9,
        popup = ~paste("<b>", start_station_name, "</b><br>Rides:", format(rides, big.mark = ","))
      )
  })
  
  output$top_start_plot <- renderPlotly({
    df <- filtered_data() %>% count(start_station_name, sort = TRUE) %>% head(10)
    plot_ly(df, x = ~n, y = ~reorder(start_station_name, n), type = 'bar', orientation = 'h',
            marker = list(color = "#34495E")) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Rides"))
  })
  
  output$top_end_plot <- renderPlotly({
    df <- filtered_data() %>% count(end_station_name, sort = TRUE) %>% head(10)
    plot_ly(df, x = ~n, y = ~reorder(end_station_name, n), type = 'bar', orientation = 'h',
            marker = list(color = "#16A085")) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Rides"))
  })
  
  output$temp_scatter <- renderPlotly({
    daily <- filtered_data() %>% group_by(start_date, tmax, member_casual) %>% summarise(rides = n(), .groups = "drop")
    p <- ggplot(daily, aes(x = tmax, y = rides, color = member_casual)) +
      geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
      scale_color_manual(values = color_palette) +
      theme_minimal() + labs(x = "Max Temp (°F)", y = "Daily Rides")
    ggplotly(p)
  })
  
  output$rain_box <- renderPlotly({
    daily <- filtered_data() %>% 
      group_by(start_date, rain_category, member_casual) %>% 
      summarise(rides = n(), .groups = "drop") %>% 
      filter(rain_category != "Missing") %>%
      mutate(rain_category = factor(rain_category, levels = c("No Rain", "Light Rain", "Medium Rain", "Heavy Rain")))
    
    p <- ggplot(daily, aes(x = rain_category, y = rides, fill = member_casual)) +
      geom_boxplot() + scale_fill_manual(values = color_palette) +
      theme_minimal() + labs(x = "Rain Condition", y = "Daily Rides")
    ggplotly(p) %>% layout(boxmode = "group")
  })
  
  output$duration_density <- renderPlotly({
    df <- filtered_data() %>% filter(ride_duration_min < 60)
    p <- ggplot(df, aes(x = ride_duration_min, fill = member_casual)) +
      geom_density(alpha = 0.5) + scale_fill_manual(values = color_palette) +
      theme_minimal() + labs(x = "Duration (min)", y = "Density")
    ggplotly(p)
  })
  
  output$weekend_prop_plot <- renderPlotly({
    df <- filtered_data() %>% count(member_casual, weekend_and_holiday) %>% group_by(member_casual) %>% mutate(prop = n/sum(n))
    p <- ggplot(df, aes(x = member_casual, y = prop, fill = weekend_and_holiday)) +
      geom_col() + scale_fill_brewer(palette = "Pastel1") +
      scale_y_continuous(labels = scales::percent) + theme_minimal() + labs(x = "", y = "Proportion", fill = "Day Type")
    ggplotly(p)
  })
  
  output$bike_type_plot <- renderPlotly({
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
      layout(title = "Impact on Ridership Volume (%)", xaxis = list(title = "% Change"))
  })
  
  output$coef_plot_duration <- renderPlotly({
    coef_data <- data.frame(
      Term = c("Dist (+1 km)", "Temp (+10°F)", "Rain (+1 inch)", "Casual User"),
      Estimate = c(4.3, 0.2, -0.5, 6.5)
    )
    plot_ly(coef_data, x = ~Estimate, y = ~reorder(Term, Estimate), type = 'bar', orientation = 'h',
            marker = list(color = "#3498DB")) %>%
      layout(title = "Impact on Duration (Minutes)", xaxis = list(title = "Minutes Change"))
  })
  
  output$raw_table <- renderDT({
    filtered_data() %>%
      select(Date=start_date, Station=start_station_name, User=member_casual, 
             Dur=ride_duration_min, Dist=distance_km, Temp=tmax, Rain=prcp) %>%
      head(500) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { paste("citibike_data_", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(filtered_data(), file) }
  )
}

shinyApp(ui, server)