# ================================
# Final assignment
# Covid 19 Data Dashboard
# ================================


library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(ggplot2)

# Packages
# required_packages <- c("shiny", "leaflet", "dplyr", "plotly", "ggplot2")
# install.packages(setdiff(required_packages, rownames(installed.packages())))


# Load Covid Data
data <- read.csv("data/CovidData.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Standardize country names if necessary for alignment
data <- data %>%
  mutate(country = case_when(
    country == "USA" ~ "United States",
    country == "United States of America" ~ "United States",
    country == "Russian Federation" ~ "Russia",
    country == "Democratic Republic Of The Congo" ~ "The Congo",
    country == "Saint Vincent And The Grenadines" ~ "Saint Vincent",
    TRUE ~ country
  ))

# Load coordinates data
country_coords <- read.csv("data/country-coord.csv") %>%
  rename(
    country = `Country`,
    lat = `Latitude..average.`,
    lon = `Longitude..average.`
  ) %>%
  mutate(country = case_when(
    country == "United States of America" ~ "United States",
    country == "Russian Federation" ~ "Russia",
    TRUE ~ country
  )) %>%
  filter(country %in% data$country)

# Join filtered coordinates with the main COVID-19 data
data <- data %>% left_join(country_coords, by = "country")

# Load plot functions from the provided files
source("plotting_methods/plot_daily_deaths.R")
source("plotting_methods/plot_cumulative_cases.R")
source("plotting_methods/plot_cumulative_deaths.R")
source("plotting_methods/plot_daily_cases.R")
source("plotting_methods/plot_daily_cases_ranking.R")
source("plotting_methods/plot_cumulative_cases_ranking.R")
source("plotting_methods/plot_daily_deaths_ranking.R")
source("plotting_methods/plot_cumulative_deaths_ranking.R")
source("plotting_methods/update_values.R")

# Dynamic function to scale bubble size based on case/death count
scale_bubble_size <- function(value, case_type, metric_type) {
  if (case_type == "Cumulative" && metric_type == "Positive Cases") {
    return(sqrt(value) / 500)
  } 
  else if (case_type == "New" && metric_type == "Positive Cases") {
    return(sqrt(value) / 70)
  } 
  else if (case_type == "Cumulative" && metric_type == "Deaths") {
    return(sqrt(value) / 100)
  } 
  else if(case_type == "New" && metric_type == "Deaths"){
    return(sqrt(value) / 5)
  }
}

country = NULL 

daily_deaths_results <- plot_daily_deaths(data, country)
daily_cases_results <- plot_daily_cases(data, country)
cumulative_cases_results <- plot_cumulative_cases(data, country)
cumulative_deaths_results <- plot_cumulative_deaths(data, country)


latest_data <- data %>%
  group_by(country, date) %>%
  summarize(
    cumulative_cases = sum(cumulative_total_cases, na.rm = TRUE),
    daily_cases = sum(daily_new_cases, na.rm = TRUE),
    cumulative_deaths = sum(cumulative_total_deaths, na.rm = TRUE),
    daily_deaths = sum(daily_new_deaths, na.rm = TRUE)
  ) %>%
  ungroup()

latest_data <- latest_data %>%
  group_by(country) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  ungroup()

# UI
ui <- fluidPage(
  div(
    h1("Global COVID-19 Tracker"),
    h3("January 21, 2020 - April 29, 2022"),
    style = "
      background-color: #0A0A49;  
      color: white;  
      text-align: left;  
      padding: 10px;  
      margin: 0;  
      height: 80px;  
    "
  ),
  
  tags$style(HTML("
    h1 {
      font-size: 24px;  /* Setzt die Schriftgröße für den Titel */
      margin-top: 0px;  /* Setzt den oberen Abstand für h1 */
    }
    h3 {
      font-size: 16px;  /* Setzt die Schriftgröße für den Untertitel */
      margin-top: 5px;
    }
  ")),
  
  
  fluidRow(
    # Left Column with Two Graphs Stacked Vertically
    column(width = 4,
           div(
             style = "background-color: white; height: 100px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
             tags$div(style = "font-size: 14px; color: gray;text-align: center;", textOutput("metric_cases")),
             tags$div(style = "font-size: 24px; color: black;text-align: center;", textOutput("last_day_cases")),
             tags$div(style = "font-size: 14px; color: gray; text-align: center;", htmlOutput("difference_percent_cases")),
             tags$div(style = "font-size: 13px; color: darkgray; text-align: center;", textOutput("second_last_cases"))
           ),
           
           div(
             style = "border: none; margin-top: 10px;",
             plotlyOutput("left_graph_cases", height = "250px")
           ),
           div(
             style = "background-color: white; height: 100px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
             tags$div(style = "font-size: 14px; color: gray; text-align: center;", textOutput("metric_deaths")),
             tags$div(style = "font-size: 24px; color: black;  text-align: center;", textOutput("last_day_deaths")),
             tags$div(style = "font-size: 14px; color: gray; text-align: center;", htmlOutput("difference_percent_deaths")),  
             tags$div(style = "font-size: 13px; color: gray; text-align: center;", textOutput("second_last_deaths"))
           ),
           div(
             style = "border: none; margin-top: 10px;",
             plotlyOutput("left_graph_deaths", height = "250px")
           )
    ),
    
    
    # Middle Column with Map and Selection Bars
    column(width = 4,
           fluidRow(
             column(width = 6,
                    selectInput("case_type", "Cumulative or New:", 
                                choices = c("Cumulative", "New"))
             ),
             column(width = 6,
                    selectInput("metric_type", "Positive Cases or Deaths", 
                                choices = c("Positive Cases", "Deaths"))
             )
           ),
           div(
             style = "background-color: white; margin-bottom: 10px; text-align: center;",
             tags$span(
               style = "font-size: 18px; color: #5e5e5e;", 
               textOutput("map_text_output", inline = TRUE)
             ),
             tags$br(),  
             tags$span(
               style = "font-size: 14px; color: #5e5e5e;", 
               textOutput("map_subtitle", inline = TRUE)
             )
           ),
           leafletOutput("map", height = "650px")
    ),
    
    # Right Column with Ranking Plot
    column(width = 4,
           fluidRow(
             div(
               style = "background-color: white; margin-bottom: 10px; text-align: center; display: flex; flex-direction: column; justify-content: flex-end; align-items: center; height: 80px;",
               tags$span(
                 style = "font-size: 18px; color: #5e5e5e;", 
                 textOutput("ranking_text_output", inline = TRUE)
               ),
               tags$span(
                 style = "font-size: 14px; color: #5e5e5e;", 
                 textOutput("ranking_subtitle", inline = TRUE)
               )
             )
             ,
             
             div(style = "height: 720px; overflow-y: scroll;",
                 plotlyOutput("ranking_plot", height = "5000px"))
           )
           
    )
  )
) 


# Server Logic
server <- function(input, output, session) {
  
  selected_country <- reactiveVal(NULL)
  
  # Calculate daily percentage differences dynamically based on input
  difference_percent_cases <- reactive({
    case_type <- input$case_type

    if (case_type == "Cumulative") {
      if (!is.null(last_day_cumulative_cases()) && !is.null(second_last_day_cumulative_cases()) && second_last_day_cumulative_cases() > 0) {
        round(((last_day_cumulative_cases() - second_last_day_cumulative_cases()) / second_last_day_cumulative_cases()) * 100, 2)
      } else {
        0.0
      }
    } 
    else {    # For "New" cases
      if (!is.null(last_day_cases()) && !is.null(second_last_day_cases()) && second_last_day_cases() > 0) {
        round(((last_day_cases() - second_last_day_cases()) / second_last_day_cases()) * 100, 2)
      } else {
        0.0
      }
    }
    
  })
  
  

  difference_percent_deaths <- reactive({
    case_type <- input$case_type
    
    if (case_type == "Cumulative") {
      if (!is.null(last_day_cumulative_deaths()) && !is.null(second_last_day_cumulative_deaths()) && second_last_day_cumulative_deaths() > 0) {
        round(((last_day_cumulative_deaths() - second_last_day_cumulative_deaths()) / second_last_day_cumulative_deaths()) * 100, 2)
      } else {
        0.0
      }
    } else {    # For "New" deaths
      if (!is.null(second_last_day_deaths()) && second_last_day_deaths() > 0) {
        round(((last_day_deaths() - second_last_day_deaths()) / second_last_day_deaths()) * 100, 2)
      } else {
        0.0
      }
    }
    
  })
  
  # Render percentage differences for cases
  output$difference_percent_cases <- renderText({
    diff_percent <- difference_percent_cases()
    if (diff_percent >= 0) {
      HTML(paste0('<span style="color: red;">▲ </span> ', round(diff_percent, 2), '% vs previous day'))
    } else {
      HTML(paste0('<span style="color: green;">▼ </span> ', round(diff_percent, 2), '% vs previous day'))
    }
  })
  
  # Render percentage differences for deaths
  output$difference_percent_deaths <- renderText({
    diff_percent <- difference_percent_deaths()
    if (diff_percent >= 0) {
      HTML(paste0('<span style="color: red;">▲ </span> ', round(diff_percent, 2), '% vs previous day'))
    } else {
      HTML(paste0('<span style="color: green;">▼ </span> ', round(diff_percent, 2), '% vs previous day'))
      }
  })
  
  plot_title <- reactive({
    case_type <- input$case_type
    metric_type <- input$metric_type
    
    if (case_type == "Cumulative" && metric_type == "Positive Cases") {
      return("Cumulative Positive Cases")
    } else if (case_type == "New" && metric_type == "Positive Cases") {
      return("New Positive Cases")
    } else if (case_type == "Cumulative" && metric_type == "Deaths") {
      return("Cumulative Deaths")
    } else if (case_type == "New" && metric_type == "Deaths") {
      return("New Deaths")
    } else {
      return("Unknown Metric")
    }
  })
  plot_subtitle <- reactive({
    return("Select a Country to see more details")
  })
  
  output$map_text_output <- renderText({
    plot_title()
  })
  output$map_subtitle <- renderText({
    plot_subtitle()
  })
  output$ranking_text_output <- renderText({
    plot_title()
  })
  output$ranking_subtitle <- renderText({
    plot_subtitle()
  })
  
  ranking_data <- reactive({
    case_type <- input$case_type
    metric_type <- input$metric_type
    
    data %>%
      group_by(country) %>%
      filter(date == max(date, na.rm = TRUE)) %>%
      summarize(
        value = if (case_type == "Cumulative" && metric_type == "Positive Cases") {
          max(cumulative_total_cases, na.rm = TRUE)
        } else if (case_type == "New" && metric_type == "Positive Cases") {
          max(daily_new_cases, na.rm = TRUE)
        } else if (case_type == "Cumulative" && metric_type == "Deaths") {
          max(cumulative_total_deaths, na.rm = TRUE)
        } else {
          max(daily_new_deaths, na.rm = TRUE)
        },
        lat = first(lat),
        lon = first(lon),
        cumulative_cases = max(cumulative_total_cases),
        cumulative_deaths = max(cumulative_total_deaths),
        daily_cases = first(daily_new_cases),  
        daily_deaths = first(daily_new_deaths),  
        .groups = "drop"
      ) %>%
      arrange(value)
  })
  
  
  
  # Render ranking plot with clickable bars and scrolling
  output$ranking_plot <- renderPlotly({
    case_type <- input$case_type
    metric_type <- input$metric_type
    country <- selected_country()
    
    if (case_type == "Cumulative" && metric_type == "Positive Cases") {
      plot_cumulative_cases_ranking(data, Country=country)
    } 
    else if (case_type == "New" && metric_type == "Positive Cases") {
      plot_daily_cases_ranking(data, Country=country)
    } 
    else if (case_type == "Cumulative" && metric_type == "Deaths") {
      plot_cumulative_deaths_ranking(data, country)
    } 
    else if(case_type == "New" && metric_type == "Deaths"){
      plot_daily_deaths_ranking(data, Country=country)
    }
  })
  
  # Observe clicks on the ranking plot to update the map and selection
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    if (!is.null(click_data) && "customdata" %in% names(click_data)) {
      clicked_country <- click_data$y
      selected_country(clicked_country)
      update_map()
    }
  })
  
  
  # Observe clicks on the map bubbles to update the barplot selection
  observeEvent(input$map_marker_click, {
    clicked_country <- input$map_marker_click$id
    selected_country(clicked_country)
    update_map()
  })
  
  last_day_cases <- reactiveVal()
  second_last_day_cases <- reactiveVal()
  percentage_difference_daily_cases <- reactiveVal()

  last_day_cumulative_cases <- reactiveVal()
  second_last_day_cumulative_cases <- reactiveVal()
  percentage_difference_cumulative_cases <- reactiveVal()

  last_day_deaths <- reactiveVal()
  second_last_day_deaths <- reactiveVal()
  percentage_difference_daily_deaths <- reactiveVal()

  last_day_cumulative_deaths <- reactiveVal()
  second_last_day_cumulative_deaths <- reactiveVal()
  percentage_difference_cumulative_deaths <- reactiveVal()

  
  
  
  observe({
    result <- update_data(data, Country = selected_country())

    last_day_cases(result$last_day_daily_cases)
    second_last_day_cases(result$second_last_day_daily_cases)
    percentage_difference_daily_cases(result$percentage_difference_daily_cases)

    last_day_cumulative_cases(result$last_day_cumulative_cases)
    second_last_day_cumulative_cases(result$second_last_day_cumulative_cases)
    percentage_difference_cumulative_cases(result$percentage_difference_cumulative_cases)

    last_day_deaths(result$last_day_daily_deaths)
    second_last_day_deaths(result$second_last_day_daily_deaths)
    percentage_difference_daily_deaths(result$percentage_difference_daily_deaths)
    
    last_day_cumulative_deaths(result$last_day_cumulative_deaths)
    second_last_day_cumulative_deaths(result$second_last_day_cumulative_deaths)
    percentage_difference_cumulative_deaths(result$percentage_difference_cumulative_deaths)
    update_map()
    })

  
  # Render graphs for cases and deaths in the left column based on selection
  output$left_graph_cases <- renderPlotly({
    case_type <- input$case_type
    country <- selected_country()
    
    # Show either cumulative or new cases plot based on input
    if (case_type == "Cumulative") {
      cumulative_cases_results <- plot_cumulative_cases(data, country)
      cumulative_cases_results$plot
    } else {
      daily_cases_results <- plot_daily_cases(data, country)
      daily_cases_results$plot
    }
  })
  
  output$left_graph_deaths <- renderPlotly({
    case_type <- input$case_type
    country <- selected_country()
    
    # Show either cumulative or new deaths plot based on input
    if (case_type == "Cumulative") {
      cumulative_deaths_results <- plot_cumulative_deaths(data, country)
      cumulative_deaths_results$plot
    } else {
      daily_deaths_results <- plot_daily_deaths(data, country)
      daily_deaths_results$plot
    }
  })

  
  # Function to update the map to highlight selected country
  update_map <- function() {
    map_data <- ranking_data()
    case_type <- input$case_type
    metric_type <- input$metric_type
    selcountry <- selected_country()
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = map_data,
        lat = ~lat,
        lng = ~lon,
        layerId = ~country,
        radius = ~scale_bubble_size(value, case_type, metric_type),
        color = ifelse(map_data$country == selcountry, "black", "deepskyblue"),
        weight = 2,
        fillColor = "deepskyblue",
        fillOpacity = ifelse(map_data$country == selected_country(), 1, 0.5),
        stroke = TRUE,
        label = ~lapply(paste0(
          "<b>", country, "</b><br><br>",
          "Cumulative Positive Cases: <b>", cumulative_cases, "</b><br>",
          "New Positive Cases: <b>", daily_cases, "</b><br><br>",
          "Cumulative Deaths: <b>", cumulative_deaths, "</b><br>",
          "New Deaths: <b>", daily_deaths, "</b><br>"
        ), HTML),
        labelOptions = labelOptions(direction = "auto")
      )
  }
  
  # Render the initial Leaflet map
  output$map <- renderLeaflet({
    case_type <- input$case_type
    metric_type <- input$metric_type
    
    map_data <- ranking_data()
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addCircleMarkers(
        lat = ~lat, lng = ~lon,
        layerId = ~country,
        radius = ~scale_bubble_size(value, case_type, metric_type), 
        color = "deepskyblue",
        weight = 2,
        fillColor = "deepskyblue",
        fillOpacity = 0.8,
        stroke = TRUE,
        label = ~lapply(paste0(
            "<b>", country, "</b><br><br>",
            "Cumulative Positive Cases: <b>", cumulative_cases, "</b><br>",
            "New Positive Cases: <b>", daily_cases, "</b><br><br>",
            "Cumulative Deaths: <b>", cumulative_deaths, "</b><br>",
            "New Deaths: <b>", daily_deaths, "</b><br>"
          ), HTML),
        ,
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      setView(lng = 0, lat = 10, zoom = 1)
    
  })

  
  output$metric_cases <- renderText({
    case_type <- input$case_type
    if (case_type == "Cumulative") {
      metric_value = "Cumulative Positive Cases"
    } 
    else{
      metric_value = "New Positive Cases"
    }
    paste(metric_value)
  })
  
  
  output$last_day_cases <- renderText({
    case_type <- input$case_type
    if (case_type == "Cumulative"){
      last_day = last_day_cumulative_cases()
    }
    else{
      last_day = last_day_cases()
    }
    formatted_last_day <- format(last_day, big.mark = ",", scientific = FALSE)
    paste(formatted_last_day)

  })
  

  output$difference_cases <- renderText({
    case_type <- input$case_type
    if (case_type == "Cumulative") {
      difference = percentage_difference_cumulative_cases()
    }
    else{
      difference = percentage_difference_daily_cases()
    }
    paste(difference)
  })

  output$second_last_cases <- renderText({
    case_type <- input$case_type
    if (case_type == "Cumulative"){
      second_last_day = second_last_day_cumulative_cases()
    }
    else{
      second_last_day = second_last_day_cases()
    }
    formatted_second_last_day <- format(second_last_day, big.mark = ",", scientific = FALSE)
    paste(formatted_second_last_day)
  })


  output$metric_deaths <- renderText({
    case_type <- input$case_type
    if (case_type == "Cumulative") {
      metric_value = "Cumulative Deaths"
    }
    else{
      metric_value = "New Deaths"
    }
    paste(metric_value)
  })

  output$last_day_deaths <- renderText({
    case_type <- input$case_type
    if (case_type == "Cumulative"){
      last_day = last_day_cumulative_deaths()
    }
    else{
      last_day = last_day_deaths()
    }
    formatted_last_day <- format(last_day, big.mark = ",", scientific = FALSE)
    paste(formatted_last_day)
  })

  output$difference_deaths <- renderText({
    case_type <- input$case_type
    if (case_type == "Cumulative") {
      difference = percentage_difference_cumulative_deaths()
    }
    else{
      difference = percentage_difference_daily_deaths()
    }
    paste(difference)
  })

  output$second_last_deaths <- renderText({
    case_type <- input$case_type
    if (case_type == "Cumulative"){
      second_last_day = second_last_day_cumulative_deaths()
    }
    else{
      second_last_day = second_last_day_deaths()
    }
    formatted_second_last_day <- format(second_last_day, big.mark = ",", scientific = FALSE)
    paste(formatted_second_last_day)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
