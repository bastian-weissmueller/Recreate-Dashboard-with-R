plot_daily_cases <- function(data, Country = NULL) {
  
  # Filter data by country if specified
  if (!is.null(Country)) {
    data <- data %>% filter(country == Country)  
  }
  
  # Summarize daily new cases by date
  daily_cases <- data %>%
    group_by(date) %>%
    summarize(total_daily_new_cases = sum(daily_new_cases, na.rm = TRUE)) %>%
    arrange(date)
  
  # Calculate statistics for the last two days
  last_day_cases <- daily_cases$total_daily_new_cases[nrow(daily_cases)]
  second_last_day_cases <- daily_cases$total_daily_new_cases[nrow(daily_cases) - 1]
  
  if (second_last_day_cases != 0) {
    percentage_difference_cases <- ((last_day_cases - second_last_day_cases) / second_last_day_cases) * 100
  } else {
    percentage_difference_cases <- 0.0
  }
  
  percentage_difference_cases <- round(percentage_difference_cases, 2)
  
  # Create ggplot
  p <- ggplot(daily_cases, aes(x = as.Date(date), y = total_daily_new_cases,
                               text = paste("<b>", format(as.Date(date), "%b %d, %Y"), "</b><br>",
                                            "New Positive Cases: <b>", total_daily_new_cases, "</b>"))) +
    geom_bar(stat = "identity", fill = "deepskyblue") +
    scale_x_date(
      date_labels = "%b %Y",            
      date_breaks = "6 months",         
      minor_breaks = NULL               
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),    
      plot.title = element_blank(),
    )
  
  # Set y-axis scale based on overall or country-specific data
  if (is.null(Country)) {
    p <- p +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = "M"), 
        breaks = seq(0, max(daily_cases$total_daily_new_cases, na.rm = TRUE), by = 1e6)
      )
  } else {
    max_cases <- max(daily_cases$total_daily_new_cases, na.rm = TRUE)
    
    if (max_cases > 1e6) {
      rounded_max <- ceiling(max_cases / 1e6) * 1e6
      scale = 1e-6
      suffix = "M"
    } else if (max_cases > 1e5) {
      rounded_max <- ceiling(max_cases / 1e5) * 1e5
      scale = 1e-3
      suffix = "K"
    } else if (max_cases > 1e4) {
      rounded_max <- ceiling(max_cases / 1e4) * 1e4
      scale = 1e-3
      suffix = "K"
    } else if (max_cases > 1e3) {
      rounded_max <- ceiling(max_cases / 1e3) * 1e3
      scale = 1e-3
      suffix = "K"
    } else {
      rounded_max <- ceiling(max_cases / 1e2) * 1e2
      scale = 1
      suffix = ""
    }
    
    step_size <- rounded_max / 4
    y_breaks <- seq(0, rounded_max, by = step_size)
    
    p <- p +
      scale_y_continuous(
        labels = scales::label_number(scale = scale, suffix = suffix),
        breaks = y_breaks
      )
  } 
  
  # Convert ggplot to Plotly
  plotly_p <- ggplotly(p, tooltip = "text") %>%
    layout(
      hoverlabel = list(
        bgcolor = "white", 
        font = list(size = 12)
      )
    )
  
  return(list(
      plot = plotly_p,
      last_day_cases = last_day_cases,
      second_last_day_cases = second_last_day_cases,
      percentage_difference_cases = percentage_difference_cases
    ))
}
