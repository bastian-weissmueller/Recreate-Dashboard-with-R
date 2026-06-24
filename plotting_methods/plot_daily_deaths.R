plot_daily_deaths <- function(data, Country = NULL) {
  
  # Filter data by country if specified
  if (!is.null(Country)) {
    data <- data %>% filter(country == Country)  
  }
  
  # Summarize daily new deaths by date
  daily_deaths <- data %>%
    group_by(date) %>%
    summarize(total_daily_new_deaths = sum(daily_new_deaths, na.rm = TRUE)) %>%
    arrange(date)
  
  # Calculate statistics for the last two days
  last_day_deaths <- daily_deaths$total_daily_new_deaths[nrow(daily_deaths)]
  second_last_day_deaths <- daily_deaths$total_daily_new_deaths[nrow(daily_deaths) - 1]
  
  if (second_last_day_deaths != 0) {
    percentage_difference_deaths <- ((last_day_deaths - second_last_day_deaths) / second_last_day_deaths) * 100
  } else {
    percentage_difference_deaths <- 0.0
  }
  
  percentage_difference_deaths <- round(percentage_difference_deaths, 2)
  
  # Create ggplot
  p <- ggplot(daily_deaths, aes(x = as.Date(date), y = total_daily_new_deaths,
                                text = paste("<b>", format(as.Date(date), "%b %d, %Y"), "</b><br>",
                                             "New Deaths: <b>", total_daily_new_deaths, "</b>"))) +
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
      plot.title = element_blank()
    )
  
  # Set y-axis scale based on overall or country-specific data
  if (is.null(Country)) {
    p <- p +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-3, suffix = "K"),
        breaks = seq(0, max(daily_deaths$total_daily_new_deaths, na.rm = TRUE), by = 1e4)
      )
  } else {
    max_cases <- max(daily_deaths$total_daily_new_deaths, na.rm = TRUE)
    
    if (max_cases > 1e4) {
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
      last_day_deaths = last_day_deaths,
      second_last_day_deaths = second_last_day_deaths,
      percentage_difference_deaths = percentage_difference_deaths
    ))
}
