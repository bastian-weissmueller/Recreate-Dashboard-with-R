plot_cumulative_deaths <- function(data, Country = NULL) {
  
  # Filter data by country if specified
  if (!is.null(Country)) {
    data <- data %>% filter(country == Country)  
  }
  
  # Summarize cumulative deaths by date
  cumulative_deaths <- data %>%
    group_by(date) %>%
    summarize(total_cumulative_deaths = sum(cumulative_total_deaths, na.rm = TRUE)) %>%
    arrange(date)
  
  # Calculate statistics for the last two days
  last_day_cumulative_deaths <- cumulative_deaths$total_cumulative_deaths[nrow(cumulative_deaths)]
  second_last_day_cumulative_deaths <- cumulative_deaths$total_cumulative_deaths[nrow(cumulative_deaths) - 1]
  
  if (second_last_day_cumulative_deaths != 0) {
    percentage_difference_deaths <- ((last_day_cumulative_deaths - second_last_day_cumulative_deaths) / second_last_day_cumulative_deaths) * 100
  } else {
    percentage_difference_deaths <- 0.0
  }
  percentage_difference_deaths <- round(percentage_difference_deaths, 1)
  
  # Create ggplot
  p <- ggplot(cumulative_deaths, aes(x = as.Date(date), y = total_cumulative_deaths,
                                     text = paste("<b>", format(as.Date(date), "%b %d, %Y"), "</b><br>",
                                                  "Cumulative Deaths: <b>", total_cumulative_deaths, "</b>"))) +
    geom_bar(stat = "identity", fill = "deepskyblue") +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "6 months",
      date_minor_breaks = "1 year",
      expand = c(0, 0)
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
        labels = scales::label_number(scale = 1e-6, suffix = "M"),
        breaks = seq(0, max(cumulative_deaths$total_cumulative_deaths, na.rm = TRUE), by = 2e6)
      )
  } else {
    max_cases <- max(cumulative_deaths$total_cumulative_deaths, na.rm = TRUE)
    
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
  plotly_p_deaths <- ggplotly(p, tooltip = "text") %>%
    layout(
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12)
      )
    )
  
  return(list(
      plot = plotly_p_deaths,
      last_day_cumulative_deaths = last_day_cumulative_deaths,
      second_last_day_cumulative_deaths = second_last_day_cumulative_deaths,
      percentage_difference_deaths = percentage_difference_deaths
    ))
  
}
