plot_cumulative_cases <- function(data, Country = NULL) {
  
  # Filter data by country if a specific country is provided
  if (!is.null(Country)) {
    data <- data %>% filter(country == Country)  
  }
  
  # Summarize cumulative cases by date
  cumulative_cases <- data %>%
    group_by(date) %>%
    summarize(total_cumulative_cases = sum(cumulative_total_cases, na.rm = TRUE)) %>%
    arrange(date)
  
  # Calculate the last day, second last day cumulative cases, and percentage difference
  last_day_cumulative_cases <- cumulative_cases$total_cumulative_cases[nrow(cumulative_cases)]
  second_last_day_cumulative_cases <- cumulative_cases$total_cumulative_cases[nrow(cumulative_cases) - 1]

    if (second_last_day_cumulative_cases != 0) {
    percentage_difference <- ((last_day_cumulative_cases - second_last_day_cumulative_cases) / second_last_day_cumulative_cases) * 100
  } else {
    percentage_difference <- 0.0
  }
  percentage_difference <- round(percentage_difference, 1)
  
  # Create ggplot
  p <- ggplot(cumulative_cases, aes(x = as.Date(date), y = total_cumulative_cases,
                                    text = paste("<b>", format(as.Date(date), "%b %d, %Y"), "</b><br>",
                                                 "Cumulative Cases: <b>", total_cumulative_cases, "</b>"))) +
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
    # For overall data
    p <- p +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = "M"),
        breaks = seq(0, max(cumulative_cases$total_cumulative_cases, na.rm = TRUE), by = 2e8)
      )
  } else {
    # For country-specific data
    max_cases <- max(cumulative_cases$total_cumulative_cases, na.rm = TRUE)
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
    } else if (max_cases > 1e2) {
      rounded_max <- ceiling(max_cases / 1e2) * 1e2
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
      last_day_cumulative_cases = last_day_cumulative_cases,
      second_last_day_cumulative_cases = second_last_day_cumulative_cases,
      percentage_difference_cases = percentage_difference
    ))
}
