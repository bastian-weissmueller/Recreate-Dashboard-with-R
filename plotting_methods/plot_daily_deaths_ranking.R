plot_daily_deaths_ranking <- function(data, Country=NULL) {
  
  data <- data %>%
    group_by(country, date) %>%
    summarize(
      cumulative_total_cases = sum(cumulative_total_cases, na.rm = TRUE),
      daily_new_cases = sum(daily_new_cases, na.rm = TRUE),
      cumulative_deaths = sum(cumulative_total_deaths, na.rm = TRUE),
      daily_new_deaths = sum(daily_new_deaths, na.rm = TRUE)
    ) %>%
    ungroup()
  
  top_countries <- data %>%
    filter(date == max(date, na.rm = TRUE)) %>%
    arrange(desc(daily_new_deaths))
  
  # Default border color is white
  border_colors <- rep('white', nrow(top_countries))
  opacities <- rep(1, nrow(top_countries))
  
  # If Country is provided, highlight it
  if (!is.null(Country) && Country %in% top_countries$country) {
    border_colors <- ifelse(top_countries$country == Country, 'black', 'white')
    opacities <- ifelse(top_countries$country == Country, 1, 0.8)
  }
  
  # Add custom data for hover text
  top_countries <- top_countries %>%
    mutate(customdata = purrr::pmap(
      list(cumulative_total_cases, daily_new_cases, cumulative_deaths, daily_new_deaths),
      ~ list(..1, ..2, ..3, ..4)
    ))
  
  # Format daily_new_deaths with thousands separators
  top_countries <- top_countries %>%
    mutate(daily_new_deaths_formatted = format(daily_new_deaths, big.mark = ","))
  
  # Create the Plotly bar chart
  p <- plot_ly(
    data = top_countries,
    x = ~daily_new_deaths,
    y = ~reorder(country, daily_new_deaths),
    type = 'bar',
    orientation = 'h',
    customdata = ~customdata,
    marker = list(
      color = 'deepskyblue',
      line = list(
        color = border_colors,
        width = 1
      ),
      opacity = opacities
    ),
    text = ~daily_new_deaths_formatted,  
    textposition = 'outside',  
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Cumulative Positive Cases: <b>%{customdata[0]}</b><br>",
      "New Positive Cases: <b>%{customdata[1]}</b><br>",
      "Cumulative Deaths: <b>%{customdata[2]}</b><br>",
      "New Deaths: <b>%{customdata[3]}</b>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      xaxis = list(
        title = "",
        showticklabels = FALSE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 12, color = "#5e5e5e"),
        showgrid = FALSE
      ),
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12)
      ),
      showlegend = FALSE
    )
  
  return(p)
}
