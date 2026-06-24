update_data <- function(data, Country = NULL) {
  
  if (!is.null(Country)) {
    data <- data %>% filter(country == Country)  
  }
  
  # Data aggregation
  data <- data %>%
    group_by(date) %>%
    summarize(
      cumulative_total_cases = sum(cumulative_total_cases, na.rm = TRUE),
      daily_new_cases = sum(daily_new_cases, na.rm = TRUE),
      cumulative_deaths = sum(cumulative_total_deaths, na.rm = TRUE),
      daily_new_deaths = sum(daily_new_deaths, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Latest data cases
  last_day_cumulative_cases <- tail(data$cumulative_total_cases, 1)
  second_last_day_cumulative_cases <- tail(data$cumulative_total_cases, 2)[1]
  
  if (second_last_day_cumulative_cases != 0) {
    percentage_difference_cases <- ((last_day_cumulative_cases - second_last_day_cumulative_cases) / second_last_day_cumulative_cases) * 100
  } else {
    percentage_difference_cases <- 0.0
  }
  percentage_difference_cases <- round(percentage_difference_cases, 1)
  
  # Latest data deaths
  last_day_cumulative_deaths <- tail(data$cumulative_deaths, 1)
  second_last_day_cumulative_deaths <- tail(data$cumulative_deaths, 2)[1]
  
  if (second_last_day_cumulative_deaths != 0) {
    percentage_difference_deaths <- ((last_day_cumulative_deaths - second_last_day_cumulative_deaths) / second_last_day_cumulative_deaths) * 100
  } else {
    percentage_difference_deaths <- 0.0
  }
  percentage_difference_deaths <- round(percentage_difference_deaths, 1)
  
  # Latest data daily cases
  last_day_daily_cases <- tail(data$daily_new_cases, 1)
  second_last_day_daily_cases <- tail(data$daily_new_cases, 2)[1]
  
  if (second_last_day_daily_cases != 0) {
    percentage_difference_daily_cases <- ((last_day_daily_cases - second_last_day_daily_cases) / second_last_day_daily_cases) * 100
  } else {
    percentage_difference_daily_cases <- 0.0
  }
  percentage_difference_daily_cases <- round(percentage_difference_daily_cases, 1)
  
  # Latest data daily deaths
  last_day_daily_deaths <- tail(data$daily_new_deaths, 1)
  second_last_day_daily_deaths <- tail(data$daily_new_deaths, 2)[1]
  
  if (second_last_day_daily_deaths != 0) {
    percentage_difference_daily_deaths <- ((last_day_daily_deaths - second_last_day_daily_deaths) / second_last_day_daily_deaths) * 100
  } else {
    percentage_difference_daily_deaths <- 0.0
  }
  percentage_difference_daily_deaths <- round(percentage_difference_daily_deaths, 1)
  
  return(list(
    last_day_cumulative_cases = last_day_cumulative_cases,
    second_last_day_cumulative_cases = second_last_day_cumulative_cases,
    percentage_difference_cases = percentage_difference_cases,
    
    last_day_cumulative_deaths = last_day_cumulative_deaths,
    second_last_day_cumulative_deaths = second_last_day_cumulative_deaths,
    percentage_difference_deaths = percentage_difference_deaths,
    
    last_day_daily_cases = last_day_daily_cases,
    second_last_day_daily_cases = second_last_day_daily_cases,
    percentage_difference_daily_cases = percentage_difference_daily_cases,
    
    last_day_daily_deaths = last_day_daily_deaths,
    second_last_day_daily_deaths = second_last_day_daily_deaths,
    percentage_difference_daily_deaths = percentage_difference_daily_deaths
  ))
}
