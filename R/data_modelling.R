#' Preprocess Data for Modelling
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Preprocess and format time series data for modeling.
#'
#' @param data A numeric vector or time series data representing the observations.
#' @param start A two-element numeric vector specifying the start date in the format c(year, period).
#' @param end A two-element numeric vector specifying the end date in the format c(year, period).
#' @param frequency The number of observations per year (e.g., 12 for monthly data).
#'
#' @return  data frame with formatted data, including columns for 'deaths', 'year', and 'period'.
#' @export
#'
#' @examples
#' # Load Monthly Deaths from Lung Disease in the UK from MASS
#' deaths <- MASS::deaths
#'
#' # Define start and end dates
#' start_date <- c(1974, 1)
#' end_date <- c(1979, 12)
#'
#' # Preprocess the data
#' formatted_data <- data_modelling(data = deaths, start = start_date, end = end_date, frequency = 12)
data_modelling <- function(data, start, end, frequency){

  # Count the number of observations
  n_data <- base::length(data)

  # Calculate the start in a numeric format
  start_numeric <- start[1L] + (start[2L] - 1)/frequency

  # Calculate the end in a numeric format
  end_numeric <- end[1L] + (end[2L] - 1)/frequency

  # Calculate the time in a numeric format
  time_numeric <- base::seq(from = start_numeric, to = end_numeric, length.out = n_data)

  # Extract the year
  year_integer <- base::as.integer(trunc(time_numeric))
  # .. and the period within a year
  period_integer <- base::round((time_numeric - year_integer) * frequency) + 1L

  # Gather the data in 'res'
  res <- base::data.frame(deaths = data, year = year_integer, period = period_integer)
  # ... and return it

  return(res)
}


