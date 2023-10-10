#' Calculate Registration Delays
#'
#' Calculate and format registration delay from date of death (DoD) to date of registration (DoR).
#'
#' @param data A data frame containing the 'DoR' (date of registration) and 'DoD' (date of death) columns.
#' @param units A character string specifying the time units for delay calculation.
#'   Supported units include "secs", "mins", "hours", "days", "weeks", "months", and "years".
#'
#' @return A list containing:
#'   - 'delay_results': A data frame with 'DoR' and 'delay' columns, where 'delay' is the calculated registration delay in the specified units.
#'   - 'units': The time units used for delay calculation.
#' @export
#'
#' @examples
#' # Create a sample data frame
#' sample_data <- data.frame(
#'   DoD = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
#'   DoR = as.Date(c("2023-01-05", "2023-01-06", "2023-01-10"))
#' )
#'
#' # Calculate registration delay in days
#' delay_info <- data_registration_delay(data = sample_data, units = "days")
#'
data_registration_delay <- function(data, units){

  # Calcualte the difftime between the oberservations
  delay_registration_difftime <- base::difftime(time1 = data$DoR, time2 = data$DoD, units = units)

  # Convert to integers
  delay_integer = base::as.integer(delay_registration_difftime)

  # Collect data in a data.frame
  delay_results <- base::data.frame(DoR = data$DoR, delay = delay_integer)

  return(list(delay_results = delay_results, units = units))

}
