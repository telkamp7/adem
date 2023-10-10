#' Registration Delay Optimization
#'
#' This function performs optimization for registration delay estimation using different delay distribution models.
#'
#' @param par A numeric vector of parameters to be optimized.
#' @param x A data vector or data frame containing the observed delay data.
#' @param delay_distribution A character string specifying the delay distribution to be used.
#'  It should be either "geometric" or "negative.binomial".
#'
#' @return An optimization result object as returned by `stats::optim`.
#' @export
#'
#' @examples
#' observed_delays <- rnbinom(100, 5, prob = 0.5)
#' # Example 1: Optimize the geometric model
#' result_geo <- delay_calculation(
#'   par = c(0.1, 0.5),
#'   x = observed_delays,
#'   delay_distribution = "geometric"
#'   )
#'
#' # Example 2: Optimize the negative binomial model
#' result_nb <- delay_calculation(
#'   par = c(1, 0.2),
#'   x = observed_delays,
#'   delay_distribution = "negative.binomial"
#'   )
#'
delay_calculation <- function(par, x, delay_distribution){
  if(delay_distribution == "geometric"){
    # Optimize the geometric model
    opt <- stats::optim(par = par, fn = nll_geometric, data = x)
  }else if(delay_distribution == "negative.binomial"){
    # Optimize the negative binomial model
    opt <- stats::optim(par = par, fn = nll_negative.binomial, data = x)
  }
  return(opt)
}
