#' Append Registration Delays
#'
#' This function calculates the cumulative distribution function (CDF) of registration delays
#' based on a specified delay distribution model.
#'
#' @param t A numeric vector of time points at which to calculate the CDF.
#' @param opt_par numeric vector of optimized parameters for the chosen delay distribution.
#'   For "geometric" distribution, it should contain a single parameter (success probability).
#'   For "negative.binomial" distribution, it should contain two parameters (size and success probability).
#' @param delay_distribution A character string specifying the delay distribution to be used.
#'   It should be either "geometric" or "negative.binomial".
#'
#' @return A numeric vector containing the CDF values at the given time points.
#'
#' @export
#'
#' @examples # Example 1: Calculate CDF for the geometric model
#' cdf_geo <- delay_append(t = c(1, 2, 3), opt_par = 0.3, delay_distribution = "geometric")
#'
#' # Example 2: Calculate CDF for the negative binomial model
#' cdf_nb <- delay_append(
#'   t = c(1, 2, 3),
#'   opt_par = c(5, 0.2),
#'   delay_distribution = "negative.binomial"
#'   )
delay_append <- function(t, opt_par, delay_distribution = c("geometric", "negative.binomial")){

  # Throw error if 'delay_distribution' is not one of supported values
  delay_distribution <- rlang::arg_match(delay_distribution)

  if(delay_distribution == "geometric"){
    if(length(opt_par) > 1)
      stop(
        paste(
          "The 'geometric' distribution only takes scalar 'prob' parameters not c(",
          paste(opt_par, collapse = ","),
          ")",
          sep = ""
        )
      )
    ans <- stats::pgeom(q = t, prob = opt_par)
  }else if(delay_distribution == "negative.binomial"){
    if(length(opt_par) != 2)
      stop(
        paste(
          "The 'negative.binomial' distribution expects a vector of size two",
          " giving the 'size' and 'prob', not c(",
          paste(opt_par, collapse = ","),")",
          sep = ""
        )
      )
    ans <- stats::pnbinom(q = t, size = opt_par[1], prob = opt_par[2])
  }
  return(ans)
}
