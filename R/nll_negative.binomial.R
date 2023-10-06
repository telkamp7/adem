#' Negative Binomial Negative Log Likelihood Function
#'
#' The negative log likelihood of a negative binomial density given the data and the theta parameter.
#'
#' @details The negative binomial distribution with `size`\eqn{= n} and `prob`\eqn{= p} has density \deqn{p(x)=\frac{\Gamma(x+n)}{\Gamma(n)x!}p^n(1-p)^x} for \eqn{x=0,1,2,\cdots,n>0} and \eqn{0<p\geq 1}.
#'
#' If an element of `x` is not integer, the results of `nll_negative.binomial` is zero, with a warning.
#'
#' @param theta A numeric vector
#' @param data A scalar or vector of integer values
#'
#' @return A numeric scalar for the negative log likelihood of the geometric density
#' given the data and theta parameter.
#' @export
#'
#' @examples nll_negative.binomial(theta = c(1,0.5), data = rnbinom(10, 1, 0.5))
nll_negative.binomial <- function(theta, data){
  -sum(dnbinom(x = data, size = theta[1], prob = theta[2], log = TRUE))
}
