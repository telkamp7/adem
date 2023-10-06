#' Geometric Negative Log Likelihood Function
#'
#' The negative log likelihood of a geometric density given the data and the theta parameter.
#'
#' @details The geometric distribution with `prob`\eqn{= p} has density \deqn{p(x)=p(1-p)^x} for \eqn{x=0,1,2,\cdots,0<p\leq 1}.
#'
#' If an element of `x` is not integer, the results of `nll_geometric` is zero, with a warning.
#'
#'
#' @param theta A numeric scalar
#' @param data A scalar or vector of integer values
#'
#' @return A numeric scalar for the negative log likelihood of the geometric density
#' given the data and theta parameter.
#' @export
#'
#' @examples nll_geometric(theta = 0.5, data = rgeom(n = 10, prob = 0.5))
nll_geometric <- function(theta, data){
  -sum(stats::dgeom(x = data, prob = theta, log = TRUE))
}
