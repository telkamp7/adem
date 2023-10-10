#' Fitting Generalized Linear Models
#'
#' Fit a statistical model to data using the specified distribution.
#'
#' @param fit_formula A formula specifying the model to be fit, e.g., y ~ x1 + x2.
#' @param data A data frame containing the variables in the formula.
#' @param start A starting point for the optimization algorithm (initial parameter values).
#' @param fit_distribution A character string specifying the distribution to be used for fitting the model.
#'   Choose between `poisson` or `negative.binomial`.
#'
#' @return An object of class `glm` or `glm.nb` depending on the chosen distribution, representing the fitted model.
#' @export
#'
#' @examples
#' # Load Monthly Deaths from Lung Disease in the UK from MASS
#' deaths <- MASS::deaths
#'
#' # Reformat the data
#' my_data <- data.frame(
#' deaths = c(deaths),
#' date = time(deaths),
#' t = 1:length(deaths)
#' )
#' my_data$year <- trunc(my_data$date)
#' my_data$month <- (my_data$date - my_data$year) * 12 + 1
#'
#' # Define a model formula
#' model_formula <- deaths ~ 1 + t + sin(pi/6*month) + cos(pi/6*month)
#'
#' # Define the starting values
#' start <- c(
#' log(mean(my_data$deaths)),
#' 0, 0, 0
#' )
#'
#' # Fit a Negative Binomial model
#' negbin_model <- fit_model(
#' fit_formula = model_formula,
#' data = my_data,
#' start = start,
#' fit_distribution = "negative.binomial"
#' )
#'
#' # Print the fitted model summary
#' summary(negbin_model)
fit_model <- function(fit_formula, data, start, fit_distribution){
  if(fit_distribution == "poisson"){
    fit <- stats::glm(formula = fit_formula, family = stats::poisson(link = "log"), data = data, start = start)
  }else if(fit_distribution == "negative.binomial"){
    fit <- MASS::glm.nb(formula = fit_formula, data = data, start = start, link = "log")
  }
  return(fit)
}
