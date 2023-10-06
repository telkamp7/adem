#' Title
#'
#' @param df A data.frame of new data.
#' @param fit An object of class `glm` or `negbin`. Predictions and thresholds are made with this object.
#' @param sig_value A numeric value between 0 and 1. Controls the significance level of the interval estimates.
#' @param threshold_method A character string that defines the method used to calculate the thresholds. Choose between `prediction` or `quantile`.
#'
#' @return A data.frame, `df`, with predicted values, upper and lower bounds attached.
#' @export
#'
#' @examples
#' # Fit a Poisson model
#' fit <- glm(y ~ 1, family = "poisson", data = data.frame(y = rpois(10, 5)))
#' # Calculate a prediction and upper and lower prediction interval
#' threshold_calculation(
#'   df = data.frame(y = rpois(1, 5)),
#'   fit = fit,
#'   sig_value = 0.05,
#'   threshold_method = "prediction"
#'   )
threshold_calculation <- function(df, fit, sig_value, threshold_method){
  if(threshold_method == "prediction"){
    # Calculate the respective prediction intervals
    threshold <- ciTools::add_pi(df = df, fit = fit, alpha = sig_value, names = c("lwr", "upr"))
    lwr_threshold <- threshold$lwr
    upr_threshold <- threshold$upr
    prediction <- threshold$pred
  }else if(threshold_method == "quantile"){
    # Calculate the respective quantile
    lwr_quantile_prediction <- ciTools::add_quantile(df = df, fit = fit, p = sig_value/2, name = "lwr")
    prediction <- lwr_quantile_prediction$pred
    lwr_threshold <- lwr_quantile_prediction$lwr
    upr_threshold <- ciTools::add_quantile(df = df, fit = fit, p = 1-sig_value/2, name = "upr")$upr
  }
  return(data.frame(prediction = prediction, lwr_threshold = lwr_threshold, upr_threshold = upr_threshold))
}
