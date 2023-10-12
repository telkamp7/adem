#' Title
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Perform Automated Detection of Excess Mortality (ADEM) on time series death data.
#'
#' @param data A data frame containing the registered time series death data.
#' @param data_delay A data frame containing delay information.
#' @param fit_formula A formula specifying the model to be fit.
#' @param k An integer specifying the window size for modeling.
#' @param sig_value A numeric value controlling significance levels.
#' @param threshold_method A character string specifying threshold calculation method.
#'  It should be either "prediction" or "quantile".
#' @param theta_start A numeric vector for initial parameter values in the model.
#' @param delay_distribution A character string specifying the distribution for delay modeling.
#'  It should be either "geometric" or "negative.binomial".
#' @param delay_par A numeric scalar or vector for initial parameter values in delay modeling.
#' @param fit_distribution A character string specifying the distribution for model fitting.
#'  It should be either "poisson" or "negative.binomial".
#' @param data_start A two-element numeric vector specifying the start date in the format c(year, period).
#' @param data_end A two-element numeric vector specifying the end date in the format c(year, period).
#' @param data_frequency An integer specifying the number of observations per year.
#' @param units A character string specifying the time units for delay calculation.
#'   Supported units include "secs", "mins", "hours", "days", "weeks", "months", and "years".
#'
#' @return A tibble containing model results, including predictions, thresholds, and parameter estimates.
#' @export
#'
#'
#' @examples
#' # Load Monthly Deaths from Lung Disease in the UK from MASS
#' deaths <- MASS::deaths
#'
#' # Define the model formula
#' fit_formula = deaths ~ 1 + t + sin(pi/6*period) + cos(pi/6*period)
#'
#' # Set other parameters
#' k <- 12*2
#' sig_value = 0.05
#' threshold_method = "quantile"
#' theta_start <- c(log(mean(deaths)), 0, 0, 0)
#' fit_distribution <- "poisson"
#' data_start <- c(1974, 1)
#' data_end <- c(1979, 12)
#' data_frequency <- 12
#'
#' # Fit the automated detection of excess mortality function
#' adem_results <- adem(data = c(deaths),
#' fit_formula = fit_formula,
#' k = k,
#' sig_value = sig_value,
#' threshold_method = threshold_method,
#' theta_start = theta_start,
#' fit_distribution = fit_distribution,
#' data_start = data_start,
#' data_end = data_end,
#' data_frequency = data_frequency)
#'
#' # Print the results
#' print(adem_results)

adem <- function(
    data, data_delay = NULL, fit_formula, k, sig_value = 0.05,
    threshold_method = c("prediction", "quantile"),
    theta_start,
    delay_distribution = c("geometric", "negative.binomial"),
    delay_par,
    fit_distribution = c("negative.binomial", "poisson"),
    data_start, data_end, data_frequency,
    units = c("auto", "secs", "mins", "hours", "days", "weeks")
    )
  {

  # Throw an error if any of the inputs are not supported
  threshold_method <- rlang::arg_match(threshold_method)
  delay_distribution <- rlang::arg_match(delay_distribution)
  fit_distribution <- rlang::arg_match(fit_distribution)
  units <- rlang::arg_match(units)

  # Prepare the modelling data
  data_modelling_df <- data_modelling(
    data = data,
    start = data_start,
    end = data_end,
    frequency = data_frequency
    )

  if(is.null(data_delay) == FALSE){
    # Prepare registration delay data
    data_delay_list <- data_registration_delay(
      data = data_delay,
      units = units
    )
    # Extract data delay
    data_delay_df <- data_delay_list$delay_results
    # Initialize parameter for optimal delay
    opt_delay_iter <- NULL
  }

  # Count the number of observational weeks
  n_obs_all <- base::nrow(data_modelling_df)

  # Allocate space for the results
  results <- tidyr::tibble()

  # Allocate room for past outbreaks
  past_outliers <- tidyr::tibble()

  # Loop over time
  for(i in 1:(n_obs_all-k)){

    # Extract the period in this window
    window_period <- data_modelling_df[i:(i+k-1),]
    #... and the reference date
    window_reference <- data_modelling_df[i+k,]

    # Add a trend
    window_period$t <- -k:-1
    # ... and another one
    window_reference$t <- 0

    # # Calculate cut-off date
    delay_cutoff_iter <- ISOweek::ISOweek2date(
      weekdate = paste0(
        window_period$year[k],
        "-W",
        base::sprintf(fmt = "%02d", window_period$period[k]),
        "-7"
        )
      )

    # Exclude past observations, if they are related to excessive deaths
    if(base::nrow(past_outliers) > 0){
      window_period <- window_period %>%
        dplyr::setdiff(past_outliers)
    }

    # Add proportion of deaths registered
    window_period$prop_reg <- 1
    # ... and another one
    window_reference$prop_reg <- 1

    if(is.null(data_delay) == FALSE){
      # Extract the delay times in this iteration
      window_delay <- data_delay_df[data_delay_df$DoR <= delay_cutoff_iter,]
      # Optimize the registration delay
      opt_delay_iter <- delay_calculation(
        par = delay_par,
        x = window_delay$delay,
        delay_distribution = delay_distribution
      )
      # Adjust the proportion of deaths registered
      window_period$prop_reg <- delay_append(
        t = abs(window_period$t),
        opt_par = opt_delay_iter$par,
        delay_distribution = delay_distribution
      )
      delay_par <- opt_delay_iter$par * 0.95
    }

    # Fit the model
    fit <- fit_model(fit_formula = fit_formula, data = window_period, start = theta_start, fit_distribution = fit_distribution)

    # Extract model coefficients
    coef_fit <- stats::coef(object = fit)

    # Extract confidence intervals for the model parameters
    base::suppressMessages(CI_theta <- stats::confint(object = fit))

    # Gather optimized parameter estimates
    opt_theta <- tidyr::tibble(Parameter = base::names(coef_fit), Estimate = coef_fit, Lower = CI_theta[,1], Upper = CI_theta[,2])

    # Calculate prediction and the lower and upper thresholds given the model
    prediction_and_thresholds <- base::suppressWarnings(
      threshold_calculation(
        df = window_reference,
        fit = fit,
        sig_value = sig_value,
        threshold_method = threshold_method
        )
      )

    # Collect the results
    results <- dplyr::bind_rows(
      results,
      window_reference %>%
        dplyr::mutate(
          y_hat = prediction_and_thresholds$prediction,
          lower_threshold = prediction_and_thresholds$lwr_threshold,
          upper_threshold = prediction_and_thresholds$upr_threshold,
          underflow_deaths = .data$deaths <= .data$lower_threshold,
          excess_deaths = .data$deaths >= .data$upper_threshold,
          outlier = .data$underflow_deaths | .data$excess_deaths,
          par = list(opt_theta),
          # opt_delay_par = dplyr::if_else(exists("opt_delay_iter"), opt_delay_iter$par, NULL), # Figure out a good way to include this
          aic = fit$aic,
          df_residual = fit$df.residual,
          df_null = fit$df.null,
          deviance = fit$deviance,
          null_deviance = fit$null.deviance,
          residuals = list(fit$residuals),
          converged = fit$converged,
          window_data = list(fit$data)
          )
      )

    # Extract outlier information for this iteration
    outlier <- results$outlier[i]

    # If the reference observation is an outlier,
    # we omit it from future parameter estimation
    if(outlier == TRUE){
      past_outliers <- dplyr::bind_rows(
        past_outliers,
        results[i,] %>%
          dplyr::select("deaths", "year", "period", "t")
      )
    }

    # Update the trend column
    if(base::nrow(past_outliers) > 0){
      past_outliers <- dplyr::mutate(.data = past_outliers, t = t - 1)
    }

    # Construct parameter guess for next iteration
    theta_start <- coef_fit * 0.7

  }
  return(results)
}
