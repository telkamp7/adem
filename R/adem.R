
adem <- function(data, data_delay, fit_formula, k, sig_value, threshold_method, theta_start, delay_distribution, delay_par, fit_distribution, data_start, data_end, data_frequency){

  # Prepare the modelling data
  data_modelling_df <- data_modelling(data = data, start = data_start, end = data_end, frequency = data_frequency)

  # Prepare registration delay data
  data_delay_list <- data_registration_delay(data = data_delay, units = "days")

  # Prepare registration delay data
  # data_delay <- data %>%
  #   select(WoR, delay_integer)

  # Count the number of observational weeks
  n_obs_all <- base::nrow(data_modelling_df)

  # Allocate space for the results
  results <- tidyr::tibble()

  # Allocate room for past outbreaks
  past_outliers <- tidyr::tibble()

  # Initialize parameter for optimal delay
  opt_delay_iter <- NULL

  # i <- 1
  for(i in 1:(n_obs_all-k)){

    # Extract the period in this window
    window_period <- data_modelling_df[i:(i+k-1),]
    #... and the reference date
    window_reference <- data_modelling_df[i+k,]

    # Add a trend
    window_period$t <- -k:-1
    # ... and another one
    window_reference$t <- 0

    # # ... and the registration delay data
    # data_delay_window <- data_delay %>%
    #   filter(WoR %in% weeksInWindow)

    # Exclude past observations, if they are related to excessive deaths
    if(base::nrow(past_outliers) > 0){
      window_period <- dplyr::`%>%`(
        window_period,
        dplyr::setdiff(past_outliers)
      )
    }

    # Add proportion of deaths registered
    window_period$prop_reg <- 1
    # ... and another one
    window_reference$prop_reg <- 1

    # Optimize the registration delay
    # opt_delay_iter <- delay_calculation(par = delay_par, x = data_delay_window$delay_integer, delay_distribution = delay_distribution)
    #
    # # Adjust the proportion of deaths registered
    # y_window <- y_window %>%
    #   mutate(prop_reg = delay_append(t = abs(t), opt_par = opt_delay_iter$par, delay_distribution = delay_distribution))

    # Fit the model
    fit <- fit_model(fit_formula = fit_formula, data = window_period, start = theta_start, fit_distribution = fit_distribution)

    # Extract model coefficients
    coef_fit <- stats::coef(object = fit)

    # Extract confidence intervals for the model parameters
    base::suppressMessages(CI_theta <- stats::confint(object = fit))

    # Gather optimized parameter estimates
    opt_theta <- tidyr::tibble(Parameter = base::names(coef_fit), Estimate = coef_fit, Lower = CI_theta[,1], Upper = CI_theta[,2])

    # Calculate prediction and the lower and upper thresholds given the model
    prediction_and_thresholds <- threshold_calculation(
      df = window_reference,
      fit = fit,
      sig_value = sig_value,
      threshold_method = threshold_method
    )

    # Collect the results
    results <- dplyr::bind_rows(
      results,
      dplyr::`%>%`(
        window_reference,
        dplyr::mutate(
          y_hat = prediction_and_thresholds$prediction,
          lower_threshold = prediction_and_thresholds$lwr_threshold,
          upper_threshold = prediction_and_thresholds$upr_threshold,
          underflow_deaths = deaths <= lower_threshold,
          excess_deaths = deaths >= upper_threshold,
          outlier = underflow_deaths | excess_deaths,
          par = list(opt_theta),
          # opt_delay_par = list(opt_delay_iter$par),
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
      )

    # Extract outlier information for this iteration
    outlier <- results$outlier[i]

    # If the reference observation is an outlier,
    # we omit it from future parameter estimation
    if(outlier == TRUE){
      past_outliers <- dplyr::bind_rows(
        past_outliers,
        dplyr::`%>%`(
          results[i,],
          dplyr::select(deaths, year, period, t)
        )
      )
    }

    # Update the trend column
    if(base::nrow(past_outliers) > 0){
      past_outliers <- mutate(.data = past_outliers, t = t - 1)
    }

    # Construct parameter guess for next iteration
    theta_start <- coef_fit * 0.7

  }
  return(results)
}
