



adem <- function(data, fit_formula, k, sig_value, threshold_method, theta_start, delay_distribution, delay_par, fit_distribution){

  # Prepare modelling data
  data_modelling <- data %>%
    select(WoD) %>%
    group_by(WoD) %>%
    reframe(y = n()) %>%
    mutate(week_in_year = as.integer(str_sub(string = WoD, start = 7, end = 8)))

  # Prepare registration delay data
  data_delay <- data %>%
    select(WoR, delay_integer)

  # Count the number of observational weeks
  n_obs_all <- nrow(data_modelling)

  # Allocate space for the results
  results <- tibble()

  # Allocate room for past outbreaks
  past_outliers <- tibble()

  # Initialize parameter for optimal delay
  opt_delay_iter <- NULL

  # i <- 1
  for(i in 1:(n_obs_all-k)){

    # Compute the dates in window
    weeksInWindow <- data_modelling$WoD[i:(i+k-1)]
    #... and the reference date
    ref_date <- data_modelling$WoD[i+k]

    # Extract the observations in this window
    y_window <- data_modelling %>%
      filter(WoD %in% weeksInWindow)

    # ... and the reference data
    y_reference <- data_modelling %>%
      filter(WoD %in% ref_date)

    # ... and the registration delay data
    data_delay_window <- data_delay %>%
      filter(WoR %in% weeksInWindow)

    # Add a trend
    y_window <- y_window %>%
      mutate(t = row_number() - k - 1)
    # ... and another one
    y_reference <- y_reference %>%
      mutate(t = 0)

    # Exclude past observations, if they are related to excessive deaths
    if(nrow(past_outliers) > 0){
      y_window <- y_window %>%
        setdiff(past_outliers)
    }

    # Add proportion of deaths registered
    y_window <- y_window %>%
      mutate(prop_reg = 1)
    # ... and another one
    y_reference <- y_reference %>%
      mutate(prop_reg = 1)

    # Optimize the registration delay
    opt_delay_iter <- delay_calculation(par = delay_par, x = data_delay_window$delay_integer, delay_distribution = delay_distribution)

    # Adjust the proportion of deaths registered
    y_window <- y_window %>%
      mutate(prop_reg = delay_append(t = abs(t), opt_par = opt_delay_iter$par, delay_distribution = delay_distribution))

    # Fit the model
    fit <- fit_model(fit_formula = fit_formula, data = y_window, start = theta_start, fit_distribution = fit_distribution)

    # fit <- glm(formula = formula, family = poisson(link = "log"), data = y_window, start = theta_start)
    # Extract model coefficients
    coef_fit <- coef(object = fit)

    # Extract confidence intervals for the model parameters
    suppressMessages(CI_theta <- confint(object = fit))

    # Gather optimized parameter estimates
    opt_theta <- tibble(Parameter = names(coef_fit), Estimate = coef_fit, Lower = CI_theta[,1], Upper = CI_theta[,2])

    # Calculate prediction and the lower and upper thresholds given the model
    prediction_and_thresholds <- threshold_calculation(
      df = y_reference,
      fit = fit,
      sig_value = sig_value,
      threshold_method = threshold_method
    )

    # Collect results
    results <- bind_rows(
      results,
      y_reference %>%
        mutate(
          y_hat = prediction_and_thresholds$prediction,
          lower_threshold = prediction_and_thresholds$lwr_threshold,
          upper_threshold = prediction_and_thresholds$upr_threshold,
          underflow_deaths = y <= lower_threshold,
          excess_deaths = y >= upper_threshold,
          outlier = underflow_deaths | excess_deaths,
          par = list(opt_theta),
          opt_delay_par = list(opt_delay_iter$par),
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

    # Extract outliers
    outlier <- results %>%
      filter(WoD == ref_date) %>%
      .$outlier

    if(outlier == TRUE){
      past_outliers <- bind_rows(
        past_outliers,
        results %>%
          filter(WoD == ref_date) %>%
          select(WoD, y, week_in_year, t)
      )
    }

    # Construct parameter guess for next iteration
    theta_start <- coef_fit * 0.7

  }
  return(results)
}
