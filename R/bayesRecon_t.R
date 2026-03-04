#' Student-t forecast reconciliation
#'
#' Creates an object for probabilistic forecast reconciliation using
#' Student-t distributions. This method applies a multivariate Student-t model
#' to posterior estimation, accounting for heavier tails than
#' normal distributions.
#'
#' @param models A list of fitted models to reconcile.
#'
#' @return An object of class `lst_bayesRecon_t`.
#'
#' @export
#'
#' @examples
#' # bayesRecon_t(base_models)
bayesRecon_t <- function(models) {
  structure(models, class = c("lst_bayesRecon_t", "lst_mdl", "list"))
}

#' forecast.lst_bayesRecon_t
#'
#' Produces probabilistic forecasts reconciled via Student-t reconciliation.
#' The method reconciles base forecasts from upper and bottom level series
#' using a multivariate Student-t model with flexible prior/posterior specification.
#'
#' @importFrom fabletools forecast distribution_var
#' @importFrom stats density family frequency
#' @importFrom purrr map map_dbl map_int list_c reduce exec
#' @importFrom vctrs vec_c vec_slice
#' @importFrom bayesRecon .core_reconc_t schaferStrimmer_cov multi_log_score_optimization .compute_naive_cov
#' @importFrom dplyr full_join
#' @importFrom tsibble index_var
#' @importFrom distributional dist_student_t
#'
#' @method forecast lst_bayesRecon_t
#'
#' @param object An object of class `lst_bayesRecon_t` containing fitted  models.
#' @param key_data A keyed data frame from `fabletools`.
#' @param point_forecast A list of point forecast functions (default: `list(.mean = mean)`).
#' @param new_data Optional new data for forecasting (not currently used).
#' @param suppress_warnings If `TRUE`, suppress warnings from reconciliation.
#' @param ... Additional arguments passed to other methods.
#'
#' @return A fable object with reconciled distributions and point forecasts.
#'
#' @export
forecast.lst_bayesRecon_t <- function(
    object,
    key_data,
    point_forecast = list(.mean = mean),
    new_data = NULL,
    suppress_warnings = TRUE,
    ...
) {
  # Take models from fabletools, and prepare for BUIS
  # build_key_data_smat, does this create the aggregation matrix from key_data encoding, created by aggregate_key function.
  S <- get_S(key_data)
  
  # applies the next method ("lst_mdl", in class definition above) to extract the fitted models.
  fc <- NextMethod()
  
  # Series of lapply to extract the parameters of the distribution
  fc_dist <- lapply(fc, function(x) x[[distribution_var(x)]])

  ## START OUR REWRITE OF bayesRecon_t WITH distributional
  hier <- get_hier(S, fc_dist)
  A <- hier$A
  base_forecast_h <- hier$base_forecast_h
  upr_ts <- hier$upr_ts
  btm_ts <- hier$btm_ts
  btm_idx <- hier$btm_idx
  n_tot <- hier$n_tot

  # Check that base forecasts are Normal
  if (!all(map(base_forecast_h, family) |> list_c() == "normal")) {
    stop("t-reconciliation works under the assumption of Normal forecasts")
  }
  
  # Extrapolate additional arguments for prior/posterior specification
  add_args <- list(...)
  prior <- add_args$prior
  posterior <- add_args$posterior
  freq <- add_args$freq
  criterion <- ifelse(is.null(add_args$criterion), "RSS", add_args$criterion)
  l_shr <- ifelse(is.null(add_args$l_shr), 1e-04, add_args$l_shr)
  
  if (!is.null(posterior)) {
    # Try to get dirtly the posterior from the argument
    if (is.list(posterior)) {
      nu_post = posterior$nu
      Psi_post = posterior$Psi
      if (is.null(nu_post) | is.null(Psi_post)) {
        stop("Input error: posterior must be a list with entries nu and Psi")
      }
    } else {
      stop("Input error: posterior must be a list with entries nu and Psi")
    }
  } else {
    # Compute the covariance matrix of the residuals
    res <- get_residuals(object, upr_ts, btm_ts, btm_idx)
    covm_res <- crossprod(res) / nrow(res) 
    covm_res <- (1 - l_shr)*covm_res + l_shr*diag(diag(covm_res))
    
    if (!is.null(prior)) {
      # Try to get directly the prior from the argument
      if (is.list(prior)) {
        nu_prior = prior$nu
        Psi_prior = prior$Psi
        if (is.null(nu_prior) | is.null(Psi_prior)) {
          stop("Input error: prior must be a list with entries nu and Psi")
        }
      } else {
        stop("Input error: prior must be a list with entries nu and Psi")
      }
    } else {
      # Compute the prior from observed data ; obtain them in matrix format
      obs = map(object[c(upr_ts, btm_ts[btm_idx])], ~.$data)
      if(length(unique(map_dbl(obs, nrow))) > 1){
        # Join observed by index #199
        obs <- unname(as.matrix(reduce(obs, full_join, by = index_var(res[[1]]))[,-1]))
      } else {
        obs <- matrix(exec(c, !!!map(obs, `[[`, 2)), ncol = length(object))
      }
      
      # Identify the frequency and compute the residuals of the naive forecasts
      if (is.null(freq)){
        freq <- map_int(object[c(upr_ts, btm_ts[btm_idx])], ~ frequency(.$data))
        freq <- ifelse(length(unique(freq)) == 1, unique(freq), 1)
      }
      covm_naive <- .compute_naive_cov(obs, freq = freq, criterion = criterion)
      
      # Identify optimal prior parameters via LOO-CV
      loo_cv = multi_log_score_optimization(res, covm_naive)
      nu_prior = loo_cv$optimal_nu
      Psi_prior = (nu_prior - n_tot - 1) * covm_naive
    }
    # Compute posterior parameters
    Psi_post = Psi_prior + nrow(res) * covm_res
    nu_post = nu_prior + nrow(res)
  }

  # For all horizon steps ahead, apply independently
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    # Extrapolate point forecast
    point_fc = map_dbl(base_forecasts, mean)
    out = .core_reconc_t(
      A = A,
      point_fc = point_fc,
      Psi_post = Psi_post,
      nu_post = nu_post,
      return_uppers = TRUE,
      return_parameters = FALSE,
      suppress_warnings = FALSE
    )
    
    # Return the distributional Student-t distribution
    return(dist_student_t(
      df = out$bottom_df,
      mu = c(out$upper_mean, out$bottom_mean),
      # S: to check whether the following has to be scaled through sqrt
      sigma = sqrt(c(diag(out$upper_scale_matrix), diag(out$bottom_scale_matrix)))
    ))
  })
  # END REWRITE OF bayesRecon_t
  
  # Fable needs the horizon and models in a different format
  # Invert horizon <-> model.
  fc_dist <- transpose_vec(fc_dist)
  
  # t returns the upper/bottom ordering with upper on top and bottom below
  # fable takes in input series in any arbitrary position so we need to invert back
  # Invert <A/B> smat ordering to arbitrary key_data order
  fc_dist <- fc_dist[order(c(upr_ts, btm_ts[btm_idx]))]
  get_output_fc(fc, fc_dist, point_forecast)
}
