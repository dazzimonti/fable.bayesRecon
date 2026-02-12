#' Student-t forecast reconciliation
#'
#' Creates an object for probabilistic forecast reconciliation using
#' Student-t distributions. This method applies a multivariate Student-t model
#' to both prior and posterior estimation, accounting for heavier tails than
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
#' @importFrom stats density
#' @importFrom purrr map map2 map_dbl list_c reduce invoke
#' @importFrom vctrs vec_c vec_slice
#' @importFrom bayesRecon .core_reconc_t schaferStrimmer_cov multi_log_score_optimization
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
#' @param prior Optional list with `$nu` and `$Psi` for prior specification.
#' @param posterior Optional list with `$nu` and `$Psi` for posterior specification.
#' @param l_shr Shrinkage intensity (0 to 1) applied to sample covariance (default: 1e-4).
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
    prior = NULL,
    posterior = NULL,
    l_shr = 1e-4,
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

  ## START OUR REWRITE OF bayesRecon_t WITH distirbutional
  hier <- get_hier(S, fc_dist)
  A <- hier$A
  base_forecast_h <- hier$base_forecast_h
  upr_ts <- hier$upr_ts
  btm_ts <- hier$btm_ts
  btm_idx <- hier$btm_idx
  n_tot <- hier$n_tot

  if (!all(map(base_forecast_h, family) |> list_c() == "normal")) {
    stop("t-reconciliation works under the assumption of Normal forecasts")
  }
  
  # Compute the covariance matrix of the residuals
  res <- get_residuals(object, upr_ts, btm_ts, btm_idx)
  covm_res <- crossprod(res) / nrow(res) 
  covm_res <- (1 - l_shr)*covm_res + l_shr*diag(diag(covm_res))  
  
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
    if (!is.null(prior)) {
      # Try to get dirty the prior from the argument
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
      # Compute the prior from observed data

      # Obtain past observations in matrix format
      obs = map(object[c(upr_ts, btm_ts[btm_idx])], ~.$data)
      if(length(unique(map_dbl(obs, nrow))) > 1){
        # Join observed by index #199
        obs <- unname(as.matrix(reduce(obs, full_join, by = index_var(res[[1]]))[,-1]))
      } else {
        obs <- matrix(invoke(c, map(obs, `[[`, 2)), ncol = length(object))
      }
      # Compute the residuals of the naive
      res_naive = diff(obs)
      # TODO: handle seasonal naive (with diff(obs, freq))
      covm_naive = schaferStrimmer_cov(res_naive)$shrink_cov
      
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
      Psi = Psi_post,
      nu = nu_post,
      return_uppers = TRUE,
      return_parameters = FALSE,
      suppress_warnings = FALSE
    )
    
    # Return simply a list of distirbutional: a multivariate-t for the upper and one for the bottom
    # return(list(
    #   distributional::dist_multivariate_t(
    #     df = nu_tilde,
    #     mu = list(u_tilde),
    #     sigma = list(Sigma_tilde_U)
    #   ),
    #   distributional::dist_multivariate_t(
    #     df = nu_tilde,
    #     mu = list(b_tilde),
    #     sigma = list(Sigma_tilde_B)
    #   )))
    
    # Return the distributional Student-t distribution
    return(dist_student_t(
      df = out$bottom_df,
      mu = c(out$upper_mean, out$bottom_mean),
      # S: to check wheter the following has to be scaled through sqrt
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
