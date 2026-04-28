#'  t-Rec: reconciliation via conditioning with uncertain covariance via multivariate t-distribution
#'
#' @description
#' 
#' Specifies t-Rec reconciliation for use within `reconcile()`. 
#' 
#' Reconciles base forecasts by conditioning on the hierarchical constraints. 
#' The base forecasts are assumed to be jointly Gaussian, 
#' conditionally on the covariance matrix of the forecast errors. 
#' A Bayesian approach is adopted to account for the uncertainty of the covariance matrix. 
#' An Inverse-Wishart prior is specified on the covariance matrix, 
#' leading to a multivariate t-distribution for the base forecasts. 
#' The reconciliation via conditioning is in closed-form, 
#' yielding a multivariate t reconciled distribution.
#' 
#' Reconciliation is performed when `forecast()` is called on the resulting model.
#' Marginal reconciled forecasts follow a Student-t distribution.
#' 
#'
#' @param models A list of fitted models to reconcile.
#' @param ... Additional arguments passed to other methods, including:  
#' - `prior`: Optional list with entries `nu` and `Psi` specifying the parameters 
#' of the Inverse-Wishart prior distribution for the covariance matrix. 
#' If not provided, the prior is estimated from the data.
#' - `freq`: Optional frequency of the time series, used for estimating the naive covariance matrix
#' via seasonal naive residuals. If not provided, the frequency is inferred from the data.
#' - `criterion`: Criterion for estimating the naive covariance matrix (default: "RSS").
#' - `l_shr`: Optional shrinkage parameter (between 0 and 1) for the covariance matrix of the residuals. 
#'
#' @references
#' Carrara, C., Corani, G., Azzimonti, D., & Zambon, L. (2025). Modeling the uncertainty on the covariance
#' matrix for probabilistic forecast reconciliation. arXiv preprint arXiv:2506.19554.
#' \url{https://arxiv.org/abs/2506.19554}
#' 
#' @seealso [fabletools::reconcile()], [fabletools::aggregate_key()],
#'  [fabletools::min_trace()], [bayesRecon::reconc_t()]
#'
#' @export
bayesRecon_t <- function(models, ...) {
  structure(models, class = c("lst_bayesRecon_t", "lst_mdl", "list"),
            ...)
}

#' forecast.lst_bayesRecon_t
#'
#' Produces probabilistic forecasts reconciled via Student-t reconciliation.
#' The method reconciles base forecasts from upper and bottom level series
#' using a multivariate Student-t model with flexible prior/posterior specification.
#'
#' @importFrom fabletools forecast distribution_var
#' @importFrom stats family frequency cov2cor
#' @importFrom purrr map map_dbl map_int list_c reduce exec
#' @importFrom bayesRecon .core_reconc_t multi_log_score_optimization .compute_naive_cov
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
#' @param ... Additional arguments passed to other methods.
#'
#' @return A fable object with t-reconciled distributions and point forecasts.
#' 
#' @keywords internal
#' @export
forecast.lst_bayesRecon_t <- function(
    object,
    key_data,
    point_forecast = list(.mean = mean),
    new_data = NULL,
    ...
) {
  # Take models from fabletools, and prepare for BUIS
  # Produce the structural matrix from the key_data structure
  S <- fabletools::coherent_smat(key_data)
  
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
  n_upr <- hier$n_upr
  n_tot <- hier$n_tot

  # Check that base forecasts are Normal
  if (!all(map(base_forecast_h, family) |> list_c() == "normal")) {
    stop("t-reconciliation works under the assumption of Normal forecasts")
  }
  
  # Extract additional parameters specified as attributes 
  prior <- attr(object, "prior")
  freq <- attr(object, "freq")
  criterion <- attr(object, "criterion")
  l_shr <- attr(object, "l_shr")
  if (is.null(criterion)) criterion <- "RSS"
  if (is.null(l_shr)) l_shr <- 1e-04
  
  # Compute the covariance matrix of the residuals
  res <- get_residuals(object, upr_ts, btm_ts, btm_idx, n_upr)
  covm_res <- crossprod(res) / nrow(res) 
  R1 <- cov2cor(covm_res)
    
  # Try to get directly the prior from the argument
  if (!is.null(prior)) {
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
    
    # Compute the prior from observed data; obtain them in matrix format
    obs = map(object[c(upr_ts, btm_ts[btm_idx])], ~.$data)
    if(length(unique(map_dbl(obs, nrow))) > 1){
      obs <- unname(as.matrix(reduce(obs, full_join, by = index_var(obs[[1]]))[,-1]))
    } else {
      obs <- matrix(exec(c, !!!map(obs, `[[`, 2)), ncol = length(object))
    }
      
    # Identify the frequency and compute the residuals of the naive forecasts
    if (is.null(freq)){
      freq <- map_int(object[c(upr_ts, btm_ts[btm_idx])], ~ frequency(.$data))
      freq <- if (length(unique(freq)) == 1) unique(freq) else 1
    }
    covm_naive <- .compute_naive_cov(obs, freq = freq, criterion = criterion)
        
    # Identify optimal prior parameters via LOO-CV
    loo_cv = multi_log_score_optimization(res, covm_naive)
    nu_prior = loo_cv$optimal_nu
    Psi_prior = (nu_prior - n_tot - 1) * covm_naive
  }

  # For all horizon steps ahead, apply independently
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    # Compute posterior parameters if they are not directly specified
    var_h <- distributional::variance(base_forecasts)
    W_h <- diag(sqrt(var_h))%*%R1%*%t(diag(sqrt(var_h)))
    W_h <- (1 - l_shr)*W_h + l_shr*diag(diag(W_h))
    Psi_post = Psi_prior + nrow(res) * W_h
    nu_post = nu_prior + nrow(res)
    
    # Extrapolate point forecast
    base_fc_mean = map_dbl(base_forecasts, mean)
    out = .core_reconc_t(
      A = A,
      base_fc_mean = base_fc_mean,
      Psi_post = Psi_post,
      nu_post = nu_post,
      return_upper = TRUE,
      return_parameters = FALSE
    )
    
    # Return the distributional Student-t distribution
    return(dist_student_t(
      df = out$bottom_rec_df,
      mu = c(out$upper_rec_mean, out$bottom_rec_mean),
      # S: to check whether the following has to be scaled through sqrt
      sigma = sqrt(c(diag(out$upper_rec_scale_matrix), diag(out$bottom_rec_scale_matrix)))
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
