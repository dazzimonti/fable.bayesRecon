#' @title Probabilistic forecast reconciliation of mixed hierarchies via top-down conditioning
#'
#' @description
#'
#' Uses the top-down conditioning algorithm to draw samples from the reconciled
#' forecast distribution. Reconciliation is performed in two steps: 
#' first, the upper base forecasts are reconciled via conditioning, 
#' using only the hierarchical constraints between the upper variables; then,
#' the bottom distributions are updated via a probabilistic top-down procedure.
#' 
#' @param models A list of fitted models to reconcile.
#'
#' @return An object of class `lst_bayesRecon_TDcond`.
#'
#' @export
bayesRecon_TDcond <- function(models) {
  # For this I need an explanation
  structure(models, class = c("lst_bayesRecon_TDcond", "lst_mdl", "list"))
}

#' forecast.lst_bayesRecon_TDcond
#' 
#' @importFrom fabletools forecast distribution_var
#' @importFrom distributional generate dist_sample dist_truncated support
#' @importFrom stats density
#' @importFrom purrr map2
#' @importFrom vctrs field
#' @importFrom bayesRecon .core_reconc_TDcond schaferStrimmer_cov
#' 
#' @method forecast lst_bayesRecon_TDcond 
#' 
#' @param object TODO
#' @param key_data TODO
#' @param point_forecast TODO 
#' @param new_data TODO
#' @param n_samples number of samples for output distribution
#' @param suppress_warnings if TRUE warnings are not returned
#' @param ... extra parameters to be passed on.
#' 
#' @description takes a list of of models and returns a list of reconciled models
#' @export
forecast.lst_bayesRecon_TDcond <- function(
    object,
    key_data,
    point_forecast = list(.mean = mean),
    new_data = NULL,
    n_samples = 1000,
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
  
  ##### START OUR REWRITE OF reconc_TDcond() with distributional
  hier <- get_hier(S, fc_dist)
  A <- hier$A
  base_forecast_h <- hier$base_forecast_h
  upr_ts <- hier$upr_ts
  btm_ts <- hier$btm_ts
  btm_idx <- hier$btm_idx
  n_upr <- hier$n_upr
  
  # Compute upper sample covariance, drop rows containing nans
  res_upr <- get_residuals(object, upr_ts, btm_ts, btm_idx, "upper")
  if (n_upr == 1){
    upr_covm <- matrix(crossprod(res_upr)/nrow(res_upr))
  } else {
    upr_covm <- schaferStrimmer_cov(res_upr)$shrink_cov
  }
  
  # Iterate the reconciliation across horizons
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    # Save upper point forecast and bottom PMF
    mu_u <- base_forecasts[seq_len(n_upr)] |> mean()
    L_pmf <- make_PMF(base_forecasts[-seq_len(n_upr)])

    # Apply the core function from bayesRecon
    out <- .core_reconc_TDcond(
      A = A,
      mu_u = mu_u,
      Sigma_u = upr_covm,
      L_pmf = L_pmf,
      num_samples =  n_samples,
      return_type = "samples", 
      suppress_warnings = FALSE
    )
    
    # Return reconciled samples as a distributional object
    Y_reconc = rbind(out$upper_reconciled$samples, out$bottom_reconciled$samples)
    return(dist_sample(split(Y_reconc, row(Y_reconc))))
  })
  # END REWRITE
  
  # Fable needs the horizon and models in a different format
  # Invert horizon <-> model. 
  fc_dist <- transpose_vec(fc_dist)
  
  # MixCond returns the upper/bottom ordering with upper on top and bottom below
  # fable takes in input series in any arbitrary position so we need to invert back
  # Invert <A/B> smat ordering to arbitrary key_data order
  fc_dist <- fc_dist[order(c(upr_ts, btm_ts[btm_idx]))]
  get_output_fc(fc, fc_dist, point_forecast)
}


make_PMF <- function(dist, negative_to_zero = FALSE, toll = 1e-9, num_samples = 1e04, alpha_smoothing = 1e-9){
  supp <- dist |> support()
  # Identify the negatively supported distirbutions and truncate them at zero
  neg_lb <- supp |> field("lim") |> vapply(\(x) x[1], numeric(1)) < 0
  if (any(neg_lb)){
    warning("Negative support corrected via truncation")
    dist[neg_lb] <- dist[neg_lb] |> dist_truncated(0, Inf)
  }
  #selec thigs withs support not in N0 or N+ and truncate them at zero
  int_val <- supp |> format() |> names() %in% c("N0", "N+", "Z") 
  if (!all(int_val)){
    warning("Non-integer support corrected via rounding")
    dist[!int_val] <- dist[!int_val] |> generate(num_samples) |> 
      lapply(\(x) as.integer(x)) |> dist_sample()
  }
  # Compute the pmf up to a certain quantile to bound it
  M <- dist |> quantile(1 - toll)
  pmf <- map2(dist, M, ~ density(.x, 0:.y)[[1]])
  return(pmf)
}
