#' @title Probabilistic reconciliation of mixed hierarchies
#'
#' @description
#'
#' `bayesRecon_MixCond` specifies Mixed-Conditioning (Mix-Cond) reconciliation for use within 
#' `reconcile()`. The method uses importance sampling to draw samples from the reconciled
#' forecast distribution, obtained via conditioning, in the case of a mixed hierarchy.
#' 
#' `bayesRecon_TDcond` specifies Top-Down Conditioning reconciliation for use within 
#' `reconcile()`. The method uses a top-down conditioning algorithm: first, upper base forecasts 
#' are reconciled via conditioning using only the hierarchical constraints between the
#' upper; then, the bottom distributions are updated via a probabilistic top-down procedure.
#' 
#' Reconciliation is performed when `forecast()` is called on the resulting model.
#' Marginal reconciled forecasts follow a sample distribution.
#'
#'
#' @param models A list of fitted models to reconcile.
#' @param n_samples Number of samples to draw from the reconciled distribution.
#' @param suppress_warnings If `TRUE`, suppress warnings from reconciliation.
#' 
#' @references
#' Zambon, L., Azzimonti, D., Rubattu, N., Corani, G. (2024).
#' *Probabilistic reconciliation of mixed-type hierarchical time series*.
#' Proceedings of the Fortieth Conference on Uncertainty in Artificial Intelligence,
#' PMLR 244:4078-4095. <https://proceedings.mlr.press/v244/zambon24a.html>.
#'
#' @seealso [fabletools::reconcile()], [fabletools::aggregate_key()], 
#' [bayesRecon_BUIS()], [bayesRecon::reconc_MixCond()], [bayesRecon::reconc_TDcond()]
#'
#' @name bayesRecon_MixCond
 
#' @rdname bayesRecon_MixCond
#'
#' @export
bayesRecon_MixCond <- function(models, n_samples = 1000, suppress_warnings = TRUE) {
  structure(models, class = c("lst_bayesRecon_MixCond", "mdl_lst", "list"),
            n_samples = n_samples, suppress_warnings = suppress_warnings)
}

#' forecast.lst_bayesRecon_MixCond
#'
#' Produces probabilistic forecasts reconciled via mixed hierarchy conditioning.
#' This method handles mixed hierarchies (with cross-constraints) by applying
#' conditioning-based reconciliation to draw samples from the reconciled distributions.
#'
#' @importFrom fabletools forecast distribution_var
#' @importFrom distributional generate dist_sample
#' @importFrom bayesRecon .core_reconc_MixCond schaferStrimmer_cov
#'
#' @method forecast lst_bayesRecon_MixCond
#'
#' @param object An object of class `lst_bayesRecon_MixCond` containing fitted models.
#' @param key_data A keyed data frame from `fabletools`.
#' @param point_forecast A list of point forecast functions (default: `list(.mean = mean)`).
#' @param new_data Optional new data for forecasting (not currently used).
#' @param ... Additional arguments passed to other methods.
#'
#' @return A fable object with MixCond-reconciled distributions and point forecasts.
#'
#' @keywords internal
#' @export
forecast.lst_bayesRecon_MixCond <- function(
  object,
  key_data,
  point_forecast = list(.mean = mean),
  new_data = NULL,
  ...
) {
  # Take models from fabletools, and prepare for BUIS
  # Produce the structural matrix from the key_data structure
  S <- fabletools::coherent_smat(key_data)
  
  # applies the next method ("mdl_lst", in class definition above) to extract the fitted models.
  fc <- NextMethod()
  
  # Series of lapply to extract the parameters of the distribution
  fc_dist <- lapply(fc, function(x) x[[distribution_var(x)]])
  
  ##### START OUR REWRITE OF reconc_MixCond() with distributional
  hier <- get_hier(S, fc_dist)
  A <- hier$A
  base_forecast_h <- hier$base_forecast_h
  upr_ts <- hier$upr_ts
  btm_ts <- hier$btm_ts
  btm_idx <- hier$btm_idx
  n_upr <- hier$n_upr
  
  # Extrapolate additional parameters from the object attributes
  n_samples <- attr(object, "n_samples")
  suppress_warnings <- attr(object, "suppress_warnings")
  
  # Compute upper sample covariance, drop rows containing nans
  res_upr <- get_residuals(object, upr_ts, btm_ts, btm_idx, n_upr, "upper")
  if (n_upr == 1){
    upr_covm <- matrix(crossprod(res_upr)/nrow(res_upr))
  } else {
    upr_covm <- schaferStrimmer_cov(res_upr)$shrink_cov
  }
  
  # For all horizon steps ahead, apply independently
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    # Save upper point forecast vector and bottom samples matrix
    mean_upper <- base_forecasts[seq_len(n_upr)] |> mean()
    B <- base_forecasts[-seq_len(n_upr)] |> generate(times = n_samples) |> do.call(what=cbind)
    
    out <- .core_reconc_MixCond(
      A = A,
      B = B,
      mean_upper = mean_upper,
      cov_upper = upr_covm,
      num_samples = n_samples,
      return_type = "samples",
      suppress_warnings = suppress_warnings
    )
    
    # Return reconciled samples as a distributional object
    Y_reconc = rbind(out$upper_rec_samples, out$bottom_rec_samples)
    return(dist_sample(split(Y_reconc, row(Y_reconc))))
  })
  ###### END REWRITE

  # Fable needs the horizon and models in a different format
  # Invert horizon <-> model. 
  fc_dist <- transpose_vec(fc_dist)

  # MixCond returns the upper/bottom ordering with upper on top and bottom below
  # fable takes in input series in any arbitrary position so we need to invert back
  # Invert <A/B> smat ordering to arbitrary key_data order
  fc_dist <- fc_dist[order(c(upr_ts, btm_ts[btm_idx]))]
  get_output_fc(fc, fc_dist, point_forecast)
}
