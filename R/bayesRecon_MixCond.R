transpose_vec <- function(.l) {
  result <- lapply(seq_along(.l[[1]]), function(i) {
    do.call(vctrs::vec_c, lapply(.l, vctrs::vec_slice, i))
  })
}

#' Probabilistic forecast reconciliation of mixed hierarchies via conditioning
#'
#' @description
#'
#' Uses importance sampling to draw samples from the reconciled
#' forecast distribution, obtained via conditioning, in the case of a mixed hierarchy. 
#'
#' @param models A list of fitted models to reconcile.
#'
#' @return An object of class `lst_bayesRecon_MixCond`.
#'
#' @export
bayesRecon_MixCond <- function(models) {
  # For this I need an explanation
  structure(models, class = c("lst_bayesRecon_MixCond", "lst_mdl", "list"))
}

#' forecast.lst_bayesRecon_MixCond
#' 
# @importFrom fabletools forecast distribution_var
#' @importFrom distributional generate dist_sample
#' @importFrom stats density
#' @importFrom purrr map2
#' @importFrom vctrs vec_c vec_slice
#' @import bayesRecon
#' @import dplyr
#' @import fable
#' @import tsibble
#' @import fabletools
# @importFrom bayesRecon .check_weights .resample
#' 
#' @method forecast lst_bayesRecon_MixCond 
#' 
#' 
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
forecast.lst_bayesRecon_MixCond <- function(
  object,
  key_data,
  point_forecast = list(.mean = mean),
  new_data = NULL,
  n_samples = 1000,
  suppress_warnings = TRUE,
  ...
) {
  # browser()
  # Take models from fabletools, and prepare for BUIS
  # build_key_data_smat, does this create the aggregation matrix from key_data encoding, created by aggregate_key function.
  S <- get_S(key_data)
  
  # applies the next method ("lst_mdl", in class definition above) to extract the fitted models.
  fc <- NextMethod()
  
  # Series of lapply to extract the parameters of the distribution
  fc_dist <- lapply(fc, function(x) x[[fabletools::distribution_var(x)]])
  
  ##### START OUR REWRITE OF reconc_MixCond() with distributional
  browser()
  hier <- get_hier(S, fc_dist)
  A <- hier$A
  base_forecast_h <- hier$base_forecast_h
  upr_ts <- hier$upr_ts
  btm_ts <- hier$btm_ts
  btm_idx <- hier$btm_idx
  n_upr <- hier$n_upr
  
  # Compute upper sample covariance
  res <- get_residuals(object, upr_ts, btm_ts, btm_idx)
  if (n_upr == 1){
    upr_covm <- matrix(crossprod(res[,1])/nrow(res))
  } else {
    upr_covm <- bayesRecon::schaferStrimmer_cov(res[,1:n_upr])$shrink_cov
  }
  
  # For all horizon steps ahead, apply independently
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    browser()
    # Save upper point forecast and bottom samples
    mu_u <- base_forecasts[seq_along(n_upr)] |> mean()
    B <- base_forecasts[-seq_along(n_upr)] |> generate(times = n_samples) |> do.call(what=cbind)
    
    out <- bayesRecon::.core_reconc_MixCond(
      A = A,
      B = B, 
      mu_u = mu_u,
      Sigma_u = upr_covm,
      num_samples = n_samples,
      return_type = "samples", 
      suppress_warnings = FALSE
    )
    
    # Return reconciled samples as a distributional object
    Y_reconc = rbind(out$upper_reconciled$samples, out$bottom_reconciled$samples)
    return(distributional::dist_sample(split(Y_reconc, row(Y_reconc))))
  })
  ###### END REWRITE

  # Fable needs the horizon and models in a different format
  # Invert horizon <-> model. 
  fc_dist <- transpose_vec(fc_dist)

  # MixCond returns the upper/bottom ordering with upper on top and bottom below
  # fable takes in input series in any arbitrary position so we need to invert back
  # Invert <A/B> smat ordering to arbitrary key_data order
  fc_dist <- fc_dist[order(c(upr_ts, btm_ts[btm_idx]))]
  # browser()
  # The code below is Mitch magic that makes the returned object compatible with fable pipeline
  # you can copy paste this in other functions
  # In the next iteration of fable this will become a proper function 
  # (or it won't be needed anymore because it will be handled outside of the reconcile functions)
  purrr::map2(fc, fc_dist, function(fc, dist) {
    dimnames(dist) <- dimnames(fc[[fabletools::distribution_var(fc)]])
    fc[[fabletools::distribution_var(fc)]] <- dist
    point_fc <- fabletools:::compute_point_forecasts(dist, point_forecast)
    fc[names(point_fc)] <- point_fc
    fc
  })
}
