transpose_vec <- function(.l) {
  result <- lapply(seq_along(.l[[1]]), function(i) {
    do.call(vctrs::vec_c, lapply(.l, vctrs::vec_slice, i))
  })
}

#' TODO
#'
#' @description
#'
#' TODO
#'
#' @param models A list of fitted models to reconcile.
#'
#' @return An object of class `lst_bayesRecon_t`.
#'
#' @export
bayesRecon_t <- function(models) {
  # For this I need an explanation
  structure(models, class = c("lst_bayesRecon_t", "lst_mdl", "list"))
}

#' forecast.lst_bayesRecon_t
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
#' 
#' @method forecast lst_bayesRecon_t 
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
forecast.lst_bayesRecon_t <- function(
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
  agg_data <- fabletools:::build_key_data_smat(key_data)
  S <- matrix(
    0L,
    nrow = length(agg_data$agg),
    ncol = max(vctrs::vec_c(!!!agg_data$agg))
  )
  S[
    length(agg_data$agg) *
      (vctrs::vec_c(!!!agg_data$agg) - 1) +
      rep(seq_along(agg_data$agg), lengths(agg_data$agg))
  ] <- 1L
  
  # applies the next method ("lst_mdl", in class definition above) to extract the fitted models.
  browser()
  fc <- NextMethod()
  
  # Series of lapply to extract the parameters of the distribution
  fc_dist <- lapply(fc, function(x) x[[fabletools::distribution_var(x)]])

  
  ##### START OUR REWRITE OF reconc_t() with distributional
  upr_ts <- which(rowSums(S) > 1)
  btm_ts <- which(rowSums(S) == 1)
  A <- S[rowSums(S) > 1, , drop = FALSE]
  # The next two lines do indexing magic to return base_forecasts in the proper order for bayesRecon
  btm_idx <- apply(S[btm_ts, , drop = FALSE], 1, \(x) which(as.logical(x)))
  base_forecast_h <- transpose_vec(fc_dist[c(upr_ts, btm_ts[btm_idx])])

  # Extrapolate in-sample values for each t.s.
  fc_insample <- lapply(fc, function(x) x[[]])

  

  # For all horizon steps ahead, apply independently
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    # Save dimensions
    # S: move outside of the loop?
    n_upper = nrow(A)
    n_bottom = ncol(A)
    n_tot <- length(base_forecasts)
    
    # save two lists of upper and bottom fc
    upper_fc <- base_forecasts[seq_len(n_upper)]
    bottom_fc <- base_forecasts[-seq_len(n_upper)]
    
    res <- NULL
    ###### END REWRITE
  })
  
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