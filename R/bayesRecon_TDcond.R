#' @rdname bayesRecon_MixCond
#'
#' @export
bayesRecon_TDcond <- function(models, n_samples = 1000, suppress_warnings = TRUE) {
  structure(models, class = c("lst_bayesRecon_TDcond", "lst_mdl", "list"),
            n_samples = n_samples, suppress_warnings = suppress_warnings)
}

#' forecast.lst_bayesRecon_TDcond
#' 
#' Produces probabilistic forecasts reconciled via top-down conditioning.
#' Upper base forecasts are reconciled via conditioning. The recondiled
#' upper forecasts are then propagate to bottom via probabilisitc top-down.
#' 
#' @importFrom fabletools forecast distribution_var
#' @importFrom distributional dist_sample dist_truncated support
#' @importFrom stats density
#' @importFrom vctrs field
#' @importFrom bayesRecon .core_reconc_TDcond schaferStrimmer_cov
#' 
#' @method forecast lst_bayesRecon_TDcond 
#' 
#' @param object An object of class `lst_bayesRecon_MixCond` containing fitted models.
#' @param key_data A keyed data frame from `fabletools`.
#' @param point_forecast A list of point forecast functions (default: `list(.mean = mean)`).
#' @param new_data Optional new data for forecasting (not currently used).
#' @param ... Additional arguments passed to other methods.
#' @param ... extra parameters to be passed on.
#' 
#' @return A fable object with TDcond-reconciled distributions and point forecasts.
#' 
#' @keywords internal
#' @export
forecast.lst_bayesRecon_TDcond <- function(
    object,
    key_data,
    point_forecast = list(.mean = mean),
    new_data = NULL,
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
  
  # Extract parameters from the object attributes
  n_samples <- attr(object, "n_samples")
  suppress_warnings <- attr(object, "suppress_warnings")
  
  # Compute upper sample covariance, drop rows containing NANs
  res_upr <- get_residuals(object, upr_ts, btm_ts, btm_idx, n_upr, "upper")
  if (n_upr == 1){
    upr_covm <- matrix(crossprod(res_upr)/nrow(res_upr))
  } else {
    upr_covm <- schaferStrimmer_cov(res_upr)$shrink_cov
  }
  
  # Iterate the reconciliation across horizons
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    # Save upper point forecast and bottom PMF
    mean_upper <- base_forecasts[seq_len(n_upr)] |> mean()
    L_pmf <- make_PMF(base_forecasts[-seq_len(n_upr)])

    # Apply the core function from bayesRecon
    out <- .core_reconc_TDcond(
      A = A,
      mean_upper = mean_upper,
      cov_upper = upr_covm,
      L_pmf = L_pmf,
      num_samples =  n_samples,
      return_type = "samples", 
      min_fraction_samples_ok = .5,
      suppress_warnings = suppress_warnings
    )
    
    # Return reconciled samples as a distributional object
    Y_reconc = rbind(out$upper_rec_samples, out$bottom_rec_samples)
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


#' Create a discrete PMF from distributions
#'
#' Converts a set of distributional objects into a list of discrete PMFs used by
#' the top-down conditioning reconciliation. The function enforces nonnegative,
#' integer support by truncating negative supports and rounding non-integer
#' supports via sampling, then estimates the PMF up to the (1 - toll)
#' quantile.
#'
#' @param dist A vector or list of `distributional` objects.
#' @param negative_to_zero Logical, currently unused. Reserved for future use.
#' @param toll Tail probability used to truncate the support when estimating the PMF.
#' @param alpha_smoothing Numeric, currently unused. Reserved for future use.
#'
#' @return A list of numeric vectors, one PMF per input distribution.
#'
#' @importFrom distributional dist_truncated support dist_inflated cdf
#' @importFrom vctrs field
#' @importFrom purrr map2 pmap
#' @importFrom stats density quantile
#'
#' @keywords internal
make_PMF <- function(dist, negative_to_zero = FALSE, toll = 1e-9, alpha_smoothing = 1e-9){
  wm <- character(0)
  supp <- dist |> support()
  # Identify the negatively supported distributions
  neg_lb <- supp |> field("lim") |> vapply(\(x) x[1], numeric(1)) < 0
  if (any(neg_lb)){
    if (negative_to_zero){
      # Identify the mass below zero and make the distribution zero-inflated
      wm <- c(wm, "Negative support corrected via zero-inflation.")
      cdfzero <- dist[neg_lb] |> cdf(-toll)
      dist[neg_lb] <- map2(dist[neg_lb], cdfzero, 
                           \(d, p0) d |> 
                           dist_truncated(0, Inf) |> 
                           dist_inflated(p0, 0))
    } else {
      # Simply truncate the distribution
      wm <- c(wm, "Negative support corrected via truncation.")
      dist[neg_lb] <- dist[neg_lb] |> dist_truncated(0, Inf)
    }
  }
  # Identify real-supported distributions
  int_val <- supp |> format() |> names() %in% c("N0", "N+", "Z")
  if (any(!int_val)){
    wm <- c(wm, "Non-integer support obtained via cdf rounding.")
  }
  # Find the upper value for the pmf esitimation
  M <- dist |> quantile(1 - toll) |> ceiling() |> as.numeric()
  pmf <- pmap(list(d = dist, is_int = int_val, m = M), \(d, is_int, m) {
      if (!is_int) {
        # Approximate the pmf via cdf differences
        mass <- d |> cdf(0:m + 0.5) |> 
          (\(x) as.numeric(unlist(x, recursive = TRUE, use.names = FALSE)))() |>
          (\(x) diff(c(0, x)))()
      } else {
        # Simply estimate the pmf
        mass <- d |> density(0:m) |> 
          (\(x) as.numeric(unlist(x, recursive = TRUE, use.names = FALSE)))()
      }
      return(mass)
    }
  )
  # Display warnings if needed
  if (length(wm) > 0) warning(paste(unique(wm), collapse = " "), call. = FALSE)
  return(pmf)
}
