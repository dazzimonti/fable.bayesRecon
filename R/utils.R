#' Transpose a list of vectors by position
#'
#' Takes a list of equal-length vectors and returns a list where each element
#' contains the values at a given position across the input list.
#'
#' @details
#' This helper function is useful for converting between horizon-indexed and
#' model-indexed forecasts in hierarchical reconciliation.
#'
#' @param .l A list of vectors with the same length.
#'
#' @return A list of vectors with length equal to the input vector length.
#'
#' @keywords internal
#' @noRd
#' @importFrom vctrs vec_c vec_slice
#' @importFrom rlang abort enquo
transpose_vec <- function(.l) {
  result <- lapply(seq_along(.l[[1]]), function(i) {
    do.call(vec_c, lapply(.l, vec_slice, i))
  })
  return(result)
}

#' Extract hierarchy metadata and base forecasts
#'
#' Computes indices for upper and bottom series, extracts the hierarchy
#' submatrix, and reorders base forecast distributions for bayesRecon.
#'
#' @param S A summing matrix for the hierarchy.
#' @param fc_dist A list of base forecast distributions, one per series.
#'
#' @return A list containing:
#'   \\item{upr_ts}{Indices of upper-level series.}
#'   \\item{btm_ts}{Indices of bottom-level series.}
#'   \\item{A}{The constraint matrix for upper-level reconciliation.}
#'   \\item{btm_idx}{Reordering indices for bottom series.}
#'   \\item{base_forecast_h}{Reordered and transposed distributions.}
#'   \\item{n_upr}{Number of upper-level series.}
#'   \\item{n_btm}{Number of bottom-level series.}
#'   \\item{n_tot}{Total number of series.}
#'
#' @keywords internal
#' @noRd
get_hier <- function(S, fc_dist){
  upr_ts <- which(rowSums(S) > 1)
  btm_ts <- which(rowSums(S) == 1)
  A <- S[rowSums(S) > 1, , drop = FALSE]
  btm_idx <- apply(S[btm_ts, , drop = FALSE], 1, \(x) which(as.logical(x)))
  base_forecast_h <- transpose_vec(fc_dist[c(upr_ts, btm_ts[btm_idx])])
  n_upr <- nrow(A)
  n_btm <- ncol(A)
  n_tot <- n_upr + n_btm
  return(list(
    upr_ts = upr_ts,
    btm_ts = btm_ts,
    A = A,
    btm_idx = btm_idx,
    base_forecast_h = base_forecast_h,
    n_upr = n_upr,
    n_btm = n_btm,
    n_tot = n_tot
  ))
}


#' Assemble fable-compatible forecast output
#'
#' Inserts reconciled distributions and computed point forecasts into
#' the fable forecast object.
#'
#' @param fc List of forecast objects.
#' @param fc_dist List of distributions to insert.
#' @param point_forecast Point forecast spec passed to fabletools.
#'
#' @return A list of updated forecast objects with reconciled distributions
#'   and point forecasts inserted.
#'
#' @importFrom purrr map2
#' @importFrom fabletools distribution_var
#'
#' @keywords internal
#' @noRd
get_output_fc <- function(fc, fc_dist, point_forecast){
  # The code below is Mitch magic that makes the returned object compatible with fable pipeline
  # you can copy paste this in other functions
  # In the next iteration of fable this will become a proper function 
  # (or it won't be needed anymore because it will be handled outside of the reconcile functions)
  out_fc <- map2(fc, fc_dist, function(fc, dist) {
    dimnames(dist) <- dimnames(fc[[distribution_var(fc)]])
    fc[[distribution_var(fc)]] <- dist
    point_fc <- compute_point_forecasts(dist, point_forecast)
    fc[names(point_fc)] <- point_fc
    fc
  })
  return(out_fc)
}


#' Extract residuals for hierarchy levels
#'
#' Computes residuals from model objects and optionally returns a subset
#' corresponding to upper-level, bottom-level, or all series in the hierarchy.
#'
#' @param object List-like collection of model objects.
#' @param upr_ts Integer indices for upper series.
#' @param btm_ts Integer indices for bottom series.
#' @param btm_idx Integer order indices for bottom series.
#' @param level Which level of residuals to return: "all", "upper", or "bottom".
#'
#' @return A numeric matrix of residuals with complete cases only.
#' @importFrom purrr map map_dbl reduce exec
#' @importFrom dplyr full_join
#' @importFrom stats complete.cases residuals
#' @importFrom tsibble index_var
#' @keywords internal
#' @noRd
get_residuals <- function(object, upr_ts, btm_ts, btm_idx, n_upr, 
                          level=c("all","upper","bottom")){
  level <- match.arg(level)
  # Compute sample covariance
  res <- map(
    object[c(upr_ts, btm_ts[btm_idx])], 
    function(x, ...) residuals(x, ...), type = "response")
  if(length(unique(map_dbl(res, nrow))) > 1){
    # Join residuals by index #199
    res <- unname(as.matrix(reduce(res, full_join, by = index_var(res[[1]]))[,-1]))
  } else {
    res <- matrix(exec(c, !!!map(res, `[[`, 2)), ncol = length(object))
  }
  # select the level of residuals to return
  if (level == "upper") {
    res <- res[, seq_len(n_upr), drop = FALSE]
  } else if (level == "bottom") {
    res <- res[, -seq_len(n_upr), drop = FALSE]
  }
  # Drop lines with NAs
  res <- res[complete.cases(res), , drop = FALSE]
  return(res)
}

  
#' Copied from fabletools/R/utils.R
#' @keywords internal
#' @noRd
calc <- function(f, ...){
  f(...)
}

#' Copied from fabletools/R/forecast.R
#' @importFrom purrr map
#' @keywords internal
#' @noRd
compute_point_forecasts <- function(distribution, measures){
  map(measures, calc, distribution)
}