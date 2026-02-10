transpose_vec <- function(.l) {
  result <- lapply(seq_along(.l[[1]]), function(i) {
    do.call(vctrs::vec_c, lapply(.l, vctrs::vec_slice, i))
  })
}


get_S <- function(key_data){
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
  return(S)
}


get_hier <- function(S, fc_dist){
  upr_ts <- which(rowSums(S) > 1)
  btm_ts <- which(rowSums(S) == 1)
  A <- S[rowSums(S) > 1, , drop = FALSE]
  # The next two lines do indexing magic to return base_forecasts in the proper order for bayesRecon
  btm_idx <- apply(S[btm_ts, , drop = FALSE], 1, \(x) which(as.logical(x)))
  base_forecast_h <- transpose_vec(fc_dist[c(upr_ts, btm_ts[btm_idx])])
  n_upr = nrow(A)
  n_btm = ncol(A)
  n_tot = n_upr + n_btm
  out = list(
    upr_ts = upr_ts,
    btm_ts = btm_ts,
    A = A,
    btm_idx = btm_idx,
    base_forecast_h = base_forecast_h,
    n_upr = n_upr,
    n_btm = n_btm,
    n_tot = n_tot
  )
  return(out)
}


get_residuals <- function(object, upr_ts, btm_ts, btm_idx){
  # Compute sample covariance
  res <- purrr::map(
    object[c(upr_ts, btm_ts[btm_idx])], 
    function(x, ...) residuals(x, ...), type = "response")
  if(length(unique(purrr::map_dbl(res, nrow))) > 1){
    # Join residuals by index #199
    res <- unname(as.matrix(reduce(res, full_join, by = index_var(res[[1]]))[,-1]))
  } else {
    res <- matrix(purrr::invoke(c, purrr::map(res, `[[`, 2)), ncol = length(object))
  }
  return(res)
}
  
  