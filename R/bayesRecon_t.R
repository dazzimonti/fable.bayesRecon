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
#' @importFrom distributional mean
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
#' @param object TODO
#' @param key_data TODO
#' @param point_forecast TODO 
#' @param new_data TODO
#' @param prior Optional list containing 'nu' and 'Psi' (prior parameters).
#' @param posterior Optional list containing 'nu' and 'Psi' (posterior parameters).
#' @param l_shr Shrinkage intensity (0 to 1) for stabilizing the sample covariance matrix (default 1e-4).
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
    prior = NULL,
    posterior = NULL,
    l_shr = 1e-4,
    suppress_warnings = TRUE,
    ...
) {
  # Take models from fabletools, and prepare for t-rec
  # build_key_data_smat, does this create the aggregation matrix from key_data encoding, created by aggregate_key function.
  S <- get_S(key_data)
  
  # applies the next method ("lst_mdl", in class definition above) to extract the fitted models.
  fc <- NextMethod()
  
  # Series of lapply to extract the parameters of the distribution
  fc_dist <- lapply(fc, function(x) x[[fabletools::distribution_var(x)]])

  ##### START OUR REWRITE OF reconc_t() with distributional
  hier <- get_hier(S, fc_dist)
  A <- hier$A
  base_forecast_h <- hier$base_forecast_h
  
  if (!all(purrr::map(base_forecast_h, family) |> purrr::list_c() == "normal")){
    stop("t-reconciliation works under the assumption of Normal forecasts")
  }
  
  # Compute the covariance matrix of the residuals
  res <- get_residuals(object, hier$upr_ts, hier$btm_ts, hier$btm_idx)
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
      upr_ts <- hier$upr_ts
      btm_ts <- hier$btm_ts
      btm_idx <- hier$btm_idx
      n_tot <- hier$n_tot
      # Obtain past observations in matrix format
      obs = purrr::map(object[c(upr_ts, btm_ts[btm_idx])], ~.$data)
      if(length(unique(purrr::map_dbl(obs, nrow))) > 1){
        # Join observed by index #199
        obs <- unname(as.matrix(reduce(obs, full_join, by = index_var(res[[1]]))[,-1]))
      } else {
        obs <- matrix(purrr::invoke(c, purrr::map(obs, `[[`, 2)), ncol = length(object))
      }
      # Compute the residuals of the naive
      res_naive = diff(obs)
      # TODO: handle seasonal naive (with diff(obs, freq))
      covm_naive = bayesRecon::schaferStrimmer_cov(res_naive)$shrink_cov
      
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
    point_fc = purrr::map_dbl(base_forecasts, mean)
    out = bayesRecon::.core_reconc_t(
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
    return(distributional::dist_student_t(
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
