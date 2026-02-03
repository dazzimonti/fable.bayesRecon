transpose_vec <- function(.l) {
  result <- lapply(seq_along(.l[[1]]), function(i) {
    do.call(vctrs::vec_c, lapply(.l, vctrs::vec_slice, i))
  })
}

# Copied from recon_t.R in bayesRecon package
#' Optimize Degrees of Freedom (nu) via LOO Cross-Validation
#'
#' @param res Matrix of residuals (n_obs x n_var).
#' @param prior_mean The prior mean covariance matrix (n_var x n_var).
#' @param trim Fraction of observations to trim (0 to 1). Default is 0.1 (10%).
#'
#' @details
#' TODO: check these details
#' 
#' \strong{Leave-One-Out (LOO) Cross-Validation:}
#' This function estimates the optimal degrees of freedom \eqn{\nu} by maximizing the 
#' out-of-sample predictive performance. This is achieved by computing the 
#' log-density of each held-out observation \eqn{\mathbf{r}_i} given the remaining 
#' data \eqn{\mathbf{R}_{-i}}. The total objective function is the sum of these 
#' predictive log-densities:
#' \deqn{\mathcal{L}(\nu) = \sum_{i=1}^T \log f(\mathbf{r}_i | \mathbf{R}_{-i}, \nu)}
#'
#' \strong{The Log-Density Function:}
#' For each LOO step, the residuals are assumed to follow a Multivariate 
#' Student-t distribution. The density is expressed directly as a function of the 
#' posterior sum-of-squares matrix \eqn{\Psi}, where \eqn{\Psi} scales implicitly with \eqn{\nu}:
#' \deqn{f(\mathbf{r}_i | \Psi, \nu) = \frac{\Gamma(\frac{\nu + T}{2})}{\Gamma(\frac{\nu + T - p}{2}) \pi^{p/2}} |\Psi|^{-1/2} \left( 1 + \mathbf{r}_i^\top \Psi^{-1} \mathbf{r}_i \right)^{-\frac{\nu + T}{2}}}
#' In the code, \eqn{\Psi} is constructed as:
#' \deqn{\Psi = (\nu - p - 1)\bar{\Sigma}_{prior} + \mathbf{R}^\top\mathbf{R}}
#' By using this formulation, the standard scaling factors \eqn{1/\nu} and \eqn{\nu^{-p/2}} 
#' are absorbed into the matrix inverse and determinant, respectively.
#'
#' \strong{Efficient Computation via Sherman-Morrison:}
#' Rather than recomputing \eqn{\Psi_{-i}} and its inverse \eqn{T} times, the function uses 
#' the full-sample matrix \eqn{\Psi} and adjusts it using the leverage 
#' \eqn{h_i = \mathbf{r}_i^\top \Psi^{-1} \mathbf{r}_i}.
#' 
#' Through the Matrix Determinant Lemma and the Sherman-Morrison formula, the 
#' internal term \eqn{(1 + \mathbf{r}_i^\top \Psi_{-i}^{-1} \mathbf{r}_i)} simplifies to \eqn{(1 - h_i)^{-1}}. 
#' The final log-density contribution used in the code is:
#' \deqn{\log f_i \propto \textrm{const} - \frac{1}{2}\log|\Psi| + \frac{\nu + T - 1}{2} \log(1 - h_i)}
#' 
#' @return A list containing the optimization results:
#' * `optimal_nu`: The optimal degrees of freedom found.
#' * `min_neg_log_score`: The minimum negative log score achieved.
#' * `convergence`: The convergence status of the optimizer.
#' * `time_elapsed`: The time taken for the optimization.
#'
multi_log_score_optimization <- function(res, prior_mean, trim = 0.1) {
 
  n_obs <- nrow(res)
  n_var <- ncol(res)

  # Pre-compute cross-product of the residuals (to avoid recomputing inside the loop)
  RRt <- crossprod(res) 

  # Define the Objective Function (Negative Log Score to minimize)
  objective_function <- function(nu) {
 
    # Compute posterior Psi
    Psi <- (prior_mean * (nu - n_var - 1)) + RRt

    # Compute Cholesky decomposition 
    Psi_chol <- tryCatch(chol(Psi), error = function(e) return(NULL))
    if (is.null(Psi_chol)) return(Inf) # Return Inf if Psi is not positive definite
  
    # Invert (using Cholesky) 
    inv_Psi <- chol2inv(Psi_chol)
    
    # Calculate log-determinant
    log_det_Psi <- 2 * sum(log(diag(Psi_chol))) 
    
    # Log Gamma terms and determinant
    const_term <- lgamma((nu + n_obs) / 2) - lgamma((nu + n_obs - n_var) / 2) - log_det_Psi / 2
    
    # Compute LOO Terms (using Sherman-Morrison)
    h_i <- rowSums((res %*% inv_Psi) * res)
    if (any(h_i >= 1)) return(Inf)   # If h_i >= 1, the log is undefined
    log_adjustments <- log(1 - h_i) 
    
    # Combine to get vector of LOO Log-Likelihoods per observation
    loo_log_liks <- const_term + ((nu + n_obs - 1) / 2) * log_adjustments
    
    # Calculate how many observations to remove (based on 'trim')
    n_trim <- round(trim * n_obs)
    
    # Sort likelihoods (ascending) and remove the lowest 'n_trim' values
    # Return negative sum because optimizer minimizes 
    return(-sum(sort(loo_log_liks)[(n_trim + 1):n_obs]))
  }
  
  # Optimization Setup
  initial_guess <- n_var + 3
  lower_bound   <- n_var + 2
  upper_bound   <- max(5 * n_var, n_obs)
  
  opts <- list("algorithm" = "NLOPT_LN_BOBYQA", 
               "xtol_rel"  = 1e-5, 
               "maxeval"   = 1000)
  
  # Run Optimization
  start_time <- Sys.time()
  results <- nloptr::nloptr(
    x0     = initial_guess, 
    eval_f = objective_function, 
    lb     = lower_bound, 
    ub     = upper_bound, 
    opts   = opts
  )
  end_time <- Sys.time()
  
  return(list(
    optimal_nu = results$solution,
    min_neg_log_score = results$objective,
    convergence = results$status,
    time_elapsed = end_time - start_time
  ))
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
  n_upper = nrow(A)
  n_bottom = ncol(A)
  n_tot = n_upper + n_bottom
  
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
  
  # S: to check whether this is correct
  # S: wouldn't it be better to use Schafer and Strimmer? Am I missing something?
  covm_res = crossprod(res) / nrow(res) 
  covm_res = (1 - l_shr)*covm_res + l_shr*diag(diag(covm_res))  
  
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

  # Slice the blocks of Psi related to upper and bottoms
  idx_u = seq_len(n_upper)
  idx_b = seq(n_upper + 1, n_tot)
  Psi_u <- Psi_post[idx_u, idx_u, drop = FALSE]
  Psi_b <- Psi_post[idx_b, idx_b, drop = FALSE]
  Psi_ub <- Psi_post[idx_u, idx_b, drop = FALSE] 
  
  # Compute Q = Psi_U - (Psi_UB %*% t(A)) - (A %*% t(Psi_UB)) + (A %*% Psi_B %*% t(A))
  Psi_ub_At <- tcrossprod(Psi_ub, A)      
  A_psi_b   <- A %*% Psi_b                
  Q <- Psi_u - Psi_ub_At - t(Psi_ub_At) + tcrossprod(A_psi_b, A)
  
  # Invert Q using Cholesky (should be p.d.)
  Q_chol <- tryCatch(chol(Q), error = function(e) NULL)
  if (is.null(Q_chol)) {
    # Fallback to standard solve if Cholesky fails (numerical issues)
    warning("Cholesky decomposition of Q failed; using standard inversion.")
    inv_Q <- solve(Q)
  } else {
    inv_Q <- chol2inv(Q_chol)
  }

  # Lambda = Psi_UB^T - Psi_B A^T
  Lambda <- t(Psi_ub) - tcrossprod(Psi_b, A) 
  Lambda_invQ <- Lambda %*% inv_Q

  # For all horizon steps ahead, apply independently
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    #S: is it the case to verify that the likelihood is gaussian?
    # Extract upper and bottom point forecasts
    point_fc = purrr::map_dbl(base_forecasts, mean)
    u_hat = point_fc[seq_len(n_upper)]
    b_hat = point_fc[-seq_len(n_upper)]
    
    # Incoherence
    delta <- (A %*% b_hat) - u_hat

    # Compute b_tilde = b_hat + Lambda * Q^{-1} * delta
    b_tilde <- b_hat + (Lambda_invQ %*% delta)

    # Compute nu_tilde = nu' - n_b + 1
    nu_tilde <- nu_post - n_bottom + 1
  
    # Compute C = (1 + delta^T Q^{-1} delta) / nu_tilde
    mahalanobis_term <- sum(delta * (inv_Q %*% delta)) # efficient x^T A x
    C <- (1 + mahalanobis_term) / nu_tilde
    
    # Compute Sigma_B_tilde = C * [ Psi_B - Lambda * Q^{-1} * Lambda^T ]
    Sigma_tilde_B <- as.numeric(C) * (Psi_b - tcrossprod(Lambda_invQ, Lambda))
    
    # Compute the parameters of the upper using closure property
    u_tilde = A %*% b_tilde
    Sigma_tilde_U = A %*% Sigma_tilde_B %*% t(A)
    
    # Return the distributional Student-t distribution
    return(distributional::dist_student_t(
      df = nu_tilde,
      mu = c(u_tilde, b_tilde),
      # S: to check wheter the following has to be scaled through sqrt
      sigma = sqrt(c(diag(Sigma_tilde_U), diag(Sigma_tilde_B)))
    ))
  })
  
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
