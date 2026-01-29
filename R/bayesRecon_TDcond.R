transpose_vec <- function(.l) {
  result <- lapply(seq_along(.l[[1]]), function(i) {
    do.call(vctrs::vec_c, lapply(.l, vctrs::vec_slice, i))
  })
}

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
#' @method forecast lst_bayesRecon_TDcond 
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
forecast.lst_bayesRecon_TDcond <- function(
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
  fc <- NextMethod()
  
  # Series of lapply to extract the parameters of the distribution
  fc_dist <- lapply(fc, function(x) x[[fabletools::distribution_var(x)]])
  
  ##### START OUR REWRITE OF reconc_TDcond with distributional
  upr_ts <- which(rowSums(S) > 1)
  btm_ts <- which(rowSums(S) == 1)
  A <- S[rowSums(S) > 1, , drop = FALSE]
  # The next two lines do indexing magic to return base_forecasts in the proper order for bayesRecon
  btm_idx <- apply(S[btm_ts, , drop = FALSE], 1, \(x) which(as.logical(x)))
  base_forecast_h <- transpose_vec(fc_dist[c(upr_ts, btm_ts[btm_idx])])
  
  #S: this must be imported in a better way!
  source("R/utils.R")
  
  # Save dimensions
  n_upper = nrow(A)
  n_bottom = ncol(A)
  lowest_rows <- .lowest_lev(A)
  n_upper_l <- length(lowest_rows)
  n_upper_u <- n_upper - n_upper_l
  
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    # save two lists of upper and bottom fc
    browser()
    upper_fc <- base_forecasts[seq_len(n_upper)]
    bottom_fc <- base_forecasts[-seq_len(n_upper)]
    n_tot <- length(base_forecasts)
  
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
    
    # Estimate the covariance matrix
    if (n_upper == 1){
      upper_covm <- matrix(crossprod(res[,1])/nrow(res))
    } else {
      upper_covm <- bayesRecon::schaferStrimmer_cov(res[,1:n_upper])$shrink_cov
    }
  
    # Instanciate the MVN for uppers
    mult_upper_fc <- distributional::dist_multivariate_normal(mu = list(sapply(upper_fc,mean)),
                                                              sigma = list(upper_covm))
    
    if (n_upper == n_upper_l){
      
      browser()
      U <- mult_upper_fc |> distributional::generate(times = 7) |> 
        (\(x) round(x[[1]]))() |> asplit(MARGIN = 2)
    } else {
      # TO BE CHECKED STILL
      A_u = .get_Au(A, lowest_rows)
      
      # Analytically reconcile the upper
      # The entries of mu_u must be in the correct order, i.e. rows of A_u (upper), columns of A_u (bottom)
      mu_u_ord = c(mu_u[-lowest_rows],mu_u[lowest_rows])  
      # Same for Sigma_u
      Sigma_u_ord = matrix(nrow=n_u, ncol=n_u)
      Sigma_u_ord[1:n_u_upp, 1:n_u_upp]             = Sigma_u[-lowest_rows,-lowest_rows]
      Sigma_u_ord[1:n_u_upp, (n_u_upp+1):n_u]       = Sigma_u[-lowest_rows,lowest_rows]
      Sigma_u_ord[(n_u_upp+1):n_u, 1:n_u_upp]       = Sigma_u[lowest_rows,-lowest_rows]
      Sigma_u_ord[(n_u_upp+1):n_u, (n_u_upp+1):n_u] = Sigma_u[lowest_rows,lowest_rows]
      rec_gauss_u = reconc_gaussian(A_u, mu_u_ord, Sigma_u_ord)
      
      # Sample from reconciled MVN on the lowest level of the upper (dim: num_samples x n_u_low)
      U = .MVN_sample(n_samples = num_samples,
                      mu    = rec_gauss_u$bottom_reconciled_mean, 
                      Sigma = rec_gauss_u$bottom_reconciled_covariance)  
      U = round(U)                # round
      mode(U) <- "integer"        # convert to integer
      U_js = asplit(U, MARGIN = 2) 
    }
    
    #Then extrapolate the distribution of the bottoms (pmf)
    browser()
    B = matrix(nrow = n_b, ncol = n)) # S: to be replaced with num_samples_ok.
    for (j in 1:n_upper_l){
      B[as.logical(A[lowest_rows[j],])] = .TD_sampling(U_js[[j]], bottom_fc)
    }
  
    L_pmf_js = list()   
    for (j in lowest_rows) {
      L_pmf_js = c(L_pmf_js, list(bottom_fc[[as.logical(A[j,])]]))
    }
    
    #S: pmf is a vector such that pmf[i] = P(X = i-1) (integer)
    
    #S: does it really make sense to sooth in distributional?
    # S: in case, we have to specify the new distribution object
    PMF.smoothing = function(pmf, alpha = .ALPHA_SMOOTHING, laplace=FALSE) {
      if (is.null(alpha)) alpha = min(pmf[pmf!=0])
      if (laplace) { 
        pmf = pmf + rep(alpha, length(pmf))
      } else pmf[pmf==0] = alpha
      return(pmf / sum(pmf))
    }
    
    
    
      
      pfm_bottom_up = function(lower_distr, return_all = FALSE){
        
        if(length(lower_distr)==1){
          if (return_all) {
            return(list(lower_distr))
          } else {
            return(lower_distr[[1]])
          }
        }
        
        old_v = l_pmf
        l_l_v = list(old_v)   # list with all the step-by-step lists of pmf
        L = length(old_v)
        while (L > 1) {
          new_v = c()
          for (j in 1:(L%/%2)) {
            new_v = c(new_v, list(PMF.conv(old_v[[2*j-1]], old_v[[2*j]], 
                                           toll=toll, Rtoll=Rtoll)))
          }
          if (L%%2 == 1) new_v = c(new_v, list(old_v[[L]]))
          old_v = new_v
          l_l_v = c(l_l_v, list(old_v))
          L = length(old_v)
        }
      
        if (return_all) {
          return(l_l_v)
        } else {
          return(new_v[[1]])
        }
      }
      
        
      }
      
      PMF.bottom_up = function(l_pmf, toll=.TOLL, Rtoll=.RTOLL, return_all=FALSE,
                               smoothing=TRUE, al_smooth=.ALPHA_SMOOTHING, lap_smooth=.LAP_SMOOTHING) {
        
        # Smoothing to "cover the holes" in the supports of the bottom pmfs
        # S: this point can probsbly be ignored (see before)
        if (smoothing) l_pmf = lapply(l_pmf, PMF.smoothing, 
                                      alpha=al_smooth, laplace=lap_smooth)
        
        # In case we have an upper which is a duplicate of a bottom,
        # the bottom up is simply that bottom.
        if(length(l_pmf)==1){
          if (return_all) {
            return(list(l_pmf))
          } else {
            return(l_pmf[[1]])
          }
        }
        
        # Doesn't do convolutions sequentially 
        # Instead, for each iteration (while) it creates a new list of vectors 
        # by doing convolution between 1 and 2, 3 and 4, ...
        # Then, the new list has length halved (if L is odd, just copy the last element)
        # Ends when the list has length 1: contains just 1 vector that is the convolution 
        # of all the vectors of the list 
        old_v = l_pmf
        l_l_v = list(old_v)   # list with all the step-by-step lists of pmf
        L = length(old_v)
        while (L > 1) {
          new_v = c()
          for (j in 1:(L%/%2)) {
            new_v = c(new_v, list(PMF.conv(old_v[[2*j-1]], old_v[[2*j]], 
                                           toll=toll, Rtoll=Rtoll)))
          }
          if (L%%2 == 1) new_v = c(new_v, list(old_v[[L]]))
          old_v = new_v
          l_l_v = c(l_l_v, list(old_v))
          L = length(old_v)
        }
        
        if (return_all) {
          return(l_l_v)
        } else {
          return(new_v[[1]])
        }
      }
    
    
    
    # Given a vector u of the upper values and a list of the bottom distr pmfs,
    # returns samples (dim: n_bottom x length(u)) from the conditional distr 
    # of the bottom given the upper values
    .TD_sampling = function(u, bott_pmf, 
                            toll=.TOLL, Rtoll=.RTOLL, smoothing=TRUE, 
                            al_smooth=.ALPHA_SMOOTHING, lap_smooth=.LAP_SMOOTHING) {
      
      # If the bottom pmf list contains only 1 element,
      # then the TD samples are simply a copy of the upper samples. 
      # S: questo qui puo essere eliminato, giusto?
      # if(length(bott_pmf)==1){
         # return(matrix(u, nrow=1))
      # }
      
      l_l_pmf = rev(PMF.bottom_up(bott_pmf, toll = toll, Rtoll = Rtoll, return_all = TRUE, 
                                  smoothing=smoothing, al_smooth=al_smooth, lap_smooth=lap_smooth))
      
      b_old = matrix(u, nrow = 1)
      for (l_pmf in l_l_pmf[2:length(l_l_pmf)]) {
        L = length(l_pmf)
        b_new = matrix(ncol = length(u), nrow = L)
        for (j in 1:(L%/%2)) {
          b = .cond_biv_sampling(b_old[j,], l_pmf[[2*j-1]], l_pmf[[2*j]])
          b_new[2*j-1,] = b[[1]]
          b_new[2*j,]   = b[[2]]
        }
        if (L%%2 == 1) b_new[L,] = b_old[L%/%2 + 1,]
        b_old = b_new
      }
      
      return(b_new)
    }
    
  })
  
  
  # END REWRITE
  
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



# Given a vector u of the upper values and a list of the bottom distr pmfs,
# returns samples (dim: n_bottom x length(u)) from the conditional distr 
# of the bottom given the upper values
.TD_sampling = function(u, bott_pmf, 
                        toll=.TOLL, Rtoll=.RTOLL, smoothing=TRUE, 
                        al_smooth=.ALPHA_SMOOTHING, lap_smooth=.LAP_SMOOTHING) {
  
  # If the bottom pmf list contains only 1 element,
  # then the TD samples are simply a copy of the upper samples. 
  if(length(bott_pmf)==1){
    return(matrix(u, nrow=1))
  }
  
  l_l_pmf = rev(PMF.bottom_up(bott_pmf, toll = toll, Rtoll = Rtoll, return_all = TRUE, 
                              smoothing=smoothing, al_smooth=al_smooth, lap_smooth=lap_smooth))
  
  b_old = matrix(u, nrow = 1)
  for (l_pmf in l_l_pmf[2:length(l_l_pmf)]) {
    L = length(l_pmf)
    b_new = matrix(ncol = length(u), nrow = L)
    for (j in 1:(L%/%2)) {
      b = .cond_biv_sampling(b_old[j,], l_pmf[[2*j-1]], l_pmf[[2*j]])
      b_new[2*j-1,] = b[[1]]
      b_new[2*j,]   = b[[2]]
    }
    if (L%%2 == 1) b_new[L,] = b_old[L%/%2 + 1,]
    b_old = b_new
  }
  
  return(b_new)
}
