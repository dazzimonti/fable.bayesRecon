#' BUIS for Probabilistic Reconciliation of forecasts via conditioning
#'
#' Uses the Bottom-Up Importance Sampling algorithm to draw samples from the reconciled
#' forecast distribution, obtained via conditioning.
#'
#' @param models A list of fitted models to reconcile.
#'
#' @return An object of class `lst_bayesRecon_BUIS`.
#'
#' @export
bayesRecon_BUIS <- function(models) {
  # For this I need an explanation
  structure(models, class = c("lst_bayesRecon_BUIS", "lst_mdl", "list"))
}

#' forecast.lst_bayesRecon_BUIS
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
#' @importFrom bayesRecon .check_hierarchical
#' 
#' @method forecast lst_bayesRecon_BUIS 
#' 
#' 
#' 
#' @param object TODO
#' @param key_data TODO
#' @param point_forecast TODO 
#' @param new_data TODO
#' @param n_samples number of samples for output distribution
#' @param ... extra parameters to be passed on.
#' 
#' @description takes a list of of models and returns a list of reconciled models
#' @export
forecast.lst_bayesRecon_BUIS <- function(
  object,
  key_data,
  point_forecast = list(.mean = mean),
  new_data = NULL,
  n_samples = 1000,
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
  
  ##### START OUR REWRITE OF reconc_t() with distributional
  browser()
  hier <- get_hier(S, fc_dist)
  A <- hier$A
  base_forecast_h <- hier$base_forecast_h
  n_upr = hier$n_upr
  n_btm = hier$n_btm
  
  # For all horizon steps ahead, apply independently
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    # save two lists of upper and bottom fc
    upper_fc <- base_forecasts[seq_len(n_upr)]
    bottom_fc <- base_forecasts[-seq_len(n_btm)]

    # sample from bottom fc
    B <- bottom_fc |> distributional::generate(times = n_samples)
    # make B a matrix
    B <- do.call(cbind, B)

    # TODO: BUIS CURRENTLY IMPLEMENTED ONLY for hierarchies!
    # H, G
    # browser()
    is.hier = bayesRecon:::.check_hierarchical(A)
    # browser()
    if(is.hier){
      H <- A
      G <- NULL
      upp_base_H = upper_fc
      upp_base_G = NULL
      in_typeH = NULL
      distr_H  = NULL
      in_typeG = NULL
      distr_G  = NULL
    }else{
      get_HG.res = bayesRecon:::.get_HG(A, upper_fc, rep(0,n_upper),rep(0,n_upper))
      H = get_HG.res$H
      upp_base_H = get_HG.res$Hv
      G = get_HG.res$G
      upp_base_G = get_HG.res$Gv
      in_typeH = NULL
      distr_H  = NULL
      in_typeG = NULL
      distr_G  = NULL
    }

    .comp_w_distributional <- function(b, u,    
                                       in_type_ = NULL, 
                                       distr_ = NULL){
      return(stats::density(u, as.numeric(b))[[1L]])
    }

    B = bayesRecon::.core_reconc_BUIS(A=A,H=H,G=G,B=B,
                     upper_base_forecasts_H = upp_base_H,
                    in_typeH = in_typeH, distr_H = distr_H,
                    upper_base_forecasts_G = upp_base_G,
                    in_typeG = in_typeG, distr_G = distr_G,
                    .comp_w = .comp_w_distributional, 
                    suppress_warnings = FALSE)

    # for (hi in 1:nrow(H)) {
    #   c = H[hi,]
    #   b_mask = (c != 0)
    #   # Compute weights by evaluating the upper densities
    #   # distributional needs a numeric to apply density to many values
    #   weights = stats::density(upper_fc[hi], as.numeric(B %*% c))[[1L]]
    #   # Make weights a matrix
    #   weights <- matrix(weights, ncol = 1)

    #   # Checks on weights are inherited from bayesRecon
    #   check_weights.res = bayesRecon:::.check_weights(weights)
    #   # TODO:  Ifs are commented, check if they work
    #   # if (check_weights.res$warning & !suppress_warnings) {
    #   #   warning_msg = check_weights.res$warning_msg
    #   #   # add information to the warning message
    #   #   upper_fromA_i = which(lapply(seq_len(nrow(A)), function(i) sum(abs(A[i,] - c))) == 0)
    #   #   for (wmsg in warning_msg) {
    #   #     wmsg = paste(wmsg, paste0("Check the upper forecast at index: ", upper_fromA_i,"."))
    #   #     warning(wmsg)
    #   #   }
    #   # }
    #   # if(check_weights.res$warning & (1 %in% check_weights.res$warning_code)){
    #   #  next
    #   # }
    #   B[, b_mask] = bayesRecon:::.resample(B[, b_mask], weights)
    # }
    B = t(B)
    U = A %*% B
    Y_reconc = rbind(U, B)

    distributional::dist_sample(split(Y_reconc, row(Y_reconc)))
    ###### END REWRITE
  })

  # Old code to extract from the models the mean and sd manually. 
  # If we use distributional this is not needed
  # fc_param <- lapply(fc_dist, distributional::parameters)
  # fc_param <- lapply(fc_param, `names<-`, c("mean", "sd"))
  # fc_param <- lapply(fc_param, dplyr::slice, 1L)

  # fc_family <- lapply(fc_dist, family)

  # res <- reconc_BUIS(
  #   S[rowSums(S) > 1, , drop = FALSE],
  #   c(fc_param[upr_ts], fc_param[btm_ts][btm_idx]),
  #   in_type = "params",
  #   distr = "gaussian"
  # )

  # split(res$reconciled_samples, row(res$reconciled_samples)) |>
  #   lapply(list) |>
  #   lapply(distributional::dist_sample)

  # Fable needs the horizon and models in a different format
  # Invert horizon <-> model. 
  fc_dist <- transpose_vec(fc_dist)

  # BUIS returns the upper/bottom ordering with upper on top and bottom below
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
