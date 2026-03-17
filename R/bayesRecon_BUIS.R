#' BUIS for probabilistic reconciliation of forecasts via conditioning
#' 
#' @description
#'
#' Specifies Bottom-Up Importance Sampling (BUIS) reconciliation for use within 
#' `reconcile()`. The method uses the Bottom-Up Importance Sampling algorithm 
#' to draw samples from the reconciled forecast distribution, obtained via conditioning.
#' Reconciliation is performed when`forecast()` is called on the resulting model.
#' Marginal reconciled forecasts follow a sample distribution.
#'
#' @param models A list of fitted models to reconcile.
#' @param n_samples Number of samples to draw from the reconciled distribution.
#'
#' @return An object of class `lst_bayesRecon_BUIS`.
#'
#' @references
#' Zambon, L., Azzimonti, D. & Corani, G. (2024).
#' *Efficient probabilistic reconciliation of forecasts for real-valued and count time series*.
#' Statistics and Computing 34 (1), 21.
#' \doi{10.1007/s11222-023-10343-y}.
#'
#' @seealso
#' [reconc_gaussian()]
#'
#' @export
bayesRecon_BUIS <- function(models, n_samples = 1000) {
  structure(models, class = c("lst_bayesRecon_BUIS", "lst_mdl", "list"),
            n_samples = n_samples)
}

#' forecast.lst_bayesRecon_BUIS
#'
#' Produces probabilistic forecasts reconciled via Bottom-Up Importance Sampling (BUIS).
#' This method samples from bottom-level distributions, computes upper-level paths via
#' aggregation, and reweights samples according to upper-level forecast densities.
#'
#' @importFrom fabletools forecast distribution_var
#' @importFrom distributional generate dist_sample support
#' @importFrom stats density
#' @importFrom bayesRecon .check_hierarchical .core_reconc_BUIS .get_HG
#'
#' @method forecast lst_bayesRecon_BUIS
#'
#' @param object An object of class `lst_bayesRecon_BUIS` containing fitted models.
#' @param key_data A keyed data frame from `fabletools`.
#' @param point_forecast A list of point forecast functions (default: `list(.mean = mean)`).
#' @param new_data Optional new data for forecasting (not currently used).
#' @param ... Additional arguments passed to other methods.
#'
#' @return A fable object with BUIS-reconciled distributions and point forecasts.
#'
#' @export
forecast.lst_bayesRecon_BUIS <- function(
  object,
  key_data,
  point_forecast = list(.mean = mean),
  new_data = NULL,
  ...
) {
  # Take models from fabletools, and prepare for BUIS
  # build_key_data_smat, does this create the aggregation matrix from key_data encoding, created by aggregate_key function.
  S <- get_S(key_data)
  # core_reconc_BUIS <- getFromNamespace(".core_reconc_BUIS", "bayesRecon")
  # get_HG <- getFromNamespace(".get_HG", "bayesRecon")
  
  # applies the next method ("lst_mdl", in class definition above) to extract the fitted models.
  fc <- NextMethod()
  
  # Series of lapply to extract the parameters of the distribution
  fc_dist <- lapply(fc, function(x) x[[distribution_var(x)]])
  
  ##### START OUR REWRITE OF reconc_BUIS() with distributional
  hier <- get_hier(S, fc_dist)
  A <- hier$A
  base_forecast_h <- hier$base_forecast_h
  n_upr <- hier$n_upr
  n_btm <- hier$n_btm
  upr_ts <- hier$upr_ts
  btm_ts <- hier$btm_ts
  btm_idx <- hier$btm_idx
  
  # Check that if bottom are continuous, then all forecasts are
  for (h in seq_along(base_forecast_h)) {
    supp <- base_forecast_h[[h]] |> support()
    if (
      any(supp[-seq_len(n_upr)] |> format() |> names() %in% c("R", "R+")) && 
        !all(supp[seq_len(n_upr)] |> format() |> names() %in% c("R", "R+"))
      ) {
      stop("If bottom forecasts are continuous, upper forecasts must be continuous too.")
    }
  }
  
  # Get a core parameter from the object attributes
  n_samples <- attr(object, "n_samples")
  
  # For all horizon steps ahead, apply independently
  fc_dist <- lapply(base_forecast_h, function(base_forecasts) {
    
    # save two lists of upper and bottom fc
    upper_fc <- base_forecasts[seq_len(n_upr)]
    bottom_fc <- base_forecasts[-seq_len(n_upr)]

    # sample from bottom fc and make it a matrix
    B <- bottom_fc |> generate(times = n_samples) |> do.call(what=cbind)

    is.hier = .check_hierarchical(A)
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
      get_HG.res = .get_HG(A, upper_fc, rep(0, n_upr), rep(0, n_upr))
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
      return(density(u, as.numeric(b))[[1L]])
    }

    out = .core_reconc_BUIS(A = A, H = H, G = G, B = B,
                          upper_base_fc_H = upp_base_H,
                          in_typeH = in_typeH, distr_H = distr_H,
                          upper_base_fc_G = upp_base_G,
                          in_typeG = in_typeG, distr_G = distr_G,
                          .comp_w = .comp_w_distributional,
                          return_upper = TRUE
                          )

    Y_reconc = rbind(out$upper_rec_samples, out$bottom_rec_samples)
    return(dist_sample(split(Y_reconc, row(Y_reconc))))
    ### END REWRITE
  })

  # Fable needs the horizon and models in a different format
  # Invert horizon <-> model. 
  fc_dist <- transpose_vec(fc_dist)

  # BUIS returns the upper/bottom ordering with upper on top and bottom below
  # fable takes in input series in any arbitrary position so we need to invert back
  # Invert <A/B> smat ordering to arbitrary key_data order
  fc_dist <- fc_dist[order(c(upr_ts, btm_ts[btm_idx]))]
  get_output_fc(fc, fc_dist, point_forecast)
}
