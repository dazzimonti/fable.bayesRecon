library(fabletools)
library(tsibble)

STATICNB <- function(formula, ...) {
  staticnb_model <- new_model_class(
    "staticnb",
    train = train_staticnb,
    specials = new_specials(),
    check = all_tsbl_checks
  )
  new_model_definition(staticnb_model, !!enquo(formula), ...)
}

train_staticnb <- function(.data, specials, ...) {
  if (length(measured_vars(.data)) != 1) {
    cli::cli_abort("Only univariate responses are supported by STATICNB.")
  }
  y <- unclass(.data)[[measured_vars(.data)]]
  mu <- mean(y, na.rm = TRUE)
  mu <- validate_mu(mu, context = "train")

  structure(
    list(
      par = list(
        term = c("mean", "size", "prob"),
        estimate = c(mu, mu, 0.5)
      ),
      .mean = mu,
      .size = mu,
      .prob = 0.5,
      .fitted = rep(mu, length(y)),
      .residuals = y - mu
    ),
    class = "staticnb"
  )
}

#' @inherit forecast.ARIMA
forecast.staticnb <- function(object, new_data, ...) {
  h <- nrow(new_data)
  mu <- validate_mu(object$.mean, context = "forecast")
  prob <- object$.prob
  distributional::dist_negative_binomial(size = rep(mu, h), prob = rep(prob, h))
}

#' @inherit fitted.ARIMA
fitted.staticnb <- function(object, ...) {
    object[[".fitted"]]
}

#' @inherit tidy.ARIMA
residuals.staticnb <- function(object, ...) {
    object[[".residuals"]]
}

validate_mu <- function(mu, context) {
  if (length(mu) != 1L) {
    rlang::abort(paste0("STATICNB: ", context, " mean is not a scalar."))
  }
  if (is.na(mu) || !is.finite(mu)) {
    rlang::abort(paste0("STATICNB: ", context, " mean is NA/NaN/Inf. Check response values."))
  }
  if (mu < 0) {
    rlang::abort(paste0("STATICNB: ", context, " mean is negative. Check response values."))
  }
  mu
}



library(cli)
check_gaps <- function(x) {
  if (any(tsibble::has_gaps(x)[[".gaps"]])) {
    cli::cli_abort(sprintf("%s contains implicit gaps in time. You should check your data and convert implicit gaps into explicit missing values using `tsibble::fill_gaps()` if required.", deparse(substitute(x))))
  }
}

check_regular <- function(x) {
  if (!is_regular(x)) {
    cli::cli_abort(sprintf("%s is an irregular time series, which this model does not support. You should consider if your data can be made regular, and use `tsibble::update_tsibble(%s, regular = TRUE)` if appropriate.", deparse(substitute(x)), deparse(substitute(x))))
  }
}

check_ordered <- function(x) {
  if (!is_ordered(x)) {
    cli::cli_abort(sprintf(
      "%s is an unordered time series. To use this model, you first must sort the data in time order using `dplyr::arrange(%s, %s)`",
      deparse(substitute(x)), paste(c(deparse(substitute(x)), key_vars(x)), collapse = ", "), index_var(x)
    ))
  }
}
all_tsbl_checks <- function(.data) {
  check_gaps(.data)
  check_regular(.data)
  check_ordered(.data)
  if (NROW(.data) == 0) {
    cli::cli_abort("There is no data to model. Please provide a dataset with at least one observation.")
  }
}

# Register S3 methods for STATICNB
registerS3method("forecast", "staticnb", forecast.staticnb)
registerS3method("fitted", "staticnb", fitted.staticnb)
registerS3method("residuals", "staticnb", residuals.staticnb)
