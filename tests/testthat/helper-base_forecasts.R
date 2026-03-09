tiny_m5 <- readRDS(testthat::test_path("data", "tiny_m5.rds"))

# This function filters uperr or bottom series so that a model can be applied to them
hier_filter <- function(data, level = c("upper", "bottom")) {
  level <- match.arg(level)
  key_cols <- tsibble::key_vars(data)
  if (level == "upper") {
    data <- data |> dplyr::filter(dplyr::if_any(dplyr::all_of(key_cols), fabletools::is_aggregated))
  } else if (level == "bottom") {
    data <- data |> dplyr::filter(!dplyr::if_any(dplyr::all_of(key_cols), fabletools::is_aggregated))
  } else {
    stop("Invalid level specified. Use 'upper' or 'bottom'.")
  }
  return(data)
}


tourism_melbourne <- function() {
  data <- tsibble::tourism |>
    dplyr::filter(Region == "Melbourne") |> 
    fabletools::aggregate_key(Purpose, Trips = sum(Trips))

  fit <- data |> 
    dplyr::filter(Quarter < tsibble::yearquarter("2015 Q1")) |> 
    fabletools::model(base = fable::ETS(Trips ~ trend("A") + season("A")))
  return(fit)
}


tourism_2purposes2states <- function() {
  data <- tsibble::tourism |>
    dplyr::filter(Purpose %in% c("Business", "Holiday")) |>
    dplyr::filter(State %in% c("Northern Territory", "Western Australia")) |>
    fabletools::aggregate_key(Purpose * (State / Region), Trips = sum(Trips))
  
  fit <- data |> 
    dplyr::filter(Quarter < tsibble::yearquarter("2015 Q1")) |> 
    fabletools::model(base = fable::THETA(Trips))
  return(fit)
}


m5_ca1 <- function() {
  data <- tiny_m5 |>
    dplyr::filter(store_id == "CA_1") |>
    tsibble::tsibble(index = date, key = c(item_id, dept_id, cat_id)) |>
    fabletools::aggregate_key(cat_id / dept_id / item_id, value = sum(value))

  fit_upper <- data |> 
    dplyr::filter(date < as.Date("2016-04-01")) |> 
    hier_filter("upper") |>
    fabletools::model(base = fable::THETA(value))
  fit_bottom <- data |> 
    dplyr::filter(date < as.Date("2016-04-01")) |> 
    hier_filter("bottom") |>
    fabletools::model(base = STATICNB(value))
  fit <- dplyr::bind_rows(fit_upper, fit_bottom)
  return(fit)
}


m5_foods1046 <- function() {
  data <- tiny_m5 |>
    dplyr::filter(item_id == "FOODS_1_046") |>
    tsibble::tsibble(index = date, key = c(store_id, state_id)) |>
    fabletools::aggregate_key(state_id / store_id, value = sum(value))

  fit_upper <- data |> 
    dplyr::filter(date < as.Date("2016-04-01")) |> 
    hier_filter("upper") |>
    fabletools::model(base = fable::ETS(value ~ trend("A")))
  fit_bottom <- data |> 
    dplyr::filter(date < as.Date("2016-04-01")) |> 
    hier_filter("bottom") |>
    fabletools::model(base = STATICNB(value))
  fit <- dplyr::bind_rows(fit_upper, fit_bottom)
  return(fit)
}


m5_stores <- function() {
  data <- tiny_m5 |> 
    tsibble::tsibble(index = date, key = c(item_id, store_id, state_id)) |>
    fabletools::aggregate_key(state_id / store_id, value = sum(value))
  
  fit <- data |>
    dplyr::filter(date < as.Date("2016-04-01")) |> 
    fabletools::model(base = fable::SNAIVE(value, period = 7))
  return(fit)
}


pedestrian_all <- function() {
  data <- tsibble::pedestrian |>
    dplyr::filter(Date_Time < as.Date("2015-04-01")) |>
    fabletools::aggregate_key(Sensor, Count = sum(Count))
  
  fit_upper <- data |>
    hier_filter("upper") |>
    fabletools::model(base = fable::ETS(Count ~ trend("A") + season("A")))
  fit_bottom <- data |>
    hier_filter("bottom") |>
    fabletools::model(base = STATICNB(Count))
  fit <- dplyr::bind_rows(fit_upper, fit_bottom)
  return(fit)
}

carparts_100 <- function(){
  data <- tsibbledata::monash_forecasting_repository(4656021) |> 
    dplyr::mutate(start_timestamp = tsibble::yearmonth(start_timestamp)) |>
    dplyr::filter(series_name %in% paste0("T", 42 + 0:99)) |> 
    dplyr::filter(start_timestamp < tsibble::yearmonth("2001 Oct")) |>
    fabletools::aggregate_key(series_name, value = sum(value))
  
  fit_upper <- data |>
    hier_filter("upper") |>
    fabletools::model(base = fable::SNAIVE(value, period = 12))
  fit_bottom <- data |>
    hier_filter("bottom") |>
    fabletools::model(base = STATICNB(value))
  fit <- dplyr::bind_rows(fit_upper, fit_bottom)
}


carparts_5 <- function(){
  data <- tsibbledata::monash_forecasting_repository(4656021) |> 
    dplyr::mutate(start_timestamp = tsibble::yearmonth(start_timestamp)) |>
    dplyr::filter(series_name %in% paste0("T", 42 + 0:4)) |> 
    dplyr::filter(start_timestamp < tsibble::yearmonth("2001 Oct")) |>
    fabletools::aggregate_key(series_name, value = sum(value))
  
  fit_upper <- data |>
    hier_filter("upper") |>
    fabletools::model(base = STATICNB(value))
  fit_bottom <- data |>
    hier_filter("bottom") |>
    fabletools::model(base = fable::SNAIVE(value, period = 12))
  fit <- dplyr::bind_rows(fit_upper, fit_bottom)
  return(fit)
}


swiss_tourism_all <- function(){
  data <- bayesRecon::swiss_tourism$ts[, -1, drop = FALSE] |>
    tsibble::as_tsibble() |>
    dplyr::rename(Month = index, Canton = key, Tourists = value) |> 
    fabletools::aggregate_key(Canton, Tourists = sum(Tourists))
  fit <- data |>
    dplyr::filter(Month < tsibble::yearmonth("2024 Jan")) |> 
    fabletools::model(base = fable::ETS(Tourists ~ trend("A") + season("A")))
  return(fit)
}

extr_mkt_events_all <- function(){
  data <- bayesRecon::extr_mkt_events[, -1, drop = FALSE] |>
    tsibble::as_tsibble() |>
    dplyr::rename(MarketDay = index, Sector = key, Events = value) |> 
    fabletools::aggregate_key(Sector, Events = sum(Events))
  fit <- data |>
    dplyr::filter(MarketDay < 3499) |> 
    fabletools::model(base = STATICNB(Events))
  return(fit)
}


distributions <- function(type = c("nonnegative_integer", "nonnegative_continuous", "real_valued")){
  type <- match.arg(type)
  distr <- distributional::dist_poisson(2.)
  if (type == "nonnegative_integer") {
    distr <- c(distr, distributional::dist_negative_binomial(3., 0.6))
  } else if (type == "nonnegative_continuous") {
    distr <- c(distr, distributional::dist_gamma(2., 1.))
  } else if (type == "real_valued") {
    distr <- c(distr, distributional::dist_normal(0., 1.))
  } else {
    stop("Invalid type specified.")
  }
  return(distr)
}
