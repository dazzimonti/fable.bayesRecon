library(fable)
library(fabletools)
library(tsibble)
library(dplyr)

tiny_m5 <- readRDS(testthat::test_path("data", "tiny_m5.rds"))

# This function filters uperr or bottom series so that a model can be applied to them
hier_filter <- function(data, level = c("upper", "bottom")) {
  level <- match.arg(level)
  key_cols <- tsibble::key_vars(data)
  if (level == "upper") {
    data <- data |> filter(dplyr::if_any(dplyr::all_of(key_cols), fabletools::is_aggregated))
  } else if (level == "bottom") {
    data <- data |> filter(!dplyr::if_any(dplyr::all_of(key_cols), fabletools::is_aggregated))
  } else {
    stop("Invalid level specified. Use 'upper' or 'bottom'.")
  }
  return(data)
}



tourism_melbourne <- function() {
  data <- tsibble::tourism |>
    filter(Region == "Melbourne") |> 
    aggregate_key(Purpose, Trips = sum(Trips))

  fit <- data |> 
    filter(Quarter < yearquarter("2015 Q1")) |> 
    model(base = ETS(Trips ~ trend("A") + season("A")))
  return(fit)
}


tourism_2purposes2states <- function() {
  data <- tsibble::tourism |>
    filter(Purpose %in% c("Business", "Holiday")) |>
    filter(State %in% c("Northern Territory", "Western Australia")) |>
    aggregate_key(Purpose * (State / Region), Trips = sum(Trips))
  
  fit <- data |> 
    filter(Quarter < yearquarter("2015 Q1")) |> 
    model(base = THETA(Trips))
  return(fit)
}


m5_ca1 <- function() {
  data <- tiny_m5 |>
    filter(store_id == "CA_1") |>
    tsibble(index = date, key = c(item_id, dept_id, cat_id)) |>
    aggregate_key(cat_id / dept_id / item_id, value = sum(value))

  fit_upper <- data |> 
    filter(date < as.Date("2016-04-01")) |> 
    hier_filter("upper") |>
    model(base = THETA(value))
  fit_bottom <- data |> 
    filter(date < as.Date("2016-04-01")) |> 
    hier_filter("bottom") |>
    model(base = STATICNB(value))
  fit <- bind_rows(fit_upper, fit_bottom)
  return(fit)
}


m5_foods1046 <- function() {
  data <- tiny_m5 |>
    filter(item_id == "FOODS_1_046") |>
    tsibble(index = date, key = c(store_id, state_id)) |>
    aggregate_key(state_id / store_id, value = sum(value))

  fit_upper <- data |> 
    filter(date < as.Date("2016-04-01")) |> 
    hier_filter("upper") |>
    model(base = ETS(value ~ trend("A")))
  fit_bottom <- data |> 
    filter(date < as.Date("2016-04-01")) |> 
    hier_filter("bottom") |>
    model(base = STATICNB(value))
  fit <- bind_rows(fit_upper, fit_bottom)
  return(fit)
}


m5_stores <- function() {
  data <- tiny_m5 |> 
    tsibble(index = date, key = c(item_id, store_id, state_id)) |>
    aggregate_key(state_id / store_id, value = sum(value))
  
  fit <- data |>
    filter(date < as.Date("2016-04-01")) |> 
    model(base = SNAIVE(value, period = 7))
  return(fit)
}


pedestrian_all <- function() {
  data <- tsibble::pedestrian |>
    filter(Date_Time < as.Date("2015-04-01")) |>
    aggregate_key(Sensor, Count = sum(Count))
  
  fit_upper <- data |>
    hier_filter("upper") |>
    model(base = ETS(Count ~ trend("A") + season("A")))
  fit_bottom <- data |>
    hier_filter("bottom") |>
    model(base = STATICNB(Count))
  fit <- bind_rows(fit_upper, fit_bottom)
  return(fit)
}

carparts_20 <- function(){
  data <- tsibbledata::monash_forecasting_repository(4656021) |> 
    mutate(start_timestamp = yearmonth(start_timestamp)) |>
    filter(series_name %in% paste0("T", 42 + 0:19)) |> 
    filter(start_timestamp < yearmonth("2001 Oct")) |>
    aggregate_key(series_name, value = sum(value))
  fit <- data |> 
    model(base = STATICNB(value))
}


carparts_5 <- function(){
  data <- tsibbledata::monash_forecasting_repository(4656021) |> 
    mutate(start_timestamp = yearmonth(start_timestamp)) |>
    filter(series_name %in% paste0("T", 42 + 0:4)) |> 
    filter(start_timestamp < yearmonth("2001 Oct")) |>
    aggregate_key(series_name, value = sum(value))
  
  fit_upper <- data |>
    hier_filter("upper") |>
    model(base = STATICNB(value))
  fit_bottom <- data |>
    hier_filter("bottom") |>
    model(base = SNAIVE(value, period = 12))
  fit <- bind_rows(fit_upper, fit_bottom)
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
