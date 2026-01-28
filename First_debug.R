rm(list = ls())
library(bayesRecon)
library(tsibble)
library(fable)
library(dplyr)
devtools::load_all()
source("R/bayesRecon_BUIS.R")
source("R/bayesRecon_MixCond.R")

debug(bayesRecon_MixCond)

tourism_melb <- tourism |>
  filter(Region == "Melbourne")


tourism_melb <- tourism_melb |> aggregate_key(Purpose, Trips = sum(Trips))


tourism_melb |> autoplot(Trips)

fit <- tourism_melb %>%
  filter(Quarter < yearquarter("2015 Q1")) |> 
  model(
    ets = ETS(Trips ~ trend("A"))
  ) %>%
  reconcile(
    buis = bayesRecon_MixCond(ets)
  )


fit %>%
  forecast(h = "3 years") |>
  autoplot(tourism_melb)


fit %>%
  forecast(h = "3 years") |>
  accuracy(tourism_melb)

# fabletools:::reconcile_fbl_list
