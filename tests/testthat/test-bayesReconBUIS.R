test_that("This is a copy of the code used to debug, needs to become a test", {
  
  # YOU NEED TO TEST THIS ON A HIERARCHY WITH MORE THAN ONE UPPER
  tourism_melb <- tsibble::tourism |>
    dplyr::filter(Region == "Melbourne") |> 
    fabletools::aggregate_key(Purpose, Trips = sum(Trips))


  # tourism_melb |> autoplot(Trips)

  fit <- tourism_melb %>%
    dplyr::filter(Quarter < tsibble::yearquarter("2015 Q1")) |> 
    fabletools::model(
      ets = fable::ETS(Trips ~ trend("A"))
    ) %>%
    fabletools::reconcile(
      buis = bayesRecon_BUIS(ets)
    )


  # fit |> 
  #   forecast(h = "3 years") |>
  #   autoplot(tourism_melb)

  fit |>
    fabletools::forecast(h = "3 years") |>
    fabletools::accuracy(tourism_melb)

})
