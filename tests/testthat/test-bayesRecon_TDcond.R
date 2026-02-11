test_that("bayesRecon_TDcond runs reconciliation on base forecasts", {
  testthat::skip_if_not_installed("fable")
  testthat::skip_if_not_installed("fabletools")
  testthat::skip_if_not_installed("tsibble")
  testthat::skip_if_not_installed("distributional")

  expect_error({
    fit <- m5_ca1() |>
      fabletools::reconcile(
        tdcond = bayesRecon_TDcond(base)
      )
    fit |> fabletools::forecast(h = 28)
  }, NA)

  expect_error({
    fit_m5_foods <- m5_foods1046() |>
      fabletools::reconcile(
        tdcond = bayesRecon_TDcond(base)
      )
    fit |> fabletools::forecast(h = 28)
  }, NA)

})
