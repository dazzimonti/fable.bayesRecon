test_that("bayesRecon_MixCond runs reconciliation on base forecasts", {
  testthat::skip_if_not_installed("fable")
  testthat::skip_if_not_installed("fabletools")
  testthat::skip_if_not_installed("tsibble")
  testthat::skip_if_not_installed("distributional")

  expect_error({
    fit <- m5_ca1() |>
      fabletools::reconcile(
        mixcond = bayesRecon_MixCond(base)
      )
    fit |> fabletools::forecast(h = 28)
  }, NA)

  expect_error({
    fit <- m5_foods1046() |>
      fabletools::reconcile(
        mixcond = bayesRecon_MixCond(base)
      )
    fit |> fabletools::forecast(h = 28)
  }, NA)

  expect_error({
    fit <- pedestrian_all() |>
      fabletools::reconcile(
        mixcond = bayesRecon_MixCond(base)
      )
    fit |> fabletools::forecast(h = "24 hours")
  }, NA)
})
