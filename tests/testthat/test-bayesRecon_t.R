test_that("bayesRecon_t runs reconciliation on base forecasts", {
  testthat::skip_if_not_installed("fable")
  testthat::skip_if_not_installed("fabletools")
  testthat::skip_if_not_installed("tsibble")
  testthat::skip_if_not_installed("distributional")

  expect_error({
    fit <- tourism_melbourne() |>
      fabletools::reconcile(
        t = bayesRecon_t(base)
      )
    fit %>% fabletools::forecast(h = "2 years")
  }, NA)
  


  expect_error({
    fit <- tourism_2purposes2states() |>
      fabletools::reconcile(
        t = bayesRecon_t(base)
      )
    fit |> fabletools::forecast(h = "2 years")
  }, NA)


  expect_error({
    fit <- m5_stores() |>
      fabletools::reconcile(
        t = bayesRecon_t(base)
      )
    fit |> fabletools::forecast(h = 28)
  }, NA)
})
