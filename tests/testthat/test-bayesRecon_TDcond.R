test_that("bayesRecon_TDcond runs reconciliation and produces valid output", {
  testthat::skip_if_not_installed("fable")
  testthat::skip_if_not_installed("fabletools")
  testthat::skip_if_not_installed("tsibble")
  testthat::skip_if_not_installed("distributional")

  # Test 1
  expect_error({
    fc1 <- carparts_all() |>
      fabletools::reconcile(tdcond = bayesRecon_TDcond(base)) |>
      fabletools::forecast(h = 6) |> 
      dplyr::filter(.model == "tdcond")
    distr1 <- fc1[[fabletools::distribution_var(fc1)]]
  }, NA)
  
  # Test 2
  expect_error({
    fc2 <- m5_foods1046() |>
      fabletools::reconcile(tdcond = bayesRecon_TDcond(base)) |>
      fabletools::forecast(h = 28) |> 
      dplyr::filter(.model == "tdcond")
    distr2 <- fc2[[fabletools::distribution_var(fc2)]]
  }, NA)
  
  # Test 3 
  expect_error({
    fc3 <- m5_ca1() |>
      fabletools::reconcile(tdcond = bayesRecon_TDcond(base)) |>
      fabletools::forecast(h = 28) |> 
      dplyr::filter(.model == "tdcond")
    distr3 <- fc3[[fabletools::distribution_var(fc3)]]
  }, NA)
  
  
  # Check the class of the forecasts
  expect_s3_class(fc1, c("tbl_df", "tbl"))
  expect_s3_class(fc2, c("tbl_df", "tbl"))
  expect_s3_class(fc3, c("tbl_df", "tbl"))
  
  # Check the existence of model and mean columns
  expect_true(all(c(".model", ".mean") %in% names(fc1)))
  expect_true(all(c(".model", ".mean") %in% names(fc2)))
  expect_true(all(c(".model", ".mean") %in% names(fc3)))
  
  # Check the existence of point forecasts (not NA)
  expect_true(all(fc1$.mean |> is.finite()))
  expect_true(all(fc2$.mean |> is.finite()))
  expect_true(all(fc3$.mean |> is.finite()))
  
  # Check that the distribution is a distributional object
  expect_true(distr1 |> distributional::is_distribution())
  expect_true(distr2 |> distributional::is_distribution())
  expect_true(distr3 |> distributional::is_distribution())
  
  #TODO: check that the bottom distributions are same as the upper?
})
