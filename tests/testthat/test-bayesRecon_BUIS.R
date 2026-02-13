test_that("bayesRecon_BUIS runs reconciliation and produces valid output", {
  testthat::skip_if_not_installed("fable")
  testthat::skip_if_not_installed("fabletools")
  testthat::skip_if_not_installed("tsibble")
  testthat::skip_if_not_installed("distributional")
  
  # Run the reconciliation and forecasting for each test case
  # Test 1
  expect_error({
    fc1 <- tourism_melbourne() |>
      fabletools::reconcile(buis = bayesRecon_BUIS(base)) |>
      fabletools::forecast(h = "2 years") |> 
      dplyr::filter(.model == "buis")
    distr1 <- fc1[[fabletools::distribution_var(fc1)]]
  }, NA)
  
  # Test 2
  expect_error({
    fc2 <- tourism_2purposes2states() |>
      fabletools::reconcile(buis = bayesRecon_BUIS(base)) |>
      fabletools::forecast(h = "2 years") |> 
      dplyr::filter(.model == "buis")
    distr2 <- fc2[[fabletools::distribution_var(fc2)]]
  }, NA)
  
  # Test 3
  expect_error({
    fc3 <- m5_stores() |>
      fabletools::reconcile(buis = bayesRecon_BUIS(base)) |>
      fabletools::forecast(h = 28) |> 
      dplyr::filter(.model == "buis")
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
  
  # Check that the distribution is a sample distribution
  expect_true(all(distr1 |> family() == "sample"))
  expect_true(all(distr2 |> family() == "sample"))
  expect_true(all(distr3 |> family() == "sample"))
})
