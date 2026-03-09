testthat::test_that("bayesRecon_BUIS runs reconciliation and produces valid output", {
  testthat::skip_if_not_installed("fable")
  testthat::skip_if_not_installed("fabletools")
  testthat::skip_if_not_installed("tsibble")
  testthat::skip_if_not_installed("distributional")
  
  # Run the reconciliation and forecasting for each test case
  # Test 1
  testthat::expect_no_error({
    fc1 <- tourism_melbourne() |>
      fabletools::reconcile(buis = fable.bayesRecon::bayesRecon_BUIS(base)) |>
      fabletools::forecast(h = "2 years") |> 
      dplyr::filter(.model == "buis")
    distr1 <- fc1[[fabletools::distribution_var(fc1)]]
  })
  
  # Test 2
  testthat::expect_no_error({
    fc2 <- extr_mkt_events_all() |>
      fabletools::reconcile(buis = fable.bayesRecon::bayesRecon_BUIS(base)) |>
      fabletools::forecast(h = 10) |> 
      dplyr::filter(.model == "buis")
    distr2 <- fc2[[fabletools::distribution_var(fc2)]]
  })
  
  # Test 3
  testthat::expect_no_error({
    fc3 <- m5_foods1046() |>
      fabletools::reconcile(buis = fable.bayesRecon::bayesRecon_BUIS(base)) |>
      fabletools::forecast(h = 28) |> 
      dplyr::filter(.model == "buis")
    distr3 <- fc3[[fabletools::distribution_var(fc3)]]
  })
  
  # Test 4
  testthat::expect_no_error({
    fc4 <- swiss_tourism_all() |>
      fabletools::reconcile(buis = fable.bayesRecon::bayesRecon_BUIS(base)) |>
      fabletools::forecast(h = 12) |> 
      dplyr::filter(.model == "buis")
    distr4 <- fc4[[fabletools::distribution_var(fc4)]]
  })
  
  
  # Check the class of the forecasts
  testthat::expect_s3_class(fc1, c("tbl_df", "tbl"))
  testthat::expect_s3_class(fc2, c("tbl_df", "tbl"))
  testthat::expect_s3_class(fc3, c("tbl_df", "tbl"))
  testthat::expect_s3_class(fc4, c("fbl_ts", "tbl_ts"))
  
  # Check the existence of model and mean columns
  testthat::expect_true(all(c(".model", ".mean") %in% names(fc1)))
  testthat::expect_true(all(c(".model", ".mean") %in% names(fc2)))
  testthat::expect_true(all(c(".model", ".mean") %in% names(fc3)))
  testthat::expect_true(all(c(".model", ".mean") %in% names(fc4)))
  
  # Check the existence of point forecasts (not NA)
  testthat::expect_true(all(fc1$.mean |> is.finite()))
  testthat::expect_true(all(fc2$.mean |> is.finite()))
  testthat::expect_true(all(fc3$.mean |> is.finite()))
  testthat::expect_true(all(fc4$.mean |> is.finite()))
  
  # Check that the distribution is a distributional object
  testthat::expect_true(distr1 |> distributional::is_distribution())
  testthat::expect_true(distr2 |> distributional::is_distribution())
  testthat::expect_true(distr3 |> distributional::is_distribution())
  testthat::expect_true(distr4 |> distributional::is_distribution())
  
  # Check that the distribution is a sample distribution
  testthat::expect_true(all(distr1 |> stats::family() == "sample"))
  testthat::expect_true(all(distr2 |> stats::family() == "sample"))
  testthat::expect_true(all(distr3 |> stats::family() == "sample"))
  testthat::expect_true(all(distr4 |> stats::family() == "sample"))
  
  # Check that the model fails when bottom are continuous but uppers are not
  testthat::expect_error({
    fc5 <- carparts_5() |>
      fabletools::reconcile(buis = fable.bayesRecon::bayesRecon_BUIS(base)) |>
      fabletools::forecast(h = 6)
  })
})
