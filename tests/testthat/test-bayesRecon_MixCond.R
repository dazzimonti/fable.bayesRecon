testthat::test_that("bayesRecon_MixCond runs reconciliation and produces valid output", {
  testthat::skip_if_not_installed("fable")
  testthat::skip_if_not_installed("fabletools")
  testthat::skip_if_not_installed("tsibble")
  testthat::skip_if_not_installed("distributional")

  # Test 1
  testthat::expect_no_error({
    fc1 <- carparts_100() |>
      fabletools::reconcile(mixcond = fable.bayesRecon::bayesRecon_MixCond(base)) |>
      fabletools::forecast(h = 6) |> 
      dplyr::filter(.model == "mixcond")
    distr1 <- fc1[[fabletools::distribution_var(fc1)]]
  })
  
  # Test 2
  testthat::expect_no_error({
    fc2 <- m5_foods1046() |>
      fabletools::reconcile(mixcond = fable.bayesRecon::bayesRecon_MixCond(base)) |>
      fabletools::forecast(h = 28) |> 
      dplyr::filter(.model == "mixcond")
    distr2 <- fc2[[fabletools::distribution_var(fc2)]]
  })
  
  # Test 3
  testthat::expect_no_error({
    fc3 <- pedestrian_all() |>
      fabletools::reconcile(mixcond = fable.bayesRecon::bayesRecon_MixCond(base)) |>
      fabletools::forecast(h = "24 hours") |> 
      dplyr::filter(.model == "mixcond")
    distr3 <- fc3[[fabletools::distribution_var(fc3)]]
  })
  
  # Check the class of the forecasts
  testthat::expect_s3_class(fc1, c("tbl_df", "tbl"))
  testthat::expect_s3_class(fc2, c("tbl_df", "tbl"))
  testthat::expect_s3_class(fc3, c("tbl_df", "tbl"))
  
  # Check the existence of model and mean columns
  testthat::expect_true(all(c(".model", ".mean") %in% names(fc1)))
  testthat::expect_true(all(c(".model", ".mean") %in% names(fc2)))
  testthat::expect_true(all(c(".model", ".mean") %in% names(fc3)))
  
  # Check the existence of point forecasts (not NA)
  testthat::expect_true(all(fc1$.mean |> is.finite()))
  testthat::expect_true(all(fc2$.mean |> is.finite()))
  testthat::expect_true(all(fc3$.mean |> is.finite()))
  
  # Check that the distribution is a distributional object
  testthat::expect_true(distr1 |> distributional::is_distribution())
  testthat::expect_true(distr2 |> distributional::is_distribution())
  testthat::expect_true(distr3 |> distributional::is_distribution())
  
  # Test that the distirbution is a sample distribution
  testthat::expect_true(all(distr1 |> stats::family() == "sample"))
  testthat::expect_true(all(distr2 |> stats::family() == "sample"))
  testthat::expect_true(all(distr3 |> stats::family() == "sample"))
})
