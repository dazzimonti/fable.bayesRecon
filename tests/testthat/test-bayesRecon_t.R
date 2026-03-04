testthat::test_that("bayesRecon_t runs reconciliation and produces valid output", {

  # Test 1
  testthat::expect_no_error({
    fc1 <- tourism_melbourne() |>
      fabletools::reconcile(t = fable.bayesRecon::bayesRecon_t(base)) |>
      fabletools::forecast(h = "2 years") |> 
      dplyr::filter(.model == "t")
    distr1 <- fc1[[fabletools::distribution_var(fc1)]]
  })
  
  # Test 2
  testthat::expect_no_error({
    fc2 <- tourism_2purposes2states() |>
      fabletools::reconcile(t = fable.bayesRecon::bayesRecon_t(base)) |>
      fabletools::forecast(h = "2 years") |> 
      dplyr::filter(.model == "t")
    distr2 <- fc2[[fabletools::distribution_var(fc2)]]
  })
  
  # Test 3
  testthat::expect_no_error({
    fc3 <- m5_stores() |>
      fabletools::reconcile(t = fable.bayesRecon::bayesRecon_t(base)) |>
      fabletools::forecast(h = 28) |> 
      dplyr::filter(.model == "t")
    distr3 <- fc3[[fabletools::distribution_var(fc3)]]
  })
  
  
  # Check the class of the forecasts
  testthat::expect_s3_class(fc1, c("fbl_ts", "tbl_ts"))
  testthat::expect_s3_class(fc2, c("fbl_ts", "tbl_ts"))
  testthat::expect_s3_class(fc3, c("fbl_ts", "tbl_ts"))
  
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
  
  # Check that the distribution is a student_t distribution
  testthat::expect_true(all(distr1 |> stats::family() == "student_t"))
  testthat::expect_true(all(distr2 |> stats::family() == "student_t"))
  testthat::expect_true(all(distr3 |> stats::family() == "student_t"))
  
  # Check that the model fails when base forecasts are not Gaussian
  testthat::expect_error({
    fc4 <- carparts_5() |>
      fabletools::reconcile(t = fable.bayesRecon::bayesRecon_t(base)) |>
      fabletools::forecast(h = 12)
  })
})



