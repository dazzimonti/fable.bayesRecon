testthat::test_that("bayesRecon_TDcond runs reconciliation and produces valid output", {
  testthat::skip_if_not_installed("fable")
  testthat::skip_if_not_installed("fabletools")
  testthat::skip_if_not_installed("tsibble")
  testthat::skip_if_not_installed("distributional")

  # Test 1
  testthat::expect_no_error({
    fc1 <- carparts_100() |>
      fabletools::reconcile(tdcond = fable.bayesRecon::bayesRecon_TDcond(base, 
                                                                         n_samples = 500)) |>
      fabletools::forecast(h = 6) |> 
      dplyr::filter(.model == "tdcond")
    distr1 <- fc1[[fabletools::distribution_var(fc1)]]
  })
  
  # Test 2
  testthat::expect_no_error({
    fc2 <- m5_foods1046() |>
      fabletools::reconcile(tdcond = fable.bayesRecon::bayesRecon_TDcond(base, 
                                                                         n_samples = 2000)) |>
      fabletools::forecast(h = 28) |> 
      dplyr::filter(.model == "tdcond")
    distr2 <- fc2[[fabletools::distribution_var(fc2)]]
  })
  
  # Test 3 
  testthat::expect_no_error({
    fc3 <- m5_ca1() |>
      fabletools::reconcile(tdcond = fable.bayesRecon::bayesRecon_TDcond(base)) |>
      fabletools::forecast(h = 28) |> 
      dplyr::filter(.model == "tdcond")
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
})


testthat::test_that("make_PMF creates valid PMFs and produces appropriate warnings", {
  
  # Check that the function make_PMF runs without errors
  testthat::expect_no_error({
    pmf1 <- fable.bayesRecon:::make_PMF(distributions("nonnegative_integer"))
  })
  
  # Check that warnings are produced when bottom forecasts are not integer-valued
  testthat::expect_warning(pmf2 <- fable.bayesRecon:::make_PMF(distributions("nonnegative_continuous")))
  testthat::expect_warning(pmf3 <- fable.bayesRecon:::make_PMF(distributions("real_valued"), negative_to_zero = FALSE))
  testthat::expect_warning(pmf4 <- fable.bayesRecon:::make_PMF(distributions("real_valued"), negative_to_zero = TRUE))
  
  # Check that the pfms are a list of numeric vectors
  testthat::expect_true(is.list(pmf1) && all(sapply(pmf1, is.numeric)))
  testthat::expect_true(is.list(pmf2) && all(sapply(pmf2, is.numeric)))
  testthat::expect_true(is.list(pmf3) && all(sapply(pmf3, is.numeric)))
  testthat::expect_true(is.list(pmf4) && all(sapply(pmf4, is.numeric)))
})
