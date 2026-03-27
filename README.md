
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fable.bayesRecon: BAyesian reCONciliation in the fable framework <a href="https://idsia.github.io/bayesRecon/"><img src="man/figures/logo.png" align="right" height="150" alt="bayesRecon website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/IDSIA/fable.bayesRecon/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/IDSIA/fable.bayesRecon/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/fable.bayesRecon)](https://CRAN.R-project.org/package=fable.bayesRecon)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: LGPL (\>=
3)](https://img.shields.io/badge/license-LGPL%20(%3E%3D%203)-yellow.svg)](https://www.gnu.org/licences/lgpl-3.0)
<!-- badges: end -->

The package `fable.bayesRecon` integrates the probabilistic
reconciliation methods from
[`bayesRecon`](https://github.com/IDSIA/bayesRecon) into the
[`fable`](https://fable.tidyverts.org/) /
[`fabletools`](https://fabletools.tidyverts.org/) framework.
Reconciliation is specified via the `reconcile()` verb and produced when
`forecast()` is called, following the same tidy workflow used by
`fable`.

The reconciliation functions are:

- `bayesRecon_t`: reconciliation via conditioning with uncertain
  covariance matrix; the reconciled forecasts are multivariate
  Student-t; this is done analytically.
- `bayesRecon_BUIS`: reconciliation via conditioning of any
  probabilistic forecast via importance sampling; this is the
  recommended option for non-Gaussian base forecasts;
- `bayesRecon_MixCond`: reconciliation via conditioning of mixed
  hierarchies, where the upper forecasts are multivariate Gaussian and
  the bottom forecasts are discrete distributions;
- `bayesRecon_TDcond`: reconciliation via top-down conditioning of mixed
  hierarchies, where the upper forecasts are multivariate Gaussian and
  the bottom forecasts are discrete distributions;

## News

:boom: \[2026-TO-DO\] fable.bayesRecon v0.0.1: first release.

## Installation

You can install the **development** version from
[GitHub](https://github.com/dazzimonti/fable.bayesRecon):

``` r
# install.packages("devtools")
devtools::install_github("dazzimonti/fable.bayesRecon", build_vignettes = TRUE, dependencies = TRUE)
```

## Usage

The package follows the standard `fable` workflow:

1.  Prepare data as a `tsibble` and define the hierarchy with
    `aggregate_key()`.
2.  Fit base forecasting models with `model()`.
3.  Specify the reconciliation strategy inside `reconcile()`.
4.  Produce reconciled probabilistic forecasts with `forecast()`.

We provide in [this vignette](vignettes/fable.bayesRecon.Rmd) a simple
usage example; refer to the package documentation for more details on
the reconciliation methods and their parameters.

## References

Carrara, C., Corani, G., Azzimonti, D., Zambon, L. (2025). *Modeling the
uncertainty on the covariance matrix for probabilistic forecast
reconciliation*. arXiv preprint arXiv:2506.19554. [Available
here](https://arxiv.org/abs/2506.19554)

Zambon, L., Azzimonti, D. & Corani, G. (2024). *Efficient probabilistic
reconciliation of forecasts for real-valued and count time series*.
Statistics and Computing 34 (1), 21.
[DOI](https://doi.org/10.1007/s11222-023-10343-y)

Zambon, L., Azzimonti, D., Rubattu, N., Corani, G. (2024).
*Probabilistic reconciliation of mixed-type hierarchical time series*.
Proceedings of the Fortieth Conference on Uncertainty in Artificial
Intelligence, PMLR 244:4078-4095. [Available
here](https://proceedings.mlr.press/v244/zambon24a.html)

## Contributors

TODO (ordine da stabilire) COPILOT SUGGERISCE: Luca Zambon, Daniele
Azzimonti, Giorgio Corani, Nicola Rubattu, Carlo Carrara.

## Getting help

If you encounter a bug, please file a minimal reproducible example on
[GitHub](https://github.com/dazzimonti/fable.bayesRecon/issues).
