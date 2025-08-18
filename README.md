
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{ravingBrowser}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{ravingBrowser}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
ravingBrowser::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-08-18 12:03:03 AWST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.2) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ─────────────────────────── ravingBrowser 0.0.0.9000 ────
#> Duration: 12.8s
#> 
#> ❯ checking tests ...
#>   See below...
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> ❯ checking package subdirectories ... NOTE
#>   Problems with news in ‘NEWS.md’:
#>   No news entries found.
#> 
#> ── Test failures ───────────────────────────────────────────────── testthat ────
#> 
#> > # This file is part of the standard setup for testthat.
#> > # It is recommended that you do not modify it.
#> > #
#> > # Where should you do additional test configuration?
#> > # Learn more about the roles of various files in:
#> > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
#> > # * https://testthat.r-lib.org/articles/special-files.html
#> > 
#> > library(testthat)
#> > library(ravingBrowser)
#> > 
#> > test_check("ravingBrowser")
#> Error in `test_dir()`:
#> ! No test files found
#> Backtrace:
#>     ▆
#>  1. └─testthat::test_check("ravingBrowser")
#>  2.   └─testthat::test_dir("testthat", package = package, reporter = reporter, ..., load_package = "installed")
#>  3.     └─rlang::abort("No test files found")
#> Execution halted
#> 
#> 1 error ✖ | 0 warnings ✔ | 2 notes ✖
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
