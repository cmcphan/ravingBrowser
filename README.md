
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{ravingBrowser}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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

You are reading the doc about version : 0.0.3.3

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-09-30 16:34:07 AWST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading ravingBrowser
#> ── R CMD check results ─────────────────────────────────────────────── ravingBrowser 0.0.3.3 ────
#> Duration: 11.9s
#> 
#> ❯ checking package dependencies ... ERROR
#>   Namespace dependencies missing from DESCRIPTION Imports/Depends entries:
#>     'cowplot', 'dplyr', 'gggenes', 'ggplot2', 'ggraph',
#>     'shinycssloaders', 'tidygraph', 'usethis'
#>   
#>   See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
#>   manual.
#> 
#> 1 error ✖ | 0 warnings ✔ | 0 notes ✔
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
