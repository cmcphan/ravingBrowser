
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

You are reading the doc about version : 0.0.3.4

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-10-01 11:04:03 AWST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> 
#> ℹ Loading ravingBrowser
#> Loading required package: cowplot
#> 
#> Loading required package: dplyr
#> 
#> 
#> Attaching package: 'dplyr'
#> 
#> 
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> 
#> 
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> 
#> 
#> Loading required package: gggenes
#> 
#> Loading required package: ggplot2
#> 
#> Loading required package: ggraph
#> 
#> Loading required package: shinycssloaders
#> 
#> Loading required package: tidygraph
#> 
#> 
#> Attaching package: 'tidygraph'
#> 
#> 
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> 
#> 
#> Loading required package: usethis
#> ── R CMD check results ─────────────────────────────────────────────── ravingBrowser 0.0.3.4 ────
#> Duration: 2m 40.9s
#> 
#> ❯ checking Rd cross-references ... WARNING
#>   Missing link or links in Rd file 'BrowserData.Rd':
#>     ‘get_summaries’
#>   
#>   See section 'Cross-references' in the 'Writing R Extensions' manual.
#> 
#> ❯ checking for missing documentation entries ... WARNING
#>   Undocumented code objects:
#>     ‘browser_data’
#>   Undocumented data sets:
#>     ‘browser_data’
#>   All user-level objects in a package should have documentation entries.
#>   See chapter ‘Writing R documentation files’ in the ‘Writing R
#>   Extensions’ manual.
#> 
#> ❯ checking package dependencies ... NOTE
#>   Depends: includes the non-default packages:
#>     'cowplot', 'dplyr', 'gggenes', 'ggplot2', 'ggraph',
#>     'shinycssloaders', 'tidygraph', 'usethis'
#>   Adding so many packages to the search path is excessive and importing
#>   selectively is preferable.
#> 
#> ❯ checking installed package size ... NOTE
#>     installed size is  5.1Mb
#>     sub-directories of 1Mb or more:
#>       data   4.8Mb
#> 
#> ❯ checking R code for possible problems ... [37s/22s] NOTE
#>   .check_overlaps: no visible binding for global variable ‘y_offset’
#>   .check_overlaps: no visible binding for global variable ‘gStart’
#>   .resolve_overlaps: no visible binding for global variable ‘y_offset’
#>   app_server: no visible binding for global variable ‘browser_data’
#>   build_gene_checkbox: no visible binding for global variable
#>     ‘browser_data’
#>   cascade_genes: no visible binding for global variable ‘gStart’
#>   cascade_genes: no visible binding for global variable ‘df’
#>   cascade_genes: no visible binding for global variable ‘width’
#>   clip: no visible binding for global variable ‘tStart’
#>   clip: no visible binding for global variable ‘tEnd’
#>   draw_tads: no visible binding for global variable ‘browser_data’
#>   draw_tads: no visible binding for global variable ‘tChr’
#>   draw_tads: no visible binding for global variable ‘tEnd’
#>   draw_tads: no visible binding for global variable ‘tStart’
#>   draw_tads: no visible binding for global variable ‘tApex’
#>   draw_tads: no visible binding for global variable ‘tDist’
#>   draw_tads: no visible binding for global variable ‘x’
#>   draw_tads: no visible binding for global variable ‘xend’
#>   get_summaries : <anonymous>: no visible global function definition for
#>     ‘.’
#>   get_summaries : <anonymous>: no visible binding for global variable
#>     ‘chromosome’
#>   get_summaries : <anonymous>: no visible binding for global variable
#>     ‘start’
#>   get_summaries : <anonymous>: no visible binding for global variable
#>     ‘end’
#>   get_summaries : <anonymous>: no visible binding for global variable
#>     ‘size’
#>   mod_configure_chip_ui: no visible binding for global variable
#>     ‘browser_data’
#>   mod_configure_hic_ui: no visible binding for global variable
#>     ‘browser_data’
#>   mod_gene_select_server : <anonymous>: no visible binding for global
#>     variable ‘browser_data’
#>   mod_necessary_setup_ui: no visible binding for global variable
#>     ‘browser_data’
#>   mod_region_input_server : <anonymous>: no visible binding for global
#>     variable ‘browser_data’
#>   mod_region_input_ui: no visible binding for global variable
#>     ‘browser_data’
#>   plot_chip: no visible binding for global variable ‘browser_data’
#>   plot_chip: no visible binding for global variable ‘bw_sample_names’
#>   plot_genes: no visible binding for global variable ‘browser_data’
#>   plot_genes: no visible binding for global variable ‘gene_biotype’
#>   plot_genes: no visible binding for global variable ‘gChr’
#>   plot_genes: no visible binding for global variable ‘gEnd’
#>   plot_genes: no visible binding for global variable ‘gStart’
#>   plot_genes: no visible binding for global variable ‘width’
#>   plot_genes: no visible binding for global variable ‘y_offset’
#>   plot_genes: no visible binding for global variable ‘symbol’
#>   plot_genes: no visible binding for global variable ‘strand’
#>   plot_hic: no visible binding for global variable ‘browser_data’
#>   plot_hic: no visible binding for global variable ‘y’
#>   plot_hic: no visible binding for global variable ‘x’
#>   plot_hic: no visible binding for global variable ‘counts’
#>   plot_hic: no visible binding for global variable ‘dist’
#>   plot_loops: no visible binding for global variable ‘browser_data’
#>   plot_loops: no visible binding for global variable ‘lChr1’
#>   plot_loops: no visible binding for global variable ‘lChr2’
#>   plot_loops: no visible binding for global variable ‘from’
#>   plot_loops: no visible binding for global variable ‘to’
#>   plot_loops: no visible binding for global variable ‘bin’
#>   plot_loops: no visible binding for global variable ‘lPval’
#>   plot_pca: no visible binding for global variable ‘browser_data’
#>   plot_pca: no visible binding for global variable ‘pChr’
#>   plot_pca: no visible binding for global variable ‘pEnd’
#>   plot_pca: no visible binding for global variable ‘pStart’
#>   plot_pca: no visible binding for global variable ‘pScore’
#>   validate_hic: no visible binding for global variable ‘browser_data’
#>   validate_region: no visible binding for global variable ‘browser_data’
#>   Undefined global functions or variables:
#>     . bin browser_data bw_sample_names chromosome counts df dist end from
#>     gChr gEnd gStart gene_biotype lChr1 lChr2 lPval pChr pEnd pScore
#>     pStart size start strand symbol tApex tChr tDist tEnd tStart to width
#>     x xend y y_offset
#>   Consider adding
#>     importFrom("stats", "df", "dist", "end", "start")
#>   to your NAMESPACE file.
#> 
#> 0 errors ✔ | 2 warnings ✖ | 3 notes ✖
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
