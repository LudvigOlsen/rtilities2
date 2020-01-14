
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtilities2

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!--[![CRAN status](https://www.r-pkg.org/badges/version/rtilities2)](https://CRAN.R-project.org/package=rtilities2)-->
<!-- badges: end -->

rtilities2 provide a set of utilities and RStudio addins for developing
R packages.

**Author:** [Ludvig R. Olsen](http://ludvigolsen.dk/) (
<r-pkgs@ludvigolsen.dk> ) <br/> **Started:** January 2020

## Addins

  - `Insert Expectations` : generates `testthat` `expect_*` tests from
    selected code
  - `dput() selected` : applies `dput()` to selected code
  - `Insert checkmate AssertCollection code` : inserts code for
    initializing and reporting a checkmate AssertCollection
  - `Wrap string with paste0` : splits selected string every n
    characters and wraps in `paste0` call
  - `Insert %>%`

## Main functions

  - `stop_if()`, `warn_if()`, `message_if()`
  - `is_between()`
  - `%ni%` (read “not in”)
  - `strip()`

## Installation

``` r
devtools::install_github("ludvigolsen/rtilities2")
```
