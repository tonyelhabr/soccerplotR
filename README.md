# soccerplotR <a href='https://tonyelhabr.github.io/soccerplotR/'><img src="man/figures/logo.png" align="right" width="25%" min-width="120px"/></a>

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![CRAN status](https://www.r-pkg.org/badges/version/nbaplotR)](https://CRAN.R-project.org/package=nbaplotR)

<!-- badges: end -->

The goal of `{soccerplotR}` is to provide functions and geoms that help visualization of professional soccer analysis. It provides a `{ggplot2}` geom that does the heavy lifting of plotting soccer team logos in high quality, with correct aspect ratio and possible transparency. All of this is done by the powerful [ggpath package](https://mrcaseb.github.io/ggpath/).

## Installation

To get a bug fix or to use a feature from the development version, you can install the development version of `{soccerplotR}` from [GitHub](https://github.com/mrcaseb/soccerplotR/) with:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("tonyelhabr/soccerplotR")
```
