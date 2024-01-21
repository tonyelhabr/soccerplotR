
<!-- README.md is generated from README.Rmd. Please edit that file -->

# soccerplotR <a href='https://tonyelhabr.github.io/soccerplotR/'><img src="man/figures/logo.png" align="right" width="25%" min-width="120px"/></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/soccerplotR)](https://CRAN.R-project.org/package=soccerplotR)
[![R-CMD-check](https://github.com/tonyelhabr/soccerplotR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tonyelhabr/soccerplotR/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of `{soccerplotR}` is to provide functions and geoms that help
visualization of professional soccer analysis. It provides a `{ggplot2}`
geom that does the heavy lifting of plotting soccer team logos in high
quality, with correct aspect ratio and possible transparency. All of
this is done by the powerful [ggpath
package](https://mrcaseb.github.io/ggpath/).

## Installation

To get a bug fix or to use a feature from the development version, you
can install the development version of `{soccerplotR}` from
[GitHub](https://github.com/mrcaseb/soccerplotR/) with:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("tonyelhabr/soccerplotR")
```

## Examples

``` r
library(soccerplotR)
library(ggplot2)

FONT <- 'Kanit'
sysfonts::font_add_google(FONT)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

team_names <- unlist(unname(soccerplotR::all_valid_team_names()))
set.seed(42)
sampled_team_names <- sample(team_names, size = 45)

df <- data.frame(
  a = rep(1:5, 9),
  b = sort(rep(0:8, 5), decreasing = TRUE),
  team_name = sampled_team_names
)

ggplot(df) +
  aes(
    x = a, 
    y = b
  ) +
  geom_soccer_logos(
    aes(
      team_name = team_name
    ), 
    width = 0.075
  ) +
  geom_label(
    aes(
      label = team_name,
      fill = team_name
    ),
    color = 'white',
    family = FONT,
    size = 10 / .pt,
    nudge_y = -0.5
  ) +
  scale_fill_soccer(type = 'primary') +
  theme_void() +
  theme(
    plot.margin = margin(25, 25, 25, 25, 'pt')
  ) +
  coord_cartesian(clip = 'off') +
  labs(
    title = 'A random sample of 45 teams'
  ) +
  theme(
    plot.title.position = 'plot',
    plot.title = element_text(family = FONT, size = 18, hjust = 0.5, vjust = 1)
  )
```

<img src="man/figures/README-example-1.png" width="100%" />
