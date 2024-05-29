
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tmtyro <a href="https://jmclawson.github.io/tmtyro/"><img src="man/figures/logo.png" align="right" height="208" alt="tmtyro website" /></a>

<!-- badges: start -->
<!-- badges: end -->

tmtyro is designed to help beginners work with and analyze text for
simple and complex features. Adopting tidytext principles, tmtyro
abstracts processes a few levels further to allow tyros apply text
mining techniques before they’re deeply familiar with R code.

## Installation

You can install the development version of tmtyro from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("jmclawson/tmtyro")
```

## Use

When you’re ready, begin with the
[introduction](https://jmclawson.github.io/tmtyro/articles/tmtyro.html),
or start using the package right away to load texts from a directory,
measure sentiment, and visualize the results:

``` r
library(tmtyro)

mysteries <- load_texts("mycorpus")

mysteries <- add_sentiment(mysteries)

visualize(mysteries)
```
