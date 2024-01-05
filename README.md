
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tmtyro <a href="https://jmclawson.github.io/tmtyro/"><img src="man/figures/logo.png" align="right" height="208" alt="tmtyro website" /></a>

<!-- badges: start -->
<!-- badges: end -->

The goal of tmtyro is to help beginners work with and analyze text for
simple and complex features. Adopting tidytext principles, tmtyro
abstracts processes a few levels further to allow tyros apply text
mining techniques before they’re deeply familiar with R code.

## Installation

You can install the development version of tmtyro from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jmclawson/tmtyro")
```

## Example

One of the first steps of any text mining workflow is to load a corpus
of texts from a directory. While doing so, it may be desirable to detect
paragraph breaks and prepare a data frame with one word per row. The
`load_texts()` function makes this easy.

``` r
library(tmtyro)
mysteries <- load_texts("mycorpus")
```
