# Get sentiment matches of values in a vector

Get sentiment matches of values in a vector

## Usage

``` r
get_sentiment(
  x,
  lexicon = c("bing", "afinn", "loughran", "nrc", "nrc_eil", "nrc_vad"),
  ...
)
```

## Arguments

- x:

  A vector, such as a column of character strings

- lexicon:

  The sentiment lexicon to use from the tidytext package. Options
  include "bing", "afinn", "loughran", "nrc", "nrc_eil", or "nrc_vad".

- ...:

  Additional values passed to
  [`get_match()`](https://jmclawson.github.io/tmtyro/reference/get_match.md)

## Value

A vector or nested list of sentiments for each value of `x`.

## See also

Other vectorized functions:
[`get_cumulative_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/get_cumulative_vocabulary.md),
[`get_frequency()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md),
[`get_hir()`](https://jmclawson.github.io/tmtyro/reference/get_hir.md),
[`get_htr()`](https://jmclawson.github.io/tmtyro/reference/get_htr.md),
[`get_idf_by()`](https://jmclawson.github.io/tmtyro/reference/get_idf_by.md),
[`get_match()`](https://jmclawson.github.io/tmtyro/reference/get_match.md),
[`get_tf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tf_by.md),
[`get_tfidf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tfidf_by.md),
[`get_ttr()`](https://jmclawson.github.io/tmtyro/reference/get_ttr.md),
[`is_hapax()`](https://jmclawson.github.io/tmtyro/reference/is_hapax.md),
[`is_new()`](https://jmclawson.github.io/tmtyro/reference/is_new.md)

## Examples

``` r
my_values <- c("I", "am", "happy")

get_sentiment(my_values)
#> [1] NA         NA         "positive"
```
