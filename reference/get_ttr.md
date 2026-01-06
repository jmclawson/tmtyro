# Cumulative type-token ratio

TTR reports the ratio of unique word types to the total size of a text.

## Usage

``` r
get_ttr(x)
```

## Arguments

- x:

  A vector, such as a column of character strings

## Value

A vector of ratios

## See also

Other vectorized functions:
[`get_cumulative_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/get_cumulative_vocabulary.md),
[`get_df_by()`](https://jmclawson.github.io/tmtyro/reference/get_df_by.md),
[`get_frequency()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md),
[`get_hir()`](https://jmclawson.github.io/tmtyro/reference/get_hir.md),
[`get_htr()`](https://jmclawson.github.io/tmtyro/reference/get_htr.md),
[`get_idf_by()`](https://jmclawson.github.io/tmtyro/reference/get_idf_by.md),
[`get_match()`](https://jmclawson.github.io/tmtyro/reference/get_match.md),
[`get_sentiment()`](https://jmclawson.github.io/tmtyro/reference/get_sentiment.md),
[`get_tf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tf_by.md),
[`get_tfidf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tfidf_by.md),
[`is_hapax()`](https://jmclawson.github.io/tmtyro/reference/is_hapax.md),
[`is_new()`](https://jmclawson.github.io/tmtyro/reference/is_new.md)

## Examples

``` r
c("cat", "dog", "dog", "bat", "dog") |>
  get_ttr()
#> [1] 1.0000000 1.0000000 0.6666667 0.7500000 0.6000000
```
