# Cumulative total of vocabulary size

Cumulative total of vocabulary size

## Usage

``` r
get_cumulative_vocabulary(x)
```

## Arguments

- x:

  A vector, such as a column of character strings

## Value

A vector of counts

## See also

Other vectorized functions:
[`get_frequency()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md),
[`get_hir()`](https://jmclawson.github.io/tmtyro/reference/get_hir.md),
[`get_htr()`](https://jmclawson.github.io/tmtyro/reference/get_htr.md),
[`get_idf_by()`](https://jmclawson.github.io/tmtyro/reference/get_idf_by.md),
[`get_match()`](https://jmclawson.github.io/tmtyro/reference/get_match.md),
[`get_sentiment()`](https://jmclawson.github.io/tmtyro/reference/get_sentiment.md),
[`get_tf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tf_by.md),
[`get_tfidf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tfidf_by.md),
[`get_ttr()`](https://jmclawson.github.io/tmtyro/reference/get_ttr.md),
[`is_hapax()`](https://jmclawson.github.io/tmtyro/reference/is_hapax.md),
[`is_new()`](https://jmclawson.github.io/tmtyro/reference/is_new.md)

## Examples

``` r
c("cat", "dog", "dog", "bat", "dog") |>
  get_cumulative_vocabulary()
#> [1] 1 2 2 3 3
```
