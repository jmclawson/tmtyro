# Check for hapax legomena

Check for hapax legomena

## Usage

``` r
is_hapax(x)
```

## Arguments

- x:

  A vector, such as a column of character strings

## Value

A logical vector

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
[`get_ttr()`](https://jmclawson.github.io/tmtyro/reference/get_ttr.md),
[`is_new()`](https://jmclawson.github.io/tmtyro/reference/is_new.md)

## Examples

``` r
c("cat", "dog", "dog", "bat", "dog") |>
  is_hapax()
#> [1]  TRUE FALSE FALSE  TRUE FALSE
```
