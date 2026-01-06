# Get document frequencies of values in one vector `x` categorized by another vector `by`.

Get document frequencies of values in one vector `x` categorized by
another vector `by`.

## Usage

``` r
get_df_by(x, by, percent = TRUE)
```

## Arguments

- x:

  A vector, such as a column of character strings

- by:

  A vector of categories, such as a column of document identifiers

- percent:

  Whether to return frequencies as percentage of the whole

## Value

A vector of document frequencies for each value pair of `x` and `by`.

## See also

Other vectorized functions:
[`get_cumulative_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/get_cumulative_vocabulary.md),
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
my_values <- c(
  "the", "cat", "was", "bad",
  "the", "dog", "was", "very", "good",
  "the", "lizard", "is", "the", "most", "bad")
my_docs <- c(
  "A", "A", "A", "A",
  "B", "B", "B", "B", "B",
  "C", "C", "C", "C", "C", "C")

get_df_by(my_values, my_docs)
#>  [1] 1.0000000 0.3333333 0.6666667 0.6666667 1.0000000 0.3333333 0.6666667
#>  [8] 0.3333333 0.3333333 1.0000000 0.3333333 0.3333333 1.0000000 0.3333333
#> [15] 0.6666667
```
