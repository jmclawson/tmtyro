# Get inverse document frequencies of values in one vector `x` categorized by another vector `by`.

Get inverse document frequencies of values in one vector `x` categorized
by another vector `by`.

## Usage

``` r
get_idf_by(x, by)
```

## Arguments

- x:

  A vector, such as a column of character strings

- by:

  A vector of categories, such as a column of document identifiers

## Value

A vector of inverse document frequencies for each value pair of `x` and
`by`.

## See also

Other vectorized functions:
[`get_cumulative_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/get_cumulative_vocabulary.md),
[`get_frequency()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md),
[`get_hir()`](https://jmclawson.github.io/tmtyro/reference/get_hir.md),
[`get_htr()`](https://jmclawson.github.io/tmtyro/reference/get_htr.md),
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

get_idf_by(my_values, my_docs)
#>  [1] 0.0000000 1.0986123 0.4054651 0.4054651 0.0000000 1.0986123 0.4054651
#>  [8] 1.0986123 1.0986123 0.0000000 1.0986123 1.0986123 0.0000000 1.0986123
#> [15] 0.4054651
```
