# Term frequency–inverse document frequency

Get tf-idf weights of values in one vector `x` categorized by another
vector `by`.

## Usage

``` r
get_tfidf_by(x, by)
```

## Arguments

- x:

  A vector, such as a column of character strings

- by:

  A vector of categories, such as a column of document identifiers

## Value

A vector of term frequency–inverse document frequencies for each value
pair of `x` and `by`.

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

get_tfidf_by(my_values, my_docs)
#>  [1] 0.00000000 0.27465307 0.10136628 0.10136628 0.00000000 0.21972246
#>  [7] 0.08109302 0.21972246 0.21972246 0.00000000 0.18310205 0.18310205
#> [13] 0.00000000 0.18310205 0.06757752
```
