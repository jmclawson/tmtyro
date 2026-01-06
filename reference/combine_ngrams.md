# Combine ngram columns

Combine ngram columns

## Usage

``` r
combine_ngrams(data, feature = word, keep = FALSE, label = NULL)
```

## Arguments

- data:

  A tidy data frame, potentially containing columns called "word_1",
  "word_2", etc.

- feature:

  The column name prefix for the numbered columns of ngrams

- keep:

  Whether to keep the original columns called "word_1", "word_2", etc.,
  alongside the new "ngram" column

- label:

  Whether to label variables added to data frame

## Value

A data frame with a column called ngram

## See also

Other n-gram helpers:
[`add_ngrams()`](https://jmclawson.github.io/tmtyro/reference/add_ngrams.md),
[`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md),
[`separate_ngrams()`](https://jmclawson.github.io/tmtyro/reference/separate_ngrams.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  my_corpus <- load_texts(n = 2)

  my_bigrams <- my_corpus |>
    add_ngrams(collapse = FALSE) |>
    combine_ngrams()

dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_ngrams(2) |>
  combine_ngrams() |>
  head()
} # }
```
