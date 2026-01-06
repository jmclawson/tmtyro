# Separate one word per column

Separate one word per column

## Usage

``` r
separate_ngrams(data, names_prefix = "word", ...)
```

## Arguments

- data:

  A tidy data frame containing a column called "ngram"

- names_prefix:

  The prefixed name of the new columns, as in "word_1", "word_2", etc.

- ...:

  Additional options passed to
  [`tidyr::separate_wider_delim()`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html)

## Value

A data frame with one column separated into many

## See also

Other n-gram helpers:
[`add_ngrams()`](https://jmclawson.github.io/tmtyro/reference/add_ngrams.md),
[`combine_ngrams()`](https://jmclawson.github.io/tmtyro/reference/combine_ngrams.md),
[`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  my_corpus <- load_texts(n = 2)

  my_bigrams <- my_corpus |>
    separate_ngrams()

dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_ngrams() |>
  combine_ngrams() |>
  separate_ngrams() |>
  head()
} # }
```
