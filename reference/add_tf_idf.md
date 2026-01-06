# Compare usage across a corpus

`add_tf_idf()` adds measurements including term frequency by document
and "tf-idf" measurements for weighing relative importance in comparison
to other documents in a set.

## Usage

``` r
add_tf_idf(data, by = doc_id, feature = word, label = NULL)
```

## Arguments

- data:

  A tidy data frame, potentially containing columns called "doc_id" and
  "word"

- by:

  A column containing document grouping

- feature:

  A column containing the terms to be measured across document groupings

- label:

  Whether to label variables added to data frame

## Value

The original data frame with additional columns added for n, (the number
of times a term was used in this document), tf (term's frequency in this
document), idf (inverse document frequency), and tf_idf (previous two
columns combined).

## See also

Other tf_idf helpers:
[`summarize_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/summarize_tf_idf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_tf_idf()
} # }
```
