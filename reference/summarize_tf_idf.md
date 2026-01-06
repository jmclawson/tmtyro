# Compare usage across a corpus

`summarize_tf_idf()` prepares a summary table for each term in a corpus,
including their frequencies by document and "tf-idf" measurements for
comparing the relative importance in comparison to other documents in a
set.

## Usage

``` r
summarize_tf_idf(data, by = doc_id, feature = word)
```

## Arguments

- data:

  A tidy data frame, potentially containing columns called "doc_id" and
  "word"

- by:

  A column containing document grouping

- feature:

  A column containing the terms to be measured across document groupings

## Value

A summary of the original data frame, with rows for each document and
term pairing and columns for document identifier, term, n (the number of
times this term was used in this document), tf (term's frequency in this
document), idf (inverse document frequency), and tf_idf (previous two
columns combined).

## See also

Other tf_idf helpers:
[`add_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/add_tf_idf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  summarize_tf_idf()
} # }
```
