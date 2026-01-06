# Convert data frame from long tidy format to wider format

The resulting data frame is a simpler form of the document feature
matrix used by other packages.
`my_df |> expand_documents(percent = FALSE, sort = FALSE)` compares to
`my_df |> count(doc_id, word) |> tidytext::cast_dfm(doc_id, word, n)`,
but it is not equivalent. The latter prepares a DFM to be used with the
quanteda package.

## Usage

``` r
expand_documents(
  data,
  feature = word,
  by = doc_id,
  percent = TRUE,
  sort = TRUE,
  columns = NULL
)
```

## Arguments

- data:

  A tidy data frame, potentially containing a column called "word"

- feature:

  A column of words containing one word per row, to be counted for
  frequency

- by:

  A column containing document grouping

- percent:

  Whether frequencies should be converted to percentages on a
  per-document basis

- sort:

  Whether to sort features by frequency

- columns:

  The features to keep

## Value

A data frame with one row per document and as many features as words.

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  expand_documents()
} # }
```
