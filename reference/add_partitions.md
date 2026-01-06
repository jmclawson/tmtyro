# Divide documents in equal lengths

Divide documents in equal lengths

## Usage

``` r
add_partitions(
  data,
  size = 1000,
  overlap = 0,
  minimum = 0.25,
  by = doc_id,
  feature = word,
  character = FALSE,
  label = NULL
)
```

## Arguments

- data:

  A tidy data frame, potentially containing a column called "word"

- size:

  Size of each partition

- overlap:

  Size each partition should overlap. If a value between 0 and 1 is
  used, `overlap` will be calculated as a percentage of `size`.

- minimum:

  Minimum partition size. If a value between 0 and 1 is used, `minimum`
  will be calculated as a percentage of `size`.

- by:

  A column containing document grouping

- feature:

  The feature to partition by in each document

- character:

  Whether to return a `partition` column as a character vector with
  zeroes added for padding. This feature may be helpful if using
  [`identify_by()`](https://jmclawson.github.io/tmtyro/reference/identify_by.md)
  to consider `partition` when defining documents in a corpus.

- label:

  Whether to label variables added to data frame

## Value

The original data frame with a column added for partition.

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_partitions() |>
  head()
} # }
```
