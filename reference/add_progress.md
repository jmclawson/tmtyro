# Track progress in documents

Track progress in documents

## Usage

``` r
add_progress(
  data,
  by = doc_id,
  unit = c("percentage", "index"),
  name = progress,
  label = NULL
)

add_index(data, by = doc_id, name = word_index, label = NULL)
```

## Arguments

- data:

  A tidy data frame, divided by lines, words, or some other feature

- by:

  A grouping column identifying a document, such as `doc_id`

- unit:

  Unit for measuring progress, either as a percentage or as an index of
  row number

- name:

  The name to use for the added column

- label:

  Whether to label variables added to data frame

## Value

A data frame with one additional column

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_index() |>
  add_sentiment() |>
  drop_na() |>
  head()
} # }
```
