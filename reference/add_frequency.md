# Add frequency of words or other features

Add frequency of words or other features

## Usage

``` r
add_frequency(data, feature = word, by = doc_id, label = NULL)
```

## Arguments

- data:

  A tidy data frame, potentially containing a column called "word"

- feature:

  The feature to count in each document

- by:

  A grouping column identifying a document, such as `doc_id`

- label:

  Whether to label variables added to data frame

## Value

The original data frame with a column added for frequency

## Examples

``` r
if (FALSE) { # \dontrun{
  my_corpus <- load_texts()

  my_bigrams <- my_corpus |>
    add_frequency()
} # }

if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_frequency() |>
  head()
} # }
```
