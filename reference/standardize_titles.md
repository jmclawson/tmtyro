# Standardize document titles

Useful especially for visualizations. `standardize_titles` applies some
English-language conventions, including converting underscores to
spaces, capitalizing important words, removing leading articles, and
dropping subtitles.

## Usage

``` r
standardize_titles(data, title = doc_id, drop_articles = FALSE)
```

## Arguments

- data:

  A tidy data frame, potentially containing a title column called
  "doc_id". Alternatively, a simple character vector of titles.

- title:

  A column containing the titles to be standardized

- drop_articles:

  Whether to remove opening articles like "The" and "A"

## Value

A data frame with one column adjusted. If `data` is a character vector
instead of a data frame, then a character vector is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part)

##### Standardizing strings #####
# Before `standardize_titles()`
unique(dubliners$doc_id)

# After `standardize_titles()`
unique(dubliners$doc_id) |>
  standardize_titles()

##### Standardizing a data frame #####

dubliners_measured <- dubliners |>
  add_vocabulary()

# Before `standardize_titles()`
dubliners_measured |>
  plot_vocabulary(labeling = "inline")

# After `standardize_titles()`
dubliners_measured |>
  standardize_titles() |>
  plot_vocabulary(labeling = "inline")
} # }
```
