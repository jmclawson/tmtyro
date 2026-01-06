# Add sentiment markers

`add_sentiment()` provides simple lexicon-based measures of sentiment,
comparing words in a text to one of a number of controlled dictionaries.

## Usage

``` r
add_sentiment(
  data,
  lexicon = c("bing", "afinn", "loughran", "nrc", "nrc_eil", "nrc_vad"),
  feature = word,
  label = NULL
)
```

## Arguments

- data:

  A tidy data frame, potentially containing a column called "word"

- lexicon:

  The sentiment lexicon to use from the
  [tidytext](https://juliasilge.github.io/tidytext/reference/tidytext-package.html)
  package. Options include "bing", "afinn", "loughran", "nrc",
  "nrc_eil", or "nrc_vad"

- feature:

  A column of words containing one word per row, to be used for
  dictionary look-up

- label:

  Whether to label variables added to data frame

## Value

The original data frame with one or more sentiment columns added.

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_sentiment() |>
  drop_na() |>
  head()
} # }
```
