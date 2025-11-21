# Add sentiment markers

`add_sentiment()` provides simple lexicon-based measures of sentiment,
comparing words in a text to one of a number of controlled dictionaries.

## Usage

``` r
add_sentiment(
  df,
  lexicon = c("bing", "afinn", "loughran", "nrc", "nrc_eil", "nrc_vad"),
  feature = word
)
```

## Arguments

- df:

  A tidy data frame, potentially containing a column called "word"

- lexicon:

  The sentiment lexicon to use from the tidytext package. Options
  include "bing", "afinn", "loughran", "nrc", "nrc_eil", or "nrc_vad".

- feature:

  A column of words containing one word per row, to be used for
  dictionary look-up.

## Value

The original data frame with one or more sentiment columns added.

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
   add_sentiment() |>
   drop_na() |>
   head()
#> # A tibble: 6 × 6
#>   doc_id      title     author       part        word      sentiment
#>   <fct>       <chr>     <chr>        <chr>       <chr>     <chr>    
#> 1 The Sisters Dubliners Joyce, James THE SISTERS evenly    positive 
#> 2 The Sisters Dubliners Joyce, James THE SISTERS dead      negative 
#> 3 The Sisters Dubliners Joyce, James THE SISTERS darkened  negative 
#> 4 The Sisters Dubliners Joyce, James THE SISTERS blind     negative 
#> 5 The Sisters Dubliners Joyce, James THE SISTERS idle      negative 
#> 6 The Sisters Dubliners Joyce, James THE SISTERS strangely negative 
```
