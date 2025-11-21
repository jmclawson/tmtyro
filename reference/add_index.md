# Index document row numbers

Index document row numbers

## Usage

``` r
add_index(df, by = doc_id, name = word_index)
```

## Arguments

- df:

  A tidy data frame, divided by lines, words, or some other feature

- by:

  A grouping column identifying a document, such as `doc_id`

- name:

  The name to use for the added column

## Value

A data frame with one additional column

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_index() |>
  add_sentiment() |>
  drop_na() |>
  head()
#> # A tibble: 6 × 7
#>   doc_id      title     author       part        word_index word      sentiment
#>   <fct>       <chr>     <chr>        <chr>            <int> <chr>     <chr>    
#> 1 The Sisters Dubliners Joyce, James THE SISTERS         48 evenly    positive 
#> 2 The Sisters Dubliners Joyce, James THE SISTERS         52 dead      negative 
#> 3 The Sisters Dubliners Joyce, James THE SISTERS         64 darkened  negative 
#> 4 The Sisters Dubliners Joyce, James THE SISTERS         65 blind     negative 
#> 5 The Sisters Dubliners Joyce, James THE SISTERS        100 idle      negative 
#> 6 The Sisters Dubliners Joyce, James THE SISTERS        128 strangely negative 
```
