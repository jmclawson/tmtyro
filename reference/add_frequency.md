# Add frequency of words or other features

Add frequency of words or other features

## Usage

``` r
add_frequency(df, feature = word, by = doc_id)
```

## Arguments

- df:

  A tidy data frame, potentially containing a column called "word"

- feature:

  The feature to count in each document

- by:

  A grouping column identifying a document, such as `doc_id`.

## Value

The original data frame with a column added for frequency

## Examples

``` r
if (FALSE) { # \dontrun{
  my_corpus <- load_texts()

  my_bigrams <- my_corpus |>
    add_frequency()
} # }

dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_frequency() |>
  head()
#> # A tibble: 6 × 6
#>   doc_id      title     author       part        word      n
#>   <fct>       <chr>     <chr>        <chr>       <chr> <int>
#> 1 The Sisters Dubliners Joyce, James THE SISTERS there    15
#> 2 The Sisters Dubliners Joyce, James THE SISTERS was      56
#> 3 The Sisters Dubliners Joyce, James THE SISTERS no       16
#> 4 The Sisters Dubliners Joyce, James THE SISTERS hope      1
#> 5 The Sisters Dubliners Joyce, James THE SISTERS for      32
#> 6 The Sisters Dubliners Joyce, James THE SISTERS him      43
```
