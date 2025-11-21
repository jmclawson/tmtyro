# Remove stopwords

Remove stopwords

## Usage

``` r
drop_stopwords(df, wordlist = NULL, feature = word)
```

## Arguments

- df:

  A tidy data frame, potentially containing a column called "word"

- wordlist:

  A list of stopwords

- feature:

  A column of words containing one word per row to be checked for
  stopwords.

## Value

The original data frame with fewer rows.

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
   drop_stopwords()
#> # A tibble: 34,242 × 5
#>    doc_id      title     author       part        word    
#>    <fct>       <chr>     <chr>        <chr>       <chr>   
#>  1 The Sisters Dubliners Joyce, James THE SISTERS hope    
#>  2 The Sisters Dubliners Joyce, James THE SISTERS time    
#>  3 The Sisters Dubliners Joyce, James THE SISTERS third   
#>  4 The Sisters Dubliners Joyce, James THE SISTERS stroke  
#>  5 The Sisters Dubliners Joyce, James THE SISTERS night   
#>  6 The Sisters Dubliners Joyce, James THE SISTERS night   
#>  7 The Sisters Dubliners Joyce, James THE SISTERS passed  
#>  8 The Sisters Dubliners Joyce, James THE SISTERS house   
#>  9 The Sisters Dubliners Joyce, James THE SISTERS vacation
#> 10 The Sisters Dubliners Joyce, James THE SISTERS time    
#> # ℹ 34,232 more rows
```
