# Add ngram columns

Add ngram columns

## Usage

``` r
add_ngrams(
  df,
  n = 1:2,
  feature = word,
  keep = FALSE,
  collapse = FALSE,
  by = doc_id
)
```

## Arguments

- df:

  A tidy data frame, potentially containing a column called "word"

- n:

  A range defining the extent of an ngram—for instance, from word 1 to
  word 3. Alternatively, a single number will signal the number of words
  to include in each ngram. Default value of `1:2` will produce bigrams.

- feature:

  The feature to use when constructing ngrams

- keep:

  Whether to keep the original feature column

- collapse:

  Whether to join the ngram parts into a single column called "ngram"

- by:

  A grouping column identifying a document, such as `doc_id`.

## Value

The original data frame with columns added for subsequent parts of
ngrams

## See also

Other n-gram helpers:
[`combine_ngrams()`](https://jmclawson.github.io/tmtyro/reference/combine_ngrams.md),
[`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md),
[`separate_ngrams()`](https://jmclawson.github.io/tmtyro/reference/separate_ngrams.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  my_corpus <- load_texts()

  my_bigrams <- my_corpus |>
    add_ngrams(3)
} # }

dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_ngrams(2) |>
  head()
#> # A tibble: 6 × 6
#>   doc_id      title     author       part        word_1 word_2
#>   <fct>       <chr>     <chr>        <chr>       <chr>  <chr> 
#> 1 The Sisters Dubliners Joyce, James THE SISTERS there  was   
#> 2 The Sisters Dubliners Joyce, James THE SISTERS was    no    
#> 3 The Sisters Dubliners Joyce, James THE SISTERS no     hope  
#> 4 The Sisters Dubliners Joyce, James THE SISTERS hope   for   
#> 5 The Sisters Dubliners Joyce, James THE SISTERS for    him   
#> 6 The Sisters Dubliners Joyce, James THE SISTERS him    this  
```
