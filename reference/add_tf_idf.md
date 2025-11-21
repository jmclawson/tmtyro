# Compare usage across a corpus

`add_tf_idf()` adds measurements including term frequency by document
and "tf-idf" measurements for weighing relative importance in comparison
to other documents in a set.

## Usage

``` r
add_tf_idf(df, by = doc_id, feature = word)
```

## Arguments

- df:

  A tidy data frame, potentially containing columns called "doc_id" and
  "word"

- by:

  A column containing document grouping

- feature:

  A column containing the terms to be measured across document groupings

## Value

The original data frame with additional columns added for term,
feature_n, (the number of times this term was used in this document), tf
(term's frequency in this document), idf (inverse document frequency),
and tf_idf (previous two columns combined).

## See also

Other tf_idf helpers:
[`summarize_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/summarize_tf_idf.md)

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_tf_idf()
#> # A tibble: 67,945 × 9
#>    doc_id      title     author       part    word      n      tf    idf  tf_idf
#>    <fct>       <chr>     <chr>        <chr>   <chr> <int>   <dbl>  <dbl>   <dbl>
#>  1 The Sisters Dubliners Joyce, James THE SI… there    15 4.82e-3 0      0      
#>  2 The Sisters Dubliners Joyce, James THE SI… was      56 1.80e-2 0      0      
#>  3 The Sisters Dubliners Joyce, James THE SI… no       16 5.14e-3 0      0      
#>  4 The Sisters Dubliners Joyce, James THE SI… hope      1 3.21e-4 0.511  1.64e-4
#>  5 The Sisters Dubliners Joyce, James THE SI… for      32 1.03e-2 0      0      
#>  6 The Sisters Dubliners Joyce, James THE SI… him      43 1.38e-2 0      0      
#>  7 The Sisters Dubliners Joyce, James THE SI… this      6 1.93e-3 0.0690 1.33e-4
#>  8 The Sisters Dubliners Joyce, James THE SI… time      3 9.64e-4 0      0      
#>  9 The Sisters Dubliners Joyce, James THE SI… it       37 1.19e-2 0      0      
#> 10 The Sisters Dubliners Joyce, James THE SI… was      56 1.80e-2 0      0      
#> # ℹ 67,935 more rows
```
