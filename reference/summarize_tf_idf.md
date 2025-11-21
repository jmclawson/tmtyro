# Compare usage across a corpus

`summarize_tf_idf()` prepares a summary table for each term in a corpus,
including their frequencies by document and "tf-idf" measurements for
comparing the relative importance in comparison to other documents in a
set.

## Usage

``` r
summarize_tf_idf(df, by = doc_id, feature = word)
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

A summary of the original data frame, with rows for each document and
term pairing and columns for document identifier, term, n (the number of
times this term was used in this document), tf (term's frequency in this
document), idf (inverse document frequency), and tf_idf (previous two
columns combined).

## See also

Other tf_idf helpers:
[`add_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/add_tf_idf.md)

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  summarize_tf_idf()
#> # A tibble: 17,686 × 6
#>    doc_id                        word         n      tf   idf tf_idf
#>    <fct>                         <chr>    <int>   <dbl> <dbl>  <dbl>
#>  1 Clay                          maria       40 0.0150   2.71 0.0407
#>  2 Two Gallants                  corley      46 0.0117   2.71 0.0318
#>  3 After the Race                jimmy       24 0.0107   2.71 0.0290
#>  4 Ivy Day in the Committee Room henchy      53 0.0101   2.71 0.0274
#>  5 A Little Cloud                gallaher    48 0.00972  2.71 0.0263
#>  6 The Dead                      gabriel    142 0.00903  2.71 0.0244
#>  7 Grace                         kernan      66 0.00875  2.71 0.0237
#>  8 Ivy Day in the Committee Room o’connor    45 0.00858  2.71 0.0232
#>  9 A Little Cloud                chandler    41 0.00830  2.71 0.0225
#> 10 A Mother                      kearney     50 0.0110   2.01 0.0222
#> # ℹ 17,676 more rows
```
