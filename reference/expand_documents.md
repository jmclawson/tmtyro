# Convert data frame from long tidy format to wider format

The resulting data frame is a simpler form of the document feature
matrix used by other packages.
`my_df |> expand_documents(percent = FALSE, sort = FALSE)` compares to
`my_df |> count(doc_id, word) |> tidytext::cast_dfm(doc_id, word, n)`,
but it is not equivalent. The latter prepares a DFM to be used with the
quanteda package.

## Usage

``` r
expand_documents(
  df,
  feature = word,
  by = doc_id,
  percent = TRUE,
  sort = TRUE,
  columns = NULL
)
```

## Arguments

- df:

  A tidy data frame, potentially containing a column called "word"

- feature:

  A column of words containing one word per row, to be counted for
  frequency

- by:

  A column containing document grouping

- percent:

  Whether frequencies should be converted to percentages on a
  per-document basis

- sort:

  Whether to sort features by frequency

- columns:

  The features to keep

## Value

A data frame with one row per document and as many features as words.

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  expand_documents()
#> # A tibble: 15 × 7,340
#>    doc_id         the    and     of     to      he      a     was     his   `in`
#>    <fct>        <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>   <dbl>  <dbl>
#>  1 The Sisters 0.0549 0.0379 0.0222 0.0302 0.0180  0.0148 0.0180  0.0157  0.0173
#>  2 An Encount… 0.0556 0.0329 0.0273 0.0264 0.0310  0.0218 0.0181  0.0107  0.0129
#>  3 Araby       0.0810 0.0299 0.0273 0.0299 0.00682 0.0209 0.0171  0.00341 0.0175
#>  4 Eveline     0.0563 0.0251 0.0257 0.0371 0.0246  0.0218 0.0218  0.00601 0.0180
#>  5 After the … 0.0728 0.0299 0.0446 0.025  0.0196  0.0299 0.0268  0.0183  0.0214
#>  6 Two Gallan… 0.0548 0.0332 0.0334 0.0240 0.0416  0.0288 0.0143  0.0286  0.0120
#>  7 The Boardi… 0.0493 0.0316 0.0312 0.0312 0.0241  0.0273 0.0213  0.0174  0.0135
#>  8 A Little C… 0.0502 0.0356 0.0263 0.0247 0.0346  0.0206 0.0136  0.0261  0.0146
#>  9 Counterpar… 0.0761 0.0328 0.0275 0.0265 0.0335  0.0228 0.0187  0.0236  0.0163
#> 10 Clay        0.0594 0.0530 0.0229 0.0301 0.00978 0.0203 0.0259  0.00414 0.0117
#> 11 A Painful … 0.0665 0.0266 0.0352 0.0275 0.0398  0.0255 0.0137  0.0250  0.0190
#> 12 Ivy Day in… 0.0610 0.0242 0.0233 0.0212 0.0259  0.0274 0.00762 0.0160  0.0128
#> 13 A Mother    0.0619 0.0348 0.0227 0.0304 0.0170  0.0211 0.0227  0.0108  0.0152
#> 14 Grace       0.0643 0.0269 0.0299 0.0224 0.0235  0.0260 0.0178  0.0190  0.0150
#> 15 The Dead    0.0551 0.0362 0.0252 0.0234 0.0180  0.0216 0.0159  0.0160  0.0168
#> # ℹ 7,330 more variables: her <dbl>, had <dbl>, said <dbl>, that <dbl>,
#> #   it <dbl>, with <dbl>, `for` <dbl>, him <dbl>, at <dbl>, on <dbl>, i <dbl>,
#> #   she <dbl>, but <dbl>, as <dbl>, were <dbl>, when <dbl>, all <dbl>,
#> #   you <dbl>, they <dbl>, not <dbl>, out <dbl>, up <dbl>, be <dbl>, by <dbl>,
#> #   one <dbl>, from <dbl>, an <dbl>, would <dbl>, then <dbl>, little <dbl>,
#> #   what <dbl>, no <dbl>, have <dbl>, there <dbl>, them <dbl>, which <dbl>,
#> #   so <dbl>, could <dbl>, `if` <dbl>, into <dbl>, went <dbl>, asked <dbl>, …
```
