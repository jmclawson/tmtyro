# Measure lexical variety

`add_vocabulary()` augments a tidy text table with columns describing
the lexical variety of the corpus. Among other things, checks for
uniqueness and size of vocabulary, with additional ratios reporting
these measurements in relation to document size.

## Usage

``` r
add_vocabulary(df, by = doc_id, feature = word)
```

## Arguments

- df:

  A tidy data frame, potentially containing columns called "doc_id" and
  "word"

- by:

  A grouping column

- feature:

  A column of words containing one word per row

## Value

A data frame with 7 added columns , the first two logical and the rest
numeric:

- `new_word` (logical) Indicates whether this is the first instance of a
  given word

- `hapax` (logical) Indicates whether this word is the only incident of
  a given word, or hapax legomenon

- `vocabulary` (integer) Running count of words used

- `ttr` (double) Type-token ratio, derived from the running count of
  words divided by the total number of words used

- `hir` (double) Hapax introduction ratio, derived from the running
  count of hapax legomena divided by the total number of words used.

- `progress_words` (integer) Running count of total words used so far in
  a document

- `progress_percent` (double) Words used so far as a percentage of the
  total number of words used in a document

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
   add_vocabulary() |>
   head()
#> # A tibble: 6 × 12
#>   doc_id      title     author part  word  new_word hapax vocabulary   ttr   hir
#>   <fct>       <chr>     <chr>  <chr> <chr> <lgl>    <lgl>      <int> <dbl> <dbl>
#> 1 The Sisters Dubliners Joyce… THE … there TRUE     FALSE          1     1 0    
#> 2 The Sisters Dubliners Joyce… THE … was   TRUE     FALSE          2     1 0    
#> 3 The Sisters Dubliners Joyce… THE … no    TRUE     FALSE          3     1 0    
#> 4 The Sisters Dubliners Joyce… THE … hope  TRUE     TRUE           4     1 0.25 
#> 5 The Sisters Dubliners Joyce… THE … for   TRUE     FALSE          5     1 0.2  
#> 6 The Sisters Dubliners Joyce… THE … him   TRUE     FALSE          6     1 0.167
#> # ℹ 2 more variables: progress_words <int>, progress_percent <dbl>
```
