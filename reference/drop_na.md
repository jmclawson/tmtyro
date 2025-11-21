# Drop rows containing missing values

`drop_na()` drops rows where any column specified by `...` contains a
missing value.

## Usage

``` r
drop_na(data, ...)
```

## Arguments

- data:

  A data frame.

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to inspect for missing values. If empty, all columns are used.

## Details

Another way to interpret `drop_na()` is that it only keeps the
"complete" rows (where no rows contain missing values). Internally, this
completeness is computed through
[`vctrs::vec_detect_complete()`](https://vctrs.r-lib.org/reference/vec_detect_complete.html).

## Examples

``` r
data.frame(
  A = 1:3,
  B = c("red", NA, "green"),
  C = c(TRUE, TRUE, TRUE)) |>
drop_na()
#>   A     B    C
#> 1 1   red TRUE
#> 2 3 green TRUE
```
