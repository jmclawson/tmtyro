# Methods logging

Get, set, and append to a tmtyro methods log stored as an attribute on a
data frame. The methods log can later be rendered into a narrative with
[`narrativize()`](https://jmclawson.github.io/tmtyro/reference/narrativize.md).

## Usage

``` r
get_methods_log(x)

set_methods_log(x, methods_log)

add_methods_log(x, string)
```

## Arguments

- x:

  A data frame whose methods log is extracted or modified

- methods_log:

  A methods log to apply to `x`

- string:

  A single character string to append as a manual methods entry

## Value

`get_methods_log()` returns the methods log or `NULL` if none is
present. `set_methods_log()` and `add_methods_log()` return `x` with an
updated methods log.

## Details

The methods log is stored in a list as a data-frame attribute.
`add_methods_log()` appends a single, user- supplied entry to the
existing log.

## See also

Other logging helpers:
[`narrative_dictionary_en`](https://jmclawson.github.io/tmtyro/reference/narrative_dictionary_en.md),
[`narrativize()`](https://jmclawson.github.io/tmtyro/reference/narrativize.md)

## Examples

``` r
my_df <- data.frame(a = 1:3)

# Add a manual entry
my_df2 <- my_df |>
  dplyr::filter(!is.na(a)) |>
  add_methods_log("Removed rows with missing values.")

# Extract and re-apply
my_log <- get_methods_log(my_df2)

my_df3 <- my_df2 |>
  set_methods_log(my_log)
```
