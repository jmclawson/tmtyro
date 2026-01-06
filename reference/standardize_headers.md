# Standardize column names from HTML

needed tests: unnamed arguments, values repurposed from defaults, mix of
named and unnamed arguments, title = FALSE/TRUE

## Usage

``` r
standardize_headers(data, ..., title = TRUE)
```

## Arguments

- data:

  A tidy data frame, potentially containing header columns "h1" through
  "h6"

- ...:

  Optionally, a named list of columns and values to which they should be
  renamed, with defaults as faullback

- title:

  Whether any "h1" column should be renamed to "title"

## Value

A data frame with column names adjusted

## Examples

``` r
if (FALSE) {
  joyce2 <- joyce |>
    standardize_titles() |>
    move_column_to_text(subsection, title == "Ulysses")
}
```
