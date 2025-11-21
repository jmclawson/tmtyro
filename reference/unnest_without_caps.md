# Split text into words and drop proper nouns

Split a column of text using
[`tidytext::unnest_tokens()`](https://juliasilge.github.io/tidytext/reference/unnest_tokens.html),
flattening the table into one token per row while also omitting any
token that is present only in a capitalized form.

## Usage

``` r
unnest_without_caps(df, output = "word", input = "text", to_lower = TRUE)
```

## Arguments

- df:

  A data frame

- output:

  Output column to be created.

- input:

  Input column that gets split by word.

- to_lower:

  Whether to convert final words to lowercase.

## Value

A data frame

## Examples

``` r
if (FALSE) { # \dontrun{
mysteries <-
  load_texts("mystery-novels",
             to_lower = FALSE) |>
  unnest_without_caps()

# Since `unnest_without_caps()` is
# incorporated into `load_texts()`,
# it may be unnecessary for many
# scenarios.
mysteries <-
  load_texts("mystery-novels",
             remove_names = TRUE)
  } # }
```
