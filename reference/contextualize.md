# Show a term in context

Show a term in context

## Usage

``` r
contextualize(
  df,
  term,
  window = 3,
  limit = 1:5,
  by = doc_id,
  feature = NULL,
  match = word,
  regex = NULL,
  html = NULL
)
```

## Arguments

- df:

  A data frame which most likely contains a column called "word"

- term:

  The term to search for, exactly

- window:

  The number of terms to show before and after

- limit:

  The number of results to return in the console using cli, if installed

- by:

  The document identifier, for limiting context window

- feature:

  The column to show for context. When `NULL`, `contextualize()` looks
  first for an "original" column and then for a "word" column.

- match:

  The column to use for matching

- regex:

  When defined, a regular expression for searches using greater control

- html:

  Force message output to HTML format (e.g., for Shiny)

## Value

Invisibly, a data frame with four columns: `<by>`, `<match>`, "index",
and "context"

## Hiding results

`contextualize()` uses
[`cli::style_underline()`](https://cli.r-lib.org/reference/ansi-styles.html)
and [`fansi::to_html()`](https://rdrr.io/pkg/fansi/man/to_html.html), if
these packages are installed, to show formatted results in the console
or in a document rendered to HTML. These formatted results can be hidden
by setting `limit = 0` in the function, by suppressing messages with
[`suppressMessages()`](https://rdrr.io/r/base/message.html) in the
console, or by setting a `message: false` chunk option in Quarto or R
Markdown.

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
    load_texts(keep_original = TRUE)

contextualize(dubliners, regex = "dog[s]?$")
#> on a thick bulldog face and a
#> gone to the dogs.” “But Hogan has
#> throw to a dog. He stands and
#> order. ‘Down, ye dogs! Lie down, ye
#> these two fighting dog and devil until
```
