# Build and load a corpus from Project Gutenberg

`get_gutenberg_corpus()` improves upon the functionality of
[`gutenbergr::gutenberg_download()`](https://docs.ropensci.org/gutenbergr/reference/gutenberg_download.html)
in three key ways.

1.  Retrieving the ".htm" version of texts instead of the ".zip" version
    typically used by gutenberger dramatically improves file coverage.

2.  Parsing HTML headers allows texts to be studied by sections and
    chapters. Parsing is handled by
    [`parse_html()`](https://jmclawson.github.io/tmtyro/reference/parse_html.md),
    with
    [`move_header_to_text()`](https://jmclawson.github.io/tmtyro/reference/move_header_to_text.md)
    available for corrections.

3.  Caching files locally avoids repeated downloads, thereby improving
    code portability, allowing offline access, and reducing network use.

All changes are made with consideration for server bandwidth, so a
two-second delay is introduced between each download attempt. This will
slow down the initial acquisition of corpora, but offline caching speeds
things up considerably in subsequent use.

## Usage

``` r
get_gutenberg_corpus(
  gutenberg_id,
  download = c("auto", "always", "temp", "never"),
  dir = "gutenberg",
  meta_fields = c("gutenberg_id", "title", "author"),
  html_title = FALSE,
  ...
)
```

## Arguments

- gutenberg_id:

  A vector of ID numbers from Project Gutenberg or a data frame
  containing a `gutenberg_id` column, such as from the results of a call
  to
  [`gutenbergr::gutenberg_works()`](https://docs.ropensci.org/gutenbergr/reference/gutenberg_works.html)

- download:

  Whether files should be automatically downloaded into a project
  subdirectory as needed (the default), always downloaded into the
  project folder, temporarily downloaded once per-session, or never
  downloaded

- dir:

  The project subdirectory for storing downloaded `.htm` files

- meta_fields:

  Additional fields to add from
  [gutenbergr::gutenberg_metadata](https://docs.ropensci.org/gutenbergr/reference/gutenberg_metadata.html)
  describing each book

- html_title:

  Whether to use the h1 header from an HTML file to determine a
  document's title instead of
  [gutenbergr::gutenberg_metadata](https://docs.ropensci.org/gutenbergr/reference/gutenberg_metadata.html)

- ...:

  Additional parameters passed along to
  [`gutenbergr::gutenberg_strip()`](https://docs.ropensci.org/gutenbergr/reference/gutenberg_strip.html)

## Value

A data frame with one row for each line of the texts in the corpus

## Examples

``` r
if (FALSE) { # \dontrun{
library(gutenbergr)

dalloway <- gutenberg_works(author == "Woolf, Virginia",
                            title == "Mrs Dalloway in Bond Street") |>
  get_gutenberg_corpus()
} # }
```
