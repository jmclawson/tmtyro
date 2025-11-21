# Read HTML headers and text from file

Read HTML headers and text from file

## Usage

``` r
parse_html(html, title = TRUE)
```

## Arguments

- html:

  A file in HTML format

- title:

  Whether to keep H1 tags even when there is only one unique value

## Value

A data frame with a column called `text` and header columns called
`title`, `part`, `section`, and `subsection` as needed. Header columns
are limited to page elements tagged as h1, h2, h3, or h4.

## Examples

``` r
if (FALSE) {
  library(dplyr)
  library(stringr)
  library(tmtyro)

  orlando <-
    "http://gutenberg.net.au/ebooks02/0200331h.html" |>
    download_once() |>
    parse_html() |>
    filter(str_detect(part, "CHAPTER")) |>
    mutate(
      chapter = str_extract(part, "\\d"),
      author = "Virginia Woolf") |>
    select(author, title, chapter, text) |>
    drop_na(chapter) |>
    identify_by(title, chapter) |>
    load_texts()
}
```
