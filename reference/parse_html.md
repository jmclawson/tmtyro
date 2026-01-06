# Read HTML headers and text from file

Read HTML headers and text from file

## Usage

``` r
parse_html(html, headers = 1:6, standardize_headers = TRUE, title = TRUE)
```

## Arguments

- html:

  A file in HTML format

- headers:

  The HTML header levels to consider

- standardize_headers:

  Whether to standardize HTML headers to useful column names

- title:

  Whether to keep H1 tags even when there is only one unique value

## Value

A data frame with a column called "text" and header columns limited to
page elements like h1, h2, and h3, as included in the numeric range of
`headers`

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
