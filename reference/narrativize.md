# Describe the steps taken

`narrativize()` provides a simple method for sharing methods used to
prepare an analysis.

## Usage

``` r
narrativize(
  data,
  format = c("bullets", "text"),
  dictionary = narrative_dictionary_en,
  return = c("message", "character", "print", "html"),
  person = c(NA, "we", "I")
)
```

## Arguments

- data:

  Data processed with one or more functions from tmtyro

- format:

  The format of output to return. Use `format = "bullets"` for well
  formatted bullets and `format = "text"` for paragraph-formatted text

- dictionary:

  The narrative dictionary to use.
  [`narrative_dictionary_en`](https://jmclawson.github.io/tmtyro/reference/narrative_dictionary_en.md)
  is defined by default.

- return:

  How output should be returned, whether using
  [`message()`](https://rdrr.io/r/base/message.html),
  [`print()`](https://rdrr.io/r/base/print.html),
  [`cat()`](https://rdrr.io/r/base/cat.html), or as a standard character
  string

- person:

  The personal pronoun to use in the narrative, if any. This parameter
  depends on the narrative dictionary defined in `dictionary`.

## Value

Text

## Examples

    dubliners <- get_gutenberg_corpus(2814) |>
      load_texts() |>
      identify_by(part) |>
      standardize_titles(drop_articles = TRUE) |>
      add_frequency()

    dubliners |>
       narrativize()

Returns the following:

- Retrieved a corpus from Project Gutenberg using ID number 2814.

- Loaded texts by tokenizing words, converting to lowercase, and
  preserving paragraph breaks.

- Identified documents using the column `part`.

- Standardized titles by converting to title case.

- Counted the frequency of words used in each document.

## See also

Other logging helpers:
[`methods_log`](https://jmclawson.github.io/tmtyro/reference/methods_log.md),
[`narrative_dictionary_en`](https://jmclawson.github.io/tmtyro/reference/narrative_dictionary_en.md)
