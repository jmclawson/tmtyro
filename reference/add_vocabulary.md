# Measure lexical variety

`add_vocabulary()` augments a tidy text table with columns describing
the lexical variety of the corpus. Among other things, checks for
uniqueness and size of vocabulary, with additional ratios reporting
these measurements in relation to document size.

## Usage

``` r
add_vocabulary(data, by = doc_id, feature = word, label = NULL)
```

## Arguments

- data:

  A tidy data frame, potentially containing columns called "doc_id" and
  "word"

- by:

  A grouping column

- feature:

  A column of words containing one word per row

- label:

  Whether to label variables added to data frame

## Value

A data frame with 7 added columns , the first two logical and the rest
numeric:

- `new_word` (logical) Indicates whether this is the first instance of a
  given word

- `hapax_doc` (logical) Indicates whether this word is the only incident
  of a given word, or hapax legomenon, at the document level

- `hapax_corpus` (logical) Indicates whether this word is the only
  incident of a given word, or hapax legomenon, at the corpus level

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
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  add_vocabulary() |>
  head()
} # }
```
