# Prepare a corpus or corpora of texts

`get_corpus()` works nearly identically as
[`load_texts()`](https://jmclawson.github.io/tmtyro/reference/load_texts.md),
but it has two fundamental differences. First, it adds a "corpus" column
to the resulting table to help with record keeping. Second, it adds an
option for caching its output in a local RDS file, saved in the project
directory.

## Usage

``` r
get_corpus(
  corpus,
  name = ".txt",
  word = TRUE,
  lemma = FALSE,
  lemma_replace = FALSE,
  to_lower = TRUE,
  remove_names = FALSE,
  pos = FALSE,
  poetry = FALSE,
  paragraph = TRUE,
  cache = TRUE
)
```

## Arguments

- corpus:

  Vector of any length, where each value is either a string identifying
  a directory of texts or the first part of a filename to a cached RDS
  file prepared by tmtyro.

- name:

  What naming pattern to search for in this folder. Defaults to ".txt".

- word:

  Whether to split one word per line. Defaults to TRUE.

- lemma:

  Whether to lemmatize the text. When `word` is TRUE, adds a new column
  called `lemma`. This step can add a lot of time, so it defaults to
  FALSE.

- lemma_replace:

  When `lemma` and `word` are both TRUE, toggles whether to replace the
  `word` column with the lemmatized tokens. Defaults to FALSE

- to_lower:

  When `word` is TRUE, toggles whether to convert all words to
  lowercase. Defaults to TRUE.

- remove_names:

  When `word` is TRUE, toggles whether to remove words that only appear
  with the form of initial capitals. Defaults to FALSE.

- pos:

  Whether to add a column for part-of-speech tag. This step can add a
  lot of time, so it defaults to FALSE.

- poetry:

  Whether to detect and indicate stanza breaks and line breaks. Defaults
  to FALSE.

- paragraph:

  Whether to detect paragraph breaks for prose. Defaults to TRUE.

- cache:

  Whether to save a cached copy of the corpus. Some options like
  `pos = TRUE` and `lemma = TRUE` can add significant time to corpus
  preparation, so setting `cache = TRUE` saves the need to repeat steps
  each time a corpus is loaded. Defaults to TRUE.

## Value

A data frame with columns for corpus, doc_id, and other data.

## Examples

``` r
if (FALSE) { # \dontrun{
  austen <- get_corpus("austen")

  shakespeare <- get_corpus(
    c("comedy",
      "history",
      "tragedy"))
} # }
```
