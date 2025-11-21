# Load a folder or data frame of texts

`load_texts()` loads a corpus from a folder of texts or a data frame and
prepares it for further study using tidytext principles. By default,
`load_texts()` will add paragraph numbers (suitable for prose), and
unnest at the word level, but options exist to change these defaults for
poetry, to avoid unnesting, and even to remove words that seem like
proper nouns or to apply techniques of natural language processing for
lemmatizing words or tagging their parts of speech.

## Usage

``` r
load_texts(
  src = "data",
  name = ".txt",
  word = TRUE,
  lemma = FALSE,
  lemma_replace = FALSE,
  to_lower = TRUE,
  remove_names = FALSE,
  pos = FALSE,
  keep_original = FALSE,
  poetry = FALSE,
  paragraph = TRUE,
  n = 1L,
  ...
)
```

## Arguments

- src:

  Either a string identifying the name of a directory containing texts
  or a data frame containing an unnested column called "text" and one
  column with a name ending in "\_id". Files should either be stored in
  a directory within the project folder or under the subdirectory called
  "data". Defaults to "data" to load texts from that directory.

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

- keep_original:

  Whether to try to retain the original punctuation and capitalization
  in a parallel column. This won't always work, so it defaults to FALSE.

- poetry:

  Whether to detect and indicate stanza breaks and line breaks. Defaults
  to FALSE.

- paragraph:

  Whether to detect paragraph breaks for prose. Defaults to TRUE.

- n:

  The number of words per row. By default, `load_texts()` unnests a text
  one word at a time using a column called `word`. When `n` is a value
  greater than 1, `load_texts()` will instead use
  [`tidytext::unnest_tokens()`](https://juliasilge.github.io/tidytext/reference/unnest_tokens.html)
  with `token = "ngrams"` to create a column called `ngram`.

- ...:

  Additional arguments passed along to
  [`tidytext::unnest_tokens()`](https://juliasilge.github.io/tidytext/reference/unnest_tokens.html)
  for use with `tokenizers`

## Value

A data frame with two to five columns and one row for each token
(optionally, one row for each paragraph or one row for each line) in the
corpus.

## Examples

``` r
if (FALSE) { # \dontrun{
mysteries <-
  load_texts("mystery-novels")

dickinson <-
  load_texts("dickinson-poems",
             poetry = TRUE)

# `load_texts()` can also be used with
# a traditional tidytext workflow:
mysteries <-
  load_texts("mystery-novels",
             word = FALSE,
             to_lower = FALSE) |>
  tidytext::unnest_tokens(word, text)
} # }
```
