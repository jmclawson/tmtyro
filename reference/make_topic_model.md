# Construct a topic model

`make_topic_model()` moves a table of texts through the necessary steps
of preparation before building a topic model. The function applies seven
steps:

1.  identifies text divisions by the `doc_id` column

2.  divides each of the texts into same-sized chunks of `sample_size`
    words (default is 1000 words)

3.  unnests text table into a table with one word per row

4.  removes stop words and proper nouns (identified as any word that
    only appears with a capitalized first letter)

5.  counts word frequencies for each chunk

6.  converts the table of frequencies into a document term matrix

7.  builds a topic model with `k` topics

## Usage

``` r
make_topic_model(data, by = doc_id, sample_size = 1000, k = 15, cache = TRUE)
```

## Arguments

- data:

  A data frame with nested text in a "text" column.

- by:

  The column for identifying each document. By default, the "title"
  column will be used.

- sample_size:

  The sample size for each document chunk. By default, samples will
  include 1000 words.

- k:

  The number of topics to search for. By default, 15 topics will be
  sought.

- cache:

  Whether to cache the resulting model as an RDS file in the "data/"
  folder. Set to `TRUE` by default. Delete this RDS file to create a new
  model.

## Value

A topic model.

## Examples

``` r
if (FALSE) { # \dontrun{
mysteries <- load_texts("mystery-novels", word = FALSE)

mysteries_lda <- mysteries |>
  make_topic_model(k = 10)
  } # }
```
