# Load (or cache and load) a topic model

`load_topic_model()` checks to see whether a cached topic model exists
before creating one with
[`make_topic_model()`](https://jmclawson.github.io/tmtyro/reference/make_topic_model.md)
and caching it.

## Usage

``` r
load_topic_model(
  data,
  k = 15,
  by = doc_id,
  sample_size = 1000,
  lda_name = NULL
)
```

## Arguments

- data:

  A data frame with nested text in a "text" column.

- k:

  The number of topics to search for. By default, 15 topics will be
  sought.

- by:

  The column for identifying each document. By default, the "title"
  column will be used.

- sample_size:

  The sample size for each document chunk. By default, samples will
  include 1000 words.

- lda_name:

  The name to use when looking caching the model in the "data" folder.
  By default, this value is derived from the name of the data frame
  passed into the function, but it may be helpful to define it
  explicitly if there are intervening steps before loading the topic
  model.

## Value

A topic model.

## Examples

``` r
if (FALSE) { # \dontrun{
mysteries <- load_texts("mystery-novels", word = FALSE)

mysteries_lda <- mysteries |>
  load_topic_model(k = 10)
  } # }
```
