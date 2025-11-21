# Explore topics interactively

`interactive_topic_distributions()` uses plotly to prepare an
interactive visualization to explore a topic model, showing the top "n"
topics in each document. This kind of visualization is for use in the
interactive IDE or as a web page.

## Usage

``` r
interactive_topic_distributions(
  lda,
  top_n = 4,
  title = FALSE,
  height = NULL,
  omit = NULL,
  smooth = TRUE
)
```

## Arguments

- lda:

  The topic model to be used.

- top_n:

  The number of topics to visualize. By default, the top 4 topics in
  each document will be shown.

- title:

  By default, the function will add a title to the chart, corresponding
  to the name of the object passed to the `lda` parameter. Set it to
  FALSE to return a chart with no title.

- height:

  The height of the resulting HTML widget.

- omit:

  Upon exploration, some topics may be found to contain common stop
  words or other unhelpful material. Use the `omit` parameter to define
  a vector of topic numbers you wish to omit from a visualization.

- smooth:

  After samples are rejoined, the measured value of each topic will vary
  wildly, even in samples that are beside each other in a document. This
  can make charts distractingly jittery. The default TRUE value of this
  parameter reduces chart noise by calculating rolling averages across
  three samples. Set the parameter to FALSE to skip this step and allow
  for visualization of extreme values.

## Value

Interactive plotly object

## Examples

    austen <-
      get_gutenberg_corpus(c(105, 121, 141, 158, 161, 946, 1342)) |>
      dplyr::select(doc_id = title, text)

    austen_lda <-
      austen |>
      make_topic_model(k = 30)

    interactive_topic_distributions(austen_lda)
