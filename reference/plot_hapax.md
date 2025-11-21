# Project hapax legomena onto vocabulary growth

`plot_hapax()` visualizes a sampling of hapax legomena projected on
faceted curves of vocabulary growth over time

## Usage

``` r
plot_hapax(
  df,
  prop = 0.01,
  x = progress_words,
  y = vocabulary,
  by = doc_id,
  descriptive_labels = TRUE,
  feature = hapax
)
```

## Arguments

- df:

  A tidy data frame, potentially containing columns called "doc_id" and
  "word"

- prop:

  The proportion of hapax to sample. The chart can become illegible with
  proportions over ~1%

- x:

  The progress column to show. Default option is progress_percent, but
  progress_words is also appropriate.

- y:

  The Y-axis variable to chart. Default value is the cumulative
  vocabulary size.

- by:

  A grouping column, such as doc_id

- descriptive_labels:

  A toggle for disabling descriptive labels of progress_percent on the
  X-axis

- feature:

  The column to check for new features. Defaults to `hapax`, but the
  function might also be used with `new_word` instead to plot a sample
  of new additions to documents' vocabularies.

## Value

A ggplot object

## See also

Other visualizing helpers:
[`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md),
[`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md),
[`plot_doc_word_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_bars.md),
[`plot_doc_word_heatmap()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_heatmap.md),
[`plot_hir()`](https://jmclawson.github.io/tmtyro/reference/plot_hir.md),
[`plot_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/plot_tf_idf.md),
[`plot_topic_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_bars.md),
[`plot_topic_distributions()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_distributions.md),
[`plot_topic_wordcloud()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_wordcloud.md),
[`plot_ttr()`](https://jmclawson.github.io/tmtyro/reference/plot_ttr.md),
[`plot_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/plot_vocabulary.md),
[`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)

## Examples

``` r
if (FALSE) {
  dubliners <- get_gutenberg_corpus(2814) |>
    load_texts() |>
    identify_by(part) |>
    standardize_titles()

  dubliners_measured <- dubliners |>
    add_vocabulary()

  dubliners_measured |>
    plot_hapax()
}
```
