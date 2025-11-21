# Visualize the top terms by tf-idf

Visualize the top terms by tf-idf

## Usage

``` r
plot_tf_idf(
  df,
  rows = 1:10,
  by = doc_id,
  feature = word,
  label = FALSE,
  label_tweak = 2,
  label_inside = FALSE
)
```

## Arguments

- df:

  A tidy data frame, potentially containing columns called "doc_id" and
  "word"

- rows:

  The rows of terms to chart in each document

- by:

  A column containing document grouping

- feature:

  A column containing the terms to be measured across document groupings

- label:

  Not yet working

- label_tweak:

  Not yet working

- label_inside:

  Not yet working

## Value

A ggplot object

## See also

Other visualizing helpers:
[`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md),
[`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md),
[`plot_doc_word_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_bars.md),
[`plot_doc_word_heatmap()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_heatmap.md),
[`plot_hapax()`](https://jmclawson.github.io/tmtyro/reference/plot_hapax.md),
[`plot_hir()`](https://jmclawson.github.io/tmtyro/reference/plot_hir.md),
[`plot_topic_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_bars.md),
[`plot_topic_distributions()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_distributions.md),
[`plot_topic_wordcloud()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_wordcloud.md),
[`plot_ttr()`](https://jmclawson.github.io/tmtyro/reference/plot_ttr.md),
[`plot_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/plot_vocabulary.md),
[`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  plot_tf_idf()
```
