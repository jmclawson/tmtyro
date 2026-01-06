# Plot topic wordclouds

`plot_topic_wordcloud()` prepares, saves, and displays word clouds of
topics in a topic model. The function can display word clouds of one or
more specific topics, or it can show word clouds for every topic.

## Usage

``` r
plot_topic_wordcloud(lda, topics = NULL, crop = TRUE, savedir = "plots")
```

## Arguments

- lda:

  The topic model to be used.

- topics:

  Topic numbers to be visualized. If left undefined, all topics will be
  visualized

- crop:

  Whether to remove white space from visualized word clouds

- savedir:

  The directory to save plots in. Defaults to "plots"

## Value

Graphic(s) prepared with `knitr` for Quarto or RMarkdown

## Examples

    austen <-
      get_gutenberg_corpus(c(105, 121, 141, 158, 161, 946, 1342)) |>
      dplyr::select(doc_id = title, text)

    austen_lda <-
      austen |>
      make_topic_model(k = 30)

    austen_lda |>
      plot_topic_wordcloud(topic = 6)

![](figures/topic_cloud.png)

## See also

Other visualizing helpers:
[`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md),
[`italicize_titles()`](https://jmclawson.github.io/tmtyro/reference/italicize_titles.md),
[`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md),
[`plot_doc_word_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_bars.md),
[`plot_doc_word_heatmap()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_heatmap.md),
[`plot_hapax()`](https://jmclawson.github.io/tmtyro/reference/plot_hapax.md),
[`plot_hir()`](https://jmclawson.github.io/tmtyro/reference/plot_hir.md),
[`plot_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/plot_tf_idf.md),
[`plot_topic_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_bars.md),
[`plot_topic_distributions()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_distributions.md),
[`plot_ttr()`](https://jmclawson.github.io/tmtyro/reference/plot_ttr.md),
[`plot_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/plot_vocabulary.md),
[`theme_tmtyro()`](https://jmclawson.github.io/tmtyro/reference/theme_tmtyro.md),
[`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)
