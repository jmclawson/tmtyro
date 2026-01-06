# Plot a heatmap of ranked features

Plot a heatmap of ranked features

## Usage

``` r
plot_doc_word_heatmap(
  data,
  rows = 1:10,
  by = doc_id,
  feature = word,
  label = TRUE
)
```

## Arguments

- data:

  A tidy data frame, potentially containing columns called "doc_id" and
  "word"

- rows:

  The ranks to show, not counting ties

- by:

  The column used for document grouping, with doc_id as the default

- feature:

  The column to measure, as in "word" or "lemma"

- label:

  Whether to show the rank as a label in the heatmap

## Value

A ggplot object

## See also

Other visualizing helpers:
[`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md),
[`italicize_titles()`](https://jmclawson.github.io/tmtyro/reference/italicize_titles.md),
[`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md),
[`plot_doc_word_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_bars.md),
[`plot_hapax()`](https://jmclawson.github.io/tmtyro/reference/plot_hapax.md),
[`plot_hir()`](https://jmclawson.github.io/tmtyro/reference/plot_hir.md),
[`plot_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/plot_tf_idf.md),
[`plot_topic_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_bars.md),
[`plot_topic_distributions()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_distributions.md),
[`plot_topic_wordcloud()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_wordcloud.md),
[`plot_ttr()`](https://jmclawson.github.io/tmtyro/reference/plot_ttr.md),
[`plot_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/plot_vocabulary.md),
[`theme_tmtyro()`](https://jmclawson.github.io/tmtyro/reference/theme_tmtyro.md),
[`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts(lemma = TRUE) |>
  identify_by(part) |>
  standardize_titles()

# Make a smaller example
selected_titles <-
  c("The Sisters", "An Encounter", "Araby",
    "Counterparts", "The Dead")

dubliners |>
  dplyr::filter(doc_id %in% selected_titles) |>
  plot_doc_word_heatmap()

dubliners |>
  dplyr::filter(doc_id %in% selected_titles) |>
  plot_doc_word_heatmap(feature = lemma, rows = 1:6)
} # }
```
