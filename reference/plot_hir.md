# Show hapax introduction ratio over time

Show hapax introduction ratio over time

## Usage

``` r
plot_hir(
  data,
  x = progress,
  by = doc_id,
  identity = doc_id,
  descriptive_labels = TRUE,
  labeling = c("point", "inline", "inset"),
  log_y = TRUE
)
```

## Arguments

- data:

  A tidy data frame, potentially containing a column called "doc_id" and
  "word"

- x:

  A column showing the cumulative progress of documents

- by:

  A grouping column for colors and labels

- identity:

  A grouping column for lines

- descriptive_labels:

  A toggle for disabling descriptive labels of progress_percent on the
  X-axis

- labeling:

  Options for labeling groups:

  - `"point"` labels the final value

  - `"inline"` prints the label within a smoothed curve

  - `"inset"` prints a legend within the plot area

  - Anything else prints a legend to the right of the plot area.

- log_y:

  A toggle for logarithmic scaling to the Y-axis; defaults to TRUE

## Value

A ggplot object

## See also

Other visualizing helpers:
[`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md),
[`italicize_titles()`](https://jmclawson.github.io/tmtyro/reference/italicize_titles.md),
[`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md),
[`plot_doc_word_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_bars.md),
[`plot_doc_word_heatmap()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_heatmap.md),
[`plot_hapax()`](https://jmclawson.github.io/tmtyro/reference/plot_hapax.md),
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
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners_measured <- dubliners |>
  add_vocabulary()

dubliners_measured |>
  standardize_titles() |>
  plot_hir()
} # }
```
