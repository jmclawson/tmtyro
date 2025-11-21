# Show vocabulary growth

`plot_vocabulary()` visualizes the vocabulary growth as new words are
used in each document.

## Usage

``` r
plot_vocabulary(
  df,
  x = progress_words,
  by = doc_id,
  identity = NULL,
  descriptive_labels = TRUE,
  labeling = c("point", "inset", "inline", "axis")
)
```

## Arguments

- df:

  A tidy data frame, potentially containing columns called "doc_id" and
  "word"

- x:

  A column showing the cumulative count of words

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

  - `"axis"` prints labels where a secondary Y-axis might go

  - `"inset"` prints a legend within the plot area

  - Anything else prints a legend to the right of the plot area.

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
[`plot_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/plot_tf_idf.md),
[`plot_topic_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_bars.md),
[`plot_topic_distributions()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_distributions.md),
[`plot_topic_wordcloud()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_wordcloud.md),
[`plot_ttr()`](https://jmclawson.github.io/tmtyro/reference/plot_ttr.md),
[`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners_measured <- dubliners |>
  add_vocabulary()

dubliners_measured |>
  plot_vocabulary(progress_percent)
#> Warning: `guide_axis_truncated()` was deprecated in ggh4x 0.3.0.
#> ℹ Please use `ggplot2::guide_axis(theme)` instead.
#> ℹ The deprecated feature was likely used in the tmtyro package.
#>   Please report the issue at <https://github.com/jmclawson/tmtyro/issues>.


dubliners_measured |>
  plot_vocabulary()


if (FALSE) { # \dontrun{
  get_micusp_corpus(
    discipline %in% c("Physics", "Economics")) |>
    load_texts() |>
    add_vocabulary() |>
    plot_vocabulary(by = discipline)
} # }
```
