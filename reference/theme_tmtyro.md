# Apply a smart tmtyro theme

Apply a smart tmtyro theme

## Usage

``` r
theme_tmtyro(
  minor = FALSE,
  grid_x = NULL,
  grid_y = NULL,
  base_theme = ggplot2::theme_minimal,
  ...
)
```

## Arguments

- minor:

  Whether to show minor grid lines

- grid_x:

  Whether to show grid lines on the X-axis. When `NULL`, this will be
  determined by the data being visualized.

- grid_y:

  Whether to show grid lines on the Y-axis. When `NULL`, this will be
  determined by the data being visualized.

- base_theme:

  The base ggplot2 theme used as a starting point

- ...:

  Additional arguments passed to base theme

## Value

a ggplot2 object

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
[`plot_topic_wordcloud()`](https://jmclawson.github.io/tmtyro/reference/plot_topic_wordcloud.md),
[`plot_ttr()`](https://jmclawson.github.io/tmtyro/reference/plot_ttr.md),
[`plot_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/plot_vocabulary.md),
[`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)

## Examples

``` r
if (FALSE) {
library(ggplot2)

mtcars |>
  ggplot(aes(disp, hp)) +
  geom_point() +
  theme_tmtyro()

mtcars |>
  ggplot(aes(hp, factor(cyl))) +
  geom_point() +
  theme_tmtyro()

mtcars |>
  ggplot(aes(factor(cyl), hp)) +
  geom_point() +
  theme_tmtyro()
}
```
