# Choose other colors

`change_colors()` standardizes three methods for choosing color palettes
for color or fill mapping, providing access to Brewer and Viridis
palettes alongside custom choices.

## Usage

``` r
change_colors(
  x,
  colorset = "brewer",
  palette = 2,
  kind = "qualitative",
  direction = 1,
  start = 1
)
```

## Arguments

- x:

  A visualization made with
  [`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)

- colorset:

  Either "brewer", "viridis", "okabe-ito", "dubois", or a vector of
  colors.

- palette:

  The number or name of palette (dependent on setting `colorset` to
  either "brewer" or "viridis")

- kind:

  Used only for Brewer palettes to match numbered palette with the
  specific subset

- direction:

  The direction colors should be applied to the data. Setting to
  anything other than 1 will reverse the order.

- start:

  Useful for predefined colorsets from Okabe-Ito and Brewer to start
  from a color other than 1

## Value

A ggplot2 object

## See also

Other visualizing helpers:
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
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

# Too many titles for categorical data.
selected_titles <- dubliners$doc_id |>
  {\(x) x[grepl("^The |^A |^An ", x)]}() |>
  unique()

dubliners2 <- dubliners |>
  dplyr::filter(doc_id %in% selected_titles)

### CATEGORICAL DATA ##

# By default, ggplot2's palette is applied
dubliners2 |>
  visualize()


# change_color() starts with Brewer's "Dark2" palette
dubliners2 |>
  visualize() |>
  change_colors()


# Other color sets and palettes can be chosen
dubliners2 |>
  visualize() |>
  change_colors(colorset = "okabe")


dubliners2 |>
  visualize() |>
  change_colors(colorset = "viridis", palette = "turbo")


dubliners2 |>
  visualize() |>
  change_colors(colorset = "brewer", palette = "Set1")


# Named cases can be highlighted
dubliners2 |>
  visualize(inorder = FALSE) |>
  change_colors(c(
    rep("darkgray", 6),
    "A Painful Case" = "blue"))


### SEQUENTIAL DATA ###

# By default, the "viridis" palette is applied
dubliners2 |>
  visualize(type = "heatmap")


# change_colors()  starts with Brewer's "BuGn" palette
dubliners2 |>
  visualize(type = "heatmap") |>
  change_colors()
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.


# Palettes can be numbered or named
dubliners2 |>
  visualize(type = "heatmap") |>
  change_colors("viridis", palette = 6)
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.


dubliners2 |>
  visualize(type = "heatmap") |>
  change_colors("viridis", palette = "mako")
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.


### N-GRAMS ###
library(ggraph)
#> Loading required package: ggplot2

dubliners |>
  add_ngrams() |>
  visualize() |>
  change_colors(c("#444488","orange"))
#> Scale for edge_colour is already present.
#> Adding another scale for edge_colour, which will replace the existing scale.
```
