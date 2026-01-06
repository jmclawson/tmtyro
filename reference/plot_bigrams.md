# Visualize bigram chains

Visualize bigram chains

## Usage

``` r
plot_bigrams(
  data,
  feature = word,
  random_seed = TRUE,
  set_seed = NULL,
  legend = FALSE,
  top_n = 35
)
```

## Arguments

- data:

  A tidy data frame potentially containing a column called "word" or
  columns called "word_1" and "word_2".

- feature:

  The feature to use when constructing ngrams

- random_seed:

  Whether to randomize the creation of the network chart.

- set_seed:

  A specific seed to use if not random

- legend:

  Whether to show a legend for the edge color

- top_n:

  The number of pairs to visualize

## Value

A ggplot2 object

## See also

Other visualizing helpers:
[`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md),
[`italicize_titles()`](https://jmclawson.github.io/tmtyro/reference/italicize_titles.md),
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
[`theme_tmtyro()`](https://jmclawson.github.io/tmtyro/reference/theme_tmtyro.md),
[`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)

Other n-gram helpers:
[`add_ngrams()`](https://jmclawson.github.io/tmtyro/reference/add_ngrams.md),
[`combine_ngrams()`](https://jmclawson.github.io/tmtyro/reference/combine_ngrams.md),
[`separate_ngrams()`](https://jmclawson.github.io/tmtyro/reference/separate_ngrams.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# It isn't necessary to use add_ngrams()
data |>
  plot_bigrams()

# Adding them first allows for filtering steps
data |>
  add_ngrams() |>
  drop_stopwords(word_1) |>
  drop_stopwords(word_2) |>
  plot_bigrams()

# Only bigrams influence the visualization These show the same networks:
data |>
  add_ngrams() |>
  plot_bigrams()

data |>
  add_ngrams(4) |>
  plot_bigrams()

dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
  plot_bigrams()

# Loading `ggraph` enables edge to show connection strengths
library(ggraph)

dubliners |>
  plot_bigrams()

dubliners |>
  add_ngrams(2) |>
  drop_stopwords(feature = word_1) |>
  drop_stopwords(feature = word_2) |>
  plot_bigrams()

dubliners |>
  dplyr::filter(doc_id == "The Dead") |>
  plot_bigrams(top_n = 70) |>
  change_colors(c("black", "orange"))
} # }
```
