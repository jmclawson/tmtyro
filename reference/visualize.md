# Visualize output

`visualize()` provides a simple method for displaying results. Based on
previous functions used, `visualize()` will choose a method, resolving
to one of the other visualizing helpers.

## Usage

``` r
visualize(data, ...)
```

## Arguments

- data:

  data processed with one or more functions from `tmtyro`

- ...:

  Arguments passed on to
  [`plot_doc_word_bars`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_bars.md),
  [`plot_bigrams`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md),
  [`plot_vocabulary`](https://jmclawson.github.io/tmtyro/reference/plot_vocabulary.md),
  [`plot_ttr`](https://jmclawson.github.io/tmtyro/reference/plot_ttr.md),
  [`plot_hir`](https://jmclawson.github.io/tmtyro/reference/plot_hir.md),
  [`plot_topic_distributions`](https://jmclawson.github.io/tmtyro/reference/plot_topic_distributions.md),
  [`plot_topic_bars`](https://jmclawson.github.io/tmtyro/reference/plot_topic_bars.md),
  [`plot_topic_wordcloud`](https://jmclawson.github.io/tmtyro/reference/plot_topic_wordcloud.md)

  `rows`

  :   The features to show

  `by`

  :   The column used for document grouping, with doc_id as the default

  `feature`

  :   The column to measure, as in "word" or "lemma"

  `inorder`

  :   Whether to retain the factor order of the "by" column

  `reorder_y`

  :   Whether to reorder the Y-values by facet

  `color_y`

  :   Whether bars should be filled by Y-values

  `allow_y_blanks`

  :   The threshold number of blanks to allow on the Y-axis before
      dropping all blanks and reprinting values for each facet with a
      freely scaled Y-axis

  `percents`

  :   Whether to display word frequencies as percentage instead of raw
      counts

  `label`

  :   Whether to show the value as a label with each bar

  `label_tweak`

  :   The numeric value by which to tweak the label, if shown. For
      percentages, this value adjusts the decimal-point precision. For
      raw counts, this value adjusts labels' offset from the bars

  `label_inside`

  :   Whether to show the value as a label inside each bar

  `na_rm`

  :   Whether to drop empty features

  `random_seed`

  :   Whether to randomize the creation of the network chart.

  `set_seed`

  :   A specific seed to use if not random

  `legend`

  :   Whether to show a legend for the edge color

  `top_n`

  :   The number of pairs to visualize

  `identity`

  :   A grouping column for lines

  `descriptive_labels`

  :   A toggle for using descriptive labels for progress on the X-axis

  `labeling`

  :   Options for labeling groups:

      - `"point"` labels the final value

      - `"inline"` prints the label within a smoothed curve

      - `"inset"` prints a legend within the plot area

      - Anything else prints a legend to the right of the plot area.

  `log_y`

  :   A toggle for logarithmic scaling to the Y-axis; defaults to TRUE

  `topics`

  :   The topic numbers to view

## Value

a ggplot2 object

## Note

For some visualizations, an optional `type` parameter may be helpful to
change the visualization. For example, setting `type = "htr"`,
`type = "ttr"`, or `type = "hapax"` after
[`add_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/add_vocabulary.md)
will emphasize different columns added by that function. Similarly,
`type = "cloud"` or `type = "wordcloud"` will show topic word clouds
after
[`make_topic_model()`](https://jmclawson.github.io/tmtyro/reference/make_topic_model.md),
and `type = "heatmap"` will show an alternative visualization for word
frequencies.

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
[`theme_tmtyro()`](https://jmclawson.github.io/tmtyro/reference/theme_tmtyro.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

# A data frame with `doc_id` and `word` columns will visualize frequency by default
dubliners |>
   visualize()

# Applying `tmtyro` functions will choose an appropriate visualization

dubliners |>
   add_ngrams() |>
   visualize()

dubliners |>
   add_ngrams() |>
   combine_ngrams() |>
   visualize()

dubliners |>
   summarize_tf_idf() |>
   visualize()

dubliners |>
   add_vocabulary() |>
   visualize()

if (FALSE) { # sentiment requires interaction on first load
  dubliners |>
     add_sentiment() |>
     visualize()
}

# Some visualizations are specified with the `type` argument
dubliners |>
   add_vocabulary() |>
   visualize(type = "ttr")

if (FALSE) { # puzzlingly broken for Dubliners, but usually works
dubliners |>
   add_vocabulary() |>
   visualize(type = "hapax")
}

# Other arguments get passed along
dubliners |>
   add_ngrams() |>
   visualize(top_n = 25)

dubliners |>
   add_vocabulary() |>
   visualize(x = progress_percent)
} # }
```
