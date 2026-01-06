# Italicize document titles in a table or figure

Italicize document titles in a table or figure

## Usage

``` r
italicize_titles(x, col)
```

## Arguments

- x:

  A table data object created with
  [`tabulize()`](https://jmclawson.github.io/tmtyro/reference/tabulize.md)
  or a figure created with
  [`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)

- col:

  The column to italicize

## Value

A gt table data object or a ggplot2 figure

## Examples

    library(gt)
    library(tmtyro)

    penguins_gt <-
      penguins |>
      select(-year) |>
      summarize(
        across(
          matches("_len$|_dep$"), mean, na.rm = TRUE),
        .by = c(species, island, sex)) |>
      gt() |>
      fmt_number() |>
      tab_spanner(
        "bill",
        columns = starts_with("bill_")) |>
      tab_spanner(
        "flipper",
        starts_with("flip")) |>
      cols_label(
        bill_len = "length",
        bill_dep = "depth",
        flipper_len = "length") |>
      sub_missing()

    penguins_gt |>
      collapse_rows(species) |>
      collapse_rows(island) |>
      italicize_titles(species)

## See also

Other table helpers:
[`collapse_rows()`](https://jmclawson.github.io/tmtyro/reference/collapse_rows.md),
[`tabulize()`](https://jmclawson.github.io/tmtyro/reference/tabulize.md)

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
[`plot_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/plot_vocabulary.md),
[`theme_tmtyro()`](https://jmclawson.github.io/tmtyro/reference/theme_tmtyro.md),
[`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)
