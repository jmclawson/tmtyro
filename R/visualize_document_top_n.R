#' Plot a heatmap of ranked features
#'
#' @param df A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param num The number of ranks to show, not counting ties
#' @param by The column used for document grouping, with doc_id as the default
#' @param feature The column to measure, as in "word" or "lemma"
#' @param label Whether to show the rank as a label in the heatmap
#' @param na_color The color to show NA values; default is "white"
#' @param colorset The viridis color palette to use; alternatively, a two-item vector of colors to use as gradient
#' @param line_color The color of grid lines in the heatmap
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' library(tmtyro)
#' library(dplyr)
#'
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   filter(pos %in% c("NN", "NNS")) |>
#'   plot_doc_word_heatmap()
#'
#' austen |>
#'   filter(pos %in% c("JJ", "RB")) |>
#'   plot_doc_word_heatmap(feature = lemma, colorset = c("white", "red"))
#'
plot_doc_word_heatmap <- function(
    df,
    num = 10,
    by = doc_id,
    feature = word,
    label = TRUE,
    na_color = "white",
    colorset = "viridis",
    line_color = "gray"){
  the_df <- df |>
    dplyr::count({{ by }}, {{ feature }}) |>
    dplyr::slice_max(
      n = num,
      by = {{ by }},
      order_by = n) |>
    dplyr::group_by({{ by }}) |>
    dplyr::arrange(n) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::mutate(true_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by({{ feature }}) |>
    dplyr::mutate(ranksum = -sum(rank)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(ranksum)) |>
    dplyr::mutate(
      {{ feature }} := forcats::fct_inorder({{ feature }},
                                ordered = TRUE)) |>
    dplyr::group_by({{ by }}) |>
    dplyr::mutate(docranksum = -sum(ranksum)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(docranksum)) |>
    dplyr::mutate(
      {{ by }} := forcats::fct_inorder({{ by }},
                              ordered = TRUE),
      true_rank = factor(true_rank))

  feature_order <- the_df[[deparse(substitute(feature))]] |> levels()
  doc_order <- the_df[[deparse(substitute(by))]] |> unique()

  the_df <- the_df |>
    dplyr::select({{ by }}, {{ feature }}, true_rank) |>
    tidyr::pivot_wider(names_from = {{ by }},
                values_from = true_rank) |>
    tidyr::pivot_longer(
      cols = -c({{ feature }}),
      names_to = deparse(substitute(by)),
      values_to = "true_rank") |>
    dplyr::mutate(
      {{ by }} := factor({{ by }}, levels = doc_order,
                         ordered = TRUE),
      {{ feature }} := factor({{ feature }}, levels = feature_order), ordered = TRUE)

  max_rank <- the_df |>
    tidyr::drop_na(true_rank) |>
    dplyr::pull(true_rank) |>
    as.character() |>
    as.numeric() |>
    max()

  the_plot <- the_df |>
    ggplot2::ggplot(ggplot2::aes(x = {{ by }},
               y = {{ feature }}))

  if(label){
    the_plot <- the_plot +
      ggplot2::geom_tile(
        data = the_df,
        ggplot2::aes(fill = true_rank |>
              as.character() |>
              as.numeric()),
        color = line_color,
        show.legend = FALSE) +
      ggplot2::geom_text(
        data = ~ dplyr::filter(.x, !is.na(true_rank)),
        ggplot2::aes(label = true_rank,
            color = as.numeric(as.character(true_rank)) > (max_rank/2)),
        show.legend = FALSE
        )
  } else {
    the_plot <- the_plot +
      ggplot2::geom_tile(ggplot2::aes(fill = true_rank),
                         color = line_color)
  }

    viridis <- c(
      LETTERS[1:8],
      "magma",
      "inferno",
      "plasma",
      "viridis",
      "cividis",
      "rocket",
      "mako",
      "turbo")
    brewer <- c(
      1:18,
      "BrBG",
      "PiYG",
      "PRGn",
      "PuOr",
      "RdBu",
      "RdGy",
      "RdYlBu",
      "RdYlGn",
      "Spectral",
      "Accent",
      "Dark2",
      "Paired",
      "Pastel1",
      "Pastel2",
      "Set1",
      "Set2",
      "Set3",
      "Blues",
      "BuGn",
      "BuPu",
      "GnBu",
      "Greens",
      "Greys",
      "Oranges",
      "OrRd",
      "PuBu",
      "PuBuGn",
      "PuRd",
      "Purples",
      "RdPu",
      "Reds",
      "YlGn",
      "YlGnBu",
      "YlOrBr",
      "YlOrRd")

    if(length(colorset) == 2) {
      if(colorset[2] == "white"){
        text_low <- "black"
        text_high <- "white"
      } else {
        text_low <- "white"
        text_high <- "black"
      }
      the_plot <- the_plot +
        ggplot2::scale_fill_gradient(na.value = na_color,
                            low = colorset[2],
                            high = colorset[1]) +
        ggplot2::scale_color_manual(values = c(text_low, text_high))
  } else if (length(colorset) == 1 && colorset %in% viridis) {
    the_plot <- the_plot +
      ggplot2::scale_fill_viridis_c(
        option = colorset,
        na.value = na_color) +
      ggplot2::scale_color_manual(values = c("white", "black"))
  } else if (length(colorset) == 1 && colorset %in% brewer) {
    the_plot <- the_plot +
      ggplot2::scale_fill_distiller(
        type = "seq",
        palette = colorset,
        na.value = na_color) +
      ggplot2::scale_color_manual(values = c("white", "black"))
  } else {
    the_plot <- the_plot +
      ggplot2::scale_color_manual(values = c("white", "black"))
  }

  the_plot <- the_plot +
    ggplot2::scale_x_discrete(position = "top",
                     expand = c(0,0)) +
    ggplot2::labs(x = NULL, fill = "rank") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0),
          panel.grid = ggplot2::element_blank())

  if (label) {
    the_plot +
      ggplot2::theme(legend.position = "none")
  } else {
    the_plot +
      ggplot2::guides(fill = ggplot2::guide_legend(keywidth = 0.8,
                                 keyheight = 0.8))
  }
}

#' Plot bar graphs of frequent features
#'
#' @param df A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param num The number of features to show
#' @param by The column used for document grouping, with doc_id as the default
#' @param feature The column to measure, as in "word" or "lemma"
#' @param percents Whether to display word frequencies as percentage instead of raw counts; defaults to TRUE
#' @param label Whether to show the value as a label with each bar; defaults to FALSE
#' @param label_tweak The numeric value by which to tweak the label, if shown. For percentages, this value adjusts the decimal-point precision. For raw counts, this value adjusts labels' offset from the bars
#' @param label_inside Whether to show the value as a label inside each bar; defaults to FALSE
#' @param colorset The color palette to use, whether "default", "okabe", or one of the named qualitative palettes from Viridis or Color Brewer
#' @param outline_color The color to use for the outside of each bar. By default, no color is used.
#' @param na_rm Whether to drop empty features
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' library(tmtyro)
#' library(dplyr)
#'
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'   plot_doc_word_bars()
#'
#' austen |>
#'   mutate(
#'     pos_pair = paste(pos, lead(pos)),
#'     `adjective + noun bigram` = paste(word, lead(word))) |>
#'   filter(stringr::str_detect(pos_pair, "JJ N")) |>
#'   standardize_titles() |>
#'   plot_doc_word_bars(
#'     num = 5,
#'     feature = `adjective + noun bigram`,
#'     colorset = "Pastel2",
#'     percents = FALSE,
#'     label = TRUE,
#'     label_inside = TRUE,
#'     label_tweak = -1)
plot_doc_word_bars <- function(
    df,
    num = 10,
    by = doc_id,
    feature = word,
    percents = TRUE,
    label = FALSE,
    label_tweak = 2,
    label_inside = FALSE,
    colorset = "default",
    outline_color = NA,
    na_rm = TRUE
){
  precision <- label_tweak + 1
  offset <- label_tweak + 2

  df <- df |>
    dplyr::count({{ by }}, {{ feature }})

  if (percents) {
    df <- df |>
      dplyr::mutate(
        n = n / sum(n),
        .by = {{ by }})
  }

  if (na_rm) {
    df <- df |>
      tidyr::drop_na({{ feature }})
  }

  df <- df |>
    dplyr::slice_max(
      order_by = n,
      n = num,
      by = {{ by }}) |>
    dplyr::group_by({{ by }}) |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::mutate(
      {{ feature }} := tidytext::reorder_within(
        {{ feature }},
        by = n,
        within = {{ by }}))

  prefix <- deparse(substitute(feature))

  if (percents) {
    x_lab <- paste(prefix, "frequency")
  } else {
    x_lab <- paste(prefix, "count")
  }

  df |>
    internal_plot_word_bars(
      n, rlang::enquo(by), rlang::enquo(feature), percents, label, label_tweak, label_inside, colorset, outline_color) +
    ggplot2::labs(x = x_lab)
}

internal_plot_word_bars <- function(
    df,
    x_value,
    by,
    feature,
    percents,
    label,
    label_tweak,
    label_inside,
    colorset,
    outline_color
){
  precision <- label_tweak + 1
  offset <- label_tweak + 2

  the_plot <- df |>
    ggplot2::ggplot(ggplot2::aes(x = {{ x_value }},
                                 y = !! feature,
                                 fill = !! by)) +
    ggplot2::geom_col(
      show.legend = FALSE,
      color = outline_color)

  if (label & percents & !label_inside) {
    the_plot <- the_plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = n + 0.0005,
          label = 100 *
            round(n,
                  precision)),
        hjust = 0)
  } else if (label & percents & label_inside) {
    the_plot <- the_plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = n - 0.0005,
          label = 100 *
            round(n,
                  precision)),
        hjust = 1)
  } else if (label & !label_inside) {
    the_plot <- the_plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = n + offset,
          label = n),
        hjust = 0)
  } else if (label & label_inside) {
    the_plot <- the_plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = n - offset,
          label = n),
        hjust = 1)
  }

  if (percents) {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(
        labels = scales::label_percent(),
        expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::facet_wrap(ggplot2::vars({{ by }}),
                          scales = "free_y") +
      ggplot2::labs(#x = stringr::str_glue("{deparse(substitute(feature))} frequency"),
                    y = NULL)
  } else {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(),
        expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::facet_wrap(ggplot2::vars({{ by }}),
                          scales = "free") +
      ggplot2::labs(#x = stringr::str_glue("{deparse(substitute(feature))} count"),
                    y = NULL)
  }

  viridis <- c(
    LETTERS[1:8],
    "magma",
    "inferno",
    "plasma",
    "viridis",
    "cividis",
    "rocket",
    "mako",
    "turbo")
  brewer <- c(
    1:8,
    "BrBG",
    "PiYG",
    "PRGn",
    "PuOr",
    "RdBu",
    "RdGy",
    "RdYlBu",
    "RdYlGn",
    "Spectral",
    "Accent",
    "Dark2",
    "Paired",
    "Pastel1",
    "Pastel2",
    "Set1",
    "Set2",
    "Set3",
    "Blues",
    "BuGn",
    "BuPu",
    "GnBu",
    "Greens",
    "Greys",
    "Oranges",
    "OrRd",
    "PuBu",
    "PuBuGn",
    "PuRd",
    "Purples",
    "RdPu",
    "Reds",
    "YlGn",
    "YlGnBu",
    "YlOrBr",
    "YlOrRd")
  if (length(colorset) == 1 && colorset %in% viridis) {
    the_plot <- the_plot +
      ggplot2::scale_fill_viridis_d(
        option = colorset)
  } else if (length(colorset) == 1 && colorset %in% brewer) {
    the_plot <- the_plot +
      ggplot2::scale_fill_brewer(
        type = "qual",
        palette = colorset)
  } else if (length(colorset) == 1 && stringr::str_detect(colorset, "okabe")) {
    the_plot <- the_plot +
      ggplot2::scale_fill_manual(
        values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")
      )
  }

  the_plot +
    tidytext::scale_y_reordered() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank())
}
