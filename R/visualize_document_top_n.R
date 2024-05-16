#' Plot a heatmap of ranked features
#'
#' @param df A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param num The number of ranks to show, not counting ties
#' @param by The column used for document grouping, with doc_id as the default
#' @param feature The column to measure, as in "word" or "lemma"
#' @param label Whether to show the rank as a label in the heatmap
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
#'   plot_doc_word_heatmap(feature = lemma)
#'
plot_doc_word_heatmap <- function(
    df,
    num = 10,
    by = doc_id,
    feature = word,
    label = TRUE){
  the_df <- df |>
    dplyr::count({{ by }}, {{ feature }}) |>
    dplyr::ungroup() |>
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
        color = "gray",
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
                         color = "gray")
  }

  the_plot <- the_plot +
    ggplot2::scale_fill_viridis_c(
      option = "viridis",
      na.value = "white") +
    ggplot2::scale_color_manual(values = c("white", "black"))

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

#' @importFrom dplyr count
#' @export
dplyr::count

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
    na_rm = TRUE
){
  precision <- label_tweak + 1
  offset <- label_tweak + 2

  if (!"n" %in% colnames(df)) {
    df <- df |>
      dplyr::count({{ by }}, {{ feature }}) |>
      dplyr::ungroup()
  }

  if (percents) {
    df <- df |>
      dplyr::ungroup() |>
      dplyr::mutate(
        n = n / sum(n),
        .by = {{ by }})
  }

  if (na_rm) {
    df <- df |>
      tidyr::drop_na({{ feature }})
  }

  df <- df |>
    dplyr::ungroup() |>
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

  the_plot <- df |>
    internal_plot_word_bars(
      n, rlang::enquo(by), rlang::enquo(feature), percents, label, label_tweak, label_inside)

  if (length(unique(df[[deparse(substitute(by))]])) > 1) {
    if (percents) {
      the_plot <- the_plot +
        ggplot2::facet_wrap(ggplot2::vars({{ by }}),
                            scales = "free_y")
    } else {
      the_plot <- the_plot +
        ggplot2::facet_wrap(ggplot2::vars({{ by }}),
                            scales = "free")
    }
  }

  the_plot +
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
    label_inside
){
  precision <- label_tweak + 1
  offset <- label_tweak + 2

  the_plot <- df |>
    ggplot2::ggplot(ggplot2::aes(x = {{ x_value }},
                                 y = !! feature,
                                 fill = !! by)) +
    ggplot2::geom_col(
      show.legend = FALSE,
      color = NA)

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
        expand = ggplot2::expansion(mult = c(0, 0.05)))

    if (length(unique(df[[deparse(substitute(by))]])) > 1) {
      the_plot <- the_plot +
        ggplot2::facet_wrap(ggplot2::vars({{ by }}),
                            scales = "free_y")
    }

  } else {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(),
        expand = ggplot2::expansion(mult = c(0, 0.05)))

    if (length(unique(df[[deparse(substitute(by))]])) > 1) {
      the_plot <- the_plot +
        ggplot2::facet_wrap(ggplot2::vars({{ by }}),
                            scales = "free")
    }
  }

  the_plot <- the_plot +
    ggplot2::labs(y = NULL)

  the_plot +
    tidytext::scale_y_reordered() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank())
}
