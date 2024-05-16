internal_plot_engine <- function(
    df,
    x,
    y,
    by = doc_id,
    identity = doc_id,
    descriptive_labels = TRUE,
    labeling = c("point", "inset", "inline", "axis"),
    log_y = FALSE,
    skip_print = FALSE){
  if (is.null(identity)) {
    identity <- by
  }
  x_check <- rlang::as_name(x)
  y_check <- rlang::as_name(y)
  if(length(labeling)>2 & x_check == "progress_percent") {
    labeling <- "axis"
  } else {
    labeling <- labeling[1]
  }

  df <- df |>
    dplyr::mutate(!!by := forcats::fct_reorder2(!!by, !!x, !!y))

  the_plot <- df |>
    ggplot2::ggplot(ggplot2::aes(x = {{ x }},
               y = {{ y }},
               color = {{ by }},
               group = {{ identity }}))

  if (labeling == "inline") {
    rlang::check_installed("geomtextpath", reason = "for inline labeling")
    if (y_check == "htr") {
      the_plot <- the_plot +
        geomtextpath::geom_textline(
          ggplot2::aes(label = {{ by }}),
          straight = TRUE,
          padding = ggplot2::unit(0.18, "inch"),
          hjust = 0.9)
    } else {
      the_plot <- the_plot +
        geomtextpath::geom_textsmooth(
          ggplot2::aes(label = {{ by }}),
          hjust = 0.9,
          method = "gam")
    }
    the_plot <- the_plot +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank())
  } else if (labeling == "point") {
    max_x <- df |>
      dplyr::slice_max(order_by = {{ x }}, n = 1, by = {{ by }}) |>
      dplyr::select({{ by }}, {{ x }}, {{ y }})

    the_plot <- the_plot +
      ggplot2::geom_line() +
      ggplot2::geom_point(data = max_x) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank())

    if (rlang::is_installed("ggrepel")) {
      the_plot <- the_plot +
        ggrepel::geom_label_repel(
          data = max_x,
          ggplot2::aes(label = {{ by }}),
          force_pull = 0.7,
          min.segment.length = 0.3,
          fill = ggplot2::alpha(c("white"),0.5),
          label.size = NA)
    } else {
      the_plot <- the_plot +
        ggplot2::geom_label(
          data = max_x,
          ggplot2::aes(label = {{ by }}),
          fill = ggplot2::alpha(c("white"),0.5),
          label.size = NA)
    }

  } else if (labeling == "axis") {
    sec_y <- df |>
      dplyr::slice_max(order_by = {{ x }}, n = 1, by = {{ by }}) |>
      dplyr::select({{ by }}, {{ y }}) |>
      dplyr::arrange({{ y }}) |>
      # mutate({{ by }} := {{ by }} |> fct_inorder()) |>
      stats::setNames(c("labels", "breaks"))

    the_plot <- the_plot +
      ggplot2::geom_line() +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.ticks.y.right = ggplot2::element_blank(),
            axis.title.y.right = ggplot2::element_blank())
  } else if (labeling == "inset") {
    the_plot <- the_plot +
      ggplot2::geom_line() +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = c(0.73, 0.2),
            legend.background = ggplot2::element_rect(fill = "white", color = "white"),
            panel.grid.minor = ggplot2::element_blank())
  } else {
    the_plot <- the_plot +
      ggplot2::geom_line() +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }

  if (y_check %in% c("vocabulary", "ttr", "htr")){
    y_label <- dplyr::case_when(
      y_check == "vocabulary" ~ "vocabulary (words)",
      y_check == "ttr" ~ "type-token ratio (TTR)",
      y_check == "htr" ~ "hapax-token ratio (HTR)",
    )

    the_plot <- the_plot +
      ggplot2::labs(y = y_label,
           color = NULL)
  }

  if (log_y) {
    if (labeling == "axis") {
      the_plot <- the_plot +
        suppressWarnings(ggplot2::scale_y_continuous(
          trans = suppressWarnings(scales::log10_trans()),
          labels = scales::label_percent(),
          sec.axis = ggplot2::dup_axis(
            breaks = sec_y$breaks,
            labels = sec_y$labels,
            guide = ggh4x::guide_axis_color(
              color = scales::hue_pal(direction = -1)(nrow(sec_y))))))
    } else {
      the_plot <- the_plot +
        suppressWarnings(ggplot2::scale_y_continuous(
          trans = suppressWarnings(scales::log10_trans()),
          labels = scales::label_percent()))
    }
  } else {
    if (labeling == "axis") {
      the_plot <- the_plot +
        ggplot2::scale_y_continuous(
          labels = scales::label_comma(),
          sec.axis = ggplot2::dup_axis(
            breaks = sec_y$breaks,
            labels = sec_y$labels,
            guide = ggh4x::guide_axis_color(
              color = scales::hue_pal(direction = -1)(nrow(sec_y)))))
    } else {
      the_plot <- the_plot +
        ggplot2::scale_y_continuous(labels = scales::label_comma())
    }
  }

  if (x_check == "progress_words") {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(),
        # expand = c(0,0)
      ) +
      ggplot2::labs(x = "text length (words)")
  } else if (x_check == "progress_percent" & descriptive_labels) {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(breaks = c(0, .5, 1),
                         expand = c(0.01,0),
                         labels = c("beginning", "middle of text", "end")) +
      ggplot2::labs(x = "progress") +
      suppressWarnings(ggplot2::theme(axis.text.x  = ggplot2::element_text(hjust = c(0, 0.5, 1))))
  } else if (x_check == "progress_percent" & !descriptive_labels) {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(labels = scales::label_percent(),
                         # expand = c(0,0)
      ) +
      ggplot2::labs(x = "progress") +
      suppressWarnings(ggplot2::theme(axis.text.x  = ggplot2::element_text(hjust = c(0, 0.5, 1))))
  }

  if (!skip_print) {
    suppressWarnings(print(the_plot))
  } else {
    the_plot
  }
}

#' Measure lexical variety
#'
#' `add_lexical_variety()` augments a tidy text table with columns describing the lexical variety of the corpus. Among other things, checks for uniqueness and size of vocabulary, with additional ratios reporting these measurements in relation to document size.
#'
#' @param df A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param by A grouping column
#' @param feature A column of words containing one word per row
#'
#' @returns A data frame with 7 added columns
#' , the first two logical and the rest numeric:
#'   * `new_word` (logical) Indicates whether this is the first instance of a given word
#'   * `hapax` (logical) Indicates whether this word is the only incident of a given word, or hapax legomenon
#'   * `vocabulary` (integer) Running count of words used
#'   * `ttr` (double) Type-token ratio, derived from the running count of words divided by the total number of words used
#'   * `htr` (double) Hapax-token ratio, derived from the running count of hapax legomena divided by the total number of words used
#'   * `progress_words` (integer) Running count of total words used so far in a document
#'   * `progress_percent` (double) Words used so far as a percentage of the total number of words used in a document
#'
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen |>
#'    add_lexical_variety() |>
#'    head()
add_lexical_variety <- function(df, by = doc_id, feature = word) {
  df |>
    dplyr::ungroup() |>
    dplyr::group_by({{ by }}) |>
    dplyr::mutate(
      new_word = !duplicated({{ feature }}),
      vocabulary = cumsum(new_word), # total lexical variety
      ttr = vocabulary / dplyr::row_number(), # type to term ratio
      progress_words = dplyr::row_number(),
      progress_percent = progress_words / max(progress_words)) |>
    dplyr::ungroup() |>
    dplyr::group_by({{ by }}, {{ feature }}) |>
    dplyr::mutate(
      hapax = dplyr::if_else(dplyr::n() == 1, TRUE, FALSE),
      .after = new_word) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      htr = cumsum(hapax) / dplyr::row_number(),
      .by = {{ by }},
      .after = ttr) |>
    add_class("lexical_variety")
}

#' Show vocabulary growth
#'
#' `plot_vocabulary()` visualizes the vocabulary growth as new words are used in each document.
#'
#' @param df A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param x A column showing the cumulative count of words
#' @param by A grouping column for colors and labels
#' @param identity A grouping column for lines
#' @param descriptive_labels A toggle for disabling descriptive labels of progress_percent on the X-axis
#' @param labeling Options for labeling groups:
#' * `"point"` labels the final value
#' * `"inline"` prints the label within a smoothed curve
#' * `"axis"` prints labels where a secondary Y-axis might go
#' * `"inset"` prints a legend within the plot area
#' * Anything else prints a legend to the right of the plot area.
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen_measured <- austen |>
#'   add_lexical_variety()
#'
#' austen_measured |>
#'   standardize_titles() |>
#'   plot_vocabulary(progress_percent)
#'
#' austen_measured |>
#'   standardize_titles() |>
#'   plot_vocabulary()
#'
#' \dontrun{
#'   get_micusp_corpus(
#'     discipline %in% c("Physics", "Economics")) |>
#'     load_texts() |>
#'     add_lexical_variety() |>
#'     plot_vocabulary(by = discipline)
#' }
plot_vocabulary <- function(df, x = progress_words, by = doc_id, identity = NULL, descriptive_labels = TRUE, labeling = c("point", "inset", "inline", "axis")){

  viz_attr <- attr(df, "visualize")
  if (is.null(viz_attr)) viz_attr <- FALSE

  if (is.null(identity)) {
    internal_plot_engine(
      df, rlang::enquo(x), y = rlang::expr(vocabulary),
      rlang::enquo(by), identity = rlang::enquo(by),
      descriptive_labels, labeling, skip_print = viz_attr)
  } else {
    internal_plot_engine(
      df, rlang::enquo(x), y = rlang::expr(vocabulary),
      rlang::enquo(by), identity = rlang::enquo(identity),
      descriptive_labels, labeling, skip_print = viz_attr)
  }

}

#' Show type-token ratio over time
#'
#' @param df A tidy data frame, potentially containing a column called "doc_id" and "word"
#' @param x The progress column to show. Default option is progress_percent, but progress_words is also appropriate.
#' @param by A grouping column for colors and labels
#' @param identity A grouping column for lines
#' @param descriptive_labels A toggle for disabling descriptive labels of progress_percent on the X-axis
#' @param labeling Options for labeling groups:
#' * `"point"` labels the final value
#' * `"inline"` prints the label within a smoothed curve
#' * `"axis"` prints labels where a secondary Y-axis might go
#' * `"inset"` prints a legend within the plot area
#' * Anything else prints a legend to the right of the plot area.
#' @param log_y A toggle for logarithmic scaling to the Y-axis; defaults to TRUE
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen_measured <- austen |>
#'   add_lexical_variety()
#'
#' austen_measured |>
#'   standardize_titles() |>
#'   plot_ttr(labeling = "inline")
#'
#' austen_measured |>
#'   standardize_titles() |>
#'   plot_ttr()
plot_ttr <- function(df, x = progress_words, by = doc_id, identity = NULL, descriptive_labels = TRUE, labeling = c("point", "inline", "axis", "inset"), log_y = TRUE){

  viz_attr <- attr(df, "visualize")
  if (is.null(viz_attr)) viz_attr <- FALSE

  if (is.null(identity)) {
    internal_plot_engine(
      df, rlang::enquo(x), y = rlang::expr(ttr),
      rlang::enquo(by), rlang::enquo(by), descriptive_labels, labeling,
      log_y, skip_print = viz_attr)
  } else {
    internal_plot_engine(
      df, rlang::enquo(x), y = rlang::expr(ttr),
      rlang::enquo(by), rlang::enquo(identity), descriptive_labels, labeling,
      log_y, skip_print = viz_attr)
  }

}

#' Show hapax-token ratio over time
#'
#' @param df A tidy data frame, potentially containing a column called "doc_id" and "word"
#' @param x The progress column to show. Default option is progress_percent, but progress_words is also appropriate.
#' @param by A grouping column for colors and labels
#' @param identity A grouping column for lines
#' @param descriptive_labels A toggle for disabling descriptive labels of progress_percent on the X-axis
#' @param labeling Options for labeling groups:
#' * `"point"` labels the final value
#' * `"inline"` prints the label within a smoothed curve
#' * `"axis"` prints labels where a secondary Y-axis might go
#' * `"inset"` prints a legend within the plot area
#' * Anything else prints a legend to the right of the plot area.
#' @param log_y A toggle for logarithmic scaling to the Y-axis; defaults to TRUE
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen_measured <- austen |>
#'   add_lexical_variety()
#'
#' austen_measured |>
#'   standardize_titles() |>
#'   plot_htr()
plot_htr <- function(df, x = progress_words, by = doc_id, identity = doc_id, descriptive_labels = TRUE, labeling = c("point", "inline", "axis", "inset"), log_y = TRUE){

  viz_attr <- attr(df, "visualize")
  if (is.null(viz_attr)) viz_attr <- FALSE

  if (is.null(identity)) {
    internal_plot_engine(
      df, rlang::enquo(x), y = rlang::expr(htr),
      rlang::enquo(by), identity = rlang::enquo(by), descriptive_labels, labeling,
      log_y, skip_print = viz_attr)
  } else {
    internal_plot_engine(
      df, rlang::enquo(x), y = rlang::expr(htr),
      rlang::enquo(by), identity = rlang::enquo(identity), descriptive_labels, labeling,
      log_y, skip_print = viz_attr)
  }
}

#' Project hapax legomena onto vocabulary growth
#'
#' `plot_hapax()` visualizes a sampling of hapax legomena projected on faceted curves of vocabulary growth over time
#'
#' @param df A tidy data frame, potentially containing columns called "doc_id" and "word"
#' @param prop The proportion of hapax to sample. The chart can become illegible with proportions over ~1%
#' @param x The progress column to show. Default option is progress_percent, but progress_words is also appropriate.
#' @param y The Y-axis variable to chart. Default value is the cumulative vocabulary size.
#' @param by A grouping column, such as doc_id
#' @param descriptive_labels A toggle for disabling descriptive labels of progress_percent on the X-axis
#' @param feature The column to check for new features. Defaults to `hapax`, but the function might also be used with `new_word` instead to plot a sample of new additions to documents' vocabularies.
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' austen_measured <- austen |>
#'   add_lexical_variety()
#'
#' austen_measured |>
#'   standardize_titles() |>
#'   plot_hapax()
plot_hapax <- function(
    df,
    prop = 0.01,
    x = progress_words,
    y = vocabulary,
    by = doc_id,
    descriptive_labels = TRUE,
    feature = hapax){

  the_plot <- df |>
    dplyr::filter({{ feature }}) |>
    dplyr::slice_sample(prop = prop, by = {{ by }}) |>
    ggplot2::ggplot(ggplot2::aes(x = {{ x }},
               y = {{ y }},
               label = word,
               color =  {{ by }})) +
    ggplot2::geom_text() +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(y = "vocabulary (words)",
         color = NULL) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(legend.position = "none",
          legend.background = ggplot2::element_rect(fill = "white", color = "white"),
          panel.grid.minor = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(fill="white"),
          strip.text = ggplot2::element_text(color = "black"))

  if (deparse(substitute(x)) == "progress_words") {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      ggplot2::labs(x = "text length (words)") +
      ggplot2::facet_wrap(ggplot2::vars({{ by }}), scales = "free")
  } else if (deparse(substitute(x)) == "progress_percent" & descriptive_labels) {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(breaks = c(0, .5, 1),
                         labels = c("beginning", "middle", "end")) +
      ggplot2::labs(x = "progress") +
      suppressWarnings(ggplot2::theme(axis.text.x  = ggplot2::element_text(hjust = c(0, 0.5, 1)))) +
      ggplot2::facet_wrap(ggplot2::vars({{ by }}), scales = "free_y")
  } else if (deparse(substitute(x)) == "progress_percent" & !descriptive_labels) {
    the_plot <- the_plot +
      ggplot2::scale_x_continuous(labels = scales::label_percent()) +
      ggplot2::labs(x = "progress") +
      suppressWarnings(ggplot2::theme(axis.text.x  = ggplot2::element_text(hjust = c(0, 0.5, 1)))) +
      ggplot2::facet_wrap(ggplot2::vars({{ by }}), scales = "free_y")
  }
  the_plot
}
