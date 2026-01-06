#' Italicize document titles in a table or figure
#'
#' @param x A table data object created with `tabulize()` or a figure created with `visualize()`
#' @param col The column to italicize
#'
#' @returns A gt table data object or a ggplot2 figure
#' @family table helpers
#' @family visualizing helpers
#' @export
#'
#' @section Examples:
#' ```r
#' library(gt)
#' library(tmtyro)
#'
#' penguins_gt <-
#'   penguins |>
#'   select(-year) |>
#'   summarize(
#'     across(
#'       matches("_len$|_dep$"), mean, na.rm = TRUE),
#'     .by = c(species, island, sex)) |>
#'   gt() |>
#'   fmt_number() |>
#'   tab_spanner(
#'     "bill",
#'     columns = starts_with("bill_")) |>
#'   tab_spanner(
#'     "flipper",
#'     starts_with("flip")) |>
#'   cols_label(
#'     bill_len = "length",
#'     bill_dep = "depth",
#'     flipper_len = "length") |>
#'   sub_missing()
#'
#' penguins_gt |>
#'   collapse_rows(species) |>
#'   collapse_rows(island) |>
#'   italicize_titles(species)
#' ```
italicize_titles <- function(x, col){
  UseMethod("italicize_titles")
}

#' @export
italicize_titles.gt_tbl <- function(x, col = doc_id) {
  x |>
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = cells_body(
        columns = {{ col }}
      )
    )
}

#' @export
italicize_titles.ggplot <- function(x, col = doc_id) {
  p <- x
  col_string <- deparse(substitute(col))
  on_facet <- isTRUE(col_string %in% names(p@facet$params$facets))
  on_x <- isTRUE(rlang::as_label(p@mapping$x) == col_string)
  on_y <- isTRUE(rlang::as_label(p@mapping$y) == col_string)
  # on_fill <- isTRUE(rlang::as_label(p@mapping$fill) == col_string)
  if (on_facet) {
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "italic"))
  }

  if (on_x) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic"))
  }

  if (on_y) {
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic"))
  }

  p
}
