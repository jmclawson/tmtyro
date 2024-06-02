#' Choose other colors
#'
#' `change_colors()` standardizes three methods for choosing color palettes for color or fill mapping, providing access to Brewer and Viridis palettes alongside custom choices.
#'
#' @param x A visualization made with [visualize()]
#' @param colorset Either "brewer", "viridis", "okabe-ito", "dubois", or a vector of colors.
#' @param palette The number or name of palette (dependent on setting `colorset` to either "brewer" or "viridis")
#' @param kind Used only for Brewer palettes to match numbered palette with the specific subset
#' @param direction The direction colors should be applied to the data. Setting to anything other than 1 will reverse the order.
#' @param start Useful for predefined colorsets from Okabe-Ito and Brewer to start from a color other than 1
#'
#' @returns A ggplot2 object
#' @family visualizing helpers
#' @export
#'
#' @examples
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' # Too many titles for categorical data.
#' selected_titles <- dubliners$doc_id |>
#'   {\(x) x[grepl("^The |^A |^An ", x)]}() |>
#'   unique()
#'
#' dubliners2 <- dubliners |>
#'   dplyr::filter(doc_id %in% selected_titles)
#'
#' ### CATEGORICAL DATA ##
#'
#' # By default, ggplot2's palette is applied
#' dubliners2 |>
#'   visualize()
#'
#' # change_color() starts with Brewer's "Dark2" palette
#' dubliners2 |>
#'   visualize() |>
#'   change_colors()
#'
#' # Other color sets and palettes can be chosen
#' dubliners2 |>
#'   visualize() |>
#'   change_colors(colorset = "okabe")
#'
#' dubliners2 |>
#'   visualize() |>
#'   change_colors(colorset = "viridis", palette = "turbo")
#'
#' dubliners2 |>
#'   visualize() |>
#'   change_colors(colorset = "brewer", palette = "Set1")
#'
#' # Named cases can be highlighted
#' dubliners2 |>
#'   visualize(inorder = FALSE) |>
#'   change_colors(c(
#'     rep("darkgray", 6),
#'     "A Painful Case" = "blue"))
#'
#' ### SEQUENTIAL DATA ###
#'
#' # By default, the "viridis" palette is applied
#' dubliners2 |>
#'   visualize(type = "heatmap")
#'
#' # change_colors()  starts with Brewer's "BuGn" palette
#' dubliners2 |>
#'   visualize(type = "heatmap") |>
#'   change_colors()
#'
#' # Palettes can be numbered or named
#' dubliners2 |>
#'   visualize(type = "heatmap") |>
#'   change_colors("viridis", palette = 6)
#'
#' dubliners2 |>
#'   visualize(type = "heatmap") |>
#'   change_colors("viridis", palette = "mako")
#'
#' ### POINT DATA ###
#'
#' dubliners |>
#'   add_ngrams() |>
#'   visualize() |>
#'   change_colors("orange")
change_colors <- function(
    x,
    colorset = "brewer",
    palette = 2,
    kind = "qualitative",
    direction = 1,
    start = 1) {

  mapped <- names(x$mapping)[names(x$mapping) %in% c("color", "colour", "fill")]
  if ("ggraph" %in% class(x)) {
    mapped <- "edge_color"
  }

  secondary <- if(!is.null(x$plot_env$sec_y)) {x$plot_env$sec_y} else {NULL}
  color_map <-
    c(deparse(x$mapping$colour),
      deparse(x$mapping$color),
      deparse(x$mapping$fill)) |>
    stringr::str_remove_all("~") |>
    unique() |>
    {\(x) x[x!="NULL"]}() |>
    {\(x) ifelse(length(x) > 0, x, NA)}()
  if (length(mapped) == 0) {
    mapped <- names(x$layers[[1]]$mapping)[names(x$layers[[1]]$mapping) %in% c("color", "colour", "fill")]

    color_map <-
      c(deparse(x$layers[[1]]$mapping$colour),
        deparse(x$layers[[1]]$mapping$color),
        deparse(x$layers[[1]]$mapping$fill)) |>
      stringr::str_remove_all("^.*~") |>
      stringr::str_remove_all(",.*$") |>
      stringr::str_remove_all("[a-z .]+\\(") |>
      stringr::str_remove_all("[)]+") |>
      unique() |>
      {\(x) x[x!="NULL"]}() |>
      {\(x) ifelse(length(x) > 0, x, NA)}()

  }
  if (!"ggraph" %in% class(x)) {
    color_map_length <- x$data[[color_map]] |>
      unique() |>
      length()
    is_sequential <-
      tryCatch(is.numeric(as.numeric(as.character(x$data[[color_map]]))), warning = function(e) return(FALSE))
  } else {
    color_map_length <- x$plot_env$df_export$n |>
      unique() |>
      length()
    is_sequential <- TRUE
  }

  if (is_sequential) {
    kind <- "seq"
  }

  if (length(colorset) == 1 && tolower(colorset) == "viridis") {
    if (is.numeric(palette)) {
      palette <- LETTERS[palette]
    }
    the_colors <- color_map_length |>
      {\(x) viridis::viridis_pal(option = palette)(x)}()
  } else if (length(colorset) == 1 && grepl("okabe", tolower(colorset))) {
    the_colors <- c("#E69F00", "#56B4E9", "#009E73",
                    "#F0E442", "#0072B2", "#D55E00",
                    "#CC79A7", "#999999", "#000000")[start:(color_map_length + start - 1)]
  } else if (length(colorset) == 1 && grepl("dubois", tolower(colorset))) {
    # from https://github.com/ajstarks/dubois-data-portraits/
    the_colors <- c("#dc143c", "#ffd700", "#654321",
                    "#4682b4", "#ffc0cb", "#00aa00",
                    "#d2b48c", "#7e6583", "#000000")[start:(color_map_length + start - 1)] |>
      rev()
  } else if (length(colorset) == 1 && tolower(colorset) == "brewer") {
    rlang::check_installed("RColorBrewer")
    if (kind == "sequential") {
      kind <- "seq"
    } else if (kind == "diverging") {
      kind <- "div"
    } else {
      kind <- "qual"
    }
    if (is_sequential) {
      kind <- "seq"
    }
    if (is.numeric(palette)){
      palette <- RColorBrewer::brewer.pal.info |>
        dplyr::filter(category==kind) |>
        rownames() |>
        {\(x) x[palette]}()
    }
    if (!is_sequential) {
      if (start != 1) {
        the_colors <-  RColorBrewer::brewer.pal(8, palette)[start:(color_map_length + start - 1)]
      } else {
        the_colors <- color_map_length |>
          {\(x) ifelse(x < 3, 3, x)}() |>
          {\(x) RColorBrewer::brewer.pal(x, palette)}() |>
          {\(x) x[1:color_map_length]}()
      }
    }
  } else if (length(colorset) != color_map_length) {
    the_colors <- colorRampPalette(colorset)(color_map_length)
  } else {
    the_colors <- colorset
  }

  if (direction != 1) {
    if (length(colorset) == 1 && colorset == "brewer"){
      if (!is_sequential) {
        the_colors <- rev(the_colors)
      }
    } else if (length(colorset) == 1 && colorset != "brewer") {
      the_colors <- rev(the_colors)
    } else if (length(colorset) != 1) {
      the_colors <- rev(the_colors)
    }
  }

  if (!is.null(secondary)) {
    x +
      ggplot2::scale_fill_manual(aesthetics = mapped, values = the_colors) +
      ggplot2::scale_y_continuous(
        labels = scales::label_comma(),
        sec.axis = ggplot2::dup_axis(
          breaks = secondary$breaks,
          labels = secondary$labels,
          guide = ggh4x::guide_axis_color(
            color = rev(the_colors))))
  } else if (kind != "seq"){
    x +
      ggplot2::scale_fill_manual(aesthetics = mapped, values = the_colors)
  } else if (length(colorset)==1 && tolower(colorset) == "brewer") {
    if ("ggraph" %in% class(x)) {
      x +
        ggplot2::scale_fill_distiller(
          palette = palette,
          direction = ifelse(direction == 1, 1, -1),
          aesthetics = mapped,
          na.value = "white",
          trans = "log",
          labels = scales::label_comma(),
          guide = ggraph::guide_edge_colorbar())
    } else {
      x +
        ggplot2::scale_fill_distiller(
          palette = palette,
          direction = ifelse(direction == 1, -1, 1),
          aesthetics = mapped,
          na.value = "white")
    }
  } else if (length(colorset)==1 && tolower(colorset) == "viridis") {
    if ("ggraph" %in% class(x)) {
      x +
        ggplot2::scale_fill_viridis_c(
          option = palette,
          direction = ifelse(direction == 1, -1, 1),
          aesthetics = mapped,
          na.value = "white",
          trans = "log",
          labels = scales::label_comma(),
          guide = ggraph::guide_edge_colorbar())
    } else {
      x +
        ggplot2::scale_fill_viridis_c(
          option = palette,
          direction = ifelse(direction == 1, 1, -1),
          aesthetics = mapped,
          na.value = "white")
    }
  } else if ("ggraph" %in% class(x)) {
    x +
      ggplot2::scale_fill_gradientn(
        colours = the_colors,
        aesthetics = mapped,
        na.value = "white",
        labels = scales::label_comma(),
        guide = ggraph::guide_edge_colorbar())
  } else if (length(colorset) > 1) {
    x +
      ggplot2::scale_fill_gradientn(colours = colorset,
                                    aesthetics = mapped,
                                    na.value = "white")
  } else {
    message("Something went wrong.")
  }
}
