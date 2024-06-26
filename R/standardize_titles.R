#' Standardize document titles
#'
#' Useful especially for visualizations. `standardize_titles` applies some English-language conventions, including converting underscores to spaces, capitalizing important words, removing leading articles, and dropping subtitles.
#'
#' @param .data A tidy data frame, potentially containing a title column called "doc_id". Alternatively, a simple character vector of titles.
#' @param title A column containing the titles to be standardized
#' @param drop_articles Whether to remove opening articles like "The" and "A"
#'
#' @returns A data frame with one column adjusted. If .data is a character vector instead of a data frame, then a character vector is returned.
#' @export
#'
#' @examples
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part)
#'
#' ##### Standardizing strings #####
#' # Before `standardize_titles()`
#' unique(dubliners$doc_id)
#'
#' # After `standardize_titles()`
#' unique(dubliners$doc_id) |>
#'   standardize_titles()
#'
#' ##### Standardizing a data frame #####
#'
#' dubliners_measured <- dubliners |>
#'   add_vocabulary()
#'
#' # Before `standardize_titles()`
#' dubliners_measured |>
#'   plot_vocabulary(labeling = "inline")
#'
#' # After `standardize_titles()`
#' dubliners_measured |>
#'   standardize_titles() |>
#'   plot_vocabulary(labeling = "inline")
standardize_titles <- function(.data, title = doc_id, drop_articles = FALSE){
  # Prepositions, articles, and conjunctions
  lowercase_words <- c("a", "an", "and",
                       "about", "after", "against",
                       "as", "at",
                       "before", "between",
                       "but", "by",
                       "for", "from",
                       "in", "into",
                       "of", "on", "or",
                       "the", "through", "to",
                       "under", "upon",
                       "with", "within", "without") |>
    {\(x) paste0("(?<![.?]) ", x, " ")}()

  lowercase_names <- lowercase_words |>
    stringr::str_to_title()

  lowercase_words <-
    lowercase_words |>
    stringr::str_remove_all("\\(\\?\\<\\!\\[\\.\\?\\]\\)")

  names(lowercase_words) <- lowercase_names

  if (rlang::is_installed("utils")) {
    roman_numerals <- utils::as.roman(1:100) |>
      stringr::str_to_title() |>
      setNames(utils::as.roman(1:100))
  } else {
    make_rns <- function(base = "I",
                         mid = "V",
                         top = "X") {
      result_1.3 <- 1:3 |>
        sapply(\(x) paste0(rep(base, x), collapse =""))
      result_4 <- paste0(base, mid)
      result_5 <- mid
      result_6.8 <- paste0(mid, result_1.3)
      result_9 <- paste0(base, top)
      return(c(result_1.3, result_4, result_5,
               result_6.8, result_9))
    }
    roman_numerals <-
      expand.grid(c("", make_rns("I", "V", "X")),
                  c("", make_rns("X", "L", "C"))) |>
      dplyr::mutate(rnumeral = paste0(Var2, Var1)) |>
      dplyr::pull(rnumeral) |>
      {\(x) x[x!=""]}()
  }

  standardize_string <- function(.data, drop_articles){
    .data <- .data |>
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_title() |>
      stringr::str_remove_all("[;:].*") |>
      stringr::str_remove_all("[.]$")

    if (drop_articles) {
      .data <- .data |>
        stringr::str_remove_all("^The ") |>
        stringr::str_remove_all("^A[n]? ")
    }
    .data |>
      stringr::str_replace_all(lowercase_words)
  }

  if (is.character(.data)) {
    .data |> standardize_string(drop_articles)
  } else if (is.factor(.data)) {
    the_levels <- .data |>
      levels() |>
      standardize_string(drop_articles)
    .data |>
      standardize_string(drop_articles) |>
      factor(levels = the_levels)
  } else if ("doc_id" %in% colnames(.data) &&
             .data |>
             dplyr::pull(doc_id) |>
             is.factor()) {
    .data |>
      dplyr::mutate({{ title }} := {{ title }} |>
                      standardize_string(drop_articles) |>
                      forcats::fct_inorder())
  } else {
    .data |>
      dplyr::mutate({{ title }} := {{ title }} |>
               standardize_string(drop_articles))
  }
}
