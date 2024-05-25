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
#' austen <- "austen.rds" |>
#'   system.file(package = "tmtyro") |>
#'   readRDS()
#'
#' ##### Standardizing strings #####
#' # Before `standardize_titles()`
#' unique(austen$doc_id)
#'
#' # After `standardize_titles()`
#' unique(austen$doc_id) |>
#'   standardize_titles()
#'
#' ##### Standardizing a data frame #####
#'
#' austen_measured <- austen |>
#'   add_vocabulary()
#'
#' # Before `standardize_titles()`
#' austen_measured |>
#'   plot_vocabulary(labeling = "inline")
#'
#' # After `standardize_titles()`
#' austen_measured |>
#'   standardize_titles() |>
#'   plot_vocabulary(labeling = "inline")
standardize_titles <- function(.data, title = doc_id, drop_articles = TRUE){
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
    {\(x) paste0(" ", x, " ")}()

  names(lowercase_words) <- lowercase_words |>
    stringr::str_to_title()

  standardize_string <- function(.data, drop_articles){
    .data <- .data |>
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_title() |>
      stringr::str_remove_all("[;:].*")

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
  } else {
    .data |>
      dplyr::mutate({{ title }} := {{ title }} |>
               standardize_string(drop_articles))
  }
}
