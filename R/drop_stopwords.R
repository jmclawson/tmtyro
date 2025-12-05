#' Remove stopwords
#'
#' @param data A tidy data frame, potentially containing a column called "word"
#' @param wordlist A list of stopwords
#' @param feature A column of words containing one word per row to be checked for stopwords.
#'
#' @returns The original data frame with fewer rows.
#' @export
#'
#' @examples
#' \dontrun{
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'    drop_stopwords()
#' }
drop_stopwords <- function(data, wordlist = NULL, feature = word) {
  if (is.null(wordlist)) {
    wordlist <- tidytext::get_stopwords()$word
  }
  data |>
    dplyr::filter(!tolower({{ feature }}) %in% wordlist)
}
