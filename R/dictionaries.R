#' Create a lexicon
#'
#' `make_dictionary()` creates a dictionary for use with `add_dictionary()`.
#'
#' @param list A list of named word vectors
#' @param identifier The kind of dictionary
#'
#' @returns A data frame with two columns, "word" and <identifier>
#' @export
#'
#' @examples
#' my_sentiments <- make_dictionary(
#'   list(
#'     "😊" = c("happy", "joy", "smile"),
#'     "😔" = c("unhappy", "sad", "frown")),
#'   identifier = "emoji")
#'
#' border_states <- make_dictionary(
#'   list(
#'     "Canada" = c(
#'       "Alaska", "Washington", "Idaho",
#'       "Montana", "North_Dakota", "Minnesota",
#'       "Wisconsin", "Michigan", "Ohio",
#'       "Pennsylvania", "New_York", "Vermont",
#'       "New_Hampshire", "Maine", "AK", "WA",
#'       "ID", "MT", "ND", "MN", "WI", "MI",
#'       "OH", "PA", "NY", "VT", "NH", "ME"),
#'     "Mexico" = c(
#'       "California", "Arizona", "New_Mexico",
#'       "Texas", "CA", "AZ", "NM", "TX")),
#'   identifier = "borders")
make_dictionary <- function(
    list,
    identifier = "value") {
  make_df <- function(x){
    the_df <- data.frame(
      word = unname(unlist(x))
    )
  }
  list |>
    purrr::map(make_df) |>
    dplyr::bind_rows(.id = identifier) |>
    dplyr::relocate(word)
}

#' Add values from a dictionary
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param dictionary A data frame with two or more columns, potentially made with the `make_dictionary()` function
#' @param feature The column (like "word") to use for looking up values in the dictionary
#'
#' @returns The original data frame with one or more columns added.
#' @export
#'
#' @examples
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' my_sentiments <- make_dictionary(
#'   list(
#'     "😊" = c("happy", "joy", "smile"),
#'     "😔" = c("unhappy", "sad", "frown")),
#'   identifier = "emoji")
#'
#' dubliners |>
#'    add_dictionary(my_sentiments) |>
#'    drop_na() |>
#'    head()
add_dictionary <- function(
    df,
    dictionary,
    feature = word
) {
  if (!"data.frame" %in% class(dictionary) ||
      !"word" %in% colnames(dictionary)) {
    stop("Please be sure to pass a valid dictionary.")
  }
  df |>
    dplyr::left_join(
      dictionary,
      by = dplyr::join_by({{ feature }} == word))
}
