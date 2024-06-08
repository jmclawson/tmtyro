#' Create a lexicon
#'
#' `make_dictionary()` creates a dictionary for use with `add_dictionary()`.
#'
#' @param definitions A list of named word vectors
#' @param name The kind of dictionary
#'
#' @returns A data frame with two columns, "word" and <name>
#' @export
#'
#' @examples
#' my_sentiments <- make_dictionary(
#'   list(
#'     "😊" = c("happy", "joy", "smile"),
#'     "😔" = c("unhappy", "sad", "frown")),
#'   name = "emoji")
#'
#' border_states <- make_dictionary(
#'   definitions = list(
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
#'   name = "borders")
make_dictionary <- function(
    definitions,
    name = "value") {
  make_df <- function(x){
    the_df <- data.frame(
      word = unname(unlist(x))
    )
  }
  definitions |>
    purrr::map(make_df) |>
    dplyr::bind_rows(.id = name) |>
    dplyr::relocate(word)
}

#' Add values from a dictionary
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param dictionary A data frame with two or more columns, potentially made with `make_dictionary()`
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
#'   name = "emoji")
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
  max_ngrams <- dictionary |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .size = strsplit(word, split = " ") |>
        unlist() |>
        length()) |>
    dplyr::ungroup() |>
    dplyr::pull(.size) |>
    max(na.rm = TRUE)

  if (max_ngrams > 1) {
    results <- df |>
      add_ngram_dictionary(dictionary, feature)
    return(results)
  }

  if ("data.frame" %in% class(df) &&
      deparse(substitute(feature)) %in% colnames(df)) {
    df |>
      dplyr::left_join(
        dictionary,
        by = dplyr::join_by({{ feature }} == word))
  } else if ("data.frame" %in% class(df) &&
             "text" %in% colnames(df)) {
    dictionary_replace <- dictionary[,colnames(dictionary) != "word"]
    names(dictionary_replace) <-
      paste0("\\b",dictionary$word,"\\b")
    df |>
      dplyr::mutate(
        text = text |>
          stringr::str_replace_all(dictionary_replace)
      )
  }
}

add_ngram_dictionary <- function(df, dictionary, feature = word){
  # browser()
  dictionary <- dictionary |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .size = strsplit(word, split = " ") |>
        unlist() |>
        length()) |>
    dplyr::ungroup()

  if ("ngram" %in% colnames(df)) {
    warning("Adding an n-gram dictionary will drop the existing ngram column.")
    df <- df |>
      dplyr::select(-ngram)
  }

  # browser()

  dictionary_cols <- colnames(dictionary) |>
    {\(x) x[!x %in% c("word", ".size")]}()

  max_ngram <- max(dictionary$.size, na.rm = TRUE)

  for (i in max_ngram:1) {
    df <- df |>
      add_ngrams(
        1:i,
        feature = word,
        keep = TRUE) |>
      combine_ngrams()
    if (i == max_ngram){
      df <- df |>
        dplyr::left_join(
          dictionary,
          by = dplyr::join_by(ngram == word))
    } else {
      df <- df |>
        dplyr::rows_update(
          dplyr::rename(dictionary, ngram = word),
          by = "ngram",
          unmatched = "ignore")
    }
    df <- dplyr::select(df, -ngram)
  }
  drop_lagged_values <- function(df, num) {
    df |>
      dplyr::mutate(dplyr::across(dplyr::all_of(dictionary_cols),
          \(x) dplyr::case_when(
            lag(.size, n = num) > num ~ NA_character_,
            TRUE ~ x)))
  }
  for (i in 1:max_ngram){
    df <- drop_lagged_values(df, i)
  }
  df |>
    dplyr::select(-.size)
}
