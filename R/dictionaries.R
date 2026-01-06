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
#' emoji_weather <- make_dictionary(
#'   list(
#'     "️☔️" = c("rain", "rains", "rainy", "raining"),
#'     "️⛈️" = c("storm", "storms", "stormy", "storming"),
#'     "☁️" = c("cloud", "clouds", "cloudy"),
#'     "🌞" = c("sun", "sunny"),
#'     "🌫️" = c("fog", "fogs", "foggy", "mist", "misty"),
#'     "🌬️" = c("wind", "winds", "windy"),
#'     "️❄️" = c("snow", "snows", "snowing")),
#'   name = "weather")
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
    name = NULL) {
  make_df <- function(x){
    the_df <- data.frame(
      term = unname(unlist(x))
    )
  }

  if(is.null(name)) name <- "value"

  if (is.data.frame(definitions)) {
    result <- definitions
    colnames(result)[1:2] <- c("term", name)
    if (ncol(result) > 2) {
      colnames(result)[3:ncol(result)] <- result |>
        colnames() |>
        {\(x) x[3:length(x)]}() |>
        stringr::str_remove_all(
          paste0("^", name, "_"))
    }
    return(result)
  }

  definitions |>
    purrr::map(make_df) |>
    dplyr::bind_rows(.id = name) |>
    dplyr::relocate(term)
}

#' Add values from a dictionary
#'
#' @param data A tidy data frame, potentially containing a column called "word"
#' @param dictionary A data frame with two or more columns, potentially made with `make_dictionary()`
#' @param feature The column (like "word") to use for looking up values in the dictionary
#' @param keep_term Whether to retain the original term value. This option is especially useful with dictionaries containing terms longer than one word in length; the NULL value will keep the term for these dictionaries while discarding it for those with terms of only one word.
#' @param label Whether to label variables added to data frame
#'
#' @returns The original data frame with one or more columns added.
#' @export
#'
#' @examples
#' \dontrun{
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' emoji_weather <- make_dictionary(
#'   list(
#'     "️☔️" = c("rain", "rains", "rainy", "raining"),
#'     "️⛈️" = c("storm", "storms", "stormy", "storming"),
#'     "☁️" = c("cloud", "clouds", "cloudy"),
#'     "🌞" = c("sun", "sunny"),
#'     "🌫️" = c("fog", "fogs", "foggy", "mist", "misty"),
#'     "🌬️" = c("wind", "winds", "windy"),
#'     "️❄️" = c("snow", "snows", "snowing")),
#'   name = "weather")
#'
#' dubliners |>
#'    add_dictionary(emoji_weather) |>
#'    drop_na() |>
#'    head()
#' }
add_dictionary <- function(
    data,
    dictionary,
    feature = word,
    keep_term = NULL,
    label = NULL
) {
  if (!"data.frame" %in% class(dictionary) ||
      !"term" == colnames(dictionary)[1] ||
      ncol(dictionary) < 2) {
    stop("Please be sure to pass a valid dictionary.")
  }

  if (colnames(dictionary)[2] == "value") {
    dict_name <- deparse(substitute(dictionary)) |>
      stringr::str_replace_all("ies$", "ies_") |>
      stringr::str_remove_all("s$") |>
      stringr::str_remove_all("_$")
  } else {
    dict_name <- colnames(dictionary)[2]
  }

  max_ngrams <- dictionary[,1][[1]] |>
    sapply(\(x) strsplit(x, " ") |>
             unlist() |>
             length()) |>
    max(na.rm = TRUE)

  if (is.null(keep_term) & max_ngrams > 1) {
    keep_term <- TRUE
  } else if (is.null(keep_term)) {
    keep_term <- FALSE
  }

  if (keep_term) {
    dictionary <- dictionary |>
      dplyr::mutate({{ feature }} := term,
                    .before = 2) |>
      dplyr::relocate(term, .after = 3)
  } else {
    dictionary <- dictionary |>
      dplyr::rename({{ feature }} := term)
  }

  col_num <- length(colnames(dictionary))
  if (col_num > 2) {
    colnames(dictionary)[3:col_num] <- dict_name |>
      paste0("_", colnames(dictionary)[3:col_num])
  }

  colnames(dictionary)[2] <- dict_name

  if (max_ngrams > 1) {
    results <- data |>
      add_ngram_dictionary(dictionary)
    return(results)
  }

  if ("data.frame" %in% class(data) &&
      deparse(substitute(feature)) %in% colnames(data)) {
    result <- data |>
      dplyr::left_join(
        dictionary,
        by = dplyr::join_by({{ feature }} == word))
  } else if ("data.frame" %in% class(data) &&
             "text" %in% colnames(data)) {
    dictionary_replace <- dictionary[,colnames(dictionary) != "word"]
    names(dictionary_replace) <-
      paste0("\\b",dictionary$word,"\\b")
    result <- data |>
      dplyr::mutate(
        text = text |>
          stringr::str_replace_all(dictionary_replace)
      )
  }

  if (tmtyro_use_labels(label)) {
    attr(result[[dict_name]], "label") <- "dictionary match"
  }

  result |>
    add_class("dictionary") |>
    set_feature(dict_name)
}

add_ngram_dictionary <- function(data, dictionary){

  feature <- colnames(dictionary)[1]

  dictionary$.size <- dictionary[,1][[1]] |>
    sapply(\(x) strsplit(x, " ") |>
             unlist() |>
             length())

  if ("ngram" %in% colnames(data)) {
    warning("Adding an n-gram dictionary will drop the existing ngram column.")
    data <- data |>
      dplyr::select(-ngram)
  }

  dictionary_cols <- colnames(dictionary) |>
    {\(x) x[!x %in% c(feature, ".size")]}()

  max_ngram <- max(dictionary$.size, na.rm = TRUE)

  for (i in max_ngram:1) {
    data <- data |>
      add_ngrams(
        1:i,
        feature = eval(feature),
        keep = TRUE) |>
      combine_ngrams()
    if (i == max_ngram){
      data <- data |>
        dplyr::left_join(
          dplyr::rename(
            dictionary,
            ngram = eval(feature)))
    } else {
      data <- data |>
        dplyr::rows_patch(
          dictionary |>
            dplyr::rename(
              ngram = eval(feature)) |>
            dplyr::distinct(
              ngram,
              .keep_all = TRUE),
          by = "ngram",
          unmatched = "ignore")
    }
    data <- dplyr::select(data, -ngram)
  }
  drop_lagged_values <- function(data, num) {
    data |>
      dplyr::mutate(dplyr::across(dplyr::all_of(dictionary_cols),
          \(x) dplyr::case_when(
            is.na(lag(.size, n = num)) ~ x,
            lag(.size, n = num) <= num ~ x)))
  }
  for (i in 1:max_ngram){
    data <- drop_lagged_values(data, i)
  }

  data |>
    dplyr::select(-.size)
}
