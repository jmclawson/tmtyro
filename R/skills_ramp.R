#' Get frequencies of values in a vector
#'
#' @param x A vector, such as a column of character strings
#' @param percent Whether to return frequencies as percentage of the whole
#'
#' @returns A vector of counts or ratios for each value of `x`.
#' @family vectorized functions
#' @export
#'
#' @examples
#' my_values <- c("dog", "cat", "dog")
#'
#' get_frequency(my_values)
get_frequency <- function(x, percent = FALSE) {
  x <- as.character(x)
  result <- table(x)[x] |>
    as.integer()

  if (percent) {
    result <- result / length(x)
  }

  return(result)
}

#' @rdname get_frequency
#' @family vectorized functions
#' @export
get_tf <- function(x, percent = TRUE){
  get_frequency(x, percent)
}

#' Get term frequencies of values in one vector `x` categorized by another vector `by`.
#'
#' @param x A vector, such as a column of character strings
#' @param by A vector of categories, such as a column of document identifiers
#'
#' @returns A vector of term frequencies for each value pair of `x` and `by`.
#' @family vectorized functions
#' @export
#'
#' @examples
#' my_values <- c(
#'   "the", "cat", "was", "bad",
#'   "the", "dog", "was", "very", "good",
#'   "the", "lizard", "is", "the", "most", "bad")
#' my_docs <- c(
#'   "A", "A", "A", "A",
#'   "B", "B", "B", "B", "B",
#'   "C", "C", "C", "C", "C", "C")
#'
#' get_tf_by(my_values, my_docs)
get_tf_by <- function(x, by) {
  if (length(x) != length(by)) {
    stop("`x` and `by` must be vectors of the same length.")
  }

  data.frame(doc = by,
             word = x) |>
    dplyr::group_by(doc) |>
    dplyr::mutate(tf = get_tf(word)) |>
    dplyr::ungroup() |>
    dplyr::pull(tf)
}

#' Get inverse document frequencies of values in one vector `x` categorized by another vector `by`.
#'
#' @param x A vector, such as a column of character strings
#' @param by A vector of categories, such as a column of document identifiers
#'
#' @returns A vector of inverse document frequencies for each value pair of `x` and `by`.
#' @family vectorized functions
#' @export
#'
#' @examples
#' my_values <- c(
#'   "the", "cat", "was", "bad",
#'   "the", "dog", "was", "very", "good",
#'   "the", "lizard", "is", "the", "most", "bad")
#' my_docs <- c(
#'   "A", "A", "A", "A",
#'   "B", "B", "B", "B", "B",
#'   "C", "C", "C", "C", "C", "C")
#'
#' get_idf_by(my_values, my_docs)
get_idf_by <- function(x, by) {
  if (missing(by)) {
    stop("Missing `by`. A list of document identifiers is needed to calculate inverse document frequency.")
  }
  x <- as.character(x)
  if (length(x) != length(by)) {
    stop("`x` and `by` must be vectors of the same length.")
  }
  doc_words <-
    data.frame(doc = by,
               word = x) |>
    unique()

  term_counts <- table(doc_words$word)

  num_docs <- by |>
    unique() |>
    length()

  log(num_docs/term_counts)[x] |>
    as.numeric()
}

#' Term frequency--inverse document frequency
#'
#' Get tf-idf weights of values in one vector `x` categorized by another vector `by`.
#'
#' @param x A vector, such as a column of character strings
#' @param by A vector of categories, such as a column of document identifiers
#'
#' @returns A vector of term frequency--inverse document frequencies for each value pair of `x` and `by`.
#' @family vectorized functions
#' @export
#'
#' @examples
#' my_values <- c(
#'   "the", "cat", "was", "bad",
#'   "the", "dog", "was", "very", "good",
#'   "the", "lizard", "is", "the", "most", "bad")
#' my_docs <- c(
#'   "A", "A", "A", "A",
#'   "B", "B", "B", "B", "B",
#'   "C", "C", "C", "C", "C", "C")
#'
#' get_tfidf_by(my_values, my_docs)
get_tfidf_by <- function(x, by) {
  tf <- get_tf_by(x, by)
  idf <- get_idf_by(x, by)
  tf * idf
}

#' Get sentiment matches of values in a vector
#'
#' @param x A vector, such as a column of character strings
#' @param lexicon The sentiment lexicon to use from the [tidytext] package. Options include "bing", "afinn", "loughran", "nrc", "nrc_eil", or "nrc_vad".
#' @param ... Additional values passed to `get_match()`
#'
#' @returns A vector or nested list of sentiments for each value of `x`.
#' @family vectorized functions
#' @export
#'
#' @examples
#' my_values <- c("I", "am", "happy")
#'
#' get_sentiment(my_values)
get_sentiment <- function(
    x,
    lexicon = c("bing", "afinn", "loughran",
                "nrc", "nrc_eil", "nrc_vad"),
    ...){
  lex <- match.arg(lexicon)
  rlang::check_installed("textdata", reason = "for downloading sentiment lexicons")
  if (lex %in% c("bing", "afinn", "loughran",
                 "nrc")) {
    lex_df <- tidytext::get_sentiments(lex)
  } else {
    lex_df <- switch(
      lex,
      "nrc_eil"  = textdata::lexicon_nrc_eil(),
      "nrc_vad"  = textdata::lexicon_nrc_vad(),
      stop("Unexpected lexicon", call. = FALSE)
    )
  }
  colnames(lex_df)[colnames(lex_df) == "word"] <- "term"
  get_match(x, lex_df, ...)
}

#' Get dictionary matches of values in a vector
#'
#' @param x A vector, such as a column of character strings
#' @param dictionary A data frame with two or more columns, potentially made with `make_dictionary()`.
#' @param keep The number of matched values to keep. If NULL, returns all matched values in a nested list.
#'
#' @returns A vector or nested list of sentiments for each value of `x`.
#' @family vectorized functions
#' @export
#'
#' @examples
#' my_values <- c("It", "is", "raining")
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
#' get_match(my_values, emoji_weather)
get_match <- function(x, dictionary, keep = NULL) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  value_class <- class(dictionary[[2]])
  if (is.data.frame(dictionary)) {
    dictionary <- make_dictionary(dictionary)
  }
  match_term <- colnames(dictionary) |>
    {\(x) x[x!="term"][1]}()
  all_results <- c()
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      all_results[i] <- NA_character_
      next
    }
    single <- dictionary[[match_term]][dictionary$term == x[i]]
    if (length(single)==0) {
      single <- NA_character_
    } else if (!is.null(keep)) {
      single <- single[1:keep]
    }

    if (length(single) > 1) {
      single <- list(single)
    }

    all_results[i] <- single
  }
  class(all_results) <- value_class
  all_results
}

#' Check for new words in a vocabulary
#'
#' @param x A vector, such as a column of character strings
#'
#' @returns A logical vector
#' @family vectorized functions
#' @export
#'
#' @examples
#' c("cat", "dog", "dog", "bat", "dog") |>
#'   is_new()
is_new <- function(x) {
  result <- !duplicated(x)
  result[is.na(x)] <- FALSE
  return(result)
}

#' Check for hapax legomena
#'
#' @param x A vector, such as a column of character strings
#'
#' @returns A logical vector
#' @family vectorized functions
#' @export
#'
#' @examples
#' c("cat", "dog", "dog", "bat", "dog") |>
#'   is_hapax()
is_hapax <- function(x) {
  result <- get_frequency(x) == 1
  result[is.na(result)] <- FALSE
  result
}

#' Cumulative total of vocabulary size
#'
#' @param x A vector, such as a column of character strings
#'
#' @returns A vector of counts
#' @family vectorized functions
#' @export
#'
#' @examples
#' c("cat", "dog", "dog", "bat", "dog") |>
#'   get_cumulative_vocabulary()
get_cumulative_vocabulary <- function(x) {
  cumsum(is_new(x))
}

#' Cumulative type-token ratio
#'
#' TTR reports the ratio of unique word types to the total size of a text.
#'
#' @param x A vector, such as a column of character strings
#'
#' @returns A vector of ratios
#' @family vectorized functions
#' @export
#'
#' @examples
#' c("cat", "dog", "dog", "bat", "dog") |>
#'   get_ttr()
get_ttr <- function(x) {
  get_cumulative_vocabulary(x) / 1:length(x)
}

#' Cumulative hapax introduction ratio
#'
#' Hapax introduction ratio tracks the use of unique words as a text grows.
#'
#' @param x A vector, such as a column of character strings
#'
#' @returns A vector of ratios
#' @family vectorized functions
#' @export
#'
#' @examples
#' c("cat", "dog", "dog", "bat", "dog") |>
#'   get_hir()
get_hir <- function(x) {
  if (length(x) == 0) {
    return(numeric(0))
  }

  cumsum(is_hapax(x)) / 1:length(x)
}

#' Cumulative hapax-token ratio
#'
#' HTR reports the cumulative ratio of hapax legomena (one-off words) to the cumulative size of a text. This operation can be slow.
#'
#' @param x A vector, such as a column of character strings
#'
#' @returns A vector of ratios
#' @family vectorized functions
#' @export
#'
#' @examples
#' c("cat", "dog", "dog", "bat", "dog") |>
#'   get_htr()
get_htr <- function(x) {
  if (length(x) == 0) {
    return(numeric(0))
  }

  ratios <- numeric(length(x))
  freq_table <- table(factor(x, levels = unique(x)))

  # Loop through each element in the vector
  for (i in 1:length(x)) {
    current_x <- x[1:i]
    current_x <- current_x[!is.na(current_x)]

    freq_table <- table(factor(current_x, levels = unique(x)))

    hapaxes <- freq_table == 1

    ratios[i] <- sum(hapaxes) / length(current_x)
  }
  return(ratios)
}

# progress_words, progress_percent
