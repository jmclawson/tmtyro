# Check for the narrative option in two places:
# 1. In a document-level option
# 2. Among environment variables
# As a fallback, default to TRUE
tmtyro_use_log <- function() {
  opt <- getOption("tmtyro.use_log", NULL)
  if (!is.null(opt)) {
    return(isTRUE(opt))
  }

  env <- Sys.getenv("TMTYRO_USE_LOG", NA_character_)
  if (!is.na(env) && nzchar(env)) {
    return(tolower(env) %in% c("1", "true", "yes", "on"))
  }

  TRUE
}

narrative_dictionary <- tibble::tribble(
  ~fn, ~narrative,
  "get_gutenberg_corpus", "Retrieved a corpus from Project Gutenberg using {gutenberg_id}.",#
  "load_texts", "Loaded texts{parameters}.",#
  "identify_by", "Identified documents using the {relevant_column}.",#
  "standardize_titles", "Standardized titles by converting to title case{parameters}.",#
  "add_dictionary", "Assigned values to each {feature} using a {lexicon} dictionary.",
  "add_frequency", "Counted the frequency of {feature}s used in each document.",#
  "add_index", "Created a document-level index.",#
  "add_ngrams", "Constructed {n}-gram sequences from the text.",#
  "add_partitions", "Partitioned the text into segments of approximately {width} {feature}s with {overlap}.",#
  "add_sentiment", "Assigned {lexicon} sentiment values to each {feature}.",#
  "add_tf_idf", "Calculated TF-IDF values for each {feature} across documents.",#
  "add_vocabulary", "Measured uniquenesss of {feature}s over time.",
  "combine_ngrams", "Merged sequential tokens to form {n}-gram units.",
  "drop_na", "Removed rows with missing or incomplete values in {column}.",
  "drop_stopwords", "Removed stopwords based on the {lexicon} list.",
  "expand_documents", "Converted token-level data into a document-feature matrix.",
)

get_narrative <- function(fn, feature = "word", document = "document", arguments = NULL) {
  # arguments should be a named list like c("by" = "doc_id")
  if (!is.null(arguments)) {
    arguments_names <- paste0("\\{", names(arguments), "\\}")
    arguments <- as.character(arguments) |>
      setNames(arguments_names)
  } else {
    arguments <- c("\\{feature\\}" = "token")
  }
  narrative_dictionary[["narrative"]][narrative_dictionary$fn == fn] |>
    stringr::str_replace_all(c(
      "word" = feature,
      "doc_id" = document)
    ) |>
    stringr::str_replace_all(arguments) |>
    stringr::str_replace_all("2-gram", "bigram") |>
    stringr::str_replace_all("3-gram", "trigram")# |>
  # stringr::str_remove_all("[{}]")
}

add_logstep <- function(x, fn, arguments = NULL, call = NULL) {
  if (is.null(call)) {
    call <- sys.call(-1)
  }

  tmtyro_log <- attr(x, "tmtyro_log", exact = TRUE)
  if (is.null(tmtyro_log)) {
    tmtyro_log <- list()
  }

  this_step <- length(tmtyro_log) + 1L

  tmtyro_log[[this_step]] <- list(
    step      = this_step,
    fun       = fn,
    call      = call,
    narrative = get_narrative(fn = fn, arguments = arguments),
    version   = as.character(utils::packageVersion("tmtyro"))
  )

  attr(x, "tmtyro_log") <- tmtyro_log

  x
}

#' Describe the steps taken
#'
#' `narrativize()` provides a simple method for sharing methods used to prepare an analysis.
#'
#' @param data Data processed with one or more functions from tmtyro
#' @param format The format of output to return. Use `format = "bullets"` for well formatted bullets and `format = "text"` for paragraph-formatted text
#' @param person The personal pronoun to use in the narrative, if any
#' @param return How output should be returned, whether using `message()`, `print()`, or as a standard character string
#'
#' @returns Text
#' @export
#'
#' @section Examples:
#' ```r
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles(drop_articles = TRUE) |>
#'   add_frequency()
#'
#' dubliners |>
#'    narrativize()
#' ```
#' Returns the following:
#'  - Retrieved a corpus from Project Gutenberg using ID number 2814.
#'  - Loaded texts by tokenizing words, converting to lowercase, and preserving paragraph breaks.
#'  - Identified documents using the `part` column.
#'  - Standardized titles by converting to title case.
#'  - Counted the frequency of words used in each document.
#'
narrativize <- function(data, format = c("bullets", "text"), person = c(NA, "we", "I"), return = c("message", "character", "print")){
  format <- match.arg(format)
  person <- person[1]
  return <- match.arg(return)

  if(is.null(attr(data, "tmtyro_log"))) {
    return(NULL)
  }

  narrative <- attr(data, "tmtyro_log") |>
    purrr::map({\(x) x$narrative}) |>
    unlist()

  lower_first <- function(txt){
    if (!is.character(txt)) {
      return(txt)
    }
    if (length(txt) > 1) {
      txt |>
        strsplit("") |>
        sapply({\(x) c(tolower(x[1]), x[2:length(x)])}) |>
        sapply({\(x) paste0(unlist(x), collapse = "")})
    } else {
      txt |>
        strsplit("") |>
        sapply({\(x) c(tolower(x[1]), x[2:length(x)])}) |>
        unlist() |>
        paste0(collapse = "")
    }
  }

  upper_first <- function(txt){
    if (!is.character(txt)) {
      return(txt)
    }
    if (length(txt) > 1) {
      txt |>
        strsplit("") |>
        sapply({\(x) c(toupper(x[1]), x[2:length(x)])}) |>
        sapply({\(x) paste0(unlist(x), collapse = "")})
    } else if (nchar(txt) == 1) {
      toupper(txt)
    } else {
      txt |>
        strsplit("") |>
        sapply({\(x) c(toupper(x[1]), x[2:length(x)])}) |>
        unlist() |>
        paste0(collapse = "")
    }
  }

  total_steps <- length(narrative)

  if (!is.na(person)) {
    narrative <- paste("We", lower_first(narrative))
    if (total_steps > 1) {
      narrative[1] <- "First" |>
        paste(lower_first(narrative[1]))
    }

    if (total_steps >= 3) {
      narrative[total_steps] <- "Finally," |>
        paste(lower_first(narrative[total_steps]))
    }

    if (total_steps >= 2) {
      narrative[2] <- "Then" |>
        paste(lower_first(narrative[2]))
    }

    if (total_steps > 3) {
      narrative[3] <- "Next," |>
        paste(lower_first(narrative[3]))
    }
  }

  if (isTRUE(person != "we")) {
    Person <- upper_first(person)
    narrative <- narrative |>
      stringr::str_replace_all("\\bwe\\b", person) |>
      stringr::str_replace_all("\\bWe\\b", Person)
  }

  if (format == "text") {
    result <- narrative |>
      paste(collapse = " ")
  } else if (format == "bullets") {
    result <- paste(" -", narrative) |>
      paste(collapse = "\n")
  } else {
    result <- narrative
  }

  if (return == "message") {
    message(result)
  } else if (return == "character") {
    return(result)
  } else if (return == "print") {
    print(result)
  }
}
