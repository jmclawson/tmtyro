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

# narrative_dictionary <- tibble::tribble(
#   ~fn, ~narrative,
#   "get_gutenberg_corpus", "Retrieved a corpus from Project Gutenberg using {parameters}.",#
#   "load_texts", "Loaded texts{parameters}.",#
#   "identify_by", "Identified documents using the {relevant_column}.",#
#   "standardize_titles", "Standardized titles by converting to title case{parameters}.",#
#   "add_dictionary", "Assigned values to each {feature} using a {lexicon} dictionary.",
#   "add_frequency", "Counted the frequency of {feature}s used in each document.",#
#   "add_index", "Created a document-level index.",#
#   "add_ngrams", "Constructed {n}-gram sequences from the text.",#
#   "add_partitions", "Partitioned the text into segments of approximately {width} {feature}s with {overlap}.",#
#   "add_sentiment", "Assigned {lexicon} sentiment values to each {feature}.",#
#   "add_tf_idf", "Calculated TF-IDF values for each {feature} across documents.",#
#   "add_vocabulary", "Measured uniquenesss of {feature}s over time.",
#   "combine_ngrams", "Merged sequential tokens to form {n}-gram units.",
#   "drop_na", "Removed rows with missing or incomplete values in {column}.",
#   "drop_stopwords", "Removed stopwords based on the {lexicon} list.",
#   "expand_documents", "Converted token-level data into a document-feature matrix.",
# )
#
# parameter_dictionary <- tibble::tribble(
#   ~fn, ~parameter, ~prefix, ~suffix, ~plural_pre, ~plural_suf,
#   "get_gutenberg_corpus", "gutenberg_id", "ID number", NA, "ID numbers", NA,
#   "load_texts", "", "by", NA, NA, NA,
#   "load_texts", "word", "tokenizing words", NA, NA, NA,
#   "load_texts", "lemma", "detecting lemmas", NA, NA, NA,
#   "load_texts", "to_lower", "converting to lowercase", NA, NA, NA,
#   "load_texts", "remove_names", "removing names", NA, NA, NA,
#   "load_texts", "pos", "detecting parts of speech", NA, NA, NA,
#   "load_texts", "poetry", "preserving line breaks", NA, NA, NA,
#   "load_texts", "paragraph", "preserving paragraph breaks", NA, NA, NA,
#   "standardize_titles", "drop_articles", "and dropping introductory articles", NA, NA, NA
# )

#' Default narrative dictionary
#'
#' A reference dictionary of English narrative templates used by
#' \code{\link{narrativize}} to describe a \pkg{tmtyro} workflow
#'
#' Each row corresponds to either:
#' \enumerate{
#'   \item a function-level narrative template (i.e., when \code{fn} is present and \code{parameter = NA}),
#'   \item a parameter-level label or phrase (when \code{fn} and \code{parameter} are present), or
#'   \item a shared lookup value (when \code{fn = NA} and \code{parameter = NA}) used for general substitutions (e.g. using \code{"bigram"} in place of \code{"2-gram"}).
#' }
#'
#' Templates may include placeholders in braces (for example \code{\{parameters\}},
#' \code{\{feature\}}, \code{\{lexicon\}}). When customizing or translating the dictionary,
#' preserve these placeholder names exactly, modifying only the surrounding prose.
#'
#' This object is exported primarily as a model for creating custom narrative
#' dictionaries for other languages, institutional tone, or personal style.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{fn}{Function name key (character). Use \code{NA} for shared lookup rows.}
#'   \item{parameter}{Parameter or lookup key (character) or \code{NA}.}
#'   \item{narrative}{Function-level narrative template or lookup text (character) or \code{NA}.}
#'   \item{prefix}{Optional prefix used when assembling parameter phrases (character) or \code{NA}.}
#'   \item{suffix}{Optional suffix used when assembling parameter phrases (character) or \code{NA}.}
#'   \item{plural_pre}{Optional plural form for \code{prefix} (character) or \code{NA}.}
#'   \item{plural_suf}{Optional plural form for \code{suffix} (character) or \code{NA}.}
#' }
#'
#' @examples
#' narrative_dictionary_en
#'
#' # Use as a model to customize a dictionary
#'
#' \dontrun{
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' narrativize(
#'   dubliners,
#'   dictionary = narrative_dictionary_en)
#' }
#'
#' @seealso \code{\link{narrativize}}
#' @export
narrative_dictionary_en <- tibble::tribble(
  ~fn, ~parameter, ~narrative, ~prefix, ~suffix, ~plural_pre, ~plural_suf,
  "get_gutenberg_corpus", NA, "Retrieved a corpus from Project Gutenberg using {parameters}.", NA, NA, NA, NA, #
  "get_gutenberg_corpus", "gutenberg_id", NA, "ID number", NA, "ID numbers", NA,
  "load_texts", NA, "Loaded texts{parameters}.", NA, NA, NA, NA, #
  "load_texts", "parameters", NA, "by", NA, NA, NA,
  "load_texts", "word", "tokenizing words", NA, NA, NA, NA,
  "load_texts", "lemma", "detecting lemmas", NA, NA, NA, NA,
  "load_texts", "to_lower", "converting to lowercase", NA, NA, NA, NA,
  "load_texts", "remove_names", "removing names", NA, NA, NA, NA,
  "load_texts", "pos", "detecting parts of speech", NA, NA, NA, NA,
  "load_texts", "poetry", "preserving line breaks", NA, NA, NA, NA,
  "load_texts", "paragraph", "preserving paragraph breaks", NA, NA, NA, NA,
  "identify_by", NA, "Identified documents using the {relevant_column}.", NA, NA, NA, NA, #
  "standardize_titles", NA, "Standardized titles by converting to title case{parameters}.", NA, NA, NA, NA, #
  "standardize_titles", "drop_articles", "and dropping introductory articles", NA, NA, NA, NA,
  "add_dictionary", NA, "Assigned values to each {feature} using a {lexicon} dictionary.", NA, NA, NA, NA,
  "add_frequency", NA, "Counted the frequency of {feature}s used in each document.", NA, NA, NA, NA, #
  "add_index", NA, "Created a document-level index.", NA, NA, NA, NA, #
  "add_progress", NA, "Recorded document-level progress for each feature.", NA, NA, NA, NA, #
  "add_ngrams", NA, "Constructed {n}-gram sequences from the text.", NA, NA, NA, NA, #
  "add_partitions", NA, "Partitioned the text into segments of approximately {width} {feature}s with {overlap}.", NA, NA, NA, NA, #
  "add_sentiment", NA, "Assigned {lexicon} sentiment values to each {feature}.", NA, NA, NA, NA, #
  "add_tf_idf", NA, "Calculated TF-IDF values for each {feature} across documents.", NA, NA, NA, NA, #
  "add_vocabulary", NA, "Measured cumulative uniqueness of {feature}s in each document.", NA, NA, NA, NA,
  "combine_ngrams", NA, "Merged sequential tokens to form {n}-gram units.", NA, NA, NA, NA,
  "drop_na", NA, "Removed rows with missing or incomplete values in {column}.", NA, NA, NA, NA,
  "drop_stopwords", NA, "Removed stopwords based on the {lexicon} list.", NA, NA, NA, NA,
  "expand_documents", NA, "Converted token-level data into a document-feature matrix.", NA, NA, NA, NA,
  NA, "and", "and", NA, NA, NA, NA,
  NA, "2-gram", "bigram", NA, NA, NA, NA,
  NA, "3-gram", "trigram", NA, NA, NA, NA,
)

# get_narrative_0 <- function(fn, feature = "word", document = "document", arguments = NULL) {
#   # arguments should be a named list like c("by" = "doc_id")
#   if (!is.null(arguments)) {
#     arguments_names <- paste0("\\{", names(arguments), "\\}")
#     arguments <- as.character(arguments) |>
#       setNames(arguments_names)
#   } else {
#     arguments <- c("\\{feature\\}" = "token")
#   }
#   narrative_dictionary[["narrative"]][narrative_dictionary$fn == fn] |>
#     stringr::str_replace_all(c(
#       "word" = feature,
#       "doc_id" = document)
#     ) |>
#     stringr::str_replace_all(arguments) |>
#     stringr::str_replace_all("2-gram", "bigram") |>
#     stringr::str_replace_all("3-gram", "trigram")# |>
#   # stringr::str_remove_all("[{}]")
# }

get_narrative <- function(logstep, dictionary = NULL) {
  nd <- dictionary %||% narrative_dictionary_en
  # arguments should be a named list like list("by" = "doc_id")
  if (!is.null(logstep$arguments)) {
    arguments <- logstep$arguments
    fn <- logstep$fun

    if (!is.null(arguments$parameters)) {
      arguments$parameters <- prep_parameters(logstep, dictionary = nd)
    }

    arguments_names <- paste0("\\{", names(arguments), "\\}")
    arguments <- as.character(arguments) |>
      setNames(arguments_names)
  } else {
    arguments <- c("\\{feature\\}" = "token")
  }

  small_replacements <- nd[["narrative"]][is.na(nd$fn) & !is.na(nd$parameter)] |>
    setNames(paste0(
      "\\b",
      nd[["parameter"]][is.na(nd$fn) & !is.na(nd$parameter)],
      "\\b"))

  nd[["narrative"]][nd$fn == fn & is.na(nd$parameter)] |>
    # stringr::str_replace_all(c(
    #   "word" = feature,
    #   "doc_id" = document)
    # ) |>
    stringr::str_replace_all(arguments) |>
    stringr::str_squish() |>
    stringr::str_replace_all(small_replacements)# |>
    # stringr::str_replace_all("2-gram", "bigram") |>
    # stringr::str_replace_all("3-gram", "trigram")# |>
  # stringr::str_remove_all("[{}]")
}

prep_parameters <- function(logstep, dictionary = NULL) {
  pd <- dictionary
  fn <- unlist(logstep$fun)
  parameters <- logstep$arguments$parameters
  pd <- pd |>
    dplyr::mutate(
      prefix = ifelse(is.na(prefix), "", prefix),
      suffix = ifelse(is.na(suffix), "", suffix),
      plural_pre = ifelse(!is.na(plural_pre), plural_pre, prefix),
      plural_suf = ifelse(!is.na(plural_suf), plural_suf, suffix)
    )

  output <- c()

  fn_pd <- pd[!is.na(pd$fn) & !is.na(pd$parameter) & pd$fn == fn & pd$parameter == "parameters",]

  main_prefix <- fn_pd[["prefix"]]
  main_suffix <- fn_pd[["suffix"]]

  for (i in 1:length(parameters)) {
    p <- unlist(parameters[i])
    if (is.character(p) || is.numeric(p)) {
      is_plural <- length(p) > 1
      pre_v <- ifelse(is_plural, "plural_pre", "prefix")
      suf_v <- ifelse(is_plural, "plural_suf", "suffix")

      this_pd <- pd[pd$fn == fn & (!is.na(pd$parameter) & pd$parameter == names(p)), ]

      this_narrative <- this_pd$narrative
      this_prefix <- this_pd[[pre_v]]
      this_suffix <- this_pd[[suf_v]]

      # include flattened p for character / numeric
      this_p <- stringr::str_flatten_comma(unname(p), last = ", and ")

      if (is.na(this_narrative)) {
        this_output <- paste(this_prefix, this_p, this_suffix) |>
          stringr::str_squish()
      } else {
        this_output <- stringr::str_squish(this_narrative)
      }

      output <- c(output, this_output)
    } else if (is.logical(p) && isTRUE(p)) {
      this_pd <- pd[!is.na(pd$fn) & !is.na(pd$parameter) & pd$fn == fn & pd$parameter == names(p),]
      this_output <- this_pd[["narrative"]]
      output <- c(output, this_output)
    }
  }

  output <- stringr::str_flatten_comma(output, last = ", and ")

  if (length(main_prefix) > 0 && !is.na(main_prefix)) {
    output <- paste(main_prefix, output)
  }

  if (length(main_suffix) > 0 && !is.na(main_suffix)) {
    output <- paste(output, main_suffix)
  }

  if (nzchar(output)) {
    output <- paste0(" ", output)
  }

  output |>
    stringr::str_remove_all("[ ]$")
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
    call      = call,
    fun       = fn,
    arguments = arguments %||% list(),
    # narrative = get_narrative(fn = fn, arguments = arguments),
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
#' @param dictionary The narrative dictionary to use. [`narrative_dictionary_en`] is defined by default.
#' @param return How output should be returned, whether using `message()`, `print()`, or as a standard character string
#' @param person The personal pronoun to use in the narrative, if any. This parameter depends on the narrative dictionary defined in `dictionary`.
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
narrativize <- function(data, format = c("bullets", "text"), dictionary = narrative_dictionary_en, return = c("message", "character", "print"), person = c(NA, "we", "I")){
  format <- match.arg(format)
  person <- person[1]
  return <- match.arg(return)

  if(is.null(attr(data, "tmtyro_log"))) {
    return(NULL)
  }

  narrative <- attr(data, "tmtyro_log") |>
    purrr::map({\(x) get_narrative(x, dictionary = dictionary)}) |>
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
