# Check for the label option in three places:
# 1. Directly, when called
# 2. In a document-level option
# 3. Among environment variables
# As a fallback, default to TRUE
tmtyro_use_labels <- function(label = NULL) {
  if (!is.null(label)) {
    return(isTRUE(label))
  }

  opt <- getOption("tmtyro.use_labels", NULL)
  if (!is.null(opt)) {
    return(isTRUE(opt))
  }

  env <- Sys.getenv("TMTYRO_USE_LABELS", NA_character_)
  if (!is.na(env) && nzchar(env)) {
    return(tolower(env) %in% c("1", "true", "yes", "on"))
  }

  TRUE
}

label_dictionary <- tibble::tribble(
  ~var, ~terse, ~long,
  "df", "document frequency", "documents containing word",
  "hapax_doc", "document singleton", "sole use of word in document",
  "hapax_corpus", "corpus singleton", "sole use of word in corpus",
  "hir", "hapax introduction ratio", "hapax introduction ratio",
  "idf", "inverse document frequency", "inverse document frequency",
  "index", "document index", "document index",
  "progress", "document index", "document index",
  "progress_unit", "secondary in document", "secondary in document",
  "lemma", "word stem", "basic word stem",
  "n", "word count", "word count in doc_id",
  "new_word", "new use of word", "new use of word in doc_id",
  "ngram", "secondary-word sequence", "secondary-word sequence",
  "pos", "part of speech", "part of speech tag",
  "partition", "partition of ~secondary words", "partition of ~secondary words with ~tertiary overlap in doc_id",
  "progress_words", "words so far", "words so far in doc_id",
  # "progress_percent", "percentage of document", "percentage of doc_id",
  "sentiment", "secondary sentiment", "secondary sentiment of word",
  "tf", "term frequency", "word frequency in doc_id",
  "tf_idf", "term frequency-inverse document frequency", "term frequency-inverse doc_id frequency",
  "ttr", "text-token ratio", "text-token ratio",
  "vocabulary", "count of new words", "count of new words in doc_id"
)

get_label <- function(var, feature = "word", document = "document", secondary = NULL, tertiary = NULL, choice = c("terse", "long")) {
  the_choice <- match.arg(choice)[1]
  label_dictionary[[the_choice]][label_dictionary$var == var] |>
    stringr::str_replace_all(c(
      "word" = feature,
      "doc_id" = document,
      "secondary" = secondary,
      "tertiary" = tertiary)
    ) |>
    stringr::str_remove_all("[(][)]") |>
    stringr::str_squish()
}

assign_label_from <- function(data, var, feature, document = "document", secondary = NULL, tertiary = NULL, from_var = var) {
  attr(data[[var]], "label") <- get_label(from_var, feature, document, secondary, tertiary)
  data
}

assign_labels <- function(data, vars, feature, document = "document", secondary = NULL, tertiary = NULL) {
  for (var in vars) {
    attr(data[[var]], "label") <- get_label(var, feature, document, secondary, tertiary)
  }
  data
}

#' Drop variable labels
#'
#' @param data A data frame possibly containing labels
#' @param vars A vector of column names to be stripped of their labels. If this optional argument is not supplied, labels will be dropped for every column
#'
#' @returns An unlabeled data frame in the same format as `data`
#' @export
#'
#' @examples
#' \dontrun{
#'   drop_labels(vocab_dubliners)
#' }
drop_labels <- function(data, vars = NULL) {
  vars <- vars %||% colnames(data)
  for (var in vars) {
    attr(data[[var]], "label") <- NULL
  }
  data
}

prettify_labels <- function(data, vars) {
  for (var in vars) {
    attr(data[[var]], "label") <- var |>
      stringr::str_replace_all("_", " ")
  }
  data
}

#' Prepare a data dictionary from column labels
#'
#' @param data A tmtyro data frame
#' @param only_labeled Whether to return only the variables with descriptive labels
#'
#' @returns data frame with three columns: `variable`, `label`, and `class`
#' @export
#'
#' @examples
#' \dontrun{
#'   get_data_dictionary(vocab_dubliners)
#' }
get_data_dictionary <- function(data, only_labeled = FALSE) {
  named_vector <- data |>
    purrr::map_chr({\(x) attr(x, "label") %||% NA})
  var_class <- data |>
    purrr::map_chr(class) |>
    unname()
  out <- data.frame(
    variable = names(named_vector),
    label = unname(named_vector),
    class = var_class
  )
  if (only_labeled) out <- out[!is.na(out$label),]
  out
}

#' Assign column labels from a data dictionary
#'
#' @param data A tmtyro data frame
#' @param dictionary data frame containing at least two columns, `variable` and `label`
#'
#' @returns labeled data frame with the same structure as `data`
#' @export
#'
#' @examples
#' \dontrun{
#'   dubliners_dictionary <- data.frame(
#'     variable = "doc_id",
#'      label = "story title"
#'   )
#'
#'   vocab_dubliners <- vocab_dubliners |>
#'     set_data_dictionary(dubliners_dictionary)
#' }
set_data_dictionary <- function(data, dictionary) {
  var_labels <- dictionary$label[!is.na(dictionary$label)] |>
    setNames(dictionary$variable[!is.na(dictionary$label)])
  for (i in 1:length(var_labels)) {
    attr(data[[names(var_labels)[i]]], "label") <- unname(var_labels)[i]
  }
  data
}
