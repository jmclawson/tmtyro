#' Load a folder of texts
#'
#' `load_texts()` loads a corpus from a folder of texts and prepare it for further study using tidytext principles. By default, `load_texts()` will add paragraph numbers (suitable for prose), and unnest at the word level, but options exist to change these defaults for poetry, to avoid unnesting, and even to remove words that seem like proper nouns or to apply techniques of natural language processing for lemmatizing words or tagging their parts of speech.
#'
#' @param folder A directory containing prose texts. Defaults to "data".
#' @param name What naming pattern to search for in this folder. Defaults to ".txt".
#' @param word Whether to split one word per line. Defaults to TRUE.
#' @param lemma Whether to lemmatize the text. When `word` is TRUE, adds a new column called `lemma`. This step can add a lot of time, so it defaults to FALSE.
#' @param lemma_replace When `lemma` and `word` are both TRUE, toggles whether to replace the `word` column with the lemmatized tokens. Defaults to FALSE
#' @param to_lower When `word` is TRUE, toggles whether to convert all words to lowercase. Defaults to TRUE.
#' @param remove_names When `word` is TRUE, toggles whether to remove words that only appear with the form of initial capitals. Defaults to FALSE.
#' @param pos Whether to add a column for part-of-speech tag. This step can add a lot of time, so it defaults to FALSE.
#' @param poetry Whether to detect and indicate stanza breaks and line breaks. Defaults to FALSE.
#' @param paragraph Whether to detect paragraph breaks for prose. Defaults to TRUE.
#'
#' @returns A data frame with two to five columns and one row for each word (optionally, one row for each paragraph or one row for each line) in the corpus.
#' @export
#'
#' @examples
#' \dontrun{
#' mysteries <-
#'   load_texts("mystery-novels")
#'
#' dickinson <-
#'   load_texts("dickinson-poems",
#'              poetry = TRUE)
#'
#' # `load_texts()` can also be used with
#' # a traditional tidytext workflow:
#' mysteries <-
#'   load_texts("mystery-novels",
#'              word = FALSE,
#'              to_lower = FALSE) |>
#'   tidytext::unnest_tokens(word, text)
#' }
load_texts <- function(
    folder = "data",
    name = ".txt",
    word = TRUE,
    lemma = FALSE,
    lemma_replace = FALSE,
    to_lower = TRUE,
    remove_names = FALSE,
    pos = FALSE,
    poetry = FALSE,
    paragraph = TRUE) {
  the_files <-
    list.files(path = paste0(folder, "/"),
               pattern = name)

  full_corpus <-
    do.call(rbind,
            lapply(the_files,
                   load_one_text,
                   directory = folder,
                   poetry = poetry))

  full_corpus <- full_corpus |>
    tidy_texts_internal(word, lemma, lemma_replace, to_lower, remove_names, pos)

  if (!paragraph & !poetry) {
    full_corpus <- full_corpus |>
      dplyr::select(-par_num)
  }

  return(full_corpus)
}

tidy_texts_internal <- function(df, word, lemma, lemma_replace, to_lower, remove_names, pos = FALSE) {

  if (pos) {
    df <- df |>
      annotate_pos()
  }

  if (word & !remove_names) {
    df <- df |>
      tidytext::unnest_tokens(word, text, to_lower = FALSE)
  }

  if (word & remove_names) {
    df <-
      dplyr::group_modify(
        .data = dplyr::group_by(df, doc_id),
        .f = ~ unnest_without_caps(.x, word, text, to_lower = FALSE)) |>
      dplyr::ungroup()
  }

  if (pos & word) {
    df <- df |>
      tidyr::separate(word, into = c("word", "pos"), sep = "__", fill = "right")
  }

  if (lemma & word) {
    df <- df |>
      dplyr::mutate(lemma = textstem::lemmatize_words(word))
  }

  if (lemma & word & lemma_replace) {
    df <- df |>
      dplyr::mutate(word = lemma) |>
      dplyr::select(-lemma)
  }

  if (lemma & !word) {
    df <- df |>
      dplyr::mutate(text = textstem::lemmatize_strings(text))
  }

  if (to_lower) {
    df <- df |>
      dplyr::mutate(dplyr::across(dplyr::any_of(c("word", "lemma", "text")), tolower))
  }

  if (pos) {
    df <- df |>
      dplyr::mutate(pos = stringr::str_replace_all(pos, "dollar", "$"))
  }

  return(df)
}

annotate_pos <- function(df){
  ##### NLP section #####
  sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
  word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  pos_tag_annotator <- openNLP::Maxent_POS_Tag_Annotator()
  annotator_pipeline <-
    NLP::Annotator_Pipeline(sent_token_annotator,
                            word_token_annotator,
                            pos_tag_annotator)

  # function for annotation
  annotateDocuments <- function(doc) {
    doc <- doc |>
      stringr::str_remove_all("_") |>
      stringr::str_replace_all("\u2014{2,}"," ") |>
      stringr::str_replace_all("[-]{2,3}"," ") |>
      stringr::str_replace_all("[-]+","_") |>
      NLP::as.String()
    doc_with_annotations <- NLP::annotate(doc, annotator_pipeline)
    tags <- sapply(subset(doc_with_annotations, type=="word")$features, `[[`, "POS")
    tokens <- doc[subset(doc_with_annotations, type=="word")]
    res <- paste0(tokens, "__", tags)
    res <- paste(res, collapse = " ")
    return(res)
  }

  df |>
    dplyr::rowwise() |>
    dplyr::mutate(text = annotateDocuments(text) |>
             stringr::str_replace_all("[$] ","dollar ") |>
             stringr::str_replace_all("[$]$","dollar") |>
             stringr::str_replace_all("([a-zA-Z])(?!')[[:punct:]]+[_]{2}([A-Z])", "\\1__\\2") |>
             stringr::str_remove_all(" [:punct:]__[^ ]+") |>
               stringr::str_remove_all("^[:punct:]__[^ ]+ ")) |>
    dplyr::ungroup()
}


load_one_text <- function(file, directory, poetry = FALSE) {
  df <-
    tibble::tibble(
      doc_id = stringr::str_remove_all(file, "[.].*"),
      text = readLines(paste0(directory, "/", file))) |>
    dplyr::filter(!(text == "" & dplyr::lead(text) == ""))

  if (!poetry) {
    df <- df |>
      dplyr::mutate(par_num = cumsum(text == ""),
                    .after = doc_id) |>
      dplyr::filter(text != "") |>
      dplyr::mutate(par_num = par_num - min(par_num, na.rm = TRUE) + 1)
  } else {
    df <- df |>
      dplyr::mutate(stanza_num = cumsum(text == "") + 1) |>
      dplyr::filter(text != "") |>
      dplyr::mutate(line_num = dplyr::row_number()) |>
      dplyr::relocate(stanza_num, line_num,
                      .after = doc_id)
  }

  return(df)
}
