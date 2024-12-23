#' Load a folder or data frame of texts
#'
#' `load_texts()` loads a corpus from a folder of texts or a data frame and prepares it for further study using tidytext principles. By default, `load_texts()` will add paragraph numbers (suitable for prose), and unnest at the word level, but options exist to change these defaults for poetry, to avoid unnesting, and even to remove words that seem like proper nouns or to apply techniques of natural language processing for lemmatizing words or tagging their parts of speech.
#'
#' @param src Either a string identifying the name of a directory containing texts or a data frame containing an unnested column called "text" and one column with a name ending in "_id". Files should either be stored in a directory within the project folder or under the subdirectory called "data". Defaults to "data" to load texts from that directory.
#' @param name What naming pattern to search for in this folder. Defaults to ".txt".
#' @param word Whether to split one word per line. Defaults to TRUE.
#' @param lemma Whether to lemmatize the text. When `word` is TRUE, adds a new column called `lemma`. This step can add a lot of time, so it defaults to FALSE.
#' @param lemma_replace When `lemma` and `word` are both TRUE, toggles whether to replace the `word` column with the lemmatized tokens. Defaults to FALSE
#' @param to_lower When `word` is TRUE, toggles whether to convert all words to lowercase. Defaults to TRUE.
#' @param remove_names When `word` is TRUE, toggles whether to remove words that only appear with the form of initial capitals. Defaults to FALSE.
#' @param pos Whether to add a column for part-of-speech tag. This step can add a lot of time, so it defaults to FALSE.
#' @param keep_original Whether to try to retain the original punctuation and capitalization in a parallel column. This won't always work, so it defaults to FALSE.
#' @param poetry Whether to detect and indicate stanza breaks and line breaks. Defaults to FALSE.
#' @param paragraph Whether to detect paragraph breaks for prose. Defaults to TRUE.
#' @param n The number of words per row. By default, `load_texts()` unnests a text one word at a time using a column called `word`. When `n` is a value greater than 1, `load_texts()` will instead use [tidytext::unnest_tokens()] with `token = "ngrams"` to create a column called `ngram`.
#' @param ... Additional arguments passed along to [tidytext::unnest_tokens()] for use with `tokenizers`
#'
#' @returns A data frame with two to five columns and one row for each token (optionally, one row for each paragraph or one row for each line) in the corpus.
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
    src = "data",
    name = ".txt",
    word = TRUE,
    lemma = FALSE,
    lemma_replace = FALSE,
    to_lower = TRUE,
    remove_names = FALSE,
    pos = FALSE,
    keep_original = FALSE,
    poetry = FALSE,
    paragraph = TRUE,
    n = 1L,
    ...) {
  if (length(class(src)) == 1 && "character" %in% class(src)) {
    if (!dir.exists(src) & !dir.exists(paste0("data/",src))) {
      stop(corpus_missing(src),
           call. = FALSE)
    } else if (dir.exists(src)) {
      src_dir <- src
    } else {
      src_dir <- paste0("data/", src)
    }

    the_files <-
      list.files(
        path = paste0(src_dir, "/"),
        pattern = name)

    full_corpus <-
      do.call(rbind,
              lapply(the_files,
                     load_one_text,
                     directory = src_dir,
                     poetry = poetry))
  } else {
    if (sum(stringr::str_detect(colnames(src), "_id")) == 1) {
      index_id <- grepl("_id", colnames(src))
      colnames(src)[index_id] <- "doc_id"
    } else {
      stop('When using `load_texts()` on an existing data frame, exactly one column needs to have a column name ending with "_id". Please rename columns accordingly and try again.')
    }

    full_corpus <- src
  }
  # browser()
  if (word & keep_original) {
    original_df <- full_corpus |>
      dplyr::mutate(
        text = text |>
          stringr::str_replace_all("\\s+(?=[.,!:-])",
                                   "") |>
          stringr::str_replace_all("\\s+(?=[\u201d])",# rdquo
                                   "") |>
          stringr::str_replace_all("\\s+(?=[\u2014])",# emdash
                                   "") |>
          stringr::str_replace_all("\\s+(?=[\u2013])",# endash
                                   "")) |>
      tidytext::unnest_tokens(
        output = word,
        input = text,
        token = "regex",
        pattern = "[[:space:]]|[.]{3}",
        to_lower = FALSE) |>
      dplyr::mutate(
        adjusted = tolower(word) |>
          stringr::str_remove_all("['\u2019](?=[:punct:])") |>
          stringr::str_remove_all("(?=[:punct:])") |>
          stringr::str_remove_all("[\U0336]?[\U0337]?[\U0338]?") |>
          stringr::str_remove_all("[:punct:]+$") |>
          stringr::str_remove_all("^[:punct:]+") |>
          stringr::str_extract_all("[[:alnum:].]+[.]+(?=['\u2019][:alnum:])|(?<=[[:alnum:]][.])[[:alnum:]'\u2019]+|[[:alnum:]'\u2019.,]+")) |> # debug here
      tidyr::unnest_longer(adjusted)

    original <- original_df |>
      dplyr::pull(word)
  }

  full_corpus <- full_corpus |>
    tidy_texts_internal(word, lemma, lemma_replace, to_lower, remove_names, pos, n, ...)

  if (word & keep_original) {
    if (nrow(full_corpus) == length(original)) {
      full_corpus <- full_corpus |>
        dplyr::mutate(
          original = original,
          .before = word) |>
        dplyr::mutate(
          original = dplyr::case_when(
            original == dplyr::lag(original) &
              word != dplyr::lag(word) ~ NA_character_,
            TRUE ~ original))
    } else {
      message("Something didn't work. See `original_df`.")
      original_df <<- original_df
    }

  }

  if (!paragraph & !poetry) {
    full_corpus <- full_corpus |>
      dplyr::select(-par_num)
  }

  full_corpus |>
    add_class("tmtyro")
}

tidy_texts_internal <- function(df, to_word, lemma, lemma_replace, to_lower, remove_names, pos = FALSE, n = 1, ...) {

  if (n > 1) {
    if (remove_names) {
      warning("`remove_names` is incompatible with n > 1. Consider filtering the `ngram` column manually.")
    }
    word <- FALSE
    remove_names <- FALSE
  }

  if (pos) {
    df <- df |>
      dplyr::filter(grepl("[a-zA-Z]", text)) |>
      annotate_pos()
  }

  if (to_word & !remove_names) {
    df <- df |>
      tidytext::unnest_tokens(word, text, to_lower = FALSE, ...)
  }

  if (n > 1) {
    df <- df |>
      tidytext::unnest_tokens(ngram, text, to_lower = FALSE, token = "ngrams", n = n, ...)
  }

  if (to_word & remove_names) {
    df <-
      dplyr::group_modify(
        .data = dplyr::group_by(df, doc_id),
        .f = ~ unnest_without_caps(.x, word, text, to_lower = FALSE)) |>
      dplyr::ungroup()
  }

  if (pos & to_word) {
    df <- df |>
      tidyr::separate(word, into = c("word", "pos"), sep = "__", fill = "right")
  }

  if (pos & n > 1) {
    df <- df |>
      dplyr::mutate(
        pos = ngram |>
          stringr::str_remove_all("\\b[a-zA-Z]+__"),
        ngram = ngram |>
          stringr::str_remove_all("__[a-zA-Z]+\\b")) |>
      dplyr::relocate(pos, .after = ngram)
  }

  if (lemma & to_word) {
    rlang::check_installed("textstem")
    df <- df |>
      dplyr::mutate(lemma = textstem::lemmatize_words(word))
  }

  if (lemma & n > 1) {
    rlang::check_installed("textstem")
    df <- df |>
      dplyr::mutate(lemma = textstem::lemmatize_strings(ngram))
  }

  if (lemma & to_word & lemma_replace) {
    df <- df |>
      dplyr::mutate(word = lemma) |>
      dplyr::select(-lemma)
  }

  if (lemma & n > 1 & lemma_replace) {
    df <- df |>
      dplyr::mutate(ngram = lemma) |>
      dplyr::select(-lemma)
  }

  if (lemma & !to_word & !n > 1) {
    rlang::check_installed("textstem")
    df <- df |>
      dplyr::mutate(text = textstem::lemmatize_strings(text))
  }

  if (to_lower) {
    df <- df |>
      dplyr::mutate(dplyr::across(dplyr::any_of(c("word", "lemma", "text", "ngram")), tolower))
  }

  if (pos) {
    df <- df |>
      dplyr::mutate(pos = stringr::str_replace_all(pos, "dollar", "$"))
  }

  return(df)
}

annotate_pos <- function(df){
  rlang::check_installed(c("NLP", "openNLP"))
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
      text = readLines(paste0(directory, "/", file), warn = FALSE)) |>
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

#' Prepare a corpus or corpora of texts
#'
#' `get_corpus()` works nearly identically as `load_texts()`, but it has two fundamental differences. First, it adds a "corpus" column to the resulting table to help with record keeping. Second, it adds an option for caching its output in a local RDS file, saved in the project directory.
#'
#' @inheritParams load_texts
#' @param corpus Vector of any length, where each value is either a string identifying a directory of texts or the first part of a filename to a cached RDS file prepared by tmtyro.
#' @param cache Whether to save a cached copy of the corpus. Some options like `pos = TRUE` and `lemma = TRUE` can add significant time to corpus preparation, so setting `cache = TRUE` saves the need to repeat steps each time a corpus is loaded. Defaults to TRUE.
#'
#' @returns A data frame with columns for corpus, doc_id, and other data.
#' @export
#'
#' @examples
#' \dontrun{
#'   austen <- get_corpus("austen")
#'
#'   shakespeare <- get_corpus(
#'     c("comedy",
#'       "history",
#'       "tragedy"))
#' }
get_corpus <- function(
    corpus,
    name = ".txt",
    word = TRUE,
    lemma = FALSE,
    lemma_replace = FALSE,
    to_lower = TRUE,
    remove_names = FALSE,
    pos = FALSE,
    poetry = FALSE,
    paragraph = TRUE,
    cache = TRUE) {

  corpus_internal <- function(
    corpus,
    name,
    word,
    lemma,
    lemma_replace,
    to_lower,
    remove_names,
    pos,
    poetry,
    paragraph,
    cache) {
    x_rds <- paste0(corpus, ".rds")

    if (file.exists(x_rds) & cache) {
      df <- readRDS(x_rds)
    } else {
      if (!dir.exists(corpus)) {
        stop(corpus_missing(corpus, cache),
             call. = FALSE)
      }

      df <- load_texts(
        src = corpus,
        name,
        word,
        lemma,
        lemma_replace,
        to_lower,
        remove_names,
        pos,
        poetry,
        paragraph)
      if (cache) saveRDS(df, x_rds)
    }

    df |>
      dplyr::mutate(corpus = corpus) |>
      dplyr::relocate(corpus)
  }

  corpus |>
    purrr::map(\(x) {corpus_internal(
      x, name, word,
      lemma, lemma_replace, to_lower,
      remove_names, pos, poetry,
      paragraph, cache)}) |>
    dplyr::bind_rows()
}

corpus_missing <- function(corpus, cache = FALSE) {
  msg_1 <- stringr::str_glue("No directory or folder called '{corpus}/' could be found, neither in the working directory nor in the 'data/' subdirectory.")
  msg2 <- ifelse(cache, stringr::str_glue("Additionally, no cache file 'data/{corpus}.RDS' was found."), "")
  msg_action <- "Please relocate files and try again."
  paste(msg_1, msg2, msg_action)
}

#' Choose a new doc_id column
#'
#' @param data A data frame, potentially with a `doc_id` column
#' @param ... The column or columns that will become the new identifier
#' @param inorder Whether to establish `doc_id` order as shown in the document
#' @param sep Separator between values when identifying by multiple columns
#'
#' @returns A data frame with a redefined doc_id column
#' @export
#'
#' @examples
#' if (FALSE) {
#'   corpus |>
#'     load_texts() |>
#'     identify_by(author)
#' }
identify_by <- function(
    data,
    ...,
    inorder = TRUE,
    sep = "_") {
  cols <- colnames(data)
  relevant <- lapply(substitute(list(...))[-1], deparse) |>
    unlist()
  if (!"doc_id" %in% relevant) {
    cols <- cols[cols != "doc_id"]
  }
  # if (drop) {
  #   data <- data |>
  #     dplyr::select(tidyr::all_of(cols))
  # } else if (!is.null(old)) {
  #   data <- data |>
  #     dplyr::mutate({{ old }} := doc_id)
  # } else {
  #   new_col <- 1
  #   while (paste0("other_", new_col) %in% cols) {
  #     new_col <- new_col + 1
  #   }
  #   new_colname <- paste0("other_", new_col)
  #   data <- data |>
  #     dplyr::mutate({{ new_colname }} := doc_id)
  # }
  data <- data |>
    tidyr::unite(doc_id, ...,
                 sep = sep,
                 remove = FALSE) |>
    dplyr::relocate(doc_id)
  if (inorder) {
    data <- data |>
      dplyr::mutate(doc_id = forcats::fct_inorder(doc_id))
  }
  data
}

add_class <- function(x, class, remove = NULL) {
  class(x) <- append(class, class(x)[!class(x) %in% remove])
  x
}

set_feature <- function(x, feature) {
  attr(x, "feature") <- feature
  x
}
