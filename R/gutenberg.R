#' Build and load a corpus from Project Gutenberg
#'
#' `get_gutenberg_corpus()` improves upon the functionality
#' of [gutenbergr::gutenberg_download()] in three key ways.
#' \enumerate{
#' \item First, it retrieves the ".htm" version of texts and
#' parses the headers. This change allows for consideration
#' of texts by sections and chapters. Parsing is handled by
#' [parse_html()], and [move_header_to_text()] is available
#' to correct any parsing errors.
#' \item Second, it caches a copy of files locally to avoid
#' the need for repeated downloads. This change helps with
#' code portability, offline access, and server bandwidth.
#' }
#' All changes are made with consideration for server bandwidth,
#' so a two-second delay is introduced between each download
#' attempt. This will slow down the initial acquisition of
#' corpora, but offline caching speeds things up considerably
#' in subsequent use.
#'
#' @param gutenberg_id A vector of ID numbers from Project Gutenberg or a data frame containing a `gutenberg_id` column, such as from the results of a call to [gutenbergr::gutenberg_works()].
#' @param dir The directory for storing downloaded `.txt` files. Default value is "gutenberg".
#' @param meta_fields Additional fields to add from [gutenbergr::gutenberg_metadata] describing each book. By default, title and author are added.
#' @param html_title Whether to use the h1 header from an HTML file to determine a document's title. By default, uses [gutenbergr::gutenberg_metadata].
#' @param ... Additional parameters passed along to [gutenbergr::gutenberg_strip()].
#'
#' @returns A data frame with one row for each line of the texts in the corpus.
#' @export
#'
#' @examples
#' library(gutenbergr)
#'
#' dalloway <- gutenberg_works(author == "Woolf, Virginia",
#'                             title == "Mrs Dalloway in Bond Street") |>
#'   get_gutenberg_corpus()
get_gutenberg_corpus <- function(
    gutenberg_id, dir = "gutenberg",
    meta_fields = c("gutenberg_id", "title", "author"),
    html_title = FALSE,
    ...) {

    if ("data.frame" %in% class(gutenberg_id) &&
      "gutenberg_id" %in% colnames(gutenberg_id)) {
    gutenberg_id <- gutenberg_id[["gutenberg_id"]]
  } else if ("data.frame" %in% class(gutenberg_id)) {
    stop("A `gutenberg_id` column is necessary when using a data frame.")
  }

  make_url <- function(x) {
    y <- if (as.numeric(x) > 10) {
      strsplit(x, "") |>
        unlist() |>
        {\(x) x[1:(length(x)-1)]}() |>
        paste0(collapse = "/")
    } else {
      "0"
    }
    stringr::str_c(
      gutenbergr::gutenberg_get_mirror(verbose = FALSE),
      y, x,
      stringr::str_c(x, "-h"),
      stringr::str_c(x, "-h.htm"),
      sep = "/")
  }

  gut_df <-
    data.frame(
      id = as.character(gutenberg_id)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      url = make_url(id)) |>
    dplyr::ungroup()

  # save downloads locally to avoid repeats
  get_book <- function(url) {
    base_id <- stringr::str_extract(
      url,
      "\\d*(?=\\-h\\.htm)")
    the_book <- suppressWarnings(
      download_once(
        url,
        filename = paste0(base_id, ".htm"),
        destdir = dir))
    if (!is.null(the_book)) {
      return(the_book)
    }
    warnings(paste("Could not get", url))
    NULL
  }

  # add a 2-second delay between books
  download_slowly <- purrr::slowly(\(x) get_book(x),
                  rate = purrr::rate_delay(2), quiet = TRUE)

  if (dir.exists(dir) &&
      length(dir(path = dir)) > 0) {
    already_existing <- dir(path = dir, pattern = "htm") |>
      stringr::str_replace_all(
        "(?<=\\d)[.]htm",
        "-h.htm") |>
      paste0(collapse="|")
    some_urls <- gut_df$url |>
      stringr::str_subset(already_existing, negate = TRUE)
    if(length(some_urls) > 0) {
      some_urls |>
        purrr::walk(\(x) download_slowly(x))
    }
  } else {
    gut_df$url |>
      purrr::walk(\(x) download_slowly(x))
  }

  ids <- sort(gut_df$id)
  collapsed_ids <- paste0("/", ids, ".htm", collapse = "|")
  all_files <- dir(path = dir,
                   pattern = paste0(".htm$"),
                   full.names = TRUE)
  guten_files <- all_files[grepl(collapsed_ids, all_files)]
  id <- guten_files |>
    stringr::str_remove_all(paste0(dir, "/")) |>
    stringr::str_remove_all(".htm")

  the_books <- guten_files |>
    stats::setNames(id) |>
    purrr::map(\(x) parse_html(x, title = html_title)) |>
    purrr::discard(is.null) |>
    dplyr::bind_rows(.id = "gutenberg_id") |>
    dplyr::relocate(text, .after = tidyr::last_col()) |>
    dplyr::mutate(gutenberg_id = as.integer(gutenberg_id))

  if (length(meta_fields) > 0) {
    if (html_title) {
      meta_fields <- meta_fields[meta_fields != "title"]
    }
    the_books <- the_books[,!colnames(the_books) %in% meta_fields[meta_fields != "gutenberg_id"]]
    meta_fields <- unique(c("gutenberg_id", meta_fields))
    md <- gutenbergr::gutenberg_metadata[meta_fields]
    the_books <- dplyr::right_join(md, by = "gutenberg_id", the_books)
  }

  the_books
}


#' Read HTML headers and text from file
#'
#' @param html A file in HTML format
#' @param title Whether to keep H1 tags even when there is only one unique value
#'
#' @returns A data frame with a column called `text` and header columns called `title`, `part`, `section`, and `subsection` as needed. Header columns are limited to page elements tagged as h1, h2, h3, or h4.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   library(dplyr)
#'   library(stringr)
#'   library(tmtyro)
#'
#'   orlando <-
#'     "http://gutenberg.net.au/ebooks02/0200331h.html" |>
#'     download_once() |>
#'     parse_html() |>
#'     filter(str_detect(part, "CHAPTER")) |>
#'     mutate(
#'       chapter = str_extract(part, "\\d"),
#'       author = "Virginia Woolf") |>
#'     select(author, title, chapter, text) |>
#'     drop_na(chapter) |>
#'     identify_by(title, chapter) |>
#'     load_texts()
#' }
parse_html <- function(html, title = TRUE){
  found <- html |>
    rvest::read_html() |>
    rvest::html_elements("h1, h2, h3, h4, p")

  types <- found |>
    rvest::html_name()

  contents <- found |>
    rvest::html_text2() |>
    stringr::str_replace_all("\n", " ") |>
    stringr::str_replace_all("\r", " ") |>
    stringr::str_replace_all("[ ]+", " ") |>
    trimws()

  table <- data.frame(tag = types, text = contents) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      h1 = dplyr::if_else(tag == "h1", text, NA_character_),
      h2 = dplyr::if_else(tag == "h2", text, NA_character_),
      h3 = dplyr::if_else(tag == "h3", text, NA_character_),
      h4 = dplyr::if_else(tag == "h4", text, NA_character_),
      .before = text
    ) |>
    dplyr::mutate(
      text = dplyr::if_else(tag == "p", text, NA_character_)
    ) |>
    dplyr::select(-tag) |>
    tidyr::fill(h1, h2, h3, h4) |>
    tidyr::drop_na(text) |>
    dplyr::select(dplyr::where(function(x) mean(is.na(x)) < 1))

  if (title) {
    table <- table |>
      dplyr::select(
        c(colnames(table)[colnames(table) == "h1"],
          dplyr::where(~dplyr::n_distinct(.) > 1))
      )

    section_names <- colnames(table)[colnames(table) != "text"]

    colnames(table)[colnames(table) != "text"][1:ifelse(length(section_names) >= 4, 4, length(section_names))] <- c("title","part", "section", "subsection")[1:length(section_names)]

  } else {
    table <- table |>
      dplyr::select(
        dplyr::where(~dplyr::n_distinct(.) > 1)
      )

    section_names <- colnames(table)[colnames(table) != "text"]

    colnames(table)[colnames(table) != "text"][1:ifelse(length(section_names) >= 3, 3, length(section_names))] <- c("part", "section", "subsection")[1:length(section_names)]
  }

  table
}

#' Move a header column to text
#'
#' In some texts, header tags of a particular level indicate typographical variance that shouldn't be confused with other section tags. `move_header_to_text()` provides a simple method to adjust the table.
#'
#' @param .data A data frame with a column called `text` and at least one other column indicating parts, chapters, or sections.
#' @param column The header column to move
#' @param ... (optional) Filtering condition, such as `title == "Ulysses"`.
#'
#' @returns A data frame with the header column moved into `text`, conditional on `...`
#' @export
#'
#' @examples
#' if (FALSE) {
#'   joyce2 <- joyce |>
#'     move_column_to_text(subsection, title == "Ulysses")
#' }
move_header_to_text <- function(.data, column, ...){
  relevant_cols <- c(deparse(substitute(column)), "text")
  if (!missing(..1)) {
    .data <- .data |>
      dplyr::mutate(
        .test1 = ...,
        .test2 = duplicated({{ column }}))
    colnames(.data)[ncol(.data) - 1] <- ".test1"
  } else {
    .data <- .data |>
      dplyr::mutate(
        .test1 = TRUE,
        .test2 = duplicated({{ column }}))
  }

  .data |>
    dplyr::mutate(
      {{ column }} := dplyr::case_when(
        .test1 & .test2 ~ NA_character_,
        TRUE ~ {{ column }})) |>
    tidyr::pivot_longer(cols = tidyr::all_of(relevant_cols)) |>
    dplyr::mutate(name = dplyr::case_when(
      .test1 ~ "text",
      TRUE ~ name)) |>
    tidyr::drop_na(value) |>
    dplyr::select(-c(.test1, .test2)) |>
    tidyr::pivot_wider(values_fn = list) |>
    dplyr::select(dplyr::where(function(x) mean(is.na(x)) < 1)) |>
    tidyr::unchop(text)
}
