#' Build and load a corpus from Project Gutenberg
#'
#' `get_gutenberg_corpus()` improves upon the functionality
#' of [gutenbergr::gutenberg_download()] in three key ways.
#' \enumerate{
#' \item First, its default setting is to retrieve the ".htm"
#' version of texts and parse the headers. This change allows for
#' consideration of texts by sections and chapters. Parsing is
#' handled by [parse_html()], and [move_header_to_text()] is
#' available to correct any parsing errors.
#' \item Second, when using the ".txt" format,
#' `get_gutenberg_corpus()` doesn't rely on the presence of
#' ".zip" files. This change improves coverage of files
#' dramatically.
#' \item Finally, it caches a copy of files locally to avoid the
#' need for repeated downloads. This change helps with code
#' portability, offline access, and server bandwidth.
#' }
#' All changes are made with consideration for server bandwidth,
#' so a two-second delay is introduced between each download
#' attempt. This will slow down the initial acquisition of
#' corpora, but offline caching speeds things up considerably
#' in subsequent use.
#'
#' @param gutenberg_id A vector of ID numbers from Project Gutenberg or a data frame containing a `gutenberg_id` column, such as from the results of a call to [gutenbergr::gutenberg_works()].
#' @param dir The directory for storing downloaded `.txt` files. Default value is "gutenberg".
#' @param mirror Optionally a mirror URL to retrieve the books from. By default uses the mirror from [gutenbergr::gutenberg_get_mirror()].
#' @param meta_fields Additional fields to add from [gutenbergr::gutenberg_metadata] describing each book. By default, title and author are added.
#' @param type Indicate whether the corpus should be created from the plain text "txt" versions of books (previously the only type) or from enhanced "htm" versions, which attempt to find markers for chapters and sections.
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
get_gutenberg_corpus <- function(gutenberg_id, dir = "gutenberg",
                                 mirror = NULL,
                                 meta_fields = c("title", "author"),
                                 type = c("htm","txt"),
                                 html_title = FALSE,
                                 ...) {
  # browser()
  type <- match.arg(type)

  # adapted from gutenberg_download
  if (is.null(mirror)) {
    mirror <- gutenbergr::gutenberg_get_mirror(verbose = FALSE)
  }

  if (inherits(gutenberg_id, "data.frame")) {
    gutenberg_id <- gutenberg_id[["gutenberg_id"]]
  }

  id <- as.character(gutenberg_id)

  path <- id |>
    stringr::str_sub(1, -2) |>
    stringr::str_split("") |>
    purrr::map_chr(stringr::str_c, collapse = "/")

  path <- ifelse(nchar(id) == 1, "0", path)

  # modifications away from gutenbergr::gutenberg_download
  # .txt files don't seem to be against policy
  full_url <- stringr::str_c(mirror, path, id,
                             stringr::str_c(id, ".txt"),
                             sep = "/")
  names(full_url) <- id

  # save downloads locally to avoid repeats
  try_download <- function(url, type = "txt") {
    if (type == "txt") {
      ret <- suppressWarnings(download_once(url, destdir=dir))
      if (!is.null(ret)) {
        return(ret)
      }
      base_url <- stringr::str_replace(url, "[.][a-z]{3}$", "")
      base_id <- stringr::str_extract(base_url, "[0-9]*$")
      for (suffix in c("-8", "-0")) {
        Sys.sleep(2)
        new_url <- stringr::str_glue("{base_url}{suffix}.txt")
        ret <- suppressWarnings(
          download_once(new_url,
                        filename = paste0(base_id, ".txt"),
                        destdir = dir))
        if (!is.null(ret)) {
          return(ret)
        }
      }
    } else if (type == "htm") {
      base_url <- stringr::str_replace(url, "[.][a-z]{3}$", "")
      base_id <- stringr::str_extract(base_url, "[0-9]*$")
      url <- paste0(base_url, "-h/", base_id, "-h.htm")
      ret <- suppressWarnings(download_once(url,
                                            filename = paste0(base_id, ".htm"),
                                            destdir=dir))
      if (!is.null(ret)) {
        return(ret)
      }
    }
    warnings(paste0("Could not download a book at ", url,
                    ". The book may have been archived, or you may need to select a different mirror."))
    NULL
  }

  # add a 2-second delay between books
  download_slowly <- purrr::slowly(\(x, type) try_download(x, type),
                  rate = purrr::rate_delay(2), quiet = TRUE)

  # extra checks to avoid attempting redownload for ids with suffixes
  if (dir.exists(dir)) {
    if (length(dir(path = dir)) > 0) {
      already_existing <- dir(path = dir, pattern = type) |>
        # stringr::str_remove_all(paste0(".", type)) |>
        stringr::str_remove_all("-8") |>
        stringr::str_remove_all("-0") |>
        # paste0(".txt") |>
        unique() |>
        paste0(collapse="|")
      some_urls <- full_url[!grepl(already_existing, full_url)]
      # message(some_urls)
      if(length(some_urls) > 0) {
        some_urls |>
          purrr::walk(\(x) download_slowly(x, type))
      }
    } else {
      full_url |>
        purrr::walk(\(x) download_slowly(x, type))
    }
  } else {
    full_url |>
      purrr::walk(\(x) download_slowly(x, type))
  }

  ids <- sort(id)
  collapsed_ids <- paste0("/", ids, ".", type, collapse = "|")
  all_files <- dir(path = dir,
                   pattern = paste0(".", type, "$"),
                   full.names = TRUE)
  guten_files <- all_files[grepl(collapsed_ids, all_files)]
  if (type == "htm") { # maybe should do this for all types
    id <- guten_files |>
      stringr::str_remove_all(paste0(dir, "/")) |>
      stringr::str_remove_all(".htm")
  }

  if (type == "txt") {
    # borrowed from gutenberg_download
    # gutenbergr::gutenberg_download(ids, files = guten_files, meta_fields = meta_fields, ...)
    downloaded <- guten_files |>
      stats::setNames(id) |>
      purrr::map(readr::read_lines)

    ret <- downloaded |>
      purrr::discard(is.null) |>
      purrr::map(\(x) x |>
                   data.frame() |>
                   setNames("text")) |>
      dplyr::bind_rows(.id = "gutenberg_id") |>
      dplyr::mutate(gutenberg_id = as.integer(gutenberg_id)) |>
      dplyr::group_by(gutenberg_id) |>
      # dplyr::do(dplyr::tibble(text = gutenberg_strip(.$text, ...))) |>
      dplyr::reframe(text = gutenbergr::gutenberg_strip(text, ...)) |>
      dplyr::ungroup()
  } else if (type == "htm") {
    ret <- guten_files |>
      stats::setNames(id) |>
      purrr::map(\(x) parse_html(x, title = html_title)) |>
      purrr::discard(is.null) |>
      dplyr::bind_rows(.id = "gutenberg_id") |>
      dplyr::relocate(text, .after = tidyr::last_col()) |>
      dplyr::mutate(gutenberg_id = as.integer(gutenberg_id))
  }

  if (length(meta_fields) > 0) {
    if (html_title) {
      meta_fields <- meta_fields[meta_fields != "title"]
    }
    ret <- ret[,!colnames(ret) %in% meta_fields[meta_fields != "gutenberg_id"]]
    meta_fields <- unique(c("gutenberg_id", meta_fields))
    md <- gutenbergr::gutenberg_metadata[meta_fields]
    ret <- dplyr::right_join(md, by = "gutenberg_id", ret)
  }

  ret
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
