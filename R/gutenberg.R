#' Build and load a corpus from Project Gutenberg
#'
#' `get_gutenberg_corpus()` improves upon the functionality
#' of [gutenbergr::gutenberg_download()] in two key ways. First,
#' it downloads the ".txt" format of ebooks rather than the
#' ".zip" format. This change improves coverage dramatically.
#' This change was made with consideration for server bandwidth,
#' so a two-second delay is introduced between each download
#' attempt. Second, it caches a copy of the file locally to avoid
#' repeated downloads. This change helps with code portability and
#' offline access.
#'
#' @param gutenberg_id A vector of ID numbers from Project Gutenberg or a data frame containing a `gutenberg_id` column, such as from the results of a call to [gutenbergr::gutenberg_works()].
#' @param dir The directory for storing downloaded `.txt` files. Default value is "gutenberg".
#' @param mirror Optionally a mirror URL to retrieve the books from. By default uses the mirror from [gutenbergr::gutenberg_get_mirror()].
#' @param meta_fields Additional fields to add from [gutenbergr::gutenberg_metadata] describing each book. By default, title and author are added.
#' @param ... Additional parameters passed along to [gutenbergr::gutenberg_download()] in the final step.
#'
#' @returns A data frame with one row for each line of the texts in the corpus.
#' @export
#'
#' @examples
#' library(gutenbergr)
#'
#' dalloway <- gutenberg_works(author == "Woolf, Virginia",
#'                             title == "Mrs. Dalloway") |>
#'   get_gutenberg_corpus()
get_gutenberg_corpus <- function(gutenberg_id, dir = "gutenberg",
                                 mirror = NULL,
                                 meta_fields = c("title", "author"),
                                 ...) {
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
  try_download <- function(url) {
    ret <- suppressWarnings(download_once(url, destdir=dir))
    if (!is.null(ret)) {
      return(ret)
    }
    base_url <- stringr::str_replace(url, ".txt$", "")
    base_id <- stringr::str_extract(base_url, "[0-9]*$")
    for (suffix in c("-8", "-0")) {
      Sys.sleep(2)
      new_url <- stringr::str_glue("{base_url}{suffix}.txt")
      ret <- suppressWarnings(download_once(new_url,
                                            filename = paste0(base_id, ".txt"),
                                            destdir = dir))
      if (!is.null(ret)) {
        return(ret)
      }
    }
    cli::cli_warn(
      c(
        "!" = "Could not download a book at {url}.",
        "i" = "The book may have been archived.",
        "i" = "Alternatively, You may need to select a different mirror.",
        ">" = "See https://www.gutenberg.org/MIRRORS.ALL for options."
      )
    )
    NULL
  }

  # add a 2-second delay between books
  download_slowly <- purrr::slowly(\(x) try_download(x), rate = purrr::rate_delay(2), quiet = TRUE)

  # extra checks to avoid attempting redownload for ids with suffixes
  if (dir.exists(dir)) {
    if (length(dir(path = dir)) > 0) {
      already_existing <- dir(path = dir) |>
        stringr::str_remove_all("[.]txt") |>
        stringr::str_remove_all("-8") |>
        stringr::str_remove_all("-0") |>
        paste0(".txt") |>
        unique() |>
        paste0(collapse="|")
      some_urls <- full_url[!grepl(already_existing, full_url)]
      message(some_urls)
      if(length(some_urls) > 0) {
        some_urls |>
          purrr::walk(download_slowly)
      }
    } else {
      full_url |>
        purrr::walk(download_slowly)
    }
  } else {
    full_url |>
      purrr::walk(download_slowly)
  }

  # send filenames to gutenberg_download
  ids <- sort(id)
  collapsed_ids <- paste0("/", ids, ".txt", collapse = "|")
  guten_files <- dir(path = dir, full.names = TRUE) |>
    {\(x) x[grepl(collapsed_ids, x)]}()

  gutenbergr::gutenberg_download(ids, files = guten_files, meta_fields = meta_fields, ...)
}

is_uppercase <- function(string, nums = FALSE){
  if (nums) {
    tester <- "[^A-Z 0-9]"
  } else {
    tester <- "[^A-Z ]"
  }
  gsub(tester, "", string) == string
}

is_number <- function(string){
  check_roman <- function(string) {
    string <- toupper(string)
    all(
      grepl("^M{0,3}C?M?D?X?C{,4}X?L?I?X{,4}I?V?I{,4}$", string),
      !grepl("CMD", string),
      !grepl("CMC", string), !grepl("CDC", string),
      !grepl("XCL", string),
      !grepl("XCX", string), !grepl("XLX", string),
      !grepl("IXI", string), !grepl("IVI", string)
    )
  }
  if (is.numeric(string)) {
    return(TRUE)
  } else if (check_roman(string)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

add_parts <- function(df, feature = text, by = doc_id){

}

# parse_gutenberg_html <- function(file = "gutenberg/160.html") {
#
#   choose_xpathy <- function(html) {
#     h_levels <- html |>
#       rvest::read_html() |>
#       rvest::html_elements("h1, h2, h3, h4, h5, h6") |>
#       rvest::html_name() |>
#       unique()
#
#     div_classes <- html |>
#       rvest::read_html() |>
#       rvest::html_elements(xpath = "div[@class]") |>
#       rvest::html_attr("class") |>
#       unique()
#
#     return(section)
#   }}

parse_gutenberg_html <- function(html) {
  h_levels <- html |>
    rvest::read_html() |>
    rvest::html_elements("h1, h2, h3, h4, h5, h6") |>
    rvest::html_name() |>
    unique()

  h_subs <- h_levels |>
    {\(x) x[x != "h1"]}()

  div_classes <- html |>
    rvest::read_html() |>
    rvest::html_elements(xpath = "//div[@class]") |>
    rvest::html_attr("class") |>
    unique() |>
    {\(x) x[!x %in% c("fig", "mynote")]}()

  anchor_headers <- html |>
    rvest::read_html() |>
    rvest::html_elements(xpath = "//*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6][preceding-sibling::*[1][self::a]]")

  if (length(h_subs) == 1 &
    length(div_classes) == 1) {
    # for 55, 144, 2591
    result <- html |>
      rvest::read_html() |>
      rvest::html_elements(xpath = "//div[@class!='mynote']") |>
      purrr::map(get_simple_chapter)
  } else if (length(h_subs) == 2 &
    length(div_classes) == 1) {
    # for 160
    result <- html |>
      rvest::read_html() |>
      rvest::html_elements(xpath = "//div[@class!='mynote']") |>
      purrr::map(get_complex_chapter)
  } else if (length(div_classes) == 0 &
             length(anchor_headers) > 1) {
    result <- html |>
      get_by_anchors()
  } else if (length(div_classes) == 0) {
    # for "others"
    result <- html |>
      get_by_headers()
  }
  result
}

  get_maintitle <- function(html = "gutenberg/144.html"){
    if (is.numeric(html)) {
      html <- stringr::str_glue("gutenberg/{html}.html")
    }

    html |>
      rvest::read_html() |>
      rvest::html_element("h1") |>
      rvest::html_text2()
  }

  # works for 55, 144, 2591
  get_simple_chapter <- function(html_part) {
    title <- html_part |>
      rvest::html_element("h2, h3, h4, h5, h6") |>
      rvest::html_text2() |>
      stringr::str_replace_all("\n", " ") |>
      stringr::str_remove_all("\r") |>
      stringr::str_replace_all("[ ]{2,}", " ") |>
      trimws()

    pars <- html_part |>
      rvest::html_elements("p") |>
      rvest::html_text2()

    if (length(pars) == 0) {
      pars <- NA_character_
    }

    data.frame(
      section = title,
      text = pars
    )
  }

  # for 160
  get_complex_chapter <- function(html_part) {
    get_complex_pars <- function(html_part, xpathy){
      pars <- html_part |>
        rvest::html_elements(xpath = xpathy) |>
        rvest::html_text2() |>
        stringr::str_replace_all("\n", " ") |>
        stringr::str_remove_all("\r") |>
        stringr::str_replace_all("[ ]{2,}", " ") |>
        trimws()

      if (length(pars) == 0) {
        pars <- NA_character_
      }
      list(pars)
    }

    all_titles <- html_part |>
      rvest::html_elements("h2, h3, h4, h5") |>
      rvest::html_text2()

    if (length(all_titles) == 1) {
      result <- html_part |>
        get_simple_chapter() |>
        dplyr::mutate(subsection = NA_character_)
    } else {
      top_title_level <- html_part |>
        rvest::html_element("h2, h3, h4, h5") |>
        rvest::html_name()

      sub_title_level <- html_part |>
        rvest::html_elements("h2, h3, h4, h5") |>
        rvest::html_name() |>
        {\(x) x[-1]}()

      top_title <- html_part |>
        rvest::html_element("h2, h3, h4, h5") |>
        rvest::html_text2() |>
        stringr::str_replace_all("\n", " ") |>
        stringr::str_remove_all("\r") |>
        stringr::str_replace_all("[ ]{2,}", " ") |>
        trimws()

      sub_titles <- html_part |>
        rvest::html_elements("h2, h3, h4, h5") |>
        {\(x) x[-1]}() |>
        rvest::html_text2() |>
        stringr::str_replace_all("\n", " ") |>
        stringr::str_remove_all("\r") |>
        stringr::str_replace_all("[ ]{2,}", " ") |>
        trimws()

      link_ids <- html_part |>
        rvest::html_elements("a") |>
        rvest::html_attr("name") |>
        {\(x) x[-1]}()

      if (length(link_ids) == 0) {
        link_ids <- html_part |>
          rvest::html_elements("a") |>
          rvest::html_attr("id") |>
          {\(x) x[-1]}()
      }

      result <-
        data.frame(
          section = top_title,
          subsection = sub_titles,
          id = link_ids,
          level = sub_title_level
        ) |>
        dplyr::mutate(
          xpathy = dplyr::case_when(
            !is.na(dplyr::lead(level)) ~ stringr::str_glue("//p[preceding-sibling::{level}[a[@name='{id}']] and following-sibling::{dplyr::lead(level)}[a[@name='{dplyr::lead(id)}']]]"),
            TRUE ~ stringr::str_glue("//p[preceding-sibling::*[child::a[@name='{id}']]]")) |>
            as.character()) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          text = get_complex_pars(html_part, xpathy)
        ) |>
        tidyr::unnest(text) |>
        dplyr::select(-c(id, level, xpathy))
    }
    return(result)
  }

  # for 27200
  get_by_anchors <- function(html_part = "gutenberg/27200.html"){
    get_pars <- function(html_read, xpathy){
      pars <- html_read |>
        rvest::html_elements(xpath = xpathy) |>
        rvest::html_text2()

      if (length(pars) == 0) {
        pars <- NA_character_
      }

      list(pars)
    }

    readed <- rvest::read_html(html_part)

    golden_path <- "//a[following-sibling::*[1][self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6]]"

    a_ids <- readed |>
      rvest::html_elements(xpath = golden_path) |>
      rvest::html_attr("name")

    header_path <- "//*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6][preceding-sibling::*[1][self::a]]"

    headers <- readed |>
      rvest::html_elements(xpath = header_path) |>
      rvest::html_text2()

    toc_df <- data.frame(
      id = a_ids,
      section = headers) |>
      dplyr::mutate(
        xpathy = dplyr::case_when(
          !is.na(dplyr::lead(id)) ~ stringr::str_glue('//p[preceding-sibling::a[@name="{id}"] and following-sibling::a[@name="{dplyr::lead(id)}"]]'),
          TRUE ~ stringr::str_glue('//p[preceding-sibling::a[@name="{id}"]]')
      )) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        text = get_pars(readed, xpathy)) |>
      dplyr::ungroup() |>
      tidyr::unnest(text) |>
      dplyr::select(-c(id, xpathy))

    return(toc_df)
  }

  # fallback for "the rest"
  get_by_headers <- function(html_part = "gutenberg/27200.html"){
    get_pars <- function(html_section, xpathy){
      pars <- html_section |>
        rvest::html_elements(xpath = xpathy) |>
        rvest::html_text2()

      if (length(pars) == 0) {
        pars <- NA_character_
      }

      list(pars)
    }

    readed <- rvest::read_html(html_part)

    # selects any headers that come immediately before a p
    special_path <- "/html/body/*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6][following-sibling::*[1][self::p]]"

    h_levels <- readed |>
      rvest::html_elements(xpath = special_path) |>
      rvest::html_name()

    # check to see if headers are (mostly) 1 level
    level_props <-
      h_levels |>
      table() |>
      prop.table() |>
      {\(x) x[x>0.88]}() |>
      names()

    if(length(level_props[level_props > 0.88])) {
      main_level <- level_props[level_props > 0.88] |>
        names()

      special_path <- "/html/body/*[self::{main_level}][following-sibling::*[1][self::p]]"

      h_levels <- readed |>
        rvest::html_elements(xpath = special_path) |>
        rvest::html_name()
    }

    headers <- readed |>
      rvest::html_elements(xpath = special_path) |>
      rvest::html_text() |>
      # stringr::str_replace('"', '“') |>
      # stringr::str_replace('"', '”')
      stringr::str_replace('"', '&quot;')

    toc_df <- data.frame(
      section = headers,
      level = h_levels) |>
      dplyr::mutate(
        xpathy = dplyr::case_when(
          !is.na(dplyr::lead(level)) ~ stringr::str_glue("//p[preceding-sibling::{level}[text()=\"{section}\"] and following-sibling::{dplyr::lead(level)}[text()=\"{dplyr::lead(section)}\"]]"),
          TRUE ~ stringr::str_glue("//p[preceding-sibling::{level}[text()=\"{section}\"]]")) |> as.character()) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        text = get_pars(readed, xpathy)) |>
      tidyr::unnest(text) |>
      dplyr::select(-c(level, xpathy))

    return(toc_df)
  }

parse_newly <- function(html){
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
    janitor::remove_empty("cols") |>
    dplyr::select(where(
      ~dplyr::n_distinct(.) > 1
    ))

  section_names <- colnames(table)[colnames(table) != "text"]

  colnames(table)[colnames(table) != "text"][1:ifelse(length(section_names) >= 3, 3, length(section_names))] <- c("section", "subsection", "subsubsection")[1:length(section_names)]

  table
}


#   grab_pars <- function(html, xpathy, start_id, end_id) {
#     # browser()
#     result <- html |>
#       rvest::read_html() |>
#       rvest::html_elements(
#         xpath = xpathy) |>
#       rvest::html_text2()
#
#     if (length(result) == 0) {
#       section_headers <- html |>
#         rvest::read_html() |>
#         rvest::html_elements(
#           xpath = glue::glue("//*[child::a[@name='{start_id}']]")
#         ) |>
#         rvest::html_elements(
#           xpath = "../*[child::a]"
#         )
#
#       if (length(rvest::html_name(section_headers)) > 1 || rvest::html_name(section_headers) != "body") {
#         first_header <- html |>
#           rvest::read_html() |>
#           rvest::html_elements(
#             xpath = glue::glue("//*[child::a[@name='{start_id}']]")
#           ) |>
#           rvest::html_elements(
#             xpath = "../*[child::a][1]"
#           ) |>
#           rvest::html_elements("a") |>
#           rvest::html_attr("name")
#
#         if (length(first_header) == 1 && !is.na(first_header)) {
#           if (length(section_headers) > 1 &
#               first_header == start_id) {
#             return(NA_character_)
#           }
#         }
#
#         result <- html |>
#           rvest::read_html() |>
#           rvest::html_elements(
#             xpath = glue::glue("//div[*[child::a[@name='{start_id}']]]")
#           ) |>
#           rvest::html_elements(
#             xpath = glue::glue("//p[preceding-sibling::*[child::a[@name='{start_id}']]]")
#           ) |>
#           rvest::html_text2()
#       }
#
#     }
#     if (length(result) == 0) {
#       check_div <- html |>
#         rvest::read_html() |>
#         rvest::html_elements(
#           xpath = glue::glue("//div[*[child::a[@name='{start_id}']]]"))
#       if (length(check_div) != 0) {
#         result <- check_div |>
#           rvest::html_elements(
#             xpath = glue::glue("//p[preceding-sibling::a[@name='{start_id}'] and following-sibling::a[@name='{end_id}']]")
#           ) |>
#           rvest::html_text2()
#       } else {
#         result <- html |>
#           rvest::read_html() |>
#           rvest::html_elements(
#             xpath = glue::glue("//p[preceding-sibling::a[@name='{start_id}'] and following-sibling::a[@name='{end_id}']]")
#           ) |>
#           rvest::html_text2()
#       }
#     }
#     if (length(result) == 0) {
#       result <- NA_character_
#     }
#     return(result)
#   }
#
#   parse_outline <- function(html) {
#     toc_text <- html |>
#       rvest::read_html() |>
#       rvest::html_elements("h1, h2, h3, h4, h5, h6") |>
#       rvest::html_text2() |>
#       stringr::str_replace_all("\n", " ")
#
#     toc_ids <- html |>
#       rvest::read_html() |>
#       rvest::html_elements("h1, h2, h3, h4, h5, h6") |>
#       rvest::html_element("a") |>
#       rvest::html_attr("id")
#
#     if(mean(is.na(toc_ids)) == 1) {
#       toc_ids <- html |>
#         rvest::read_html() |>
#         rvest::html_elements("h1, h2, h3, h4, h5, h6") |>
#         rvest::html_element("a") |>
#         rvest::html_attr("name")
#     }
#
#     toc_names <- html |>
#       rvest::read_html() |>
#       rvest::html_elements("h1, h2, h3, h4, h5, h6") |>
#       rvest::html_name()
#
#     if(mean(is.na(toc_ids)) == 1) {
#       toc_a_marker <- html |>
#         rvest::read_html() |>
#         rvest::html_elements(
#           xpath = "//a[@name]")
#
#       toc_ids <- toc_a_marker |>
#         rvest::html_attr("name")
#
#       toc_text <- toc_a_marker |>
#         rvest::html_elements(xpath="./following::h2[1]|./following::h3[1]") |>
#         rvest::html_text2()
#
#       toc_names <- toc_a_marker |>
#         rvest::html_elements(xpath="./following::h2[1]|./following::h3[1]") |>
#         rvest::html_name()
#     }
#
#     toc_df <-
#       data.frame(
#         id = toc_ids,
#         level = toc_names,
#         section_title = toc_text
#       ) |>
#       tidyr::drop_na() |>
#       dplyr::mutate(
#         xpathy = glue::glue("//p[preceding-sibling::{level}[a[@name='{id}']] and following-sibling::{dplyr::lead(level)}[a[@name='{dplyr::lead(id)}']]]")
#       ) |>
#       dplyr::rowwise() |>
#       dplyr::mutate(
#         text = list(grab_pars(html, xpathy, start_id = id, end_id = dplyr::lead(id)))
#       )
#
#     toc_df
#   }
#
#   file |>
#     parse_outline() |>
#     tidyr::unnest(text) |>
#     dplyr::mutate(
#       text = text |>
#         stringr::str_replace_all("\r", "") |>
#         trimws()) |>
#     dplyr::mutate(
#       section = dplyr::case_when(
#         level == "h2" ~ section_title),
#       subsection = dplyr::case_when(
#         level == "h3" ~ section_title),
#       subsection2 = dplyr::case_when(
#         level == "h4" ~ section_title),
#       subsection3 = dplyr::case_when(
#         level == "h5" ~ section_title),
#       .before = text) |>
#     tidyr::fill(section) |>
#     dplyr::select(-c(id, xpathy, level, section_title)) |>
#     tidyr::drop_na(text) |>
#     dplyr::select(dplyr::where(~!all(is.na(.x))))
# }



#
#   pars <- "gutenberg/160.html" |>
#     rvest::read_html() |>
#     rvest::html_elements("h1 p, h2, h3, h4, h5, h6")
#
#   "gutenberg/160.html" |>
#     rvest::read_html() |>
#     rvest::html_elements(
#       xpath =
#         "//p[preceding-sibling::h3[a[@id='link2H_4_0052']]
#     and following-sibling::h3[a[@id='link2H_4_0053']]]")
#
#   "gutenberg/160.html" |>
#     rvest::read_html() |>
#     rvest::html_elements(
#       xpath = toc_df$xpath[5]) |>
#     rvest::html_text2()
#
#   "gutenberg/160.html" |>
#     rvest::read_html() |>
#     rvest::html_elements(
#       xpath =
#         "//p[preceding-sibling::h3[@id='link2H_4_0052']
#     and following-sibling::h3[@id='link2H_4_0053']]") |>
#     rvest::html_text2()
#
#   "/html/body/div[10]/p[473]"
#
#
# }

# old <- Sys.time(); Sys.time() - old
#
# oz <- get_sections("gutenberg/55.html")
# woolf <- get_sections("gutenberg/144.html")
# grimm <- get_sections("gutenberg/2591.html")
# chopin <- get_sections("gutenberg/160.html")
# hca <- get_sections("gutenberg/27200.html")


# old <- Sys.time(); oz <- get_sections("gutenberg/55.html"); Sys.time() - old # 0.282 secs; 262.4 K
# old <- Sys.time(); woolf <- get_sections("gutenberg/144.html"); Sys.time() - old # 0.473 secs; 871.5 K
# old <- Sys.time(); grimm <- get_sections("gutenberg/2591.html"); Sys.time() - old # 0.364 secs; 643.6 K
# old <- Sys.time(); chopin <- get_sections("gutenberg/160.html"); Sys.time() - old # 2.492 secs; 422.2 K
# old <- Sys.time(); hca <- get_sections("gutenberg/27200.html"); Sys.time() - old # 1.863763 mins; 2 MB or 2129.7 K
# old <- Sys.time(); hca2 <- "gutenberg/27200.html" |> get_by_anchors(); Sys.time() - old # 1.006 mins; 2 MB or 2129.7 K
#
# tibble(
#   number = c(55, 144, 2591, 160, 27200, 27200),
#   text = c("oz", "woolf", "grimm", "chopin", "hca", "hca2"),
#   time = c(0.282, .473, 0.364, 2.492, 1.863763 * 60, 1.006538 * 60),
#   size = c(262.4, 871.5, 643.6, 422.2, 2129.7, 2129.7),
#   method = c("A", "A", "A", "B", "C", "D")) |>
#   ggplot(aes(size, time)) +
#   geom_text(aes(label = text, color = method))
#
#
# old <- Sys.time(); oz <- parse_gutenberg_html("gutenberg/55.html"); Sys.time() - old # 0.313 secs; 262.4 K
# old <- Sys.time(); woolf <- parse_gutenberg_html("gutenberg/144.html"); Sys.time() - old # 0.529 secs; 871.5 K
# old <- Sys.time(); grimm <- parse_gutenberg_html("gutenberg/2591.html"); Sys.time() - old # 0.367 secs; 643.6 K
# old <- Sys.time(); chopin <- parse_gutenberg_html("gutenberg/160.html"); Sys.time() - old # 2.323 secs; 422.2 K
# old <- Sys.time(); hca2 <- parse_gutenberg_html("gutenberg/27200.html"); Sys.time() - old # 59.759 secs; 2 MB or 2129.7 K
#
#
# old <- Sys.time(); get_by_headers("gutenberg/55.html"); Sys.time() - old # 0.439 secs; 262.4 K
# old <- Sys.time(); get_by_headers("gutenberg/144.html"); Sys.time() - old # 0.529 secs; 871.5 K
# old <- Sys.time(); get_by_headers("gutenberg/2591.html"); Sys.time() - old # 0.367 secs; 643.6 K


