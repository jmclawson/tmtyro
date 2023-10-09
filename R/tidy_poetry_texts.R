tidy_poetry_texts <-
  function(folder = "data",
           name = ".txt",
           word = TRUE) {
    tidy_one_text <- function(file, directory) {
      tibble::tibble(document = stringr::str_remove_all(file, "[.].*"),
                     text = readLines(paste0(directory, "/", file))) |>
        dplyr::mutate(stanza_num = cumsum(text == "") + 1) |>
        dplyr::filter(text != "") |>
        dplyr::mutate(line_num = dplyr::row_number()) |>
        dplyr::relocate(stanza_num, line_num,
                        .after = document)
    }

    the_files <-
      list.files(path = paste0(folder, "/"), pattern = name)

    full_poetry <-
      do.call(rbind, lapply(the_files, tidy_one_text, directory = folder))

    if (word) {
      full_poetry <- full_poetry |>
        tidytext::unnest_tokens(word, text)
    }

    full_poetry
  }
