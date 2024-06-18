#' Divide documents in equal lengths
#'
#' @param df A tidy data frame, potentially containing a column called "word"
#' @param size Size of each partition
#' @param overlap Size each partition should overlap. If a value between 0 and 1 is used, `overlap` will be calculated as a percentage of `size`.
#' @param minimum Minimum partition size. If a value between 0 and 1 is used, `minimum` will be calculated as a percentage of `size`.
#' @param by A column containing document grouping
#' @param character Whether to return a `partition` column as a character vector with zeroes added for padding. This feature may be helpful if using [identify_by()] to consider `partition` when defining documents in a corpus.
#'
#' @returns The original data frame with a column added for partition.
#' @export
#'
#' @examples
#' dubliners <- get_gutenberg_corpus(2814) |>
#'   load_texts() |>
#'   identify_by(part) |>
#'   standardize_titles()
#'
#' dubliners |>
#'   add_partitions() |>
#'   head()
add_partitions <- function(
    df,
    size = 1000,
    overlap = 0,
    minimum = 0.25,
    by = doc_id,
    character = FALSE) {

  if (!"word_index" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(
        word_index = dplyr::row_number(),
        .by = {{ by }}
      )
    drop <- TRUE
  } else {
    drop <- FALSE
  }

  if (overlap < 1 & overlap > 0) {
    overlap <- size * overlap
  }

  if (minimum < 1 & minimum > 0) {
    minimum <- size * minimum
  }

  df <- df |>
    dplyr::group_by({{ by }}) |>
    dplyr::group_modify(
      ~ join_partition(.x, size, overlap)) |>
    dplyr::ungroup() |>
    dplyr::group_by({{ by }}, partition) |>
    dplyr::filter(dplyr::n() >= minimum) |>
    dplyr::ungroup() |>
    dplyr::arrange({{ by }}, partition, word_index)

  if (drop) {
    df <- df |>
      dplyr::select(-word_index)
  }

  # Convert partition to character with
  # zeroes for padding. This is needed
  # for doing something like this:
  # df |>
  #   identify_by(doc_id, partition)
  if (character) {
    df |>
      dplyr::mutate(
        partition = partition |>
          stringr::str_pad(
            width = max(floor(log10(partition)) + 1),
            pad = "0"))
  } else {
    df
  }
}

# This function isn't exported because
# it can't be guaranteed to do the job
combine_partitions <- function(
    df,
    size = 1000,
    overlap = 0,
    by = doc_id,
    drop = NULL) {
  if (overlap < 1 & overlap > 0) {
    overlap <- size * overlap
  }
  if ("word_index" %in% colnames(df) & is.null(drop)) {
    drop <- FALSE
  } else if (is.null(drop)) {
    drop <- TRUE
  }

  df <- df |>
    dplyr::group_by({{ by }}) |>
    dplyr::group_modify(
      ~ tack_partition(.x, size, size - overlap)) |>
    dplyr::ungroup() |>
    dplyr::select(-partition) |>
    dplyr::distinct()
  if (drop) {
    df <- df |>
      dplyr::select(-word_index)
  }
  df
}

get_partitions <- function(nrow, size, overlap) {
  if (overlap > size) stop("Partition overlap can't exceed partition size.")
  offset <- size - overlap
  start_values <- 1 + (0:(nrow %/% offset))*offset
  end_values <- start_values + size - 1
  data.frame(start = start_values,
             end = end_values,
             partition = 1:length(start_values)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      word_index = list(c(start:end))
    ) |>
    dplyr::select(word_index, partition) |>
    tidyr::unnest_longer(word_index)
}

join_partition <- function(df, size, overlap){
  df |>
    dplyr::left_join(
      get_partitions(nrow(df),
                     size,
                     overlap),
      by = dplyr::join_by(word_index)) |>
    {\(x) if ("original" %in% colnames(x)) {
      dplyr::relocate(x,
                      partition,
                      .before = original)
    } else if ("word" %in% colnames(x)) {
      dplyr::relocate(x,
                      partition,
                      .before = word)
    }}()
}

tack_partition <- function(df, size, overlap){
  df |>
    dplyr::mutate(
      word_index = get_partitions(
        nrow(df),
        size,
        overlap)$word_index[1:nrow(df)])
}
