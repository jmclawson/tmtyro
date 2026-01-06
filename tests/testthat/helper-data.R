make_sample_sentences <- function(split = FALSE) {
  result <- "The cat was bad. The dog was very good. The lavish lizard is the most bad."

  if (split) {
    result <- result |> gsub(
      pattern = "[.] ",
      replacement = ".\t",
      x = _) |>
      strsplit("\t") |>
      unlist()
  }

  result
}

make_sample_texts <- function() {
  data.frame(
    doc_id = c("A", "B", "C"),
    text = make_sample_sentences(split = TRUE))
}

make_sample_table <- function() {
  words <- c(
    "the", "cat", "was", "bad",
    "the", "dog", "was", "very", "good",
    "the", "lavish", "lizard", "is", "the", "most", "bad")
  docs <- c(
    "A", "A", "A", "A",
    "B", "B", "B", "B", "B",
    "C", "C", "C", "C", "C", "C", "C")

  data.frame(doc_id = docs, word = words) |>
    add_class("tmtyro")
}

drop_log <- function(x){
  attr(x, 'tmtyro_log') <- NULL
  x
}

