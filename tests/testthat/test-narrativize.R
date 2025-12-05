test_that("`narrativize()` returns bullets", {
  txt_checked <- make_sample_texts() |>
    load_texts() |>
    add_frequency() |>
    narrativize(return = "character")

  predicted <- " - Loaded texts by tokenizing words, converting to lowercase, and preserving paragraph breaks.\n - Counted the frequency of words used in each document."

  expect_equal(txt_checked, predicted)
})

test_that('`narrativize(format = "text")` returns inline text', {
  txt_checked <- make_sample_texts() |>
    load_texts() |>
    add_frequency() |>
    narrativize(format = "text", return = "character")

  predicted <- "Loaded texts by tokenizing words, converting to lowercase, and preserving paragraph breaks. Counted the frequency of words used in each document."

  expect_equal(txt_checked, predicted)
})

test_that('`narrativize(person = "we")` uses capitalized pronoun for one step', {
  txt_checked <- make_sample_texts() |>
    load_texts() |>
    narrativize(person = "we", return = "character")

  predicted <- " - We loaded texts by tokenizing words, converting to lowercase, and preserving paragraph breaks."

  expect_equal(txt_checked, predicted)
})

test_that('`narrativize(person = "we")` with multiple steps adds step markers and uses lowercase pronouns where appropriate', {
  txt_checked <- make_sample_texts() |>
    load_texts() |>
    add_frequency() |>
    narrativize(person = "we", return = "character")

  predicted <- " - First we loaded texts by tokenizing words, converting to lowercase, and preserving paragraph breaks.\n - Then we counted the frequency of words used in each document."

  expect_equal(txt_checked, predicted)
})

test_that("`narrativize()` replaces bigrams", {
  txt_checked <- make_sample_table() |>
    add_ngrams() |>
    narrativize(return = "character")

  predicted <- " - Constructed bigram sequences from the text."

  expect_equal(txt_checked, predicted)
})

test_that("`narrativize()` replaces trigrams", {
  txt_checked <- make_sample_table() |>
    add_ngrams(3) |>
    narrativize(return = "character")

  predicted <- " - Constructed trigram sequences from the text."

  expect_equal(txt_checked, predicted)
})


# Future tests:
# - Make sure `add_...()` functions retain prior steps
