# Get a MICUSP corpus

The function accepts filters on columns from
[`micusp_metadata()`](https://jmclawson.github.io/tmtyro/reference/micusp_metadata.md)
and downloads and parses MICUSP (Michigan Corpus of Upper-level Student
Papers) texts locally if copies don't yet exist. It returns a table
combining metadata and text data for further processing.

## Usage

``` r
get_micusp_corpus(...)
```

## Arguments

- ...:

  A filter on rows and columns from
  [`micusp_metadata()`](https://jmclawson.github.io/tmtyro/reference/micusp_metadata.md).
  Accepted columns include the following: `paper_id`, `title`,
  `discipline`, `paper_type`, `student_level`, `sex`, `nativeness`, and
  `textual_features`.

## Value

A data frame with 1 row for each document in the corpus and 9 columns.
The first 8 columns contain metadata, and the final column called `text`
contains the full text of each document.

## Examples

``` r
if (FALSE) { # \dontrun{
physics_f <- get_micusp_corpus(discipline == "Physics", sex == "Female")
physics_m <- get_micusp_corpus(discipline == "Physics", sex == "Male")

discipline_by_sex <-
  micusp_metadata() |>
  dplyr::count(discipline, sex) |>
  tidyr::pivot_wider(
    names_from = "sex",
    values_from = "n") |>
  dplyr::mutate(
    ratio_f = (Female) / (Male + Female)) |>
    dplyr::arrange(ratio_f)

 disciplines_low_f <-
  discipline_by_sex |>
  head(3) |>
  dplyr::pull(discipline)

 disciplines_low_m <-
  discipline_by_sex |>
  tail(3) |>
  dplyr::pull(discipline)

low_representation_f <- get_micusp_corpus(
  sex == "Female",
  discipline %in% disciplines_low_f)

 low_representation_m <- get_micusp_corpus(
  sex == "Male",
  discipline %in% disciplines_low_m)
} # }
```
