# Remove stopwords

Remove stopwords

## Usage

``` r
drop_stopwords(data, wordlist = NULL, feature = word)
```

## Arguments

- data:

  A tidy data frame, potentially containing a column called "word"

- wordlist:

  A list of stopwords

- feature:

  A column of words containing one word per row to be checked for
  stopwords.

## Value

The original data frame with fewer rows.

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

dubliners |>
   drop_stopwords()
} # }
```
