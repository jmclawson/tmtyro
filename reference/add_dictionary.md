# Add values from a dictionary

Add values from a dictionary

## Usage

``` r
add_dictionary(
  data,
  dictionary,
  feature = word,
  keep_term = NULL,
  label = NULL
)
```

## Arguments

- data:

  A tidy data frame, potentially containing a column called "word"

- dictionary:

  A data frame with two or more columns, potentially made with
  [`make_dictionary()`](https://jmclawson.github.io/tmtyro/reference/make_dictionary.md)

- feature:

  The column (like "word") to use for looking up values in the
  dictionary

- keep_term:

  Whether to retain the original term value. This option is especially
  useful with dictionaries containing terms longer than one word in
  length; the NULL value will keep the term for these dictionaries while
  discarding it for those with terms of only one word.

- label:

  Whether to label variables added to data frame

## Value

The original data frame with one or more columns added.

## Examples

``` r
if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

emoji_weather <- make_dictionary(
  list(
    "️☔️" = c("rain", "rains", "rainy", "raining"),
    "️⛈️" = c("storm", "storms", "stormy", "storming"),
    "☁️" = c("cloud", "clouds", "cloudy"),
    "🌞" = c("sun", "sunny"),
    "🌫️" = c("fog", "fogs", "foggy", "mist", "misty"),
    "🌬️" = c("wind", "winds", "windy"),
    "️❄️" = c("snow", "snows", "snowing")),
  name = "weather")

dubliners |>
   add_dictionary(emoji_weather) |>
   drop_na() |>
   head()
} # }
```
