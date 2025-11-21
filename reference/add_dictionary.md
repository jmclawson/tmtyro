# Add values from a dictionary

Add values from a dictionary

## Usage

``` r
add_dictionary(df, dictionary, feature = word, keep_term = NULL)
```

## Arguments

- df:

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

## Value

The original data frame with one or more columns added.

## Examples

``` r
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
#> # A tibble: 6 × 6
#>   doc_id       title     author       part         word   weather
#>   <fct>        <chr>     <chr>        <chr>        <chr>  <chr>  
#> 1 The Sisters  Dubliners Joyce, James THE SISTERS  clouds ☁️      
#> 2 The Sisters  Dubliners Joyce, James THE SISTERS  sunny  🌞     
#> 3 The Sisters  Dubliners Joyce, James THE SISTERS  sun    🌞     
#> 4 The Sisters  Dubliners Joyce, James THE SISTERS  clouds ☁️      
#> 5 An Encounter Dubliners Joyce, James AN ENCOUNTER storm  ️⛈️      
#> 6 An Encounter Dubliners Joyce, James AN ENCOUNTER sunny  🌞     
```
