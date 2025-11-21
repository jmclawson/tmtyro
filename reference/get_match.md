# Get dictionary matches of values in a vector

Get dictionary matches of values in a vector

## Usage

``` r
get_match(x, dictionary, keep = NULL)
```

## Arguments

- x:

  A vector, such as a column of character strings

- dictionary:

  A data frame with two or more columns, potentially made with
  [`make_dictionary()`](https://jmclawson.github.io/tmtyro/reference/make_dictionary.md).

- keep:

  The number of matched values to keep. If NULL, returns all matched
  values in a nested list.

## Value

A vector or nested list of sentiments for each value of `x`.

## See also

Other vectorized functions:
[`get_cumulative_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/get_cumulative_vocabulary.md),
[`get_frequency()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md),
[`get_hir()`](https://jmclawson.github.io/tmtyro/reference/get_hir.md),
[`get_htr()`](https://jmclawson.github.io/tmtyro/reference/get_htr.md),
[`get_idf_by()`](https://jmclawson.github.io/tmtyro/reference/get_idf_by.md),
[`get_sentiment()`](https://jmclawson.github.io/tmtyro/reference/get_sentiment.md),
[`get_tf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tf_by.md),
[`get_tfidf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tfidf_by.md),
[`get_ttr()`](https://jmclawson.github.io/tmtyro/reference/get_ttr.md),
[`is_hapax()`](https://jmclawson.github.io/tmtyro/reference/is_hapax.md),
[`is_new()`](https://jmclawson.github.io/tmtyro/reference/is_new.md)

## Examples

``` r
my_values <- c("It", "is", "raining")

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

get_match(my_values, emoji_weather)
#> [1] NA   NA   "️☔️"
```
