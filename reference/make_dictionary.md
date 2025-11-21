# Create a lexicon

`make_dictionary()` creates a dictionary for use with
[`add_dictionary()`](https://jmclawson.github.io/tmtyro/reference/add_dictionary.md).

## Usage

``` r
make_dictionary(definitions, name = NULL)
```

## Arguments

- definitions:

  A list of named word vectors

- name:

  The kind of dictionary

## Value

A data frame with two columns, "word" and

## Examples

``` r
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

border_states <- make_dictionary(
  definitions = list(
    "Canada" = c(
      "Alaska", "Washington", "Idaho",
      "Montana", "North_Dakota", "Minnesota",
      "Wisconsin", "Michigan", "Ohio",
      "Pennsylvania", "New_York", "Vermont",
      "New_Hampshire", "Maine", "AK", "WA",
      "ID", "MT", "ND", "MN", "WI", "MI",
      "OH", "PA", "NY", "VT", "NH", "ME"),
    "Mexico" = c(
      "California", "Arizona", "New_Mexico",
      "Texas", "CA", "AZ", "NM", "TX")),
  name = "borders")
```
