# Assign column labels from a data dictionary

Assign column labels from a data dictionary

## Usage

``` r
set_data_dictionary(data, dictionary)
```

## Arguments

- data:

  A tmtyro data frame

- dictionary:

  data frame containing at least two columns, `variable` and `label`

## Value

labeled data frame with the same structure as `data`

## Examples

``` r
if (FALSE) { # \dontrun{
  dubliners_dictionary <- data.frame(
    variable = "doc_id",
     label = "story title"
  )

  vocab_dubliners <- vocab_dubliners |>
    set_data_dictionary(dubliners_dictionary)
} # }
```
