# Prepare a data dictionary from column labels

Prepare a data dictionary from column labels

## Usage

``` r
get_data_dictionary(data, only_labeled = FALSE)
```

## Arguments

- data:

  A tmtyro data frame

- only_labeled:

  Whether to return only the variables with descriptive labels

## Value

data frame with three columns: `variable`, `label`, and `class`

## Examples

``` r
if (FALSE) { # \dontrun{
  get_data_dictionary(vocab_dubliners)
} # }
```
