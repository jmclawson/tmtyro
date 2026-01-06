# Drop variable labels

Drop variable labels

## Usage

``` r
drop_labels(data, vars = NULL)
```

## Arguments

- data:

  A data frame possibly containing labels

- vars:

  A vector of column names to be stripped of their labels. If this
  optional argument is not supplied, labels will be dropped for every
  column

## Value

An unlabeled data frame in the same format as `data`

## Examples

``` r
if (FALSE) { # \dontrun{
  drop_labels(vocab_dubliners)
} # }
```
