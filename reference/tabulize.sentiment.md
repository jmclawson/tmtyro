# Prepare a table for sentiment analysis

Prepare a table for sentiment analysis

## Usage

``` r
# S3 method for class 'sentiment'
tabulize(
  data,
  inorder = TRUE,
  digits = 2,
  drop_na = FALSE,
  ignore = NULL,
  rows = NULL,
  count = TRUE,
  ...
)
```

## Arguments

- data:

  data processed with one or more functions from `tmtyro`

- inorder:

  Indicates whether labels in the `doc_id` column should have their
  order preserved

- digits:

  The number of digits to show past the decimal point

- drop_na:

  Removes rows lacking sentiment

- ignore:

  Removes rows matching set sentiments

- rows:

  Chooses rows to be shown

- count:

  Determines whether frequencies will be counted for sentiments

- ...:

  optional parameters passed along to methods
