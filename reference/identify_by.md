# Choose a new doc_id column

Choose a new doc_id column

## Usage

``` r
identify_by(data, ..., inorder = TRUE, sep = "_")
```

## Arguments

- data:

  A data frame, potentially with a `doc_id` column

- ...:

  The column or columns that will become the new identifier

- inorder:

  Whether to establish `doc_id` order as shown in the document

- sep:

  Separator between values when identifying by multiple columns

## Value

A data frame with a redefined doc_id column

## Examples

``` r
if (FALSE) {
  corpus |>
    load_texts() |>
    identify_by(author)
}
```
