# Move a header column to text

In some texts, header tags of a particular level indicate typographical
variance that shouldn't be confused with other section tags.
`move_header_to_text()` provides a simple method to adjust the table.

## Usage

``` r
move_header_to_text(.data, column, ...)
```

## Arguments

- .data:

  A data frame with a column called `text` and at least one other column
  indicating parts, chapters, or sections.

- column:

  The header column to move

- ...:

  (optional) Filtering condition, such as `title == "Ulysses"`.

## Value

A data frame with the header column moved into `text`, conditional on
`...`

## Examples

``` r
if (FALSE) {
  joyce2 <- joyce |>
    move_column_to_text(subsection, title == "Ulysses")
}
```
