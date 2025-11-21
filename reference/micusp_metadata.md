# Get MICUSP metadata

Explore metadata of available MICUSP (Michigan Corpus of Upper-level
Student Papers) texts to choose a corpus. On first use, the function
creates a folder in the working directory and downloads the
"micusp_metadata.csv" file before opening it. Subsequent use of the
function will load from the local copy.

## Usage

``` r
micusp_metadata(micusp_dir = "micusp")
```

## Arguments

- micusp_dir:

  The name of the directory for storing local copies of MICUSP
  materials. Defaults to "micusp/".

## Value

A data frame with 1 row for each document in the corpus and 8 columns of
metadata: `paper_id`, `title`, `discipline`, `paper_type`,
`student_level`, `sex`, `nativeness`, and `textual_features`

## Examples

``` r
if (FALSE) { # \dontrun{
micusp_metadata() |> head()
} # }
```
