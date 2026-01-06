# Default narrative dictionary

A reference dictionary of English narrative templates used by
[`narrativize`](https://jmclawson.github.io/tmtyro/reference/narrativize.md)
to describe a tmtyro workflow

## Usage

``` r
narrative_dictionary_en
```

## Format

A tibble with columns:

- fn:

  Function name key (character). Use `NA` for shared lookup rows.

- parameter:

  Parameter or lookup key (character) or `NA`.

- narrative:

  Function-level narrative template or lookup text (character) or `NA`.

- prefix:

  Optional prefix used when assembling parameter phrases (character) or
  `NA`.

- suffix:

  Optional suffix used when assembling parameter phrases (character) or
  `NA`.

- plural_pre:

  Optional plural form for `prefix` (character) or `NA`.

- plural_suf:

  Optional plural form for `suffix` (character) or `NA`.

## Details

Each row corresponds to either:

1.  a function-level narrative template (i.e., when `fn` is present and
    `parameter = NA`),

2.  a parameter-level label or phrase (when `fn` and `parameter` are
    present), or

3.  a shared lookup value (when `fn = NA` and `parameter = NA`) used for
    general substitutions (e.g. using `"bigram"` in place of
    `"2-gram"`).

Templates may include placeholders in braces (for example
`{parameters}`, `{feature}`, `{lexicon}`). When customizing or
translating the dictionary, preserve these placeholder names exactly,
modifying only the surrounding prose.

This object is exported primarily as a model for creating custom
narrative dictionaries for other languages, institutional tone, or
personal style.

## See also

Other logging helpers:
[`methods_log`](https://jmclawson.github.io/tmtyro/reference/methods_log.md),
[`narrativize()`](https://jmclawson.github.io/tmtyro/reference/narrativize.md)

## Examples

``` r
narrative_dictionary_en
#> # A tibble: 32 × 7
#>    fn                   parameter  narrative prefix suffix plural_pre plural_suf
#>    <chr>                <chr>      <chr>     <chr>  <lgl>  <chr>      <lgl>     
#>  1 user                 NA         {argumen… NA     NA     NA         NA        
#>  2 get_gutenberg_corpus NA         Retrieve… NA     NA     NA         NA        
#>  3 get_gutenberg_corpus gutenberg… NA        ID nu… NA     ID numbers NA        
#>  4 load_texts           NA         Loaded t… NA     NA     NA         NA        
#>  5 load_texts           parameters NA        by     NA     NA         NA        
#>  6 load_texts           word       tokenizi… NA     NA     NA         NA        
#>  7 load_texts           lemma      detectin… NA     NA     NA         NA        
#>  8 load_texts           to_lower   converti… NA     NA     NA         NA        
#>  9 load_texts           remove_na… removing… NA     NA     NA         NA        
#> 10 load_texts           pos        detectin… NA     NA     NA         NA        
#> # ℹ 22 more rows

# Use as a model to customize a dictionary

if (FALSE) { # \dontrun{
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part) |>
  standardize_titles()

narrativize(
  dubliners,
  dictionary = narrative_dictionary_en)
} # }
```
