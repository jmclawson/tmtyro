# Standardize document titles

Useful especially for visualizations. `standardize_titles` applies some
English-language conventions, including converting underscores to
spaces, capitalizing important words, removing leading articles, and
dropping subtitles.

## Usage

``` r
standardize_titles(.data, title = doc_id, drop_articles = FALSE)
```

## Arguments

- .data:

  A tidy data frame, potentially containing a title column called
  "doc_id". Alternatively, a simple character vector of titles.

- title:

  A column containing the titles to be standardized

- drop_articles:

  Whether to remove opening articles like "The" and "A"

## Value

A data frame with one column adjusted. If .data is a character vector
instead of a data frame, then a character vector is returned.

## Examples

``` r
dubliners <- get_gutenberg_corpus(2814) |>
  load_texts() |>
  identify_by(part)

##### Standardizing strings #####
# Before `standardize_titles()`
unique(dubliners$doc_id)
#>  [1] THE SISTERS                   AN ENCOUNTER                 
#>  [3] ARABY                         EVELINE                      
#>  [5] AFTER THE RACE                TWO GALLANTS                 
#>  [7] THE BOARDING HOUSE            A LITTLE CLOUD               
#>  [9] COUNTERPARTS                  CLAY                         
#> [11] A PAINFUL CASE                IVY DAY IN THE COMMITTEE ROOM
#> [13] A MOTHER                      GRACE                        
#> [15] THE DEAD                     
#> 15 Levels: THE SISTERS AN ENCOUNTER ARABY EVELINE ... THE DEAD

# After `standardize_titles()`
unique(dubliners$doc_id) |>
  standardize_titles()
#>  [1] The Sisters                   An Encounter                 
#>  [3] Araby                         Eveline                      
#>  [5] After the Race                Two Gallants                 
#>  [7] The Boarding House            A Little Cloud               
#>  [9] Counterparts                  Clay                         
#> [11] A Painful Case                Ivy Day in the Committee Room
#> [13] A Mother                      Grace                        
#> [15] The Dead                     
#> 15 Levels: The Sisters An Encounter Araby Eveline ... The Dead

##### Standardizing a data frame #####

dubliners_measured <- dubliners |>
  add_vocabulary()

# Before `standardize_titles()`
dubliners_measured |>
  plot_vocabulary(labeling = "inline")
#> `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'


# After `standardize_titles()`
dubliners_measured |>
  standardize_titles() |>
  plot_vocabulary(labeling = "inline")
#> `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```
