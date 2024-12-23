# tmtyro 0.5

* New functions `add_frequency()` and `add_tf_idf()` allow for consistent phrasing of workflows. These new methods are supported by `visualize()` and `tabulize()`.
* New vectorized functions support using dplyr's `mutate()` and similar use cases: `get_frequency()` for returning counts and ratios of values in a vector; `is_new()` and `is_hapax()` for testing uniqueness of values in a vector; `get_cumulative_vocabulary()`, `get_ttr()`, `get_hir()`, and `get_htr()` for measuring the cumulative change of a vector over time; `get_match()` and `get_sentiment()` for matching values in a dictionary; and `get_tf()`, `get_tf_by()`, `get_idf_by()`, and `get_tfidf_by()` for weighing elements of term frequency--inverse document frequency.
* Bar plots of words per document now use better logic with labels, and a new `label_color` argument allows for customizing label color when needed.
* Added "skills ramp" article documenting vectorized functions and customized tables and figures 

# tmtyro 0.4.1

* `get_gutenberg_corpus()` should do less, and now it does. Other functionality is available via gutenbergr.

# tmtyro 0.4.0

* New function `contextualize()` shows terms in a window of context
* New function `add_index()` adds a column showing word indices within each document
* `load_texts()` adds support to keep original capitalization and punctuation alongside the tokenized `word` column with the `keep_original` argument. This process does not work in all instances, so the option defaults to `FALSE`.
* `add_dictionary()` includes an option to keep original terms. This is useful for n-gram dictionaries, where a match might otherwise span multiple rows.
* `add_ngrams()` supports negative ranges, for building context windows
* `add_partitions()` supports overlapping partitions
* `standardize_titles()` capitalizes words after terminal punctuation

# tmtyro 0.3.0

* `add_dictionary()` now supports n-gram dictionaries, matching across multiple words
* `make_dictionary()` has a slightly changed syntax, with clearer argument names `definitions` and `name`
* Along with its related `visualize()` methods, `plot_doc_word_bars()` improves support for `color_y = TRUE` and `reorder_y = TRUE`
* When naming colors, `change_color()` now allows setting colors for unnamed values
* `standardize_titles()` capitalizes Roman numerals 
* `load_texts()` adds support for custom tokenization using the dots parameter from `tidytext::unnest_tokens()`

# tmtyro 0.2.0

* New function `add_partitions()` adds a partition column, useful for getting same-sized samples
* `identify_by()` now works with multiple columns, and it keeps existing metadata columns. This is especially useful with the new `add_partitions()` column, using something like `my_corpus() |> add_partitions() |> identify_by(title, partition)` before continuing to work with partitioned documents. To return framing to unpartitioned data, used `identify_by(title)` or whatever other column is most relevant.
* New visualization and tabulization methods for `expand_documents()`
* Functions now imported: `count()` and `drop_na()`
* When the ggraph package is loaded, `plot_bigrams()` now uses a color scale on edges, rather than spot color on nodes, with full support for `change_color()`
* Improved documentation with website articles for customizing colors and showing code comparisons

# tmtyro 0.1.0

* First "public" release! 🎉
* Unnecessary components removed and dependencies reduced
* Examples standardized and made reproducible
* `change_colors()` now works with `plot_bigrams()`
* `change_colors()` now includes a "dubois" colorset
* `tabulize()` documentation is now improved for online output
* `standardize_titles()` now works with factors
* Added default behavior for `visualize()` on a corpus
* Part of speech tagging should now work for more texts

# tmtyro (development version 0.0.8.9000)

* New `tabulize()` generic function for preparing tables with supported methods
* Standardizing argument names between `visualize()` and `tabulize()`
* New package documentation for getting started
* New `collapse_rows()` function for clean tables using `gt::gt()`
* New feature in `standardize_titles()` to keep initial articles
* New options in `plot_doc_word_bars()` to keep the order of Y-axis values consistent and to color by Y-axis value instead of by facet
* Rename `add_lexical_diversity()` to `add_vocabulary()`
* Add option for renaming existing `doc_id` column when using `identify_by()`

# tmtyro (development version 0.0.7.9000)

* `get_gutenberg_corpus()` now retrieves HTML versions of texts from Project Gutenberg and parses header tags for section markers
* New function `parse_html()` for reading headers in an HTML file
* New function `move_header_to_text()` for converting header to text
* New function `identify_by()` to simplify using something other than `doc_id`
* Improved internal linking within documentation

# tmtyro (development version 0.0.6.9000)

* Better working `visualize()` function as generic with supported methods
* Improved `change_colors()` with added support for the Okabe-Ito colorset and the option of starting with something other than the first color of a palette. With these changes, color options have been removed from other visualization functions to consolidate them within `change_colors()`.
* When a data set includes only one unique `doc_id`, visualizations are no longer divided into facets.
* In an effort to reduce the number of dependencies, many packages have been removed from "Imports" (geomtextpath, ggrepel, glue, NLP, openNLP, plotly, RColorBrewer, stopwords, textstem, wordcloud). Where appropriate, these have been shifted to "Suggests" or dropped entirely.
