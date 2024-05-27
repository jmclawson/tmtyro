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
