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
