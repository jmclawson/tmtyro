# Changelog

## tmtyro 0.6

- Variables now gain descriptive labels with new options to toggle label
  use. Turn off labels locally by setting
  `options(tmtyro.use_labels = FALSE)` within a document or turn them
  off for a profile or project by setting the environment variable
  `TMTYRO_USE_LABELS` to `FALSE`.
  [`get_data_dictionary()`](https://jmclawson.github.io/tmtyro/reference/get_data_dictionary.md)
  prepares an explanatory data dictionary using these labels,
  [`set_data_dictionary()`](https://jmclawson.github.io/tmtyro/reference/set_data_dictionary.md)
  modifies labels from a provided data dictionary, and
  [`drop_labels()`](https://jmclawson.github.io/tmtyro/reference/drop_labels.md)
  removes labels.
- Optional logging now records steps for most tmtyro functions working
  at the level of data frame, with options to toggle use of this log.
  Turn off logging locally by setting `options(tmtyro.use_log = FALSE)`
  within a document or turn it off for a profile or project by setting
  the environment variable `TMTYRO_USE_LOG` to `FALSE`. Helper functions
  [`get_methods_log()`](https://jmclawson.github.io/tmtyro/reference/methods_log.md),
  [`set_methods_log()`](https://jmclawson.github.io/tmtyro/reference/methods_log.md),
  and
  [`add_methods_log()`](https://jmclawson.github.io/tmtyro/reference/methods_log.md)
  allow for management and adding manual entries to the log.
- New
  [`narrativize()`](https://jmclawson.github.io/tmtyro/reference/narrativize.md)
  function uses log to print a narrative describing methods used.
- Support for ggplot2 version 4.0+.
- New
  [`theme_tmtyro()`](https://jmclawson.github.io/tmtyro/reference/theme_tmtyro.md)
  function extracts and modularizes theme defaults with smart grid lines
  using S7 methods.
- New `percent` argument in
  [`get_tf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tf_by.md)
  for consonance with
  [`get_tf()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md).
- New
  [`get_df_by()`](https://jmclawson.github.io/tmtyro/reference/get_df_by.md)
  function for getting document frequencies in correspondence with
  [`get_tf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tf_by.md).
- New `html` parameter in
  [`contextualize()`](https://jmclawson.github.io/tmtyro/reference/contextualize.md)
  ensures HTML output.
- Improvements to
  [`get_gutenberg_corpus()`](https://jmclawson.github.io/tmtyro/reference/get_gutenberg_corpus.md)
  handle file downloading:
  - Cached files can now be used without a network connection
  - New `download` argument directs handling of file downloads and
    location
- Improvements to
  [`parse_html()`](https://jmclawson.github.io/tmtyro/reference/parse_html.md):
  - New `headers` argument limits headers to user-assigned range
  - New `standardize_headers` argument allows keeping header tags for
    transparency
  - New
    [`standardize_headers()`](https://jmclawson.github.io/tmtyro/reference/standardize_headers.md)
    function for managing standardization of column names from HTML tags
- [`add_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/add_vocabulary.md)
  now adds fewer columns. Chain this function with
  [`add_progress()`](https://jmclawson.github.io/tmtyro/reference/add_progress.md)
  to regain those that have been dropped.
- [`add_index()`](https://jmclawson.github.io/tmtyro/reference/add_progress.md)
  is now a thin wrapper for
  [`add_progress()`](https://jmclawson.github.io/tmtyro/reference/add_progress.md),
  a new function that adds support for measuring progress by percentage
  and for specifying units used in labels.
- [`tabulize()`](https://jmclawson.github.io/tmtyro/reference/tabulize.md)
  returns better formatted tables for every type.
- A new
  [`italicize_titles()`](https://jmclawson.github.io/tmtyro/reference/italicize_titles.md)
  generic function simplifies formatting of the doc_id column (or other
  columns) for tables made with
  [`tabulize()`](https://jmclawson.github.io/tmtyro/reference/tabulize.md)
  and for figures made with
  [`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md).
- Where possible, visualizations avoid reprinting Y-axis values for
  small multiples from
  [`plot_doc_word_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_bars.md)
  when Y-axis isn’t rearranged.
- More unit tests added to increase coverage
- REMOVED: Setting `labeling = "axis"` is no longer supported because of
  deprecations in ggh4x, which is no longer imported

## tmtyro 0.5

- New functions
  [`add_frequency()`](https://jmclawson.github.io/tmtyro/reference/add_frequency.md)
  and
  [`add_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/add_tf_idf.md)
  allow for consistent phrasing of workflows. These new methods are
  supported by
  [`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)
  and
  [`tabulize()`](https://jmclawson.github.io/tmtyro/reference/tabulize.md).
- New vectorized functions support using dplyr’s
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
  similar use cases:
  [`get_frequency()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md)
  for returning counts and ratios of values in a vector;
  [`is_new()`](https://jmclawson.github.io/tmtyro/reference/is_new.md)
  and
  [`is_hapax()`](https://jmclawson.github.io/tmtyro/reference/is_hapax.md)
  for testing uniqueness of values in a vector;
  [`get_cumulative_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/get_cumulative_vocabulary.md),
  [`get_ttr()`](https://jmclawson.github.io/tmtyro/reference/get_ttr.md),
  [`get_hir()`](https://jmclawson.github.io/tmtyro/reference/get_hir.md),
  and
  [`get_htr()`](https://jmclawson.github.io/tmtyro/reference/get_htr.md)
  for measuring the cumulative change of a vector over time;
  [`get_match()`](https://jmclawson.github.io/tmtyro/reference/get_match.md)
  and
  [`get_sentiment()`](https://jmclawson.github.io/tmtyro/reference/get_sentiment.md)
  for matching values in a dictionary; and
  [`get_tf()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md),
  [`get_tf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tf_by.md),
  [`get_idf_by()`](https://jmclawson.github.io/tmtyro/reference/get_idf_by.md),
  and
  [`get_tfidf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tfidf_by.md)
  for weighing elements of term frequency–inverse document frequency.
- Bar plots of words per document now use better logic with labels, and
  a new `label_color` argument allows for customizing label color when
  needed.
- Added articles documenting vectorized functions, customizing tables,
  and customizing figures.
- [`count()`](https://dplyr.tidyverse.org/reference/count.html) function
  no longer imported from dplyr.
- Demonstrate
  [`expand_documents()`](https://jmclawson.github.io/tmtyro/reference/expand_documents.md)
  in package documentation.

## tmtyro 0.4.1

- [`get_gutenberg_corpus()`](https://jmclawson.github.io/tmtyro/reference/get_gutenberg_corpus.md)
  should do less, and now it does. Other functionality is available via
  gutenbergr.

## tmtyro 0.4.0

- New function
  [`contextualize()`](https://jmclawson.github.io/tmtyro/reference/contextualize.md)
  shows terms in a window of context
- New function
  [`add_index()`](https://jmclawson.github.io/tmtyro/reference/add_progress.md)
  adds a column showing word indices within each document
- [`load_texts()`](https://jmclawson.github.io/tmtyro/reference/load_texts.md)
  adds support to keep original capitalization and punctuation alongside
  the tokenized `word` column with the `keep_original` argument. This
  process does not work in all instances, so the option defaults to
  `FALSE`.
- [`add_dictionary()`](https://jmclawson.github.io/tmtyro/reference/add_dictionary.md)
  includes an option to keep original terms. This is useful for n-gram
  dictionaries, where a match might otherwise span multiple rows.
- [`add_ngrams()`](https://jmclawson.github.io/tmtyro/reference/add_ngrams.md)
  supports negative ranges, for building context windows
- [`add_partitions()`](https://jmclawson.github.io/tmtyro/reference/add_partitions.md)
  supports overlapping partitions
- [`standardize_titles()`](https://jmclawson.github.io/tmtyro/reference/standardize_titles.md)
  capitalizes words after terminal punctuation

## tmtyro 0.3.0

- [`add_dictionary()`](https://jmclawson.github.io/tmtyro/reference/add_dictionary.md)
  now supports n-gram dictionaries, matching across multiple words
- [`make_dictionary()`](https://jmclawson.github.io/tmtyro/reference/make_dictionary.md)
  has a slightly changed syntax, with clearer argument names
  `definitions` and `name`
- Along with its related
  [`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)
  methods,
  [`plot_doc_word_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_bars.md)
  improves support for `color_y = TRUE` and `reorder_y = TRUE`
- When naming colors, `change_color()` now allows setting colors for
  unnamed values
- [`standardize_titles()`](https://jmclawson.github.io/tmtyro/reference/standardize_titles.md)
  capitalizes Roman numerals
- [`load_texts()`](https://jmclawson.github.io/tmtyro/reference/load_texts.md)
  adds support for custom tokenization using the dots parameter from
  [`tidytext::unnest_tokens()`](https://juliasilge.github.io/tidytext/reference/unnest_tokens.html)

## tmtyro 0.2.0

- New function
  [`add_partitions()`](https://jmclawson.github.io/tmtyro/reference/add_partitions.md)
  adds a partition column, useful for getting same-sized samples
- [`identify_by()`](https://jmclawson.github.io/tmtyro/reference/identify_by.md)
  now works with multiple columns, and it keeps existing metadata
  columns. This is especially useful with the new
  [`add_partitions()`](https://jmclawson.github.io/tmtyro/reference/add_partitions.md)
  column, using something like
  `my_corpus() |> add_partitions() |> identify_by(title, partition)`
  before continuing to work with partitioned documents. To return
  framing to unpartitioned data, used `identify_by(title)` or whatever
  other column is most relevant.
- New visualization and tabulization methods for
  [`expand_documents()`](https://jmclawson.github.io/tmtyro/reference/expand_documents.md)
- Functions now imported:
  [`count()`](https://dplyr.tidyverse.org/reference/count.html) and
  [`drop_na()`](https://jmclawson.github.io/tmtyro/reference/drop_na.md)
- When the ggraph package is loaded,
  [`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md)
  now uses a color scale on edges, rather than spot color on nodes, with
  full support for `change_color()`
- Improved documentation with website articles for customizing colors
  and showing code comparisons

## tmtyro 0.1.0

- First “public” release! 🎉
- Unnecessary components removed and dependencies reduced
- Examples standardized and made reproducible
- [`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md)
  now works with
  [`plot_bigrams()`](https://jmclawson.github.io/tmtyro/reference/plot_bigrams.md)
- [`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md)
  now includes a “dubois” colorset
- [`tabulize()`](https://jmclawson.github.io/tmtyro/reference/tabulize.md)
  documentation is now improved for online output
- [`standardize_titles()`](https://jmclawson.github.io/tmtyro/reference/standardize_titles.md)
  now works with factors
- Added default behavior for
  [`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)
  on a corpus
- Part of speech tagging should now work for more texts

## tmtyro (development version 0.0.8.9000)

- New
  [`tabulize()`](https://jmclawson.github.io/tmtyro/reference/tabulize.md)
  generic function for preparing tables with supported methods
- Standardizing argument names between
  [`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)
  and
  [`tabulize()`](https://jmclawson.github.io/tmtyro/reference/tabulize.md)
- New package documentation for getting started
- New
  [`collapse_rows()`](https://jmclawson.github.io/tmtyro/reference/collapse_rows.md)
  function for clean tables using
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html)
- New feature in
  [`standardize_titles()`](https://jmclawson.github.io/tmtyro/reference/standardize_titles.md)
  to keep initial articles
- New options in
  [`plot_doc_word_bars()`](https://jmclawson.github.io/tmtyro/reference/plot_doc_word_bars.md)
  to keep the order of Y-axis values consistent and to color by Y-axis
  value instead of by facet
- Rename `add_lexical_diversity()` to
  [`add_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/add_vocabulary.md)
- Add option for renaming existing `doc_id` column when using
  [`identify_by()`](https://jmclawson.github.io/tmtyro/reference/identify_by.md)

## tmtyro (development version 0.0.7.9000)

- [`get_gutenberg_corpus()`](https://jmclawson.github.io/tmtyro/reference/get_gutenberg_corpus.md)
  now retrieves HTML versions of texts from Project Gutenberg and parses
  header tags for section markers
- New function
  [`parse_html()`](https://jmclawson.github.io/tmtyro/reference/parse_html.md)
  for reading headers in an HTML file
- New function
  [`move_header_to_text()`](https://jmclawson.github.io/tmtyro/reference/move_header_to_text.md)
  for converting header to text
- New function
  [`identify_by()`](https://jmclawson.github.io/tmtyro/reference/identify_by.md)
  to simplify using something other than `doc_id`
- Improved internal linking within documentation

## tmtyro (development version 0.0.6.9000)

- Better working
  [`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)
  function as generic with supported methods
- Improved
  [`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md)
  with added support for the Okabe-Ito colorset and the option of
  starting with something other than the first color of a palette. With
  these changes, color options have been removed from other
  visualization functions to consolidate them within
  [`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md).
- When a data set includes only one unique `doc_id`, visualizations are
  no longer divided into facets.
- In an effort to reduce the number of dependencies, many packages have
  been removed from “Imports” (geomtextpath, ggrepel, glue, NLP,
  openNLP, plotly, RColorBrewer, stopwords, textstem, wordcloud). Where
  appropriate, these have been shifted to “Suggests” or dropped
  entirely.
