# Package index

## Prepare Texts

Functions for collecting, loading, and cleaning a corpus of texts.

### Collecting Texts

- [`get_corpus()`](https://jmclawson.github.io/tmtyro/reference/get_corpus.md)
  : Prepare a corpus or corpora of texts
- [`get_gutenberg_corpus()`](https://jmclawson.github.io/tmtyro/reference/get_gutenberg_corpus.md)
  : Build and load a corpus from Project Gutenberg
- [`get_micusp_corpus()`](https://jmclawson.github.io/tmtyro/reference/get_micusp_corpus.md)
  : Get a MICUSP corpus
- [`download_once()`](https://jmclawson.github.io/tmtyro/reference/download_once.md)
  : Download a file once
- [`micusp_metadata()`](https://jmclawson.github.io/tmtyro/reference/micusp_metadata.md)
  : Get MICUSP metadata
- [`parse_html()`](https://jmclawson.github.io/tmtyro/reference/parse_html.md)
  : Read HTML headers and text from file

### Loading Texts

- [`load_texts()`](https://jmclawson.github.io/tmtyro/reference/load_texts.md)
  : Load a folder or data frame of texts

### Cleaning Text and Metadata

- [`move_header_to_text()`](https://jmclawson.github.io/tmtyro/reference/move_header_to_text.md)
  : Move a header column to text
- [`identify_by()`](https://jmclawson.github.io/tmtyro/reference/identify_by.md)
  : Choose a new doc_id column
- [`standardize_headers()`](https://jmclawson.github.io/tmtyro/reference/standardize_headers.md)
  : Standardize column names from HTML
- [`standardize_titles()`](https://jmclawson.github.io/tmtyro/reference/standardize_titles.md)
  : Standardize document titles
- [`unnest_without_caps()`](https://jmclawson.github.io/tmtyro/reference/unnest_without_caps.md)
  : Split text into words and drop proper nouns

## Measure Text Features

Functions for measuring features of texts and being choosy about how you
do it.

- [`add_dictionary()`](https://jmclawson.github.io/tmtyro/reference/add_dictionary.md)
  : Add values from a dictionary
- [`add_frequency()`](https://jmclawson.github.io/tmtyro/reference/add_frequency.md)
  : Add frequency of words or other features
- [`add_ngrams()`](https://jmclawson.github.io/tmtyro/reference/add_ngrams.md)
  : Add ngram columns
- [`add_partitions()`](https://jmclawson.github.io/tmtyro/reference/add_partitions.md)
  : Divide documents in equal lengths
- [`add_progress()`](https://jmclawson.github.io/tmtyro/reference/add_progress.md)
  [`add_index()`](https://jmclawson.github.io/tmtyro/reference/add_progress.md)
  : Track progress in documents
- [`add_sentiment()`](https://jmclawson.github.io/tmtyro/reference/add_sentiment.md)
  : Add sentiment markers
- [`add_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/add_tf_idf.md)
  : Compare usage across a corpus
- [`add_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/add_vocabulary.md)
  : Measure lexical variety
- [`get_methods_log()`](https://jmclawson.github.io/tmtyro/reference/methods_log.md)
  [`set_methods_log()`](https://jmclawson.github.io/tmtyro/reference/methods_log.md)
  [`add_methods_log()`](https://jmclawson.github.io/tmtyro/reference/methods_log.md)
  : Methods logging
- [`drop_labels()`](https://jmclawson.github.io/tmtyro/reference/drop_labels.md)
  : Drop variable labels
- [`drop_na()`](https://jmclawson.github.io/tmtyro/reference/drop_na.md)
  : Drop rows containing missing values
- [`drop_stopwords()`](https://jmclawson.github.io/tmtyro/reference/drop_stopwords.md)
  : Remove stopwords
- [`summarize_tf_idf()`](https://jmclawson.github.io/tmtyro/reference/summarize_tf_idf.md)
  : Compare usage across a corpus
- [`expand_documents()`](https://jmclawson.github.io/tmtyro/reference/expand_documents.md)
  : Convert data frame from long tidy format to wider format
- [`combine_ngrams()`](https://jmclawson.github.io/tmtyro/reference/combine_ngrams.md)
  : Combine ngram columns
- [`separate_ngrams()`](https://jmclawson.github.io/tmtyro/reference/separate_ngrams.md)
  : Separate one word per column
- [`make_dictionary()`](https://jmclawson.github.io/tmtyro/reference/make_dictionary.md)
  : Create a lexicon

### Model Topics

Model complex relationships in a corpus.

- [`load_topic_model()`](https://jmclawson.github.io/tmtyro/reference/load_topic_model.md)
  : Load (or cache and load) a topic model
- [`make_topic_model()`](https://jmclawson.github.io/tmtyro/reference/make_topic_model.md)
  : Construct a topic model

## Explore Results

Generic functions make it easy to share results with an audience (or
keep them to yourself)

- [`contextualize()`](https://jmclawson.github.io/tmtyro/reference/contextualize.md)
  : Show a term in context
- [`tabulize()`](https://jmclawson.github.io/tmtyro/reference/tabulize.md)
  : Prepare a table of data
- [`visualize()`](https://jmclawson.github.io/tmtyro/reference/visualize.md)
  : Visualize output

### Adjusting tables and figures

- [`collapse_rows()`](https://jmclawson.github.io/tmtyro/reference/collapse_rows.md)
  : Collapse gt rows in the style of kableExtra
- [`change_colors()`](https://jmclawson.github.io/tmtyro/reference/change_colors.md)
  : Choose other colors
- [`italicize_titles()`](https://jmclawson.github.io/tmtyro/reference/italicize_titles.md)
  : Italicize document titles in a table or figure
- [`theme_tmtyro()`](https://jmclawson.github.io/tmtyro/reference/theme_tmtyro.md)
  : Apply a smart tmtyro theme

## Understanding Process

Labels and method logs keep workflows comprehensible

- [`narrativize()`](https://jmclawson.github.io/tmtyro/reference/narrativize.md)
  : Describe the steps taken

### Adjusting labels and managing the methods log

- [`narrative_dictionary_en`](https://jmclawson.github.io/tmtyro/reference/narrative_dictionary_en.md)
  : Default narrative dictionary
- [`get_data_dictionary()`](https://jmclawson.github.io/tmtyro/reference/get_data_dictionary.md)
  : Prepare a data dictionary from column labels
- [`set_data_dictionary()`](https://jmclawson.github.io/tmtyro/reference/set_data_dictionary.md)
  : Assign column labels from a data dictionary
- [`get_methods_log()`](https://jmclawson.github.io/tmtyro/reference/methods_log.md)
  [`set_methods_log()`](https://jmclawson.github.io/tmtyro/reference/methods_log.md)
  [`add_methods_log()`](https://jmclawson.github.io/tmtyro/reference/methods_log.md)
  : Methods logging

## Vectorized functions

- [`get_cumulative_vocabulary()`](https://jmclawson.github.io/tmtyro/reference/get_cumulative_vocabulary.md)
  : Cumulative total of vocabulary size

- [`get_df_by()`](https://jmclawson.github.io/tmtyro/reference/get_df_by.md)
  :

  Get document frequencies of values in one vector `x` categorized by
  another vector `by`.

- [`get_frequency()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md)
  [`get_tf()`](https://jmclawson.github.io/tmtyro/reference/get_frequency.md)
  : Get frequencies of values in a vector

- [`get_hir()`](https://jmclawson.github.io/tmtyro/reference/get_hir.md)
  : Cumulative hapax introduction ratio

- [`get_htr()`](https://jmclawson.github.io/tmtyro/reference/get_htr.md)
  : Cumulative hapax-token ratio

- [`get_idf_by()`](https://jmclawson.github.io/tmtyro/reference/get_idf_by.md)
  :

  Get inverse document frequencies of values in one vector `x`
  categorized by another vector `by`.

- [`get_match()`](https://jmclawson.github.io/tmtyro/reference/get_match.md)
  : Get dictionary matches of values in a vector

- [`get_sentiment()`](https://jmclawson.github.io/tmtyro/reference/get_sentiment.md)
  : Get sentiment matches of values in a vector

- [`get_tf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tf_by.md)
  :

  Get term frequencies of values in one vector `x` categorized by
  another vector `by`.

- [`get_tfidf_by()`](https://jmclawson.github.io/tmtyro/reference/get_tfidf_by.md)
  : Term frequency–inverse document frequency

- [`get_ttr()`](https://jmclawson.github.io/tmtyro/reference/get_ttr.md)
  : Cumulative type-token ratio

- [`is_hapax()`](https://jmclawson.github.io/tmtyro/reference/is_hapax.md)
  : Check for hapax legomena

- [`is_new()`](https://jmclawson.github.io/tmtyro/reference/is_new.md) :
  Check for new words in a vocabulary

## Data

Data included

- [`pos_tags`](https://jmclawson.github.io/tmtyro/reference/pos_tags.md)
  : Part of speech tags
