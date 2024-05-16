# tmtyro (development version 0.0.6.9000)

* Corpora with just one document will no longer be divided into facets
* `visualize()` converted to a generic function
* Parameters related to colors (`colorset`, `na_color`, `line_color`) have been removed from most plotting functions. Instead, use `change_colors()`.
* Okabe-Ito `colorset` option added to `change_colors()`.
* `start` parameter added to `change_colors()`.
* Packages removed from Imports: geomtextpath, ggrepel, glue, NLP, openNLP, plotly, RColorBrewer, stopwords, textstem, wordcloud
* Packages added to Suggests: geomtextpath, ggrepel, NLP, openNLP, plotly, RColorBrewer, textstem, wordcloud
