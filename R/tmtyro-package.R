## fixing "no visible binding" notes
affect_dimension <- temp <- NULL
adjusted <- temp <- NULL
arousal <- temp <- NULL
category <- temp <- NULL
context <- temp <- NULL
corpus <- temp <- NULL
.data <- temp <- NULL
display <- temp <- NULL
doc_id <- doc <- temp <- NULL
docranksum <- temp <- NULL
document <- temp <- NULL
dominance <- temp <- NULL
end <- temp <- NULL
feature <- temp <- NULL
fill_it <- temp <- NULL
Freq <- temp <- NULL
h1 <- h2 <- h3 <- h4 <- h5 <- h6 <- temp <- NULL
hapax <- temp <- NULL
hir <- temp <- NULL
hlemona <- temp <- NULL
htr <- temp <- NULL
id <- temp <- NULL
idf <- temp <- NULL
label_color <- temp <- NULL
level <- temp <- NULL
line_num <- temp <- NULL
n <- temp <- NULL
n2 <- temp <- NULL
n3 <- temp <- NULL
n_smooth <- temp <- NULL
name <- temp <- NULL
ngram <- temp <- NULL
new_word <- temp <- NULL
node1.name <- node2.name <- temp <- NULL
original <- temp <- NULL
paper_id <- temp <- NULL
par_num <- temp <- NULL
partition <- temp <- NULL
percent <- temp <- NULL
progress_words <- temp <- NULL
ranksum <- temp <- NULL
reorder <- temp <- NULL
rnumeral <- temp <- NULL
sentiment_valence <- temp <- NULL
score <- temp <- NULL
sentiment <- temp <- NULL
set <- temp <- NULL
set_count <- temp <- NULL
set_id <- temp <- NULL
.size <- temp <- NULL
stanza_num <- temp <- NULL
start <- temp <- NULL
tag <- temp <- NULL
term <- temp <- NULL
.test1 <- temp <- NULL
.test2 <- temp <- NULL
text <- temp <- NULL
tf <- temp <- NULL
tf_idf <- temp <- NULL
title <- temp <- NULL
topic <- temp <- NULL
topics <- temp <- NULL
topic_mean <- temp <- NULL
topic_rank <- temp <- NULL
true_rank <- temp <- NULL
ttr <- temp <- NULL
type <- temp <- NULL
valence <- temp <- NULL
value <- temp <- NULL
Var1 <- temp <- NULL
Var2 <- temp <- NULL
vocab <- temp <- NULL
vocabulary <- temp <- NULL
word <- temp <- NULL
word_index <- temp <- NULL
words <- temp <- NULL

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom ggplot2 update_ggplot class_ggplot
#' @importFrom grDevices colorRampPalette
#' @importFrom gt gt cells_body cols_label css fmt_number sub_missing tab_spanner tab_style
#' @importFrom gutenbergr gutenberg_download gutenberg_get_mirror
#' @importFrom rlang :=
#' @importFrom stats median reorder setNames
#' @importFrom tidyr drop_na
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("S7")) {
    S7::methods_register()
  }
}
