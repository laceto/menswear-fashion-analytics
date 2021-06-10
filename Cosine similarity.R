# http://varianceexplained.org/r/op-ed-text-analysis/
library(purrr)
library(shiny)
library(shinydashboard)
library(stringr)
library(tibble)
library(DT)
library(tm)
library(SnowballC)
library(tidytext)
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)

  
text_df <- readRDS("./data/text_df.rds")
stopwords_IT2 <- read.table("./data/stopwords IT.txt", header = T, sep = "\t")
word <- tm::stopwords("it")
tm_stopwords <- tibble::as_tibble(word) %>%
  dplyr::rename(word = value)
lemma <- read.csv("./data/lemmatizzazione_IT.txt", header = T, sep = "\t", fill = T) %>%
  dplyr::mutate(lemma = trimws(lemma),
                word = trimws(word)) %>%
  dplyr::as_tibble()
verbi <- readxl::read_xlsx("./data/verbi.xlsx") %>%
  dplyr::as_tibble()
tidy_books <- text_df %>%
  unnest_tokens(word, text)
stopword_brand <- text_df$line %>% unlist %>% trimws() %>% str_split(., "\\s+")
stopword_brand <- stopword_brand %>% unlist()
stopword_brand <- tibble::as_tibble(stopword_brand) %>%
  dplyr::rename(stopword_brand = value)

tidy_books <- tidy_books %>%
  dplyr::anti_join(., stopwords_IT2, by = c("word" = "stopwords_IT")) %>%
  dplyr::anti_join(tm_stopwords) %>%
  dplyr::anti_join(stopword_brand, by = c("word" = "stopword_brand")) %>%
  dplyr::anti_join(lemma) %>%
  dplyr::anti_join(verbi) %>%
  dplyr::group_by(line) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 5) %>%
  dplyr::mutate(n = NULL)

word_counts <- tidy_books %>%
  dplyr::count(line, word, sort = TRUE)

word_counts  %>%
  dplyr::count(word, sort = TRUE) %>%
  head(16) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  labs(y = "# of uses among staff Twitter accounts")

# Compute TF-IDF using "word" as term and "screen_name" as document
word_tf_idf <- word_counts %>%
  bind_tf_idf(word, line, n) %>%
  arrange(desc(tf_idf))

# word_tf_idf %>%
#   dplyr::filter(n > 1)

# word_tf_idf %>%
#   dplyr::select(line) %>%
#   dplyr::distinct() %>%
#   dplyr::arrange(line) %>% view()

library(drlib)
selected <- c("tonello", "aglini", "briglia 1949", "berwich", "carhartt", "borriello")

word_tf_idf %>%
  dplyr::filter(line %in% selected) %>%
  dplyr::group_by(line) %>%
  dplyr::top_n(12, tf_idf) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(word = reorder_within(word, tf_idf, line)) %>%
  ggplot(aes(word, tf_idf, fill = line)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ line, scales = "free_y") +
  labs(x = "",
       y = "TF-IDF vectors of this word for this user",
       title = "TF-IDF: top words for selected staff members")


library(widyr)

# Find similarities between screen names
# upper = FALSE specifies that we don't want both A-B and B-A matches
# Look only at the similarity of the op-ed to other documents

brand <- "cucinelli"

op_ed_similarity <- word_tf_idf %>%
  pairwise_similarity(line, word, tf_idf, upper = FALSE, sort = TRUE) %>%
  # filter(item1 ==  "obvious")) %>%
  filter(str_detect(item1, brand))

op_ed_similarity %>%
  head(12) %>%
  mutate(item2 = reorder(item2, similarity)) %>%
  ggplot(aes(item2, similarity)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ item1, scales = "free_y") +
  labs(x = "",
       y = "Cosine similarity between TF-IDF vectors",
       subtitle = "Based on 69 selected staff accounts",
       title = "Twitter accounts using words similar to NYTimes op-ed")







# This takes a little R judo, but it's worth the effort

# First we normalize the TF-IDF vector for each screen name,
# necessary for cosine similarity
tf_idf <- word_tf_idf %>%
  group_by(line) %>%
  mutate(normalized = tf_idf / sqrt(sum(tf_idf ^ 2))) %>%
  ungroup()

# Then we join the op-ed words with the full corpus, and find
# the product of their TF-IDF with it in other documents
word_combinations <- tf_idf %>%
  filter(line == brand) %>%
  select(-line) %>%
  inner_join(tf_idf, by = "word", suffix = c("_oped", "_twitter")) %>%
  filter(line != brand) %>%
  mutate(contribution = normalized_oped * normalized_twitter) %>%
  arrange(desc(contribution)) %>%
  select(line, word, tf_idf_oped, tf_idf_twitter, contribution)

word_combinations %>%
  filter(line %in% head(op_ed_similarity$item2)) %>%
  mutate(line = reorder(line, -contribution, sum),
         word = reorder_within(word, contribution, line)) %>%
  group_by(line) %>%
  top_n(20, contribution) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, contribution, line)) %>%
  ggplot(aes(word, contribution, fill = line)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ line, scales = "free_y") +
  coord_flip() +
  labs(x = "",
       y = "Contribution to similarity score",
       title = "What caused each Twitter account to be similar to the article",
       subtitle = "For the 6 accounts with the highest similarity score")
