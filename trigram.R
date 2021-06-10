# http://uc-r.github.io/creating-text-features

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
library(corpus)

text_df <- readRDS("./data/text_df.rds")
stopwords_IT2 <- read.table("./data/stopwords IT.txt", header = T, sep = "\t")
tm_stopwords <- tibble::as_tibble(tm::stopwords("it")) %>%
  dplyr::rename(word = value)
stopwords_IT2 <- bind_rows(
  stopwords_IT2,
  tm_stopwords
)
tm_stopwords <- NULL

# lemma <- read.csv("./data/lemmatizzazione_IT.txt", header = T, sep = "\t", fill = T) %>%
#   dplyr::mutate(lemma = trimws(lemma),
#                 word = trimws(word)) %>%
#   dplyr::as_tibble()
# verbi <- readxl::read_xlsx("./data/verbi.xlsx") %>%
#   dplyr::as_tibble()


tidy_books <- text_df %>%
  unnest_tokens(word, text)
stopword_brand <- text_df$line %>% unlist %>% trimws() %>% str_split(., "\\s+")
stopword_brand <- stopword_brand %>% unlist()
stopword_brand <- tibble::as_tibble(stopword_brand) %>%
  dplyr::rename(stopword_brand = value)

tidy_books <- tidy_books %>%
  dplyr::anti_join(., stopwords_IT2, by = c("word" = "stopwords_IT")) %>%
  # dplyr::anti_join(tm_stopwords) %>%
  dplyr::anti_join(stopword_brand, by = c("word" = "stopword_brand")) %>%
  # dplyr::anti_join(lemma) %>%
  # dplyr::anti_join(verbi) %>%
  filter(
    !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  )
# %>%
#   dplyr::group_by(line) %>%
#   dplyr::mutate(n = n()) %>%
#   dplyr::ungroup() %>%
#   # dplyr::filter(n > 5) %>%
#   dplyr::mutate(n = NULL)


word_counts <- tidy_books %>%
  dplyr::count(word, sort = TRUE)

word_counts %>%
  ggplot(aes(n)) +
  geom_histogram() +
  scale_x_log10()

word_counts  %>%
  arrange(n)

word_counts <- tidy_books %>%
  dplyr::mutate(word = corpus::text_tokens(word, stemmer = "it") %>% unlist()) %>% # add stemming process
  dplyr::count(word, sort = TRUE)

word_counts %>%
  ggplot(aes(n)) +
  geom_histogram() +
  scale_x_log10()

word_counts  %>%
  arrange(n)

bigram <- text_df %>%
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- bigram %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  dplyr::filter(!word1 %in% stopword_brand$stopword_brand) %>%
  dplyr::filter(!word2 %in% stopword_brand$stopword_brand) %>%
  dplyr::filter(!word1 %in% stopwords_IT2$stopwords_IT) %>%
  dplyr::filter(!word2 %in% stopwords_IT2$stopwords_IT) %>%
  filter(
    !str_detect(word1, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word1, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word1, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word1, pattern = "\\b(.)\\b"),    # removes any remaining single letter words
    !str_detect(word2, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word2, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word2, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word2, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  )

ngram_list <- bigrams_filtered %>%
  tidyr::unite("bigram", c(word1, word2), sep = " ") %>%
  dplyr::count(bigram) %>%
  dplyr::filter(n >= 10) %>% # filter for bi-grams used 10 or more times
  dplyr::pull(bigram)


trigram <- text_df %>%
  tidytext::unnest_tokens(trigram, text, token = "ngrams", n = 3)

trigrams_separated <- biagram %>%
  tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  dplyr::filter(!word1 %in% stopword_brand$stopword_brand) %>%
  dplyr::filter(!word2 %in% stopword_brand$stopword_brand) %>%
  dplyr::filter(!word3 %in% stopword_brand$stopword_brand) %>%
  dplyr::filter(!word1 %in% stopwords_IT2$stopwords_IT) %>%
  dplyr::filter(!word2 %in% stopwords_IT2$stopwords_IT) %>%
  dplyr::filter(!word3 %in% stopwords_IT2$stopwords_IT) %>%
  filter(
    !str_detect(word1, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word1, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word1, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word1, pattern = "\\b(.)\\b"),    # removes any remaining single letter words
    !str_detect(word2, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word2, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word2, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word2, pattern = "\\b(.)\\b"),    # removes any remaining single letter words
    !str_detect(word3, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word3, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word3, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word3, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  )

ngram_list <- trigrams_filtered %>%
  tidyr::unite("trigram", c(word1, word2, word3), sep = " ") %>%
  dplyr::count(trigram, sort = T) %>%
  dplyr::filter(n >= 5) %>% # filter for bi-grams used 10 or more times
  dplyr::pull(trigram)



bigrams_filtered <- bigrams_filtered %>%
  dplyr::mutate(line = NULL)

count_w1 <- bigrams_filtered %>%
  dplyr::count(word1)

count_w2 <- bigrams_filtered %>%
  dplyr::count(word2)

# compute counts for bi-grams
count_w12 <- bigrams_filtered %>%
  dplyr::count(word1, word2)

# get the original number of all bi-grams
N <- nrow(bigrams_filtered)

# join this information and compute log-likelihood
LL_test <- count_w12 %>%
  dplyr::left_join(count_w1, by = "word1") %>%
  dplyr::left_join(count_w2, by = "word2") %>%
  dplyr::rename(c_w1 = n.y, c_w2 = n, c_w12 = n.x) %>%
  dplyr::mutate(
    p = c_w2 / N,
    p1 = c_w12 / c_w1,
    p2 = (c_w2 - c_w12) / (N - c_w1),
    LL = log((pbinom(c_w12, c_w1, p) * pbinom(c_w2 - c_w12, N - c_w1, p)) / (pbinom(c_w12, c_w1, p1) * pbinom(c_w2 - c_w12, N - c_w1, p)))
  )
head(LL_test)

unique_bigrams <- LL_test %>%
  mutate(
    Chi_value = -2 * LL,
    pvalue = pchisq(LL, df = 1)
  ) %>%
  filter(pvalue < 0.05) %>%
  select(word1, word2) %>%
  unite(bigram, word1, word2, sep = " ")

head(unique_bigrams)


# https://juliasilge.com/blog/tidy-word-vectors/
unigram_probs <- word_counts %>%
  dplyr::mutate(p = n / sum(n))

unigram_probs


# https://juanitorduz.github.io/text-mining-networks-and-visualization-plebiscito-tweets/

bi.gram.count <- bigrams_filtered %>%
  dplyr::count(word1, word2) %>% 
  dplyr::rename(weight = n)

bi.gram.count %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram Weight Distribution")

bi.gram.count %>% 
  mutate(weight = log(weight + 1)) %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram log-Weight Distribution")


threshold <- 8

# For visualization purposes we scale by a global factor. 
ScaleWeight <- function(x, lambda) {
  x / lambda
}

library(igraph)
network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
  graph_from_data_frame(directed = FALSE)

network

is.weighted(network)

library(glue)
plot(
  network, 
  vertex.size = 1,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  edge.color = 'gray', 
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)


# Store the degree.
V(network)$degree <- strength(graph = network)

# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

plot(
  network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 2*V(network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(network)$width ,
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)


# Get all connected components.
clusters(graph = network)


# Select biggest connected component.  
V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)

cc.network 

# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)

# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)

plot(
  cc.network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 10*V(cc.network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(cc.network)$width ,
  main = 'Bigram Count Network (Biggest Connected Component)', 
  sub = glue('Weiight Threshold: {threshold}'), 
  alpha = 50
)


library(networkD3)
library(magrittr)
# Treshold
# threshold <- 5

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)


# Compute the centrality measures for the biggest connected component from above.
node.impo.df <- tibble(
  word = V(cc.network)$name,  
  degree = strength(graph = cc.network),
  closeness = closeness(graph = cc.network), 
  betweenness = betweenness(graph = cc.network)
)

node.impo.df %>% 
  arrange(- degree) %>%
  head(10)

node.impo.df %>% 
  arrange(- closeness) %>%
  head(10)


node.impo.df %>% 
  arrange(- betweenness) %>% 
  head(10)

plt.deg <- node.impo.df %>% 
  ggplot(mapping = aes(x = degree)) +
  theme_light() +
  geom_histogram(fill = 'blue', alpha = 0.8, bins = 30)

plt.clo <- node.impo.df %>% 
  ggplot(mapping = aes(x = closeness)) +
  theme_light() +
  geom_histogram(fill = 'red', alpha = 0.8, bins = 30)

plt.bet <- node.impo.df %>% 
  ggplot(mapping = aes(x = betweenness)) +
  theme_light() +
  geom_histogram(fill = 'green4', alpha = 0.8, bins = 30)

library(cowplot)
plot_grid(
  ... = plt.deg, 
  plt.clo, 
  plt.bet, 
  ncol = 1, 
  align = 'v'
)


comm.det.obj <- cluster_louvain(
  graph = cc.network, 
  weights = E(cc.network)$weight
)

comm.det.obj

V(cc.network)$membership <- membership(comm.det.obj)

# We use the membership label to color the nodes.
network.D3$nodes$Group <- V(cc.network)$membership

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)
