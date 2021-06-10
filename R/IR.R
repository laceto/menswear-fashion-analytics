#load all content files
text_df <- readRDS("./data/text_df.rds")
stopwords_IT2 <- read.table("./data/stopwords IT.txt", header = T, sep = "\t")
tm_stopwords <- tibble(word = tm::stopwords("it"))
lemma <- read.csv("./data/lemmatizzazione_IT.txt", header = T, sep = "\t", fill = T) %>%
  dplyr::mutate(lemma = trimws(lemma),
                word = trimws(word)) %>%
  dplyr::as_tibble()
verbi <- readxl::read_xlsx("./data/verbi.xlsx") %>%
  dplyr::as_tibble()

#---- data manipulation

tidy_books <- text_df %>%
  unnest_tokens(word, text)

stopword_brand <- text_df$line %>% unlist %>% trimws() %>% str_split(., "\\s+")
stopword_brand <- stopword_brand %>% unlist()
stopword_brand <- tibble(stopword_brand = stopword_brand)

tidy_books <- tidy_books %>%
  dplyr::anti_join(., stopwords_IT2, by = c("word" = "stopwords_IT")) %>%
  dplyr::anti_join(tm_stopwords) %>%
  dplyr::anti_join(stopword_brand, by = c("word" = "stopword_brand")) %>%
  # dplyr::anti_join(lemma) %>%
  dplyr::anti_join(verbi)

tidy_books <- tidy_books %>%
  dplyr::left_join(lemma) %>%
  dplyr::mutate(word = if_else(is.na(lemma), word, lemma),
                lemma = NULL)

books <- tidy_books %>%
  dplyr::group_by(line) %>%
  dplyr::summarise(text = paste(word, collapse = " "))

# lemma  %>%
#   dplyr::filter(str_detect(word, "camicie"))

# news_list = books$text
# N.docs = length(news_list)
# names(news_list) = books$line
# 
# #load search queries
# search_queries = "abbigliamento sartoriale contemporaneo"
# queries_list = unlist(search_queries)
# N.query = length(queries_list)
# names(queries_list) = paste0("query", c(1:N.query))
# 
# #preprocess data news content
# #append both content and search queries together, convert the lists to VectorSource
# newscorpus = VectorSource(c(news_list,queries_list))
# newscorpus$Names = c(names(news_list),names(queries_list))
# #convert to corpus format
# newscorpus_preproc = Corpus(newscorpus)
# #cleaning the data
# newscorpus_preproc = tm_map(newscorpus_preproc,stripWhitespace)
# newscorpus_preproc = tm_map(newscorpus_preproc,removePunctuation)
# newscorpus_preproc = tm_map(newscorpus_preproc,content_transformer(tolower))
# newscorpus_preproc = tm_map(newscorpus_preproc,removeWords,stopwords("italian"))
# 
# #create tdm using weighted tfidf weightage
# tdm = TermDocumentMatrix(newscorpus_preproc,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
# tdm_mat = as.matrix(tdm)
# colnames(tdm_mat) = c(names(news_list),names(queries_list))
# 
# #normalizing the term document matrix
# tfidf_mat <- scale(tdm_mat, center = FALSE,scale = sqrt(colSums(tdm_mat^2)))
# 
# #seperating query tdm matrix and content tdm matrix
# query.vectors <- tfidf_mat[, (N.docs + 1):(N.docs+N.query)]
# tfidf_mat <- tfidf_mat[, 1:N.docs]
# 
# #calculating the similarity scores
# doc.scores <- t(query.vectors) %*% tfidf_mat
# 
# results.df <- data.frame(querylist = queries_list,doc.scores)
# 
# #function to display the final results
# query <- search_queries
# showTopresults <- function(query){
#   x = results.df[which(results.df$querylist == query),]
#   yy =  data.frame(t(x),rownames(t(x)),row.names = NULL)[-1,]
#   names(yy) = c("score","docs")
#   yy$score = as.numeric(as.character(yy$score))
#   yyy = yy[order(yy$score,decreasing = T),]
#   
#   return(yyy[which(yyy$score > 0),])
# }
# 
# #test the function
# showTopresults(search_queries)

# books <- tidy_books %>%
#   dplyr::group_by(line) %>%
#   dplyr::summarise(text = paste(word, collapse = " "))
# %>%
#   dplyr::filter(str_detect(line, "berwich|briglia|torino|adidas|fiesoli|lardini|hogan"))
# search_queries = "abbigliamento sartoriale contemporaneo"

IR <- function(books, search_queries, n_result){
  
  search_queries <- tibble(query = search_queries) %>%
    unnest_tokens(word, query) %>%
    dplyr::anti_join(., stopwords_IT2, by = c("word" = "stopwords_IT")) %>%
    dplyr::anti_join(tm_stopwords) %>%
    dplyr::anti_join(stopword_brand, by = c("word" = "stopword_brand")) %>%
    # dplyr::anti_join(lemma) %>%
    dplyr::anti_join(verbi)
  
  search_queries <- search_queries %>%
    dplyr::left_join(lemma) %>%
    dplyr::mutate(word = if_else(is.na(lemma), word, lemma),
                  lemma = NULL) %>%
    dplyr::distinct(word) %>%
    dplyr::summarise(text = paste(word, collapse = " "))
  
  news_list = books$text
  N.docs = length(news_list)
  names(news_list) = books$line
  
  #load search queries
  queries_list = unlist(search_queries)
  N.query = length(queries_list)
  names(queries_list) = paste0("query", c(1:N.query))
  
  #preprocess data news content
  #append both content and search queries together, convert the lists to VectorSource
  newscorpus = VectorSource(c(news_list,queries_list))
  newscorpus$Names = c(names(news_list),names(queries_list))
  #convert to corpus format
  newscorpus_preproc = Corpus(newscorpus)
  #cleaning the data
  # newscorpus_preproc = tm_map(newscorpus_preproc,stripWhitespace)
  # newscorpus_preproc = tm_map(newscorpus_preproc,removePunctuation)
  # newscorpus_preproc = tm_map(newscorpus_preproc,content_transformer(tolower))
  newscorpus_preproc = tm_map(newscorpus_preproc,removeWords,stopwords("italian"))
  
  #create tdm using weighted tfidf weightage
  tdm = TermDocumentMatrix(newscorpus_preproc,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  tdm_mat = as.matrix(tdm)
  colnames(tdm_mat) = c(names(news_list),names(queries_list))
  
  #normalizing the term document matrix
  tfidf_mat <- scale(tdm_mat, center = FALSE,scale = sqrt(colSums(tdm_mat^2)))
  
  #seperating query tdm matrix and content tdm matrix
  query.vectors <- tfidf_mat[, (N.docs + 1):(N.docs+N.query)]
  tfidf_mat <- tfidf_mat[, 1:N.docs]
  
  #calculating the similarity scores
  doc.scores <- t(query.vectors) %*% tfidf_mat
  # doc.scores[1, ]
  
  # y <- doc.scores %>%
  #   t() %>%
  #   tibble() %>% 
  #   mutate(line = colnames(doc.scores),
  #          score = .) %>%
  #   rename(score = ".") %>% tibble()
  

  results.df <- data.frame(querylist = queries_list,doc.scores)
  
  x <- results.df[which(results.df$querylist == search_queries),]
  yy =  data.frame(t(x),rownames(t(x)),row.names = NULL)[-1,]
  names(yy) = c("score","docs")
  yy$score = as.numeric(as.character(yy$score))
  yyy = yy[order(yy$score,decreasing = T),]
  
  yyy <- yyy %>%
    dplyr::filter(score > 0) %>%
    dplyr::arrange(desc(score)) %>%
    head(n_result)
  
  #function to display the final results
  # query <- search_queries
  # showTopresults <- function(query){
    # x = results.df[which(results.df$querylist == query),]
    # yy =  data.frame(t(x),rownames(t(x)),row.names = NULL)[-1,]
    # names(yy) = c("score","docs")
    # yy$score = as.numeric(as.character(yy$score))
    # yyy = yy[order(yy$score,decreasing = T),]
  #   
  #   return(yyy[which(yyy$score > 0),])
  # }
    return(yyy)
  
}
search_queries <- "giacche sartoriali materiale pregiati"
IR(books = books, search_queries = search_queries, n_result = 20) %>%
  dplyr::mutate(score = round(score * 100, 1),
                docs = forcats::fct_reorder(docs, score)) %>%
  ggplot2::ggplot(., aes(x = docs, y = score)) +
  ggplot2::geom_point() +
  ggplot2::coord_flip()
