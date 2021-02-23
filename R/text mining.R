library(wordcloud)
library(irlba)
library(tm)
library(stopwords)
library(plyr)
# library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(dplyr)
# library(tidyr)
library(ggplot2)
library(RCurl)
library(XML)
library(plyr)
library(reshape2)
options(error = recover)
library(foreach)
library(doParallel)
library(tibble)
library(textmineR)
library(openxlsx)

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

# openxlsx::write.xlsx("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/fashion glossary/tipo_tessuti.xlsx")
tipo_tessuti <- readxl::read_xlsx("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/fashion glossary/tipo_tessuti.xlsx")
tipo_materiali <- readxl::read_xlsx("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/fashion glossary/tipo_materiali.xlsx")
tipo_colori <- readxl::read_xlsx("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/fashion glossary/tipo_colori.xlsx")

aggettivi <- readxl::read_xlsx("C:/Users/laceto/Desktop/lavoro/web scraping italiano/aggettivi.xlsx")
verbi <- readxl::read_xlsx("C:/Users/laceto/Desktop/lavoro/web scraping italiano/verbi.xlsx")

colori <- read.table("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/fashion glossary/colori.txt", sep = "\t", header = T) %>%
  tidyr::separate(., "value", into = c("value", "a"), sep = " ") %>%
  dplyr::select(- a) %>%
  tidyr::separate(., "value", into = c("value", "a"), sep = "-") %>%
  dplyr::select(- a) %>%
  dplyr::mutate(value = trimws(value),
                value = tolower(value),
                value = tm::removePunctuation(value))
colori$value <- rm_accent(colori$value)

countries <- read.table("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/countries_df.txt", sep = "\t", header = T, fill = T) %>%
  tidyr::separate(., "language", into = c("nationality", "b"), sep = ",") %>%
  dplyr::select(- b) %>%
  dplyr::mutate(country = as.character(country), 
                country = tm::removePunctuation(country)) %>%
  dplyr::as_tibble() %>%
  tidyr::gather(attribute, value, country:nationality)

yoox_category <- readxl::read_excel("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/fashion glossary/yoox category.xlsx")

yoox_category <- yoox_category %>%
  dplyr::rename(attribute = categorie,
                value = item,
                gender = sesso,
                language = lingua) %>% 
  dplyr::filter(language == "italiano") 

gioielli <- readxl::read_excel("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/fashion glossary/gioielli.xlsx")

tessuti <- readxl::read_excel("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/fashion glossary/tessuti.xlsx")

tessuti <- tessuti %>%
  dplyr::filter(language == "italiano") 

tipo_abbigliamento <- readxl::read_excel("C:/Users/laceto/Desktop/lavoro/web scraping shopping map/fashion glossary/tipo abbigliamento.xlsx")
tipo_abbigliamento <- tipo_abbigliamento %>%
  dplyr::group_by(value) %>%
  dplyr::filter(row_number() == 1)


marchi_info <- readxl::read_xlsx("brand_info.xlsx")

text_df <- tibble(line = marchi_info$name, text = marchi_info$description) %>%
  dplyr::mutate(text = str_replace_all(text, "\\.", " "), 
                text = str_replace_all(text, "\r*\n", " "),
                line = trimws(tolower(line)),
                text = trimws(tolower(text))) %>%
  dplyr::mutate(text = trimws(text)) %>%
  dplyr::filter(!is.na(text), text != "") 


remove_all_word = function(x){
  return(str_remove_all(text, x))
}
i <- 4
for (i in 1:nrow(text_df)) {
  
  skip_to_next <- FALSE
  
  text <- as.character(text_df$text[i])
  brand <- as.character(text_df$line[i])

  brand <- unlist(str_split(brand, " "))

  for(j in 1:length(brand)){
    text <- tryCatch(remove_all_word(brand[j]), error = function(e){text})
  }
  
  if(skip_to_next) { next }  
  text_df$text[i] <- text
}

text_df <- text_df %>%
  dplyr::mutate(text = tm::removeNumbers(text),
                text = tm::removePunctuation(text))

text_df$line <- rm_accent(text_df$line)
text_df$text <- rm_accent(text_df$text)

text_df$line <- tm::removePunctuation(text_df$line)

for (i in 1:nrow(text_df)) {
  
  skip_to_next <- FALSE
  
  text <- as.character(text_df$text[i])
  brand <- as.character(text_df$line[i])

  brand <- unlist(str_split(brand, " "))

  j <- 2
  for(j in 1:length(brand)){
    text <- tryCatch(remove_all_word(brand[j]), error = function(e){text})
  }
  
  if(skip_to_next) { next }  
  text_df$text[i] <- text
}

text_df <- text_df %>%
  dplyr::mutate(text = str_replace_all(text, "total look", "totallook"),
                text = str_replace_all(text, "ready to wear", "readytowear"),
                text = str_replace_all(text, "street wear", "streetwear"),
                text = str_replace_all(text, "haute couture", "hautecouture"),
                text = str_replace_all(text, "contemporary", "contemporaneo"),
                text = str_replace_all(text, "emergenti", "emergente"),
                text = str_replace_all(text, "bon ton", "bonton"),
                text = str_replace_all(text, "britannico", "british"),
                text = str_replace_all(text, "urbano", "urban"),
                text = str_replace_all(text, "è un", ""),
                text = str_replace_all(text, "è la", ""),
                text = str_replace_all(text, "è il", "")) 

text_df %>%
  dplyr::mutate(text = trimws(text)) %>%
  dplyr::filter(!is.na(text), text != "") %>% view()

stopwords_IT <- tibble(word = stopwords(language = "it", source = "snowball"))
stopwords_IT2 <- read.table("C:/Users/laceto/Desktop/lavoro/Text Mining/stopwords IT.txt", header = T, sep = "\t")

lemma <- read.table("C:/Users/laceto/Desktop/lavoro/Text Mining/lemmatizatione_IT.txt", header = T, sep = "\t", fill = T)

tm_stopwords <- tibble(word = tm::stopwords("it"))

tidy_books <- text_df %>%
  tidytext::unnest_tokens(word, text) %>%
  dplyr::anti_join(stopwords_IT) %>% 
  dplyr::anti_join(., stopwords_IT2, by = c("word" = "stopwords_IT")) %>%
  dplyr::anti_join(tidytext::stop_words) %>%
  dplyr::anti_join(tm_stopwords)

tidy_books <- tidy_books %>%
  dplyr::mutate(word = str_replace(word, "marchio", "brand"))

frequency <- tidy_books %>%
  dplyr::count(word, sort = TRUE) 

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

countries <- countries %>%
  dplyr::distinct() %>%
  dplyr::mutate(value = trimws(value))

tidy_books %>%
  dplyr::left_join(., countries, by = c("word" = "value")) %>%
  dplyr::filter(!is.na(attribute),
                attribute == "country") %>%
  dplyr::count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

tidy_books %>%
  dplyr::left_join(., countries, by = c("word" = "value")) %>%
  dplyr::filter(!is.na(attribute),
                attribute == "nationality") %>% 
  dplyr::count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

tidy_books %>%
  dplyr::left_join(., countries, by = c("word" = "value")) %>%
  dplyr::filter(!is.na(attribute),
                attribute == "nationality") %>% 
  dplyr::count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  with(wordcloud(word, n, max.words = 100))

yoox_category <- yoox_category %>%
  dplyr::select(- gender, - language) %>% 
  dplyr::distinct(attribute, value)

tessuti <- tessuti %>%
  dplyr::select(- language)

tipo_abbigliamento <- tipo_abbigliamento %>%
  tidyr::separate(., value, c("a", "value"), sep = " ", remove = F) %>%
  dplyr::select(- a) %>%
  dplyr::group_by(value) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::select(- count) %>%
  dplyr::mutate(attribute = "tipo_abbigliamento")

colori <- colori %>%
  dplyr::mutate(value = as.character(value)) %>%
  dplyr::distinct()

glossary <- bind_rows(countries, yoox_category, tessuti, tipo_abbigliamento, colori, gioielli,
                      tipo_tessuti, tipo_materiali, tipo_colori)

glossary <- glossary %>%
  dplyr::distinct()

glossary <- glossary %>%
  dplyr::group_by(value) %>%
  dplyr::summarise(attribute = first(attribute))

# ci sono doppioni dopo left join
frequency %>%
  dplyr::left_join(., glossary, by = c("word" = "value")) %>% 
  dplyr::filter(is.na(attribute))

verbi <- verbi %>% 
  dplyr::mutate(url = NULL,
                check = NULL,
                attribute = "verbo") %>%
  dplyr::rename(infinito = verbi) 

verbi <- verbi %>%
  dplyr::mutate(infinito = NULL,
                word = tm::removePunctuation(word)) %>%
  dplyr::distinct()

verbi$word <- rm_accent(verbi$word)

verbi <- verbi %>%
  dplyr::distinct()

nouns <- tibble(word = c("capi", 
                         "collezioni",
                         "colori",
                         "dettagli",
                         "modelli",
                         "lusso",
                         "gusto",
                         "tagli",
                         "tessuto",
                         "modello",
                         "idea",
                         "ricami",
                         "disegni",
                         "taglio",
                         "motivi",
                         "marchi",
                         "negozi"
                         ))

frequency %>%
  dplyr::left_join(., glossary, by = c("word" = "value")) %>%
  dplyr::filter(is.na(attribute)) %>%
  view()

frequency %>%
  dplyr::left_join(., glossary, by = c("word" = "value")) %>%
  dplyr::filter(is.na(attribute)) %>%
  dplyr::left_join(verbi, by = c("word" = "word")) %>% 
  dplyr::filter(!is.na(attribute.y)) %>%
  view()

biagram <- text_df %>%
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- biagram %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  dplyr::filter(!word1 %in% stopwords_IT$word) %>%
  dplyr::filter(!word2 %in% stopwords_IT$word)%>%
  dplyr::filter(!word1 %in% stopwords_IT2$stopwords_IT) %>%
  dplyr::filter(!word2 %in% stopwords_IT2$stopwords_IT) %>%
  dplyr::filter(!word1 %in% tidytext::stop_words) %>%
  dplyr::filter(!word2 %in% tidytext::stop_words) %>%
  dplyr::filter(!word1 %in% tm_stopwords) %>%
  dplyr::filter(!word2 %in% tm_stopwords)

bigrams_filtered %>%
  dplyr::filter(word1 == "qualita") %>%
  dplyr::left_join(., glossary, by = c("word2" = "value")) %>%
  dplyr::filter(is.na(attribute)) %>%
  dplyr::count(word2, sort = T)%>% view()
  dplyr::count(word1, word2, attribute, sort = T) %>% view()

bigrams_filtered %>%
  dplyr::filter(word1 == "stile") %>%
  dplyr::left_join(., glossary, by = c("word2" = "value")) %>%
  # dplyr::filter(is.na(attribute)) %>%
  dplyr::count(word1, word2, attribute, sort = T) %>% 
  dplyr::filter(is.na(attribute)) %>% view()

# bigrams_filtered %>%
#   dplyr::filter(word1 == "colori")  %>%
#   dplyr::count(word1, word2, sort = T) %>%
#   openxlsx::write.xlsx("C:/Users/laceto/Desktop/fashion glossary/tipo_colori.xlsx")
  

trigrams_filtered <- text_df %>%
  tidytext::unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  dplyr::filter(!word1 %in% stopwords_IT$word,
                !word2 %in% stopwords_IT$word,
                !word3 %in% stopwords_IT$word,
                !word1 %in% stopwords_IT2$stopwords_IT,
                !word2 %in% stopwords_IT2$stopwords_IT,
                !word3 %in% stopwords_IT2$stopwords_IT)

trigrams_filtered %>%
  dplyr::count(word1, word2, word3, sort = TRUE) %>%
  dplyr::filter(word2 == "brand") %>% view()


