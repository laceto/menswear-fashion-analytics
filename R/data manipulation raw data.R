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

# tagging key fashion words

#---- loading input data

marchi_info <- readxl::read_xlsx("./data/brand_info.xlsx")
stopwords_IT2 <- read.table("./data/stopwords IT.txt", header = T, sep = "\t")
tm_stopwords <- tibble(word = tm::stopwords("it"))

#---- functions

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

remove_accent <- function(str){
  chartr(paste(symbols, collapse=" "), paste(nudeSymbols, collapse=" "), str)
}

#---- data manipulation

text_df <- tibble(line = marchi_info$name, text = marchi_info$description)


# text_df <- text_df %>%
#   dplyr::filter(stringr::str_detect(line, "giveness"))
text_df <- text_df %>%
  dplyr::mutate(line = str_replace_all(line, "\\.", " "), 
                text = str_replace_all(text, "\\.", " "), 
                line = str_replace_all(line, "\r*\n", " "),
                text = str_replace_all(text, "\r*\n", " "),
                text = str_remove_all(text, "\\b[A-Z][a-zA-Z]*\\b"),
                line = trimws(tolower(line)),
                text = trimws(tolower(text))) %>%
  dplyr::filter(!is.na(text), text != "") 

text_df <- text_df %>%
  dplyr::mutate(line = remove_accent(line),
                text = remove_accent(text),
                # line = tm::removePunctuation(line),
                # text = tm::removePunctuation(text),
                text = str_replace_all(text, "[^0-9A-Za-z]", " "),
                line = str_replace_all(line, "[^0-9A-Za-z]", " "))

tidy_books <- text_df %>%
  unnest_tokens(word, text)

stopword_brand <- text_df$line %>% unlist %>% trimws() %>% str_split(., "\\s+")
stopword_brand <- stopword_brand %>% unlist()
stopword_brand <- tibble(stopword_brand = stopword_brand)

tidy_books <- tidy_books %>%
  dplyr::anti_join(., stopwords_IT2, by = c("word" = "stopwords_IT")) %>%
  dplyr::anti_join(tm_stopwords) %>%
  dplyr::anti_join(stopword_brand, by = c("word" = "stopword_brand"))

# text_df <- text_df %>%
#   dplyr::mutate(text = str_replace_all(text, "[^A-Za-z]", " "),
#                 line = str_replace_all(line, "[^A-Za-z]", " ")
#   )

text_df %>%
  saveRDS(., "./data/text_df.rds")

tipo_abbigliamento <- readxl::read_xlsx("./data/tipo abbigliamento.xlsx")
tipo_abbigliamento <- tipo_abbigliamento %>% 
  dplyr::group_by(value)

tidy_books <- tidy_books %>%
  dplyr::count(line, word) %>%
  dplyr::mutate(n = NULL)

vlookup <- function(x){
  dplyr::left_join(tidy_books, x, by = c("word" = "value"))
}

tidy_books_abbigliamento <- tipo_abbigliamento %>% 
  plyr::ddply(., "attribute", vlookup) %>%
  dplyr::arrange(line, attribute) %>%
  dplyr::filter(!is.na(attribute))

tidy_books_abbigliamento %>%
  saveRDS(., "./data/tipo_abbigliamento_tag.rds")

yoox_category <- readxl::read_xlsx("./data/yoox category.xlsx") %>% 
  dplyr::group_by(item)

yoox_category <- yoox_category %>% 
  dplyr::rename(value = item) %>%
  dplyr::count(categorie, value) %>%
  dplyr::select(-n)

tidy_books_categorie <- yoox_category %>%
  plyr::ddply(., "categorie", vlookup) %>%
  dplyr::arrange(word) %>%
  dplyr::filter(!is.na(categorie))

tidy_books_categorie %>%
  saveRDS(., "./data/categorie_tag.rds")

tipo_tessuti <- readxl::read_xlsx("./data/tipo_tessuti.xlsx")
tipo_tessuti <- tipo_tessuti %>% 
  dplyr::count(attribute, value) %>%
  dplyr::select(-n)

tidy_books_tipo_tessuti <- tipo_tessuti %>%
  plyr::ddply(., "attribute", vlookup) %>%
  dplyr::arrange(word) %>%
  dplyr::filter(!is.na(attribute))

tidy_books_tipo_tessuti %>%
  saveRDS(., "./data/tipo_tessuti_tag.rds")

tipo_materiali <- readxl::read_xlsx("./data/tipo_materiali.xlsx")
tipo_materiali <- tipo_materiali %>% 
  dplyr::count(attribute, value) %>%
  dplyr::select(-n)

tidy_books_tipo_materiali <- tipo_materiali %>%
  plyr::ddply(., "attribute", vlookup) %>%
  dplyr::arrange(word) %>%
  dplyr::filter(!is.na(attribute))

tidy_books_tipo_materiali %>%
  saveRDS(., "./data/tipo_materiali_tag.rds")

tipo_colori <- readxl::read_xlsx("./data/tipo_colori.xlsx")
tipo_colori <- tipo_colori %>% 
  dplyr::count(attribute, value) %>%
  dplyr::select(-n)

tidy_books_tipo_colori <- tipo_colori %>%
  plyr::ddply(., "attribute", vlookup) %>%
  dplyr::arrange(word) %>%
  dplyr::filter(!is.na(attribute))

tidy_books_tipo_colori %>%
  saveRDS(., "./data/tipo_colori_tag.rds")

tessuti <- readxl::read_xlsx("./data/tessuti.xlsx")
tessuti <- tessuti %>% 
  dplyr::count(attribute, value) %>%
  dplyr::select(-n)

tidy_books_tessuti <- tessuti %>%
  plyr::ddply(., "attribute", vlookup) %>%
  dplyr::arrange(word) %>%
  dplyr::filter(!is.na(attribute))

tidy_books_tessuti %>%
  saveRDS(., "./data/tessuti_tag.rds")
