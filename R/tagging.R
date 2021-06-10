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

#---- loading input data

text_df <- readRDS("./data/text_df.rds")

tipo_abbigliamento_tag <- readRDS("./data/tipo_abbigliamento_tag.rds")

# text_df %>%
#   dplyr::left_join(., tipo_abbigliamento_tag) %>% 
#   dplyr::group_by(line, text, attribute) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-text) %>% 
#   dplyr::distinct() %>%                   # join POS
#   dplyr::count(attribute) %>%                                    # count
#   dplyr::filter(!is.na(attribute)) %>%
#   dplyr::mutate(prop=n/sum(n))
# 
# text_df %>%
#   dplyr::left_join(., tipo_abbigliamento_tag) %>% 
#   dplyr::group_by(line, text, attribute) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-text) %>% 
#   dplyr::distinct() %>%                   # join POS
#   dplyr::count(line, attribute) %>%                                    # count
#   dplyr::filter(!is.na(attribute)) %>%
#   dplyr::mutate(prop=n/sum(n)) %>%
#   ggplot2::ggplot(aes(attribute, prop)) + 
#   geom_boxplot() + 
#   coord_flip()
# 
# text_df %>%
#   dplyr::left_join(., tipo_abbigliamento_tag) %>% 
#   dplyr::group_by(line, text, attribute) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-text) %>% 
#   dplyr::distinct() %>%                   # join POS
#   dplyr::count(line, attribute) %>%                                    # count
#   dplyr::filter(!is.na(attribute)) %>%
#   dplyr::group_by(line) %>%
#   dplyr::summarise(total = sum(n))
  
text_df %>%
  dplyr::left_join(., tipo_abbigliamento_tag) %>% 
  dplyr::group_by(line, text, attribute) %>%
  dplyr::ungroup() %>%
  dplyr::select(-text) %>% 
  dplyr::distinct() %>%                   # join POS
  dplyr::count(line, attribute) %>%                                    # count
  dplyr::filter(!is.na(attribute)) %>%
  dplyr::left_join(text_df %>%
                     dplyr::left_join(., tipo_abbigliamento_tag) %>% 
                     dplyr::group_by(line, text, attribute) %>%
                     dplyr::ungroup() %>%
                     dplyr::select(-text) %>% 
                     dplyr::distinct() %>%                   # join POS
                     dplyr::count(line, attribute) %>%                                    # count
                     dplyr::filter(!is.na(attribute)) %>%
                     dplyr::group_by(line) %>%
                     dplyr::summarise(total = sum(n))) %>%
  dplyr::mutate(prop=n/total) %>%
  ggplot2::ggplot(aes(attribute, prop)) + 
  geom_boxplot() + 
  coord_flip()

text_df %>%
  dplyr::left_join(., tipo_abbigliamento_tag) %>% 
  dplyr::group_by(line, text, attribute) %>%
  dplyr::ungroup() %>%
  dplyr::select(-text) %>% 
  dplyr::distinct() %>%                   # join POS
  dplyr::count(word, attribute) %>%                                    # count
  dplyr::filter(!is.na(attribute)) %>%
  dplyr::left_join(text_df %>%
                     dplyr::left_join(., tipo_abbigliamento_tag) %>% 
                     dplyr::group_by(line, text, attribute) %>%
                     dplyr::ungroup() %>%
                     dplyr::select(-text) %>% 
                     dplyr::distinct() %>%                   # join POS
                     dplyr::count(line, attribute) %>%                                    # count
                     dplyr::filter(!is.na(attribute)) %>%
                     dplyr::group_by(line) %>%
                     dplyr::summarise(total = sum(n))) %>%
  dplyr::mutate(prop=n/total) %>%
  ggplot2::ggplot(aes(attribute, prop)) + 
  geom_boxplot() + 
  coord_flip()



text_df_tipo_abbigliamento_tag <- text_df %>%
  dplyr::left_join(., tipo_abbigliamento_tag) %>% 
  dplyr::group_by(line, text, attribute) %>%
  dplyr::summarise(word = paste0(word, collapse = ",")) %>% 
  tidyr::pivot_wider(names_from = "attribute", values_from = "word") %>%
  dplyr::mutate('NA' = NULL)

categorie_tag <- readRDS("./data/categorie_tag.rds")

text_df_categorie_tag <- text_df %>%
  dplyr::left_join(., categorie_tag) %>% 
  dplyr::group_by(line, text, categorie) %>%
  dplyr::summarise(word = paste0(word, collapse = ",")) %>% 
  tidyr::pivot_wider(names_from = "categorie", values_from = "word") %>%
  dplyr::mutate('NA' = NULL)

tessuti_tag <- readRDS("./data/tessuti_tag.rds")

text_df_tessuti_tag <- text_df %>%
  dplyr::left_join(., tessuti_tag) %>% 
  dplyr::group_by(line, text, attribute) %>%
  dplyr::summarise(word = paste0(word, collapse = ",")) %>% 
  tidyr::pivot_wider(names_from = "attribute", values_from = "word") %>%
  dplyr::mutate('NA' = NULL)

tipo_colori_tag <- readRDS("./data/tipo_colori_tag.rds")

text_df_tipo_colori_tag <- text_df %>%
  dplyr::left_join(., tipo_colori_tag) %>% 
  dplyr::group_by(line, text, attribute) %>%
  dplyr::summarise(word = paste0(word, collapse = ",")) %>% 
  tidyr::pivot_wider(names_from = "attribute", values_from = "word") %>%
  dplyr::mutate('NA' = NULL)

tipo_materiali_tag <- readRDS("./data/tipo_materiali_tag.rds")

text_df_tipo_materiali_tag <- text_df %>%
  dplyr::left_join(., tipo_materiali_tag) %>% 
  dplyr::group_by(line, text, attribute) %>%
  dplyr::summarise(word = paste0(word, collapse = ",")) %>% 
  tidyr::pivot_wider(names_from = "attribute", values_from = "word") %>%
  dplyr::mutate('NA' = NULL)

tipo_tessuti_tag <- readRDS("./data/tipo_tessuti_tag.rds")

text_df_tipo_tessuti_tag <- text_df %>%
  dplyr::left_join(., tipo_tessuti_tag) %>% 
  dplyr::group_by(line, text, attribute) %>%
  dplyr::summarise(word = paste0(word, collapse = ",")) %>% 
  tidyr::pivot_wider(names_from = "attribute", values_from = "word") %>%
  dplyr::mutate('NA' = NULL)

# text_df_categorie_tag
tidy_tag <- text_df_tipo_abbigliamento_tag %>% 
  dplyr::left_join(., text_df_tessuti_tag) %>% 
  dplyr::left_join(., text_df_tipo_colori_tag) %>% 
  dplyr::left_join(., text_df_tipo_materiali_tag) %>% 
  dplyr::left_join(., text_df_tipo_tessuti_tag) %>% 
  tidyr::pivot_longer(!c(line, text), names_to = "item", values_to = "value") %>% 
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = strsplit(as.character(value), ",")) %>% 
  tidyr::unnest(value)
saveRDS(tidy_tag, "./data/tidy_tag.rds")

text_df_categorie_tag <- text_df_categorie_tag %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_longer(!c(line, text), names_to = "item", values_to = "value") %>%
  dplyr::select(line, item, value) %>% 
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = strsplit(as.character(value), ",")) %>% 
  tidyr::unnest(value)
saveRDS(text_df_categorie_tag, "./data/text_df_categorie_tag.rds")
