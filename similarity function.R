similarity_all <- function(brand, op_ed_similarity, cut_off){
  
  # brand <- "berwich"
  
  op_ed_similarity <- word_tf_idf %>%
    pairwise_similarity(line, word, tf_idf, upper = FALSE, sort = TRUE) %>%
    # filter(item1 ==  "obvious")) %>%
    dplyr::filter(str_detect(item1, brand))
  
  op_ed_similarity %>%
    dplyr::filter(similarity > cut_off)
  
  # op_ed_similarity %>%
  #   head(12) %>%
  #   mutate(item2 = reorder(item2, similarity)) %>%
  #   ggplot(aes(item2, similarity)) +
  #   geom_col() +
  #   scale_x_reordered() +
  #   coord_flip() +
  #   facet_wrap(~ item1, scales = "free_y") +
  #   labs(x = "",
  #        y = "Cosine similarity between TF-IDF vectors",
  #        subtitle = "Based on 69 selected staff accounts",
  #        title = "Twitter accounts using words similar to NYTimes op-ed")
}

similarity_all("incotex", op_ed_similarity, 0.1)



