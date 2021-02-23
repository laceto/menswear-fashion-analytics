tidy_books %>%
  count(word) %>%
  ggplot(aes(n)) +
  geom_histogram() +
  scale_x_log10()

tidy_books <- tidy_books %>%
  dplyr::filter(
    !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  ) %>%
  dplyr::count(word) %>%
  # dplyr::mutate(word = if_else(n <= 3, "infrequent", word)) %>% # categorize infrequent words
  dplyr::group_by(word) %>%
  dplyr::summarize(n = sum(n)) %>%
  dplyr::arrange(desc(n))


verbi <- readxl::read_xlsx("./data/verbi.xlsx") %>%
  dplyr::select(- url)
verbi <- verbi %>%
  dplyr::distinct()

tidy_books %>%
  dplyr::left_join(., verbi) %>% 
  dplyr::filter(
    !is.na(verbi)
  ) %>% 
  dplyr::filter(
    word != verbi
  )
