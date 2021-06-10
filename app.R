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
library(tidyverse)

header <- dashboardHeader(
    
    title = "Fashion analytics",
    titleWidth = 350
)

# sidebar function ----

sidebar <- dashboardSidebar(disable = F,
                            width = 350,
                            sidebarMenu(
                                menuItem("database", tabName = "database", icon = icon("running")),
                                menuItem("frequencies", tabName = "frequencies", icon = icon("running"))
                            )
)

tab_database_original <- fluidPage(
    
    
    DTOutput("database_original")
)

tab_freq <- fluidPage(
    
    fluidRow(numericInput(inputId = "top_freq", label = "select top frequencies", value = "5")),
    
    fluidRow(    
        column(3,
               DTOutput("database_frequencies")
               ,
               verbatimTextOutput('row_selected')
               ),
        column(3,
               DTOutput("database_bigrams"),
               verbatimTextOutput('row_selected_bigram')
               ),
        column(3,
               DTOutput("database_brand_bigram_selected"),
               verbatimTextOutput("brand_selected")
               ),
        column(3, 
               DTOutput("brand_selected_show"),
               style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
               )
        )
    )

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "database",
                tab_database_original),
        tabItem(tabName = "frequencies",
                tab_freq)
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
    
    #---- loading input data
    
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
    
    #---- data manipulation
    
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
        dplyr::anti_join(verbi)

    biagram <- text_df %>%
        dplyr::mutate(line = trimws(line)) %>%
        tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2)
    
    bigrams_separated <- biagram %>%
        tidyr::separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>%
        # dplyr::filter(!word1 %in% stopword_brand$stopword_brand) %>%
        # dplyr::filter(!word2 %in% stopword_brand$stopword_brand) %>%
        dplyr::filter(!word1 %in% stopwords_IT2$stopwords_IT) %>%
        dplyr::filter(!word2 %in% stopwords_IT2$stopwords_IT) %>%
        # dplyr::filter(!word1 %in% tidytext::stop_words) %>%
        # dplyr::filter(!word2 %in% tidytext::stop_words) %>%
        dplyr::filter(!word1 %in% tm_stopwords) %>%
        dplyr::filter(!word2 %in% tm_stopwords) %>%
        dplyr::filter(!word1 %in% lemma$word) %>%
        dplyr::filter(!word2 %in% lemma$word) %>%
        dplyr::filter(!word1 %in% verbi$word) %>%
        dplyr::filter(!word2 %in% verbi$word)

    #---- tab_database_original
    
    output$database_original <- renderDT(
        text_df
    )
    
    #---- tab_freq
    
    frequencies <- reactive({
        tidy_books %>%
            dplyr::count(word, sort = TRUE) %>%
            dplyr::top_n(., input$top_freq)
    })

    database_frequencies <- reactive({
        datatable(
            frequencies(),
            selection = "single",
            rownames = F,
            filter = "none", 
            options = list(pageLength = input$top_freq, dom = 'tip'),
            caption = 'Unigram frequencies table'
        )
    })
    
    output$database_frequencies <- renderDataTable(
        database_frequencies()
    )
    
    word_selected <- reactive({
        input$database_frequencies_rows_selected
    })
    
    observeEvent(input$database_frequencies_rows_selected,{
        output$row_selected <- renderPrint({
            cat('\n\nSelected rows:\n\n')
            if(length(input$database_frequencies_rows_selected) == 0) {
                cat(' ')
            } else {
            cat(frequencies() %>% dplyr::select(word) %>% dplyr::slice(word_selected()) %>% unlist, sep = ', ')
            }
        })
        output$database_bigrams <- renderDataTable(
            if(length(input$database_frequencies_rows_selected) == 0) {
                
            } else {
                datatable_database_bigrams()
            }
        )
    })

    database_bigrams <- reactive({
        word1_selected <- frequencies() %>% dplyr::select(word) %>% dplyr::slice(word_selected()) %>% unlist

        bigrams_filtered %>%
            dplyr::filter(word1 %in% word1_selected) %>%
            dplyr::count(word1, word2, sort = T) %>%
            dplyr::mutate(n = NULL)
    })

    datatable_database_bigrams <- reactive({

        datatable(
            database_bigrams(),
            selection = "single",
            rownames = F,
            filter = "none",
            options = list(pageLength = input$top_freq, dom = 'tip'),
            caption = 'Bigram frequencies table'
        )
    })

    bigram_selected <- reactive({
        input$database_bigrams_rows_selected
    })

    observeEvent(input$database_frequencies_rows_selected,{

        output$row_selected_bigram <- renderPrint({
            cat('\n\nSelected rows:\n\n')
            if(length(input$database_bigrams_rows_selected) == 0) {
                cat(' ')
            } else {
                cat(database_bigrams() %>% dplyr::select(word2) %>% dplyr::slice(bigram_selected()) %>% unlist, sep = ', ')
                }
            })
        output$database_brand_bigram_selected <- renderDataTable(
            if(length(input$database_bigrams_rows_selected) == 0) {
                
            } else {
                datatable_brand_bigram_selected()
            }
        )
        
        })
    
    brand_selected <- reactive({
        input$database_brand_bigram_selected_rows_selected
    })
    
    observeEvent(input$database_frequencies_rows_selected, {
        
        output$brand_selected <- renderPrint({
            cat('\n\nSelected rows:\n\n')
            if(length(input$database_brand_bigram_selected_rows_selected) == 0) {
                cat(' ')
            } else {
                cat(brand_bigram_selected() %>% dplyr::slice(input$database_brand_bigram_selected_rows_selected) %>% dplyr::pull(line) , sep = ', ')
            }
        })
        output$brand_selected_show <- renderDataTable(
            if(length(input$database_brand_bigram_selected_rows_selected) == 0) {

            } else {
                brand_to_filter <- brand_bigram_selected() %>% dplyr::slice(input$database_brand_bigram_selected_rows_selected) %>% dplyr::pull(line)

                datatable(
                    text_df %>%
                        dplyr::filter(line == brand_to_filter) %>%
                        dplyr::select(text),
                    selection = "single",
                    rownames = F,
                    filter = "none",
                    height = "80px",
                    options = list(pageLength = input$top_freq, dom = 'tip'),
                    caption = 'Brand description'
                )
            }
        )
    })

     brand_bigram_selected <- reactive({
        word1_sel <- frequencies() %>% dplyr::select(word) %>% dplyr::slice(word_selected()) %>% unlist
        word2_sel <- database_bigrams() %>% dplyr::select(word2) %>% dplyr::slice(bigram_selected()) %>% unlist

        bigrams_filtered %>%
            dplyr::filter(word1 == word1_sel, word2 == word2_sel) %>%
            dplyr::count(line) %>%
            dplyr::select(line)
    })

    datatable_brand_bigram_selected <- reactive({
        datatable(
            brand_bigram_selected(),
            selection = "single",
            rownames = F,
            filter = "none",
            options = list(pageLength = input$top_freq, dom = 'tip'),
            caption = 'Filtered brands'
            )
        })

}

# Run the application 
shinyApp(ui = ui, server = server)


