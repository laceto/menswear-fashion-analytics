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
               DTOutput("database_frequencies"),
               verbatimTextOutput('row_selected')
               ),
        column(3,
               DTOutput("database_bigrams"),
               verbatimTextOutput('row_selected_bigram'),
               plotOutput("plot_frequencies")
               ),
        column(3,
               DTOutput("database_brand_bigram_selected")
               # verbatimTextOutput("database_brand_bigram_selected")
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
    tm_stopwords <- tibble(word = tm::stopwords("it"))
    lemma <- read.csv("./data/lemmatizzazione_IT.txt", header = T, sep = "\t", fill = T) %>%
        dplyr::mutate(lemma = trimws(lemma),
                      word = trimws(word)) %>%
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
        dplyr::anti_join(lemma)

    biagram <- text_df %>%
        dplyr::mutate(line = trimws(line)) %>%
        tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2)
    
    bigrams_separated <- biagram %>%
        tidyr::separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>%
        dplyr::filter(!word1 %in% stopword_brand$stopword_brand) %>%
        dplyr::filter(!word2 %in% stopword_brand$stopword_brand) %>%
        dplyr::filter(!word1 %in% stopwords_IT2$stopwords_IT) %>%
        dplyr::filter(!word2 %in% stopwords_IT2$stopwords_IT) %>%
        dplyr::filter(!word1 %in% tidytext::stop_words) %>%
        dplyr::filter(!word2 %in% tidytext::stop_words) %>%
        dplyr::filter(!word1 %in% tm_stopwords) %>%
        dplyr::filter(!word2 %in% tm_stopwords) %>%
        dplyr::filter(!word1 %in% lemma$word) %>%
        dplyr::filter(!word2 %in% lemma$word)

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
    
    output$database_frequencies <- renderDT(
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
    })

    output$plot_frequencies <- renderPlot({
        tidy_books %>%
            dplyr::count(word, sort = TRUE) %>%
            dplyr::top_n(input$top_freq) %>%
            dplyr::mutate(word = reorder(word, n)) %>%
            ggplot2::ggplot(aes(n, word)) +
            ggplot2::geom_col() +
            ggplot2::labs(y = NULL)
    })
    
    database_bigrams <- reactive({
        word1_selected <- frequencies() %>% dplyr::select(word) %>% dplyr::slice(word_selected()) %>% unlist
        
        bigrams_filtered %>%
            dplyr::filter(word1 %in% word1_selected) %>%
            dplyr::count(word1, word2, sort = T) %>%
            dplyr::mutate(n = NULL)
    })
    
    datatable_database_bigrams <- reactive({
        # word1_selected <- frequencies() %>% dplyr::select(word) %>% dplyr::slice(word_selected()) %>% unlist
        datatable(
            database_bigrams(),
            selection = "single",
            rownames = F,
            filter = "none", 
            options = list(pageLength = input$top_freq, dom = 'tip'),
            caption = 'Bigram frequencies table'
        )
    })
    
    observeEvent(input$database_frequencies_rows_selected,{
        
        output$database_bigrams <- renderDataTable(
            datatable_database_bigrams()
        )
    })
    
    bigram_selected <- reactive({
        input$database_bigrams_rows_selected
    })
    
    observeEvent(input$database_frequencies_rows_selected,{
        
        output$row_selected_bigram <- renderPrint({
            cat('\n\nSelected rows:\n\n')
            cat(database_bigrams() %>% dplyr::select(word2) %>% dplyr::slice(bigram_selected()) %>% unlist, sep = ', ')
            })
        output$database_brand_bigram_selected <- renderDataTable(datatable_brand_bigram_selected())
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
            caption = 'Unigram frequencies table'
            )
        })

}

# Run the application 
shinyApp(ui = ui, server = server)


