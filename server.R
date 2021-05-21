library(shiny)

df_cleaned <- read_data(sample = F) %>% clean_data()
hotel_names <- sort(unique(df_cleaned$hotel_name))

shinyServer(function(input, output, session) {
  params <- reactiveValues()
  
  # Welcome page
  output$welcome <- renderUI({
    h1('hello')
  })
  
  # Render wordcloud after filter
  observeEvent(input$wordcloud_filter, {

    output$wordcloud <- renderUI({
      wc <- df_cleaned %>%
        tokenize_count() %>%
        tfidf() %>%
        plot_wordcloud(input)
      return(wc)

    })

  }) # close observeEvent 'tab'
  
  output$pickerInput <- renderUI({
    pickerInput(
      inputId = "wordcloud_filter",
      label = "Live search", 
      choices = hotel_names,
      options = list(
        `live-search` = TRUE,
        size = 5)
    )
  })
  
})