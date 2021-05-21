library(shiny)

shinyServer(function(input, output, session) {
  params <- reactiveValues()
  
  observeEvent(input$tab, {
    
    # Welcome page
    output$welcome <- renderUI({
      h1('hello')
    })
    
    # Filter page
    output$hotel_chooser <- renderUI({
      h1('hello filter')
      # multiInput(
      #   inputId = "hotel_chooser",
      #   label = "hotels", 
      #   choices = NULL,
      #   choiceNames = ,
      #   choiceValues = 
      # )
    })
    
    # Wordcloud page
    output$wordcloud <- renderUI({
      h1('hello cloud')
      read_data()
    })
    
  }) # close observeEvent 'tab'
  
})