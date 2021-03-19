library(shiny)

source("functions.R")

ui <- fluidPage(
  textInput("query", "Search"),
  numericInput("limit", "Limit of articles", value = 50),
  numericInput("days_before", "Within the specified number of days", value = 365),
  actionButton("button_search", "Search!"),
  textOutput("articles")
)

server <- function(input, output, session) {
  
  result <- eventReactive(input$button_search, {
    input$query %>% 
      query_to_pmid(limit = input$limit, days_before = input$days_before) %>% 
      pmid_to_refs() %>% 
      refs_to_freqs()
  })

  output$articles <- renderText(result())
  
}

shinyApp(ui, server)
