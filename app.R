library(shiny)

source("functions.R")

ui <- fluidPage(
  textInput("query", "Search"),
  numericInput("limit", "Limit of articles", value = 50),
  numericInput("days_before", "Within the specified number of days", value = 365),
  actionButton("button_search", "Search!"),
  tableOutput("table")
)

server <- function(input, output, session) {
  
  result <- eventReactive(input$button_search, {
    input$query %>% 
      query_to_pmid(limit = input$limit, days_before = input$days_before) %>% 
      pmid_to_refs() %>%
      refs_to_freqs()
  })
  
  output$table <- renderTable(tibble(result(), Article = pmid_to_article(result()$PMID)))
  
}

shinyApp(ui, server)
