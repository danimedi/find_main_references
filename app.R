library(shiny)

ui <- fluidPage(
  textInput("query", "Search"),
  numericInput("limit", "Limit of articles", value = 2),
  textInput("api", "API key (optional)"),
  actionButton("button_search", "Search!"),
  textOutput("articles")
)

server <- function(input, output, session) {
  dois <- eventReactive(
    input$button_search, 
    dois_from_query(input$query, limit = input$limit, 
                    api_key = if (input$api == "") NULL else input$api)
  )
  refs <- reactive(refs_from_dois(dois()))

  output$articles <- renderText(refs(), sep = "")
}

shinyApp(ui, server)
