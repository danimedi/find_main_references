library(shiny)
library(shinycssloaders)
library(tibble)
library(shinythemes)

source("functions.R")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Find the most cited articles"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("query", "Search"),
      numericInput("limit", "Limit of articles to search their references", value = 300),
      actionButton("button_search", "Search!")
    ),
    mainPanel(
      tableOutput("table") %>% withSpinner(color = "#3498db")
    )
  )
)

server <- function(input, output, session) {
  
  # a list with the names and frequencies of the PMIDs of references
  result <- eventReactive(input$button_search,
    refs_to_freqs(pmid_to_refs(query_to_pmid(input$query, limit = input$limit)))
  )
  
  # obtain the columns for the names and link of the articles
  article <- reactive(pmid_to_article(result()$PMID))
  link <- reactive(str_c("https://pubmed.ncbi.nlm.nih.gov/", result()$PMID))
  
  # table with the results
  output$table <- renderTable(
    tibble(
      PMID = result()$PMID,
      n = result()$n,
      Article = article(),
      Link = link()
    )
  )
  
}

shinyApp(ui, server)
