library(shiny)
library(shinycssloaders)
library(tibble)

source("functions.R")

ui <- fluidPage(
  textInput("query", "Search"),
  actionButton("button_search", "Search!"),
  tableOutput("table") %>% withSpinner()
)

server <- function(input, output, session) {
  
  # a list with the names and frequencies of the PMIDs of references
  result <- eventReactive(input$button_search,
    refs_to_freqs(pmid_to_refs(query_to_pmid(input$query, limit = 330)))
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
