library(shiny)
library(shinycssloaders)
library(tibble)

source("functions.R")

ui <- fluidPage(
  titlePanel("Find the most cited articles"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("query", "Search"),
      numericInput("limit", "Limit of articles to search their references", value = 300),
      actionButton("button_search", "Search!"),
      tabsetPanel(
        id = "download_tabs",
        type = "hidden",
        tabPanel("panel_empty", ""),
        tabPanel("panel_download", 
          HTML("<br><br>"),
          downloadButton("download_bib", "References as a BIB file"),
          downloadButton("download_links", "Links as a text file")
        )
      )
    ),
    mainPanel(
      tableOutput("table") %>% withSpinner()
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
  table_res <- reactive(
    tibble(
      PMID = result()$PMID,
      n = result()$n,
      Article = article(),
      Link = link()
    )
  )
  output$table <- renderTable(table_res())
  
  # show download buttons when needed
  observeEvent(table_res(), {
    if (is_tibble(table_res())) {
      updateTabsetPanel(inputId = "download_tabs", selected = "panel_download")
    } else {
      updateTabsetPanel(inputId = "download_tabs", selected = "panel_empty")
    }
  })
  
}

shinyApp(ui, server)
