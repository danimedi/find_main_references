library(shiny)
library(shinycssloaders)
library(tibble)
library(stringr)

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
          downloadButton("download_links", "Download text file")
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
  
  # create the files to be downloaded
  output$download_links <- downloadHandler(
    filename = function() {
      paste0(input$query, ".txt")
    },
    content = function(file) {
      join_link_title <- function(title, link) {
        if (length(title) == length(link)) {
          paste0(
            vapply(
              seq_along(title),
              function(i) paste0(title[[i]], "\n", link[[i]]),
              character(1)
            ),
            collapse = "\n\n"
          )
        } else {
          validate("The number of articles is not the same as the links")
        }
      }
      writeLines(join_link_title(article(), link()), file)
    }
  )
}

shinyApp(ui, server)
