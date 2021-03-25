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
      tabsetPanel(
        tabPanel("Most cited articles", tableOutput("table_ref") %>% withSpinner()),
        tabPanel("Searched articles", tableOutput("table_art") %>% withSpinner())
      )
    )
  )
)

server <- function(input, output, session) {
  
  # PMIDs from the initial search
  pmid_art <- eventReactive(input$button_search, query_to_pmid(input$query, limit = input$limit))
  
  # obtain list containing titles and files from the searched articles
  link_title_art <- reactive(pmid_to_title_link(pmid_art()))
  # create table with this information
  output$table_art <- renderTable({
    req(input$button_search)
    as_tibble(link_title_art())
  })
  
  # table with the results of references
  result_ref <- reactive(refs_to_freqs(pmid_to_refs(pmid_art())))
  link_title_ref <- reactive(pmid_to_title_link(result_ref()$PMID))
  table_ref <- reactive(
    tibble(
      PMID = result_ref()$PMID,
      n = result_ref()$n,
      Article = link_title_ref()$Article,
      Link = link_title_ref()$Link
    )
  )
  output$table_ref <- renderTable({
    req(input$button_search)
    table_ref()
  })
  
  # show download buttons when needed
  observeEvent(table_ref(), {
    if (is_tibble(table_ref())) {
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
      writeLines(join_link_title(names_ref(), links_ref()), file)
    }
  )
}

shinyApp(ui, server)
