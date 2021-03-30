library(shiny)
library(shinycssloaders)
library(shinyhelper)
library(tibble)
library(stringr)
library(purrr)

# source the R files in the directory `www`
walk(list.files("www", full.names = TRUE, pattern = "[.]R$"), source)

# read HTML files
readLines2 <- function(file) {
  paste0(readLines(file), collapse = "")
}

ui <- fluidPage(
  
  titlePanel("Find the most cited articles"),
  
  sidebarLayout(
    sidebarPanel(
      
      # instructions
      HTML(readLines2("www/introduction.html")),
      HTML("<br><br>"),
      
      # browser with a helper (question mark) with instructions
      helper(
        textInput("query", "Search"), 
        type = "inline",
        content = readLines2("www/help_search.html")
      ),
      
      actionButton("button_search", "Search!"),
      
      # download buttons (hidden until needed)
      tabsetPanel(
        id = "download_tabs",
        type = "hidden",
        tabPanel("panel_empty", ""),
        tabPanel("panel_download", 
          HTML("<br><br>"),
          HTML("<b>Download the most cited articles:</b><br>"),
          downloadButton("download_table", HTML("Table as TSV")),
          downloadButton("download_links", HTML("Text file with links"))
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
  
  # observe helpers (question marks)
  observe_helpers()
  
  # OBTAIN INFORMATION FROM THE API AND CREATE TABLES ------------------
  
  # PMIDs from the initial search
  pmid_art <- eventReactive(input$button_search, 
    query_to_pmid(input$query, limit = 300)
  )
  
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
  
  # DOWNLOAD BUTTONS -----------------------------------
  
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
      writeLines(join_link_title(link_title_ref()$Article, link_title_ref()$Link), file)
    }
  )
  output$download_table <- downloadHandler(
    filename = function() {
      paste0(input$query, ".tsv")
    },
    content = function(file) {
      write.table(table_ref(), sep = "\t", file = file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
