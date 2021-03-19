library(RISmed)
library(httr)
library(glue)
library(purrr)
library(magrittr)
library(xml2)



query_to_pmid <- function(query, limit = 50, days_before = 365) {
  # use RISmed package to obtain the PMIDs of the search query
  search_query <- EUtilsSummary(query, retmax = limit, reldate = days_before)
  medline_object <- EUtilsGet(search_query)
  ArticleId(medline_object)
}



pmid_to_refs <- function(pmid) {
  # obtain the PMIDs of the references from a list of PMIDs
  map(pmid, function(id) {
    Sys.sleep(1/3)
    base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
    db <- "pubmed"
    url <- paste0(
      base,
      glue("elink.fcgi?dbfrom={db}&linkname=pubmed_pubmed_refs&id={id}")
    )
    output <- GET(url)
    links <- content(output) %>% 
      xml_contents() %>% 
      xml_find_all("//LinkSetDb/Link") %>% 
      xml_text()
    links
  })
}



refs_to_freqs <- function(refs) {
  # collapse all the PMIDs of the references and detect the repeated PMIDs, return those repeated codes,
  # they contain the "important" articles of the search
  freqs <- refs %>% 
    unlist() %>% 
    table() %>% 
    sort(decreasing = TRUE) %>% 
    .[. > 1]
  if (length(freqs) > 0) freqs else "There is not enough information to suggest any specific articles"
}

