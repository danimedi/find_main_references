library(RISmed)
library(httr)
library(magrittr)
library(xml2)
library(tibble)



query_to_pmid <- function(query, limit = 50, days_before = 365) {
  # use RISmed package to obtain the PMIDs of the search query
  search_query <- EUtilsSummary(query, retmax = limit, reldate = days_before)
  medline_object <- EUtilsGet(search_query)
  ArticleId(medline_object)
}



pmid_to_refs <- function(pmid) {
  # obtain the PMIDs of the references from a list of PMIDs
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  url_id <- paste0("&id=", paste0(pmid, collapse = "&id="))
  url <- paste0(
    base,
    "elink.fcgi?dbfrom='pubmed'&linkname=pubmed_pubmed_refs",
    url_id
  )
  output <- GET(url)
  res <- content(output) %>% 
    xml_contents() %>% 
    xml_find_all("//LinkSetDb/Link") %>% 
    xml_text()
  res
}



refs_to_freqs <- function(pmid) {
  # detect repeated PMIDs and obtain their frequency
  freq <- sort(table(pmid), decreasing = TRUE)
  freq <- freq[freq > 1]
  table_results <- tibble(PMID = names(freq), n = unname(freq))
  
  if (length(freq) > 0) {
    table_results
  } else {
    "There is not enough information to suggest any specific articles"
  }
}



pmid_to_article <- function(pmid) {
  # obtain the titles for the PMIDs
  query <- paste0(pmid, collapse = "[PMID] OR ")
  query <- paste0(query, "[PMID]")
  ArticleTitle(EUtilsGet(query))
}

