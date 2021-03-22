library(RISmed)
library(httr)
library(magrittr)
library(xml2)
library(tibble)
library(stringr)


# analysis of perfomance showed:
# expensive "base" functions:
# EUtilsGet x2
# expensive created functions:
# 1. pmid_to_article
# 2. query_to_pmid
# 3. pmid_to_refs


xml_extract <- function(response, xpath) {
  # extract elements of a response of the API using xpaths
  xml_text(xml_find_all(xml_contents(content(response)), xpath))
}



query_to_pmid <- function(query, limit = 100, days_before = 365) {
  # use RISmed package to obtain the PMIDs of the search query
  search_query <- EUtilsSummary(query, retmax = limit, reldate = days_before)
  medline_object <- EUtilsGet(search_query)
  ArticleId(medline_object)
}



pmid_to_refs <- function(pmid) {
  # obtain the PMIDs of the references from a list of PMIDs
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  url_id <- str_c("&id=", str_c(pmid, collapse = "&id="))
  url <- str_c(
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
    validate("There is not enough information to suggest any specific articles")
  }
}



pmid_to_article <- function(pmid) {
  # obtain the titles for the PMIDs
  ids <- str_c(pmid, collapse = ",")
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  url <- str_c(base, "esummary.fcgi?db=pubmed&id=", ids)
  output <- GET(url)
  xml_extract(output, "//DocSum/Item[@Name='Title']")
}
