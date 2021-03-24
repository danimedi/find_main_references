library(httr)
library(xml2)
library(stringr)


xml_extract_text <- function(response, xpath) {
  # extract elements of a response of the API using xpaths
  xml_text(xml_find_all(xml_contents(content(response)), xpath))
}

xml_extract_attrs <- function(response, xpath) {
  unlist(
    xml_attrs(xml_find_all(xml_contents(content(response)), xpath)), 
    use.names = FALSE
  )
}

create_query <- function(query) {
  query <- URLencode(enc2utf8(query), reserved = TRUE)
  query <- gsub("%20", "+", query)
  query
}



# the URL does not support more than 335 PMIDs of the article
query_to_pmid <- function(query, limit = 1000) {
  # first translate the query (ESearch)
  query <- create_query(query)
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  url <- str_c(base, "esearch.fcgi?db=pubmed&term=", query, "&usehistory=y")
  output <- GET(url)
  web <- xml_extract_text(output, "//WebEnv")
  key <- xml_extract_text(output, "//QueryKey")
  # then obtain the PMIDs
  url <- str_c(base, "esummary.fcgi?db=pubmed&query_key=", key, "&WebEnv=", web, "&version=2.0",
               "&retmax=", limit)
  output <- GET(url)
  xml_extract_attrs(output, "//DocumentSummarySet/DocumentSummary")
}



pmid_to_refs <- function(pmid) {
  # obtain the PMIDs of the references from a list of PMIDs
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  
  # here we're using `"&id="` instead of commas, this is because that way you obtain
  # the references from artiles independently, otherwise the repeated articles would
  # collapse
  url_id <- str_c("&id=", str_c(pmid, collapse = "&id="))
  
  if (length(pmid) <= 300) {
    url <- str_c(
      base,
      "elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs",
      url_id
    )
    output <- GET(url)
  } else {
    output <- POST(
      url = str_c(base, "elink.fcgi?"),
      body = str_c("dbfrom=pubmed&linkname=pubmed_pubmed_refs", url_id)
    )
  }
  xml_extract_text(output, "//LinkSet/LinkSetDb/Link")
}



refs_to_freqs <- function(pmid) {
  # detect repeated PMIDs and obtain their frequency
  freq <- sort(table(pmid), decreasing = TRUE)
  freq <- freq[freq > 1]
  result <- list(PMID = names(freq), n = unname(freq))
  if (length(freq) > 0) {
    result
  } else {
    validate("There is not enough information to suggest any specific articles")
  }
}



pmid_to_article <- function(pmid) {
  # obtain the titles for the PMIDs
  ids <- str_c(pmid, collapse = ",")
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  if (length(pmid) <= 300) {
    url <- str_c(base, "esummary.fcgi?db=pubmed&id=", ids, "&version=2.0")
    output <- GET(url)
  } else {
    output <- POST(
      url = str_c(base, "esummary.fcgi?"),
      body = str_c("dbfrom=pubmed&id=", ids, "&version=2.0")
    )
  }
  xml_extract_text(output, "//DocumentSummarySet/DocumentSummary/Title")
}

