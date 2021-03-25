#' Obtain the results as PMIDs of a search query in PubMed
#'
#' @param query string containing the search query (e.g. "(anemia OR anaemia) AND covid")
#' @param limit maximum number of articles to be retrieved from the API
#'
#' @return A vector of strings containing the PMIDs of those articles obtained from the API used
#' in the search query
#' @export
#'
#' @examples
#' query_to_pmid("(anemia OR anaemia) AND covid", 100)
#' 
query_to_pmid <- function(query, limit = 300) {
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
