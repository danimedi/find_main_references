#' Obtain titles of articles from PMIDs
#' 
#' Use the API to obtain the titles from the PMIDs
#'
#' @param pmid character vector of PMIDs
#'
#' @return A character vector with the titles of the PMIDs used as inputs.
#' @export
#'
#' @examples
#' 
pmid_to_article <- function(pmid) {
  ids <- stringr::str_c(pmid, collapse = ",")
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  if (length(pmid) <= 300) {
    url <- stringr::str_c(base, "esummary.fcgi?db=pubmed&id=", ids, "&version=2.0")
    output <- httr::GET(url)
  } else {
    output <- httr::POST(
      url = stringr::str_c(base, "esummary.fcgi?"),
      body = stringr::str_c("db=pubmed&id=", ids, "&version=2.0")
    )
  }
  xml2::xml_extract_text(output, "//DocumentSummarySet/DocumentSummary/Title")
}
