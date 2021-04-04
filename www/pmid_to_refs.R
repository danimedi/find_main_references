#' Use the PMIDs of articles to obtain the PMIDs of the references of that articles
#' 
#' Search the references of a list of articles (as PMIDs) and return them as PMIDs too.
#'
#' @param pmid character vector with the PMIDs of articles
#'
#' @return A character vector with the PMIDs of the references of the articles used as
#' inputs in the function
#' @export
#'
#' @examples
#' 
pmid_to_refs <- function(pmid) {
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  
  # here we're using `"&id="` instead of commas, this is because that way you obtain
  # the references from artiles independently, otherwise the repeated articles would
  # collapse
  url_id <- stringr::str_c("&id=", stringr::str_c(pmid, collapse = "&id="))
  
  if (length(pmid) <= 300) {
    url <- stringr::str_c(
      base,
      "elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs",
      url_id
    )
    output <- httr::GET(url)
  } else {
    output <- httr::POST(
      url = stringr::str_c(base, "elink.fcgi?"),
      body = stringr::str_c("dbfrom=pubmed&linkname=pubmed_pubmed_refs", url_id)
    )
  }
  xml2::xml_extract_text(output, "//LinkSet/LinkSetDb/Link")
}
