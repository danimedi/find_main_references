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
