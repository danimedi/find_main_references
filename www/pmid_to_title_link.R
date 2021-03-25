#' Obtain both title and link from a list of PMIDs
#' 
#' It uses the function `pmid_to_article` to obtain the titles of the articles from the NCBI API.
#' Then it creates a list adding the link based on those PMIDs.
#'
#' @param pmid character vector of PMIDs
#'
#' @return A list with 2 elements: `Article` and `Link` containing the names of the articles and
#' the links of those articles.
#' @export
#'
#' @examples
#' 
pmid_to_title_link <- function(pmid) {
  list(
    Article = pmid_to_article(pmid),
    Link = str_c("https://pubmed.ncbi.nlm.nih.gov/", pmid)
  )
}
