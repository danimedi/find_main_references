#' Obtain the DOIs from articles in PubMed obtained using the search query
#'
#' @param query string containing the search query
#' @param limit limit of articles
#' @param api_key NCBI API key, it's **optional**
#'
#' @return A character vector of the DOIs from the articles obtained using the search query.
#' @export
#'
#' @examples
#' doi_from_query("anemia OR anaemia", limit = 20)
#' 
dois_from_query <- function(query, limit = 20, api_key = NULL) {
  D <- pubmedR::pmApiRequest(query = query, limit = limit, api_key = api_key)
  M <- pubmedR::pmApi2df(D)
  M$DI[!is.na(M$DI)]
}
