#' Obtain the citations of the articles from DOIs
#'
#' @param dois character vector with the DOIs
#' @param format name of the format. One of "rdf-xml", "turtle", "citeproc-json", 
#' "citeproc-json-ish", "text", "ris", "bibtex" (default), "crossref-xml", 
#' "datacite-xml","bibentry", or "crossref-tdm". Those formats depend on the function 
#' `rcrossref::cr_cn`
#'
#' @return A character vector with citations in the APA style for the articles obtained 
#' from the DOIs
#' @export
#'
#' @examples
#' 
refs_from_dois <- function(dois, format = "text") {
  refs <- rcrossref::cr_cn(dois = dois, format = format)
  refs <- unlist(refs)
  refs <- refs[!is.na(refs)]
  refs
}
