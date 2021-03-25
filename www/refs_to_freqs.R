#' Obtain the frequency of the elements of a character vector
#' 
#' It is used to count the number of PMIDs to be used in the app. If there are no PMIDs with 
#' a frequency higher than 1, then an error for the Shiny app is returned (`validate()`). 
#' Finally a **list** is returned with the PMIDs and the frequency.
#'
#' @param pmid character vector containing the PMIDs used to obtain the frequency
#'
#' @return A list with 2 elements: `PMID` and `n`, containing the elements and its frequency, 
#' respectively.
#' @export
#'
#' @examples
#' 
refs_to_freqs <- function(pmid) {
  freq <- sort(table(pmid), decreasing = TRUE)
  freq <- freq[freq > 1]
  result <- list(PMID = names(freq), n = unname(freq))
  if (length(freq) > 0) {
    result
  } else {
    validate("There is not enough information to suggest any specific articles")
  }
}
