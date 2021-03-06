Find main references
================
Daniel Medina-Neira

## Introduction

This app was created to obtain the most cited articles from a specific
topic using information from PubMed. This simple app, to achieve that,
uses the NCBI API services to retrieve information from different data
bases.

## How does the app obtains the information?

The app follows the following steps:

1.  Transform the search query into a query with terms used by PubMed.
2.  Obtain the PMIDs from this new “improved” query (also extract
    information like the titles and links).
3.  Obtain the references as PMIDs from these PMIDs of the searched
    articles.
4.  Obtain the frequencies of those PMIDs, this represents how many
    times an article was referenced.
5.  Obtain the titles of the articles from those PMIDs
6.  Finally the information obtained is used to generate tables and
    other interactions in the app and the app can also have some inputs
    to control different parameters of the process.

All this process is realized by the different functions used in the app.
The documentation of those functions is located in the individual
scripts.

### Example

This is an example of some of the steps in the process of retrieving
information from a search query:

``` r
query <- "gastritis AND covid"
# PMIDs of search
search_pmids <- query_to_pmid("(anemia OR anaemia) AND covid", limit = 30)
# PMIDs of citations
citation_pmids <- pmid_to_refs(search_pmids)
# frequency of citations
result_freqs <- refs_to_freqs(citation_pmids)

# obtain article titles and also links to those articles from a character vector with PMIDs
x <- pmid_to_title_link(result_freqs$PMID)
```
