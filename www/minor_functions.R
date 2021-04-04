
# extract elements of a response of the API using xpaths
xml_extract_text <- function(response, xpath) {
  xml_text(xml_find_all(xml_contents(content(response)), xpath))
}

# extract arguments of a response of the API using xpaths
xml_extract_attrs <- function(response, xpath) {
  unlist(
    xml_attrs(xml_find_all(xml_contents(content(response)), xpath)), 
    use.names = FALSE
  )
}

# create a query with UTF-8 URL percent encoding
create_query <- function(query) {
  query <- URLencode(enc2utf8(query), reserved = TRUE)
  query <- gsub("%20", "+", query)
  query
}
