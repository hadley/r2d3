# JSON inspection/validation - currently internal only

# Needs jsonpp (http://jmhodges.github.com/jsonpp/) to work
pp <- function(x) {
  json <- json(x)
  
  jsonpp <- pipe("jsonpp")
  on.exit(close(jsonpp))
  
  writeChar(json, jsonpp, eos = NULL)
}
val <- function(x) {
  json <- json(x)
  browseURL(paste0("http://jsonlint.com/?json=", URLencode(json)))
}

