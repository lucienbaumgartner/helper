library(XML)
library(RCurl)
library(pbapply)

extract_txt <- function(urls, add.queries=NULL){
  if(is.list(urls)) url <- unlist(urls)
  
  pbsapply(urls, function(s.url){
    html <- getURL(s.url, followlocation = TRUE)
    
    # parse html
    doc = htmlParse(html, asText=TRUE)
    queries <- c(title = "//title", text = "//p", add.queries)
    plain.text <- xpathSApply(doc, queries, xmlValue)
    plain.text <- gsub('\\{.*\\}', '', plain.text)
    plain.text <- plain.text[!grepl('(^(\\s+)?$)|(\\\n(\\s+)?)', plain.text)]
    return(paste(plain.text, collapse = "\n"))
  })
}



