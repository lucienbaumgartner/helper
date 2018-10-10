library(XML)
library(RCurl)
library(pbapply)

extract_txt <- function(urls, add.queries=NULL, preproc.expr=NULL){
  if(is.list(urls)) url <- unlist(urls)
  
  pbsapply(urls, function(s.url){
    html <- getURL(s.url, followlocation = TRUE)
    
    doc = htmlParse(html, asText=TRUE)
    queries <- c(title = "//title", text = "//p", add.queries)
    plain.text <- xpathSApply(doc, queries, xmlValue)
    plain.text <- gsub('\\{.*\\}', '', plain.text)
    if(!is.null(preproc.expr)) plain.text <- plain.text[!grepl(preproc.expr, plain.text)]
    return(paste(plain.text, collapse = "\n"))
  })
}



