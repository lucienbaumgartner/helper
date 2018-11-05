library(XML)
library(RCurl)

extract_txt <- function(urls, merged=TRUE, add.queries=NULL, preproc.expr=NULL){
  if(is.list(urls)) url <- unlist(urls)
  
  sapply(urls, function(s.url){
    html <- tryCatch(getURL(s.url, followlocation = TRUE), error=function(e) return(NA))
    if(is.na(html)) return(NA)
    
    doc = htmlParse(html, asText=TRUE)
    queries <- c(title = "//title", text = "//p", add.queries)
    plain.text <- xpathSApply(doc, queries, xmlValue)
    plain.text <- gsub('(\\{.*\\}(\\.)?)|(^\\.$)', '', plain.text)
    if(!is.null(preproc.expr)) plain.text <- plain.text[!grepl(preproc.expr, plain.text)]
    if(isTRUE(merged)){
      return(paste(plain.text, collapse = "\n"))
      }else{
        plain.text <- as.list(as.vector(plain.text))
        return(plain.text)
      }
  })
}

