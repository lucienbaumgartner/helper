library(stringr)

snowballer <- function(url, keywords=NULL, key.match='outer'){
  trunk <- str_extract(url, '^https?://[^/]+')
  html <- paste(suppressWarnings(tryCatch({readLines(url, skipNul = T)}, error=function(e){e})), collapse="\n")
  if(!suppressWarnings(grepl('Error', html))){
    matched <- str_match_all(html, "<a href=\"(.*?)\"")
    links <- matched[[1]][,2] %>% unique
    links <- sapply(links, function(x){
      if(grepl('^/', x)){return(paste0(trunk, x))}else{
        if(grepl('^#', x)|!grepl('(http?(s))|(\\.html)', x)){return(NA)}else{
          return(x)
        }
      }
    }) %>% 
      unname %>% 
      na.omit
    if(!is.null(keywords)&key.match=='outer') links <- links[grepl(keywords, tolower(links))]
    if(!is.null(keywords)&key.match=='inner'){
      log <- sapply(links, function(x){
        if(url.exists(x)){
          html <- getURL(x, followlocation = TRUE)
          doc = htmlParse(html, asText=TRUE)
          queries <- c(title = "//title", text = "//p")
          plain.text <- xpathSApply(doc, queries, xmlValue)
          return(any(grepl(keywords, tolower(plain.text))))
        }else{
          return(NA)
        }
      })
      links <- links[ifelse(is.na(log), FALSE, log)]
    }
    return(links)
  }else{
    return(NULL)
  }
}

cycler <- function(start, time.limit, keywords, key.match='outer'){
  url_log <- NULL  
  url_list <- start
  strt <- Sys.time()
  while(any(!url_list%in%url_log)){
    url_log <- c(url_list[!url_list%in%url_log], url_log)
    url_list <- tryCatch(lapply(url_list, function(x){
      res <- snowballer(x, keywords=keywords)
      stp <- Sys.time()
      if(difftime(stp, strt, units = c('secs'))>time.limit){break}else{return(res)}
      }) %>% 
      unlist %>% 
      c(url_list, .) %>% 
      unique, error=function(e){return(unique(c(url_list, url_log)))})
    stp <- Sys.time()
    if(difftime(stp, strt, units = c('secs'))>time.limit) break
  }
  
  return(url_list)
}
