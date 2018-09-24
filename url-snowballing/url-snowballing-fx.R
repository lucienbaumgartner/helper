snowballer <- function(url, keywords){
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
      na.omit %>% 
      .[grepl(keywords, .)]
    return(links)
  }else{
    return(NULL)
  }
}

cycler <- function(start, time.limit, keywords){
  url_log <- NULL  
  url_list <- start
  strt <- Sys.time()
  while(any(!url_list%in%url_log)){
    url_log <- c(url_list[!url_list%in%url_log], url_log)
    url_list <- lapply(url_list, function(x) snowballer(x, keywords=keywords)) %>% 
      unlist %>% 
      c(url_list, .) %>% 
      unique
    stp <- Sys.time()
    if(stp-strt>time.limit) break
  }
  return(url_list)
}