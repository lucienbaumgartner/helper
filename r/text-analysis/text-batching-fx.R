batch_text <- function(usr.text, batchsize){
  container <- list()
  fit <- substr(usr.text, 1, batchsize) 
  
  for(i in 1:(round(nchar(usr.text)/batchsize, 0)+20)){
    
    container[[i]] <- tryCatch({fit %>% 
        gsub('\\.\\.\\.', '\\C-x', .) %>% 
        strsplit(., '(?:(?<=[?!.]\\«\\s)|(?<=[?!.]\\s))', perl=T) %>% 
        unlist(., recursive = F) %>%  
        .[length(.)] %>% 
        paste0(., '$') %>% 
        gsub('\\C-x', '\\.\\.\\.', .) %>%
        gsub(., '', fit)}, error = function(e){e})
    
    if(grepl('\\berror\\b|\\bError\\b', container[[i]])){
      container <- 'batching error'
      break
    }
    
    if(container[[i]]=='') container[[i]] <- fit
    
    ptrn <- str_sub(container[[i]], start= -30) %>% 
      gsub('([[:punct:]])', '\\\\\\1', .) 
    
    fit <- gsub(paste0('.*', ptrn), '', usr.text) %>% 
      substr(., 1, batchsize)
    
    if(fit=='') break
  }
  return(container)
} 
