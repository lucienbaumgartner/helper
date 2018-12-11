translate_document <- function(txt, source_lang = NULL, target_lang = 'EN', base = 'https://api.deepl.com', auth_key, id = NULL){
  
  Encoding(txt) <- 'UTF-8'
  
  txt <- batch_text(txt, 5000) %>% 
    lapply(., function(x) gsub('&', '+', x))
  
  if(!is.null(source_lang)){
    transl <- lapply(
      txt, function(txt){
        GET(paste0(base, 
                   '/v1/translate?text=', 
                   URLencode(txt),  
                   '&target_lang=', target_lang, 
                   '&auth_key=', auth_key
        )
        ) %>% 
          content(.) %>% 
          unlist(., recursive = F) %>% 
          unlist(., recursive = F) %>% 
          as_tibble
      }
    ) %>% 
      do.call(rbind, .)
  }else{
    transl <- lapply(
      txt, function(txt){
        GET(paste0(base, 
                   '/v1/translate?text=', 
                   URLencode(txt), 
                   '&source_lang=', toupper(source_lang),  
                   '&target_lang=', target_lang, 
                   '&auth_key=', auth_key
        )
        ) %>% 
          content(.) %>% 
          unlist(., recursive = F) %>% 
          unlist(., recursive = F) %>% 
          as_tibble
      }
    ) %>% 
      do.call(rbind, .)
    
  }
  
  
  if(is.null(id)){
    return(transl)
  }else{
    return(mutate(transl, id=id))
  }
  
}