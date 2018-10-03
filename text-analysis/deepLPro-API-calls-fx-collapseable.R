translate_document <- function(txt, source_lang = NULL, target_lang = 'EN', id = NULL, collapse = FALSE, base = 'https://api.deepl.com', auth_key){
  
  Encoding(txt) <- 'UTF-8'
  
  if(nchar(txt) >= 5000){
    txt <- batch_text(txt, 5000) %>% 
      lapply(., function(x) gsub('&', '+', x))
  }else{txt <- list(txt)}
  
  if(!is.null(source_lang)){
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
  }else{
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
    
  }
  
  if(isTRUE(collapse)){
    transl <- tryCatch({transl %>% 
        summarise(translations.detected_source_language = paste0(unique(translations.detected_source_language), collapse = ' '), 
                  translations.text = paste0(unique(translations.text), collapse = ' '))}, 
        error = function(e){transl})
  }
  
  if(is.null(id)){
    return(transl)
  }else{
    return(mutate(transl, id=id))
  }
  
}
