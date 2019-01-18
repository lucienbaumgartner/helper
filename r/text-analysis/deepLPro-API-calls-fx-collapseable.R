#################################################################
# DeepL API fx
#################################################################
# Content
#################################################################
# Dependencies
# API Calls
#################################################################


#################################################################
# Dependencies
#################################################################
# global
library(httr)
library(dplyr)
# local
source('~/hub/helper/r/text-analysis/text-batching-fx.R')

#################################################################
# API Calls
#################################################################
translate_document <- function(txt, source_lang = NULL, target_lang = 'EN', id = NULL, collapse = FALSE, base = 'https://api.deepl.com', auth_key){
  # define encoding
  Encoding(txt) <- 'UTF-8'
  # batch text if neeeded
  if(nchar(txt) >= 5000){
    txt <- batch_text(txt, 5000) %>%
      lapply(., function(x) gsub('&', '+', x))
  }else{txt <- list(txt)}
  # call if source language is specified
  if(!is.null(source_lang)){
    # apply over all batches
    transl <- lapply(
      txt, function(txt){
        # API call
        GET(paste0(base,
                   '/v1/translate?text=',
                   URLencode(txt),
                   '&source_lang=', toupper(source_lang),
                   '&target_lang=', target_lang,
                   '&auth_key=', auth_key
        )
        ) %>%
          # extracting output
          content(.) %>%
          unlist(., recursive = F) %>%
          unlist(., recursive = F) %>%
          as_tibble
      }
    ) %>%
      do.call(rbind, .)
  }else{
    # call if it is not specified
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
  # collapse batches back to full text
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
