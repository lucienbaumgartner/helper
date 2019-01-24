regexify_names <- function(x, method='strict'){
  frac <- unlist(strsplit(x, '\\s'))
  if(any(grepl('(^(v|V)on$)|(^(d|D)(e(s)?|i)$)', frac))) frac <- c(paste(frac[1], frac[2]), frac[3:length(frac)])
  if(any(grepl('[a-z]\\-[A-Z]', frac))){
    for(i in frac[grepl('\\-', frac)]){
      frac[frac==i] <- paste0(
        '((', gsub('(.*[A-z]+\\-)', '(\\1)?', i),')|(', gsub('(\\-[A-z].*)', '(\\1)?', i),'))'
      )
    }
  }
  if(length(frac)==2){
    frac <- paste(frac[2], frac[1])
  }else{
    if(grepl('\\(.*\\)', frac[length(frac)])){
      frac[length(frac)] <- gsub('([[:punct:]])', '(\\\\\\1)?', frac[length(frac)])
      frac <- paste0(frac[2], # formerly: frac[length(frac)-1]
                     paste0('((\\s)?(',frac[-c(1:2)],')?(\\s)?)', collapse = '(\\s)?'), 
                     # formerly: paste0('((\\s)?(',frac[-c(1, length(frac))],')?(\\s)?)', collapse = '(\\s)?')
                     frac[1]
      )
    }else{
      if(method=='loose'){
        frac.var <- paste0(
          frac[length(frac)],
          #paste0('((\\s)?(',frac[-c(1:2)],')?(\\s)?)', collapse = '(\\s)?'), 
          paste0(
            '(\\s', frac[1],
            '|',
            paste0('((\\s)?(',frac[-c(1,length(frac))],')(\\s)?)', collapse = '(\\s)?'),
            ')'
          )
        )
        frac <- paste0(frac[2], # formerly: frac[length(frac)]
                       paste0('((\\s)?(',frac[-c(1:2)],')?(\\s)?)', collapse = '(\\s)?'), 
                       # paste0('((\\s)?(',frac[-c(1,length(frac))],')?(\\s)?)', collapse = '(\\s)?')
                       frac[1]
        )
        frac <- paste0('(', paste0('(', frac, ')|(', frac.var, ')'), ')')
      }else{
        frac <- paste0(frac[2], # formerly: frac[length(frac)]
                       paste0('((\\s)?(',frac[-c(1:2)],')?(\\s)?)', collapse = '(\\s)?'), 
                       # paste0('((\\s)?(',frac[-c(1,length(frac))],')?(\\s)?)', collapse = '(\\s)?')
                       frac[1]
        )
      }
    }
    
  }
  return(paste0('(', gsub(' ', '\\\\s', frac), ')'))
}



############## dev
regexify_names2 <- function(firstN, lastN,  method='strict'){
  if(grepl('[a-z](\\-|\\s)[A-Z]', lastN)){
    if(grepl('[a-z](\\-)[A-Z]', lastN)){
      lastN <- 
        paste0(
          '((', gsub('(.*[A-z]+\\-)', '(\\1)?', lastN),')|(', gsub('(\\-[A-z].*)', '(\\1)?', lastN),'))'
        )
    }
    if(grepl('[a-z](\\s)[A-Z]', lastN)){
      lastN <- 
        paste0(
          '((', gsub('(.*[A-z]+)\\s', '(\\1\\\\s)?', lastN),')|(', gsub('\\s([A-z].*)', '(\\\\s\\1)?', lastN),'))'
        )
    }
    
  }
  if(length(unlist(strsplit(firstN, '\\s')))>1){
    firstN <- unlist(strsplit(firstN, '\\s'))
    firstN <-
      paste0(
        firstN[1],
        paste0(
          '((\\s)?(',firstN[2:length(firstN)],')?(\\s)?)', collapse = '(\\s)?'
        )
      )
  }else{
    firstN <- paste0(firstN, '\\s')
  }
  return(paste0(firstN, lastN))
}

#regexify_names2(firstN='Heiri', lastN='Müller Hans')
