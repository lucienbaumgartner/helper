library(stringi)
library(dplyr)


buzz_matches <- function(char.list, buzzwords){
  matches <- stri_extract_all_regex(tolower(char.list), buzzwords)
  
  matches <- sapply(matches, function(x){
    if(!any(is.na(x))){paste0(x, collapse = ';')}else{x}
  })
  
  return(matches)
}

default.buzzwords <- c('twitter',
                       'facebook', 
                       '(^impressum$)', 
                       'account', 
                       'aktivieren', 
                       'deaktivieren',
                       'abo', 
                       'adresse',
                       'strasse',
                       'e-mail',
                       'benutzerkonto',
                       '(anmelden (und|oder) registrieren)',
                       'angaben zu ihrer person',
                       'bleiben sie auf dem laufenden',
                       'keine news verpassen')

default.slicing.keywords <- c('impressum',
                              'bleiben sie auf dem laufenden',
                              'keine news verpassen')

clean_text <- function(txt,
                       raw=F,
                       hard.filter=NULL,
                       buzzwords=NULL, # character string with buzzwords
                       slicing=T, # if slicing should be performed
                       slicing.keywords=NULL, # deterministic slicing keywords [caution!]
                       scnd.step.slicing=3, # last part of strings (here: last third) that will be used to compute the sum buzzwords
                       scnd.step.threshold=4, # max number of buzzwords allowed in scnd.step.slicing before proceeding to slice the data
                       min.words=3, # minimum number of words per single character string (used for filtering)
                       min.avg.characters=3, # minimum number of avg.characters per word for each single character string (used for filtering)
                       max.buzzwords=2, # max. number of buzzwords allowed per character string
                       recover.fs=50
){
  if(is.list(txt)) txt <- unlist(txt, recursive = F)
  if(length(txt)%in%c(0,1)){return(NA)}
  if(is.null(buzzwords)) buzzwords <- default.buzzwords
  buzzwords <- paste0(c(buzzwords, default.buzzwords), collapse = '|')
  if(is.null(slicing.keywords)) slicing.keywords <- default.slicing.keywords
  slicing.keywords <- paste0(c(slicing.keywords, default.slicing.keywords), collapse = '|')
  txt_splt <- sapply(txt, function(string) strsplit(string, ' '))
  txt <- tibble(txt=unlist(txt), 
                n.words=lengths(txt_splt),
                nchar.words.mean=sapply(txt_splt, function(x) mean(nchar(x), na.rm = T)),
                buzz.words=buzz_matches(unlist(txt), buzzwords),
                n.buzzwords=lengths(
                  sapply(
                    strsplit(buzz.words, ';'), 
                    function(x) if(any(is.na(x))){NULL}else{x}
                  )
                ),
                id=paste0('id_', 1:length(n.buzzwords))
  )
  txt_log <- txt
  if(isTRUE(raw)){
    return(txt)
  }else{
    if(isTRUE(slicing)&length(grep(slicing.keywords, txt$buzz.words))>0){
      if(any(table(txt$buzz.words, useNA = 'no')>1)){
        txt <- txt[as.integer(seq_len(max(which(grepl(slicing.keywords, txt$buzz.words)))-1)),]
      }else{
        txt <- txt[as.integer(seq_len(min(which(grepl(slicing.keywords, txt$buzz.words)))-1)),]
      }
    }
    txt <- filter(txt, !(n.words<min.words|nchar.words.mean<min.avg.characters|n.buzzwords>=max.buzzwords))
    if(!is.null(hard.filter)) txt <- filter(txt, !grepl(paste0(hard.filter, collapse = '|'), txt))
    if(isTRUE(slicing)&sum(txt$n.buzzwords[round(nrow(txt)-nrow(txt)/scnd.step.slicing, 0):nrow(txt)])>scnd.step.threshold){
      temp <- txt[round(nrow(txt)-nrow(txt)/scnd.step.slicing, 0):nrow(txt),]
      id <- temp$id[min(which(temp$n.buzzwords>0))]
      txt <- txt[as.integer(seq_len(min(which(txt$id==id))-1)),]
    }
    txt <- txt_log[(txt_log$id%in%txt$id)|(txt_log$n.words>=recover.fs),]
    return(txt)
  }
  
}
                           
