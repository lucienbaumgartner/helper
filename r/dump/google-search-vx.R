library(XML)
library(RCurl)
library(pbapply)
library(dplyr)
library(stringi)
library(pbmcapply)

rm(list=ls())

source('~/r-helpers/google-search/google-search-fx.R')
source('~/r-helpers/selenium-hacks/check-http-status-codes-fx.R')
source('~/r-helpers/selenium-hacks/extract-txt-fx.R')
source('~/r-helpers/text-analysis/text-cleaning-fx.R')

# enter search term
search.term <- "NoBillag"

## specify the URL for searches:
# quotes:: quoted search term (T/F)
# n.pages:: number pages that should be returned; if n.pages > 1 -> additional urls are gerated for each page
search.url <- get_search_url(search.term=search.term, language = 'de', quotes=F, n.pages=2)

## get hits back
# raw:: if you want the raw url (T/F)
# drop.recursive:: if you want to drop results from picture result suggestions etc. (T/F)
hits <- pblapply(search.url, function(x) get_google_hits(x, raw=F, drop.recursives = T))

# check if they can be accessed or are dead links (is able to handle lists and vectors!)
hits.b <- check_status_code(hits) %>% print(n=100)

## get texts
# add.queries:: add additional queries; default: //p & //title
# preproc.expr:: additional regex expressions for preprocessing
# merged:: if T: return collapsed text (\n-sep); if F :returns a list with character strings (contents for each html-element)
txt <- pbmclapply(hits.b$url[hits.b$boolean==T], function(x){
  extract_txt(x,
              merged = FALSE,
              add.queries = c(h2 = '//h2', h3 = '//h3', li='//li'), 
              preproc.expr = '(^(\\s+)?$)|(\\\n(\\s+)?)|(\\s{2,})'
  )
}, mc.cores=4)

# example
txt[[18]]

## clean text
# raw:: if T: only returns annotated texts (tibble); if F: actually computes transformations
# buzzwords:: additional buzzwords (default can be accessed by running `default.buzzwords`)
#         |_ buzzwords are used to indicate possible sclicing points and to compute filter conditions
# min.words:: minimum number of words per single character string (used for filtering, default: 3)
# min.avg.characters:: minimum number of avg.characters per word for each single character string (used for filtering, default: 3)
# max.buzzwords:: max. number of buzzwords allowed per character string (default: 2)
# sclicing:: whether slicing is allowed or not [slicing==deleting last part of text according to user-specified conditions] (T/F)
# slicing.keywords:: character string with additional slicing keywords (default can be accessed by running `default.slicing.keywords`)
#         |_ [!!!] each slicing keywords have to be identical to a buzzword
#         |_ [!!!] HENCE: sclicing.keyword is a subset of buzzwords
#         |_ [!!!] it is recommended to use them very cautiously, since they lead to deletions of whole text parts
# scnd.step.slicing:: last share of the document that will be used to compute the sum buzzwords (default: 3; meaning: last third of the document)
# scnd.step.threshold:: max number of buzzwords allowed in scnd.step.slicing before proceeding to slice the data (default: 4)

txt.clean <- pblapply(txt, function(x) clean_text(x,
                                                  min.words = 4,
                                                  max.buzzwords = 4,
                                                  scnd.step.slicing = 4, 
                                                  scnd.step.threshold = 21,
                                                  buzzwords = c('bild',
                                                                'video',
                                                                'kontakt', 
                                                                'links',
                                                                '(\\&)',
                                                                'registrier',
                                                                'vielen dank',
                                                                'wir wünschen ihnen',
                                                                'nzz', 
                                                                'blick',
                                                                'anmeld',
                                                                'passwort',
                                                                'alle rechte',
                                                                'medienwoche',
                                                                '([0-9]{5,})',
                                                                '(\\|)')))

# example
txt.clean[[6]] %>% print(n=500)

# all the deleted elements 
unlist(sapply(1:length(txt),function(x) txt[[x]][!txt[[x]]%in%txt.clean[[x]]$txt]))


parameters <- expand.grid(scnd.step.slicing=3:5, 
                          scnd.step.threshold=20:30, 
                          min.words=1:4, 
                          min.avg.characters=3:6, 
                          max.buzzwords=1:5)
buzzwords <- c('bild',
               'video',
               'kontakt', 
               'links',
               '(\\&)',
               'registrier',
               'vielen dank',
               'wir wünschen ihnen',
               'nzz', 
               'blick',
               'anmeld',
               'passwort',
               'alle rechte',
               'medienwoche',
               '([0-9]{5,})',
               '(\\|)')

benchmarking <- lapply(txt, function(txt){
  temp <- mapply(function(scnd.step.slicing, scnd.step.threshold, min.words, min.avg.characters, max.buzzwords){
    temp <- clean_text(txt=txt[[1]], 
                       buzzwords = buzzwords, 
                       slicing = T,
                       slicing.keywords = NULL,
                       scnd.step.slicing = scnd.step.slicing,
                       scnd.step.threshold = scnd.step.threshold,
                       min.words = min.words,
                       min.avg.characters = min.avg.characters,
                       max.buzzwords = max.buzzwords
    )
    
    return(tibble(avg.nchars = mean(sapply(tmp, sum(nchar, nr.rm = T))),
                  p.identical = txt[!txt%in%tmp$txt]/nrow(txt)))
  }, scnd.step.slicing=parameters$scnd.step.slicing, list(scnd.step.threshold=parameters$scnd.step.threshold,
                                                          min.words=parameters$min.words,
                                                          min.avg.characters=parameters$min.avg.characters,
                                                          max.buzzwords=parameters$max.buzzwords))
  return(temp)
})


temp <- lapply(1:length(txt), function(indx){
  print(indx)
  temp <- pbmclapply(1:nrow(parameters), function(x){
    clean_text(txt=txt[[indx]], 
               buzzwords = buzzwords, 
               slicing = T,
               slicing.keywords = NULL,
               scnd.step.slicing=parameters$scnd.step.slicing[x],
               scnd.step.threshold=parameters$scnd.step.threshold[x],
               min.words=parameters$min.words[x],
               min.avg.characters=parameters$min.avg.characters[x],
               max.buzzwords=parameters$max.buzzwords[x]
    )
    
  }, mc.cores = 5)})

benchmarking.avg <- lapply(1:length(txt), function(indx){
  print(indx)
  avg <- pbmclapply(1:length(temp[[1]]), function(y){
    tibble(
      avg.nchars = mean(sapply(temp[[indx]][[y]]$txt, function(x) sum(nchar(x), nr.rm = T))),
      p.identical = length(txt[[indx]][!txt[[indx]]%in%temp[[indx]][[y]]$txt])/nrow(txt[[indx]])
    )
  }, mc.cores = 5) %>% 
    do.call(rbind, .)
})

