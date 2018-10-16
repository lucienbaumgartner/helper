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
source('~/r-helpers/text-analysis/text-post-processing-fx.R')

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
txt <- pbmclapply(hits.b$url[hits.b$boolean==T], function(x){
  extract_txt(x,
              merged = FALSE,
              add.queries = c(h2 = '//h2', h3 = '//h3', li='//li'), 
              preproc.expr = '(^(\\s+)?$)|(\\\n(\\s+)?)|(\\s{2,})'
              )
}, mc.cores=4)
txt

## clean text
txt.clean <- pblapply(txt, function(x) post_processing(x,
                                                       min.words = 4,
                                                       max.buzzwords = 3,
                                                       scnd.step.slicing=3, 
                                                       scnd.step.threshold = 5,
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

txt.clean[[18]] %>% print(n=500)

checks <- sapply(1:length(txt),function(x) txt[[x]][!txt[[x]]%in%txt.clean[[x]]$txt]) 
unlist(sapply(1:length(txt),function(x) txt[[x]][!txt[[x]]%in%txt.clean[[x]]$txt])) %>% tail(1000)
checks[1]
