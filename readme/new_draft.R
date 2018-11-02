library(XML)
library(RCurl)
library(pbapply)
library(dplyr)
library(stringi)
library(pbmcapply)
library(textcat)


rm(list=ls())

source('~/r-helpers/google-search/google-search-fx.R')
source('~/r-helpers/selenium-hacks/check-http-status-codes-fx.R')
source('~/r-helpers/selenium-hacks/extract-txt-fx.R')
source('~/r-helpers/text-analysis/text-cleaning-fx.R')
source('~/r-helpers/url-snowballing/url-snowballing-fx.R')

setwd('~/share/socsc1/output/')
# enter search term
search.term <- "No Billag"

## specify the URL for searches:
# quotes:: quoted search term (T/F)
# n.pages:: number pages that should be returned; if n.pages > 1 -> additional urls are gerated for each page
search.url <- get_search_url(search.term=search.term, language = 'de', quotes=F, n.pages=400)

## get hits back
# raw:: if you want the raw url (T/F)
# drop.recursive:: if you want to drop results from picture result suggestions etc. (T/F)
hits <- pbmclapply(search.url, function(x) get_google_hits(x, raw=F, drop.recursives = T), mc.cores=4)

# filter out all pages with no results
hits <- hits[lengths(hits)>0]

# save hits
save(hits, file=paste0('hits-full-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

# check if they can be accessed or are dead links (is able to handle lists and vectors!)
hits.b <- check_status_code(hits) %>% print(n=100)

# save checks
save(hits.b, file=paste0('hits-checked-', tolower(gsub('\\s+', '', search.term)), '.RDS'))



## use the snowballing algorithm
# NOT RUN: we use a keyword-restriction, only collecting links containing (no-billag|nobillag)
snow.balls <- pbmclapply(hits.b$url[hits.b$boolean==T], function(x){
  cycler(start=x, 
         time.limit=60,
         keywords='(no(-|\\s)?)?billag',
         key.match = 'inner'
  )
}, mc.cores=4)

# save
save(snow.balls, file=paste0('snowballing-res-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

unique(unlist(snow.balls))

# check them again
snow.balls.b <- check_status_code(snow.balls) %>% print(n=100)

save(snow.balls.b, file=paste0('snowballing-res-checked-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
load(paste0('snowballing-res-checked-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

snow.balls.b$url[snow.balls.b$boolean==T&!grepl('pdf|PDF|png|PNG|jpeg|jpg|JPEG|JPG', snow.balls.b$url)][310]
## get texts
# add.queries:: add additional queries; default: //p & //title
# preproc.expr:: additional regex expressions for preprocessing
# merged:: if T: return collapsed text (\n-sep); if F :returns a list with character strings (contents for each html-element)
# sort out pdfs/pngs/jpegs!!!!
txt <- pblapply(snow.balls.b$url[snow.balls.b$boolean==T&!grepl('pdf|PDF|png|PNG|jpeg|jpg|JPEG|JPG', snow.balls.b$url)], function(x){
  extract_txt(x,
              merged = FALSE,
              add.queries = c(h2 = '//h2', h3 = '//h3', li='//li'), 
              preproc.expr = '(^(\\s+)?$)|(\\\n(\\s+)?)|(\\s{2,})'
  )
})

# save texts
save(txt, file=paste0('txt-extrctd-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
load(paste0('txt-extrctd-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

## clean texts
# load list with newspaper name
load('~/share/socsc1/input/liste-ch-zeitungen-wiki.RDS')
# some names a quite problematic, since they are liekly to occur in a normal sentence too...
# let's filter them out
newspapers <- newspapers[!newspapers%in%c('.ch', 
                                          'Der Bund', 
                                          'Die Hauptstadt',
                                          'Die Heimat',
                                          'News',
                                          'heute',
                                          'Saiten',
                                          'Zürich',
                                          'Zollikerberg',
                                          'Der Eidgenosse',
                                          'Das Volk',
                                          'Die Nation',
                                          'Die Woche')]

# raw:: if T: only returns annotated texts (tibble); if F: actually computes transformations
# hard.filter:: if grepl returns true on any of these elements, the respective line (== HTML-element) will be removed
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

# define buzzwords
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
               '(\\|)',
               '©',
               'app',
               'bitte',
               'kommentar',
               'kompaktansicht',
               'weiterlesen',
               'jetzt spenden',
               'hol dir',
               '…',
               'woz',
               '20 minuten',
               'live',
               'resultate',
               'youtube',
               'das könnte sie interessieren',
               'fonction',
               'sauter',
               'navigation',
               'recherche',
               'all rights reserved',
               'external link',
               'click here',
               'newsletter',
               'signing up',
               'swissinfo.ch',
               'copyright',
               'cookies', 
               'watson',
               'radio life',
               'erf medien',
               '\\(c\\)',
               'channel',
               tolower(newspapers)) %>% 
  gsub('\\.', '\\\\.', .)  

# check for correct implementation of punctation
grep('\\.', buzzwords, value=T)

# define additional hardfilter grepl regex
hard.filter <- c('\\((fr|de|it|pt|es|en|ru|ar)\\)', 
                 '([0-9]{2}\\:[0-9]{2}[A-z]+)|([0-9]{2}\\:[0-9]{3}\\s)|([0-9]{2}\\:[0-9]{4}\\.[0-9])', 
                 '([A-Z][a-z]+[A-Z][a-z]+)')

# call cleaner
txt.clean <- pblapply(txt, function(x){
  clean_text(x,
             hard.filter = hard.filter,
             slicing = T,
             min.words = 6,
             max.buzzwords = 3,
             scnd.step.slicing = 3, 
             scnd.step.threshold = 20,
             buzzwords = buzzwords,
             recover.fs = 50)
})

# save
save(txt.clean, file=paste0('txt-cleaned-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
load(paste0('txt-cleaned-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

# all texts
txt.clean

# control elements that have been deleted
unlist(sapply(1:length(txt),function(x){
  if(length(txt[[x]])>1){
    # print(x) # untag for error handling
    txt[[x]][!txt[[x]]%in%txt.clean[[x]]$txt]
  }else{NA}}))

# get rid of elments that were single line documents (<- NA), and those that don't contain any information anymore after cleaning
txt.clean <- txt.clean[!is.na(txt.clean)] %>% 
  .[unlist(sapply(., nrow))>1]

# save
save(txt.clean, file=paste0('txt-cleaned-noNAs-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
load(paste0('txt-cleaned-noNAs-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

# add language variable, merge texts, and convert them to true UTF-8
corp <- tibble(txt.burnt=txt.clean, 
               language=pbsapply(txt.clean, function(x){textcat(paste0(x$txt, collapse = ' '))}), 
               txt.merged=sapply(txt.clean, function(x){paste0(x$txt, collapse = '\n')}),
               txt.merged.utf8=iconv(txt.merged, from = "UTF-8", to = "MAC") %>% iconv(., from = "MAC", to="UTF-8")
               ) %>% 
  mutate(txt.merged.utf8=ifelse(is.na(txt.merged.utf8), iconv(corp$txt.merged, from = "UTF-8" , to ="WINDOWS-1252"), txt.merged.utf8))
# save
save(corp, file=paste0('corpus-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
load(paste0('corpus-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

# subset 
corp.de <- filter(corp, language=='german')
dim(corp)
