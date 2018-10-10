library(XML)
library(RCurl)
library(pbapply)

rm(list=ls())

source('~/r-helpers/google-search/google-search-fx.R')
source('~/r-helpers/selenium-hacks/check-http-status-codes-fx.R')

# enter search term
search.term <- "NoBillag"

## specify the URL for searches:
# quotes:: quoted search term (T/F)
# n.pages:: number pages that should be returned; if n.pages > 1 -> additional urls are gerated for each page
search.url <- get_search_url(search.term=search.term, quotes=F, n.pages=2)

## get hits back
# raw:: if you want the raw url (T/F)
# drop.recursive:: if you want to drop results from picture result suggestions etc. (T/F)
hits <- pblapply(search.url, function(x) get_google_hits(x, raw=F, drop.recursives = T))

# check if they can be accessed or are dead links (is able to handle lists and vectors!)
hits.b <- check_status_code(hits)

print(hits.b, n=100)
