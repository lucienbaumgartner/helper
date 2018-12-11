---
title: "Google Search crawling, HTTP status checks, URL snowballing, text scraping, and document burning"
author: "Lucien Baumgartner"
date: "10/25/2018"
alias: /post/6301645915/how-i-keep-limited-pressing-running/index.html
output:
  html_document:
    toc: true
    number_sections: true
    keep_md: yes

---



<style type="text/css">
pre code, pre, code {
  white-space: pre !important;
  overflow-x: scroll !important;
  word-break: keep-all !important;
  word-wrap: initial !important;
}

pre.html{
  background-color:white;
}

pre.r{
  background-color:black;
  color:white;
}
</style>



# Overview
The following procedure has been developed to provide a stable URL collection method than can be coupled with text extraction and text burning features. The method was used for [this](https://github.com/lucienbaumgartner/share/blob/master/socsc1/research-proposal-socsc1.pdf) research proposal for SocialScienceOne. The main goal of the workflow is to generate a corpus of URLs related to online media content for all Swiss referanda from June, 2017 to March, 2018, including their text body. The starting point for the URL collection is a list of the top Google searches for each initiative prior to the respective ballots. From there, the algorithm accesses all URLs and extracts all potential links to other informational content on the referendum, this is hyperreferences indluded on those pages. To avoid including links to web-ads or other unrelated content we applied a keyword filter to the newly scraped URLs. The new URLs are then matched to the ones from the initial collection. Only the new entries are then accessed in turn, to extract all possible references, and so on, until there is no new reference or a time limit has been reached.

In essence, the workflow has the following structure:

```
[[Google SEARCH CRAWLING]]
_ create Google Search URLs
  |_ extract top hits from n pages
    x .. [[HTTP STATUS CHECKS]]
    |_ check whether the webpages are still available
      |_ y: keep
        x .. [[URL SNOWBALLING]]
        |_ for ervery Google hit; SET:
          |_ URL restriction parameter
            [keywords that have to be in the URL for it to be added to the collection]
          |_ time limit for QUERY
            [from root URL]
        |_ for ervery Google hit; DO.CALL: snowballing algorithm
            |_ access root URL; extract all hrefs
            |_ add hrefs to LOG-file{ONLY IF: new}
              |_ access new hrefs from LOG; extract all hrefs
              |_ add hrefs to LOG-file{ONLY IF: new}
                |_ .. UNTIL no new unique hrefs
                    .. BREAK{IF: time reached before collection completed}
                      [prevents endless crawling]
                      x .. [[HTTP STATUS CHECKS]]
                      |_ check whether the webpages are still available
                        |_ y: keep
                          ///// OPTIONAL: .. [[TEXT MINING]]
                          x
                          |_ exctract texts; SET:
                            |_ HTML elements that should be extracted
                            |_ preprocessing regex
                            |_ merged (y/n) [single/multielement]
                              x
                              |_ burning algorithm; SET PARAM
                                ///// for EXPL see below
                        |_ n: drop
      |_ n: drop
```

# Functions

These are the functions written for each task.

## Google Search crawling

### Get search URLs

This function does nothing more than generate URLs to access the top n pages of Google hits for the respective search term, and the language.


```r
get_search_url
```

```
## function (search.term, language = "de", domain = ".ch", quotes = TRUE,
##     n.pages = 1)
## {
##     search.term <- gsub(" ", "%20", search.term)
##     if (isTRUE(quotes))
##         search.term <- paste("%22", search.term, "%22", sep = "")
##     google.url <- paste("http://www.google", domain, "/search?",
##         "hl=", language, "&q=", search.term, sep = "")
##     if (n.pages > 1) {
##         return(c(google.url, paste0(google.url, "&ei=q-W9W-2MBoTCwALs5aPwBg&start=",
##             (1:(n.pages - 1)) * 10, "&sa=N")))
##     }
##     else {
##         return(google.url)
##     }
## }
```

### Get top n pages of hits

This function extracts all top links for the respective Google search, and either returns them in unabbreviated or stemmed form. Furthermore, it is possible to drop recursive links, i.e. links to Google image or Google maps results for the search term.


```r
get_google_hits
```

```
## function (google.url, raw = T, drop.recursives = F)
## {
##     doc <- getURL(URLencode(google.url), .opts = curlOptions(followlocation = TRUE,
##         cookiefile = "nosuchfile"))
##     html <- htmlTreeParse(doc, useInternalNodes = TRUE, error = function(...) {
##     })
##     nodes <- getNodeSet(html, "//h3[@class='r']//a")
##     raw.refs <- sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]])
##     if (isTRUE(drop.recursives))
##         raw.refs <- raw.refs[!grepl("\\/search\\?q\\=", raw.refs)]
##     if (isTRUE(raw)) {
##         return(raw.refs)
##     }
##     else {
##         clean.refs <- gsub("(\\/url\\?q\\=)|(\\&sa.*)", "", raw.refs) %>%
##             sapply(., function(x) URLdecode(URLdecode(x)))
##         return(clean.refs)
##     }
## }
```

## HTTP status checks

This function is definitely a bottleneck in the whole workflow. It's essentially a simple wrapper that uses `httr`'s `http_error()`-function to check for dead links and returns a tibble with both the Boolean value and the URL. It contains a timeout-catcher to prevent getting stuck in http collisions. All dead links are dropped (see workflow scheme above).


```r
check_status_code
```

```
## function (hits)
## {
##     if (is.list(hits))
##         hits <- unlist(hits)
##     hits.bool <- pbsapply(hits, function(x) {
##         res <- tryCatch({
##             withTimeout({
##                 !http_error(x)
##             }, timeout = 10)
##         }, TimeoutException = function(ex) {
##             NA
##         }, error = function(e) {
##             NA
##         })
##     }) %>% tibble(boolean = ., url = names(.))
##     return(hits.bool)
## }
```


## URL snowballing

### Snowballing algorithm

The snowballing algorithm first extracts all `href` elements from the parsed connection (URL), and checks whether the URL-trunks of the newly scraped links are problematic (either doesn't start with `http?(s)` or doesn't end with `\.html`); if so, the URL is dropped. Otherwise the URL is further checked for user-specified keywords (regex-optimized). This can be done by matching the keywords on the URL (`outer` matching), or by matching the keywords on both `title` and `p` HTML-elements (`inner` matching). Links that don't produce matches are dropped, all the others are returned.


```r
snowballer
```

```
## function (url, keywords = NULL, key.match = "outer")
## {
##     trunk <- str_extract(url, "^https?://[^/]+")
##     html <- paste(suppressWarnings(tryCatch({
##         readLines(url, skipNul = T)
##     }, error = function(e) {
##         e
##     })), collapse = "\n")
##     if (!suppressWarnings(grepl("Error", html))) {
##         matched <- str_match_all(html, "<a href=\"(.*?)\"")
##         links <- matched[[1]][, 2] %>% unique
##         links <- sapply(links, function(x) {
##             if (grepl("^/", x)) {
##                 return(paste0(trunk, x))
##             }
##             else {
##                 if (grepl("^#", x) | !grepl("(http?(s))|(\\.html)",
##                   x)) {
##                   return(NA)
##                 }
##                 else {
##                   return(x)
##                 }
##             }
##         }) %>% unname %>% na.omit
##         if (!is.null(keywords) & key.match == "outer")
##             links <- links[grepl(keywords, tolower(links))]
##         if (!is.null(keywords) & key.match == "inner") {
##             log <- sapply(links, function(x) {
##                 if (url.exists(x)) {
##                   html <- getURL(x, followlocation = TRUE)
##                   doc = htmlParse(html, asText = TRUE)
##                   queries <- c(title = "//title", text = "//p")
##                   plain.text <- xpathSApply(doc, queries, xmlValue)
##                   return(any(grepl(keywords, tolower(plain.text))))
##                 }
##                 else {
##                   return(NA)
##                 }
##             })
##             links <- links[ifelse(is.na(log), FALSE, log)]
##         }
##         return(links)
##     }
##     else {
##         return(NULL)
##     }
## }
```

### Cycler

The snowballing cycler is the driver that feeds all *new* and *unique* URLs to the snowballing algorithm and keeps a log file with with all URLs scraped in one iteration, i.e. stemming from the same root (single URL fed to the *cycler*). It includes a timeout parameter to prevent endless cycling.


```r
cycler
```

```
## function (start, time.limit, keywords, key.match = "outer")
## {
##     url_log <- NULL
##     url_list <- start
##     strt <- Sys.time()
##     while (any(!url_list %in% url_log)) {
##         url_log <- c(url_list[!url_list %in% url_log], url_log)
##         url_list <- tryCatch(lapply(url_list, function(x) {
##             res <- snowballer(x, keywords = keywords)
##             stp <- Sys.time()
##             if (difftime(stp, strt, units = c("secs")) > time.limit) {
##                 break
##             }
##             else {
##                 return(res)
##             }
##         }) %>% unlist %>% c(url_list, .) %>% unique, error = function(e) {
##             return(unique(c(url_list, url_log)))
##         })
##         stp <- Sys.time()
##         if (difftime(stp, strt, units = c("secs")) > time.limit)
##             break
##     }
##     return(url_list)
## }
```

## Text mining


### Text extraction

```r
extract_txt
```

```
## function (urls, merged = TRUE, add.queries = NULL, preproc.expr = NULL)
## {
##     if (is.list(urls))
##         url <- unlist(urls)
##     sapply(urls, function(s.url) {
##         html <- tryCatch(withTimeout({
##             getURL(s.url, followlocation = TRUE)
##         }, timeout = 10), TimeoutException = function(ex) {
##             NA
##         }, error = function(e) {
##             return(NA)
##         })
##         if (is.na(html))
##             return(NA)
##         doc <- tryCatch(withTimeout({
##             htmlParse(html, asText = TRUE)
##         }, timeout = 10), TimeoutException = function(ex) {
##             NA
##         }, error = function(e) {
##             return(NA)
##         })
##         if (is.na(doc))
##             return(NA)
##         queries <- c(title = "//title", text = "//p", add.queries)
##         plain.text <- tryCatch(withTimeout({
##             xpathSApply(doc, queries, xmlValue)
##         }, timeout = 10), TimeoutException = function(ex) {
##             NA
##         }, error = function(e) {
##             return(NA)
##         })
##         if (all(is.na(plain.text)))
##             return(NA)
##         plain.text <- gsub("(\\{.*\\}(\\.)?)|(^\\.$)", "", plain.text)
##         if (!is.null(preproc.expr))
##             plain.text <- plain.text[!grepl(preproc.expr, plain.text)]
##         if (isTRUE(merged)) {
##             return(paste(plain.text, collapse = "\n"))
##         }
##         else {
##             plain.text <- as.list(as.vector(plain.text))
##             return(plain.text)
##         }
##     })
## }
```

### Text burning
The text burning fuction has several parameters:

```
# raw:: if T: only returns annotated texts (tibble); if F: actually computes transformations
# hard.filter:: if grepl returns true on any of these elements, the respective line (== HTML-element) will be removed (default=NULL)
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
```


```r
clean_text
```

```
## function (txt, raw = F, hard.filter = NULL, buzzwords = NULL,
##     slicing = T, slicing.keywords = NULL, scnd.step.slicing = 3,
##     scnd.step.threshold = 4, min.words = 3, min.avg.characters = 3,
##     max.buzzwords = 2, recover.fs = 50)
## {
##     if (is.list(txt))
##         txt <- unlist(txt, recursive = F)
##     if (length(txt) %in% c(0, 1)) {
##         return(NA)
##     }
##     if (is.null(buzzwords))
##         buzzwords <- default.buzzwords
##     buzzwords <- paste0(c(buzzwords, default.buzzwords), collapse = "|")
##     if (is.null(slicing.keywords))
##         slicing.keywords <- default.slicing.keywords
##     slicing.keywords <- paste0(c(slicing.keywords, default.slicing.keywords),
##         collapse = "|")
##     txt <- iconv(enc2utf8(txt), "UTF-8", "ASCII")
##     txt_splt <- sapply(txt, function(string) strsplit(string,
##         " "))
##     txt <- tibble(txt = txt, n.words = lengths(txt_splt), nchar.words.mean = sapply(txt_splt,
##         function(x) mean(nchar(x), na.rm = T)), buzz.words = buzz_matches(unlist(txt),
##         buzzwords), n.buzzwords = lengths(sapply(strsplit(buzz.words,
##         ";"), function(x) if (any(is.na(x))) {
##         NULL
##     }
##     else {
##         x
##     })), id = paste0("id_", 1:length(n.buzzwords)))
##     txt_log <- txt
##     if (isTRUE(raw)) {
##         return(txt)
##     }
##     else {
##         if (isTRUE(slicing) & length(grep(slicing.keywords, txt$buzz.words)) >
##             0) {
##             if (any(table(txt$buzz.words, useNA = "no") > 1)) {
##                 txt <- txt[as.integer(seq_len(max(which(grepl(slicing.keywords,
##                   txt$buzz.words))) - 1)), ]
##             }
##             else {
##                 txt <- txt[as.integer(seq_len(min(which(grepl(slicing.keywords,
##                   txt$buzz.words))) - 1)), ]
##             }
##         }
##         txt <- filter(txt, !(n.words < min.words | nchar.words.mean <
##             min.avg.characters | n.buzzwords >= max.buzzwords))
##         if (!is.null(hard.filter))
##             txt <- filter(txt, !grepl(paste0(hard.filter, collapse = "|"),
##                 txt))
##         if (isTRUE(slicing) & sum(txt$n.buzzwords[round(nrow(txt) -
##             nrow(txt)/scnd.step.slicing, 0):nrow(txt)]) > scnd.step.threshold) {
##             temp <- txt[round(nrow(txt) - nrow(txt)/scnd.step.slicing,
##                 0):nrow(txt), ]
##             id <- temp$id[min(which(temp$n.buzzwords > 0))]
##             txt <- txt[as.integer(seq_len(min(which(txt$id ==
##                 id)) - 1)), ]
##         }
##         txt <- txt_log[(txt_log$id %in% txt$id) | (txt_log$n.words >=
##             recover.fs), ]
##         return(txt)
##     }
## }
```

The logic behind the burning:

```
- collect parameters
  x
  |_ split strings into word-based lists and create burning template; CONTAINS:
    |_ raw text
    |_ number of words per string
    |_ average number of characters per word / string
    |_ buzzword matches
    |_ number of buzzword matches
    |_ string id
      x
      |_IF isTRUE(raw):: return burning template
      |_ELSE
        x .. [[CONDITIONAL SLICING]]
        |_IF isTRUE(slicing) & there are slicing keywords in the documents::
          |_IF a buzzword exists more than once:: select index of last occurence as slicing point
          |_ELSE select index of the occurence as slicing point
        x .. [[SOFT FILTERING]]  
        |_ remove all strings for which isTRUE(n.words<min.words|nchar.words.mean<min.avg.characters|n.buzzwords>=max.buzzwords)
        x .. [[HARD FILTERING]]
        |_IF !is.null(hard.filer):: remove all strings for which isTRUE(grepl(paste0(hard.filter, collapse = '|'), ..))
        x .. [[SECOND STEP SLICING]]
        |_IF isTRUE(slicing) & the sum of all buzzwords in the user-defined tailing portion of the text (==scnd.step.slicing) exceeds the userdefined threshold (==scnd.step.threshold):: remove tail
```

Dependencies:


```r
buzz_matches
```

```
## function (char.list, buzzwords)
## {
##     matches <- stri_extract_all_regex(tolower(char.list), buzzwords)
##     matches <- sapply(matches, function(x) {
##         if (!any(is.na(x))) {
##             paste0(x, collapse = ";")
##         }
##         else {
##             x
##         }
##     })
##     return(matches)
## }
```

# Display Case

```r
library(XML)
library(RCurl)
library(pbapply)
library(dplyr)
library(stringi)
library(pbmcapply)
library(textcat)


rm(list=ls())

source('~/r-helpers/Google-search/Google-search-fx.R')
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
hits <- pbmclapply(search.url[1:10], function(x) get_Google_hits(x, raw=F, drop.recursives = T), mc.cores=4)

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
         keywords='no?(-)billag'
  )
}, mc.cores=3)

# save
# save(snow.balls, file=paste0('snowballing-res-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

unique(unlist(snow.balls))

# check them again
snow.balls.b <- check_status_code(snow.balls) %>% print(n=100)

# save(snow.balls.b, file=paste0('snowballing-res-checked-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
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
# save(txt, file=paste0('txt-extrctd-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
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
             buzzwords = buzzwords)
})

# save
# save(txt.clean, file=paste0('txt-cleaned-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
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
# save(txt.clean, file=paste0('txt-cleaned-noNAs-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
load(paste0('txt-cleaned-noNAs-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

# add language variable, merge texts, and convert them to true UTF-8
corp <- tibble(txt.burnt=txt.clean,
               language=pbsapply(txt.clean, function(x){textcat(paste0(x$txt, collapse = ' '))}),
               txt.merged=sapply(txt.clean, function(x){paste0(x$txt, collapse = '\n')}),
               txt.merged.utf8=iconv(txt.merged, from = "UTF-8", to = "MAC") %>% iconv(., from = "MAC", to="UTF-8"))
# save
# save(corp, file=paste0('corpus-', tolower(gsub('\\s+', '', search.term)), '.RDS'))
load(paste0('corpus-', tolower(gsub('\\s+', '', search.term)), '.RDS'))

# subset
corp.de <- filter(corp, language=='german')

# print texts
cat(corp.de$txt.merged.utf8)

# hardcopy of files from dropbox to sharefolder
system('cp -a ~/Dropbox/share/socsc1/output/ ~/share/socsc1/output-directLink/')
```
