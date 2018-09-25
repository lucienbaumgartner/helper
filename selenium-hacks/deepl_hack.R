## RSelenium is not available in CRAN right now, but we can install a legacy version if needed:
# library(devtools)
# install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
# install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
# install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")

rm(list=ls())
library(dplyr)
library(rvest)
library(stringr)
library(stringi)
library(textcat)
library(pbapply)
library(RSelenium)
library(pbmcapply)
library(reshape2)

## functions
# set language of text snippets (we only do this ONCE PER TEXT (== once per snippet batch))
set_lang <- function(user.text, driver){
  browser <- driver
  lang <- names(tail(table(textcat(user.text)), n=1))
  
  setlang <- NULL
  start.t <- Sys.time()
  while(is.null(setlang)){
    setlang <- tryCatch({browser$findElement(using='css', '.lmt__language_select__opener')}, 
                        error = function(e){NULL})
    stop.t <- Sys.time()
    if(stop.t-start.t>10){
      print('Could not find handle [.lmt__language_select__opener]')
      break
    }
  }
  
  if(is.null(setlang)){
    browser$close()
    browser$quit()
    stop()
  }
  
  tryclick <- try(setlang$clickElement(), silent=T)
  while(class(tryclick)=='try-error'){
    tryclick <- try(setlang$clickElement(), silent=T)
  }
  
  Sys.sleep(0.5)
  if(lang=='german') inplang <- browser$findElement(using='xpath', '//*[@id="dl_translator"]/div[1]/div[1]/div[1]/div/ul/li[3]')
  if(lang=='italian') inplang <- browser$findElement(using='xpath', '//*[@id="dl_translator"]/div[1]/div[1]/div[1]/div/ul/li[6]')
  if(lang=='french') inplang <- browser$findElement(using='xpath', '//*[@id="dl_translator"]/div[1]/div[1]/div[1]/div/ul/li[4]')
  
  tryclick <- try(inplang$clickElement(), silent=T)
  if(class(tryclick)=='try-error'){
    tryclick <- try(inplang$clickElement(), silent=T)
  }
  
  setting <- browser$findElement(using = 'xpath', '//*[@id="dl_translator"]/div[1]/div[1]/div[1]/div/label/strong')
  Sys.sleep(2)
  if(tolower(unlist(setting$getElementAttribute('innerHTML')))==lang){print('Language has correctly been set')}else{print('Problems with language setting')}
}

# upload snippet; dynamic waiting; text extraction
get_transl <- function(user.text, driver){
  browser <- driver
  input <- NULL
  start.t <- Sys.time()
  while(is.null(input)){
    input <- tryCatch({browser$findElement(using='css', '.lmt__source_textarea')}, 
             error = function(e){NULL})
    stop.t <- Sys.time()
    if(stop.t-start.t>10){
      print('Could not find handle [.lmt__source_textarea][1]')
      browser$close()
      browser$quit()
      break
    }
  }
 
  tryclick <- try(input$clickElement(), silent=T)
  start.t <- Sys.time()
  while(class(tryclick) == "try-error"){
    tryclick <- try(input$clickElement(), silent=T)
    stop.t <- Sys.time()
    if(stop.t-start.t>10){
      print('Could not find handle [.lmt__source_textarea][2]')
      browser$close()
      browser$quit()
      break
    }
  }
  
  input$sendKeysToElement(list(user.text))
  ## dynamic wait
  # there are several dynamic elements that can be used 
  busy <- browser$findElement(using = 'css', '.lmt__busy_indicator')
  while(grepl('active', unlist(busy$getElementAttribute('outerHTML')))){
    busy <- browser$findElement(using = 'css', '.lmt__busy_indicator')
  }
  trgt <- browser$findElement(using='css', ".lmt__translations_as_text__main_translation > span:nth-child(1)")
  clsf <- trgt$getElementAttribute('innerHTML')
  input$clearElement()
  return(clsf)
  
}

# start browser
SelRun <- function(portN=4444, startpage='https://www.google.com/', browser='chrome', timeout=F, test=F){
  browser <- remoteDriver(port = portN, browserName = browser)
  browser$open()
  if(isTRUE(timeout)) browser$setTimeout(type = 'page load', milliseconds = 10000)
  browser$navigate(startpage)
  
  url <- browser$getCurrentUrl()
  while(!identical(unlist(url), startpage)){
    browser$navigate(startpage)
    url <- browser$getCurrentUrl()
  }
  if(identical(unlist(url), startpage)&!isTRUE(test)) print('Page successfully loaded')
  
  if(isTRUE(test)){
    browser$close()
    browser$quit()
    print('Test successful')
  }
  
  if(!isTRUE(test)) return(browser)
}

# get source function
get_sourcetxt <- function(Source){
  # some preprocessing
  txt_temp <- read_html(Source) %>% 
    html_node('#gutenb') %>% 
    html_text() %>% 
    gsub('\\n|\\s{2}', '', .)
  return(txt_temp)
}

# fallada batching
# the Fallada was developed using this snippet 'http://gutenberg.spiegel.de/buch/anton-und-gerda-10066/2', the gate to a regex-hell

fallada <- function(usr.text, batchsize){
  container <- list()
  fit <- substr(usr.text, 1, batchsize) 
  
  for(i in 1:(round(nchar(usr.text)/batchsize, 0)+20)){
    
    container[[i]] <- fit %>% 
      gsub('\\.\\.\\.', '\\C-x', .) %>% 
      strsplit(., '(?:(?<=[?!.]\\«\\s)|(?<=[?!.]\\s))', perl=T) %>% 
      unlist(., recursive = F) %>%  
      .[length(.)] %>% 
      paste0(., '$') %>% 
      gsub('\\C-x', '\\.\\.\\.', .) %>%
      gsub('*', '\\*', .) %>% 
      gsub(., '', fit) 
    
    if(container[[i]]=='') container[[i]] <- fit
    
    ptrn <- gsub('([[:punct:]])', '\\\\\\1', container[[i]]) %>% 
      str_sub(., start= -30)
    
    fit <- gsub(paste0('.*', ptrn), '', usr.text) %>% 
      substr(., 1, batchsize)
    
    if(fit=='') break
  }
  return(container)
} 


# list or vector of sources
urls <- c('http://gutenberg.spiegel.de/buch/das-blaue-buch-von-vaterland-und-freiheit-7448/5',
          'http://gutenberg.spiegel.de/buch/die-befreiungskriege-1813-bis-1815-8993/3',
          'http://gutenberg.spiegel.de/buch/die-8-henna-legenden-9069/3',
          'http://gutenberg.spiegel.de/buch/aus-dem-bohmerwalde-10108/2',
          'http://gutenberg.spiegel.de/buch/ein-abenteuer-mit-jenny-lind-5815/1',
          'http://gutenberg.spiegel.de/buch/bekenntnisse-eines-englischen-opiumessers-2024/1',
          'http://gutenberg.spiegel.de/buch/bekenntnisse-eines-englischen-opiumessers-2024/2',
          'http://gutenberg.spiegel.de/buch/bekenntnisse-eines-englischen-opiumessers-2024/3',
          'http://gutenberg.spiegel.de/buch/bekenntnisse-eines-englischen-opiumessers-2024/4',
          'http://gutenberg.spiegel.de/buch/bekenntnisse-eines-englischen-opiumessers-2024/5')

#
SelRun(startpage = 'https://www.deepl.com/translator', timeout = T, test = T)

p <- pblapply(urls, function(lru){
  lala <- get_sourcetxt(lru)
  final <- fallada(lala, 5000)
  return(final)
})

p

txt_t <- pblapply(urls, function(urls){
  txt <- get_sourcetxt(urls)
  txt_batch <- fallada(txt, 5000)
  
  fbatch <- txt_batch %>% unlist %>% paste0(., collapse='')
  
  print('Batching Done')
  
  if(!identical(nchar(fbatch), nchar(txt))){return('Batching Problem')}else{
    
    browser <- SelRun(startpage = 'https://www.deepl.com/translator', timeout = T)
    
    Sys.sleep(5)
    set_lang(user.text = txt_batch, driver = browser)
    res <- tryCatch({pblapply(txt_batch[1:4], function(x) get_transl(user.text = unlist(x), driver = browser))}, 
                    error = function(e) return('Translation Problem'))
    res <- paste0(unlist(res), collapse=' ') %>% gsub('\\s{2,}', ' ', .)
    names(res) <- urls
    browser$close()
    browser$quit()
    return(res)
  }
  
}) 










###### dump
txt_t

result <- tryCatch({
  suppressMessages({
    webElem <- remDr$findElement('xpath', "//tr[(((count(preceding-sibling::*) + 1) = 9) and parent::*)]//span[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]")
    rank1_US <- webElem$getElementText() %>% unlist(.) %>% ifelse(length(.) == 0, NA, .)
    rank1_US
  })
}, 
error = function(e) {
  NA_character_
}
)


txt <- read_html(urls[1]) %>% 
  html_node('#gutenb') %>% 
  html_text() %>% 
  gsub('\\n|\\s{2}', '', .)

cont <- list()
fit <- substr(txt, 1, 5000) 
fit 
for(i in 1:(round(nchar(txt)/5000, 0)+20)){
  
  cont[[i]] <- fit %>% 
    gsub('\\.\\.\\.', '\\C-x', .) %>% 
    strsplit(., '(?:(?<=[?!.]\\«\\s)|(?<=[?!.]\\s))', perl=T) %>% 
    unlist(., recursive = F) %>%  
    .[length(.)] %>% 
    paste0(., '$') %>% 
    gsub('\\C-x', '\\.\\.\\.', .) %>% 
    gsub(., '', fit)
  
  if(cont[[i]]=='') cont[[i]] <- fit
  
  ptrn <- gsub('([[:punct:]])', '\\\\\\1', cont[[i]]) %>% 
    str_sub(., start= -30)
  
  fit <- gsub(paste0('.*', ptrn), '', txt) %>% 
    substr(., 1, 5000)
  
  if(fit=='') break
  
}

fbatch <- cont %>% unlist %>% paste0(., collapse='')
nchar(fbatch)==nchar(txt)
cont