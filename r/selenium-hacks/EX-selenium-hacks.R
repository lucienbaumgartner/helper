#################################################################
# DeepL hack EXAMPLE
#################################################################
# Content
#################################################################
# Dependencies
# Load data and Selenium driver
# Translate documents
#################################################################

rm(list = ls())

#################################################################
# Dependencies
#################################################################
# global
library(httr)
library(dplyr)
library(pbapply)
library(pbmcapply)
library(stringr)
library(RSelenium)
library(textcat)
library(wdman)
# local
source('~/hub/helper/r/selenium-hacks/deepl-hacks-fx.R')
source('~/hub/helper/r/selenium-hacks/selenium-hacks-fx.R')
source('~/hub/helper/r/text-analysis/text-batching-fx.R')

#################################################################
# Load data and Selenium driver
#################################################################
load('~/../../some-data.RData')
cDrv <- chrome(port=4567L)
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

#################################################################
# Translate documents
#################################################################
setwd('~/../../res/')
# TEST opening the webpage
SelRun(startpage = 'https://www.deepl.com/translator', timeout = 20000, test = T, browser = 'chrome', portN = 4567L, extraCapabilities = eCaps)
# specify sequence of documents that will constitute a makro-batch
# a makro-batch will be processed together and save in a single object
# makro-batching is used to secure already translated documents in case of a loop abortion and prevent single file saving
for(i in c((seq(1, nrow(df), 10)+1))){
  batch <- i:(i+9)
  
  print(batch)
  
  system.time(
    # apply translation function over a makro-batch
    res <- lapply(batch, function(index){
      txt <- df$`ht+body`[index]
      Encoding(txt) <- 'UTF-8'
      # if a single document in a makro-batch is longer than 5000 characters, batch it 
      if(nchar(txt) >= 5000){
        txt_batch <- batch_text(txt, 5000) %>% 
          lapply(., function(x) gsub('&', '+', x))
      }else{txt_batch <- list(txt)}
      
      fbatch <- txt_batch %>% unlist %>% paste0(., collapse='')
      
      print('Batching Done')

      if(!identical(nchar(fbatch), nchar(txt))){return('Batching Problem')}else{
        # open the browser for real
        browser <- SelRun(startpage = 'https://www.deepl.com/translator', timeout = 20000, portN = 4567L, extraCapabilities = eCaps)
        
        Sys.sleep(5)
        # set source language
        set_lang(user.text = txt_batch, driver = browser, language = 'german')
        # translate documents
        res <- tryCatch({lapply(txt_batch, function(x) get_transl(user.text = unlist(x), driver = browser))}, 
                        error = function(e) return('Translation Problem'))
        # collapse batch to single document
        res <- paste0(unlist(res), collapse=' ') %>% gsub('\\s{2,}', ' ', .)
        # add the index of the document as name
        names(res) <- df$id[index]
        browser$close()
        browser$quit()
        # close session
        return(res)
      }
      
    }) 
  )
  # save batch
  save(file = paste0('some-prefix', min(batch), '_', max(batch), '_HACKY-REMOTE.RData'), res)
}

cDrv$stop()
