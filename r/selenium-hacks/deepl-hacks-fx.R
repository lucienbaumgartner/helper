#################################################################
# DeepL hack
#################################################################
# An example of how the functions can be used is provided @ 
# https://github.com/lucienbaumgartner/helper/blob/master/r/selenium-hacks/EX-selenium-hacks.R 
#################################################################
# Content
#################################################################
# Dependencies
# Language setting
# Upload and download fx (== translation)
#################################################################

#################################################################
# Dependencies
#################################################################
# global
library(dplyr)
library(rvest)
library(stringr)
library(stringi)
library(textcat)
library(pbapply)
library(RSelenium)
library(pbmcapply)
library(reshape2)
# local (needed for de facto execution)
#source('~/hub/helper/r/selenium-hacks/selenium-hacks-fx.R')
#source('~/hub/helper/r/text-analysis/text-batching-fx.R')

#################################################################
# Language setting
#################################################################
# parameter:
## user.text:: the text to be translated, batched
## driver:: the active Selenium driver object (not as character!)
## language:: how SOURCE language should be set: dynamically using textcat, or static by providing the language as a string ('german', 'italian', 'french')
# the target language is always english!
set_lang <- function(user.text, driver, language = 'dynamic'){
  # define driver
  browser <- driver
  # set language
  if(identical(language, 'dynamic')){lang <- names(tail(table(textcat(user.text)), n=1))}else{lang=language}
  # try to get a hold of the language setting element on the page; there is a timout set to 10s per default
  # this could easily be parametrized if needed
  # logic: as long as :setlang: is null, which it is as long as catching the handle returns an unavailability error, we try to catch it again, over and over
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
  # if the handle is still not available after the timout, we quit the Selenium session
  if(is.null(setlang)){
    browser$close()
    browser$quit()
    stop()
  }
  # if the handle could be located, we try to click the handle
  tryclick <- try(setlang$clickElement(), silent=T)
  while(class(tryclick)=='try-error'){
    tryclick <- try(setlang$clickElement(), silent=T)
  }
  # depending on the language that has to be set, we choose the corresponding sub-handle
  Sys.sleep(0.5)
  if(lang=='german') inplang <- browser$findElement(using='xpath', '//*[@id="dl_translator"]/div[1]/div[1]/div[1]/div/ul/li[3]')
  if(lang=='italian') inplang <- browser$findElement(using='xpath', '//*[@id="dl_translator"]/div[1]/div[1]/div[1]/div/ul/li[6]')
  if(lang=='french') inplang <- browser$findElement(using='xpath', '//*[@id="dl_translator"]/div[1]/div[1]/div[1]/div/ul/li[4]')
  
  tryclick <- try(inplang$clickElement(), silent=T)
  if(class(tryclick)=='try-error'){
    tryclick <- try(inplang$clickElement(), silent=T)
  }
  # check whether language has correctly been set or not
  setting <- browser$findElement(using = 'xpath', '//*[@id="dl_translator"]/div[1]/div[1]/div[1]/div/label/strong')
  Sys.sleep(2)
  if(tolower(unlist(setting$getElementAttribute('innerHTML')))==lang){print('Language has correctly been set')}else{print('Problems with language setting')}
}

#################################################################
# Upload and download fx (== translation)
#################################################################
# parameter:
## user.text:: the text to be translated, batched
## driver:: the active Selenium driver object (not as character!)
get_transl <- function(user.text, driver){
  browser <- driver # set driver
  # try to get a hold of the text input element on the page; there is a timout set to 10s per default
  # this could easily be parametrized if needed
  # logic: as long as :input: is null, which it is as long as catching the handle returns an unavailability error, we try to catch it again, over and over
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
  # if the handle could be located, we try to click the handle, if timeout, abort session
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
  # send text to source container
  input$sendKeysToElement(list(user.text))
  # dynamic wait loop
  # we have to wait for the translation to be processed in order to extract it from the output container
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