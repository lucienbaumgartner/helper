#################################################################
# DeepL hack
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
source('~/hub/helper/r/selenium-hacks/selenium-hacks-fx.R')

#################################################################
# Language setting
#################################################################
set_lang <- function(user.text, driver, language = 'dynamic'){
  browser <- driver
  if(identical(language, 'dynamic')){lang <- names(tail(table(textcat(user.text)), n=1))}else{lang=language}

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
