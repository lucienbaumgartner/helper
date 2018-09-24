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