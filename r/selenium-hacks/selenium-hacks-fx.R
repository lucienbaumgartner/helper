SelRun <- function(portN=4444L, startpage='https://www.google.com/', browser='chrome', timeout=F, test=F, headless=F){
  if(headless){
    eCaps <- list(chromeOptions = list(
      args = c('--headless', '--disable-gpu', '--window-size=1280,800')
       , binary = "/Users/lucienbaumgartner/Selenium/chromedriver.exec"
    ))
    nap <- remoteDriver(port = portN, browserName = browser, extraCapabilities=eCaps)
    nap$open()
    dr <- rsDriver(port = portN, browser = browser, extraCapabilities=eCaps, check=FALSE, geckover = NULL, iedrver = NULL, phantomver = NULL)
  }else{
    # browser <- remoteDriver(port = portN, browserName = browser)
    dr <- rsDriver(port = portN, browser = browser, check=FALSE, geckover = NULL, iedrver = NULL, phantomver = NULL, verbose=FALSE)
  }
  
  browser <- dr$client
  browser$open()
  if(isTRUE(timeout)) browser$setTimeout(type = 'page load', milliseconds = 100000)
  browser$navigate(startpage)
  
  url <- browser$getCurrentUrl()
  while(!identical(unlist(url), startpage)){
    browser$navigate(startpage)
    url <- browser$getCurrentUrl()
  }
  if(identical(unlist(url), startpage)&!isTRUE(test)) print('Page successfully loaded')
  
  if(test){
    browser$close()
    browser$quit()
    server$server$stop()
    print('Test successful')
  }
  
  # if(!test) return(browser)
  if(!test) return(list(browser, server[['server']]))
}
