check_status_code <- function(hots){
  
  if(is.list(hits)) hits <- unlist(hits)
  
  hits.bool <- pbsapply(hits, url.exists) %>% 
    tibble(boolean=., url=names(.))
  return(hits.bool)
}