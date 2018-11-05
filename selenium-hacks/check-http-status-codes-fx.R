library(R.utils)

check_status_code <- function(hits){
  
  if(is.list(hits)) hits <- unlist(hits)
  
  hits.bool <- pbsapply(hits, function(x){
    tryCatch(
      {res <- withTimeout({url.exists(x)}, timeout = 10)},
      TimeoutException = function(ex){NA}
      ) 
  }) %>% 
    tibble(boolean=., url=names(.))
  return(hits.bool)
}
