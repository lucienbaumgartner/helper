modus <- function(v, na.rm=T) {
  if(isTRUE(na.rm)){
    uniqv <- unique(v)[!is.na(unique(v))]
    uniqv[which.max(tabulate(match(v[!is.na(v)], uniqv)))]
  }else{
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
}
