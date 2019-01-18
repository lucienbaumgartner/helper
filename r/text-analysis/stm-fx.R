runSTM <- function(k, corpus, formula_prev, n_words, lang, outname.root) { #formula_cont, 
  #k <- as.numeric(unlist(strsplit(param, "-"))[1])
  #alpha <- as.numeric(unlist(strsplit(param, "-"))[2])
  #eta <- as.numeric(unlist(strsplit(param, "-"))[3])
  STM <- stm(corpus$documents, corpus$vocab, K=k, 
             as.formula(formula_prev), #as.formula(formula_cont),
             data = corpus$meta, init.type="Spectral", verbose=T, 
             control = list(nits = 100, burnin = 25)) #, alpha = alpha, eta = eta))
  words <- labelTopics(STM, n = n_words)
  words <- as.data.frame(words[[1]])
  #outname <- paste0("./results/res/", k, ".txt") # ORDNER ANPASSEN
  outname <- paste0(outname.root, k, ".txt") # ORDNER ANPASSEN
  write.table(words, outname, col.names = F, row.names = F, sep = "\t", quote = F)
  return(STM)
}

#A James-Stein Estimator Shrinking to a Uniform Distribution
#This draws from the Hausser and Strimmer (2009) JMLR piece.
js.estimate <- function(prob, ct) {
  if(ct<=1) {
    #basically if we only observe a count of 1
    #the variance goes to infinity and we get the uniform distribution.
    return(rep(1/length(prob), length(prob)))
  }
  # MLE of prob estimate
  mlvar <- prob*(1-prob)/(ct-1)
  unif <- rep(1/length(prob), length(prob)) 
  
  # Deviation from uniform
  deviation <- sum((prob-unif)^2)
  
  #take care of special case,if no difference it doesn't matter
  if(deviation==0) return(prob)
  
  lambda <- sum(mlvar)/deviation
  #if despite  our best efforts we ended up with an NaN number-just return the uniform distribution.
  if(is.nan(lambda)) return(unif)
  
  #truncate
  if(lambda>1) lambda <- 1
  if(lambda<0) lambda <- 0
  
  #Construct shrinkage estimator as convex combination of the two
  lambda*unif + (1 - lambda)*prob
}

col.lse <- function(mat) {
  colLogSumExps(mat)
}

safelog <- function(x) {
  out <- log(x)
  out[which(out< -1000)] <- -1000
  out
}