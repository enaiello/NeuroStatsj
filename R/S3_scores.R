
## this is modified version of facchin et al https://pmc.ncbi.nlm.nih.gov/articles/PMC9385822/

es_percentiles <- function(x,worse, what="cutoff") UseMethod(".es_perc")

.es_perc.default <- function(x,worse, what="cutoff") {

    if (worse=="low") i<-3 else i<-4
    
    x<-x[order(x,decreasing=FALSE)]
    OTL <- tolerance::nptol.int(x,0.05,0.95,1,method="WILKS")[[i]]
    ITL <- tolerance::nptol.int(x,0.95,0.95,1,method="WILKS")[[i]]

   #Find the position of OTL
    pES <- which.min(abs(x - OTL))

    #Position of Median
    pES[4] <- ceiling((length(x)+1)/2)

    #Range
    step <- floor((pES[4] - pES[1])/3) 
    pES[2] <- pES[1]+step
    pES[3] <- pES[1]+2*step
    

    covalues<-sort(x,decreasing = F)[pES]
    
    values<-c(covalues,ITL,OTL)
    names(values)<-c("es1","es2","es3","es4","itl","otl")
    values$method<-"Percentiles"
    return(values)
    
   
}




es_normal <- function(x,worse="low",conf_level=.95, dist="binom") UseMethod(".es_norm")

.es_norm.default <- function(x,worse, conf_level=.95, dist="binom") {


  decreasing <- (worse == "high")
  norm_scores <- sort(x, decreasing = decreasing)
  n <- length(norm_scores)
  
  # Compute OTL using qbinom()
  alpha <- 1 - conf_level
  otl<-NULL
  if (dist=="binom") {
    method="Binomial"
    k <- qbinom(alpha, size = n, prob = 1 / n)
    k <- max(1, k)
    otl <- norm_scores[k]
  }
  if (dist=="beta") {
      method="Beta"
      k_beta <- floor(n * qbeta(alpha, 1, n)) + 1
      k <- max(1, min(k_beta, n))
      otl <- norm_scores[k]
  }
  if (is.null(otl)) stop("No distribution defined for OTL")
  
  itl_index <- which(
    if (worse == "low") norm_scores > otl else norm_scores < otl
  )[1]
  if (is.na(itl_index)) stop("No ITL found beyond OTL.")
  itl <- norm_scores[itl_index]

  # Valid scores for ES > 0
  if (worse == "low") {
    valid_scores <- norm_scores[norm_scores > otl]
    p<-c(0.25, 0.5, 0.75, 1)
    mfun<-min
    } else {
    valid_scores <- norm_scores[norm_scores < otl]
    p<-c(0.75, 0.5, .25, 0)
    mfun<-max
      }
  
  if (length(valid_scores) < 4) {
    stop("Not enough valid data beyond OTL to compute ES cutoffs.")
  }

  
  # ES cutoffs: upper bounds for ES = 1, 2, 3
  covalues <- quantile(valid_scores, probs = p, type = 1)

  values<-c(covalues,itl,otl)
  names(values)<-c("es1","es2","es3","es4","itl","otl")
  values$method<-method
  return(values)  

}


fix_es <- function(cutoffs_df, worse = c("low", "high")) {
  worse <- match.arg(worse)
  
  fmt <- function(x) formatC(x, format = "f", digits = 3)
  
  for (i in 1:ncol(cutoffs_df)) cutoffs_df[[i]]<-unlist(cutoffs_df[[i]])
  # Preallocate result matrix
  n <- nrow(cutoffs_df)
  out <- list()

  for (i in seq_len(n)) {
    x <- cutoffs_df[i, ]
    es1 <- x$es1
    es2 <- x$es2
    es3 <- x$es3
    es4 <- x$es4
    otl <- x$otl
    itl <- x$itl
    
    if (worse == "high") {
      ch1<-"&ge;"
      ch2<- "&lt;"
      op <- -1
    } else {
      op <- 1
      ch1<-"&le;"
      ch2<- "&gt;"
      
    }
    
    get_max_digits <- function(x) {
      x_str <- format(x, scientific = FALSE, trim = TRUE)
      decs <- function(s) if (grepl("\\.", s)) nchar(sub("^[^.]*\\.", "", s)) else 0
      min(3,max(vapply(x_str, decs, numeric(1))))
  }

    digits <- get_max_digits(c(es1, es2, es3, es4, otl, itl))
    inc <- 10^(-digits)
      ranges <- list(
        es0=paste0(ch1, fmt(otl)),
        es1=paste(fmt(otl+op*inc), fmt(es1), sep = "–"),
        es2=paste(fmt(es1 + op*inc ), fmt(es2), sep = "–"),
        es3=paste(fmt(es2 + op*inc ), fmt(es3), sep = "–"),
        es4=paste0(ch2, fmt(es3)),
        otl=otl,
        itl=itl
      )
    
    out[[i]] <- ranges
  }
  
  return(out)
}
