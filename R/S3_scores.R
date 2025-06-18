
## this is modified version of facchin et al https://pmc.ncbi.nlm.nih.gov/articles/PMC9385822/

es_percentiles <- function(x,worse) UseMethod(".es_perc")

.es_perc.default <- function(x,worse) {

    if (worse=="low") i<-3 else i<-4
    
    x<-x[order(x,decreasing=FALSE)]
    method<-"WILKS"
    OTL <- tolerance::nptol.int(x,0.05,0.95,1,method=method)[[i]]
    ITL <- tolerance::nptol.int(x,0.95,0.95,1,method=method)[[i]]

   #Find the position of OTL
    values<-numeric(4)
    values[1] <- which.min(abs(x - OTL))

    #Position of Median
    values[4] <- ceiling((length(x)+1)/2)

    #Range
    step <- floor((values[4] - values[1])/3) 
    values[2] <- values[1]+step
    values[3] <- values[1]+2*step
    
    covalues<-sort(x,decreasing = F)[values]
    values<-c(covalues,ITL,OTL)
    names(values)<-c("es0","es1","es2","es3","itl","otl")
    values$method<-"Percentiles"
    return(values)
    
   
}




es_normal <- function(x,worse="low",conf_level=.95) UseMethod(".es_norm")

.es_norm.default <- function(x,worse, conf_level=.95) {

    if (worse=="low") {
        i<-3 
        op <- 1
      }  else {
        i<-4
        op <- -1
      }
    
    x<-x[order(x,decreasing=FALSE)]
    method<-"WALD"
    OTL <- tolerance::nptol.int(x,0.05,0.95,1,method=method)[[i]]
    ITL <- tolerance::nptol.int(x,0.95,0.95,1,method=method)[[i]]
    M <- median(x)
   #Find the position of OTL

    values<-numeric(4)
    values[1]<-OTL
    #Range
    step <- abs(OTL - M)/3 
    values[2] <- OTL+op*step
    values[3] <- OTL+2*op*step
    values[4] <- M
    values<-c(values,ITL,OTL)
    names(values)<-c("es0","es1","es2","es3","itl","otl")
   
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
    es0 <- x$es0
    es1 <- x$es1
    es2 <- x$es2
    es3 <- x$es3
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

    digits <- get_max_digits(c(es0, es1, es2, es3, otl, itl))
    inc <- 10^(-digits)
      ranges <- list(
        es0=paste0(ch1, fmt(es0)),
        es1=paste(fmt(es0+op*inc), fmt(es1), sep = "–"),
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



