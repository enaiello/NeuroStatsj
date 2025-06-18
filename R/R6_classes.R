Adjuster <- R6::R6Class("Adjuster",
  inherit = Scaffold,
  public = list(
  
    perc_type=NULL,
    which_worse ="high",    
    model_data=NULL,
    adjust=function() {
      
      if (is.null(self$model)) stop("Please pass a model before adjusting")
      model<-self$model
      if (length(model$coefficients)==1) return(model.response(model.frame(model)))
      dep<-stats::formula(model)[[2]]
      terms<-colnames(attr(terms(model),"factors"))
      mm<-model.matrix(model)
      mm<-mm[,attr(mm,"assign")>0]
      self$model_data<-mm
      for (n in colnames(mm)) {
         if (length(unique(mm[,n]))==2) {
            mm[,n]<-as.numeric(as.character(mm[,n]))
         }
         else mm[,n]<-as.numeric(scale(mm[,n],scale=FALSE))
      }

      coefs<-coef(model)[-1]
      coefs<-round(coefs,digits=4)
      preds<-  as.matrix(mm) %*% coefs
      preds<-  private$.link_inv(preds)
      op<-private$.adj_fun$op
      score<-op(model$model[[dep]],preds)
      return(as.numeric(score))
    },
    percentiles=function() {
      if (is.null(self$perc_type))
          p<- seq(0.01, .99, by = 0.01)
      else
          p <- self$perc_type/100
      values<-self$adjust()
      ps<-data.frame(val=quantile(values, probs = p))
      ps$perc<-rownames(ps)
      rownames(ps)<-NULL
      if (self$which_worse=="higher") {
        ps<-ps[order(ps$val,decreasing=TRUE),]
        ps$perc<-rev(ps$perc)
        rownames(ps)<-NULL
      }
      ps
      
    },
    
    es_perc=function() {
        values<-self$adjust()
        es_percentiles(values,worse=self$which_worse)

    },
    es_zscore=function() {
        values<-self$adjust()
        es_normal(values,worse=self$which_worse)
    },
    es_default= function() {
      
      if (self$option("zscore"))
          return(self$es_class("es_zscore"))
      else
          return(self$es_class("es_rank"))
            
    },
    es_class= function(what) {
      
       worse <- self$which_worse
       
       if (what == "zscore")
           es    <- self$es_zscore()
       else
           es    <- self$es_perc()
  
      es0 <- es[1]
      es1 <- es[2]
      es2 <- es[3]
      es3 <- es[4]
      
      scores<-self$adjust()
      out <- numeric(length(scores))
      
     

    if (worse == "low") {
      out[scores <= es0] <- 0
      out[scores > es0 & scores <= es1] <- 1
      out[scores > es1 & scores <= es2] <- 2
      out[scores > es2 & scores <= es3] <- 3
      out[scores > es3] <- 4

    } else {
      out[scores >= es0] <- 0
      out[scores < es0 & scores >= es1] <- 1
      out[scores < es1 & scores >= es2] <- 2
      out[scores < es2 & scores >= es3] <- 3
      out[scores < es3] <- 4
    }
    out <- factor(paste0("ES", out), levels = paste0("ES", 0:4))
    return(data.frame(score=scores,es=out))  

   }


    
  ), ## end of public
  active= list(
  
    
    model = function(model) {
          
      if (missing(model)) {
        return(private$.model)
      }  
      private$.model<-model
      private$.model_class <- class(model)
      private$.family      <- family(model)
      private$.link_fun    <- private$.family$linkfun
      private$.link_inv    <- private$.family$linkinv
      private$.adj_fun     <- adj_fun(model)
      
    } , 
    model_class=function(value) {
      
      if (missing(value)) {
        return(private$.model_class)
      } else {
        private$.model_class<-value
      }
      
    },
    link_fun=function(value) {
      
      if (missing(value)) {
        return(private$.link_fun)
      } else {
           private$.link_fun<-value
      }
      
    },
    link_inv=function(value) {
      
      if (missing(value)) {
        return(private$.link_inv)
      } else {
           private$.link_inv<-value
      }
      
    },
    adj_fun=function(value) {
      
      if (missing(value)) {
        return(private$.adj_fun)
      } else {
           private$.adj_fun<-value
      }
    }

  ), # end of active
  private=list(
    
    .model = NULL,
    .model_class = NULL,
    .family=NULL,
    .link_fun=NULL,
    .link_inv=NULL,
    .adj_fun=NULL
    
  )
)


Selector <- R6::R6Class("Selector",
                    
  public = list(
    model      = NULL,
    model_type = NULL,
    dep        = NULL,
    adjuster   = NULL,
    which_worse  = "high",
    best_transf=list(),
    selected   = list(),
    included   = NULL,
    stepwise    = NULL,
    initialize = function(data=NULL) {
      self$data <- data
      self$model_fun<-lm
    },
    
    univariate = function() { 
      
         df<-data.frame(name=NA,fun=NA,id=NA,type=NA)
         tabname<-lapply(self$
                           
                           covs,function(x) lapply(x$transformations, function(z) z$name))
         tabid<-lapply(self$covs,function(x) lapply(x$transformations, function(z) z$id))
       
         cols<-names(tabname)
         for (i in seq_along(tabname)) {
           for (j in seq_along(unique(tabname[[i]]))) {
                   x<-j+i-1
                   df[x,]<-c(cols[i],tabname[[i]][j],tabid[[i]][j],"covariate")
           }
         }
         for (f in self$factors) {
           x<-x+1
           df[x,]<-c(f$name,fun="None",id="none",type="factor")
         }
         if (is.something(self$data) && nrow(self$data)> 0) {
           
           private$.maketerms(df)
           df<-private$.testuniv(df)     
           
         }
         private$.univariate_tab<-df  
         df
         },

    find_best = function() {

      if (is.null(private$.univariate_tab)) stop("Please run univariate tests first")
      
      vars<-c(names(private$.covs),names(private$.factors))
      data<-private$.univariate_tab
      
      crit<-"r2"
      fun<-which.max
      if (any(is.na(data$r2))) {
        crit="aic"
        fun<-which.min
      }
      
      tab<-lapply(vars,function(x) {
         .data<-data[data$name==x,]
         .max<-fun(.data[[crit]])
         list(name=x,id=.data[.max,"id"], var=paste0(x,"_",.data[.max,"id"]),type=.data[.max,"type"])
         
      })
      names(tab)<-vars
      self$best_transf<-tab

    },
    multiple=function() {
      
      if (is.null(self$dep)) stop("The outcome variable  must be defined")
      if (is.null(self$best_transf)) stop("The covariates variable  must be selected")
      vars<-lapply(self$best_transf,function(x) x$var)
    
      form<-jmvcore::composeFormula(self$dep,vars)
      opts<-c(self$opts,list(formula=form,data=private$.data))
      self$model<-do.call(self$model_fun,opts)
      ss<-coefficients_table(self$model)
      ss$fun<-unlist(lapply(self$best_transf,function(x) TRANSFUN[[x$id]]$name))
      return(ss)
      
    },
    select = function() {    
      
      vars<-lapply(self$best_transf,function(x) x$var)
      form<-as.formula(jmvcore::composeFormula(self$dep,vars))
      opts<-self$opts
      opts[["formula"]]<-form
      opts[["data"]]<-private$.data
   
      model<-do.call(self$model_fun,opts)
      
      model<-fix_call(model)
      scope<-list(lower=~1, upper=form)
      if (is.something(self$included)) {
        included<-self$best_transf[self$included]
        included<-unlist(lapply(included,function(x) x$var))
        scope<-list(lower=as.formula(jmvcore::composeFormula(NULL,included)), upper=form)
      }

      steps<-MASS::stepAIC(model,direction="both",trace=0, scope=scope)
      self$model<-steps
      if (length(self$model$coefficients)==1)
         return()
      res<-as.data.frame(summary(steps)$coefficients)[-1,]
      names(res)<-c("estimate","se","test","p")
      res$df<-stats::df.residual(steps)
      res$rowname<-rownames(res)

      res$name<-NA
      res$fun<-NA
      for (var in self$best_transf) {
        res$name[res$rowname==var$var]<-var$name
        res$fun[res$rowname==var$var]<-TRANSFUN[[var$id]]$name
        if (any(res$rowname==var$var)) self$selected[[var$name]]<-var
      }
      self$stepwise<-res
 
      return(res)

    },
    
    formulate=function() {
      
      if (is.null(self$model)) stop("Please estimate the model first.")
      make_formula(self$model,self)
      
    },
    pretty_formulate=function() {
      form<-self$formulate()
      text<-paste("<h2> Adjustment formula</h2>","<p><b>",form,"</b></p>")
      for (var in self$selected) {
        if (var$type=="factor") {
          cnt<-contrasts(self$data[[var$name]])
          atext<-  paste("<p>Variable", var$name, "is coded:", paste(rownames(cnt),cnt,sep=" = ",collapse=", "))
          text<-paste(text,atext)
        }
      }
    text
      
    }

    


  ), ## end of public
  active= list(
    
    opts=function(alist) {
      
      if (missing(alist)) {
        return(private$.opts)
      } else {
        lapply(names(alist),function(x) private$.opts[[x]]<-alist[[x]] )
      }
    },
    covs=function(alist) {
      
      if (missing(alist)) {
        return(private$.covs)
      } else {
        private$.covs_info<-alist
        lapply(alist,function(x) {
           private$.covs[[x$name]]$name<-x$name 
           if(is.null(x$forced) || isFALSE(x$forced)) private$.covs[[x$name]]$transformations<-private$.transformations
           else private$.covs[[x$name]]$transformations<-list(TRANSFUN[[x$forced]])
           })
         
#        lapply(alist,function(x) private$.covs[[x$name]]$mean<-mean(self$data[[x$name]],na.rm=T) )
        names(private$.covs)<-sapply(alist,function(x) x$name )      
        self$included<-c(self$included,unlist(lapply(alist, function(x) if (hasName(x,"include") && x$include==TRUE) x$name else NULL)))
        jinfo("Covariates set")
      }
      },
    covs_info=function(alist) {
      
      if (missing(alist)) {
        return(private$.covs_info)
       }
      },

     factors=function(alist) {
      
      if (missing(alist)) {
        return(private$.factors)
      } else {
        
        names<-unlist(lapply(alist, function(x) x$name))
        self$included<-c(self$included,unlist(lapply(alist, function(x) if (hasName(x,"include") && x$include==TRUE) x$name else NULL)))
        private$.factors<-alist
        names(private$.factors)<-names

        }
       },
       vars = function(alist) {
         
         if (missing(alist)) {
            return(c(private$.covs,private$.factors))
         }
         return(NULL)
         
       },
      model_fun  = function(afun) {
      
        if (missing(afun)) {
        return(private$.model_fun)
        } else {
          private$.model_fun<-afun
        }
      },
        
      data = function(df) {
          
        if (missing(df)) {
        return(private$.data)
        } else {
          for (name in names(df)) {
            if (is.factor(df[[name]])) {
             contrasts(df[[name]])<- structure(matrix(c(-1, 1), ncol = 1), dimnames = list(NULL, ""))
            }
          }
          private$.data<-df
        }
      },
    transformations = function(translist) {
      
        if (missing(translist)) {
        return(private$.transformations)
        } else {
          private$.transformations<-TRANSFUN[translist]
        }        
      
      
      
    }

  ), # end of active
  private=list(
  
    .model_class = NULL,
    .opts=list(),
    .covs=list(),
    .covs_info=NULL,
    .factors=NULL,
    .vars=NULL,
    .data=NULL,
    .model_fun=NULL,
    .transformations=NULL,
    .univariate_tab=NULL,
    .maketerms = function(df) {
      
      private$.data<-self$data
      
      for (i in seq_len(nrow(df))) {
          row<-df[i,]
          fun<-TRANSFUN[[row$id]]$fun
          name<-paste0(row$name,"_",row$id)
          private$.data[[name]]<-fun(private$.data[[row$name]])
      }

    },
    .testuniv = function(df) {
      
      dfout<-list()
      for (i in seq_len(nrow(df))) {
          row<-df[i,]
          name<-paste0(row$name,"_",row$id)
          ltab<-private$.makemodel(name)
          ladd(dfout)<-cbind(row,ltab)
      }
      as.data.frame(do.call(rbind,dfout))
      
    },

    .makemodel= function(var) {
      form<-as.formula(jmvcore::composeFormula(self$dep,var))
      opts<-self$opts
      opts[["formula"]]<-form
      opts[["data"]]<-private$.data
      model<-do.call(self$model_fun,opts)
      tab<-coefficients_table(model)
      return(tab)

    }

  )
)
