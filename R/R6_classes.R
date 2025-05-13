Adjuster <- R6::R6Class("Adjuster",
  public = list(
    model = NULL,
    initialize = function(model) {
      self$model <- model
      private$.model_class <- class(model)
      private$.family      <- family(model)
      private$.link_fun    <- private$.family$linkfun
      private$.link_inv    <- private$.family$linkinv
      private$.adj_fun     <- adj_fun(model)
    },
    
    adjust=function() {
      
      model<-self$model
      cat ("adjusting with ", deparse(stats::formula(model)))
      dep<-stats::formula(model)[[2]]
      terms<-colnames(attr(terms(model),"factors"))
      mm<-model.matrix(model)
      mm<-mm[,attr(mm,"assign")>0]
      for (n in colnames(mm)) {
         if (length(unique(mm[,n]))==2) {
            warning("Variable ",n," coerced to numeric dichotomous variable")
            mm[,n]<-as.numeric(factor(mm[,n]))-1.5
         }
         else mm[,n]<-as.numeric(scale(mm[,n],scale=FALSE))
      }

      coefs<-coef(model)[-1]
      coefs<-round(coefs,digits=4)
      preds<-  as.matrix(mm) %*% coefs
      preds<-  private$.link_inv(preds)
      op<-private$.adj_fun$op
      score<-op(model$model[[dep]],preds)
      return(score)
    },
    percentiles=function(what=NULL,dir) {
      if (is.null(what))
          p<- seq(0.01, .99, by = 0.01)
      else
          p <- what/100
      values<-self$adjust()
      ps<-data.frame(val=quantile(values, probs = p))
      ps$perc<-rownames(ps)
      rownames(ps)<-NULL
      if (dir=="decreasing") {
        ps<-ps[order(ps$val,decreasing=TRUE),]
        ps$perc<-rev(ps$perc)
        rownames(ps)<-NULL
      }
      ps
      
    }
  ), ## end of public
  active= list(
    
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
    data       = NULL,
    adjuster   = NULL,
    perc_direction  = "increasing",
    selected_covs   =list(),
    selected_factors=list(),
    stepwise    = NULL,
    initialize = function(data=NULL) {
      self$data <- data
      private$.data<-data
      self$model_fun<-lm
    },
    
    estimate=function() {
      
      if (is.null(self$dep)) stop("The outcome variable  must be defined")
      if (is.null(private$.covs)) stop("The covariates variable  must be defined")
      vars<-c(self$selected_covs,self$selected_factors)
      form<-jmvcore::composeFormula(self$dep,vars)
      opts<-c(self$opts,list(formula=form,data=private$.data))
      self$model<-do.call(self$model_fun,opts)
      ss<-summary(self$model)$coefficients
      return(ss)
      
    },
    select = function() {
      
      for (cov in private$.covs) {
           private$.maketerm(cov)
      }
      
      for (cov in private$.covs) {
           private$.selectterm(cov)
      }
      
      private$.testterm()
      
    },
    criteria=function() {
      
      covs<-private$.covs[names(self$selected_covs)]
      crit<-lapply(covs,function(x) x$selection$table[,2])
      do.call(cbind,crit)

    },
    adjust=function() {
      
      self$adjuster<-Adjuster$new(self$model)
      self$adjuster$adjust()
      
    },
    approx_adjust=function() {
      
      if (is.null(self$model)) stop("Please estimate the model first.")
      
      expr_string <- make_hand_formula(self$model,self)  
      with(private$.data, eval(parse(text = expr_string)))

    },
     eval_one=function(value,covs) {
      
      if (is.null(self$model)) stop("Please estimate the model first.")
      values<-lapply(self$covs, function(x) {
        if (!("selection" %in% names(x))) return()
        x$selection$info$fun(covs[[x$name]])-x$selection$mean    
      })
      coefs<-coef(self$model)[-1]
      adj<-value-sum(coefs * unlist(values))
      ps<-self$percentiles()
      p<-which.min(abs(adj-unlist(ps$val)))
      print(adj)
      print(p)

     },


    percentiles=function(what=NULL) {
      
      self$adjuster<-Adjuster$new(self$model)
      self$adjuster$percentiles(what, self$perc_direction)
      
    },

    formulate=function() {
      
      if (is.null(self$model)) stop("Please estimate the model first.")
      make_formula(self$model,self)
      
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
        lapply(alist,function(x) private$.covs[[x$name]]<-x )
        lapply(alist,function(x) private$.covs[[x$name]]$mean<-mean(self$data[[x$name]],na.rm=T) )
        selected<-sapply(alist,function(x) x$name )
        names(selected)<-selected
        self$selected_covs<-selected
        private$.covs_names<-selected
        names(private$.covs)<-selected       
      }
      },
     factors=function(alist) {
      
      if (missing(alist)) {
        return(private$.factors)
      } else {
        
        names<-unlist(lapply(alist, function(x) x$name))
        private$.factors<-alist
        names(private$.factors)<-names
        private$.factors_names<-names
        self$selected_factors<-names

        }
       },
      model_fun  = function(afun) {
      
        if (missing(afun)) {
        return(private$.model_fun)
        } else {
          private$.model_fun<-afun
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
    .covs_names=NULL,
    .factors=NULL,
    .data=NULL,
    .model_fun=NULL,
    .factors_names=NULL,
    .transformations=NULL,
    .maketerm = function(varobj) {
      
      if (is.null(varobj$force))
         trans<-TRASNFUN
      else
         trans<-TRASNFUN[varobj$force]
    for (tran in trans) {
      
      name<-paste0(tran$id,"_",varobj$name)
      trans[[tran$id]]$varname<-name
      private$.data[[name]]<-tran$fun(private$.data[[varobj$name]])
    }
    private$.covs[[varobj$name]]$trans<-trans
  
    },
    .selectterm = function(varobj) {
      
    crit<-lapply(varobj$trans, function(tran) list(name=tran$varname,R2=private$.criteria(tran$varname)))
    crit<-do.call(rbind,crit)
    win<-which.max(crit[,2])
    
    smean<-mean(private$.data[[varobj$trans[[as.numeric(win)]]$varname]],na.rm=TRUE)
    private$.covs[[varobj$name]]$selection<-list(table=crit,
                                                 crit=unlist(crit[as.numeric(win),2]),
                                                 info=varobj$trans[[as.numeric(win)]],
                                                 mean=smean)
    self$selected_covs[[varobj$name]]<-varobj$trans[[as.numeric(win)]]$varname

    },
    .criteria= function(var) {
      form<-as.formula(jmvcore::composeFormula(self$dep,var))
      opts<-self$opts
      opts[["formula"]]<-form
      opts[["data"]]<-private$.data
      model<-do.call(self$model_fun,opts)
      as.numeric(performance::r2(model)[[1]])

    },
   .testterm = function() {
    
      vars<-c(self$selected_covs,self$selected_factors)
      form<-as.formula(jmvcore::composeFormula(self$dep,vars))
      opts<-self$opts
      opts[["formula"]]<-form
      opts[["data"]]<-private$.data
      model<-do.call(self$model_fun,opts)
      steps<-MASS::stepAIC(model,direction="both",trace=0)
      self$stepwise<-steps$anova
      rr<-colnames(attr(terms(steps),"factors"))
      selected_covs <- clapply(self$selected_covs, function(x) if(x %in% rr) x else NULL)

      keep<-clapply(private$.covs,function(x) if ("include" %in% names(x) && x$include) x$selection$info$varname)
      for (x in names(keep)) 
         selected_covs[[x]]<-keep[[x]]
      self$selected_covs<-selected_covs
      selected_factors <- clapply(self$selected_factors, function(x) if(x %in% rr) x else NULL)
      keep<-clapply(private$.factors,function(x) if ("include" %in% names(x) && x$include) x$name)
      for (x in names(keep)) 
           selected_factors[[x]]<-keep[[x]]
       self$selected_factors<-selected_factors

   }


  )
)
