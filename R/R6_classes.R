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
    adjuster   = NULL,
    perc_direction  = "increasing",
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
         tabname<-lapply(self$covs,function(x) lapply(x$transformations, function(z) z$name))
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
      tab<-lapply(vars,function(x) {
         .data<-data[data$name==x,]
         .max<-which.max(.data$r2)
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
      
      scope<-list(lower=~1, upper=form)
      if (is.something(self$included)) {
        included<-self$best_transf[self$included]
        included<-unlist(lapply(included,function(x) x$var))
        scope<-list(lower=as.formula(jmvcore::composeFormula(NULL,included)), upper=form)
      }

      steps<-MASS::stepAIC(model,direction="both",trace=0, scope=scope)
      self$model<-steps
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
      
    },
    pretty_formulate=function() {
      form<-self$formulate()
      paste("<h2> Adjustment formula</h2>","<p>",form,"</p>")
      
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
        private$.factors_names<-names

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
    .factors_names=NULL,
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
          ladd(dfout)<-cbind(row,private$.makemodel(name))
      }
      as.data.frame(do.call(rbind,dfout))
      
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
    
    .makemodel= function(var) {
      form<-as.formula(jmvcore::composeFormula(self$dep,var))
      opts<-self$opts
      opts[["formula"]]<-form
      opts[["data"]]<-private$.data
      model<-do.call(self$model_fun,opts)
      tab<-coefficients_table(model)
      return(tab)

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
