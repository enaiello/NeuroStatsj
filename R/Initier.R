Initer <- R6::R6Class(
    "Initer",
    class = TRUE,
    cloneable = FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
    inherit = Scaffold,
    public = list(
        dispatcher = NULL,
        data = NULL,
        selector= NULL,
        adjuster=NULL,
        initialize = function(jmvobj) {
            super$initialize(jmvobj)
            self$data<-jmvobj$data
            ### we want to clean the html message objects
            dispatch_message_cleaner(jmvobj)
            ## initialize the "info" accordion
            si<-SmartInfo$new(jmvobj)
            si$infovec<-INFO
            si$info()
          #### check if we can go
         
            if (is.null(self$options$dep)) {
                         self$warning<-list(topic="issues",
                                      message="Please select the outcome variable (Raw Score).",
                                      head="info")
              self$ok<-FALSE
            }
            if (length(c(self$options$covs,self$options$factors))==0) {
                         self$warning<-list(topic="issues",
                                      message="Please select at least one covariate or one factor.",
                                      head="info")
              self$ok<-FALSE
            }
            
            if (!self$ok) return()
            
          ## set the selector clss as the selector
          self$selector<-Selector$new(private$.checkdata())
          ## general stuff
          self$selector$model_fun<- MODEL_TYPE[[self$options$model_type]]$fun
          self$selector$opts <- MODEL_TYPE[[self$options$model_type]]$opts
          self$selector$transformations<-self$options$covsTransformations

          ## deal with variables
          forced<-self$options$forced
          included<-self$options$included
          covs<-lapply(self$options$covs,function(x) {
             trans<-"auto"
             method<-self$options$method
             inc<-FALSE

             for (f in forced) if (f$var==x) trans<-f$type
             if (!self$options$select)
                 if (trans=="auto") trans<-"lin"
    
             
             for (f in included) if (f==x) inc<-TRUE
             if (inc) method="user"
             method=METHOD_LABEL[[method]]
             list(name=x,type="Covariate",forced=TRANSFUN[[trans]]$id,transf=TRANSFUN[[trans]]$name,include=inc,method=method)}
             )
          factors<-lapply(self$options$factors,function(x) {
            
                    method<-self$options$method
                    inc<-FALSE
                    for (f in included) if (f==x) inc<-TRUE
                    if (inc) method="user"
                    method=METHOD_LABEL[[method]]
                    list(name=x,type="Factor",transf="None",include=inc, method=method)
          })
          ## fill the selector
          self$selector$dep<-self$options$dep
          self$selector$covs<-covs
          self$selector$factors<-factors
          
          ## prepare the adjuster
          self$adjuster<-Adjuster$new(self$analysis)
          self$adjuster$which_worse<-self$options$direction

            
        },
        init_varstab= function() {
          
          c(self$selector$covs_info,self$selector$factors)

          
          },
        init_univariate= function() {
  
           self$selector$univariate()

        },

        init_multiple= function() {
  
          c(self$selector$covs_info,self$selector$factors)

        },
        init_scores_es= function() {
  
           tab<-list()
          if (self$options$es_zscore) ladd(tab)<-list("method"="z-scores")
          if (self$options$es_rank)   ladd(tab)<-list("method"="Percentiles")
          return(tab)


        },
        
        init_scores_percentiles= function() {
  

          switch (self$options$perc_type,
                  eby5 = {data<-data.frame(perc=c(1:5,seq(10,95,5),96:99))},
                  eby10= {data<-data.frame(perc=c(1:5,seq(10,90,10),95:99))},
                  by10= {data<-data.frame(perc=c(1,seq(10,90,10),99))},
                  by5= {data<-data.frame(perc=c(1,seq(10,90,5),99))}
                  
          )
          self$adjuster$perc_type=data$perc
          data

        }



        #### init functions #####
    ), # End public

    private = list(
      
      .checkdata = function() {
        
        data<-na.omit(self$data)
        ### check factors
        dep<-self$options$dep
       
        if (is.factor(data[[dep]])) {
            self$stop(paste("Raw score ",dep," must be a continuous or ordinal variable."))
          
        }
    
        if (self$options$model_type %in% c("nb","pois" )) {
          
          var<-data[[dep]]
          if (!all(var == floor(var)))
             self$stop(paste("Negative binomial and Poisson regression requires variable ",dep," values to be integers."))
          
        }

        if (self$options$model_type %in% c("beta")) {
          
          var<-data[[dep]]
          if (max(var)>1 || min(var)<0)
             self$stop(paste("Beta regression requires variable ",dep," values to be between 0 and 1."))
          
        }
        
        for (var in self$options$factors) {
          
          if (!is.factor(data[[var]])) {
            data[[var]]<-factor(data[[var]])
            self$warning(topic="issues",message=paste("Variable",var," has been coerced to factor"))
          }
          
          if (nlevels(data[[var]])>2) {
            self$stop(paste("Variable",var," has more than two levels. Only dichotomous factors are allowed."))
          }
          
        }
        
        
        data
        
        
        
      }

      
      
    ) # end of private
) # End Rclass
