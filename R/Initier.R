Initer <- R6::R6Class(
    "Initer",
    class = TRUE,
    cloneable = FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
    inherit = Scaffold,
    public = list(
        dispatcher = NULL,
        data = NULL,
        machine= NULL,
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
            
          ## set the selector clss as the machine
          self$machine<-Selector$new(private$.checkdata())
          ## general stuff
          self$machine$model_fun<- MODEL_TYPE[[self$options$model_type]]$fun
          self$machine$opts <- MODEL_TYPE[[self$options$model_type]]$opts
          self$machine$transformations<-self$options$covsTransformations

          ## deal with variables
          forced<-self$options$forced
          included<-self$options$included
          covs<-lapply(self$options$covs,function(x) {
             trans<-"auto"
             method<-self$options$method
             inc<-FALSE
             for (f in forced) if (f$var==x) trans<-f$type
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
          ## fill the machine
          self$machine$dep<-self$options$dep
          self$machine$covs<-covs
          self$machine$factors<-factors

            
        },
        init_varstab= function() {
          
          c(self$machine$covs_info,self$machine$factors)

          
          },
        init_univariate= function() {
  
           self$machine$univariate()

        },
        init_multiple= function() {
  
          c(self$machine$covs_info,self$machine$factors)

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
