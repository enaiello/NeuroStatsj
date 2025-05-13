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
        },
        init_varstab= function() {
          
          ## set the selector clss as the machine
          self$machine<-Selector$new(self$data)
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
             list(name=x,type="Covariate",forced=TRANSFUN[[trans]]$name,include=inc,method=method)}
             )
          factors<-lapply(self$options$factors,function(x) {
            
           method<-self$options$method
           inc<-FALSE
           for (f in included) if (f==x) inc<-TRUE
             if (inc) method="user"
             method=METHOD_LABEL[[method]]
             list(name=x,type="Factor",forced="None",include=inc, method=method)}
             )
          ## fill the machine
          self$machine$covs<-covs
          self$machine$factors<-factors
          self$machine$model_fun<- MODEL_TYPE[[self$options$model_type]]
          self$machine$transformations<-self$options$covsTransformations
          ## return info
          vars <- c(covs, factors)
          vars
          
          },
        init_univariate= function() {
          

          
          
        }

        #### init functions #####
    ), # End public

    private = list() # end of private
) # End Rclass
