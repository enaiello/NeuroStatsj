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
            
                      ## set the selector clss as the machine
          self$machine<-Selector$new(self$data)
          ## general stuff
          self$machine$model_fun<- MODEL_TYPE[[self$options$model_type]]$fun
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

    private = list() # end of private
) # End Rclass
