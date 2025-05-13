
# This file is a generated template, your changes will not be overwritten

scoringClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "scoringClass",
    inherit = scoringBase,
    private = list(
        .data=NULL,
        .init =function() {
            ginfo("init")
            
            
        },
        
        .run = function() {
            ginfo("run")
            private$.data<-jmvcore::naOmit(self$data)
            
            
        },
        .cleandata=function() {
            ### here we check the data, remove missing, and change variables if necessary
            ### here you want to check if the transformations can be applied, if factors are
            ### coded well, etc.
            ### for now, it just remove the missing
            private$.data<-jmvcore::naOmit(self$data)
        }

    )
)

