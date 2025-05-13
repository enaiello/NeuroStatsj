
# This file is a generated template, your changes will not be overwritten

scoringClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "scoringClass",
    inherit = scoringBase,
    private = list(
        .time = NULL,
        .ready = FALSE,
        .smartObjs = list(),
        .plotter = NULL,
        .runner = NULL,
        .init =function() {
          
                jinfo(paste("MODULE:  NeuroStatsj #### phase init  ####"))
                private$.time <- Sys.time()
                class(private$.results) <- c("scoring", class(private$.results)) ## this is useful in R interface

                ### set up the R6 workhorse class
                private$.runner <- Runner$new(self)
                ### univariate table ###
                aSmartObj <- SmartTable$new(self$results$varstab, private$.runner)
                ladd(private$.smartObjs) <- aSmartObj
                ### init all ####
                for (tab in private$.smartObjs) {
                    tab$initTable()
                }
            
        },
        
        .run = function() {
            ginfo("run")
            private$.runner$run()
        
            
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

