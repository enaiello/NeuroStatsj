
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
                ### info table ###
                aSmartObj <- SmartTable$new(self$results$varstab, private$.runner)
                ladd(private$.smartObjs) <- aSmartObj
                ### univariate table ###
                aSmartObj <- SmartTable$new(self$results$univariate, private$.runner)
                aSmartObj$spaceBy <-"name"
                aSmartObj$combineBelow <-"name"
                aSmartObj$hideOn <- list(r2 = NA,df=NA)
                ladd(private$.smartObjs) <- aSmartObj

                ### multiple table ###
                aSmartObj <- SmartTable$new(self$results$multiple, private$.runner)
                ladd(private$.smartObjs) <- aSmartObj
                aSmartObj$hideOn <- list(df = NA)
                ### final table ###
                aSmartObj <- SmartTable$new(self$results$final, private$.runner)
                aSmartObj$hideOn <- list(df = NA)
                ladd(private$.smartObjs) <- aSmartObj

                ### scores_es table ###
                aSmartObj <- SmartTable$new(self$results$scores$es, private$.runner)
                aSmartObj$combineBelow <- "method"
                ladd(private$.smartObjs) <- aSmartObj

                ### scores_percentiles table ###
                aSmartObj <- SmartTable$new(self$results$scores$percentiles, private$.runner)
                ladd(private$.smartObjs) <- aSmartObj

                ### scores_raws table ###
                aSmartObj <- SmartTable$new(self$results$scores$raws, private$.runner)
                aSmartObj$expandOnRun<-T
                aSmartObj$expandFrom<-2
                ladd(private$.smartObjs) <- aSmartObj
                
                ### init all ####
                for (tab in private$.smartObjs) {
                    tab$initTable()
                }
                
                private$.plotter <- Plotter$new(self, private$.runner)
                private$.plotter$init_plots()
            
        },
        
        .run = function() {
            ginfo("run")
            private$.runner$data<-stats::na.omit(self$data)
            private$.runner$run()
            for (tab in private$.smartObjs) {
                    tab$runTable()
                }
            private$.plotter$prepare_plots()
            
            
        },
        .cleandata=function() {
            ### here we check the data, remove missing, and change variables if necessary
            ### here you want to check if the transformations can be applied, if factors are
            ### coded well, etc.
            ### for now, it just remove the missing
            private$.data<-jmvcore::naOmit(self$data)
        },
        ### plots
        
        .plot_adjusted=function(image, ggtheme, theme, ...) {
            plot <- private$.plotter$plot_adjusted(image, ggtheme, theme)
            return(plot)
          
        }

    )
)

