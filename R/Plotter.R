Plotter <- R6::R6Class(
    "Plotter",
    cloneable = FALSE,
    class = TRUE,
    inherit = Scaffold,
    public = list(
        results = NULL,
        plots   = NULL,
        initialize = function(jmvobj, operator) {
            super$initialize(jmvobj)
            private$.results <- jmvobj$results
            private$.operator <- operator
          
        },
        init_plots= function() {
          
        },
        prepare_plots= function() {
      
          private$.prepare_adjusted()    
        },
        plot_adjusted=function(image, ggtheme, theme) {
            
            df<-data.frame(score=image$state$score)
            return()
            plot <- ggplot(df, aes(x = score)) 
            plot <- plot +  geom_histogram(bins = 20, fill = "lightgray", color = "white") 
            plot <- plot +  geom_vline(xintercept = cutoffs, 
                                       aes(color = cutoff_names),
                                       linetype = "dashed", 
                                       linewidth = 1.1) 
            plot <- plot + scale_color_manual(values = c("red", "blue", rep("darkgreen", 4))) 
            plot <- plot + labs(x = "Score", y = "Count") 
            plot <- plot + ggtheme
            return(plot)
        }
    ), ## end of public
    private= list(
      .results=NULL,
      .operator=NULL,
      
      .prepare_adjusted=function() {
         return()
          aplot<-private$.results$plots$get("adj_es")
          score<-private$.operator$adjuster$adjust()
          if (self$options$es_rank)
             es<-private$.operator$adjuster$es_binom()
          if (self$options$es_zbeta)
             es<-private$.operator$adjuster$es_binom()
          if (self$options$es_zbinom)
             es<-private$.operator$adjuster$es_binom()
          state=list(score=score,es=es)
          aplot$setState(state)
        
      }
      
      
    )
) # end of class