## makes plots. It gets the runner (as usual) which contains $selector with all the info
## of the selection process and $adjuster, that deal with adjusting and scoring
Plotter <- R6::R6Class(
    "Plotter",
    cloneable = FALSE,
    class = TRUE,
    inherit = Scaffold,
    public = list(
        results = NULL,
        plots   = NULL,
        adjuster=NULL,
        runner= NULL,
        data = NULL,
        initialize = function(jmvobj, runner) {
            super$initialize(jmvobj)
            private$.results <- jmvobj$results
            self$adjuster    <- runner$adjuster
            self$runner      <- runner
            self$ok          <- runner$ok
          
        },
        init_plots= function() {
          
        },
        prepare_plots= function() {
      
          private$.prepare_adjusted()    
          private$.prepare_adj_obs()    
          private$.prepare_adj_pred()    

        },
        plot_adjusted=function(image, ggtheme, theme) {
            
            if (is.null(image$state)) return()
                
            df<-image$state$df

          plot <- ggplot2::ggplot(df, ggplot2::aes(x = score, fill = es)) +
                  ggplot2::geom_histogram(bins = 50, position="stack",alpha=.6,color = "black") 
          
 
          plot <- plot +   ggplot2::labs(x = "Adj. Score", y = "Count", color="Eq. Scores") 
          plot <- plot  +  ggtheme
          plot <- plot + ggplot2::scale_fill_manual(values =private$.es_colors,
                                             name = "Equivalent Score")           
          return(plot)
        },
        plot_adj_obs = function(image, ggtheme, theme) {

          if (is.null(image)) return()
          df<-image$state$df
          xname<-image$state$xname
          if (utils::hasName(df,"es")) 
                       plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = score, color=es)) 
          else
                       plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = score)) 
            
          # Scatterplot
        
          plot <- plot +  ggplot2::geom_point(size = 2) 
          plot <- plot +  ggplot2::xlab(xname) + ggplot2::ylab("Adj. Score")
          plot <- plot +  ggtheme 
          plot <- plot + ggplot2::scale_color_manual(values =private$.es_colors,
                                             name = "Equivalent Score")  

          return(plot)
        },

        plot_adj_pred = function(image, ggtheme, theme) {
          
          if (is.null(image)) return()
          df<-image$state$df
          xname<-image$state$xname
          if (utils::hasName(df,"es")) 
                       plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = score, color=es)) 
          else
                       plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = score)) 
            
          # Scatterplot
        
          plot <- plot +  ggplot2::geom_point(size = 2) 
          plot <- plot +  ggplot2::xlab(xname) + ggplot2::ylab("Adj. Score")
          plot <- plot +  ggtheme 
          plot <- plot + ggplot2::scale_color_manual(values =private$.es_colors,
                                             name = "Equivalent Score")  

          return(plot)
        }
    ), ## end of public
    private= list(
      .results=NULL,
      .operator=NULL,
      .es_colors=c("#D73027", "#FDB56E", "#FEE08B", "#91BFDB", "#4575B4"),
      .prepare_adjusted=function() {

          if (!self$option("plots","adj")) return()
          if (!self$ok) return()
         
          df<-self$adjuster$es_default()
          
          aplot<-private$.results$plots$get("adj")
          state=list(df=df)
          aplot$setState(state)
        
      },
      .prepare_adj_obs=function() {

          if (!self$option("plots","adj_obs")) return()
          if (!self$ok) return()
        

          dep <-self$runner$selector$dep
          df  <-self$runner$adjuster$es_default()
          df$x <- self$runner$data[,dep]
          if (!self$option("plot_es")) df$es<-NULL
          aplot<-private$.results$plots$get("adj_obs")
          
          aplot$setState(list(df=df))
          
      },

      .prepare_adj_pred=function() {

          if (!self$option("plots","adj_pred")) return()
          if (!self$ok) return()
        

          es <- self$adjuster$es_default()
          covs<-list_get(self$runner$selector$selected,"name")
          if (is.null(covs))
              self$warning<-list(topic="plots_issues",message="No predictor to plot",head="warning")
          array<-private$.results$plots$get("adj_pred")
          for (i in seq_along(covs)) {
              array$addItem(key = i)
              aplot<-array$get(key=i)
              df<-data.frame(score=es$score,x=self$runner$data[,covs[i]])
              if (self$option("plot_es")) df$es<-es$es
              state<-list(df=df,xname=covs[i])
              aplot$setState(state)
          }
      }

      
      
    )
) # end of class