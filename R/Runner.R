## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
    inherit = Initer,
    cloneable = FALSE,
    class = TRUE,
    public = list(
        univariate_tab=NULL,
        multiple_tab=NULL,
        final_tab=NULL,
        run = function() {
          
            ## we stop if initier is not ok
            if (!self$ok) return()
            jinfo("NeuroStatsj: Runner: estimations")
            ### self$machine is the selector initialized in Initier
            self$machine$data<-private$.checkdata()
            ## we stop if data are not ok
            if (!self$ok) return()

            self$univariate_tab<-self$machine$univariate()
            self$machine$find_best()
            self$multiple_tab<-self$machine$multiple()
            self$final_tab<-self$machine$select()
            if (is.null(self$final_tab)) {
              self$final_tab<-list(list(name="No predictors", fun="Original score"))
            }
            form<-self$machine$pretty_formulate()
            self$warning<-list(topic="scores_formula",message=form,head="info")

            ### fix some column name            
            if (self$options$model_type=="lm") {
              attr(self$univariate_tab,"titles")<-list(test="t")
              attr(self$multiple_tab,"titles")<-list(test="t")
              attr(self$final_tab,"titles")<-list(test="t")

            }
            
            ## fill the adjuster
            self$adjuster$model<-self$machine$model
            

        },
        run_univariate= function() {
          tab<-self$univariate_tab
          tab
        },
        run_multiple= function() {
          self$multiple_tab
        },
        run_final= function() {
          self$final_tab
        },
        run_varstab= function() {
          res<-list()
          for (var in self$machine$vars) {
              trans<-"None"
              selected="False"
              test<-self$machine$selected[[var$name]]
              if (length(test)>0) {
                      trans<-TRANSFUN[[self$machine$selected[[var$name]]$id]]$label(var$name)
                      selected="True"
              }
            ladd(res)<-list(name=var$name,
                            selected=selected,
                            selected_trans=trans)
          }
         
          res
        },
        
        run_scores_percentiles = function() {
          
          perc<-self$adjuster$percentiles()
          perc
        },
        run_scores_es = function() {
          
          tab<-list()
          if (self$options$es_zbinom) ladd(tab)<-self$adjuster$es_binom()
          if (self$options$es_zbeta)  ladd(tab)<-self$adjuster$es_beta()
          if (self$options$es_rank)   ladd(tab)<-self$adjuster$es_perc()
          tab<-as.data.frame(do.call(rbind,tab))
          mark(tab)
          tab<-fix_es(tab,self$options$direction)
          return(tab)
        },

        run_scores_raws = function() {
          
          data<-data.frame(id=rownames(self$data))
          data$adj<-unlist(self$adjuster$adjust())
          data$raw<-self$machine$data[[self$options$dep]]
          if (self$options$score_preds) {
          for (var in self$machine$selected)
               data[[var$name]]<-self$machine$data[[var$name]]
          }
     
          if ("inc" %in% self$options$score_sort) 
                 data<-data[order(data$adj,decreasing=FALSE),]
          if ("dec" %in% self$options$score_sort) 
                 data<-data[order(data$adj,decreasing=TRUE),]
         
          if (nrow(self$data)>200) {
            data<-data[1:200,]
            self$warning<-list(topic="scores_raws",message="Only the first 100 cases are shown")
          }
          data
        }

        
    ), # end of public 

    private = list(
      

    ) # end of private
) # end of class
