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
            self$machine$data<-private$.checkdata()
            ## we stop if data are not ok
            if (!self$ok) return()

            self$univariate_tab<-self$machine$univariate()
            self$machine$find_best()
            self$multiple_tab<-self$machine$multiple()
            self$final_tab<-self$machine$select()
            form<-self$machine$pretty_formulate()
            self$warning<-list(topic="formula",message=form,head="info")

            ### fix some column name            
            if (self$options$model_type=="lm") {
              attr(self$univariate_tab,"titles")<-list(test="t")
              attr(self$multiple_tab,"titles")<-list(test="t")
              attr(self$final_tab,"titles")<-list(test="t")

            }

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
        }

        
    ), # end of public 

    private = list(
      

    ) # end of private
) # end of class
