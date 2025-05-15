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
            # this is run before any table or plot is filled.
            # it produces the basic estimation required for all tables and plots
            # it fills self$data with all power parameters
            jinfo("NeuroStatsj: Runner: estimations")
            self$machine$data<-self$data
            self$univariate_tab<-self$machine$univariate()
            self$machine$find_best()
            self$multiple_tab<-self$machine$multiple()
            self$final_tab<-self$machine$select()
            form<-self$machine$pretty_formulate()
            self$warning<-list(topic="formula",message=form,head="info")

        },
        run_univariate= function() {
          self$univariate_tab
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
          mark(res)
          res
        }

        
    ), # end of public 

    private = list(
        # do private stuff
    ) # end of private
) # end of class
