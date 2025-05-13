## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
    inherit = Initer,
    cloneable = FALSE,
    class = TRUE,
    public = list(
        run = function() {
            # this is run before any table or plot is filled.
            # it produces the basic estimation required for all tables and plots
            # it fills self$data with all power parameters
            jinfo("NeuroStatsj: Runner: checking data")

        }
    ), # end of public 

    private = list(
        # do private stuff
    ) # end of private
) # end of class
