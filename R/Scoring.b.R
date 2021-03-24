
# This file is a generated template, your changes will not be overwritten

ScoringClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ScoringClass",
    inherit = ScoringBase,
    private = list(
        .data=NULL,
        .init =function() {
            ginfo("init")
            # init is run at the beginning with empty data (definitions of variables are present)
            # usuful to setup tables and other stuff
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            
            # as an example, I'm preparing the table "univariate" defined in .r.yaml with
            # one row for each continuous variable for each transformation flagged in the UI
            
            # collect the table (defined in .r.yaml)
            aTable<-self$results$univariate
            # collect the continuous variables (options are defined in .a.yaml)
            covs<-self$options$covs
            ## I use mark to see in console what I got 
            # then I collect the transformations
            trans<-self$options$covsTransformations

            # and go through them, adding a row to the table filling the column term with a label 
            # the labels. Later on we need to make this label look nicer
            i<-0
            for (cov in covs) {
                for (tran in trans) {
                    i<-i+1
                    label<-paste0(cov,"-",tran)
                    aTable$addRow(rowKey=i,list(term=label))
                }
            }
            # preparing the table like this prevents jamovi to squish and redraw the table every time
            # something is updated in the UI 
            # the second table we defined in .r.yaml is `multiple`. We do not prepare it because we do not
            # know in advance how many rows will need
            
        },
        .run = function() {
            ginfo("run")
            # clean the data and store them in private$.data
            private$.cleandata()
            # collect the outcome (options are defined in .a.yaml)
            dep<-self$options$dep
            
            # collect the continuous variables (options are defined in .a.yaml)
            covs<-self$options$covs
            # collect the categorical variables (options are defined in .a.yaml)
            factors<-self$options$factors
            
            # here is when the actual computation happens. You can call any function from the /R folder, packages loaded
            # in DESCRIPTION or local function in private$.xxx
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            # we stop any work if no outcome is defined
            if (!is.something(dep))
                return()
            # we stop any work if no predictor is defined
            if (!is.something(c(factors,covs)))
                return()
            
            # as an example, we fill the "univariate table" with lm() results
            # collect the table (defined in .init())
            aTable<-self$results$univariate
            # then I collect the transformations
            trans<-self$options$covsTransformations
            # I prepared a function .estimate_univariate() in private$ to estimate the model
            # and return the line of results required
            i<-0
            signif<-list()
            for (cov in covs) {
                for (tran in trans) {
                    i<-i+1
                    results<-private$.estimate_univariate(cov,tran)
                    ### we need to know which is signiicant for later on
                    if (results$p<.05)
                        signif[[length(signif)+1]]<-c(cov,tran)
                    ### fill the table
                    aTable$setRow(rowKey=i,results)
                }
            }
         
            ######### prepare the full model #####
            if (self$options$method=="univariate") {
                model_results<-private$.estimate_full_univariate(signif)
            } else
                model_results<-private$.estimate_full_best()
            mark(class(model_results))
            ### model_results is of class summary.lm, so we can take all the info we need
            if ("summary.lm" %in% class(model_results)) {
                mycoeffs<-as.data.frame(model_results$coefficients)
                names(mycoeffs)<-c("b","se","t","p")
            ## we add the df
                mycoeffs$df<-model_results$fstatistic[[3]]
            ## and the labels
            ## get the table
                aTable<-self$results$multiple
                mycoeffs$term<-row.names(mycoeffs)
                for (i in 1:nrow(mycoeffs))
                        aTable$addRow(rowKey=i,mycoeffs[i,])
            ## Here we add some info in footnote
                aTable$setNote("r2", paste("The R-squared is",round(model_results$r.squared,digits = 3)))
                
                
            #### saving scores back to datasheet: as an example I save the z-scores####
                
                if (("standardized" %in% self$options$scoreTypes) & self$results$zscores$isNotFilled()) {
                    ginfo("Saving z-scores")
                    zscores<-as.numeric(scale(private$.data[,dep]))
                    # we need the rownames in case there are missing in the datasheet
                    zscores <- data.frame(zscores=zscores, row.names=rownames(private$.data))
                    self$results$zscores$setValues(zscores)
                }
                
            }        
                },
        .cleandata=function() {
            ### here we check the data, remove missing, and change variables if necessary
            ### here you want to check if the transformations can be applied, if factors are
            ### coded well, etc.
            ### for now, it just remove the missing
            private$.data<-jmvcore::naOmit(self$data)
        },
        .estimate_univariate=function(term,tran) {
            ## the info about the transformations are in the constant list
            ## TINFO defined in constants.
            dep<-self$options$dep
            ## prepare the formula
            info<-TINFO[[tran]]
            termGood<-gsub("_._VAR_._",term,info$template,fixed=TRUE)
            lin<-"1"
            if (info$require=="linear")
                  lin<-term
            form<-as.formula(paste(dep,"~",lin,"+",termGood))
            ### do the estimation with lm
            mod<-lm(form,data=private$.data)
            # collect the info we need. Names of the list should correspond to the columns of the
            # table defined in .r.yaml
            b<-mod$coefficients[[length(mod$coefficients)]]
            sumry<-summary(mod)
            r2<-sumry$r.squared
            ftest<-sumry$fstatistic
            p<-pf(ftest[[1]], ftest[[2]], ftest[[3]], lower.tail = FALSE)
            results<-list(b=b,r2=r2,ftest=ftest[[1]],df1=ftest[[2]],df2=ftest[[3]],p=p)
            results
        },
        .estimate_full_univariate=function(terms) {
            ## check is something comes out from univarate test
            if (!is.something(terms))
                 return(FALSE)
            ### composte the formula ####
            ### first we get the terms ###
            myterms<-lapply(terms,function(term) {
                info<-TINFO[[term[[2]]]]
                gsub("_._VAR_._",term,info$template,fixed=TRUE)
            })
            form<-paste(self$options$dep,"~",paste(myterms,collapse = "+"))
            mod<-lm(form,private$.data)
            sumry<-summary(mod)
            sumry

        }
        
        
        
        
        )
)
