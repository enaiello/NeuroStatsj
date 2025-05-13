#### S3 methods ####

## define functions to obtain the adusted score depending on the type of model

adj_fun <- function(x, ...) UseMethod(".adj_fun")

.adj_fun.default<-function(x) stop("no adj fun for class",class(x))

.adj_fun.lm<-function(x) {
  list(op=function(a,b) a-b,symbol="-")
}

.adj_fun.glm<-function(x) {
  cat("using glm formulas\n")
  list(op=function(a,b) a/b,symbol="/")
}

## define functions to obtain write out the formula to adjust

make_formula <- function(model, obj) UseMethod(".make_formula")

.make_formula.default<-function(model,obj) stop("No formula for class ", paste(class(model),collapse=", "))

.make_formula.lm<-function(model,obj) {
  
   form<-.make_formula_res(model,obj)
   form<-paste0("Adj_score = Raw_score - [",form,"]")
   form
   
}

.make_formula.glm<-function(model,obj) {
  
   
   form<-.make_formula_res(model,obj)
   form<-paste0("Adj score = Raw score / EXP[",form,"]")
   form
   
}

.make_formula_res<-function(model,obj) {
  
   ### first the covariates

   vars<-names(obj$selected_covs)
   terms<-unlist(obj$selected_covs)
   covsobj<-obj$covs[vars]
   coefs<-coef(obj$model)[names(coef(obj$model)) %in% terms ]
   text<-paste("Adjusted score = raw score -")
   labs<-unlist(lapply(covsobj, function(x) x$selection$info$label(x$name)))
   means<-lapply(covsobj, function(x) x$selection$mean)
   form<-paste0(sprintf("%.4f * ( %s - %.4f)",coefs, labs, means),collapse=" + ")
   form <- gsub("+ -"," - ",form,fixed=T)
   ## then the factors
   if (length(obj$selected_factors)>0) {
     coefs<-coef(obj$model)[names(coef(obj$model)) %in% obj$selected_factors]
     labs<-obj$selected_factors
     fform<-paste0(sprintf("%.4f * %s",coefs, labs),collapse=" + ")
     form<-paste(form,fform)
   }
  
   form
  
}


## define functions to obtain "hand" calculated to adjust

make_hand_formula <- function(model, obj) UseMethod(".make_hand_formula")

.make_hand_formula.default<-function(model,obj) stop("No formula for class ", paste(class(model),collapse=", "))

.make_hand_formula.lm<-function(model,obj) {
  
   res<-.make_hand_formula_res(model,obj)
   paste(obj$dep ,"-","(",res,")")
}




make_calc_formula <- function(model, obj) UseMethod(".make_calc_formula")

.make_calc_formula.default<-function(model,obj) stop("No formula for class ", paste(class(model),collapse=", "))

.make_calc_formula.lm<-function(model,obj) {
  
   res<-.make_hand_formula_res(model,obj)
   paste(" %s-","(",res,")")
}



.make_hand_formula_res<-function(model,obj) {
  
   ### first the covariates

   vars<-names(obj$selected_covs)
   terms<-unlist(obj$selected_covs)
   covsobj<-obj$covs[vars]
   coefs<-coef(obj$model)[names(coef(obj$model)) %in% terms ]
   means<-lapply(covsobj, function(x) x$selection$mean)
   form<-paste0(sprintf("%.4f * ( %s - %.4f)",coefs, names(coefs), means),collapse=" + ")
   form <- gsub("+ -"," - ",form,fixed=T)
   ## then the factors
   if (length(obj$selected_factors)>0) {
     coefs<-coef(obj$model)[names(coef(obj$model)) %in% obj$selected_factors]
     labs<-obj$selected_factors
     fform<-paste0(sprintf("%.4f * %s",coefs, labs),collapse=" + ")
     form<-paste(form,fform)
   }
  
   form
  
}
