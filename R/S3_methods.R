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

.make_formula.default<-function(model,obj) {
  warning("No formula for class ", paste(class(model),collapse=", "))
  .make_formula.lm(model,obj)
}

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
  

   data<-model$model
   covs<-list()
   factors<-list()
   for (x in obj$selected) {
        if (!is.factor(data[[x$var]])) {
          covs[[x$name]]<-x
        } else {
          factors[[x$name]]<-x
        }
   }

   coefs<-coef(model)[names(coef(model)) %in% unlist(lapply(covs, function(x) x$var)) ]
   text<-paste("Adjusted score = raw score -")
   labs<-unlist(lapply(covs, function(x) TRANSFUN[[x$id]]$label(x$name)))
   means<-lapply(covs, function(x) mean(data[[x$var]],na.rm=TRUE))
   form<-paste0(sprintf("%.4f * ( %s - %.4f)",coefs, labs, means),collapse=" + ")
   form <- gsub("+ -"," - ",form,fixed=T)
   ## then the factors

   if (length(factors)>0) {
     coefs<-coef(model)[names(coef(model)) %in% unlist(lapply(factors, function(x) x$var))]
     fform<-paste0(sprintf("%.4f * %s",coefs, unlist(lapply(factors, function(x) x$name))),collapse=" + ")
     form<-paste(form,fform,sep=" + ")
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


#### some formatting 

coefficients_table <- function(model) UseMethod(".coefficients")

.coefficients.default <-function(model) {
  
  tab<-as.data.frame(summary(model)$coefficients)[-1,]
  names(tab)<-c("estimate","se","test","p")

  if (nrow(tab)==1) {
    tab$r2  <-  as.numeric(performance::r2(model)[[1]])
    tab$aic <-  stats::AIC(model)
  }
  tab
  
}

.coefficients.lm <-function(model) {

  tab<-as.data.frame(summary(model)$coefficients)[-1,]
  names(tab)<-c("estimate","se","test","p")
  tab$df <- df.residual(model)
  if (nrow(tab)==1) {
    tab$r2  <-  as.numeric(performance::r2(model)[[1]])
    tab$aic <-  stats::AIC(model)
  }
  tab
  
}

.coefficients.glm <-function(model) {
  
  jinfo("coefficient table for glm")

  tab<-as.data.frame(summary(model)$coefficients)[-1,]
  names(tab)<-c("estimate","se","test","p")
  if (nrow(tab)==1) {
    tab$r2  <-   as.numeric(performance::r2(model)[[1]])
    tab$aic <-  stats::AIC(model)
  }

  tab
  
}


.coefficients.negbin <-function(model) {
  
  jinfo("coefficient table for negbin")

  tab<-as.data.frame(summary(model)$coefficients)[-1,]
  names(tab)<-c("estimate","se","test","p")
  if (nrow(tab)==1) {
    tab$r2  <-   as.numeric(performance::r2(model)[[1]])
    tab$aic <-  stats::AIC(model)
  }

  tab
  
}

.coefficients.betareg <-function(model) {
  
  jinfo("coefficient table for betareg")

  tab<-as.data.frame(summary(model)$coefficients)[-1,1:4]
  names(tab)<-c("estimate","se","test","p")
  if (nrow(tab)==1) {
    tab$r2  <-  model$pseudo.r.squared
    tab$aic <-  stats::AIC(model)
  }
  tab
  
}



fix_call <- function(model) UseMethod(".fix_call")

.fix_call.default <-function(model) {
   model
}

.fix_call.betareg <- function(model) {
  
  opts<-list(formula=formula(model),data=model$model)
  call<- as.call(c(parse(text = "betareg::betareg")[[1]], opts))
  model$call<-call
  model
  
  
  
}

#' @export
extractAIC.betareg <- function(fit, scale, k=2, ...) {
  
    n <- length(fit$residuals)
    edf <- n - fit$df.residual
    c(edf, -2*fit$loglik + k * edf)  
  
}

#' @export
update.betareg <- function(object, formula., ..., evaluate = TRUE) {
  
  if (is.null(call <- getCall(object)))
    stop("object has no call slot")

  if (!missing(formula.)) {
    # betareg uses Formula objects internally — coerce to regular formula
    base_formula <- formula(object)
    if (inherits(base_formula, "Formula")) {
      base_formula <- formula(base_formula, lhs = TRUE, rhs = 1)  # use only mean part
    }

    call$formula <- update.formula(base_formula, formula.)
  }

  extras <- match.call(expand.dots = FALSE)$...
  if (length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }

  if (evaluate) eval(call, parent.frame()) else call
}


