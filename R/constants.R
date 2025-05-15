ERROR_TABLE="issues"
### these are definitions of functions and translations
TRANSFUN<-list()
TRANSFUN[["lin"]]<-list(id="lin",name="Linear",fun=identity,label=function(x) sprintf("%s",x))
TRANSFUN[["ln"]]<-list(id="ln",name="LN",fun=function(x) log(x),label=function(x) sprintf("LN(%s)",x))
TRANSFUN[["log10"]]<-list(id="log10",name="Log",fun=function(x) log10(x),label=function(x) sprintf("LN(%s)",x))
TRANSFUN[["log100"]]<-list(id="log100",name="Log(100-x)",fun=function(x) log10(100-x),label=function(x) sprintf("LOG10(100-%s)",x))
TRANSFUN[["rec"]]<-list(id="rec",name="Reciprocal",fun=function(x) 1/x,label=function(x) sprintf("1/%s",x))
TRANSFUN[["quad"]]<-list(id="quad",name="Quadratic",fun=function(x) x^2,label=function(x) sprintf("%s\u00B2",x))
TRANSFUN[["cub"]]<-list(id="cub",name="Cubic",fun=function(x) x^3,label=function(x) sprintf("%s\u00B3",x))
TRANSFUN[["sqrt"]]<-list(id="sqrt",name="Squared Root",fun=function(x) sqrt(x),label=function(x) sprintf("%s\u221A",x))
TRANSFUN[["auto"]]<-list(id=NULL,name="Auto",label="Automatic")
TRANSFUN[["none"]]<-list(id="none",name="None",fun=identity,label=function(x) sprintf("%s",x))


MODEL_TYPE<-list()
MODEL_TYPE[["lm"]]<-list(id="lm",name="Linear",fun=stats::lm)
MODEL_TYPE[["nb"]]<-list(id="nb",name="Negative Binobial",fun=MASS::glm.nb)
MODEL_TYPE[["beta"]]<-list(id="beta",name="Beta",fun=betareg::betareg)
MODEL_TYPE[["pois"]]<-list(id="pois",name="Poisson",fun=stats::glm,opts=list(family=stats::poisson()))

METHOD_LABEL<-list()
METHOD_LABEL[["user"]]<-"User"
METHOD_LABEL[["step"]]<-"Step-wise"
METHOD_LABEL[["significant"]]<-"Significance"
