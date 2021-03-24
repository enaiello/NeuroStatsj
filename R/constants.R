### these controls debug in console
NSj_DEBUG=T
NSj_INFO=T

## this defines how to transform variables. The name should correspond to `covTransformation` names defined in .a.yaml
TINFO<-list()
TINFO[["linear"]]<-list(template="_._VAR_._",require="linear")

TINFO[["quadratic"]]<-list(template="I(_._VAR_._^2)",require=FALSE)
TINFO[["cubic"]]<-list(template="I(_._VAR_._^3)",require=FALSE)
TINFO[["sqrt"]]<-list(template="I(_._VAR_._^.5)",require=FALSE)
TINFO[["log"]]<-list(template="I(log(_._VAR_._))",require=FALSE)
TINFO[["log10"]]<-list(template="I(log10(_._VAR_._))",require=FALSE)
TINFO[["reciprocal"]]<-list(template="I(1/_._VAR_._)",require=FALSE)

