y<-rbeta(100,1,1)
y<-rpois(100,1)
x<-rnorm(100)
z<-rep(c(-1,1),5)
f<-factor(z)
contrasts(f)<- structure(matrix(c(-0.5, 0.5), ncol = 1), dimnames = list(NULL, ""))

model<-MASS::glm.nb(y~x)
extractAIC(model)
stats:::extractAIC.negbin

model<-betareg::betareg(y~x)
#model<-lm(y~f)
library(betareg)
summary(model)
summary(model)
stats::AIC(model)
step(model)
formula(model)
model$loglik
logLik(model)
library(NeuroStatsj)
as.numeric(performance::r2(model)[[1]])
formula(model)
model$call
a<-MASS::stepAIC(model,direction="both",trace=0)
summary(a)
a<-extractAIC(model,k=2)
df.resid
logLik(model)*2
AIC(model,k=2)

ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20 - numdead)
model <- glm(SF ~ sex*ldose, family = binomial)
k<-2
extractAIC(model,k=k)
AIC(model,k=k)
-2*logLik(model)+(k*4)
(AIC(model,k=k)+2*logLik(model))/k

deviance(model)
attributes(a$anova)              
a$anova         
attributes(a$anova)$heading[[5]]

              fixclasses<-function(){

  fixes<-yaml::yaml.load_file("jamovi/fixclasses.yaml")
  refs<-as.data.frame(do.call("rbind",fixes))
  refs$name<-unlist(refs$name)
  refs$class<-unlist(refs$class)
  files<-list.files("R/",".h.R")
  comms<-unlist(lapply(files, function(f) gsub(".h.R","",f,fixed=T)))
  for (i in seq_along(files)) {
    f<-files[[i]]
    txt<-readLines(paste0("R/",f))
    newtxt<-character()
    name<-comms[[i]]
    .class<-refs[refs$name==name,"class"]
    cat("Fixing",f,"for class",.class,"....")
    .search<-paste0(.class,"Results <- if")
    if (length(grep("classname",txt))>0) {
      cat("not necessary\n")
      next()
      
    }
    for (t in txt) {
      newtxt[[length(newtxt)+1]]<-t
      if (length(grep(.search,t,fixed=T))>0) {
        .classstring<-paste0("       classname=c(\"gamlj\",\"",.class,"\"),")
        newtxt[[length(newtxt)+1]]<-.classstring
        cat("done\n")
      }
    }
    writeLines(newtxt, con=paste0("R/",f))
  }
  
  
}


installme<-function(what) {
  library(what,character.only=TRUE)
  s<-sessionInfo()
  pkg<-s$otherPkgs[[what]]
  pv<-pkg$Version
  zf<-yaml::read_yaml("jamovi/0000.yaml")
  zv<-zf$version
  h<-git2r::repository_head()
  gv<-gsub("Version.","",h$name,fixed = T)
  gv<-gsub("version.","",gv,fixed = T)
  cat("yaml version:",zv,"\n")
  cat("pack version:",pv,"\n")
  cat("git version:",gv,"\n")
  
  if (all(c(pv,zv)==gv))
    jmvtools::install(home = "flatpak")
  else
    warning("versions mismatch")
  
}
