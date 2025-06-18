y<-rbeta(100,1,1)
y<-rpois(100,1)
x<-rnorm(100)
z<-rep(c(-1,1),5)
f<-factor(z)
contrasts(f)<- structure(matrix(c(-0.5, 0.5), ncol = 1), dimnames = list(NULL, ""))

model<-MASS::glm.nb(y~x)
betareg::betar_family()

model.response(model.frame(model))
model<-betareg::betareg(y~x)
model$pseudo.r.squared
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


library(betareg)
library(MASS)

ops<-list(formula=y~x,data="data")
q<-"stats::lm"
as.call(c(parse(text = "betareg::betareg")[[1]], ops))

fit <- betareg(Ozone/100 ~ Wind + Temp, data = airquality, subset = !is.na(Ozone))

stepAIC(fit)
