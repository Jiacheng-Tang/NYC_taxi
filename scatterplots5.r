# Produce the scatterplots found in Notes5.




# Looking at the lakes.txt data
dat=as.data.frame(read.table("lakes.txt",header=TRUE))

y=dat$Species
X=cbind(dat$MaxDepth,dat$MeanDepth,dat$Elev,dat$Lat,dat$Long,dat$Dist,dat$NLakes)
colnames(X)=c("maxdepth","meandepth","elevation","latitude","longitude","distance","numlakes")
# See table 8.5 for a further description of the variables:
# Y = species : number of zooplankton species
# X1= maxdepth: maximum depth of lake in m
# X2=meandepth: mean lake depth in m
# X3=elevation: elevation in m
# X4=latitude : N latitude in degrees
# X5=longitude: W longitude in degrees
# X6= distance: distance to nearest lake in km
# X7= numlakes: number of lakes within 20 km's


# Transformations from "somewhere"
y=sqrt(y)
X[,1]=log(X[,1])
X[,2]=log(X[,2])
X[,3]=((X[,3]+2)^(0.25)-1)/0.25
X[,4]=(X[,4]^(-1.5)-1)/(-1.5)
X[,5]=sqrt(X[,5]+10)

which(X[,5]<6)  #influential, remove
# 68 69
ix=c(1:11,13:67)
y=y[ix]
X=X[ix,]

X[,6]=log(X[,6])
X[,7]=log(X[,7])



# Now we want to fit a model... which model?
# First, lets try the exhaustive search approach using the leaps package:

library(leaps) #nice all subsets regression functionality

regsubsets.out=regsubsets(y ~ X,
			   data = as.data.frame(cbind(y,X)),
               nbest = 2,       # 2 best models for each number of predictors
               nvmax = NULL,    # NULL for no limit on number of variables
               force.in = NULL, force.out = NULL,
               method = "exhaustive") # or "forward" for forward selection,
                                      # "backward" for backward elimination
regsubsets.out
sum.reg=summary(regsubsets.out)
sum.reg




# Make some plots of the output of regsubsets
# plot.regsubsets options:
#   scale= bic, Cp, adjr2 or r2
par(mfrow=c(1,3))
plot(regsubsets.out, scale = "r2", main = "R^2",cex.axis=2,cex.lab=2,cex=2)
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2",cex.axis=2,cex.lab=2,cex=2)
plot(regsubsets.out, scale = "Cp", main = "Mallow's Cp",cex.axis=2,cex.lab=2,cex=2)


# We can look at the actual values:
sum.reg$rsq
sum.reg$adjr2
sum.reg$cp



# plot for the best models
ixbest=c(1,3,5,7,9,11,13)
par(mfrow=c(1,2))
plot(ixbest,sum.reg$rsq[ixbest],pch=20,xlab="Model",ylab="R2, adjR2",cex.lab=2,cex.axis=2)
points(ixbest,sum.reg$adjr2[ixbest],pch=5)
abline(v=11)
plot(ixbest,sum.reg$cp[ixbest],pch=20,xlab="Model",ylab="Mallow's Cp",cex.lab=2,cex.axis=2)
abline(v=9)



# Another view of the results
# subsets() in leaps provides a nice plot of the output of regsubsets.
library(car)
layout(matrix(1:2, ncol = 2))
## Adjusted R2
res.legend=subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 2, main = "Adjusted R^2",cex.subsets=.5)
## Mallow Cp
res.legend=subsets(regsubsets.out, statistic="cp", legend = FALSE, min.size = 2, main = "Mallow Cp",cex.subsets=.5)
abline(a = 1, b = 1, lty = 2)



# Using Cp, plot suggests taking first model that intercepts p+1,
# in this case it's the model with maxdepth,elevation,longitude,distance and numlakes (Model #9)
# AdjR2 instead suggests Model #11

# What are the models 9 and 11?
sum.reg$which[9,]
# (Intercept)   Xmaxdepth  Xmeandepth  Xelevation   Xlatitude  Xlongitude 
#        TRUE        TRUE       FALSE        TRUE       FALSE        TRUE 
#   Xdistance   Xnumlakes 
#        TRUE        TRUE 

sum.reg$which[11,]
# (Intercept)   Xmaxdepth  Xmeandepth  Xelevation   Xlatitude  Xlongitude 
#        TRUE        TRUE       FALSE        TRUE        TRUE        TRUE 
#   Xdistance   Xnumlakes 
#        TRUE        TRUE 




# So lets fit to the model #9
fit=lm(y~maxdepth+elevation+longitude+distance+numlakes,dat=as.data.frame(cbind(y,X)))
summary(fit) # only maxdepth and longitude look significant
plot(fit$residuals)

# and to model #11
fit2=lm(y~maxdepth+elevation+latitude+longitude+distance+numlakes,dat=as.data.frame(cbind(y,X)))
summary(fit2) # only maxdepth and longitude look significant
plot(fit$residuals)

anova(fit,fit2)
# Anova suggests the adding latitude doesn't do much




# When feasible, such search algorithms are nice.  But we can also do variable
# selection by prescribing forward selection or backward elimination procedures
# using the step() function in R:

dat=read.table("mantel.txt",header=TRUE)

fit0=lm(Y~1,dat=dat)
fitfull=lm(Y~.,dat=dat)

# Forward selection
step(fit0,scope=list(lower=fit0,upper=fitfull),direction="forward")

# Backward elimination
step(fitfull,scope=list(lower=fit0,upper=fitfull),direction="backward")

# Stepwise regression
step(fit0,scope=list(lower=fit0,upper=fitfull),direction="both")



# Note that by default step does its FS/BE/SW using AIC.  We can get it
# to use BIC by setting the option k=log(n) where n is # of observations.
# Note: BIC is sometimes called SBC, but probably you won't run into that.

# Other helpful functions are add1() and drop1(), which can report amongst other
# things the F-test for adding/dropping a predictor.
# For example:
add1(fit0,Y~X1+X2+X3,test="F")

# So start with X3, and run add1() again
fit1=lm(Y~X3,dat=dat)
add1(fit1,Y~X1+X2+X3,test="F")

# Nothing significant, so try drop1()
drop1(fit1,test="F")
# which says nothing to drop.

# What if we were at a model with X1 and X3 and try drop1()
fit2=lm(Y~X1+X3,dat=dat)
drop1(fit2,test="F")
# so this says to drop X1.

# Note: Sum of Sq column refers to the SSR(new var | current model)
#       (ie additional reduction in SSE).
# Note: RSS refers to the SSE for the model that contains
#       the current variables and the new variable.



# A function to calculate PRESS for the linear model
#   fit is the output from running lm().
press<-function(fit)
{
	pr=resid(fit)/(1 - lm.influence(fit)$hat) #theoretical shortcut for linear model
	return(sum(pr^2))
}

# PRESS is also available in package qpcR, which we'll show next.



# Weird Examples
set.seed(99)
y=rnorm(30)
X=matrix(rnorm(30*20),ncol=20)
dat=as.data.frame(cbind(y,X))
fit0=lm(y~1,dat=dat)
fitfull=lm(y~.,dat=dat)
step(fit0,scope=list(lower=fit0,upper=fitfull),direction="forward")
# Ending model is y ~ V20 + V18 + V10 + V9 + V4... really?
fitstar=lm(formula = y ~ V20 + V18 + V10 + V9 + V4, data = dat)
summary(fitstar)
AIC(fitstar)
BIC(fitstar)
# and almost all of them were significant(!)
plot(y)
points(fitstar$residuals,pch=20)

# Another one
set.seed(11111)
y=rnorm(20)
x=rnorm(20)
X=cbind(x,x^2,x^3,x^4,x^5,x^6)
colnames(X)=c("x","x2","x3","x4","x5","x6")
dat=as.data.frame(cbind(y,X))
fit0=lm(y~1,dat=dat)
fitfull=lm(y~.,dat=dat)
step(fit0,scope=list(lower=fit0,upper=fitfull),direction="forward")
fitstar=lm(y~x4+x6,dat=dat)
summary(fitstar)
AIC(fitstar)
BIC(fitstar)
# Now this one is saying x^4 and x^6 are significant... but we don't really believe this!
plot(y)
points(fitstar$residuals,pch=20)
# What does PRESS say about this?
pr=press(fitstar)
#press is ~16.33
anova(fitstar) # while SSE is ~12.56
# Can also use PRESS in R package qpcR to calculate it:
library(qpcR)
PRESS(fitstar)$stat




# Regularization methods -- first, lets try Ridge regression
# we'll use the lakes data again.
dat=as.data.frame(read.table("lakes.txt",header=TRUE))

y=dat$Species
X=cbind(dat$MaxDepth,dat$MeanDepth,dat$Elev,dat$Lat,dat$Long,dat$Dist,dat$NLakes)
colnames(X)=c("maxdepth","meandepth","elevation","latitude","longitude","distance","numlakes")
# See table 8.5 for a further description of the variables:
# Y = species : number of zooplankton species
# X1= maxdepth: maximum depth of lake in m
# X2=meandepth: mean lake depth in m
# X3=elevation: elevation in m
# X4=latitude : N latitude in degrees
# X5=longitude: W longitude in degrees
# X6= distance: distance to nearest lake in km
# X7= numlakes: number of lakes within 20 km's


# Transformations from "somewhere"
y=sqrt(y)
X[,1]=log(X[,1])
X[,2]=log(X[,2])
X[,3]=((X[,3]+2)^(0.25)-1)/0.25
X[,4]=(X[,4]^(-1.5)-1)/(-1.5)
X[,5]=sqrt(X[,5]+10)

which(X[,5]<6)  #influential, remove
# 68 69
ix=c(1:11,13:67)
y=y[ix]
X=X[ix,]

X[,6]=log(X[,6])
X[,7]=log(X[,7])




# MASS has lm.ridge()
library(MASS)
dat=as.data.frame(cbind(y,X))

lambdas=seq(0,100,1)
fit.ridge=lm.ridge(y~., lambda=lambdas,dat=dat)
plot(lambdas,fit.ridge$GCV,type='l')
which(fit.ridge$GCV==min(fit.ridge$GCV))
lambdas[15:16]
# looks like lambda=14.5 is a reasonable estimate.

fit.ridge=lm.ridge(y~., lambda=14.5,dat=dat)
fit.ridge$coef
# latitude is close to zero which seems compatible with what we saw earlier.
# of course ridge regression won't eliminate a variable by exactly 
# estimating the coeficient to be zero.  So let's try lasso.


# packages lars and glmnet have lasso regression
library(lars)

fit.las=lars(X,y,type="lasso")
fit.las
plot(fit.las, plottype="coefficients")
plot(fit.las, plottype="Cp")

lambda.lasso=c(fit.las$lambda,0)
beta=coef(fit.las)
colors=rainbow(8)

# Make a nice plot
beta.scale=attr(fit.las$beta, "scaled:scale")
beta.rescaled=beta
for(j in 1:8){
    beta.rescaled[j,]=beta.rescaled[j,]*beta.scale
}

matplot(lambda.lasso, beta.rescaled, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
    ylab=expression(paste("rescaled ",hat(beta))), col=colors,cex=2,cex.lab=1,cex.axis=2)
text(rep(0, 7), beta.rescaled[8,], colnames(X), pos=4, col=colors,cex=2)
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)

# Which variables to keep?
cv=cv.lars(X,y,K=10,mode="step")
idx=which.max(cv$cv-cv$cv.error <= min(cv$cv))
coef(fit.las)[idx,]

# Pacakge glmnet does this a bit nicer:
library(glmnet)
cvfit=cv.glmnet(X, y)
coef(cvfit, s = "lambda.1se")
yhat=predict(cvfit,X,s="lambda.1se")
coef(cvfit, s = "lambda.min")
yhat2=predict(cvfit,X,s="lambda.min")

# For glmnet, 2 popular choices for lambda are 
# lambda.1se: largest lambda s.t. error is within 1 s.e. of the minimum
# lambda.min: lambda that minizes the mean cross-validated error

mean((y-yhat)^2)
mean((y-yhat2)^2)

# If we use glmnet directly (ie before cross-validation) we can get a path plot 
# versus log lambda.
glmfit=glmnet(X,y)
plot(glmfit,xvar="lambda")

# Note that you can also perform ridge regression using glmnet.
# To do so, simply set the parameter alpha=0 (the defualt is 1 which is LASSO, 
# while 0 corresponds to Ridge).
# This might be nice since you can use one package to perform both methods.
rrfit=cv.glmnet(X,y,alpha=0)
coef(rrfit,s="lambda.min")
yhat3=predict(rrfit,X,s="lambda.min")
mean((y-yhat3)^2)


# Finally, since both lars and glmnet are old-school (they require you to pass a 
# a deisgn matrix X and a response vector y), a shortcut to creating these
# matrices can be useful:
dat2=as.data.frame(cbind(X,y)) #put the txformed vars back into a dataframe
A=model.matrix(y~.,dat2)[,-1]
A
# The [,-] removes the intercept column.  This is very important since both lars and glment
# handle the intercept automiatically (since we don't want to shrink it).

B=model.matrix(y~.+I(maxdepth^2)+distance:elevation,dat2)[,-1]

