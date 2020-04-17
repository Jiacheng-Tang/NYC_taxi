# Produce the scatterplots found in Notes6.


# Example of CART in R.
library(rpart)

dat=read.csv("treedata.csv")
# look at red oaks only:
dat2 = subset(dat,dat$species=="Quercus rubra")
ro.plots = unique(as.character(dat2$plotID))     # plots with red oak
u.plots = unique(as.character(dat$plotID))       # all plots
nob.plots = u.plots[is.element(u.plots,ro.plots)==F] # plots without red oak
dat3 = subset(dat,is.element(as.character(dat$plotID),nob.plots)) # dataset of no red oak
dat4 = subset(dat3,duplicated(dat3$plotID)==F)   # one row per plot
dat4$cover = 0              # cover of red oak is zero in these plots
rodat = rbind(dat2,dat4)    # new dataframe of presences and absences

# Fit a CART Model for classification
fit = rpart(factor(cover>0)~utme+utmn+elev+tci+streamdist+beers,dat=rodat,method="class")
fit
plot(fit)
text(fit,use.n=TRUE,cex=0.5)  # here cex will size the text labelling of nodes/leafs.

# Rpart uses the Gini index by default.  It also lists a complexity parameter which is the 
# cost of performing an additional split.  By default it won't split nodes that have a 
# complexity parameter less than 0.01 (but you can change this minimum).
summary(fit)

# We can look at the cross-validated error "xerror" as we grow the tree by the printcp function:
# Notice the errror starts to incrase again after a tree with 4 nodes.
printcp(fit)

# It looks like a tree of depth 4 or 5 is good but we can decrease the min cp to make sure.
fit = rpart(factor(cover>0)~utme+utmn+elev+tci+streamdist+beers,dat=rodat,method="class", control = rpart.control(cp = 0.0001))
printcp(fit)
plotcp(fit)

# The rule of thumb according to the plot would have us take cp=0.02
# So lets prune the tree based on that (we should get a tree with 4 nodes).
fit.pruned = prune(fit,cp=.02) 
fit.pruned
summary(fit.pruned)
plot(fit.pruned)
text(fit.pruned,use.n=TRUE,cex=1.2)

# Since this is a classification tree, lets calculate the misclassification rate.
preds = predict(fit.pruned,type="class") 
obs = factor(rodat$cover>0)  # presence/absense of red oak
sum(as.numeric(preds!=obs))    


# How about a continuous response?
# Let's try the housing data
dat=read.table("housing.txt",header=FALSE)
colnames(dat)=c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax",
				"ptratio","b","lstat","medv")

fit = rpart(medv~.,dat=dat,method="anova",control = rpart.control(cp = 0.001))
fit
summary(fit)
# Error just starts to incrase at the very bottom of the xerror list.  But that is a pretty deep tree!

plot(fit)
text(fit,use.n=TRUE,cex=1.2)

printcp(fit)
plotcp(fit)
# Looks like a cutoff of 0.022 or so would be reasonable.


fit.pruned = prune(fit,cp=.022)
fit.pruned
summary(fit.pruned)

plot(fit.pruned)
text(fit.pruned,use.n=TRUE,cex=1.2)

printcp(fit.pruned)
plotcp(fit.pruned)

# Now lets calculate the predicted values
preds = predict(fit.pruned,type="vector")
mean((preds-dat$medv)^2)

# By comparison, the *full* MLR model had an in-sample MSE of 21.89 
# and a mean CV error of 33.36.

# Here is the same CV applied to our tree model:
K=10
if(!isTRUE((K>1) && K<=nrow(dat)))
        stop("Number of folds is invalid (K=",K," n=",nrow(dat),"\n")
folds = ceiling(seq(1,nrow(dat),by=nrow(dat)/K)) #folds[i] is index of beginning of subset
cv.errors=matrix(NA,K)
for(i in 1:K)
{
	tx=folds[i]:(folds[i]+nrow(dat)/K-1)
	dat.train=dat[-tx,,drop=FALSE]
	dat.test=dat[tx,,drop=FALSE]
	fit=rpart(medv~.,dat=dat,method="anova",control = rpart.control(cp = 0.001))
	fit = prune(fit,cp=.022)
	preds=predict(fit,newdata=dat.test,type="vector")
	cv.errors[i]=mean((dat$medv[tx]-preds)^2)
}
sqrt(mean(cv.errors))

# The CV MSE estimate is 17.7245, much smaller than the MLR model.


# Note: There is also a tree library in R.  It is somewhat easier to use than rpart, 
#       but rpart is faster, which may be relevant for larger datasets.





# GLM's in R
# We use the glm(formula,family,data,weights,...) function.  
# This looks mostly like the lm() function did except for the family
# parameter, which allows us to specify the choice of variance and link functions:
#
# Family 	Variance 	Link
# Gaussian 	Gaussian	identity
# binomial	binomial	logit,probit or cloglog
# Poisson	Poisson		log,identity or sqrt
# gamma		gamma		inverse,identity, or log
# inverse.gaussian	inverse.gaussian 	1/mu^2
#
# As long as you want the default link (first one listed), you don't need to
# specify the link.

# Example of Logistic Regression on the CompAnalyst.txt dataset
dat=read.table("CompAnalyst.txt",header=T)

pdf("logistic1.pdf",width=5,height=5)
plot(dat,xlab="Months Experience",ylab="Task Success", pch=20,cex=1.5)
dev.off()

# Fit logistic regression
fit = glm(TaskSuccess ~ MonthsExperience, data=dat, family=binomial(link="logit"))
summary(fit)

# we can also get confidence intervals
confint(fit)

# and on exponential scale (i.e. odds ratio)
exp(coef(fit))
exp(confint(fit))

# Plot fitted logistic function and data
pdf("logistic3.pdf",width=5,height=5)
plot(dat,xlab="Months Experience",ylab="Task Success", pch=20,cex=1.5,xlim=c(4,32))
predMonths=seq(4,32,by=1)
logit.hat=predict.glm(fit,newdata=data.frame(MonthsExperience=predMonths),se.fit=TRUE)
# fitted probabilities
prob.hat=exp(logit.hat$fit)/(1+exp(logit.hat$fit))
lines(predMonths,prob.hat)
# 95% interval for probabilities
prob.lwr=exp(logit.hat$fit-1.96*logit.hat$se.fit)/(1+exp(logit.hat$fit-1.96*logit.hat$se.fit))
prob.upr=exp(logit.hat$fit+1.96*logit.hat$se.fit)/(1+exp(logit.hat$fit+1.96*logit.hat$se.fit))
lines(predMonths,prob.lwr,lty=2)
lines(predMonths,prob.upr,lty=2)
dev.off()

# We can also plot the logit response
pdf("logistic4.pdf",width=5,height=5)
plot(predMonths,logit.hat$fit,type='l',xlab="Months Experience",ylab="logit")
lines(predMonths,logit.hat$fit-1.96*logit.hat$se.fit,lty=2)
lines(predMonths,logit.hat$fit+1.96*logit.hat$se.fit,lty=2)
dev.off()

# aside:
# this doesn't work, it excludes the intercept for some reason???
#lines(predict.glm(fit,newdata=data.frame(MonthsExperience=seq(5,30,by=1)),type="response"))

library(car)
res=residuals(fit)
par(mfrow=c(1,2))
plot(res,pch=20)
qqPlot(res)

# Residual plots are dicey when there is only 1 observation per covariate pattern.
# For example, these are not useful:
plot((predict(fit)),res,pch=20)
plot(dat$MonthsExperience,res,pch=20)

# R's plot.glm also gives some additional nice plots
par(mfrow=c(2,2))
plot(fit)

# And car's influencePlot is useful as well.
influencePlot(fit)


# We can compare nested models using anova.
fit0=glm(TaskSuccess~1,data=dat,family=binomial(link="logit"))
anova(fit0,fit)

# Since fit0 here is just the null model, this is the same as 
anova(fit0)

# we can ask for the p-value of the chisq test using
anova(fit0,fit,test="Chisq")
anova(fit,test="Chisq")

# here we can see that the null model is further away from the saturated model (Resid.dev=34.296)
# than the model with MonthsExperience is away from the saturated model (Resid.dev=25.425)
# and that the difference in the deviances (8.8719) is significant, indicating
# that the model with MonthsExperience is useful.



# Multple logistic regression example
#
# Example of Logistic Regression on the engall.csv dataset
# We have covariates Age and Years of Service, both quantitative continuous 
# The response is binary indicating termination or non-termination of employee
#
dat=read.csv("engall.csv",header=T)

# Fit logistic regression
fit = glm(Terminated ~ Age + YrofService, data = dat, family = binomial(link="logit"))
summary(fit)

res=residuals(fit)
par(mfrow=c(1,2))
plot(res,pch=20,cex=2,cex.lab=2,cex.axis=2)
qqPlot(res)

par(mfrow=c(2,2))
plot(fit)

influencePlot(fit)



# Binomial Logistic Regression example
#
# Example of Binomial Regression on the challenger data
# If the responses Y_i are binomial(n_i,p_i), we need to create a variable
# (call it n) whose entries are the n_i and add it to our data matrix.
# Then we specify n in the weights option in the call to glm.
dat=read.table("challeng.txt", header = T)
dat$n
# We have 6 replicates observations for each unique covariate setting now.  This is
# still on the small side but may be enough to make the residual plots more useful.

# plot data
pdf("binomial1.pdf",width=9,height=9)
par(mfrow=c(2,2))
plot(dat$Temp,dat$Fail,xlab="Temperature",ylab="Failure Counts", pch=20,cex=1.5)
plot(dat$Pres,dat$Fail,xlab="Pressure",ylab="Failure Counts", pch=20,cex=1.5)
plot(dat$Temp,dat$Fail/dat$n,xlab="Temperature",ylab="Proportion of Failures", pch=20,cex=1.5)
plot(dat$Pres,dat$Fail/dat$n,xlab="Pressure",ylab="Proportion of Failures", pch=20,cex=1.5)
dev.off()

fit = glm(Fail/n ~ Temp + Pres, family = binomial(link="logit"), data = dat, weights = n)
summary(fit)

# Alternatively:
fit = glm(cbind(Fail,n-Fail) ~ Temp + Pres, family = binomial(link="logit"), data = dat)
summary(fit)

# we can also get confidence intervals
confint(fit)

# and on exponential scale (i.e. odds ratio)
exp(coef(fit))
exp(confint(fit))


# Plot fitted logistic function and data
pdf("binomial3.pdf",width=5,height=5)
plot(dat$Temp,dat$Fail/dat$n,xlab="Temperature",ylab="Fitted Probability of Failure", pch=20,cex=1.5,xlim=c(53,81))
predTemp=seq(53,81,by=1)
unique(dat$Pres)

logit.hat.low=predict.glm(fit,newdata=data.frame(Temp=predTemp,Pres=50),se.fit=TRUE)
logit.hat.med=predict.glm(fit,newdata=data.frame(Temp=predTemp,Pres=100),se.fit=TRUE)
logit.hat.hgh=predict.glm(fit,newdata=data.frame(Temp=predTemp,Pres=200),se.fit=TRUE)
# fitted probabilities
prob.hat.low=exp(logit.hat.low$fit)/(1+exp(logit.hat.low$fit))
prob.hat.med=exp(logit.hat.med$fit)/(1+exp(logit.hat.med$fit))
prob.hat.hgh=exp(logit.hat.hgh$fit)/(1+exp(logit.hat.hgh$fit))

lines(predTemp,prob.hat.low,col="blue")
lines(predTemp,prob.hat.med,col="green")
lines(predTemp,prob.hat.hgh,col="red")
legend(74,0.32,legend=c("Pres=50","Pres=100","Pres=200"),lty=rep(1,3),col=c("blue","green","red"))
dev.off()

# We can also plot the logit response
pdf("binomial4.pdf",width=5,height=5)
plot(predTemp,logit.hat.low$fit,type='l',xlab="Temperature",ylab="logit",col="blue",ylim=c(-7,1.1),lwd=3)
lines(predTemp,logit.hat.low$fit-1.96*logit.hat.low$se.fit,lty=2,col="blue")
lines(predTemp,logit.hat.low$fit+1.96*logit.hat.low$se.fit,lty=2,col="blue")
lines(predTemp,logit.hat.med$fit,col="green",lwd=3)
lines(predTemp,logit.hat.med$fit-1.96*logit.hat.med$se.fit,lty=2,col="green")
lines(predTemp,logit.hat.med$fit+1.96*logit.hat.med$se.fit,lty=2,col="green")
lines(predTemp,logit.hat.hgh$fit,col="red",lwd=3)
lines(predTemp,logit.hat.hgh$fit-1.96*logit.hat.hgh$se.fit,lty=2,col="red")
lines(predTemp,logit.hat.hgh$fit+1.96*logit.hat.hgh$se.fit,lty=2,col="red")
dev.off()

# and look at sequential tests by anova
anova(fit,test="Chisq")

# Residual plots
res=residuals(fit)
par(mfrow=c(1,2))
plot(res,pch=20)
qqPlot(res)

# Still here many of these residual plots are not useful.
plot((predict(fit)),res,pch=20,cex=2,cex.lab=2)
plot(dat$Temp,res,pch=20,cex=2,cex.lab=2)
plot(dat$Pres,res,pch=20,cex=2,cex.lab=2)

# R's plot.glm also gives some additional nice plots
par(mfrow=c(2,2))
plot(fit)

# Overall though, it looks like there is potential for outlier(s) and an influential point.

# And car's influencePlot is useful as well.
influencePlot(fit)






### Poisson regression example.
# The Miller lumber company is a large retailer of lumber and paint as well as plumbing, electrical
# and other household supplies.  During a representative 2-week period, in-store surveys were conducted
# and addresses of customers obtained.  The addresses were used to indentify the metro area census
# tract where the customer resides.  At the end of the survey period, the total number of customers
# who visited the store from each census tract within a 10-mile radius was determined and relevant
# demographic information for each tract (avg income, # housing units, etc) was obtained.
# Several other variables expected to be related to customer counts were constructed including
# distance from census tract to nearest competitor and distance to store.
# The variables were:
# houses: number of housing units
# income: average income in dollars
# age: average housing unit age
# dist: distance to nearest competitor in miles
# sdist: distance to store in miles
# numcust: number of customers who visited the store from census tract

dat=read.table("miller.txt", header = FALSE)
colnames(dat)=c("numcust","houses","income","age","dist","sdist")

# Plot counts versus predictors
plot(dat,pch=20)

# Since log is the link, makes sense to look at log(numcust) versus predictors as well
any(dat$numcust==0)
# But since we have some 0 counts, i'll use log(1+numcust)
dat2=dat
dat2$numcust=1+dat$numcust
plot(dat2,pch=20)

# Patterns look the same in this case.
# Looks like houses has a potentially weak relationship with numcust 
# while dist and sdist are stronger but in opposite directions
# with dist and sdist being collinear (negatively) although there is something
# more complex going on with larger values of sdist.

fit = glm(numcust ~ ., family = poisson(link="log"), data = dat)
summary(fit)

# we can also get confidence intervals
confint(fit)

# and on exponential scale
exp(coef(fit))
exp(confint(fit))

# For instance, it looks like a 1-unit increase in houses causes approximately an exp(1)=2.718
# multiplicative increase in the mean rate of occurences.
# For a variable we would need the cofficient to be estiamted around 0 since exp(0)=1 which
# would indicate no multiplicative effect.


# Plot fitted inverse link function and data
pdf("poisson1.pdf",width=5,height=5)
plot(dat$numcust,dat$dist,xlab="Competitor Distance",ylab="Number of Customers", pch=20,cex=1.5)
preddist=seq(from=0.34,to=6.61,length=100)

log.hat=predict.glm(fit,newdata=data.frame(houses=mean(dat$houses),income=mean(dat$income),age=mean(dat$age),dist=preddist,sdist=mean(dat$sdist)),se.fit=TRUE)
# fitted counts
lam.hat=exp(log.hat$fit)
lam.hat.low=exp(log.hat$fit-1.96*log.hat$se.fit)
lam.hat.high=exp(log.hat$fit+1.96*log.hat$se.fit)

lines(lam.hat,preddist,col="blue")
lines(lam.hat.low,preddist,col="blue",lty=2)
lines(lam.hat.high,preddist,col="blue",lty=2)
legend(74,0.32,legend=c("Pres=50","Pres=100","Pres=200"),lty=rep(1,3),col=c("blue","green","red"))
dev.off()

# Plot fitted link function
pdf("poisson2.pdf",width=5,height=5)
log.hat.pred=log.hat$fit
log.hat.low=log.hat$fit-1.96*log.hat$se.fit
log.hat.high=log.hat$fit+1.96*log.hat$se.fit
plot(preddist,log.hat.pred,type='l',xlab="Competitor Distance",ylab="Fitted Link")
lines(preddist,log.hat.low,lty=2)
lines(preddist,log.hat.high,lty=2)
dev.off()

# and look at sequential tests by anova
anova(fit,test="Chisq")

# How does our model compare to a null model?
fit0=glm(numcust ~ 1, family = poisson(link="log"), data = dat)
anova(fit0,fit,test="Chisq")
# cleary it is more useful than interept only.

# What about a model only in houses, income, dist and sdist?
fit2=glm(numcust~houses+income+dist+sdist,family=poisson(link="log"),data=dat)
anova(fit2,fit,test="Chisq")
# some evidence that one still needs age.

# A quicker way might be this:
library(car)
Anova(fit)

# Looks like all the varaibles are adding something after accounting for the others.


# Residual plots
pdf("poisson3.pdf",width=10,height=5)
res=residuals(fit)
par(mfrow=c(1,2))
plot(res,pch=20,cex=2)
qqPlot(res,cex=2)
dev.off()

# Residuals look reasonable.  There are a few large negative residuals which the textbook
# says are resulting from areas with no customers (so count=0).

par(mfrow=c(1,1))
plot((predict(fit,type="response")),res,pch=20)
# Maybe weak evidence that the variance is changing with the mean?  It's hard to tell 
# since we don't have that many observations with large predicted counts.

# R's plot.glm also gives some additional nice plots
par(mfrow=c(2,2))
plot(fit)

# Overall though, it looks like there is potential for outlier(s) with those couple observations with 0 counts.

# And car's influencePlot is useful as well.
influencePlot(fit)

# clearly obs 7 is one of those 0 counts:
dat[7,]

# but what is obs 43?
dat[43,]
range(dat$dist)



