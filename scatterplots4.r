# Produce the scatterplots found in Notes4.


# Polynomial regression example
# The following data is from an experiment to determine the effect of planting
# rate X (in thousands of plants per acre) on yields Y (in bu/acre) of irrigated
# corn in Nebraska.  Several plots were planted at each of 5 planting rates.
#
# Planting rates	Yields
# 12				130.5	129.6
# 16				142.5	140.3	143.4
# 20				145.2 	144.9	144.2
# 24				147.8	146.6	148.4
# 28				134.8	135.1
#
# We'll look at fitting polynomial models to this data...

rate=c(12,12,16,16,16,20,20,20,24,24,24,28,28)
yield=c(130.5,129.6,142.5,140.3,143.4,145.2,144.9,144.2,147.8,146.6,148.4,134.8,135.1)

pdf("corndata.pdf",width=5,height=5)
plot(rate,yield,pch=20,cex=1.5,xlab="Planting Rate (1K plants/acre)",ylab="Corn Yield (bu/acre)")
dev.off()

# Try fitting a line
fit1=lm(yield~rate)
summary(fit1)
anova(fit1)

# LOF test for the line since we have replicates:
fit1.lof=lm(yield~0+as.factor(rate))
anova(fit1,fit1.lof)

# Try fitting a quadratic
rate2=rate^2
fit2=lm(yield~rate+rate2)
summary(fit2)
anova(fit2)

# LOF test for the quadratic since we have replicates:
anova(fit2,fit1.lof)

# Plot the fitted curve:
rate.pred=seq(min(rate),max(rate),length=100)
x.pred=as.data.frame(cbind(rate.pred,rate.pred^2))
colnames(x.pred)=c("rate","rate2")
yield.pred=predict(fit2,x.pred)
pdf("corndatafit2a.pdf",width=5,height=5)
plot(rate,yield,pch=20,cex=1.5,xlab="Planting Rate (1K plants/acre)",ylab="Corn Yield (bu/acre)")
lines(rate.pred,yield.pred,lwd=1.5)
dev.off()


# Plot the fitted plane in x,x^2 space:
library(rgl)
rate.pred=as.matrix(expand.grid(seq(min(rate),max(rate),length=10),
	seq(min(rate^2),max(rate^2),length=10)))
x.pred=as.data.frame(rate.pred)
colnames(x.pred)=c("rate","rate2")
yield.pred=predict(fit2,x.pred)
persp3d(unique(x.pred[,1]),unique(x.pred[,2]),matrix(yield.pred,10,10),
	xlab="Rate",ylab="Rate^2",zlab="Yield",col="grey")
exaggerated=(yield-predict(fit2))*20+predict(fit2)
points3d(rate,rate^2,exaggerated,col="red",size=7)
points3d(rate,rate^2,yield,col="blue",size=5)

# now add the curve fit by projecting to the yield vs rate plane
rate.pred=seq(min(rate),max(rate),length=100)
x.pred=as.data.frame(cbind(rate.pred,rate.pred^2))
colnames(x.pred)=c("rate","rate2")
yield.pred=predict(fit2,x.pred)
points3d(rate,784,yield,col="blue")
lines3d(rate.pred,784,yield.pred)

# and add the curve showing the relationship between the predictors
lines3d(rate.pred,rate.pred^2,0,col="grey",lwd=2)

# and plot it
rgl.snapshot("corndatafit2b.png")


# Cubic model:
fit3=lm(yield~rate+I(rate^2)+I(rate^3))
summary(fit3)
anova(fit3)
anova(fit3,fit1.lof)


# Quartic model:
fit4=lm(yield~rate+I(rate^2)+I(rate^3)+I(rate^4))
summary(fit4)
anova(fit4)
anova(fit4,fit1.lof)


# Look at residual plots of fits
par(cex=4)
plot(rate,fit1$residuals,pch='L',xlab="Rate",ylab="Residuals")
abline(h=0)
points(rate,fit2$residuals,pch='Q')
points(rate,fit3$residuals,pch='C',col="blue")
points(rate,fit4$residuals,pch='R',col="red")


# The quartic model passes right through the means of the observations:
plot(rate,yield,pch=20,cex=1.5,xlab="Rate",ylab="Yield")
xb=c(12,16,20,24,28)
yb=c((130.5+129.6)/2,(142.5+140.3+143.4)/3,(145.2+144.9+144.2)/3,(147.8+146.6+148.4)/3,(134.8+135.1)/2)
points(xb,yb,pch=20,col="grey",cex=2)
points(rate,fit4$fitted,pch=20,col="red")



# Calculate maximum and re-plot the fitted curve:
bhat0=coef(fit2)[1]
bhat1=coef(fit2)[2]
bhat2=coef(fit2)[3]
optimum=-bhat1/(2*bhat2)
MSE=sum(residuals(fit2)^2)/(13-3)
X=as.matrix(cbind(1,rate,rate^2))
V=MSE*solve(t(X)%*%X)
varOptim=1/(4*bhat2^2)*(V[2,2]+bhat1^2/bhat2^2*V[3,3]-2*bhat1/bhat2*V[2,3])
seOptim=sqrt(varOptim)

rate.pred=seq(min(rate),max(rate),length=100)
x.pred=as.data.frame(cbind(rate.pred,rate.pred^2))
colnames(x.pred)=c("rate","rate2")
yield.pred=predict(fit2,x.pred)
pdf("corndatafit2c.pdf",width=5,height=5)
plot(rate,yield,pch=20,cex=1.5,xlab="Planting Rate (1K plants/acre)",ylab="Corn Yield (bu/acre)")
lines(rate.pred,yield.pred,lwd=1.5)
abline(v=optimum,lwd=2)
abline(v=optimum-2*seOptim,lty=3)
abline(v=optimum+2*seOptim,lty=3)
dev.off()


# Bootstrap a CI for the optimum value.
B=1000
n=length(rate)
optimum.resam=rep(0,B)
for(i in 1:B)
{
	idx=sample(1:n,replace=TRUE)
	rate.resam=rate[idx]
	yield.resam=yield[idx]
	fit.resam=lm(yield.resam~rate.resam+I(rate.resam^2))
	bhat1.resam=coef(fit.resam)[2]
	bhat2.resam=coef(fit.resam)[3]
	optimum.resam[i]=-bhat1.resam/(2*bhat2.resam)
}
optimum.med=median(optimum.resam)
optimum.025=quantile(optimum.resam,0.025)
optimum.975=quantile(optimum.resam,0.975)

# Now redo our plot with both estimates and uncertainties.
pdf("corndatafit2d.pdf",width=5,height=5)
plot(rate,yield,pch=20,cex=1.5,xlab="Planting Rate (1K plants/acre)",ylab="Corn Yield (bu/acre)")
lines(rate.pred,yield.pred,lwd=1.5)
abline(v=optimum,lwd=2)
abline(v=optimum-2*seOptim,lwd=2,lty=3)
abline(v=optimum+2*seOptim,lwd=2,lty=3)
abline(v=optimum.med,lwd=1,col="red")
abline(v=optimum.025,lwd=2,lty=3,col="red")
abline(v=optimum.975,lwd=2,lty=3,col="red")
dev.off()



# Example of how to use orthogonalized polynomials in R
#
x=1:10
y=5+0.5*x+3*x^2-4*x^3+rnorm(10)
plot(x,y)
dat=cbind(x,y)
dat=as.data.frame(dat)
fit3=lm(y~1+x+I(x^2)+I(x^3),dat=dat)
summary(fit3)
fit2=lm(y~1+x+I(x^2),dat=dat)
summary(fit2)
fit2p=lm(y~poly(x,2),dat=dat)
summary(fit2p)
fit3p=lm(y~poly(x,3),dat=dat)
summary(fit3p)

# Predict can be used to map from the unorthogonalized "raw" x's to
# the orthognolized scale:
z=poly(x,3)
xnew=4.5
predict(z,newdata=xnew)

# one could also reconstruct ``znew'' using the orthogonal polynomial recursion
# and the alpha, and norm^2 values stored in the z object.
# For notation, let's call a_d the element in index d of alpha and let's call 
# n_d the element in index d of norm2. F_d(x) will be the orthogonal polynomial 
# of degree d that is generated. For some base cases we have:
# F_0(x) = 1 / sqrt(n_2)
# F_1(x) = (x-a_1) / sqrt(n_3)
# The rest are recursively defined as:
# F_d(x) = [(x-a_d) * sqrt(n_{d+1}) * F_{d-1}(x) - n_{d+1} / sqrt(n_d) * F_{d-2}(x)] / sqrt(n_{d+2})
attributes(z)$coefs
a=attributes(z)$coefs$alpha
n=attributes(z)$coefs$norm2
f0=1 /sqrt(n[2])
f1=(xnew-a[1]) / sqrt(n[3])
f2=((xnew-a[2]) * sqrt(n[3]) * f1 - n[3] / sqrt(n[2]) * f0) / sqrt(n[4])
f3=((xnew-a[3]) * sqrt(n[4]) * f2 - n[4] / sqrt(n[3]) * f1) / sqrt(n[5])
f1; f2; f3;

# When actually doing regression though, predict() (for the regression)
# can tell that an orthogonalized polynomial was used in lm() and so
# handles things automagically:
xnew=as.data.frame(matrix(xnew,ncol=1))
colnames(xnew)="x"
predict(fit3p,xnew)

# but we could always verify it manually too:
znew=predict(z,newdata=xnew[1,1])
bhat=coef(fit3p)
yhat=bhat[1]+znew%*%bhat[2:4] #should match predict(fit,xnew) from above



# Electronic Experiment example
temp=c(0,0,0,25,25,25,50,50,50,75,75,75,100,100,100)
life=c(53,50,47,60,62,58,67,70,73,65,68,62,58,62,60)
pdf("templife.pdf",width=5,height=5)
plot(temp,life,pch=20,cex=1.5,xlab="Temperature (deg C)",ylab="Component Lifetime")
dev.off()

# fit a 4th order polynomial using R's orthogonal polynomial features
fit=lm(life~poly(temp,4))
anova(fit)
# shows the SSR is 660.00

E=data.frame(poly(temp,4))
fit2=lm(life~E[,1]+E[,2]+E[,3]+E[,4])
anova(fit2)
# this gives us the sequential sums of squares
# check that they sum to 660:
187.5+433.93+38.57
#[1] 660


#
# Smoothing Splines - a simple 1d example without penalization
###################
size=c(1.42,1.58,1.78,1.99,1.99,1.99,2.13,2.13,2.13,2.32,2.32,2.32,2.32,2.32,2.43,2.43,2.78,2.98,2.98)
wear=c(4,4.2,2.5,2.6,2.8,2.4,3.2,2.4,2.6,4.8,2.9,3.8,3.0,2.7,3.1,3.3,3.0,2.8,1.7)
# scale size to 0,1
x=size-min(size)
x=x/max(x)

# Function to create a cubic spline assuming x in 0,1.
rk<-function(x,c)
{
	((c-.5)^2-1/12)*((x-.5)^2-1/12)/4-
	((abs(x-c)-.5)^4-(abs(x-c)-.5)^2/2+7/240)/24
}
# Function to make the design matrix including the splines
makeX<-function(x,knots)
{
	q=length(knots)+2
	n=length(x)
	X=matrix(1,nrow=n,ncol=q)
	X[,2]=x
	X[,3:q]=outer(x,knots,FUN=rk)
	X
}

pdf("splinesexample.pdf",width=11,height=11)
par(mfrow=c(2,2),cex.axis=1.5,cex.lab=1.5)
# Now we can try it.  Let's use 4 knots.
knots=1:4/5
X=makeX(x,knots)
fit=lm(wear~X-1)
xpred=0:100/100
Xp=makeX(xpred,knots)
plot(x,wear,xlab="size (rescaled)",ylab="wear",main="m=4",pch=20,cex=1.5)
lines(xpred,Xp%*%coef(fit),lwd=2)

# What if we change the number of knots?  How about 5 knots.
knots=1:5/6
X=makeX(x,knots)
fit=lm(wear~X-1)
xpred=0:100/100
Xp=makeX(xpred,knots)
plot(x,wear,xlab="size (rescaled)",ylab="wear",main="m=5",pch=20,cex=1.5)
lines(xpred,Xp%*%coef(fit),lwd=2)

# How about 6 knots.
knots=1:6/7
X=makeX(x,knots)
fit=lm(wear~X-1)
xpred=0:100/100
Xp=makeX(xpred,knots)
plot(x,wear,xlab="size (rescaled)",ylab="wear",main="m=6",pch=20,cex=1.5)
lines(xpred,Xp%*%coef(fit),lwd=2)

# Or 7 knots.
knots=1:7/8
X=makeX(x,knots)
fit=lm(wear~X-1)
xpred=0:100/100
Xp=makeX(xpred,knots)
plot(x,wear,xlab="size (rescaled)",ylab="wear",main="m=7",pch=20,cex=1.5)
lines(xpred,Xp%*%coef(fit),lwd=2)

dev.off()


# Now, select lambda by using the Generalized Cross-Validation (GCV) method.
library(mgcv) # package implementing smoothing spline regression
dat=as.data.frame(cbind(wear,size))
lambda=1e-8*1.5^(0:59)
V=rep(0,60)
for(i in 1:60)
{
	fit=gam(wear~size+s(size,bs="cr",k=7+2),sp=lambda[i],data=dat)
	V[i]=fit$gcv.ubre
}

pdf("gcvscore.pdf",width=5,height=5)
plot(lambda,V,xlab="lambda",ylab="GCV score",pch=20,type='o')
dev.off()

# Okay so lets do the fit with lambda=25.
fit=gam(wear~size+s(size,bs="cr",k=7+2),sp=25,data=dat)
xpred=seq(min(size),max(size),length=100)
xpred=data.frame(size=xpred)
ypred=predict(fit,newdata=xpred)

pdf("gcvpredict.pdf",width=5,height=5)
plot(size,wear,pch=20,cex=1.5,xlab="size",ylab="wear",main=expression(paste(lambda,"=25")))
lines(xpred$size,ypred,lwd=2)
dev.off()

# We can also see summary information from the mgcv fit object.
summary(fit)
anova(fit)

# Lets compare to a model fitted without the smooth:
fit2=gam(wear~size,data=dat)
summary(fit2)
anova(fit2)

anova(fit2,fit,test="F") # compare the two models and do an F test.



