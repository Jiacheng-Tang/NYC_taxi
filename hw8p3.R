library(rpart)
library(alr4)
data("BGSboys")
dat = BGSboys[,c(1:6,8)]
summary(powerTransform(dat))
dat[,1] = log(dat[,1])
dat[,c(3,5)] = dat[,c(3,5)]%%(-1)
names(dat)[c(1,3,5)]=c("logWT2","invWT9","invLG9")

fit = rpart(HT18~.,dat=dat,method="anova",
            control = rpart.control(cp = 0.001, minsplit = 5))
fit
plot(fit)
text(fit,use.n=TRUE,cex=1.2)
printcp(fit)

fit.pruned = prune(fit,cp=.0215)
fit.pruned
plot(fit.pruned)
text(fit.pruned,use.n=TRUE)
printcp(fit.pruned)

preds = predict(fit.pruned)
mean((preds-dat$HT18)^2)