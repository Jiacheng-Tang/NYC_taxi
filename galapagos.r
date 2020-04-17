
# Now let's look at an example concerning the number of species of tortoise on the various Galapagos Islands.
# There are 30 cases (Islands) and 7 variables in the dataset. We start by reading the data into R and examining
# it (the data gala.txt is available on Carmen)

gala <- read.table("gala.txt", header=T) # read the data into R
gala

pairs(gala)

# The variables are:
# 
# Species - The number of species of tortoise found on the island
# Endemics - The number of endemic species
# Elevation - The highest elevation of the island (m)
# Nearest - The distance from the nearest island (km)
# Scruz - The distance from Santa Cruz island (km)
# Adjacent - The area of the adjacent island (km2)


# The data were presented by Johnson and Raven (1973) and also appear in Weisberg (1985). I have filled
# in some missing values for simplicity. 


# It looks like there is a potentially influential observation with very large Area.
# Let's find out which one it is.
which(gala$Area>2000)
gala[16,]

# We can calculate it's Cooks distance.
cooks.distance(lm(Species~.,dat=gala))
cooks.distance(lm(Species~.,dat=gala))[16] #Yikes!
qf(0.5,ncol(gala)+1,nrow(gala)-ncol(gala)-1)

# Removing it and replotting,
gala2=gala[-16,]
pairs(gala2)

# That helps, but the Area still is clumped to the left with a few larger observations.
# Maybe a transformation is warranted?
hist(gala2$Area)
hist(log(gala2$Area))

# Log looks a lot better!  What does box-cox say?
library(car)
summary(powerTransform(gala2$Area~1))

# So let's take the log transform.
gala3=gala2
gala3$Area=log(gala3$Area)
pairs(gala3)

# Now we notice there is also a very large value of Adjacent that is likely influential.
# There is also a somewhat large value of Elevation that doesn't seem to follow the pattern
# of the rest of the data.  Which one is the large Adjacent value?
which(gala3$Adjacent>2000)
gala3[12,]
cooks.distance(lm(Species~.,dat=gala3))
cooks.distance(lm(Species~.,dat=gala3))[12]
qf(0.5,ncol(gala3)+1,nrow(gala3)-ncol(gala3)-1)

# It looks like it is the same observation that has both large Adjacent and large Elevation.
# Removing it,
gala4=gala3[-12,]
pairs(gala4)

# This is looking a lot better, even if nothing is perfect.
# We can see it appears Species is related to Endemics, Area and Elevation.
# The relationship to Nearest, Scruz and Adjacent is not very clear, if any at all.
# There is also some multicollinearity between Edemics, Area and Elevation.  This is 
# not that surprising.
# We might also think there are 2 influential (large) values of Scruz but lets leave those for now.

# What if we do the MV Yeo-Johnson transform?  Might it suggest a transform of Adjacent?
summary(powerTransform(cbind(Endemics,Area,Elevation,Nearest, Scruz,Adjacent)~1,gala4,family="yjPower"))

# This suggests that while Area looks okay, actually there is some evidence for transfomring all the
# remaining predictors according to MV boxcox with YJ transforms.
# But let's leave things as they are for now.

# Our researcher is intersted in 2 things
# 1. a best overall model for predicting Species
# 2. a model relating Species to geographical properties of the islands, i.e. *not* Endemics.


# Let's work on #1 first.
fit1=lm(Species~Endemics+Area+Elevation+Nearest+Scruz+Adjacent,data=gala4)
summary(fit1)
anova(fit1)

# It looks like Endemics is strongly related and to some degree Area as well.
# The rest are not significant.
# However, we know there is multicollinearity between Endemics, Area and Elevation so
# these p-values can be a bit misleading.
# Overall this model is doing good, with an R^2 around 95% and the model F-test shows
# it is significant.

# For example, what if we put Area and Elevation first?
fit1b=lm(Species~Area+Elevation+Endemics+Nearest+Scruz+Adjacent,data=gala4)
anova(fit1b)

# Now all three are significant.  So depending on what is in our model, we may be able to 
# gain useful relationships with variables other than just Endemic, which could be helpful
# for question 2.

# Let's make some plots.
avPlots(fit1)
# These added-variable plot's agree with our model output above.

# Let's look at the residuals
plot(rstudent(fit1))
plot(predict(fit1),rstudent(fit1))
plot(gala4$Endemics,rstudent(fit1))
plot(gala4$Area,rstudent(fit1))
plot(gala4$Elevation,rstudent(fit1))
plot(gala4$Nearest,rstudent(fit1))
plot(gala4$Scruz,rstudent(fit1))
# Some are okay, some suggest non-constant variance and also possibly 1 or 2 outliers.

# What does the qqplot say?
qqPlot(fit1)
# Maybe a couple outliers and while the rest of the distribution is within the error bars,
# it is also consistently off the diagonal.

# Thinking back to our pairs plot
pairs(gala4)
# there are 4-ish observations much larger values of Speices than the rest and also 
# a curvilinear relationship between Species vs Endemics and Vs Area.  Elevation hard to tell.
# One potential fix could be using a quadratic in our regression model.
# Or, we could consider transforming y.  Since that's easy to check with boxcox,
# let's see what it says.

boxcox(fit1)
tst=powerTransform(fit1)
summary(tst)
testTransform(tst,c(0.5))


# Wow, this suggests very strongly a square-root transformation.
# Also check out some plots:

# the inverse fitted value plot
yhat=predict(fit1)
plot(gala4$Species,yhat,pch=20,cex=1.5,xlab="Species",ylab="Predicted Species",main="Inverse Fitted Plot")
plot(bcPower(gala4$Species,0.5),yhat,pch=20,cex=1.5,xlab="Sqrt(Species)",ylab="Predicted Species",main="Inverse Fitted Plot")



# Let's do this and re-fit.

gala5=gala4
gala5=transform(gala5,Species=bcPower(Species,0.5))
pairs(gala5)
# Well, it looks like the relationships are more linear although we still do have those larger
# values of Species by themselves.

# Let's repeat our modeling process and see what we get.
fit2=lm(Species~Endemics+Area+Elevation+Nearest+Scruz+Adjacent,data=gala5)
summary(fit2)
anova(fit2)

# Now only Endemics appears significant.

# What if we put Area and Elevation first?
fit2b=lm(Species~Area+Elevation+Endemics+Nearest+Scruz+Adjacent,data=gala5)
anova(fit2b)

# Relationship to the Area and Elevation is still there.

# Let's make some plots.
avPlots(fit2)
# These added-variable plot's agree with our model output above.
# Maybe a slightly greater potnetial for relationsihp to scruz and Adjancent than before.

# Let's look at the residuals
plot(rstudent(fit2))
plot(predict(fit2),rstudent(fit2))
plot(gala5$Endemics,rstudent(fit2))
plot(gala5$Area,rstudent(fit2))
plot(gala5$Elevation,rstudent(fit2))
plot(gala5$Nearest,rstudent(fit2))
plot(gala5$Scruz,rstudent(fit2))
# Looks a lot better than before - not much evidence for non-constant variance 
# But it looks like we still have maybe 1 outlier.

# What does the qqplot say?
qqPlot(fit2)
# Much better!
# Depending how many times you re-run that line, you might or might not have the slightest
# suggestion of 1 outlier.  So it doesn't seem too serious an issue.


# Based on our analysis so far, we might have a few ideas for recommending models.
# For #1, we might consider the model
# E[Y]=beta_0+beta_1*Endemics
#
# For #2, we might consider the model 
# E[Y]=beta_0+beta_1*Area+beta_2*Elevation+beta_3*Nearest+beta_4*Scruz+beta_5*Adjacent
# and maybe this could be simpfified, likely to
# E[Y]=beta_0+beta_1*Area+beta_2*Elevation


# Model 1
fit.mod1=lm(Species~Endemics,data=gala5)
summary(fit.mod1)

# Seems like a good model.  R^2 is 0.9429, R^2_a=0.9407 and
# model significance F-test has a p-value <2.2e-16.
qqPlot(fit.mod1)
plot(rstudent(fit.mod1))
plot(predict(fit.mod1),rstudent(fit.mod1))
plot(gala5$Endemics,rstudent(fit.mod1))

# the residuals look "okay" but maybe not as good as the full model from above.

powerTransform(fit.mod1)
# No further transformations suggested by boxcox.

# Model 2
fit.mod2=lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,data=gala5)
summary(fit.mod2)
# Looks like Area and Elevation are useful but not the others.
# So can we get away with the reduced model?
# Lets do a test and see.
fit.mod2b=lm(Species~Area+Elevation,data=gala5)
summary(fit.mod2b)
# Very little reduction in R^2 and the model significnace F-test is sitll highly significnat.

# Now we can do our GLT to direclty compare these two models.
# H0: beta_nearest=beta_scruz=beta_adjacent=0
# vs.
# H1: at least one of beta_nearest, beta_scruz, beta_adjacent is not 0.
anova(fit.mod2b,fit.mod2)

# We cannot reject the null.  So we do not have evidence against our null assumption.
# This suggests that our reduced model is adequate.

# Let's check our residuals, etc.
plot(rstudent(fit.mod2b))
plot(predict(fit.mod2b),rstudent(fit.mod2b))
plot(gala5$Area,rstudent(fit.mod2b))
plot(gala5$Elevation,rstudent(fit.mod2b))

# These residuals look generally pretty good, with possible exception of the Elevation
# plot.  There is maybe also the slightest hint that the residuals at very small
# predictor values have different spread than the rest.

# qq plot
qqPlot(fit.mod2b)

# on the other hand the qqplot looks great.

par(mfrow=c(1,2))
qqPlot(fit.mod1,main="Model in Endemics")
qqPlot(fit.mod2b,main="Model in Area+Elevation")
plot(rstudent(fit.mod1),main="Model in Endemics")
plot(rstudent(fit.mod2b),main="Model in Area+Elevation")
plot(predict(fit.mod1),rstudent(fit.mod1),main="Model in Endemics")
plot(predict(fit.mod2b),rstudent(fit.mod2b),main="Model in Area+Elevation")
plot(gala5$Area,rstudent(fit.mod1),main="Model in Endemics")
plot(gala5$Area,rstudent(fit.mod2b),main="Model in Area+Elevation")
plot(gala5$Elevation,rstudent(fit.mod1),main="Model in Endemics")
plot(gala5$Elevation,rstudent(fit.mod2b),main="Model in Area+Elevation")

# We can check the cooks distance also and the h_ii values from our hat matrix.
cooks.distance(fit.mod1)
influence(fit.mod1)$hat
qf(0.5,2,nrow(gala3)-2)

# Accorindg to our rule of thumb, it looks like SanSalvador could be influential.
# If one checks, this is also the observation that looks like a potential outlier.
# Interestingly, although the potential influence of SantaCruz is high, the actual
# cooks distance is not worriesome.

cooks.distance(fit.mod2b)
influence(fit.mod2b)$hat
qf(0.5,3,nrow(gala3)-3)

# Clearly, in terms of cooks distance our second model appears much better.

summary(fit.mod1)
summary(fit.mod2b)

# In terms of overall metrics, the R^2 is notably higher for our Endemics model
# although the R^2 of our Area+Elevation model is not that bad either.

# So, which model is "best"
# And what is a good model that only involves geographical predictors?

# It seems like we have answered #2 reasonably well.
# But maybe #1 is a bit more of a delicate matter. 
#   - we might consider a more complex model in Area+Elevation
#   - we might consider removing the outlier in the Endemics model

# We also can remember the pairs plot and the MV BoxCox on the predictors did 
# not leave us entirely satisfied.  Could look at doing more there:
pairs(gala5)
tst2=powerTransform(cbind(Endemics,Area,Elevation,Nearest, Scruz,Adjacent)~1,gala4,family="yjPower")
summary(tst2)
testTransform(tst2,c(0.5,1.0,0,0,0,-0.5)) #no
testTransform(tst2,c(0.33,1.0,0,0,0,-0.5)) #yes
testTransform(tst2,c(0.5,1.0,0,-0.25,0,-0.5)) #no
testTransform(tst2,c(0.33,1.0,0,0,0.25,-0.5)) #yes
testTransform(tst2,c(0.33,1.0,0,0,0.25,-0.25)) #yes --strongest

gala6=gala5
gala6=transform(gala6,Endemics=yjPower(Endemics,0.33),
					  Elevation=bcPower(Elevation,0),
					  Nearest=bcPower(Nearest,0),
					  Scruz=yjPower(Scruz,0.25),
					  Adjacent=yjPower(Adjacent,-0.25) )

pairs(gala6)
plot(gala6$Endemics,gala$Species[-c(12,16)])

# Now that is certainly interesting.

# Does it change what we would view as a 'good' transform of Species?
fitx=lm(gala$Species[-c(12,16)]~.,data=gala6)
boxcox(fitx)
tst=powerTransform(fitx)
summary(tst)
testTransform(tst,c(0.5))
testTransform(tst,c(0))
testTransform(tst,c(0.06))

# the inverse fitted value plot
yhat=predict(fitx)
plot(gala$Species[-c(12,16)],yhat,pch=20,cex=1.5,xlab="Species",ylab="Predicted Species",main="Inverse Fitted Plot")
plot(bcPower(gala$Species[-c(12,16)],0.06),yhat,pch=20,cex=1.5,xlab="Sqrt(Species)",ylab="Predicted Species",main="Inverse Fitted Plot")

# A power of 0.06 hardly seems to have obvious interpretation.
# And the low-noise relationship to Endemics suggests a curvilinear regression is
# what one will need rather than a fancy transformation of y.


# Example 3rd-order model in Endemics:
fit3rd=lm(gala$Species[-c(12,16)]~Endemics+I(Endemics^2)+I(Endemics^3),data=gala6)
summary(fit3rd)

par(mfrow=c(2,2))
plot(rstudent(fit3rd),main="Cubic model in Endemics")
plot(predict(fit3rd),rstudent(fit3rd),main="Cubic model in Endemics")
plot(gala6$Area,rstudent(fit3rd),main="Cubic model in Endemics")
plot(gala6$Elevation,rstudent(fit3rd),main="Cubic model in Endemics")
# evidence of an outlier and that there remains trend that has not been remoevd.



