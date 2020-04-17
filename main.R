source("taxi_filter.R")
taxi_filter("yellow_tripdata_2019-12.csv", "08", "Manhattan")

source("taxi_cleaning.R")
taxi_cleaning("yellow_tripdata_2019-12_08_Manhattan.csv")

# Load corrected data
library(readr)
type.list <- cols(
  VendorID = col_integer(),
  passenger_count = col_integer(),
  trip_distance = col_double(),
  PULocationID = col_integer(),
  DOLocationID = col_integer(),
  payment_type = col_integer(),
  fare_amount = col_double(),
  total_amount = col_double(),
  trip_date = col_integer(),
  trip_time = col_integer(),
  trip_weekday = col_integer(),
  PU_long = col_double(),
  PU_lat = col_double(),
  DO_long = col_double(),
  DO_lat = col_double()
)
dat <- as.data.frame(read_csv("yellow_tripdata_2019-12_08_Manhattan_corrected.csv",
                              col_types = type.list))
rm(type.list)
nrow(dat)
hist(dat$trip_date,breaks = 0:31)

# Remove holiday season
dat <- dat[dat$trip_date<22,]
nrow(dat)

# Get demand from data
source("taxi_demand.R")
AAM <- taxi_demand(dat)
demand <- 3600/AAM
demandPU$demand <- rowSums(3600/demand, na.rm = TRUE)
demandPU$ID <- names(demand)
demandDO <- colSums(3600/demand, na.rm = TRUE)

# Get random samples of size 10000
library(dplyr)
set.seed(500)
sample.1 <- sample_n(dat, 10000)
sample.2 <- sample_n(dat, 10000)

# Do analysis on sample set 1
library(alr4)
attach(sample.1)

# Do SLR without transformation
model.1 <- lm(trip_time~trip_distance)
model.1LOF <- lm(trip_time~0 + factor(trip_distance))
summary(model.1)
# Plot fitted line
plot(trip_distance,trip_time)
abline(coef(model.1))
# Check LOF
anova(model.1,model.1LOF)
# Check residual plot
plot(trip_distance,residuals(model.1))
# Check transformation for predictors
summary(powerTransform(trip_distance))

# transform predictor
plot(log(trip_distance),trip_time)
model.2 <- lm(trip_time~log(trip_distance))
plot(trip_distance,residuals(model.2))
inverseResponsePlot(model.2)

# transform response
model.3 <- lm(log(trip_time) ~ log(trip_distance))
model.3LOF <- lm(log(trip_time) ~ 0 + factor(log(trip_distance)))
summary(model.3)
# Plot fitted line
plot(log(trip_distance),log(trip_time))
abline(coef(model.3))
# Check LOF
anova(model.3,model.3LOF)
# Check residual plot
plot(log(trip_distance),residuals(model.3))
abline(h=0)

# do log transformation on dataset
names(sample.1)
sample.1a <- sample.1[,-(1:11)]
names(sample.1a)
sample.1a$trip_distance.log = log(trip_distance)
sample.1a$trip_time.log = log(trip_time)
sample.1a$trip_weekday = as.factor(trip_weekday)

# Try exhaustive search
require(leaps)
X <- model.matrix(trip_time.log ~ ., sample.1a)[, -1]
y <- sample.1a$trip_time.log
model.exh <- regsubsets(y ~ X,
			   data = sample.1a,
               nbest = 2,       # 2 best models for each number of predictors
               nvmax = NULL,    # NULL for no limit on number of variables
               force.in = NULL, force.out = NULL,
               method = "exhaustive") # or "forward" for forward selection,
                                      # "backward" for backward elimination
par(mfrow=c(2,2))
plot(model.exh, scale = "r2", main = "R^2")
plot(model.exh, scale = "adjr2", main = "Adjusted R^2")
plot(model.exh, scale = "bic", main = "BIC")
plot(model.exh, scale = "Cp", main = "Mallow's Cp")

# Try stepwise regression
model.0 <- lm(trip_time.log ~ 1, data = sample.1a)
model.full <- lm(trip_time.log ~ ., data = sample.1a)
model.step <- step(model.0, scope = list(upper=model.full), data=sample.1a, direction="both")

# use 4 predictors
model.v4 <- lm(trip_time.log ~ trip_distance.log + trip_weekday + PU_lat + DO_lat,
			   data = sample.1a)
summary(model.v4)

model.v4LOF <- lm(trip_time.log ~ as.factor(trip_distance.log) + as.factor(trip_weekday) +
			   as.factor(PU_lat) + as.factor(DO_lat), data = sample.1a)
anova(model.v4,model.v4LOF)

# use 6 predictors
model.v6 <- lm(trip_time.log ~ trip_distance.log + trip_weekday + PU_lat + DO_lat + PU_long + DO_long,
			   data = sample.1a)
summary(model.v6)

model.v6LOF <- lm(trip_time.log ~ as.factor(trip_distance.log) + as.factor(trip_weekday) +
			as.factor(PU_lat) + as.factor(DO_lat) + as.factor(PU_long) + as.factor(DO_long),
			data = sample.1a)
anova(model.v6,model.v6LOF)
anova(model.v4,model.v6)

# Try binary tree approach
library(rpart)
model.binTr <- rpart(trip_time.log ~ ., dat=sample.1a, method="anova",
                     control = rpart.control(cp = 0.0001, minsplit = 500))
printcp(model.binTr)
model.binTr2 <- prune(model.binTr, cp=.01)
par(mfrow=c(1,1))
plot(model.binTr2)
text(model.binTr2,use.n=TRUE)
printcp(model.binTr2)

# Calculate in-sample MSE for all models
preds.binTr2 = predict(model.binTr2,type="vector")
mean((preds.binTr2-sample.1a$trip_time.log)^2)
preds.1 = predict(model.1)
mean((log(preds.1)-sample.1a$trip_time.log)^2)
preds.3 = predict(model.3)
mean((preds.3-sample.1a$trip_time.log)^2)
preds.v4 = predict(model.v4)
mean((preds.v4-sample.1a$trip_time.log)^2)
preds.v6 = predict(model.v6)
mean((preds.v6-sample.1a$trip_time.log)^2)

# Try hybrid model build binary tree on top of MLR residuals
model.reduced <- lm(trip_time.log ~ trip_distance.log + trip_weekday, data=sample.1a)
summary(model.reduced)
model.reducedLOF <- lm(trip_time.log ~ as.factor(trip_distance.log) + trip_weekday, data=sample.1a)
anova(model.reduced,model.reducedLOF)
names(sample.1a)
sample.1b <- sample.1a[,2:5]
sample.1b$Res <- residuals(model.reduced)

# Generate binary tree using geometric data
model.hy <- rpart(Res~., dat=sample.1b, method="anova",
                     control = rpart.control(cp = 0.0001, minsplit = 500))
printcp(model.hy)
model.hy2 <- prune(model.hy, cp=.01)
plot(model.hy2)
text(model.hy2,use.n=TRUE)
printcp(model.hy2)
preds.hy2 = predict(model.hy2,type="vector")
mean((preds.hy2-sample.1b$Res)^2)

# Check splits
model.hy2$splits[!(duplicated(model.hy2$split[,1])|(model.hy2$split[,1]==0)),4]

source("taxi_map.R")
main.layer <- taxi_map("Queens")
main.layer

# Try binary splits on main layer
source("taxi_binarySplits.R")
taxi_binarySplits(main.layer,model.binTr2)
taxi_binarySplits(main.layer,model.hy2)

detach(sample.1)