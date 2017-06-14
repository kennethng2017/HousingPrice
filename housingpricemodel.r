#run a Linear R on below and send me results. Use 60 Training and 40 Test. Do Random Sampling.Â 


#1. With base attributes.

#2. With added attributes.

library(caret)
setwd("~/data/housingprice")

HometrainX <- read.table("Add_train.csv", header = TRUE, sep = ",")
HometrainY <- HometrainX$SalePrice

#get 60 train and 43 test data
set.seed(1)
Rows <- createDataPartition(HometrainY,
 p = (100/1459.0),
 list= FALSE)

index <- c(1:60)
trainset <- Rows[index,]

trainPredictors <- HometrainX[trainset,]
trainClasses <- HometrainY[trainset]


testPredictors <- HometrainX[-trainset,]
testClasses <- HometrainY[-trainset]

train_data <- trainPredictors
train_data$SalePrice <- trainClasses

linear_model <- lm(SalePrice ~ GarageArea + GrLivArea + TotalBsmtSF, data = train_data)


summary(linear_model)

lmPred1 <- predict(linear_model, testPredictors)

lmValues1 <- data.frame(obs = testClasses, pred = lmPred1)

defaultSummary(lmValues1)

linear_model2 <- lm(SalePrice ~ GarageArea + GrLivArea + TotalBsmtSF + unemplyRate + spOpen + spHigh + spLow + spClose + spAdjClose + spVolume +CCIValue + fedfunds_rate + prime_rate, data = train_data)


summary(linear_model2)

lmPred2 <- predict(linear_model2, testPredictors)

lmValues2 <- data.frame(obs = testClasses, pred = lmPred2)

defaultSummary(lmValues2)

