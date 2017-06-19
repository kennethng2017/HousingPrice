library(caret)
library(doSNOW)
library(Metrics)
library(elasticnet)

load("/Users/ken/Documents/HousingPrice/complete_data5.Rdata")

set.seed(1)

Rows <- createDataPartition(HometrainY,
                            p = .80,
                            list= FALSE)

trainPredictors <- HometrainX[Rows,]
trainClasses <- HometrainY[Rows]

testPredictors <- HometrainX[-Rows,]
testClasses <- HometrainY[-Rows]

set.seed(123)

cl <- makeCluster(2)
registerDoSNOW(cl)

set.seed(123)
ridgeModel <- enet(x = as.matrix(trainPredictors), y = trainClasses,
                   lambda = 0.0001)

stopCluster(cl)

pred <- predict(ridgeModel, testPredictors)

lmValues1 <- data.frame(obs = testClasses, pred = Pred)

rmse(lmValues1$obs, lmValues1$pred)

nearfac <- nearZeroVar(trainPredictors)


