library(glmnet)
library(caret)
library(doSNOW)
library(Metrics)

load("/Users/ken/Documents/HousingPrice/complete_data.Rdata")

set.seed(134)

Rows <- createDataPartition(HometrainY,
                            p = .80,
                            list= FALSE)

trainPredictors <- HometrainX[Rows,]
trainClasses <- HometrainY[Rows]

testPredictors <- HometrainX[-Rows,]
testClasses <- HometrainY[-Rows]

trainPredictors <- as.matrix(trainPredictors)
testPredictors <- as.matrix(testPredictors)

set.seed(123)

cl <- makeCluster(2)
registerDoSNOW(cl)

set.seed(123)

lasso = glmnet(as.matrix(trainPredictors), trainClasses, family="gaussian", alpha=1)

stopCluster(cl)

#the ones without zero have been factored selected.
coef(lasso)[, 10]

lasso.coef = coef(lasso, s=0.045, exact=TRUE)

lasso.coef

lasso.y = predict(lasso, testPredictors, type="response", s=0.7)

mean((testClasses - lasso.y)^2)

