library(gbm)
library(caret)
library(doSNOW)
library(Metrics)

set.seed(56)

Rows <- createDataPartition(HometrainY,
                            p = .80,
                            list= FALSE)

trainPredictors <- HometrainX[Rows,]
trainClasses <- HometrainY[Rows]

testPredictors <- HometrainX[-Rows,]
testClasses <- HometrainY[-Rows]

set.seed(46)

control = trainControl(method = "cv", number = 10)

cl <- makeCluster(3)
registerDoSNOW(cl)

grid = expand.grid(.n.trees=seq(2000, 3000, by= 100),
                   .interaction.depth=seq(1,7, by=1), .shrinkage=c( .01),
                   .n.minobsinnode=10)

gbm_train = train(x = trainPredictors, y = trainClasses, method ="gbm", 
                       trControl= control, 
                       preProc = c("BoxCox", "center", "scale", "spatialSign","pca"), 
                       tuneGrid = grid)


stopCluster(cl)

gbm_train

summary(gbm_train)

Pred <-predict(gbm_train, testPredictors)

lmValues1 <- data.frame(obs = testClasses, pred = Pred)

rmse(lmValues1$obs, lmValues1$pred)

trainPredictors$SalePrice <- trainClasses

gbm_pros = gbm(SalePrice~., data=trainPredictors, n.trees=2100, interaction.depth=5,
               shrinkage=0.01, distribution="gaussian")

gbm_pros_test = predict(gbm_pros, newdata=testPredictors, n.trees=2100)

lmValues1 <- data.frame(obs = testClasses, pred = gbm_pros_test)

rmse(lmValues1$obs, lmValues1$pred)


#submission

lmPred3 <- predict(gbm_pros, newdata = HometestX, n.trees = 2100)

testing <- expm1(lmPred3)

submission <- c(1461:2919)

submission <- as.data.frame(submission)

submission$SalePrice <- testing

names(submission)[1] <- c("Id")

write.table(submission, file = "submission1.csv", sep = ",", row.names = FALSE)

