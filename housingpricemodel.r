run a Linear R on below and send me results. Use 60 Training and 40 Test. Do Random Sampling. 


1. With base attributes.

2. With added attributes.

library(caret)




Rows <- createDataPartition(classes,
+ p = (100/1459.0),
+ list= FALSE)

index <- c(1:60)
trainset <- Rows[index,]

trainPredictors <- hometrainx[trainset,]
trainClasses <- hometrainy[trainset]


testPredictors <- hometestx[-trainset,]
testClasses <- hometesty[-trainset]

train_data <- trainPredictors
train_data$SalePrice <- trainClasses

linear_model <- lm(SalePrice ~ GarageArea + GrLivArea + TotalBsmtSF, data = train_data)


summary(linear_model)

lmPred1 <- predict(linear_model, testPredictors)

lmValues1 <- data.frame(obs = testClasses, pred = lmPred1)


linear_model2 <- lm(SalePrice ~ GarageArea + GrLivArea + TotalBsmtSF + unemplyRate + spOpen + spHigh + spLow + spClose + spAdjClose +spVolume +CCIValue + fedfunds_rate + prime_rate, data = train_data)


summary(linear_model2)

lmPred1 <- predict(linear_model2, testPredictors)

lmValues1 <- data.frame(obs = testClasses, pred = lmPred2)