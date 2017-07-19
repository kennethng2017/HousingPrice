library(dplyr)
library(mice)
library(data.table)
library(xgboost)
library(Metrics)
library(doParallel)
library(caret)



#set directory

setwd("~/data/housingprice")


#Read dataframe "trainhome.csv" and "testhome.csv"

trainhome <- read.csv("trainhome.csv")

testhome <- read.csv("testhome.csv")


#add a new column call "SalePrice" in testhome
y_train <- trainhome$SalePrice

trainhome$SalePrice <- NULL


#combine both data

data_combined <- rbind(trainhome, testhome)




#Find missing value and percantage by each column

propmiss <- function(dataframe) {
  
  m <- sapply(dataframe, function(x) {
    
    data.frame(
      
      nmiss=sum(is.na(x)), 
      
      n=length(x), 
      
      propmiss=sum(is.na(x))/length(x)
      
    )
    
  })
  
  d <- data.frame(t(m))
  
  d <- sapply(d, unlist)
  
  d <- as.data.frame(d)
  
  d$variable <- row.names(d)
  
  row.names(d) <- NULL
  
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  
  return(d[order(d$propmiss), ])
  
}


propmiss(data_combined)



drops <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu")


data_combined <- data_combined[,!(names(data_combined) %in% drops)]


#sum "Fullbath" and "BsmtFullBath"

data_combined$TotalFullBath <- rowSums(data_combined[,c("FullBath", "BsmtFullBath")])


#sum "HalfBath" and "BsmtHalfBath"

data_combined$TotalHalfBath <- rowSums(data_combined[,c("HalfBath", "BsmtHalfBath")])


drops <- c("HalfBath", "BsmtHalfBath", "FullBath", "BsmtFullBath")

data_combined <- data_combined[,!(names(data_combined) %in% drops)]


#select all Near Zero variance and remove

nearfac <- nearZeroVar(data_combined)


nearfac


data_combined <- data_combined[,-nearfac]


#split x variable and y variable

x_data <- data_combined



#remove id

x_data$Id <- NULL


#use mice package on x_data

x_implute <- complete(mice(x_data, m=1, method = 'cart'))


propmiss(x_data)


x_data$Exterior1st=x_implute$Exterior1st

x_data$Exterior2nd=x_implute$Exterior2nd

x_data$BsmtFinSF1=x_implute$BsmtFinSF1

x_data$BsmtUnfSF=x_implute$BsmtUnfSF

x_data$TotalBsmtSF=x_implute$TotalBsmtSF

x_data$Electrical = x_implute$Electrical

x_data$BsmtFinSF2=x_implute$BsmtFinSF2

x_data$KitchenQual=x_implute$KitchenQual

x_data$GarageCars=x_implute$GarageCars

x_data$GarageArea=x_implute$GarageArea

x_data$SaleType=x_implute$SaleType

x_data$TotalHalfBath=x_implute$TotalHalfBath

x_data$TotalFullBath=x_implute$TotalFullBath

x_data$MSZoning=x_implute$MSZoning

x_data$MasVnrArea=x_implute$MasVnrArea

x_data$MasVnrType=x_implute$MasVnrType

x_data$MSSubClass=x_implute$MSSubClass

x_data$BsmtFinType1=x_implute$BsmtFinType1

x_data$BsmtQual=x_implute$BsmtQual

x_data$BsmtExposure=x_implute$BsmtExposure

x_data$GarageFinish=x_implute$GarageFinish

x_data$GarageYrBlt=x_implute$GarageYrBlt

x_data$LotFrontage=x_implute$LotFrontage

x_data$GarageType=x_implute$GarageType


propmiss(x_data)


str(x_data)


train_update = as.data.table(x_data)


str(train_update)

write.csv(train_update, file = "train_update.csv", row.names = FALSE)

train_update <- read.csv("train_update.csv", sep = ",")

str(train_update)

#Create a dummy variable for all factor variables in train.update dataframe
simpleMod <- dummyVars("~. ", data = train_update)
dummies_update <- predict(simpleMod, train_update)

train_update1 <- data.frame(dummies_update)


#Separate train set X and test set X

HometrainX <- train_update1[1:1460,]

HometestX <- train_update1[1461:2919,]


#train set Y

HometrainY <- y_train


#start model
set.seed(134)

Rows <- createDataPartition(HometrainY,
                            p = .80,
                            list= FALSE)

trainPredictors <- HometrainX[Rows,]
trainClasses <- HometrainY[Rows]

testPredictors <- HometrainX[-Rows,]
testClasses <- HometrainY[-Rows]


set.seed(123)

cl <- makeCluster(3, type = "SOCK")
registerDoParallel(cl)

ctrl <- trainControl(method = "cv", number = 10)

enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),
                        .fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(trainPredictors, trainClasses,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"))

#Error in train.default(trainPredictors, trainClasses, method = "enet",  : 
#                         Stopping
 #                      In addition: Warning message:
 #                        In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
 #                                                  There were missing values in resampled performance measures.

