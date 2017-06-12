library(dplyr)
library(caret)
library(corrplot)

#set directory
setwd("~/data/housingprice")

#Read dataframe "trainhome.csv" and "testhome.csv"
trainhome <- read.csv("trainhome.csv")
testhome <- read.csv("testhome.csv")

#change some variable into factor
names <- c("MSSubClass","OverallQual", 
           "OverallCond",
           "YearBuilt",
           "YearRemodAdd",
           "GarageYrBlt",
           "MoSold",
           "YrSold")
trainhome[,names] <- lapply(trainhome[,names] , factor)

#add a new column call "SalePrice" in testhome
testhome$SalePrice <- NA

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

#select all numeric into a dataframe
train_int <- data_combined  %>%
  select_if(is.integer)

#select all factor into another table
train_fac <- data_combined %>%
  select_if(is.factor)

#select all Near Zero variance and remove
nearfac <- nearZeroVar(train_fac)
train_fac <- train_fac[,-nearfac]

#remove variable with missing value b/c variable doesn't provide better accuracy
drops <- c("BsmtFinType1", "BsmtExposure", "BsmtFinType2",
           "YearBuilt", "GarageType", "GarageYrBlt", 
           "GarageFinish", "FireplaceQu", "Fence", "Alley", 
           "MiscFeature", "PoolQC", "MasVnrType", "GarageQual",
           "GarageCond", "BsmtQual", "BsmtCond")
train_fac <- train_fac[,!(names(train_fac) %in% drops)]

DF <- c(1380, 1556, 1916, 2152, 2217, 2251, 2490, 2819, 2905)

#Remove the row for missing value and REMEMBER TO REMOVE IT ON train_int data.
DF <- c(1380, 1556, 1916, 2152, 2217, 2251, 2490, 2819, 2905)
train_fac <- train_fac[-DF,]
train_int <- train_int[-DF,]

#save it for any mistake later
save(data_combined, testhome, train_fac, trainhome, file = "draft_dat.Rdata")

#Remove BsmtFinSF1, BsmtFinSF2, BsmtUnfSF
drops <- c("BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "LowQualFinSF")
train_int <- train_int[,!(names(train_int) %in% drops)]

#sum "EnclosedPorch", "X3SsnPorch","ScreenPorch", "PoolArea"
train_int$PorchAndPool <- rowSums(train_int[,c("OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolArea")])

#sum "Fullbath" and "BsmtFullBath"
train_int$TotalFullBath <- rowSums(train_int[,c("FullBath", "BsmtFullBath")])

#sum "HalfBath" and "BsmtHalfBath"
train_int$TotalHalfBath <- rowSums(train_int[,c("HalfBath", "BsmtHalfBath")])

#remove "HalfBath", "BsmtHalfBath", "Fullbath" and "BsmtFullBath"
drops <- c("HalfBath", "BsmtHalfBath", "FullBath", "BsmtFullBath")
train_int <- train_int[,!(names(train_int) %in% drops)]

#remove "EnclosedPorch", "X3SsnPorch","ScreenPorch", "PoolArea"
drops <- c("OpenPorchSF", "EnclosedPorch", "X3SsnPorch","ScreenPorch", "PoolArea")
train_int <- train_int[,!(names(train_int) %in% drops)]

#Change all NA value into 0(Zero) in train.int
train_int <- data.frame(lapply(train_int, as.character), stringsAsFactors=FALSE)
train_int[is.na(train_int)]<-0
train_int <- data.frame(lapply(train_int, as.numeric))

#find correlation and remove correlation variables
correlations <- cor(train_int)

corrplot(correlations, order = "hclust")

highCorr <- findCorrelation(correlations, cutoff = .75)

Corrtrain <- train_int[,highCorr]

#remove high correlation variable
drops <- c("GarageCars", "X1stFlrSF", "Id")
train_int <- train_int[,!(names(train_int) %in% drops)]

#merge train_int and train_fac
train_int$Id <- rep(1:2910)
train_fac$Id <- rep(1:2910)

train_update <- merge(train_int, train_fac, by = "Id")
train_update <- train_update[, -1]

#Create a dummy variable for all factor variables in train.update dataframe
simpleMod <- dummyVars("~. ", data = train_update)
dummies_update <- predict(simpleMod, train_update)
train_update1 <- dummies_update

#Change dummies.update into a dataframe
train_update1 <- data.frame(dummies_update)

#Separate train set X and test set X
HometrainX <- train_update1[1:1459,]
HometestX <- train_update1[1460:2910,]

#Log transform train set Y
HometrainY <- HometrainX$SalePrice
HometrainY <- log1p(HometrainY)


drops <- c("SalePrice")
HometrainX <- HometrainX[,!(names(HometrainX) %in% drops)]
HometestX <- HometestX[,!(names(HometestX) %in%  drops)]


save(HometrainX, HometestX, HometrainY, file = "complete_data.Rdata")
