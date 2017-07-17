library(dplyr)
library(caret)
library(corrplot)
library(ggplot2)
library(reshape2)

#set directory
setwd("~/documents/housingprice")

#Read dataframe "trainhome.csv" and "testhome.csv"
trainhome <- read.csv("trainhome.csv")
testhome <- read.csv("testhome.csv")

#change some variable into factor
name <- c("MSSubClass","OverallQual", 
          "OverallCond",
          "GarageYrBlt")
trainhome[,name] <- lapply(trainhome[,name] , factor)

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

propmiss(data_combined)

#look into every missing value
melt_df <- c("BsmtFinType1", "BsmtExposure", "BsmtFinType2",
             "YearBuilt", "GarageType", "GarageYrBlt", 
             "GarageFinish", "FireplaceQu", "Fence", "Alley", 
             "MiscFeature", "PoolQC", "MasVnrType", "GarageQual",
             "GarageCond", "BsmtQual", "BsmtCond", "GarageType")

df <- data_combined[,melt_df]

ggplot(df, aes(x= GarageCond)) +
  geom_bar()

ggplot(df, aes(x= GarageQual)) +
  geom_bar()

ggplot(df, aes(x= BsmtQual)) +
  geom_bar()

data_combined$BsmtQual[is.na(data_combined$BsmtQual)] <- "TA"

ggplot(df, aes(x= BsmtCond)) +
  geom_bar()

#plot all variable with missing data and see which ones are more important
ggplot(df, aes(x= BsmtFinType1)) +
  geom_bar()

#Replace NA value
data_combined$BsmtFinType1[is.na(data_combined$BsmtFinType1)] <- "GLQ"

ggplot(df, aes(x= BsmtExposure)) +
  geom_bar()
#Replace NA value
data_combined$BsmtExposure[is.na(data_combined$BsmtExposure)]<- "No"

ggplot(df, aes(x= BsmtFinType2)) +
  geom_bar()

ggplot(df, aes(x= YearBuilt)) +
  geom_bar()

ggplot(df, aes(x= GarageType)) +
  geom_bar()
#Replace NA value
data_combined$GarageType[is.na(data_combined$GarageType)]<- "Attchd"

ggplot(df, aes(x= GarageFinish)) +
  geom_bar()
#Replace NA value
data_combined$GarageFinish[is.na(data_combined$GarageFinish)]<- "Unf"

ggplot(data_combined, aes(x= LotFrontage)) +
  geom_histogram()

data_combined$LotFrontage[is.na(data_combined$LotFrontage)]<- mean(data_combined$LotFrontage, na.rm = TRUE)

ggplot(data_combined, aes(x= MasVnrArea)) +
  geom_histogram()

data_combined$MasVnrArea[is.na(data_combined$MasVnrArea)]<- mean(data_combined$MasVnrArea, na.rm = TRUE)


ggplot(data_combined, aes(x= MasVnrType)) +
  geom_bar()

data_combined$MasVnrType[is.na(data_combined$MasVnrType)]<- "None"

ggplot(data_combined, aes(x= MSZoning)) +
  geom_bar()

data_combined$MSZoning[is.na(data_combined$MSZoning)]<- "RL"

ggplot(data_combined, aes(x= Functional)) +
  geom_bar()

data_combined$Functional[is.na(data_combined$Functional)]<- "Typ"

data_combined$BsmtFullBath[is.na(data_combined$BsmtFullBath)]<- round(mean(data_combined$BsmtFullBath, na.rm = TRUE))

data_combined$BsmtHalfBath[is.na(data_combined$BsmtHalfBath)] <- round(mean(data_combined$BsmtHalfBath, na.rm = TRUE))

ggplot(data_combined, aes(x= Utilities)) +
  geom_bar()

data_combined$Utilities[is.na(data_combined$Utilities)]<- "AllPub"

ggplot(data_combined, aes(x= SaleType)) +
  geom_bar()

data_combined$SaleType[is.na(data_combined$SaleType)]<- "WD"

ggplot(data_combined, aes(x= KitchenQual)) +
  geom_bar()

data_combined$KitchenQual[is.na(data_combined$KitchenQual)]<- "TA"

data_combined$GarageArea[is.na(data_combined$GarageArea)] <- round(mean(data_combined$GarageArea, na.rm = TRUE))

data_combined$GarageCars[is.na(data_combined$GarageCars)] <- round(mean(data_combined$GarageCars, na.rm = TRUE))

data_combined$TotalBsmtSF[is.na(data_combined$TotalBsmtSF)] <- round(mean(data_combined$TotalBsmtSF, na.rm = TRUE))

data_combined$BsmtUnfSF[is.na(data_combined$BsmtUnfSF)] <- round(mean(data_combined$BsmtUnfSF, na.rm = TRUE))

data_combined$BsmtFinSF1[is.na(data_combined$BsmtFinSF1)] <- round(mean(data_combined$BsmtFinSF1, na.rm = TRUE))

data_combined$BsmtFinSF2[is.na(data_combined$BsmtFinSF2)] <- round(mean(data_combined$BsmtFinSF2, na.rm = TRUE))

ggplot(data_combined, aes(x= Exterior2nd)) +
  geom_bar()

data_combined$Exterior2nd[is.na(data_combined$Exterior2nd)]<- "VinylSd"

ggplot(data_combined, aes(x= Exterior1st)) +
  geom_bar()

data_combined$Exterior1st[is.na(data_combined$Exterior1st)]<- "VinylSd"

ggplot(data_combined, aes(x= MSSubClass)) +
  geom_bar()

ggplot(data_combined, aes(x= GarageYrBlt)) +
  geom_bar()

data_combined$MSSubClass[is.na(data_combined$MSSubClass)]<- "20"

DF <- c(935, 1299, 1380)

data_combined <- data_combined[-DF,]

propmiss(data_combined)

drops <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu",
           "GarageQual", "GarageCond", "BsmtCond", "MasVnrArea", "GarageYrBlt",
           "BsmtFinType2")
data_combined <- data_combined[,!(names(data_combined) %in% drops)]

#sum "Fullbath" and "BsmtFullBath"
data_combined$TotalFullBath <- rowSums(data_combined[,c("FullBath", "BsmtFullBath")])

#sum "HalfBath" and "BsmtHalfBath"
data_combined$TotalHalfBath <- rowSums(data_combined[,c("HalfBath", "BsmtHalfBath")])

drops <- c("HalfBath", "BsmtHalfBath", "FullBath", "BsmtFullBath")
data_combined <- data_combined[,!(names(data_combined) %in% drops)]

#select all numeric into a dataframe
nums <- !sapply(data_combined, is.factor)
trainnumber <- data_combined[ , nums]

#select all factor into another table
train_fac <- data_combined %>%
  select_if(is.factor)

#select all Near Zero variance and remove
nearfac <- nearZeroVar(train_fac)

nearfac

train_fac <- train_fac[,-nearfac]

#check for missing variable again
propmiss(train_fac)

#look into interger variable
propmiss(trainnumber)

#Change all NA value into 0(Zero) in train.int
trainnumber <- data.frame(lapply(trainnumber, as.character), stringsAsFactors=FALSE)
trainnumber[is.na(trainnumber)]<-0
trainnumber <- data.frame(lapply(trainnumber, as.numeric))


#find correlation and remove correlation variables
train_train <- trainnumber[1:1459,]

correlations <- cor(train_train)

corrplot(correlations, order = "hclust")

highCorr <- findCorrelation(correlations, cutoff = .65)

trainnumber$Id <- NULL

#merge trainnumber and train_fac
trainnumber$Id <- rep(1:2916)
train_fac$Id <- rep(1:2916)

train_update <- merge(trainnumber, train_fac, by = "Id")
train_update <- train_update[, -1]

save(data_combined, train_update, file = "prepredata_version2.Rdata" )
