
load("/Users/ken/Documents/HousingPrice/prepredata_version2.Rdata")

keeps <- c("YearBuilt","YearRemodAdd", "TotalBsmtSF",
           "GrLivArea", "GarageCars", "TotalFullBath",
           "GarageFinish", "SalePrice","ExterQual", 
           "GarageArea", "Fireplaces", "BsmtFinSF1",
           "X1stFlrSF", "TotRmsAbvGrd", "OpenPorchSF",
           "BsmtUnfSF", "LotFrontage", "LotArea", 
           "OverallQual", "MasVnrType", "BsmtQual",
           "CentralAir", "Electrical", "PavedDrive",
           "X2ndFlrSF", "BedroomAbvGr", "EnclosedPorch",
            "WoodDeckSF", "PoolArea", "Neighborhood")
train_update <- train_update[,(names(train_update) %in% keeps)]


#Create a dummy variable for all factor variables in train.update dataframe
simpleMod <- dummyVars("~. ", data = train_update)
dummies_update <- predict(simpleMod, train_update)
train_update1 <- dummies_update

#Change dummies.update into a dataframe
train_update1 <- data.frame(dummies_update)

#Separate train set X and test set X
HometrainX <- train_update1[1:1459,]
HometestX <- train_update1[1460:2918,]

#Log transform train set Y
HometrainY <- HometrainX$SalePrice
HometrainY <- log1p(HometrainY)


drops <- c("SalePrice")
HometrainX <- HometrainX[,!(names(HometrainX) %in% drops)]
HometestX <- HometestX[,!(names(HometestX) %in%  drops)]

