# DSEsalq
TCC code
########################
# Instalação de pacotes
pacotes <- c('tidyverse',  # Pacote básico de datawrangling
             'viridis',
             'lubridate',     
             'forecast', # Conjunto com Rpart, plota a parvore
             'plotly',     # funções auxiliares como quantcut,
             'tseries',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret',      # Funções úteis para machine learning
             'gamlss','GA',
             'gamlss.add',
             'TTR',       #financial package
             'e1071',
             'kernlab',
             'RColorBrewer',
             'quantmod',
             'caretEnsemble')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#to avoid scientific notation
options(scpen=999)

#""""""""""""""""""""""""""""""""""""BITCOIN""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
getSymbols("BTC-USD",from="2018-02-01", to="2023-04-21" ,src="yahoo",periodicity="daily")
write.csv(`BTC-USD`,'C:\\Users\\kasiv\\OneDrive - Vestas Wind Systems A S\\Documents\\personal\\TCC\\xx_TCC\\BTCdata2.csv', row.names = FALSE)

#


BTC_network<- read.csv('networkdata.csv', fileEncoding ='UTF-8-BOM')
knitr::kable(head(btc_network))
chartSeries(`BTC-USD`,
            theme = chartTheme("white"), # Theme
            bar.type = "hlc",  # High low close 
            up.col = "green",  # Up candle color
            dn.col = "red")   # Down candle color)

#calculate index
#Bitcoin
m<-nrow(`BTC-USD`)
n<-m-1
BTC_data <- `BTC-USD`[,6] #only adjusted closed prices
BTC_others <- `BTC-USD`[, 2:4]
BTC_data$SMA7 <- SMA(BTC_data, n=7)
BTC_data$SMA14 <- SMA(BTC_data[,1],n=14)
BTC_data$RSI <- RSI(BTC_data[,1],n=14)
BTC_data$MACD <- MACD(BTC_data[,1], nFast=12, nSlow=26, nSig =9)$macd
BTC_data$ROC7 <- ROC(BTC_data[,1], n = 7)
BTC_data$ROC14 <- ROC(BTC_data[,1], n = 14)
BTC_data$ROC21 <- ROC(BTC_data[,1], n = 21)
# Directional Movement Index
BTC_data$adx <- ADX(BTC_others[,c(1,2,3)])
# Moving Averages
BTC_data$EMA7 <- EMA(BTC_data[,1], n=7)
BTC_data$EMA14 <- EMA(BTC_data[,1], n=14)
BTC_data$WMA7 <- WMA(BTC_data[,1], n=7)
BTC_data$WMA14 <- WMA(BTC_data[,1], n=14)

# Stochastics
BTC_data$stoch <- stoch(BTC_others[,c(1,2,3)])
#SAR
BTC_data$sar <- SAR(BTC_data[,c(2,3)])
#deltas
BTC_data$SMA7d <- (BTC_data$BTC.USD.Adjusted-BTC_data$SMA7)
BTC_data$SMA14d <- (BTC_data$BTC.USD.Adjusted-BTC_data$SMA14)
BTC_data$WMA7d <- BTC_data$BTC.USD.Adjusted-BTC_data$WMA7
BTC_data$WMA14d <- (BTC_data$BTC.USD.Adjusted-BTC_data$WMA14)

#transactions
BTC_data$trans1<-(BTC_network$transantion)
BTC_data$trans<-SMA(BTC_network$transantion, n=7)
BTC_data$transd<-(BTC_data$trans-BTC_data$trans1)
BTC_data$hash1<-(BTC_network$hash_difficult)
BTC_data$hash<-SMA(BTC_network$hash_difficult, n=7)
BTC_data$hashd<-(BTC_data$hash-BTC_data$hash1)
BTC_data$nadd1<-(BTC_network$new_address)
BTC_data$nadd<-SMA(BTC_network$new_address, n=7)
BTC_data$naddd<-(BTC_data$nadd-BTC_data$nadd1)
BTC_data$fgd<-(BTC_network$fear_greed)
#LAG
BTC_data$lag1 <- lead(BTC_data$BTC.USD.Adjusted,7)

BTC <- as.data.frame(BTC_data)
colnames(BTC)[1] ="Price"
BTC$PriceChange <- BTC$lag1-BTC$Price 
BTC$Class <- ifelse(BTC$PriceChange   > 0,    'U',   'D')

BTC <- BTC[-(1:27),]
n<-nrow(BTC) - 7
m<-nrow(BTC)
BTC <- BTC[-(n:m),]
BTC$ROC7norm   = (2 * ((BTC$ROC7 - min(BTC$ROC7)) / (max(BTC$ROC7) - min(BTC$ROC7))) - 1)
BTC$ROC14norm  = (2 * ((BTC$ROC14 - min(BTC$ROC14)) / (max(BTC$ROC14) - min(BTC$ROC14))) - 1)
BTC$ROC21norm  = (2 * ((BTC$ROC21 - min(BTC$ROC21)) / (max(BTC$ROC21) - min(BTC$ROC21))) - 1)
BTC_set <- BTC [,c(38:40)]
#normalize all -1 1
BTC_set$RSInorm    = (2 * ((BTC$RSI - min(BTC$RSI)) / (max(BTC$RSI) - min(BTC$RSI))) - 1)
BTC_set$MACDnorm   = (2 * ((BTC$MACD - min(BTC$MACD)) / (max(BTC$MACD) - min(BTC$MACD))) - 1)
BTC_set$ADXnorm    = (2 * ((BTC$adx - min(BTC$adx)) / (max(BTC$adx) - min(BTC$adx))) - 1)
BTC_set$stochnorm  = (2 * ((BTC$stoch - min(BTC$stoch)) / (max(BTC$stoch) - min(BTC$stoch))) - 1)
BTC_set$sarnorm    = (2 * ((BTC$sar - min(BTC$sar)) / (max(BTC$sar) - min(BTC$sar))) - 1)
BTC_set$SMA7dnorm  = (2 * ((BTC$SMA7d - min(BTC$SMA7d)) / (max(BTC$SMA7d) - min(BTC$SMA7d))) - 1)
BTC_set$SMA14dnorm = (2 * ((BTC$SMA14d - min(BTC$SMA14d)) / (max(BTC$SMA14d) - min(BTC$SMA14d))) - 1)
BTC_set$WMA7dnorm  = (2 * ((BTC$WMA7d - min(BTC$WMA7d)) / (max(BTC$WMA7d) - min(BTC$WMA7d))) - 1)
BTC_set$WMA14dnorm = (2 * ((BTC$WMA14d - min(BTC$WMA14d)) / (max(BTC$WMA14d) - min(BTC$WMA14d))) - 1)
BTC_set$transnorm  = (2 * ((BTC$transd - min(BTC$transd)) / (max(BTC$transd) - min(BTC$transd))) - 1)
BTC_set$hashnorm   = (2 * ((BTC$hashd - min(BTC$hashd)) / (max(BTC$hashd) - min(BTC$hashd))) - 1)
BTC_set$naddnorm   = (2 * ((BTC$naddd - min(BTC$naddd)) / (max(BTC$naddd) - min(BTC$naddd))) - 1)
BTC_set$fgdorm     = (2 * ((BTC$fgd - min(BTC$fgd)) / (max(BTC$fgd) - min(BTC$fgd))) - 1)
BTC_set$Class=BTC$Class

trainPerc   <- 0.75

trainset_BTC <- BTC_set[1:floor(nrow(BTC_set)    * trainPerc),   ]
X_trainset<-trainset_BTC[,c(1:16)]
Y_trainset<-trainset_BTC[,17]

testset_BTC <-  BTC_set[(floor(nrow(BTC_set)    * trainPerc)    + 1):nrow(BTC_set), ]
X_testset <-testset_BTC[,c(1:16)]
Y_testset<-testset_BTC[,17]

#Choose C and Sigma

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
#Set1

svm_1 <- train(x= X_trainset[,c(4,5,12,13,14,15,16)] , y = Y_trainset, 
                    method = "svmRadial", trControl = train_control, tuneGrid = data.frame(C = c(10,25,50,75),sigma =  c(0.25,0.5,1.0,1.25)))

svm_1
predict_1 <- predict(svm_1,X_testset[,c(4,5,12,13,14,15,16)])

mean(predict_1 == Y_testset)

svm_1_accuracy <- confusionMatrix(predict_1,as.factor(Y_testset))


svm_inear <- train(x= X_trainset[,c(4,5,12,13,14,15,16)] , y = Y_trainset, 
                    method = "svmLinear", trControl = train_control)

svm_inear
predict_linear <- predict(svm_inear,X_testset[,c(4,5,12,13,14,15,16)])

mean(predict_linear == Y_testset)

svm_linear_accuracy <- confusionMatrix(predict_linear,as.factor(Y_testset))


#SVM GA Set1--------------------------------------------------------------------------------------------------------------------------
X_trainset_sel1<-X_trainset[,c(4,5,12,13,14,15,16)]
#GA
svm_fitness1 <- function(features1, X_trainset_sel1, Y_trainset) {
  # Select subset of X based on features
  col <- as.numeric(features1)
  
  X_subset <- as.data.frame(X_trainset_sel1[, col == 1 ])
  
  # Define cross-validation method
  contv <- trainControl(method="cv", number=10, classProbs=TRUE)
  
  # Train SVM using subset of X
  svm_model <- train(x = X_subset, y =  Y_trainset,  method = "svmRadial", trControl = contv, tuneGrid = data.frame(C = c(25),sigma = 0.5))
  # Return accuracy of SVM model
  return(max(svm_model$results$Accuracy))
}

# Define GA parameters
num_features1 <- ncol(X_trainset_sel1)
col_names1<-colnames(X_trainset_sel1)

fitness_function1 <- function(features1) svm_fitness1(features1,X_trainset_sel1, Y_trainset)

# Run GA
GA_output1 <- ga(type = "binary", fitness = fitness_function1, nBits = num_features1, names= col_names1,
                 popSize = 25, maxiter = 5, pcrossover = 0.8, pmutation = 0.1, elitism = 1)
GA_output1@solution

svm_radial1 <- train(x= X_trainset[,c(4,5,12,14,16)] , y = Y_trainset, 
                     method = "svmRadial", trControl = train_control, tuneGrid = data.frame(C = c(25),sigma =  c(0.5)))

svm_radial1
predict_radial1 <- predict(svm_radial1,X_testset[,c(4,5,12,14,16)])
mean(predict_radial1 == Y_testset)
svm_radial_accuracy1 <- confusionMatrix(predict_radial1,as.factor(Y_testset))
#--------------------------------------------------------------------------------------------------------------

#SVM GA Set2--------------------------------------------------------------------------------------------------------------------------
X_trainset_sel2<-X_trainset[,c(1,2,3,4,5,6,7)]
#GA
svm_fitness2 <- function(features2, X_trainset_sel2, Y_trainset) {
  # Select subset of X based on features
  col <- as.numeric(features2)
  
  X_subset <- as.data.frame(X_trainset_sel2[, col == 1 ])
  
  # Define cross-validation method
  contv <- trainControl(method="cv", number=10, classProbs=TRUE)
  
  # Train SVM using subset of X
  svm_model <- train(x = X_subset, y =  Y_trainset,  method = "svmRadial", trControl = contv, tuneGrid = data.frame(C = c(50),sigma = 1.0))
  # Return accuracy of SVM model
  return(max(svm_model$results$Accuracy))
}

# Define GA parameters
num_features2 <- ncol(X_trainset_sel2)
col_names2<-colnames(X_trainset_sel2)

fitness_function2 <- function(features2) svm_fitness2(features2,X_trainset_sel2, Y_trainset)

# Run GA
GA_output2 <- ga(type = "binary", fitness = fitness_function2, nBits = num_features2, names= col_names2,
                 popSize = 25, maxiter = 5, pcrossover = 0.8, pmutation = 0.1, elitism = 1)


GA_output2@solution
svm_radial2 <- train(x= X_trainset[,c(3,4,5,6,7)] , y = Y_trainset, 
                     method = "svmRadial", trControl = train_control, tuneGrid = data.frame(C = c(50),sigma =  c(1.0)))

svm_radial2
predict_radial2 <- predict(svm_radial2,X_testset[,c(3,4,5,6,7)])
mean(predict_radial2 == Y_testset)
svm_radial_accuracy2 <- confusionMatrix(predict_radial2,as.factor(Y_testset))

#SVM GA Set3--------------------------------------------------------------------------------------------------------------------------
X_trainset_sel3<-X_trainset[,c(4,5,6,7,8,11,12)]
#GA
svm_fitness3 <- function(features3, X_trainset_sel3, Y_trainset) {
  # Select subset of X based on features
  col <- as.numeric(features3)
  
  X_subset <- as.data.frame(X_trainset_sel3[, col == 1 ])
  
  # Define cross-validation method
  contv <- trainControl(method="cv", number=10, classProbs=TRUE)
  
  # Train SVM using subset of X
  svm_model <- train(x = X_subset, y =  Y_trainset,  method = "svmRadial", trControl = contv, tuneGrid = data.frame(C = c(75),sigma = 1.25))
  # Return accuracy of SVM model
  return(max(svm_model$results$Accuracy))
}

# Define GA parameters
num_features3 <- ncol(X_trainset_sel3)
col_names3<-colnames(X_trainset_sel3)

fitness_function3 <- function(features3) svm_fitness3(features3,X_trainset_sel3, Y_trainset)

# Run GA
GA_output3 <- ga(type = "binary", fitness = fitness_function3, nBits = num_features3, names= col_names3,
                 popSize = 25, maxiter = 5, pcrossover = 0.8, pmutation = 0.1, elitism = 1)

GA_output3@solution
svm_radial3 <- train(x= X_trainset[,c(4,5,6,7,8,12)] , y = Y_trainset, 
                     method = "svmRadial", trControl = train_control, tuneGrid = data.frame(C = c(75),sigma =  c(1.25)))

svm_radial3
predict_radial3 <- predict(svm_radial3,X_testset[,c(4,5,6,7,8,12)])
mean(predict_radial3 == Y_testset)
svm_radial_accuracy3 <- confusionMatrix(predict_radial3,as.factor(Y_testset))

#Majority vote

install.packages('mclust')
library(mclust)

set_1<- as.vector(predict_radial1)                                   
set_2<- as.vector(predict_radial2) 
set_3<- as.vector(predict_radial3) 
k<-nrow(X_testset)
combined_pred<-character(k)

for (i in 1:k) {
  com_aux<-c(set_1[i], set_2[i], set_3[i])
  
  A <- majorityVote(com_aux)
  combined_pred[i]<-A$majority}

mean(combined_pred == Y_testset)

#"""""""""""""""""""""""""""""""""""""""""""""""ETHEREUM""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#getSymbols("ETH-USD",from="2014-09-17", to="2023-04-21" ,src="yahoo",periodicity="daily")
getSymbols("ETH-USD",from="2017-11-09",to="2023-04-21",src="yahoo",periodicity="daily")
write.csv(`ETH-USD`,'C:\\Users\\kasiv\\OneDrive - Vestas Wind Systems A S\\Documents\\personal\\TCC\\xx_TCC\\ETHdata.csv', row.names = FALSE)
#C:\Users\kasiv\OneDrive - Vestas Wind Systems A S\Documents\personal\TCC\xx_TCC
eth_network<- read.csv('ether.csv', fileEncoding ='UTF-8-BOM')
knitr::kable(head(eth_network))
eth_network<-eth_network[-(1:312),]
chartSeries(`ETH-USD`,
            theme = chartTheme("white"), # Theme
            bar.type = "hlc",  # High low close 
            up.col = "green",  # Up candle color
            dn.col = "red")   # Down candle color)

#calculate index
#Ethereum
m<-nrow(`ETH-USD`)
n<-m-1
ETH_data <- `ETH-USD`[,6] #only adjusted closed prices
ETH_others <- `ETH-USD`[, 2:4]
ETH_data$SMA7 <- SMA(ETH_data, n=7)
ETH_data$SMA14 <- SMA(ETH_data[,1],n=14)
ETH_data$RSI <- RSI(ETH_data[,1],n=14)
ETH_data$MACD <- MACD(ETH_data[,1], nFast=12, nSlow=26, nSig =9)$macd
ETH_data$ROC7 <- ROC(ETH_data[,1], n = 7)
ETH_data$ROC14 <- ROC(ETH_data[,1], n = 14)
ETH_data$ROC21 <- ROC(ETH_data[,1], n = 21)
# Directional Movement Index
ETH_data$adx <- ADX(ETH_others[,c(1,2,3)])
# Moving Averages
ETH_data$EMA7 <- EMA(ETH_data[,1], n=7)
ETH_data$EMA14 <- EMA(ETH_data[,1], n=14)
ETH_data$WMA7 <- WMA(ETH_data[,1], n=7)
ETH_data$WMA14 <- WMA(ETH_data[,1], n=14)

# Stochastics
ETH_data$stoch <- stoch(ETH_others[,c(1,2,3)])
#SAR
ETH_data$sar <- SAR(ETH_data[,c(2,3)])
#deltas
ETH_data$SMA7d <- (ETH_data$ETH.USD.Adjusted-ETH_data$SMA7)
ETH_data$SMA14d <- (ETH_data$ETH.USD.Adjusted-ETH_data$SMA14)
ETH_data$WMA7d <- ETH_data$ETH.USD.Adjusted-ETH_data$WMA7
ETH_data$WMA14d <- (ETH_data$ETH.USD.Adjusted-ETH_data$WMA14)

#transactions
ETH_data$trans1<-(eth_network$trans)
ETH_data$trans<-SMA(eth_network$trans, n=7)
ETH_data$transd<-(ETH_data$trans-ETH_data$trans1)
ETH_data$fees1<-(eth_network$fees)
ETH_data$fees<-SMA(eth_network$fees, n=7)
ETH_data$feesd<-(ETH_data$fees-ETH_data$fees1)
#LAG
ETH_data$lag1 <- lead(ETH_data$ETH.USD.Adjusted,7)

ETH <- as.data.frame(ETH_data)
colnames(ETH)[1] ="Price"
ETH$PriceChange <- ETH$lag1-ETH$Price 
ETH$Class <- ifelse(ETH$PriceChange   > 0,    'U',   'D')

ETH <- ETH[-(1:27),]
n<-nrow(ETH) - 7
m<-nrow(ETH)
ETH <- ETH[-(n:m),]
ETH$ROC7norm   = (2 * ((ETH$ROC7 - min(ETH$ROC7)) / (max(ETH$ROC7) - min(ETH$ROC7))) - 1)
ETH$ROC14norm  = (2 * ((ETH$ROC14 - min(ETH$ROC14)) / (max(ETH$ROC14) - min(ETH$ROC14))) - 1)
ETH$ROC21norm  = (2 * ((ETH$ROC21 - min(ETH$ROC21)) / (max(ETH$ROC21) - min(ETH$ROC21))) - 1)
eth_set <- ETH [,c(34:36)]
#normalize all -1 1
eth_set$RSInorm    = (2 * ((ETH$RSI - min(ETH$RSI)) / (max(ETH$RSI) - min(ETH$RSI))) - 1)
eth_set$MACDnorm   = (2 * ((ETH$MACD - min(ETH$MACD)) / (max(ETH$MACD) - min(ETH$MACD))) - 1)
eth_set$ADXnorm    = (2 * ((ETH$adx - min(ETH$adx)) / (max(ETH$adx) - min(ETH$adx))) - 1)
eth_set$stochnorm  = (2 * ((ETH$stoch - min(ETH$stoch)) / (max(ETH$stoch) - min(ETH$stoch))) - 1)
eth_set$sarnorm    = (2 * ((ETH$sar - min(ETH$sar)) / (max(ETH$sar) - min(ETH$sar))) - 1)
eth_set$SMA7dnorm  = (2 * ((ETH$SMA7d - min(ETH$SMA7d)) / (max(ETH$SMA7d) - min(ETH$SMA7d))) - 1)
eth_set$SMA14dnorm = (2 * ((ETH$SMA14d - min(ETH$SMA14d)) / (max(ETH$SMA14d) - min(ETH$SMA14d))) - 1)
eth_set$WMA7dnorm  = (2 * ((ETH$WMA7d - min(ETH$WMA7d)) / (max(ETH$WMA7d) - min(ETH$WMA7d))) - 1)
eth_set$WMA14dnorm = (2 * ((ETH$WMA14d - min(ETH$WMA14d)) / (max(ETH$WMA14d) - min(ETH$WMA14d))) - 1)
eth_set$transnorm  = (2 * ((ETH$transd - min(ETH$transd)) / (max(ETH$transd) - min(ETH$transd))) - 1)
eth_set$feesnorm   = (2 * ((ETH$feesd - min(ETH$feesd)) / (max(ETH$feesd) - min(ETH$feesd))) - 1)

eth_set$Class=ETH$Class

trainPerc   <- 0.75

trainset_eth <- eth_set[1:floor(nrow(eth_set)    * trainPerc),   ]
X_trainset<-trainset_eth[,c(1:14)]
Y_trainset<-trainset_eth[,15]

testset_eth <-  eth_set[(floor(nrow(eth_set)    * trainPerc)    + 1):nrow(eth_set), ]
X_testset <-testset_eth[,c(1:14)]
Y_testset<-testset_eth[,15]
#Choose C and Sigma

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)


svm_radial <- train(x= X_trainset[,c(4,5,6,7,8,13,14)] , y = Y_trainset, 
                    method = "svmRadial", trControl = train_control, tuneGrid = data.frame(C = c(10,25,50,75),sigma =  c(0.25,0.5,1.0,1.25)))

svm_radial
predict_radial <- predict(svm_radial,X_testset[,c(4,5,6,7,8,13,14)])


mean(predict_radial == Y_testset)

svm_radial_accuracy <- confusionMatrix(predict_radial,as.factor(Y_testset))

#SVM GA Set1--------------------------------------------------------------------------------------------------------------------------
X_trainset_sel1<-X_trainset[,c(4,5,6,7,8,13,14)]
#GA
svm_fitness1 <- function(features1, X_trainset_sel1, Y_trainset) {
  # Select subset of X based on features
  col <- as.numeric(features1)
  
  X_subset <- as.data.frame(X_trainset_sel1[, col == 1 ])
  
  # Define cross-validation method
  contv <- trainControl(method="cv", number=10, classProbs=TRUE)
  
  # Train SVM using subset of X
  svm_model <- train(x = X_subset, y =  Y_trainset,  method = "svmRadial", trControl = contv, tuneGrid = data.frame(C = c(50),sigma = 1.0))
  # Return accuracy of SVM model
  return(max(svm_model$results$Accuracy))
}

# Define GA parameters
num_features1 <- ncol(X_trainset_sel1)
col_names1<-colnames(X_trainset_sel1)

fitness_function1 <- function(features1) svm_fitness1(features1,X_trainset_sel1, Y_trainset)

# Run GA
GA_output1 <- ga(type = "binary", fitness = fitness_function1, nBits = num_features1, names= col_names1,
                popSize = 25, maxiter = 5, pcrossover = 0.8, pmutation = 0.1, elitism = 1)
GA_output1@solution

svm_radial1 <- train(x= X_trainset[,c(4,5,6,7,8,13)] , y = Y_trainset, 
                    method = "svmRadial", trControl = train_control, tuneGrid = data.frame(C = c(50),sigma =  c(1.0)))

svm_radial1
predict_radial1 <- predict(svm_radial1,X_testset[,c(4,5,6,7,8,13)])
mean(predict_radial1 == Y_testset)
svm_radial_accuracy1 <- confusionMatrix(predict_radial1,as.factor(Y_testset))
#--------------------------------------------------------------------------------------------------------------

#SVM GA Set2--------------------------------------------------------------------------------------------------------------------------
X_trainset_sel2<-X_trainset[,c(1,2,3,4,5,6,7)]
#GA
svm_fitness2 <- function(features2, X_trainset_sel2, Y_trainset) {
  # Select subset of X based on features
  col <- as.numeric(features2)
  
  X_subset <- as.data.frame(X_trainset_sel2[, col == 1 ])
  
  # Define cross-validation method
  contv <- trainControl(method="cv", number=10, classProbs=TRUE)
  
  # Train SVM using subset of X
  svm_model <- train(x = X_subset, y =  Y_trainset,  method = "svmRadial", trControl = contv, tuneGrid = data.frame(C = c(50),sigma = 1.0))
  # Return accuracy of SVM model
  return(max(svm_model$results$Accuracy))
}

# Define GA parameters
num_features2 <- ncol(X_trainset_sel2)
col_names2<-colnames(X_trainset_sel2)

fitness_function2 <- function(features2) svm_fitness2(features2,X_trainset_sel2, Y_trainset)

# Run GA
GA_output2 <- ga(type = "binary", fitness = fitness_function2, nBits = num_features2, names= col_names2,
                 popSize = 25, maxiter = 25, pcrossover = 0.8, pmutation = 0.1, elitism = 1)


GA_output2@solution
svm_radial2 <- train(x= X_trainset[,c(2,3,4,5,6,7)] , y = Y_trainset, 
                     method = "svmRadial", trControl = train_control, tuneGrid = data.frame(C = c(50),sigma =  c(1.0)))

svm_radial2
predict_radial2 <- predict(svm_radial2,X_testset[,c(2,3,4,5,6,7)])
mean(predict_radial2 == Y_testset)
svm_radial_accuracy2 <- confusionMatrix(predict_radial2,as.factor(Y_testset))

#SVM GA Set3--------------------------------------------------------------------------------------------------------------------------
X_trainset_sel3<-X_trainset[,c(4,5,6,7,8,11,12)]
#GA
svm_fitness3 <- function(features3, X_trainset_sel3, Y_trainset) {
  # Select subset of X based on features
  col <- as.numeric(features3)
  
  X_subset <- as.data.frame(X_trainset_sel3[, col == 1 ])
  
  # Define cross-validation method
  contv <- trainControl(method="cv", number=10, classProbs=TRUE)
  
  # Train SVM using subset of X
  svm_model <- train(x = X_subset, y =  Y_trainset,  method = "svmRadial", trControl = contv, tuneGrid = data.frame(C = c(75),sigma = 1.25))
  # Return accuracy of SVM model
  return(max(svm_model$results$Accuracy))
}

# Define GA parameters
num_features3 <- ncol(X_trainset_sel3)
col_names3<-colnames(X_trainset_sel3)

fitness_function3 <- function(features3) svm_fitness3(features3,X_trainset_sel3, Y_trainset)

# Run GA
GA_output3 <- ga(type = "binary", fitness = fitness_function3, nBits = num_features3, names= col_names3,
                 popSize = 25, maxiter = 5, pcrossover = 0.8, pmutation = 0.1, elitism = 1)

GA_output3@solution
svm_radial3 <- train(x= X_trainset[,c(4,5,6,7,8)] , y = Y_trainset, 
                     method = "svmRadial", trControl = train_control, tuneGrid = data.frame(C = c(75),sigma =  c(1.25)))

svm_radial3
predict_radial3 <- predict(svm_radial3,X_testset[,c(4,5,6,7,8)])
mean(predict_radial3 == Y_testset)
svm_radial_accuracy3 <- confusionMatrix(predict_radial3,as.factor(Y_testset))

#Majority vote

install.packages('mclust')
library(mclust)

set_1<- as.vector(predict_radial1)                                   
set_2<- as.vector(predict_radial2) 
set_3<- as.vector(predict_radial3) 
k<-nrow(X_testset)
combined_pred<-character(k)

for (i in 1:k) {
  com_aux<-c(set_1[i], set_2[i], set_3[i])
  
  A <- majorityVote(com_aux)
  combined_pred[i]<-A$majority}

mean(combined_pred == Y_testset)
