#Part 1
#Load the rating data (Homework 2-MKT436R.Data.csv) into R and save as a data frame
library(reshape2)
ratingData=read.csv(file.choose(),header = T, sep = ",")
#Re-name the columns of the dataframe and fill the dataframe
df =dcast(ratingData, formula = consumerID ~ rockyID, value.var = "rating")
names(df) <- c("consumerID","rocky1","rocky2","rocky3","rocky4","rocky5" )
df[df$consumerID==490432,]

#Part 2
#a) Compute correlations between the ratings of each of the five movies
cor(x=df[,-1],y=df[,-1], use = "na.or.complete")
#b) Find the mean rating of each movie
colMeans(df[,-1],na.rm = T)
#c) subset rows containing Rocky 4 and compute the mean rating of each movie
ratingRocky4 <- df[!(is.na(df$rocky4)),]
colMeans(ratingRocky4[,-1],na.rm = T)
#d) Load new dataset called "Homework 2 - completeDB.csv"
completeDB <- read.csv(file.choose(),header = T, sep = ",") 

# Part 3
#a) generate different interactions among the predictor variables:
firstInteractions = model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4),completeDB)
secondInteractions = model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4)^2,completeDB)
thirdInteractions = model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4)^3,completeDB)
fourthInteractions = model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4)^4,completeDB)
#b) Run linear regression:
firstLM=lm(rocky5~firstInteractions,completeDB)
secondLM=lm(rocky5~secondInteractions,completeDB)
thirdLM=lm(rocky5~thirdInteractions,completeDB)
fourthLM=lm(rocky5~fourthInteractions,completeDB)
#c) Calculate AIC and BIC of each interactions:
aic= c(AIC(firstLM),AIC(secondLM),AIC(thirdLM),AIC(fourthLM))
bic= c(BIC(firstLM),BIC(secondLM),BIC(thirdLM),BIC(fourthLM))
part3c=data.frame(aic,bic,row.names = c('first','second','third','fourth'))
# When comparing a set of models for the data, the preferred model is the one
# with the lowest value of AIC or BIC, so the fourth model is preferred by the AIC
# and the second model is preferred by the BIC
#d) Lasso model
install.packages('glmnet')
library('glmnet')
lassoFit = glmnet(fourthInteractions,completeDB$rocky5,alpha=1)
predict(lassoFit,s = 0.05, type = 'coefficients')
predict(lassoFit,s = 0.5, type = 'coefficients')
#e) Calculate optimal penalty parameter
lassoFitCV = cv.glmnet(fourthInteractions,completeDB$rocky5,alpha=1)
# f) Ridge estimator
ridgeFit = cv.glmnet(fourthInteractions,completeDB$rocky5,alpha=0)
# g) Extract the coefficients from the lasso and the ridge regression
predict(lassoFitCV,s = lassoFitCV$lambda.min, type = 'coefficients')
predict(ridgeFit,s = ridgeFit$lambda.min, type = 'coefficients')


# Part 4
# a)
### This part is included in part b) as we created training and validation datasets when doing
### cross validation for each type of model
# b)

###In this part, when doing cross validation to find the best model, we set nFold as 10,
###which means the validation set is 90% of completeDB dataset.
###As for the criterion of selecting the best model, we choose the one which has the 
### average min out of sample MSE.

#c) Find the model
###  Linear Regression Model
### First, we got the combination of all independent variables in fourth interactions
### and name it as linearReg.
### According to the results calculated by lassoFit and ridgeFit in Part3, we found the
### ratings for rocky1 has very little impact on the ratings for rocky5, so we drop it in the Linear
### regression predictive analysis. Considering all the interactions in fourthInteractions and
### log transformations, we estimate 385 models for this type.

vector <- c('rocky2','rocky3','rocky4')
str_comb <- function(vector){
  n <- length(vector)
  num=0
  col=1
  for (i in 1:n){
    num=num+choose(n,i)
  }
  comb_matrix <- matrix(,nrow=num,ncol=1)
    for (j in 1:n){
    comb_res <- combn(vector,j)
    m <- ncol(comb_res)
    for (l in 1:m){
      comb_matrix[col,1] <- paste(comb_res[,l],collapse = "*")
      col=col+1
      if(col==num)break
    }
  }
  return(comb_matrix)
}

vector2=str_comb(vector)

str_comb2 <- function(vector){
  n <- length(vector)
  num=0
  col=1
  for (i in 1:n){
    num=num+choose(n,i)
  }
  comb_matrix <- matrix(,nrow=num,ncol=1)
  for (j in 1:n){
    comb_res <- combn(vector,j)
    m <- ncol(comb_res)
    for (l in 1:m){
      comb_matrix[col,1] <- paste(comb_res[,l],collapse = "+")
      col=col+1
      if(col==num)break
    }
  }
  return(comb_matrix)
}

vectorLog <- c('log(rocky2)','log(rocky3)','log(rocky4)')
linearReg= str_comb2(vector2)
linearLog=str_comb2(str_comb(vectorLog))

#LM model:

set.seed(1)
nFold=10
valNum=floor(runif(nrow(completeDB))*nFold)+1
linearmodelPerformance = matrix(NA,nFold,385)

for(fold in 1:nFold){
  trainingData=subset(completeDB,valNum!=fold)
  validationData=subset(completeDB,valNum==fold)

valid= vector()

valid[1] = mean((validationData$rocky5 - predict(lm(rocky5~rocky1+rocky2+rocky3+rocky4,data=trainingData),validationData))^2)^.5
valid[2] = mean((validationData$rocky5 - predict(lm(rocky5~(rocky1+rocky2+rocky3+rocky4)^2,data=trainingData),validationData))^2)^.5
valid[3] = mean((validationData$rocky5 - predict(lm(rocky5~(rocky1+rocky2+rocky3+rocky4)^3,data=trainingData),validationData))^2)^.5
valid[4] = mean((validationData$rocky5 - predict(lm(rocky5~(rocky1+rocky2+rocky3+rocky4)^4, data=trainingData),validationData))^2)^.5
for (i in 1:nrow(linearReg)){
  valid[i+4] = mean((validationData$rocky5 - predict(lm(rocky5~eval(parse(text = linearReg[i])), data=trainingData),validationData))^2)^.5
  valid[131+i] = mean((validationData$rocky5 - exp(predict(lm(log(rocky5)~eval(parse(text = linearLog[i])), data=trainingData),validationData)))^2)^.5
  valid[258+i] = mean((validationData$rocky5 - predict(lm(rocky5~eval(parse(text = linearLog[i])), data=trainingData),validationData))^2)^.5
  
  }

linearmodelPerformance[fold,]=valid
}


#MARSModel
### By changing degree and thres in models, we estimate 12 models for this type.
install.packages("earth")
library("earth")
set.seed(1)
nFold=10
valNum=floor(runif(nrow(completeDB))*nFold)+1
MARSmodelPerformance = matrix(NA,nFold,12)

for(fold in 1:nFold){
  trainingData=subset(completeDB,valNum!=fold)
  validationData=subset(completeDB,valNum==fold)
 
  valid1= vector() 
for (i in (1:3)) {
    for (k in c(0.0001,0.001,0.01,0.1)) {
        model = earth(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, data = trainingData,  degree = i, trace = 2, thres = k)
        MARSvalid = mean((validationData$rocky5 - predict(model,validationData))^2)^.5
        valid1 <- append(valid1, MARSvalid)
      }
  }
MARSmodelPerformance[fold,]=valid1
}

#KNNModel

### By changing size and distance in models, we estimate 100 models for this type.
install.packages("kknn")
library("kknn")
set.seed(1)
nFold=10
valNum=floor(runif(nrow(completeDB))*nFold)+1
KNNmodelPerformance = matrix(NA,nFold,100)

for(fold in 1:nFold){
  trainingData=subset(completeDB,valNum!=fold)
  validationData=subset(completeDB,valNum==fold)
  
  valid2= vector() 
  for (i in (1:10)) {
    for (v in (1:10)) {
      knnvalidmodel = kknn(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,trainingData, validationData, k = i, distance = v)
      knnvalid = mean((validationData$rocky5 - knnvalidmodel$fitted.values)^2)^.5
      valid2 <- append(valid2, knnvalid)
    }
  }
  
  KNNmodelPerformance[fold,]=valid2
}


# Neural networks models
### By changing size and skip arguement in models, we estimate 40 models for this type.

installed.packages("nnet")
library("nnet")
set.seed(1)
nFold=10
valNum=floor(runif(nrow(completeDB))*nFold)+1
NeuralmodelPerformanceF = matrix(NA,nFold,20)

for(fold in 1:nFold){
  trainingData=subset(completeDB,valNum!=fold)
  validationData=subset(completeDB,valNum==fold)


for (i in 1:20){
    
    nnetFit=nnet(rocky5~rocky1+rocky2+rocky3+rocky4,data=trainingData,linout=1,size=i,maxit=4000,skip=FALSE)
    a=mean((validationData$rocky5-predict(nnetFit,validationData))^2)^.5
    NeuralmodelPerformanceF[fold,i]=a
    }
  
}

set.seed(1)
nFold=10
valNum=floor(runif(nrow(completeDB))*nFold)+1
NeuralmodelPerformanceT = matrix(NA,nFold,20)

for(fold in 1:nFold){
  trainingData=subset(completeDB,valNum!=fold)
  validationData=subset(completeDB,valNum==fold)
  
  
  for (i in 1:20){
    
    nnetFit=nnet(rocky5~rocky1+rocky2+rocky3+rocky4,data=trainingData,linout=1,size=i,maxit=4000,skip=TRUE)
    a=mean((validationData$rocky5-predict(nnetFit,validationData))^2)^.5
    NeuralmodelPerformanceT[fold,i]=a
  }
  
  
}

# Find the best model with min MES for each type of model:
whichmodel=c(which.min(colMeans(linearmodelPerformance)),which.min(colMeans(MARSmodelPerformance)),
             which.min(colMeans(KNNmodelPerformance)),which.min(colMeans(NeuralmodelPerformanceT)),which.min(colMeans(NeuralmodelPerformanceF)))
minMSE=c(min(colMeans(linearmodelPerformance)),min(colMeans(MARSmodelPerformance)),
         min(colMeans(KNNmodelPerformance)),min(colMeans(NeuralmodelPerformanceT)),min(colMeans(NeuralmodelPerformanceF)))

#d)
#Using the entire data set, re-estimate the each of the best models in part b):
entireLinearModel=lm(rocky5~I(log(rocky3)*log(rocky4))+I(log(rocky2)*log(rocky3)*log(rocky4)),data=completeDB)
entireMARSModel=earth(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,degree = 3, trace = 2, thres = 0.001,data=completeDB)
entireKNNModel=kknn(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, k = 10, distance = 5,trainingData,completeDB)
entireNeuralModel=nnet(rocky5~rocky1+rocky2+rocky3+rocky4,linout=1,size=7,maxit=4000,skip=FALSE,data=completeDB)
entireKNNModelfinal=kknn(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, k = 10, distance = 5,trainingData,completeDB)

# Observe the preformance(out of sample MSE) of the best models on entire data set:
linearMSEentire=mean((completeDB$rocky5 - predict(entireLinearModel,completeDB))^2)^.5
MARSMSEentire=mean((completeDB$rocky5 - predict(entireMARSModel,completeDB))^2)^.5
NeuralModelentire=mean((completeDB$rocky5 - predict(entireNeuralModel,completeDB))^2)^.5
KNNModelentire=mean((completeDB$rocky5-entireKNNModelfinal$fitted.values)^2)^.5

c(linearMSEentire,MARSMSEentire,KNNModelentire,NeuralModelentire)

#e)
#load the test dataset called 'Homework 2-Test Set.csv'
testSet = read.csv(file.choose(),header = T, sep = ",") 
# Use each of the chosen models to generate a prediction of the rating of
# Rocky 5 for each consumer in the test data set:
testSet["LinearModel"]=predict(entireLinearModel, newdata = testSet) 
testSet["MARSModel"]=predict(entireMARSModel, newdata = testSet)
testSet["NeuralModel"]=predict(entireNeuralModel,newdata = testSet)
entireKNNModelTest=kknn(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, k = 10, distance = 5,completeDB,testSet)
testSet["KNNModel"]= entireKNNModelTest$fitted.values

#Save the results in csv file:
bestPredictions12=testSet[,5:8]
setwd("/Users/xieqi/Desktop/BA Courses&homework&CMC /Winter/Marketing Analytcs/HW/HW2")
write.csv(bestPredictions12,file="bestPredictions12.csv")

bestPredictions12 = read.csv('bestPredictions12')
save(bestPredictions12, file = 'bestPredictions12.Rda')
a
