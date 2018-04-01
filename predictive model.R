# Part 1
# a)
# set working directory and load dataset 
setwd("~/Desktop/Winter/R/hw/hw4")
soup <- read.csv('Homework 4 Student Data.csv')

# Figure out the most popular productNum in the dataset by units sold
popularproductNum <- aggregate(units~productNum, data=soup, FUN = length)
mostpopular <- popularproductNum[ , 'productNum'][which.max(popularproductNum[ , 'units'])]

# b)
# take a subset
upcFile <- soup[soup$productNum == mostpopular, ]

# c)
aggUPCFile = aggregate(cbind(totalCost,units)~weekInYearNum+overallWeekNum+storeNum+isFeature+isDisplay,data=upcFile,FUN = sum)
aggUPCFile$pricePerCan = aggUPCFile$totalCost/aggUPCFile$units
model1 = lm(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4 )+factor(storeNum),data=aggUPCFile)

# d)
possiblePrices = data.frame(price = seq(0,10,.01)) 
possiblePrices$demand = NA
newData = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices$price)

# e)
# get the predicted demand and adjust log(units) to units
possiblePrices$demand <- exp(predict(model1, newData))

# f)
# calculate expected profit
expectedprofit <- possiblePrices$demand * (possiblePrices$price - 0.3)

# g)
# find the optimal price and the expected profit
df <- cbind(possiblePrices, expectedprofit)
optimalprice <- df[which.max(df$expectedprofit),][1]
optimalprofit <- df[which.max(df$expectedprofit),][3]

# h)
# use a new model
model2 = lm(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile)

# create a new dataframe 
possiblePrices2 = data.frame(price = seq(0,10,.01)) 
possiblePrices2$demand = NA
newData2 = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices2$price)

# predict demand
possiblePrices2$demand <- exp(predict(model2, newData2))

# predict profit
expectedprofit2 <- possiblePrices2$demand * (possiblePrices2$price - 0.3)

# find the optimal price and the expected profit
df2 <- cbind(possiblePrices2, expectedprofit2)
optimalprice2 <- df2[which.max(df2$expectedprofit2),][1]
optimalprofit2 <- df2[which.max(df2$expectedprofit2),][3]

# Part 2
# a)
library('nnet')
set.seed(1)
nnet1 = nnet(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+factor(storeNum),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)
nnet2 = nnet(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)

# b)
# use nnet1 and nnet2 to predict demand when the price is 50 cents
nnet10.5 <- exp(predict(nnet1, newData[newData$pricePerCan == 0.5, ]))
nnet20.5 <- exp(predict(nnet2, newData[newData$pricePerCan == 0.5, ]))

# use nnet1 and nnet2 to predict demand when the price is 1 dollar
nnet11 <- exp(predict(nnet1, newData[newData$pricePerCan == 1, ]))
nnet21 <- exp(predict(nnet2, newData[newData$pricePerCan == 1, ]))

demandchange1 <- nnet10.5 - nnet11
demandchange2 <- nnet20.5 - nnet21

# c)
# use nnet1 to get the optimal price
possiblePrices3 = data.frame(price = seq(0,10,.01)) 
possiblePrices3$demand = NA
newData3 = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices3$price)

possiblePrices3$demand <- exp(predict(nnet1, newData3))

expectedprofit3 <- possiblePrices3$demand * (possiblePrices3$price - 0.3)

df3 <- cbind(possiblePrices3, expectedprofit3)
optimalprice3 <- df3[which.max(df3$expectedprofit3),][1]

# use nnet2 to get the optimal price
possiblePrices4 = data.frame(price = seq(0,10,.01)) 
possiblePrices4$demand = NA
newData4 = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices4$price)

possiblePrices4$demand <- exp(predict(nnet2, newData3))

expectedprofit4 <- possiblePrices4$demand * (possiblePrices4$price - 0.3)

df4 <- cbind(possiblePrices4, expectedprofit4)
optimalprice4 <- df4[which.max(df4$expectedprofit4),][1]
optimalprofit4 <- df4[which.max(df4$expectedprofit4),][3]

# d)
# Plot predicted profit versus price
plot(df3$expectedprofit3~df3$price)
plot(df4$expectedprofit4~df4$price)

set.seed(1)
nnet1 = nnet(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+factor(storeNum),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)

# out of sample error, so predict within the sample range, use the maximum and minimum price in aggUPCFile 
possiblePrices5 = data.frame(price = seq(min(aggUPCFile$pricePerCan),max(aggUPCFile$pricePerCan),.01)) 
possiblePrices5$demand = NA
newData5 = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices5$price)

possiblePrices5$demand <- exp(predict(nnet1, newData5))

expectedprofit5 <- possiblePrices5$demand * (possiblePrices5$price - 0.3)

df5 <- cbind(possiblePrices5, expectedprofit5)
optimalprice5 <- df5[which.max(df5$expectedprofit5),][1]
optimalprofit5 <- df5[which.max(df5$expectedprofit5),][3]

# Plot predicted profit versus price
plot(df5$expectedprofit5~df5$price)
