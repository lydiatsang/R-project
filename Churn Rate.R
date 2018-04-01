#load data
setwd('~/Desktop')
load("DSBchurn.Rda")

#1  
set.seed(3478)
churn$stay<-factor(churn$stay,levels=c("LEAVE","STAY"))
train <- sample(1:nrow(churn),2*nrow(churn)/3)
churn.train <- churn[train,]
churn.test <- churn[-train,]
str(churn)

library(rpart)
library(caret)
fit <- rpart(stay~.,data=churn.train,
             control=rpart.control(xval=10,minsplit=2,cp=0))
fit

#2
churn.pred=predict(fit, churn.test,type='class')
churn.actual=churn.test$stay 
confusion.matrix=table(churn.pred,churn.actual)
confusion.matrix
bigtree=confusionMatrix(confusion.matrix,positive='STAY')
bigtree

# error rates of big tree
alpha1=(1-bigtree$byClass['Specificity'])
beta1=(1-bigtree$byClass['Sensitivity'])
Accuracy1=bigtree$overall['Accuracy']

# print the error rates
alpha1
beta1
Accuracy1

# best cp
plotcp(fit)
cp <- fit$cptable[which.min(fit$cptable[,"xerror"]),'CP']
cp


#3
fit.nice=rpart(stay~.,data=churn.train,
               control=rpart.control(xval=10,cp=cp))
churn.pred=predict(fit.nice, churn.test,type='class')
churn.actual=churn.test$stay 
confusion.matrix=table(churn.pred,churn.actual)
confusion.matrix
nicetree=confusionMatrix(confusion.matrix,positive='STAY')
nicetree
# error rates of pruned tree
alpha2=(1-nicetree$byClass['Specificity'])
beta2=(1-nicetree$byClass['Sensitivity'])
Accuracy2=nicetree$overall['Accuracy']

# print error rates
alpha2
beta2
Accuracy2

#plot the pruned tree
plot(fit.nice,uniform=T,
     branch=0.5,
     compress=F,
     main="Pruned Tree",
     margin=0.1)
text(fit.nice, use.n=T,
     all=T)

#4
library(ROCR)
churn.pred=predict(fit.nice,churn.train,type='prob')
churn.pred.score=prediction(churn.pred[,2],churn.train$stay)
churn.pred.perf=performance(churn.pred.score, 'tpr','fpr')
plot(churn.pred.perf,colorize=T,
     lwd=4)
abline(0,1)
abline(h=1)
abline(v=0)
churn.cost=performance(churn.pred.score, measure='cost',cost.fn=196000000,cost.fp=51000000)
plot(churn.cost)
str(churn.cost)

# best threshold
cutoff<-data.frame(Cutoff=churn.cost@x.values, ExplicitCost=churn.cost@y.values)
colnames(cutoff)<-c("cutoff","expense")
cutoff=cutoff[which.min(cutoff[,2]),][[1]]
cutoff

# best threshold pruned tree
churn.pred.test=predict(fit.nice,churn.test,type='prob')
churn.pred.test.cutoff=ifelse(churn.pred.test[,2] < cutoff,'LEAVE','STAY')
besttree=confusionMatrix(table(pred=churn.pred.test.cutoff,actual=churn.test$stay),positive='STAY')
besttree

# error rates
alpha3=(1-besttree$byClass['Specificity'])
beta3=(1-besttree$byClass['Sensitivity'])
Accuracy3=besttree$overall['Accuracy']

# print the error rates
alpha3
beta3
Accuracy3

#5
Expectedvalue1=541000000-51000000*alpha1-196000000*beta1
Expectedvalue2=541000000-51000000*alpha2-196000000*beta2
Expectedvalue3=541000000-51000000*alpha3-196000000*beta3
alpha=c(alpha1,alpha2,alpha3)
beta=c(beta1,beta2,beta3)
Accuracy=c(Accuracy1,Accuracy2,Accuracy3)
Expectedvalue=c(Expectedvalue1,Expectedvalue2,Expectedvalue3)
results <- data.frame(alpha,beta,Accuracy,Expectedvalue,
                      row.names=c('Big Tree','Pruned Tree','Best Threshold Pruned Tree'))
results