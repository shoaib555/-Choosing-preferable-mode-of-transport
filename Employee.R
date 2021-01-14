#The numeric variables Salary and Work.Exp are Skewed to the right, some variable have 
#been converted to factors and a new variable Tranport1 has been created by combining 
#the levels 2-wheeler and Public transport (0=Other and 1= Car)
#which will be used as a DV for further analysis, missing values have been omitted.
rm(list = ls())
ca=read.csv("cars.csv",header = T)
ca=na.omit(ca)
ca$Transport1=ifelse(ca$Transport==2,0,ifelse(ca$Transport=="P",0,1))
table(ca$Transport1)
library(DataExplorer)
plot_histogram(ca) 
ca$Engineer=as.factor(ca$Engineer)
ca$MBA=as.factor(ca$MBA)
ca$Gender=as.factor(ca$Gender)
ca$Transport1=as.factor(ca$Transport1)
ca$license=as.factor(ca$license)
summary(ca)
str(ca)
ca=ca[,-9]
cai=ca[,c(1,5,6,7)]
library(RColorBrewer)
boxplot(cai,
        las=3,
        horizontal = TRUE,
        cex= 0.8,
        par(cex.axis = 0.8),
        col=brewer.pal(8,"Set1"),
        main = "Boxplots of continous variables")

#Visualizing the factor and Numeric variables separately against the DV Transport1. 
cef=ca[,c(2,3,4,8)]
par(mfrow=c(2,2))
for (i in names(cef)) {
  print(i)
  print(table(ca$Transport1, cef[[i]]))
  barplot(table(ca$Transport1, cef[[i]]),
          col=c("grey","blue"),
          main = names(cef[i]))}
par(mfrow=c(1,1))

#Observations
#a)In terms of Graduation, Under-Graduates (Engineer) contribute 6% as 
#compared with the Post-Graduates (MBA) who contribute only 2% to the overall Car usage which is 8%
#b)In terms of Gender, Males seems to dominate cars use with a contribution of 7% to overall 8%
#c)In terms of license, Employees with License contribute 7% to the overall 8%.

cai=ca[,c(1,5,6,7)]
ca2 <- cbind(cai, ca$Transport1)
colnames(ca2)[5] <- "Transport"
str(ca2)
# stack the data using melt function. 
library(reshape2)
nd2.melt<- melt(ca2, id = c("Transport"))
#  box plots
library(tidyverse)
zz <- ggplot(nd2.melt, aes(x=Transport, y=value))
zz+geom_boxplot(aes(color = Transport), alpha=0.7 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 3)+
  ggtitle("Box Plots for continous variables vs DV")+coord_flip()

#Observation:
#a)The Interquartile range for the employees who use car to commute are higher as compared to employees who use other modes to commute.
#b)Average Salary for employees who use car to commute is 41.29 as compared to the other mode of transport which is 13.05.
#c)Average Age of employees who use car as a mode to commute is 36 as compare with other mode of transport which is 26.
#d)Average Work Exp for employees who use car to commute is 17.5 as compare with other mode of transport which is 4.8.
#e)Average Distance for employees who uses car to commute is 17 as compare with other mode of transport which is 10.

#Finding relation between variable by using Multi-variate analysis.
bk=ggplot(ca,aes(x=Salary,y=Age,color=Transport1))
bk+geom_point(aes(color=Transport1))+ggtitle("Gender & Engineer vs Age & Salary")+facet_grid(Gender~Engineer)

#Observation: 
#There appears to be a linear relationship between the variables.

bk=ggplot(ca,aes(x=Distance,y=Age,color=Transport1))
bk+geom_point(aes(color=Transport1))+ggtitle("Gender & Engineer vs Age & Distance")+facet_grid(Gender~Engineer)

#Observation: 
#Male Engineer employees above 30 years in age and where distance is greater than 12.5 prefer commuting in car.

#Dropping Work.Exp from further analysis after checking for multi-collinearity and
#Variance Inflation using Linear regression.
library(corrplot)
str(ca)
ca$Gender=ifelse(ca$Gender=="Male",1,0)
ca[,1:9]=lapply(ca[,1:9],as.integer)
str(ca)
ca$Transport1=as.numeric(ca$Transport1)
colnames(ca)[9]="Transport"
corrplot(cor(ca),method="number")

library(car)
str(ca)
#After Dropping Work.Exp
m=lm(Distance~Age+Gender+MBA+license+Salary+Transport+Engineer,data=ca)
summary(m)
vif(m)
ca=ca[,-6]

#KNN With Smote:
#Preparing the data and splitting it into 70:30 ratio.
library(caTools)
nor <-function(x) { (x -min(x))/(max(x)-min(x))}
ca_norm <- as.data.frame(lapply(ca[,c(1:8)], nor))
summary(ca_norm)
ca_norm$Transport=as.factor(ca_norm$Transport)
set.seed(1900)
str(ca_norm)
spl=sample.split(ca_norm,SplitRatio = 0.7)
train=subset(ca_norm,spl==T)
test=subset(ca_norm,spl==F)
dim(train)
dim(test)
prop.table(table(train$Transport))
prop.table(table(test$Transport))
str(ca_norm)


library(caret)
library(DMwR)

#Setting the control parameter  will be using 5 as number of neighbours for the best tune
ctrl=trainControl(method = "repeatedcv",
                  number=10,
                  repeats = 10,
                  verboseIter = F,
                  sampling = "smote")


knn_fit = train(Transport ~., data = train, method = "knn",
                trControl = ctrl,
                tuneLength = 5)
knn_fit$bestTune$k

#Confusion Matrix:
final=data.frame(actual=test$Transport,predict(knn_fit,newdata = test,type="prob"))
final$pred=ifelse(final$X0>0.5,"other","car")
table(test$Transport,final$pred)

pred=predict(knn_fit,newdata=test,type="raw")
tab=table(test$Transport,pred)
tab
confusionMatrix(test$Transport,data=pred,positive="1")
varImp(knn_fit)

#Naïve Bayes with Smote:
#Preparing the data and splitting it into 70:30 ratio.
ca=read.csv("cars.csv",header = T)
ca$Transport=ifelse(ca$Transport==2,0,ifelse(ca$Transport=="P",0,1))
summary(ca)
ca=na.omit(ca)
ca$Transport=as.factor(ca$Transport)
ca=ca[,-5]
summary(ca)
ca$Age1=as.numeric(cut(ca$Age,4))
ca$Age1=as.factor(ca$Age1)
summary(ca$Age1)
ca$Salary1=as.factor(as.numeric(cut(ca$Salary,4)))
summary(ca$Salary1)
ca$Distance1=as.factor(as.numeric(cut(ca$Distance,4)))
summary(ca)
ca$Engineer=as.factor(ca$Engineer)
ca$MBA=as.factor(ca$MBA)
ca$license=as.factor(ca$MBA)
ca$Gender=ifelse(ca$Gender=="Male",1,0)
ca$Gender=as.factor(ca$Gender)
str(ca)
can=ca[,-c(1,5,6)]
summary(can)
set.seed(100)
spl=sample.split(can,SplitRatio = 0.7)
trn=subset(can,spl==T)
tes=subset(can,spl==F)
dim(trn)
dim(tes)
prop.table(table(trn$Transport))
prop.table(table(tes$Transport))

nbb = train(Transport~., data = trn, method = "nb",
            trControl = ctrl,
            tuneLength = 10)

final=data.frame(actual=tes$Transport,predict(nbb,newdata = tes,type="prob"))
final$pred=ifelse(final$X1>0.5,"1","0")
table(final$pred,test$Transport)
table(test$Transport)
pre=predict(nbb,newdata=tes,type="raw")
tab=table(tes$Transport,pre)
tab
confusionMatrix(test$Transport,data=pre,positive="1")
varImp(nbb)

#Logistic Regression with Smote:
#Preparing the data and partitioning to 70:30 ratio
library(caTools)
ca=read.csv("cars.csv",header = T)
str(ca)
summary(ca)
ca=na.omit(ca)
summary(ca)
ca$Transport=ifelse(ca$Transport==2,0,ifelse(ca$Transport=="P",0,1))
str(ca)
ca$Engineer=as.factor(ca$Engineer)
ca$MBA=as.factor(ca$MBA)
ca$Gender=as.factor(ca$Gender)
ca$Transport=as.factor(ca$Transport)
str(ca)
summary(ca)
ca$license=as.factor(ca$license)
summary(ca)
ca=ca[,-5]
set.seed(1979)
spl=sample.split(ca,SplitRatio = 0.7)
train=subset(ca,spl==T)
test=subset(ca,spl==F)
dim(train)
dim(test)
summary(ca)
library(DMwR)
balanced.gd <- SMOTE(Transport~.,train, perc.over = 100, k = 5, perc.under = 450)
table(balanced.gd$Transport)
logi=glm(Transport~Distance+Salary,data=balanced.gd,family=binomial(link="logit"))
summary(logi)
logi$residuals
table(test$Transport)
pred=predict(logi,newdata=test,type="response")
test$Pred=predict(logi,test,type="response")
test$Pred=ifelse(test$Pred>0.5,1,0)
test$Pred=as.factor(test$Pred)
confusionMatrix(test$Transport,test$Pred,positive = "1")

#Random Forest with Smote(Bagging Technique):
library(caTools)
library(caret)
set.seed(122)
spl=sample.split(ca,SplitRatio = 0.7)
train=subset(ca,spl==T)
test=subset(ca,spl==F)
dim(train)
dim(test)
library(DMwR)
balanced.gd <- SMOTE(Transport~.,train, perc.over = 150, k = 5, perc.under = 400)
table(balanced.gd$Transport)
mr=train(Transport~.,data =balanced.gd,method="rf",trcontrol=ctrl,family=binomial)
pre=predict(mr,newdata=test,type="raw")
confusionMatrix(test$Transport,data=pre,positive="1")
varImp(mr)

#Bagging using Smote:
library(ipred)
library(rpart)
str(ca)
set.seed(100)
spl=sample.split(ca,SplitRatio = 0.7)
train=subset(ca,spl==T)
test=subset(ca,spl==F)
dim(train)
dim(test)
balanced.gd <- SMOTE(Transport~.,train, perc.over = 100, k = 5, perc.under = 400)
table(balanced.gd$Transport)
bag=bagging(Transport~.,data=balanced.gd,control=rpart.control(maxdepth=5, minsplit=4))
pred=predict(bag,newdata=test,type="class")
confusionMatrix(test$Transport,data=pred,positive = "1")
varImp(bag)

#Gradient Boosting without Smote:
ca=read.csv("cars.csv",header = T)
ca=na.omit(ca)
ca$Transport=ifelse(ca$Transport==2,0,ifelse(ca$Transport=="P",0,1))
str(ca)
summary(ca)
ca[,1:9]=lapply(ca[,1:9],as.integer)
nor <-function(x) { (x -min(x))/(max(x)-min(x))}
ca_norm <- as.data.frame(lapply(ca[,c(1:9)], nor))
library(caTools)
set.seed(1547)
spl=sample.split(ca_norm,SplitRatio = 0.7)
train=subset(ca_norm,spl==T)
test=subset(ca_norm,spl==F)
dim(train)
dim(test)
library(gbm)
gbm.fit <- gbm(
  formula = Transport ~ .,
  distribution = "bernoulli",
  data = train,
  n.trees = 100,
  interaction.depth = 1,
  shrinkage = 0.01,
  cv.folds = 5,
  n.cores = NULL, 
  verbose = FALSE
)  
test$pred=predict(gbm.fit,test,type="response")
test$pred<- ifelse(test$pred<0.5,0,1)
table(test$Transport,test$pred)
confusionMatrix(data=factor(test$pred),
                reference=factor(test$Transport),
                positive='1')

#XgBoost with Smote:
library(xgboost)
ca=read.csv("cars.csv",header = T)
str(ca)
summary(ca)
ca=na.omit(ca)
summary(ca)
ca$Transport=ifelse(ca$Transport==2,0,ifelse(ca$Transport=="P",0,1))
str(ca)
ca$Engineer=as.factor(ca$Engineer)
ca$MBA=as.factor(ca$MBA)
ca$Gender=as.factor(ca$Gender)
ca$Transport=as.factor(ca$Transport)
str(ca)
summary(ca)
ca$license=as.factor(ca$license)
summary(ca)
ca=ca[,-5]
ca[,1:8]=lapply(ca[,1:8],as.integer)
nor <-function(x) { (x -min(x))/(max(x)-min(x))}
ca_norm <- as.data.frame(lapply(ca[,c(1:8)], nor))
set.seed(1478)
spl=sample.split(ca_norm,SplitRatio = 0.7)
train=subset(ca_norm,spl==T)
test=subset(ca_norm,spl==F)
dim(train)
dim(test)
str(train)
train$Transport=as.factor(train$Transport)
library(DMwR)
balanced.gd <- SMOTE(Transport~.,train, perc.over = 150, k = 5, perc.under = 400)
table(balanced.gd$Transport)
str(balanced.gd)
balanced.gd[,1:8]=lapply(balanced.gd[,1:8],as.integer)
prop.table(table(balanced.gd$Transport))
str(balanced.gd)
balanced.gd$Transport=ifelse(balanced.gd$Transport==2,1,0)
table(balanced.gd$Transport)
smote_features_train<-as.matrix(balanced.gd[,1:7])
smote_label_train<-as.matrix(balanced.gd[,8])
smote_features_test<-as.matrix(test[,1:7])
smote.xgb.fit <- xgboost(
  data = smote_features_train,
  label = smote_label_train,
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  nrounds = 50,
  nfold = 5,
  objective = "binary:logistic",  
  verbose = 0,               
  early_stopping_rounds = 10
)
test$pred<- predict(smote.xgb.fit,smote_features_test,type="response")
test$pred<- ifelse(test$pred<0.5,0,1)
table(test$Transport,test$pred)
table(test$Transport)
confusionMatrix(data=factor(test$pred),
                reference=factor(test$Transport),
                positive='1')

