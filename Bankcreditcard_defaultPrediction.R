library(gains)
library(dplyr)
library(irr)
library(caret)
library(e1071)
#Setting path to read file
setwd("D:\\Data science\\logistic regression\\assignment\\Dataset")
#Reading csv file
dm1<-read.csv('BankCreditCard.csv')

summary(dm1)
plot(dm1$Credit_Amount,dm1$Default_Payment)
summary(dm1$Gender)
dm1$Gender<-as.factor(dm1$Gender)
plot(dm1$Gender,dm1$Default_Payment)
plot(dm1$Academic_Qualification,dm1$Default_Payment)
plot(dm1$Marital,dm1$Default_Payment)
plot(dm1$Age_Years,dm1$Default_Payment)
plot(dm1$Repayment_Status_Jan,dm1$Default_Payment)
plot(dm1$Repayment_Status_Jan,dm1$Default_Payment)

#spiliting data sets into test and training datasets
set.seed(200)
index<-sample(nrow(dm1),.70*nrow(dm1),replace = F)
train1<-dm1[index,]
test1<-dm1[-index,]
model1<-glm(data=dm1,Default_Payment~., family = "binomial")
summary(model1)

#stepwise regression
step(model1,direction = "both")

model2<-glm(formula = Default_Payment ~ Credit_Amount + Gender + Academic_Qualification + 
      Marital + Age_Years + Repayment_Status_Jan + Repayment_Status_Feb + 
      Repayment_Status_March + Repayment_Status_April + Repayment_Status_May + 
      Repayment_Status_June + Jan_Bill_Amount + Feb_Bill_Amount + 
      Previous_Payment_Jan + Previous_Payment_Feb + Previous_Payment_March + 
      Previous_Payment_April + Previous_Payment_May + Previous_Payment_June, 
    family = "binomial", data = dm1)
summary(model2)

model3<-glm(formula = Default_Payment ~ Credit_Amount + Gender + Academic_Qualification + 
            Marital + Age_Years + Repayment_Status_Jan + 
            Repayment_Status_March + Repayment_Status_April + Repayment_Status_May + 
            Repayment_Status_June + Jan_Bill_Amount + Feb_Bill_Amount + 
            Previous_Payment_Jan + Previous_Payment_Feb + 
            Previous_Payment_April + Previous_Payment_May + Previous_Payment_June, 
          family = "binomial", data = dm1)
summary(model3)


pred1<-predict(model3,type = "response",newdata = test1)
head(pred1)

table(dm1$Default_Payment)/nrow(dm1)
pred1<-ifelse(pred1>=0.2212,1,0)

kappa2(data.frame(test1$Default_Payment,pred1))

#using confusion matrix
confusionMatrix(as.factor(pred1),as.factor(test1$Default_Payment),positive = "1")
 