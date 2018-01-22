setwd('/Users/navneetsonak/Desktop/ClassesSlides/DataMining/R')
library(readxl)
library(dummies)
library(tree)
library(ISLR)
library(dplyr)
library(gbm)
library(randomForest)

## Importing Credit Data and doing data preprocessing.

credit <- read_excel('credit3.xlsx',sheet=1)
credit$profitable <- NULL
change <- function(x){
  if (x<0)
    return(0)
  else
    return(1)
}
credit$profitable <- sapply(credit$NPV, change)

credit$CHK_ACCT <- as.factor(credit$CHK_ACCT)
credit$SAV_ACCT <- as.factor(credit$SAV_ACCT)
credit$HISTORY <- as.factor(credit$HISTORY)
credit$JOB <- as.factor(credit$JOB)
credit$TYPE <- as.factor(credit$TYPE)


credit <- dummy.data.frame(credit, all=TRUE)
credit$profitable <- as.factor(credit$profitable)

## Dividing datset into test and train
set.seed(12345)
train <- sample(nrow(credit),0.7*nrow(credit))
credit_train <- credit[train,]
credit_test <- credit[-train,]


## Running Classification tree alogorithm
tree.credit = tree(profitable~., data=credit_train[,-c(1,42,43)])
summary(tree.credit)
tree.pred1=predict(tree.credit,credit_test[,-c(1,42,43)],type="class")
table(tree.pred1,credit_test$profitable)
plot(tree.credit)
text(tree.credit,pretty=0)

## Choosing the best Pruned Classification tree size
set.seed(123)
cv.credit=cv.tree(tree.credit,K=10)
names(cv.credit)
plot(cv.credit$k,cv.credit$dev,type="b")
plot(cv.credit$size,cv.credit$dev,type="b")

##Pruned the tree
prune.credit=prune.misclass(tree.credit,best=2)
plot(prune.credit)
text(prune.credit,pretty=0)

tree.pred=predict(prune.credit,credit_test[,-c(1,42,43)],type="class")
tree_confisuion<- table(tree.pred,credit_test$profitable)
accuracy_tree <- (tree_confisuion[1,1]+tree_confisuion[2,2])/sum(tree_confisuion)
accuracy_tree

##This is how the above classification tree will work for the following data:
##The student is 27 years old, domestic, has $100 in her checking account but no savings account. 
##The applicant has 1 existing credits, and a credit duration of 12 months, and the credit was paid back duly.  
##The applicant has been renting her current place for less than 12 months, does not own any real estate, just started graduate school (the present employment variable is set to 1 and nature of job to 2). 
##The applicant has no dependents and no guarantor. 
##The applicant wants to buy a used car and has requested $4,500 in credit, and therefore the Installment rate is quite high or 2.5%, 
##however the applicant does not have other installment plan credits. 
##Finally, the applicant has a phone in her name.

pred_data <-data.frame(AGE =27, CHK_ACCT1=1,CHK_ACCT0=0,CHK_ACCT2=0,CHK_ACCT3=0, SAV_ACCT0=1,SAV_ACCT1=0,SAV_ACCT2=0,SAV_ACCT3=0,SAV_ACCT4=0,NUM_CREDITS = 1, DURATION = 12, HISTORY2=1,HISTORY0=0,HISTORY1=0,HISTORY3=0,HISTORY4=0, PRESENT_RESIDENT=1, EMPLOYMENT=1, JOB2=1,JOB0=0,JOB1=0,JOB3=0, NUM_DEPENDENTS = 1, RENT=1, INSTALL_RATE=3, GUARANTOR=0, OTHER_INSTALL=0, OWN_RES=0, TELEPHONE=1, FOREIGN=0, REAL_ESTATE=0, TYPE2=1,TYPE1=0,TYPE0=0,TYPE3=0,TYPE4=0,TYPE5=0,TYPE6=0, AMOUNT_REQUESTED=4500)
tree.pred_data <- predict(prune.credit,pred_data,type="class")
tree.pred_data_prob <- predict(prune.credit,pred_data)
tree.pred_data_prob

## Running a regression tree algorithm
tree.credit_reg= tree(NPV~.,credit_train[,-c(1,42,44)])
summary(tree.credit_reg)

pred_credit_reg <- predict(tree.credit_reg,newdata=credit_test[,-c(1,42,44)])
length(pred_credit_reg)

plot(tree.credit_reg)
text(tree.credit_reg, pretty=0)

mse <- mean((pred_credit_reg-credit_test$NPV)^2)

## pruning the regression tree
set.seed(123)
cv.credit_reg=cv.tree(tree.credit_reg)
plot(cv.credit_reg$size,cv.credit_reg$dev,type='b')

## Pruned the tree 
prune.credit_reg=prune.tree(tree.credit_reg,best=10)
plot(prune.credit_reg)
text(prune.credit_reg,pretty=0)


## Bagging
set.seed(12345)
bag.credit=randomForest(profitable~.,data=credit_train[,-c(1,42,43,45)],mtry=40,importance=TRUE)
bag.credit
bag.credit_pred <- predict(bag.credit, newdata = credit_test[,-c(1,42,43,45)])
bagging_confusion<- table(credit_test$profitable,bag.credit_pred) 
accuracy_bag <- (bagging_confusion[1,1]+bagging_confusion[2,2])/sum(bagging_confusion)
accuracy_bag
test_bag <- data.frame(credit_test$NPV, bag.credit_pred)

## Random Forest
rf.credit=randomForest(profitable~.,data=credit_train[,-c(1,42,43,45)],mtry=10,importance=TRUE)
rf.credit_pred <- predict(rf.credit, newdata = credit_test[,-c(1,42,43,45)])
rf_confusion <- table(credit_test$profitable,rf.credit_pred) 
accuracy_rf <- (rf_confusion[1,1]+rf_confusion[2,2])/sum(rf_confusion)
accuracy_rf
test_rf <- data.frame(credit_test$NPV, rf.credit_pred)





