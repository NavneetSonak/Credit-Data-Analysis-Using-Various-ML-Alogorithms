setwd('/Users/navneetsonak/Desktop/ClassesSlides/DataMining/R')
library(dummies)
library(class)
library(readxl)


## Importing Credit data
credit <- read_excel('credit3.xlsx',sheet=1)
tail(credit)
typeof(credit$NPV)

credit$CHK_ACCT <- as.factor(credit$CHK_ACCT)
credit$SAV_ACCT <- as.factor(credit$SAV_ACCT)
credit$HISTORY <- as.factor(credit$HISTORY)
credit$JOB <- as.factor(credit$JOB)
credit$TYPE <- as.factor(credit$TYPE)

credit <- dummy.data.frame(credit, all=TRUE)

## Converting NPV to categorical variable
credit$predict <- NULL
change <- function(x){
  if (x<0)
    return(0)
  else
    return(1)
}
credit$predict <- sapply(credit$NPV, change)


## Removing NPV, Credit Extended and OBS columns as they cannot be used to classify their account performance.

credit$NPV <- NULL
credit$CREDIT_EXTENDED <- NULL
credit$`OBS#` <- NULL

nrow(credit)
set.seed(12345)
train <- sample(nrow(credit),0.7*nrow(credit))
credit_train <- credit[train,]
credit_test <- credit[-train,]

head(credit_train)



## Applying KNN for various values of k and checking error percentage for each k.
sapply(credit,class)
fun <- function(x){ 
  a <- mean(x) 
  b <- sd(x) 
  (x - a)/(b) 
} 
credit_train[,c(1,3,15)] <- as.data.frame(scale(credit_train[,c(1,3,15)]))
credit_test[,c(1,3,15)] <- as.data.frame(scale(credit_test[,c(1,3,15)]))

df <- credit_train$predict
credit_train$predict <-NULL
train_input <- credit_train
train_output <- df
df1 <- credit_test$predict
credit_test$predict <- NULL
test_input <- credit_test

kmax <- 15
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)

for (i in 1:kmax){
  prediction <- knn(train_input, test_input,train_output, k=i)
  prediction1 <- knn(train_input, train_input, train_output,k=i)
  CM1 <- table(prediction1, df)
  # The training error rate is:
  ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for testing data is: 
  CM2 <- table(prediction, df1)
  ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}
ER2
## Plotting error rate vs k. 

plot(c(1,kmax),c(0,0.5),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
legend(9, 0.1, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
z <- which.min(ER2)
cat("Minimum Validation Error k:", z)

## using the Mininum error k to make predictions.
pred <- knn(train_input, train_input,train_output, k=z)
pred2 <- knn(train_input, test_input,train_output, k=z)
CM <- table(df1, pred2)
error_class1 <- (CM[2,1])/sum(CM[2,1]+CM[2,2])
error_class1
error_class0 <- (CM[1,2])/sum(CM[1,1]+CM[1,2])
error_class0
