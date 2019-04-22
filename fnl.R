setwd("E:/MiniProject/All projects/Project_10")

loan.train <- read.delim("Loans_Training", sep = ",", stringsAsFactor = TRUE, header=TRUE)
choose <- sample(dim(loan.train)[1], size = 1000)
train <- loan.train[choose,-5]
library(rpart)
library(caret)
train$DtIR.z <- (train$Debt.to.Income.Ratio - mean(train$Debt.to.Income.Ratio))/sd(train$Debt.to.Income.Ratio)
train$FICOs.z <- (train$FICO.Score - mean(train$FICO.Score))/sd(train$FICO.Score)
train$ReqAmt.z <- (train$Request.Amount - mean(train$Request.Amount))/sd(train$Request.Amount)
train <- train[,-c(2:4)]


train$Approval=factor(train$Approval,
                           levels=c('FALSE','TRUE'),
                           labels=c(0,1))


test<-read.csv("E:/MiniProject/All projects/Project_10/Loans_Test")

test$DtIR.z <- (test$Debt.to.Income.Ratio - mean(test$Debt.to.Income.Ratio))/sd(test$Debt.to.Income.Ratio)
test$FICOs.z <- (test$FICO.Score - mean(test$FICO.Score))/sd(test$FICO.Score)
test$ReqAmt.z <- (test$Request.Amount - mean(test$Request.Amount))/sd(test$Request.Amount)
test <- test[,-c(2:4)]

test$Approval=factor(test$Approval,
                          levels=c('FALSE','TRUE'),
                          labels=c(0,1))



#costs <??? list(loss = matrix(c(???13427, 13427, 6042, ???6042), ncol=2, byrow=TRUE))
#costs$loss[1,] <??? costs$loss[1,]+13427
#costs$loss[2,] <??? costs$loss[2,]+6042
cart.woCost <- rpart(Approval ~ DtIR.z+FICOs.z+ReqAmt.z,data = train ,method = "class")
#cart.withCost <??? rpart(Approval ~ DtIR.z+FICOs.z+ReqAmt.z,data = train ,method = "class", parms = costs)
conf <- predict(cart.woCost, newdata = train, type = "prob")
#conf.cost <??? predict(cart.withCost, newdata = train, type = "prob")
m <- data.frame(NoCost = conf[,2])
# our.lift <??? lift(as.factor(train[,1]) ~ NoCost + Cost, data = m)
our.lift <- lift(as.factor(train[,1]) ~ NoCost, data = m)
xyplot(our.lift, plot = "lift", auto.key = list(columns = 2), main = "Lift for CART Model")

# Gains Chart
xyplot(our.lift, plot = "gain", auto.key = list(columns = 2), main = "Gain for CART Model")


################################################### C5.0 ###################################################
library(C50)
C50<- C5.0(train[,2:4],train[,1])
conf2=predict(C50,train,type="prob")

k <- data.frame(NoCost1 = conf2[,2])

your.lift <- lift(as.factor(train[,1]) ~ NoCost1, data = k)
xyplot(your.lift, plot = "lift", auto.key = list(columns = 2), main = "Lift for C5.0 Model")

xyplot(your.lift, plot = "gain", auto.key = list(columns = 2), main = "Gain for C5.0 Model")



