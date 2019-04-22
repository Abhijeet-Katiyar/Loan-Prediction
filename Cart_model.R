# loading the data
test_data<-read.csv("E:/MiniProject/All projects/Project_10/Loans_Test")
train_data<-read.csv("E:/MiniProject/All projects/Project_10/Loans_Training")
head(train_data)
tail(train_data)
summary(train_data)

# Converting categorical data into numerical values
train_data$Approval=factor(train_data$Approval,
                    levels=c('FALSE','TRUE'),
                    labels=c(0,1))

test_data$Approval=factor(test_data$Approval,
                           levels=c('FALSE','TRUE'),
                           labels=c(0,1))


# Count of NA values
sapply(train_data, function(x) sum(is.na(x)))
# Count of empty strings
sapply(train_data, function(x) length(which(x=='')))
# Counting the Count of Number of Unique values in every Column
sapply(train_data, function(x) length(unique(x)))

# checking outliers
#library(ggplot2)
#ggplot(data = train_data, aes(y=train_data$Debt.to.Income.Ratio)) + geom_boxplot() + ggtitle("Boxplot of")
#ggplot(data = train_data, aes(y=train_data$FICO.Score)) + geom_boxplot() + ggtitle("Boxplot of ")
#ggplot(data = train_data, aes(y=train_data$Request.Amount)) + geom_boxplot() + ggtitle("Boxplot of")
#ggplot(data = train_data, aes(y=train_data$Interest)) + geom_boxplot() + ggtitle("Boxplot of ")

train <- train_data[1:4]
train[,2:4] <- scale(train[,2:4]) 

test<-test_data[1:4]
test[,2:4]<-scale(test[,2:4])


library(rpart)
library(rpart.plot)

cart <- rpart(formula = Approval~Debt.to.Income.Ratio+FICO.Score+Request.Amount,
              data = train,
              method = "class")
print(cart)
summary(cart)


cart_predicts <- predict(object = cart,
                            
                            newdata = test,
                            
                            type = "class")
library(Metrics)
library(caret)

table(test_data[,1],cart_predicts)
confusionMatrix(data=cart_predicts,reference=test_data$Approval)


library(C50)
C50<- C5.0(train[,2:4],train[,1])
c50_predict=predict(C50,test,type="class")
table(test[,1],c50_predict)
confusionMatrix(data = c50_predict,
                reference = test$Approval) 

#########################################################################################################
#######################lift chart########################################################################
#########################################################################################################
# install.packages('BCA')
library('BCA')
lift.chart(c(cart,C50),data=train)

#########################################################################################################
#######################gain chart########################################################################
#########################################################################################################
library(ROCR)
gain<-prediction(c(cart_predicts,c50_predict),labels = test$Approval)


