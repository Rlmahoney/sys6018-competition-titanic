library(readr)  
library(dplyr)


train<-read_csv("train.csv")
test<-read_csv("test.csv") 

#convert train columns to factors
train$Survived<-factor(train$Survived)
train$Pclass<-factor(train$Pclass)
train$Sex<-factor(train$Sex)
train$Embarked<-factor(train$Embarked)
train$Ticket<-factor(train$Ticket)
train$Cabin<-factor(train$Cabin)

#convert test columns to factors 
test$Pclass<-factor(test$Pclass)
test$Sex<-factor(test$Sex)
test$Embarked<-factor(test$Embarked)
test$Ticket<-factor(test$Ticket)
test$Cabin<-factor(test$Cabin)



#run initial logistic regression
train.lg <- glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, family = "binomial")

#summary(train.lg)

#anova(train.lg,test="Chisq")

#Pclass, Sex, age, and SibSp appear as the most statistically significant predictors so rerun the regression using only those variables as inputs
train.lg2 <- glm(Survived~Pclass+Sex+Age+SibSp, data=train, family = "binomial")

#summary(train.lg2)
#anova(train.lg2,test="Chisq")


#set the age for the observations in the test with no value in the age field to be the average age from the training set
train_mean_age<-mean(train$Age,na.rm = TRUE)
test$Age[is.na(test$Age)==TRUE]<-train_mean_age

#apply logistic regression model to the test data 

probs<-as.vector(predict(train.lg2,newdata=test, type="response"))
predictions <- rep(0,418)  # Initialize prediction vector
predictions[probs>0.5] <- 1 # if probs >.5 predict survival 

#create output csv 
output<-cbind(test$PassengerId,predictions)
colnames(output) <- c("Passengerid", "Survived")
write.csv(output, file = "rlm4bj_titanic.csv",row.names=FALSE)


