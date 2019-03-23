setwd("C:/Users/Kamal/Desktop/Projects")
wine<-read.csv("red-wine-quality.csv", header = TRUE)
wine<-na.omit(wine)
str(wine)
wine$quality<-as.factor(wine$quality)
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
library(randomForest)
library(rattle)
#Dividing the dataset into Training and Testing sets. 
set.seed(1)
index<-createDataPartition(wine$quality, p= .8, list=FALSE)
Train<-wine[index,]
Test<-wine[-index,]
#Building the decision tree 
set.seed(1)
tree<-rpart(wine$quality~., data=wine)
prp(tree, type=3, extra=3, tweak=0.8, main="The Quality of Wine", compress=TRUE )
#Making predictions 
model1<-rpart(Train$quality~., data=Train)
pred<-predict(model1, Test, type="class")
confusionMatrix(pred, Test$quality)
# Random Forest 
set.seed(1)
model2<-randomForest(Train$quality~., data=Train, ntree=50, do.trace=T, importance=T)
# Let's look at the important variables
varImpPlot(model2)
# making predictions 
pred2<-predict(model2, newdata=Test, type="class")
confusionMatrix(pred2, Test$quality)
