#Load packages
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

#Getting the data
UrlTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
UrlTest <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(UrlTrain), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(UrlTest), na.strings=c("NA","#DIV/0!",""))

#Removed unwanted variables for both training and testing data:
#clean training data 

subTrain <- training[,names(training)[!(nzv(training,saveMetrics=T)[,4])]] 
subTrain <- subTrain[,names(subTrain)[sapply(subTrain,function(x)!(any(is.na(x)|x=="")))]]
subTrain <- subTrain[,-1]
subTrain <- subTrain[,c(1:3,5:58)]

#clean testing data
testing <- testing[,names(testing)[!(nzv(testing,saveMetrics=T)[,4])]]
testing <- testing[,names(testing)[sapply(testing,function(x)!(any(is.na(x)|x=="")))]]
testing <- testing[,-1]
testing <- testing[,c(1:3,5:58)]

dim(subTrain)
dim(testing)

#Separate the training data to be used for Cross Validation

inTrain <- createDataPartition(subTrain$classe, p = 0.6, list = FALSE)
subTraining <- subTrain[inTrain,]
subValidation <- subTrain[-inTrain,]

#Modeling-using decision tree

modFit <- rpart(classe ~., data=subTraining, method="class")
fancyRpartPlot(modFit)
predictions <- predict(modFit,subValidation,type="class")
confusionMatrix(predictions,subValidation$classe)

#Prediction-using Random Forest

modFit2 <- randomForest(classe ~., data=subTraining)
predictions2 <- predict(modFit2,subValidation, type="class")
confusionMatrix(predictions2, subValidation$classe)

#Using modFit2 to predict 20 testing cases

predictions3 <- predict(modFit2, testing, type="class")

#generate files to submit assignment

pml_write_files = function(x){
+   n = length(x)
+   for(i in 1:n){
+     filename = paste0("problem_id_",i,".txt")
+     write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
+   }
+ }

pml_write_files(prediction3)







