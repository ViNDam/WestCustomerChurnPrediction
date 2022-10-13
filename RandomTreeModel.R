###########################
# Author: Vi Dam
# Version: 2018 05 15
###########################

setwd("C:/Users/vdam/Project Data")
#BAGGING Cleaned Data

#Load Libraries
library(DMwR)
library(randomForest)
library(pROC)


#Load csv file for lean data
project = read.csv("Clean_Data_mon1.csv")
project <- project[-c(1)]


#Load csv file for clean data
project = read.csv("Clean_Data_Fri.csv")


set.seed(1029)

#Set some of the data as a factor
project[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")] <- lapply(project[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")], factor)

#Divide Data
n = nrow(project)
sample_size = 0.7*n

train = sample(n, sample_size)
test = -train


training.set = project[train,]
test.set = project[-train,]


#Use Smote to increase chruned predicitions
as.data.frame(table(training.set$churned))
balanced.data <- SMOTE(churned ~., training.set, perc.over = 950, k = 50, perc.under =165)
as.data.frame(table(balanced.data$churned))


#Random Forest model-------------------------
rf.project = randomForest(churned ~ ., data = balanced.data, importance = TRUE, ntree=1000)


#Prediction
rf.pred = predict(rf.project, newdata = test.set)

#Confusion Matrix
library(caret)
confusionMatrix(rf.pred, test.set$churned, positive = "1")


# Draw Roc and get AUC
rf.prob = predict(rf.project, newdata = test.set, type = "prob")[,2]
roc(test.set$churned, rf.prob, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE)



#BOOSTING MODEL-------------------------
library(gbm)
library(DMwR)

setwd("C:/Users/vdam/Project Data")

#Load CSV file for cleaned Data
project = read.csv("Clean_Data_Fri.csv")

#Load csv file for lean data
project = read.csv("Clean_Data_mon1.csv")
project <- project[-c(1)]

#Set data as factor

project[,c("Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")] <- lapply(project[,c("Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")], factor)


#Divide data
set.seed(1300)
n = nrow(project)
sample_size = 0.7*n

train = sample(n, sample_size)
test = -train

#Set new data frame 
project.gbm = project

#Change data to factor
project.gbm[,c("total_accounts","Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")] <- lapply(project.gbm[,c("total_accounts","Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")], factor)

#Changed churned to characters
project.gbm$churned = ifelse(project.gbm$churned == 0, "0", "1")


#Data frames for training and test
training.set = project.gbm[train,]
test.set = project.gbm[-train,]
training.set$churned = factor(training.set$churned)


#Smote model to increase churned value-------------------------
as.data.frame(table(training.set$churned))
balanced.data <- SMOTE(churned ~., training.set, perc.over = 950, k = 50, perc.under =165)

as.data.frame(table(balanced.data$churned))

#Convert back to character after Smote
balanced.data$churned = ifelse(balanced.data$churned == 0, "0", "1")


#Create GBM model 
gbm.project = gbm(churned ~ ., data = balanced.data, n.trees = 1000, cv.folds = 5, shrinkage = 0.01)


#Data Prediction
gbm.probs = predict( gbm.project, newdata = test.set, n.trees = 1000, type = "response")
gbm.pred = rep('0', length(gbm.probs))
gbm.pred[gbm.probs > 0.5] = '1'
gbm.pred = factor(gbm.pred)

#Create Confusion Matrix
confusionMatrix(gbm.pred, factor(test.set$churned), positive = "1")
roc(test.set$churned, gbm.probs, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE)

