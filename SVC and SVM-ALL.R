###########################
# Author: Vi Dam
# Version: 2018 05 15
# This code applies Random Forest, Boosting and Bagging, SVC, QDA, and multiple types of SVM models to predict customer's churn.
###########################

# ------------------------ RANDOM FOREST ------------------------
library(DMwR)
library(randomForest)
library(pROC)

setwd("C:/Users/vdam/Project Data")

# Load csv file for clean data
data = read.csv("Clean_Data_Fri.csv")

set.seed(1029)

# Set some of the data as a factor
data[,c("total_accounts","Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")] <- lapply(data[,c("total_accounts","Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")], factor)

# Divide Data
n = nrow(data)
sample_size = 0.7*n

train = sample(n, sample_size)
test = -train

training.set = data[train,]
test.set = data[-train,]

# Use Smote to create balance data
as.data.frame(table(training.set$churned))
balanced.data <- SMOTE(churned ~., training.set, perc.over = 950, k = 50, perc.under =165)
as.data.frame(table(balanced.data$churned))

# Random Forest model
rf.project = randomForest(churned ~ ., data = balanced.data, importance = TRUE, ntree=1000)
rf.pred = predict(rf.project, newdata = test.set)

# Confusion Matrix
library(caret)
confusionMatrix(rf.pred, test.set$churned, positive = "1")

# Draw Roc and get AUC
rf.prob = predict(rf.project, newdata = test.set, type = "prob")[,2]
roc(test.set$churned, rf.prob, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE)


#-------------------------BOOSTING MODEL-------------------------
library(gbm)
library(DMwR)

setwd("C:/Users/vdam/Project Data")

# Load CSV file for cleaned Data
data = read.csv("Clean_Data_Fri.csv")

# Set data as factor
data[,c("Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")] <- lapply(data[,c("Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")], factor)

# Divide data
set.seed(1300)
n = nrow(data)
sample_size = 0.7*n

train = sample(n, sample_size)
test = -train

# Set new data frame 
data.gbm = data

# Change data to factor
data.gbm[,c("total_accounts","Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")] <- lapply(data.gbm[,c("total_accounts","Chief_Executive_Officer_Gender_C","Global_Ultimate_Indicator","Site_Status" ,"Owns_Rents_Code","Manufacturing_Indicator","Location_Type","Public_Private_Indicator","Small_Business_Indicator", "Minority_Owned_Indicator","churned")], factor)

# Changed churned to characters
data.gbm$churned = ifelse(data.gbm$churned == 0, "0", "1")

# Data frames for training and test
training.set = data.gbm[train,]
test.set = data.gbm[-train,]
training.set$churned = factor(training.set$churned)


# Smote model to increase churned value-------------------------
as.data.frame(table(training.set$churned))
balanced.data <- SMOTE(churned ~., training.set, perc.over = 950, k = 50, perc.under =165)

as.data.frame(table(balanced.data$churned))

# Convert back to character after Smote
balanced.data$churned = ifelse(balanced.data$churned == 0, "0", "1")

# Create GBM model 
gbm.project = gbm(churned ~ ., data = balanced.data, n.trees = 1000, cv.folds = 5, shrinkage = 0.01)

# Data Prediction
gbm.probs = predict( gbm.project, newdata = test.set, n.trees = 1000, type = "response")
gbm.pred = rep('0', length(gbm.probs))
gbm.pred[gbm.probs > 0.5] = '1'
gbm.pred = factor(gbm.pred)

# Create Confusion Matrix
confusionMatrix(gbm.pred, factor(test.set$churned), positive = "1")
roc(test.set$churned, gbm.probs, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE)

# ------------------------ SUPPORT VECTOR CLASSIFIER SVM ------------------------
library(e1071) #package for SVM in R
library(caret)
library(pROC)
library(forcats)

setwd("/Users/vdam/Project Data/")
data <- read.csv("Clean_Data_Fri.csv")

data[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")] <- 
  lapply(data[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")], factor)

set.seed(1)

n = nrow(data)
sample_size = 0.7*n

train = sample(n, sample_size)
test = -train

training.set = data[train,]
test.set = data[-train,]

# Balance Dataset------------------------
# balance variables so predicted values are even
set.seed(1029)
library(DMwR)

# change po and pu for other type of data set: 100-900, 1000-200, 200-300, 500-120, -100
po = 1000
pu = 200
as.data.frame(table(training.set$churned))
balanced.data <- SMOTE(churned ~., training.set, perc.over = 1000, k = 5, perc.under = 200)
as.data.frame(table(balanced.data$churned))

training.set <- balanced.data

# Model
tune.settings = tune.control(sampling = "cross", cross = 2)

tune.svc = tune(svm, churned ~ ., data = training.set, kernel = "linear", 
                ranges=list(cost=c(1,5,10)), 
                tunecontrol = tune.settings, probability = TRUE)

summary(tune.svc)
best.model = tune.svc$best.model
summary(best.model)

best.model.pred = predict(best.model, newdata = test.set)
best.model.prob = predict(best.model, newdata = test.set, probability = TRUE)
best.model.prob = attr(best.model.prob, "probabilities")[,2]

# Confusion Matrix -----
confusionMatrix(best.model.pred, test.set$churned, positive = "1")
#roc(test.set$churned, best.model.prob, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE)
save.image(file=paste("Linear-", po, "-", pu,"-cost-",cst,".RData", sep=""))

# ROC curve -----
png("Rplots.png")
plot(roc(test.set$churned, best.model.prob, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE))
dev.off()

pred0.3 <- best.model.prob
pred0.3[pred0.3 > 0.3] <- 1
pred0.3[pred0.3 <= 0.3] <- 0

confusionMatrix(as.factor(pred0.3), test.set$churned, positive = "1")

plot(roc(test.set$churned, as.numeric(pred0.3), plot = TRUE, print.auc = TRUE, legacy.axes = TRUE))
dev.off()

# ------------------------SUPPORT VECTOR MACHINE SVM Radial------------------------
setwd("/Users/ViDam/Project Data/")

library(e1071) #package for SVM in R
library(caret)
library(pROC)
library(forcats)

data <- read.csv("Clean_Data_Fri.csv")
data[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")] <-
  lapply(data[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")], factor)

set.seed(1)

n = nrow(data)
sample_size = 0.7*n

train = sample(n, sample_size)
test = -train

training.set = data[train,]
test.set = data[-train,]

set.seed(1029)
library(DMwR)

as.data.frame(table(training.set$churned))

# change po and pu for other data set: 100-900, 1000-200, 200-300, 500-120, -100
print("balanced.data <- SMOTE(churned ~., training.set, perc.over = 100, k = 5, perc.under = 900)")
balanced.data <- SMOTE(churned ~., training.set, perc.over = 100, k = 5, perc.under = 900)
as.data.frame(table(balanced.data$churned))

training.set <- balanced.data

tune.settings = tune.control(sampling = "cross", cross = 2)
tune.svm = tune(svm, churned ~ ., data = training.set, kernel = "radial",
                ranges=list(cost=c(0.5,1,100),gamma=c(0.01,0.05,1)),
                tunecontrol = tune.settings, probability = TRUE)

# let's look at the summary
summary(tune.svm)

# and return the best model
best.model.svm = tune.svm$best.model

summary(best.model.svm)

# let's calculate the test set performance for this
best.model.svm.pred = predict(best.model.svm, newdata = test.set)

best.model.svm.prob = predict(best.model.svm, newdata = test.set, probability = TRUE)
best.model.svm.prob = attr(best.model.svm.prob, "probabilities")[,2]

# confusion matrix -----
confusionMatrix(best.model.svm.pred, test.set$churned, positive = "1")

save.image(file="SVMRadial-100-900.RData")

# ------------------------SUPPORT VECTOR MACHINE SVMPoly------------------------
library(e1071) #package for SVM in R
library(caret)
library(pROC)
library(forcats)

data <- read.csv("Clean_Data_Fri.csv",stringsAsFactors = FALSE)
#data[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")] <-
#  lapply(data[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")], factor)

set.seed(1)

n = nrow(data)
sample_size = 0.7*n

train = sample(n, sample_size)
test = -train

training.set = data[train,]
test.set = data[-train,]

set.seed(1029)
library(DMwR)

as.data.frame(table(training.set$churned))
print("balanced.data <- SMOTE(churned ~., training.set, perc.over = 200, k = 5, perc.under = 300)")
balanced.data <- SMOTE(churned ~., training.set, perc.over = 200, k = 5, perc.under = 300)
as.data.frame(table(balanced.data$churned))

training.set <- balanced.data
tune.settings = tune.control(sampling = "cross", cross = 2)
tune.svm.poly = tune(svm, churned ~ ., data = training.set, kernel = "polynomial",
                     ranges=list(cost=c(1),coef0=c(0,1)),
                     tunecontrol = tune.settings, probability = TRUE)

# let's look at the summary
summary(tune.svm.poly)

# and return the best model
best.model.svm.poly = tune.svm.poly$best.model

summary(best.model.svm.poly)

# let's calculate the test set performance for this
best.model.svm.pred.poly = predict(best.model.svm.poly, newdata = test.set)

best.model.svm.prob.poly = predict(best.model.svm.poly, newdata = test.set, probability = TRUE)
best.model.svm.prob.poly = attr(best.model.svm.prob.poly, "probabilities")[,2]

#confusion matrix -----
confusionMatrix(best.model.svm.pred.poly, test.set$churned, positive = "1")

save.image(file="SVMPoly-200-300.RData")

# ------------------------ SUPPORT VECTOR MACHINE SVMSigmoid------------------------
library(e1071) #package for SVM in R
library(caret)
library(pROC)
library(forcats)

data <- read.csv("Clean_Data_Fri.csv")
data[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")] <-
  lapply(data[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")], factor)

set.seed(1)

n = nrow(data)
sample_size = 0.7*n

train = sample(n, sample_size)
test = -train

training.set = data[train,]
test.set = data[-train,]

set.seed(1029)
library(DMwR)

as.data.frame(table(training.set$churned))
## change perc.over = 1000, perc.under = 200 for the other Sigmoid data that we chose as one of the best
print("balanced.data <- SMOTE(churned ~., training.set, perc.over = 500, k = 5, perc.under = 120)")
balanced.data <- SMOTE(churned ~., training.set, perc.over = 500, k = 5, perc.under = 120)
as.data.frame(table(balanced.data$churned))

training.set <- balanced.data

tune.settings = tune.control(sampling = "cross", cross = 2)
tune.svm.sig = tune(svm, churned ~ ., data = training.set, kernel = "sigmoid",
                    ranges=list(cost=c(0.01,1,10),d=c(3,4),
                                gamma=c(0.01,0.5,1),coef0=c(0,1)),
                    tunecontrol = tune.settings, probability = TRUE)
# let's look at the summary
summary(tune.svm.sig)

# and return the best model
best.model.svm.sig = tune.svm.sig$best.model

summary(best.model.svm.sig)

# let's calculate the test set performance for this
best.model.svm.pred.sig = predict(best.model.svm.sig, newdata = test.set)

best.model.svm.prob.sig = predict(best.model.svm.sig, newdata = test.set, probability = TRUE)
best.model.svm.prob.sig = attr(best.model.svm.prob.sig, "probabilities")[,2]

# confusion Matrix -----
confusionMatrix(best.model.svm.pred.sig, test.set$churned, positive = "1")
save.image(file="SVMSigmoid-500-120.Rdata")
# ROC curve -----
png("Rplots.png")
plot(roc(test.set$churned, best.model.svm.prob.sig, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE))
dev.off()

# best model-----
pred0.3 <- best.model.svm.prob.sig
pred0.3[pred0.3 > 0.3] <- 1
pred0.3[pred0.3 <= 0.3] <- 0

# confusion Matrix -----
confusionMatrix(as.factor(pred0.3), test.set$churned, positive = "1")
# ROC curve -----
plot(roc(test.set$churned, as.numeric(pred0.3), plot = TRUE, print.auc = TRUE, legacy.axes = TRUE))
dev.off()
