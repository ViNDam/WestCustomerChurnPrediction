###########################
# Author: Vi Dam
# Version: 2018 05 15
###########################
# List of libraries
library(ISLR)
library(caTools)
library(caret)
library(ggplot2)
library(pROC)
library(MASS)  

# Data 
clean_dat_fri<-read.csv("/Users/vdam/Downloads/Clean_Data_Fri.csv", header=T, row.names=1)

col_numeric<-sapply(clean_dat_fri, is.numeric)

clean_dat_fri[,c("Business_Code","Location_Type","BEMFAB__Marketability_","Public_Private_Indicator","Small_Business_Indicator","Minority_Owned_Indicator","Import_Export_Agent_Code","Site_Status","Revenue_Range","Global_Ultimate_Indicator","Major_Industry_Category_Name","Chief_Executive_Officer_Gender_C")]<-
  lapply(clean_dat_fri[,c("Business_Code","Location_Type","BEMFAB__Marketability_","Public_Private_Indicator","Small_Business_Indicator","Minority_Owned_Indicator","Import_Export_Agent_Code","Site_Status","Revenue_Range","Global_Ultimate_Indicator","Major_Industry_Category_Name","Chief_Executive_Officer_Gender_C")], factor)

clean_dat_fri[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")] <- lapply(clean_dat_fri[,c("Owns_Rents_Code","Manufacturing_Indicator","Legal_Status_Code","Population_Code","Hierarchy_Code","Revenue_Range","churned")], factor)

# Split the 70% / 30%
set.seed(1000)

n = nrow(clean_dat_fri)
sample_size = 0.7*n

train_clean<-sample(n, sample_size)
test_clean<- -train_clean

training.clean<-clean_dat_fri[train_clean,]
test.clean<-clean_dat_fri[-train_clean,]

# Balance the training set

set.seed(1029)
library(DMwR)

as.data.frame(table(training.clean$churned))

balanced.train <- SMOTE(churned ~., training.clean, perc.over = 1100, k = 10, perc.under = 110)
as.data.frame(table(balanced.train$churned))

## Perform the QDA model 
qda.fit<-qda(churned ~ Company_Number + Business_Code +Location_Type + Small_Business_Indicator +Minority_Owned_Indicator + 
              Square_Footage  + Number_of_Family_Members  +Employee_Count_Total + Domestic_Ultimate_Employee_Count + 
              Domestic_Ultimate_Revenue +  Global_Ultimate_Indicator + US_1987_SIC_1 + Chief_Executive_Officer_Gender_C + 
              total_products + total_transactions + total_revenue + total_usage + total_accounts, data=balanced.train)

# Predict fucntion on test data. 
qda.pred.test<- predict(qda.fit, newdata= test.clean, type="response")

# Confusion matrix
qda.class.test<- factor(qda.pred.test$class)
levels(qda.class.test) <- levels(test.clean$churned)
confusionMatrix(qda.class.test, test.clean$churned, positive="1")

# ROC curve
pdf('qda.test.roc.pdf')
plot(roc(test.clean$churned, qda.pred.test$posterior[,1], plot = TRUE, print.auc = TRUE, legacy.axes = TRUE))
dev.off()



