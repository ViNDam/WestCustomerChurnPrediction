library(forcats)
library(DataExplorer)
library(ggplot)
library(GGally)
library(scales)
library(reshape)
library(ggplot2)

setwd("C:/Users/vdam/Project Data/")

#Load CSV
Project <- read.csv("Firmographic Data for Students.csv")
Projectb <- read.csv("Model Dataset for Students.csv")

colnames(Projectb)[colnames(Projectb)=="Company_number"] <- "Company_Number"

total <- merge(Project,Projectb, By="Company_Number",all.y= TRUE)
newdata <-total
newdata[,c("churned","Year_Started", "Owns_Rents_Code","Subsidiary_Indicator","Manufacturing_Indicator","Legal_Status_Code","Currency_Code","Population_Code","Hierarchy_Code","Status_Code")]<- lapply(newdata[,c("churned","Year_Started", "Owns_Rents_Code","Subsidiary_Indicator","Manufacturing_Indicator","Legal_Status_Code","Currency_Code","Population_Code","Hierarchy_Code","Status_Code")],factor)


# replace irrelevant characters to NA
for (i in 1:length(newdata)){ 
  
  #newdata[,i]
  if ((any(sapply(newdata[,i], function(x) "" %in% x)))){
    newdata[,i][newdata[,i] == ""] <- NA
    
  }
  if ((any(sapply(newdata[,i], function(x) "." %in% x)))){
    newdata[,i][newdata[,i] == "."] <- NA
    
  }
  if ((any(sapply(newdata[,i], function(x) "--" %in% x)))){
    newdata[,i][newdata[,i] == "--"] <- NA
    
  }
  if ((any(sapply(newdata[,i], function(x) "----" %in% x)))){
    newdata[,i][newdata[,i] == "----"] <- NA
    
  }
  if ((any(sapply(newdata[,i], function(x) "," %in% x)))){
    newdata[,i][newdata[,i] == ","] <- NA
    newdata$Employee_Count_Total
  }
  if ((any(sapply(newdata[,i], function(x) "0" %in% x))&& ((colnames(newdata[i])=="CITY")||(colnames(newdata[i])=="ZIP"))||(colnames(newdata[i])=="Year_Started"))){
    newdata[,i][newdata[,i] == 0] <- NA
    
  }
  if (colnames(newdata[i])=="ZIP"){
    z1 <-substring(as.character(newdata[,i]),1,2)
    newdata$ZIP_cat<-as.numeric(z1)
  }
  if (is.factor(newdata[,i])){
    newdata[,i]<-fct_explicit_na(newdata[,i])
  }
  }


newdata$total_accounts_cat <- as.factor(newdata$total_accounts) 
newdata$ZIP_cat <- as.factor(newdata$ZIP_cat)

# assign catergorize variables
newdata[,c("churned","Year_Started", "Owns_Rents_Code","Subsidiary_Indicator","Manufacturing_Indicator","Legal_Status_Code","Currency_Code","Population_Code","Hierarchy_Code","Status_Code","ZIP_cat")]<- lapply(newdata[,c("churned","Year_Started", "Owns_Rents_Code","Subsidiary_Indicator","Manufacturing_Indicator","Legal_Status_Code","Currency_Code","Population_Code","Hierarchy_Code","Status_Code","ZIP_cat")],factor)

plot_missing(newdata)

# replace NA values of Business_Code to 'missing' and consider as missing level
a<-fct_explicit_na(newdata[,2])
levels(a)
newdata$Business_Code <- a

# col 48: churned
# col 2: Business_Code
counts <- table(newdata[,48], newdata[,2])


# plot the catergorize variables with the level equal or less than 21
for (i in 1:length(newdata)){ 
  if (i!=48 && length(unique(newdata[,i]))<22  ){
    
    counts <- table(newdata[,48], newdata[,i])
    print(counts)
    
    
    c=counts
    for (i in 1:(length(counts)/2)){
      c[1,i]<- (counts[1,i]/sum(counts[,i]))
      c[2,i] <-( counts[2,i]/sum(counts[,i]))}
    
  print (colnames(newdata[i]))
   barplot(c, main=colnames(newdata[i]), las=2,
            xlab=colnames(c[,2]), col=c("darkblue","red"),
            legend = rownames(c))
  }
}


datm <- melt(cbind(counts, ind = rownames(counts)), id.vars = c('ind'))

ggplot(datm,aes(x = X2, y = value,fill = X1)) +
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())


# remove all variables that contain too many missing values
myvars <- names(newdata) %in% c("COUNTRY_CODE","STATE","CITY","ZIP","Related_Industries","Line_of_Business",
                                "Chief_Executive_Officer_Title","First_Executive_Title","Second_Executive_Title","Third_Executive_Title",
                                "Company_Creation_Date")

#adjusted <- newdata[!myvars]


ggpairs(adjusted)

# any(sapply(newdata$Year_Started, function(x) "0" %in% x))
# 
# sum(is.na(newdata$total_products))
# 
# unique(newdata$BEMFAB__Marketability_)

c=counts
for (i in 1:(length(counts)/2)){
  c[1,i]<- (counts[1,i]/sum(counts[,i]))
  c[2,i] <-( counts[2,i]/sum(counts[,i]))
}

adjusted <-newdata
adjusted2 <- adjusted

adjusted3 <- data.frame()
adjusted3 <- rbind(adjusted3,adjusted2["churned"])
adjusted3 <- subset(adjusted3,select=-c(churned))

# Calculate NA percentage (Numerical) and number of levels (Category)
# select the numerical variables that has missing values less than 30%
# select the catergoical variables that have level equal or less than 21
for (i in 1:(length(adjusted2))){
  #col_name <- colnames(adjusted2[i])
  if (is.numeric(adjusted2[,i]) && (sum(is.na(adjusted2[,i]))/length(adjusted2[,i])) <0.2){
    
    #print (sum(is.na(adjusted2[,i]))/length(adjusted2[,i]))
    #print(mean(adjusted2[,i], na.rm = TRUE))
    adjusted2[is.na(adjusted2[,i]), i] <- mean(adjusted2[,i], na.rm = TRUE)
    
    adjusted3$name <- adjusted2[,i]
    colnames(adjusted3)[which(names(adjusted3)=="name")] <- colnames(adjusted2[i])
    
    #colnames(test)[which(names(test) == "i")] <- colnames(adjusted2[,i])
    #print(colnames(adjusted2[i]))
    
  }
  else if ((is.factor(adjusted2[,i]) && length(unique(adjusted2[,i]))<22 )){
  adjusted3$name <- adjusted2[,i]
  colnames(adjusted3)[which(names(adjusted3)=="name")] <- colnames(adjusted2[i])
  }
}

adjusted3 <- subset(adjusted3,select=-c(Subsidiary_Indicator,Status_Code))

levels(adjusted3$Revenue_Range) <- c(levels(adjusted3$Revenue_Range),'More than $500 mil','$1 mil to less than $500 mil')

adjusted3$Revenue_Range[adjusted3$Revenue_Range == ""] <- "(Missing)"

new_RR <- c('$500 mil to less than $1 bil','$1 bil and above')
adjusted3$Revenue_Range[adjusted3$Revenue_Range %in% new_RR] <- 'More than $500 mil'

new_RR <- c('$1 mil to less than $5 mil','$10 mil to less than $25 mil','$100 mil to less than $250 mil',
            '$25 mil to less than $50 mil','$250 mil to less than $500 mil','$5 mil to less than $10 mil',
            '$50 mil to less than $100 mil')
adjusted3$Revenue_Range[adjusted3$Revenue_Range %in% new_RR] <- '$1 mil to less than $500 mil'

#levels(adjusted3$Revenue_Range) <- c('Less than $1 mil','More than $500 mil','$1 mil to less than $500 mil','(Missing)')
adjusted4 <- adjusted3

write.csv(adjusted3, "Clean_Data_Fri.csv")



