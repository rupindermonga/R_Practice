ls() # current objects
rm(list=ls())  # deleting all objects
#Understanding Logistic Regression 
options(scipen=999)

# loading the required dpackages
install.packages("ISLR")
library(ISLR)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

setwd("D:/Personal/Learning/R/Logistic Regression")
# Reading the default data
default_data<-read.csv("Default.csv")

# Getting a understanding of the data
head(default_data)
str(default_data)

# plotting the logistic curve using the default data
default_data %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")

# Converting the default variable to a factor variable
default_data$default= ifelse(default_data$default == "Yes",1, 0)
#default_data$default <- factor(default_data$default, levels= c("0","1"))

# Checking the structure post conversion into factor variable
str(default_data)
View(default_data)

#Simple Logistic Regression using a categorical dependent variable
model1 <- glm(default ~ student, family = "binomial", data = default_data) 

#model summary
summary(model1)

#Simple Logistic Regression using a continuous dependent variable
model2 <- glm(default ~ balance, family = "binomial", data = default_data) 

#model summary
summary(model2)

######################################################################################################################3

#Solving the bank product purchase case study

#Step 1: Identification of dependent variable

#Step 2: Importing data

bank_data<-read.csv("bank-full.csv")
str(bank_data)

#We do not need the ID variable in any prediction hence remocing the same
bank_data<-bank_data[,-c(1)]
head(bank_data)

#Checking if the imported is correct or nor the data 
# checking the dimensions
dim(bank_data)

# Checking the structure
str(bank_data)

# Checking the variable names
names(bank_data)

# Check Sample records
head(bank_data)
tail(bank_data)

# Quick check for how many different values for each feature
sapply(bank_data, function(x) length(unique(x)))

#Step 3: Analyzing the data. Checking for quality of data
summary(bank_data)

# Check if you any unusual observations or mising values
## Imputation with mean / median / mode

# Output the number of missing values for each column
sapply(bank_data,function(x) sum(is.na(x)))

library(Hmisc)

#replacing missing value in age
bank_data$age<-as.numeric(impute(bank_data$age, mean))  # replace with mean

#replacing missing value in balance
bank_data$balance<-as.numeric(impute(bank_data$balance, mean))  # replace with mean

#checking the data post missing value treatment
summary(bank_data$age)
summary(bank_data$balance)

# R should automatically code job as a factor(). A factor is R's way of dealing with categorical variables
is.factor(bank_data$job)         # Returns TRUE
is.factor(bank_data$education)    # Returns TRUE

# Check categorical variables encoding for better understanding of the fitted model
contrasts(bank_data$job)

## Treating the outliers and corrupt values
# Correcting outliers for age 
outlier_values <- boxplot.stats(bank_data$age)$out  # outlier values.
boxplot(bank_data$age, main="Age", pars=list(boxwex=0.1))
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

## Detect outliers
#treating the outliers
# correcting outliers for the age column
x <- bank_data$age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

# replacing the age column
bank_data$age<-x
summary(bank_data$age)
boxplot(bank_data$age, main="Age", pars=list(boxwex=0.1))

# correcting outliers for the campaign column
x <- bank_data$campaign
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

# replacing the age column
bank_data$campaign<-x
summary(bank_data$campaign)
boxplot(bank_data$campaign, main="Campaign", pars=list(boxwex=0.1))

#Step 4: Bivariate analysis
##Graphical understandingo the data
#Analyzing the data to get quick understanding of the data
install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)
boxplot(bank_data$age~bank_data$y,xlab="Product Purchase",ylab="Balance",col=c("pink","lightblue"),
        main="Exploratory Data Analysis Plot\n of Purchase Versus Age")

## default vs product buy
l <- ggplot(bank_data, aes(education,fill = y))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(bank_data$y) - 1 ,bank_data$education,mean)

#Job vs Product purchase (y)
l <- ggplot(bank_data, aes(job,fill = y))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(bank_data$y) - 1 ,bank_data$job,mean)
mean(as.numeric(bank_data$y) - 1)

#Step 5: Checking correlation and multicollinearity
# install.packages("yaml")
# install.packages("ggplot2")
# install.packages("GGally")
# Visualizing correlation

library(ggplot2)
library(GGally)
library(dplyr)

# filter numeric data
ggpairs(data=bank_data[,sapply(bank_data,is.numeric)], title="bank data")

# creating a matrix of correlation
correlation_matrix<-cor(bank_data[,sapply(bank_data,is.numeric)]) #removal of 1st categorical variable as correlation is applicable for numeric values

# exporting the correlation matrix
write.csv(correlation_matrix,"correlation_matrix.csv")

#Now we see that their are few variables that are correlated. Let's check multicollibearity (relationship of one independent variable with other independent variables)
#Converting the response variable to a numeric variable for multicollinearity 
bank_data_mult<-bank_data
bank_data_mult$y<-ifelse(bank_data$y == "yes", 1, 0)

# filtering the numeric data and also adding the dependent variable 
bank_data_numeric<-select(bank_data_mult,age,balance,campaign,pdays,previous,y)
str(bank_data_numeric)

# first we will need to run the linear model
bank_lm_model<-lm(y~., data=bank_data_numeric)

# using the library car for calculaing multicllinearity
# install.packages("car")
library(car)
vif(bank_lm_model)

#Remove variables that have vif higher than 5 (this range can be from 2-6 depending on the business problem and data)
#there are no variables with high multicollinearity

# Step6
#Gauging the importance of the variables using a impurity factor (IV- Information value). This helps us remove non important variables in case you have a lot of independent variables
library(plyr)
library(dplyr)
install.packages("sqldf")
library(sqldf)
install.packages("rpart")
library(rpart)
install.packages("devtools")
library(devtools)
install.packages("rlang")
library(rlang)
install.packages("pkgload")
library(pkgload)
install.packages("pkgdown")
library(pkgdown)
install.packages("woe")
library(woe)

#Checking the information value to understand the variable importance
iv.mult(bank_data, y="y", vars=c("job","marital","education","default","housing","loan",
                                 "contact","month","poutcome"), summary="TRUE")

#The higher the IV value the  ore explanatory power it has
#Creating dummy variable for all the categorical variales. Though this is not needed in R - as it automatically creates dummy variables, it would be a standard practice for any other analytical tool

#loading the Caret package to create dummy variables

library(caret)

dmy <- dummyVars(" ~ job+marital+education+poutcome+contact+month", data = bank_data_mult,fullRank = T)
bank_data_transformed <- data.frame(predict(dmy, newdata = bank_data_mult))
summary(bank_data_transformed)

# binding the dummy data with original data
final_data<-cbind(bank_data_mult,bank_data_transformed)
str(final_data)

# removal of the redundant variables
final_data<-select(final_data,-c(job,marital,contact,education,poutcome,month))
str(final_data)

#Step 6: Creation of train and validation data
## 75% of the sample size
smp_size <- floor(0.75 * nrow(final_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(final_data)), size = smp_size)

train <- final_data[train_ind, ]
test <- final_data[-train_ind, ]

#Step 7: Creating the model on the train data
# build linear regression model on train data.
logit_model <- glm(y ~ ., data = train, family = "binomial")


# Check the model output
summary(logit_model)

#Step 7: Removal of insignificant variables. Variables with p value less than .05
#removing of the non significant variables 
names(train)

log_model <- glm(y ~ ., data=select(train,-c(age,marital.single,default,balance,housing,
                                             contact.telephone,pdays,previous,job.management,
                                             job.self.employed, job.unknown,poutcome.unknown,
                                             poutcome.unknown,job.unemployed,job.management)),family = "binomial")

# Check the model output
summary(log_model)

#similarly all insignificant variables need to be removed
#Step 8: Using the created model to make predictions
#making predictions on the test data. The command type=response gives you probabilities, else you will get log of odd ratios
predictions<-predict(log_model, test,type = "response" )
head(predictions)

#combining the predictions with the original test data
combined_data<-cbind(test,predictions)

#Converting the probabilities into 
combined_data$response <- as.factor(ifelse(combined_data$predictions>0.1, 1, 0))
str(combined_data)

#Step 9
#Check Performance on the Validation Set 
#Confusion Matrix
#Loading the caret package for the confusiob matrix
library(caret)
conf_matrix<-confusionMatrix(combined_data$response,combined_data$y,positive = "1")
conf_matrix

View(combined_data)

# plotting the ROC curve
#Loading the ROCR package
library(ROCR)

#Creating the ROCR data
logit_scores <- prediction(predictions=combined_data$predictions, labels=combined_data$y)
logit_perf <- performance(logit_scores, "tpr", "fpr")

#plotting the ROC curve
plot(logit_perf,col = "darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA, main="ROC Curve")
box()
abline(0,1, lty = 300, col = "green")
grid(col="aquamarine")

# Calculating the Area Under Curve (AUC)
logit_auc <- performance(logit_scores, "auc")
as.numeric(logit_auc@y.values)  ##AUC Value

#Calculating the KS Values
logit_ks <- max(logit_perf@y.values[[1]]-logit_perf@x.values[[1]])
logit_ks

# LCalculating the Lift
# Since the base is 0,creating the probablity for class 1
combined_data$predictions_1<-1-combined_data$predictions
str(combined_data)

table(combined_data$y)
trellis.par.set(caretTheme())
caretLift = caret::lift(y ~ predictions_1,
                        data = combined_data, cuts = 101)
plot(caretLift, values = 60, auto.key = list(columns = 3,
                                             lines = TRUE,
                                             points = FALSE))

#Finally exporting the prediction data 
write.csv(combined_data,"final_probs_test.csv")
