## Example Problem
#For this analysis, we will use the cars dataset that comes with R by default. cars is a standard built-in dataset, You can access this dataset simply by typing in cars in your R console. You will find that it consists of 50 observations(rows) and 2 variables (columns) dist and speed. Lets print out the first six observations here..
head(cars)  # display the first 6 observations

## Graphical Analysis
### Scatter Plot
plot(cars$dist)
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

### BoxPlot to Check for outliers
#Generally, any datapoint that lies outside the 1.5interquartile-range(1.5*IQR) is considered an outlier
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

### Density plot to Check if the response variable is close to normality
install.packages("e1071")
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")

plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")

## Correlation
cor(cars$speed, cars$dist) 

## Build Linear Model
linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)
#dist = -17.579 + 3.932*speed

## Linear Regression Diagnostics
summary(linearMod) 

### The p Value: Checking for statistical significance

### How to calculate the t Statistic and p-Values?

modelSummary <- summary(linearMod)  # capture model summary as an object

modelCoeffs <- modelSummary$coefficients  # model coefficients
modelCoeffs
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
beta.estimate
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed

t_value <- beta.estimate/std.error  # calc t statistic

p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value

f_statistic <- linearMod$fstatistic[1]  # fstatistic
f_statistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc

model_p <- pf(f[1], f[2], f[3], lower=FALSE)

p_value

### Standard Error and F-Statistic
#The Akaike information criterion - AIC (Akaike, 1974) and the Bayesian information criterion - BIC (Schwarz, 1978) are measures of the goodness of fit of an estimated statistical model and can also be used for model selection. Both criteria depend on the maximized value of the likelihood function L for the estimated model.
AIC(linearMod)
BIC(linearMod)

## Predicting Linear Models
### Step 1: Create the training (development) and test (validation) data samples from original data.
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

### Step 2: Develop the model on the training data and use it to predict the distance on test data
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
distPred

### Step 3: Review diagnostic measures.
summary (lmMod)

### Step 4: Calculate prediction accuracy and error rates
#A simple correlation between the actuals and predicted values can be used as a form of accuracy measure. A higher correlation accuracy implies that the actuals and predicted values have similar directional movement, i.e. when the actuals values increase the predicteds also increase and vice-versa.
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
correlation_accuracy

#Now lets calculate the Min Max accuracy and MAPE: 
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

## k- Fold Cross validation
install.packages("DAAG")
install.packages("lattice") # required to load DAAG
library(lattice)
library(DAAG)

cvResults <- suppressWarnings(CVlm(data=cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals"))
attr(cvResults, 'ms')

#-------------------------------------------------------------------------------------------------------------------

#Multiple Linear Regression In R - Case Study
#Step 1: Importing data
setwd("D:/Personal/Learning/R/Linear Regression")
mashable_data<-read.csv("OnlineNewsPopularity.csv", stringsAsFactors = FALSE)
head(mashable_data)

#Step 2: Checking if the imported is correct or nor the data 
# checking the dimensions
dim(mashable_data)

# Checking the structure
str(mashable_data)

# Checking the variable names
names(mashable_data)

# Check Sample records
head(mashable_data)
tail(mashable_data)

#Step 3: Analyzing the data. Checking for quality of data
summary(mashable_data)

# Check if you any unusual observations or missing values
## Imputation with mean / median / mode
install.packages("Hmisc") # for data analysis/ missing value imputation
library(Hmisc)
mashable_data$n_tokens_content<-as.numeric(impute(mashable_data$n_tokens_content, mean))  # replace with mean
#impute(mashable_data$n_tokens_content, median)  # median
# or if you want to impute manually
#mashable_data$n_tokens_content[is.na(mashable_data$n_tokens_content)] <- mean(mashable_data$n_tokens_content, na.rm = T)  # not run
summary(mashable_data$n_tokens_content)

## Treating the outliers
## Detect outliers, outside 1.5IQR
outlier_values <- boxplot.stats(mashable_data$n_tokens_content)$out  # outlier values
boxplot(mashable_data$n_tokens_content, main="No of tokens", pars=list(boxwex=0.1))
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
View(outlier_values)

###Once the outliers are identified and you have decided to make amends as per the nature of the problem, you may consider one of the following approaches.
### 1. Imputation
### Imputation with mean / median / mode. This method has been dealt with in detail in the discussion about treating missing values.
### 2. Capping
x <- mashable_data$n_tokens_content
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
caps
H <- 1.5 * IQR(x, na.rm = T)
H
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

# replacing the existing column
mashable_data$n_tokens_content<-x
summary(mashable_data$n_tokens_content)
boxplot(mashable_data$n_tokens_content, main="No of tokens", pars=list(boxwex=0.1))

#Step 4: Checking correlation and multicollinearity
install.packages("yaml")
install.packages("ggplot2")
install.packages("GGally")

# Visualizing correlation
library(yaml)
library(ggplot2)
library(GGally)
ggpairs(data=mashable_data, columns=3:10, title="mashable data")
# creating a matrix of correlation
correlation_matrix<-cor(mashable_data[,-1]) #removal of 1st categorical variable as correlation is applicable for numeric values

#check vif
install.packages("car")
library(car)
# first we will need to run the linear model
car_model<-lm(shares~., data=mashable_data[,-1])
vif(car_model)

#Remove variables that have vif higher than 5 (this range can be from 2-6 depending on the business problem and data)
# removal of variables that have high multicollinearity
names(mashable_data)
car_model<-lm(shares~., data=mashable_data[,-c(1,5)])
vif(car_model)

#Step 5: Creation of train and validation data
## 75% of the sample size
smp_size <- floor(0.75 * nrow(mashable_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(mashable_data)), size = smp_size)

train <- mashable_data[train_ind, ]
test <- mashable_data[-train_ind, ]

#Step 6: Creating the model on the train data
linearMod <- lm(shares ~ ., data=train[,-c(1,5)])  # build linear regression model on full data

# Check the model output
summary(linearMod)

#Step 7: Removal of insignificant variables. Variables with p value less than .05

#removing of the non significant variables 
names(mashable_data)

linearMod <- lm(shares ~ ., data=train[,-c(1,2,4,5,7,10,12,13,14,15,16,19,20,21,22)])  # build linear regression model on full data

# Check the model output
summary(linearMod)

#similarly all insignificant variables need to be removed
#Step 8: Using the created model to make preidctions
shares_pred <- predict(linearMod, test)  # predict shares
shares_pred<-ifelse(shares_pred<0,0,shares_pred)
head(as.data.frame(shares_pred))

#Step 9: Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=train$shares, predicteds=shares_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

### Now lets calculate the Min Max accuracy and MAPE: 
mape <- mean((abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals),na.rm = TRUE)

# printing the output
mape

# Checking residual plots
plot(linearMod)

# treating heteroskadisticity
install.packages("caret")
library(caret)
library(lattice)
library(e1071)

# Box box transformation to correct for Heteroscedasticity 
shares_new<-BoxCoxTrans(train$shares)
train_data_corrected <- cbind(train, shares_corrected=predict(shares_new, train$shares)) # append the transformed 
linearMod <- lm(shares_corrected ~ ., data=train_data_corrected[,-c(1,2,4,5,7,10,12,13,14,15,16,19,20,21,22,23)])  # build linear regression model on full data

# Check the model output
summary(linearMod)
plot(linearMod)
