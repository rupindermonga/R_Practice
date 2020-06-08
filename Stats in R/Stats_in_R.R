setwd('D:/Personal/Learning/R/Stats in R')

#Read the file
cc<-read.csv('Credit_Card_Data.csv')

#Getting Column Names
names(cc)

#Data Snapshot
View(cc)
str(cc)
summary(cc)

#Central Tendency - Finding Mean
mean(cc$Number.of.Credit.Cards)

#If we check the mean for a categorical variable:
mean(cc$Marital.Status.of.the.Customer)

#Central Tendency - Finding Median
median(cc$Number.of.Credit.Cards)

#If we try to calculate the median of a categorical variable:
median(cc$Marital.Status.of.the.Customer)
mode(cc$Number.of.Credit.Cards) #tells the mode of data and not mode(central tendency)

#Central Tendency - To find Mode, install a package 'modeest':
install.packages('modeest')
library(modeest)
mlv(cc$Number.of.Credit.Cards) 

#Mode - Categorical variables
mlv(cc$Marital.Status.of.the.Customer)

#Creating a code for mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(cc$Number.of.Credit.Cards)
Mode(cc$Marital.Status.of.the.Customer)

#Summarize in one go for a dataset, and can store in an object too:
scc<-summary(cc)
sncc<-summary(cc$Number.of.Credit.Cards)
scc
sncc
class(sncc)

#To access individual values of these outputs
class(sncc)
names(sncc)

#To fetch the values
sncc['Max.']

#For full data set
class(scc)
names(scc)

#Accessing with numeric indices
scc[1,1]

#Variation - Variance/Standard Deviation
var(cc$Number.of.Credit.Cards)
sd(cc$Number.of.Credit.Cards)
sqrt(var(cc$Number.of.Credit.Cards))

#Variation - Range
range(cc$Number.of.Credit.Cards)

#One can also calculate by working on max. and min.
max(cc$Number.of.Credit.Cards)
min(cc$Number.of.Credit.Cards)

#Variation - Inter quartile range
IQR(cc$Number.of.Credit.Cards)

#Variation - Mean Absolute Deviation
mad(cc$Number.of.Credit.Cards)

#Summarizing categorical variables
tab1<-table(cc$Gender.of.the.Customer)
tab1

#Relative Frequency Table
prop.table(tab1)
round(prop.table(tab1),2)
prop.table(tab1)*100

#Frequency table for two more variables
tab2<-table(cc$Gender.of.the.Customer,cc$Marital.Status.of.the.Customer)
tab3<-xtabs(~Gender.of.the.Customer+Marital.Status.of.the.Customer,data=cc)
tab2
tab3

#Relative Frequency Table for two or more variables:
prop.table(tab3)
prop.table(tab3,1)
prop.table(tab3,2)

#Frequency Distribution
freqd<-table(cc$Number.of.Credit.Cards)
freqd
barplot(freqd)

#cummlative
cfreq <- transform(freqd, cumFreq = cumsum(Freq), relative = prop.table(Freq))
cfreq
plot(cfreq$Var1,cfreq$cumFreq)

#Symmetry - Skewness
skewness(cc$Number.of.Credit.Cards)

#Idea of shape - Box plot and histograms
boxplot(cc$Number.of.Credit.Cards)
hist(cc$Number.of.Credit.Cards)

install.packages('psych')
library(psych)
des<-describe(cc)
des
class(des)

#------------

#Read the file
ed<-read.csv("Education_Data.csv")

#Getting Column names
names(ed)

#Top and Bottom six (default) data points
head(ed)
tail(ed)

#Find out how many students are studying in which class
freq_stu<-table(ed$Class)
freq_stu
barplot(freq_stu)

#Relative frequency - same as probability
prop.table(freq_stu)*100

#Counting number of students in class 3
ab<-table(ed$Class)
ab[names(ab)==3]

#Not in class 3
nrow(ed)-ab[names(ab)==3]
sum(ab[names(ab)!=3])

#Probability of being not in class 3
prob_x0<-1-ab[names(ab)==3]/nrow(ed)

#Probability of x=0
prob_x0*prob_x0

#for x=1
prob_x1<-ab[names(ab)==3]/nrow(ed)

#Probability of x=1
prob_x0*prob_x1*2

#for x=2
prob_x2<-ab[names(ab)==3]/nrow(ed)

#Probability of x=2
prob_x2*prob_x2

#Finding probability of pass (>=40)
pass_count<-length(which(ed$Marks.Scored>=40))
pass_prob<-pass_count/nrow(ed)
pass_prob

#Applying binomial distribution
dbinom(7,size=10, pass_prob) #Exactly 7 students pass the exam
pbinom(7,size = 10,pass_prob) #Less than equal to 7 students pass the exam

#Identifying Normal distribution
hist(ed$Marks.Scored) #Almost a normal distribution

#Finding mean and standard deviation
mean_marks<-mean(ed$Marks.Scored)
sd_marks<-sd(ed$Marks.Scored)
mean_marks
sd_marks

#Applying Normal Distribution
pnorm(84,mean=mean_marks,sd=sd_marks,lower.tail = FALSE) #we need upper tail only, Greater than 84

#Applying Normal Distribution (Between 40 and 84)
p_84<-pnorm(84,mean=mean_marks,sd=sd_marks) #Less than 84
p_40<-pnorm(40,mean=mean_marks,sd=sd_marks) #Less than 40
p_84-p_40 #between 40 and 84


#Hypothesis Testing
mean_income<-20000 #Given
sd_income<-4000 #Given
sample_students<-100 #Given
decision_point<-19000 #Given

se_income<-sd_income/sqrt(sample_students)
se_income

#Multiple ways to find out whether to reject Null hypothesis or not
#1st way is to determine critical value of x for 95% Confidence interval
xcritical<-qnorm(0.95,decision_point,se_income) #Population mean is 19000 which we want to check and not 20000
xcritical

#2nd way is to compare z critical value with z computed
z_sample<-(mean_income-decision_point)/se_income
z_sample

#3rd way or the direct way is to calculate p-value
p_value<-pnorm(mean_income,mean = decision_point,sd=se_income,lower.tail = FALSE)
p_value

#Two tail test, significance level is not 5% but 2.5%
#Let's find two critical values
xcritical_lower<-qnorm(0.025, decision_point,se_income)
xcritical_upper<-qnorm(0.975, decision_point,se_income)
xcritical_lower
xcritical_upper


