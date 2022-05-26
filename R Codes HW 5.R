#Homework 5 Case Study Question 1

#Load the packages into R
library(ggpubr) #helps us to create results that are publication ready for your report
library(tidyverse) #for data manipulation and visualization
library(broom) #tidy model output
library("readxl") #Will help us to read .xlsx files
library(gclus) #Allows us to have our scatterplot with correlation embedded in it
theme_set(theme_pubr())

#part a####
#Import data into RStudio
bank_df <- read_excel("Documents/BUS2_194A_Statistical_Analysis/Chapter_15/HW5_Bank.xlsx")
head(bank_df) #Gives us a snapshot of the excel file 

plot(bank_df$Balance, bank_df$Direct, main = "Scatter plot", xlab = "average monthly checking account balance ",
     ylab = "whether the customer contacted signed up for payroll direct deposit", pch = 19) 

ggplot(bank_df, aes(Balance, Direct)) + 
  geom_point() + 
  stat_smooth(method = glm, se = FALSE,method.args = list(family=binomial), col = "green")
#interpretation of the scatter plot: values of E(y) range from 0 to 1 and that the curve is S-shaped makes 
#our equation ideally suited to model the probability the dependent variable is equal to 1. Also note that the 
#value of E(y) gradually approaches 1 as the value of x becomes larger 

###part b and c####
#build logistic regression equation 
bank_logit <- glm(Direct ~ Balance, data = bank_df, family = "binomial")
summary(bank_logit)
#interpretation: based on summary statistics, our equation is y hat = (e^-2.633+0.22x)/(1 + e^-2.633+0.22x)
#the intercept (Bo) is -2.633 and the slope (B1) is 0.22. 

##part d####
#y hat = (e^-2.633+0.22x)/(1 + e^-2.633+0.22x)
#y hat = (e^-2.633+0.22*12)/(1 + e^-2.633+0.22*12) = 0.502
exp(-2.633+0.22*12)/(1 + exp(-2.633+0.22*12))

newdatabank_df<-read_excel("Documents/BUS2_194A_Statistical_Analysis/Chapter_15/newdatabank.xlsx")
newdatabank_df
prediction <- predict(bank_logit, newdata = newdatabank_df, type = "response") 
prediction
#the probability that customers with an average monthly balance of $1200 will sign up for direct payroll 
#deposit is 50.2% 

#part e####
prediction <- predict(bank_logit, data = bank_df, type = "response") 
prediction
#the average monthly balance should be $1200 or more to achieve 0.5 or higher probability of signing up for 
#direct payroll deposit 

#part f####
exp(cbind(OR = coef(bank_logit), confint(bank_logit, level = 0.95))) #odds ratio 
#interpretation: the estimated odds ratio for Balance is 1.25, which means that the estimated odds of signing up for 
#direct payroll deposit is 1.25 times greater for any one-unit change in Balance variable. Furthermore, the range of 
#confidence interval does not contain the value of 1 which indicates that Balance has a significant relationship with 
#the estimated odds ratio. 

#however, for any increase of more than one unit for Balance variable, we would need to investigate the relationship between 
#the odds ratio and the regression coefficients. 
exp(5*0.22)
#for example, this output tells us that the estimated odds of signing up for direct payroll deposit is 3 times greater for 
#any 5-unit change in Balance variable, i.e. the estimated odds ratio for an increase of $500 in account balance is 3. 

