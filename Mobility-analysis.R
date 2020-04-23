## Statistical analysis script
##Independent mobility of adolescents and road traffic injuries
## Authors:
## Uzma Rahim Khan
## Junaid Razzak
## Martin Gerdin Wärnberg


## Fixing the working director
setwd("C:/Users/uzma.khan/Desktop/R-IM-URK")

## Load required packages
install.packages("data.table")
library(data.table)
install.packages("tableone")
library(tableone)
install.packages("survival")
library(survival)
install.packages("ISLR")
library(ISLR)
install.packages("labelled")
library(labelled)

## Reading the data from the above mentioned directory
data <- data.table::fread("mobility-data.csv", data.table = FALSE)

## Data description
## Description of attribute of the variable
head(data)
## Data observation and number of variable 
dim(data)
## Checking qualitative and quantitative variable, need to rename the
## Cafe variable because the accent is hard for R to work with
names(data)[names(data) == "Caf\xe9"] <- "Cafe"
str(data)

## Get Variable names
dput(names(data))

## Categories of Age groups of children
data$agecat <- as.factor(ifelse(data$Age < 13, 1, ifelse(data$Age > 14, 3, 2)))
levels(data$agecat) <- c("9 to 12","13 to 14","15 to 20")
summary(data$agecat)

## Vector of variable to summarize
myVars <- c("agecat", "Age", "Gender", "Grade", "TYPEOFSC", "shift_school", 
            "School.transport", "Transport_2or3wheelers", "Travel_Accompany", 
            "Travel.accompany1", "Travel.accompany2", "TimeSchool", "time_school", 
            "SchoolHomeTransport", "home_schooltransportusual", "Returned_Travel_Accompany", 
            "Parents_Trust", "Cross_road", "Allow_Public_Bus", "RTI", "RTI_Consult")
catVar <- c("Grade")

## You need to deal with missing data somewhere here

## Create Descriptive table
tab1 <-tableone::CreateTableOne(vars = myVars,data = data,factorVar = catVar)
tab1

## Create Cross tabulation by taking outcome RTI
tab2 <-tableone::CreateTableOne(vars = myVars, strata = "RTI", data = data, factorVars = catVar)
tab2

## Univariate Logistic regression of age groups with RTI
data$RTI <- as.numeric(data$RTI)
model1 <- glm(RTI ~ agecat,  
             family = "binomial", data = data)
lreg.or <-exp(cbind(OR = coef(model1), confint(model1)))
round(lreg.or, digits=2)
summary(model1)

# Logistic regression of public versus private school with RTI
model2 <- glm(RTI ~ TYPEOFSC, family = "binomial", data = data)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

# Logistic regression of Gender with RTI

model5 <- glm(RTI ~ Gender,  family = "binomial", data = data)
summary(model5)
exp(cbind(OR = coef(model5), confint(model5)))

# Logistic regression of Accompany to school with RTI

model6 <- glm(RTI ~ relevel(Travel_Accompany, ref = 1),  
              family = "binomial", data = data)
summary(model6)
exp(cbind(OR = coef(model6), confint(model6)))

# Logistic regression of Returned_Travel_Accompany with RTI

model7=glm(data$RTI ~ data$Returned_Travel_Accompany,  family = "binomial")
summary(model7)
exp(cbind(OR = coef(model7), confint(model7)))

# Logistic regression of Parent Trust with RTI

model8=glm(data$RTI ~ data$Parents_Trust,  family = "binomial")
summary(model8)
exp(cbind(OR = coef(model8), confint(model8)))

# Logistic regression of Allow_Public_Bus with RTI

model9=glm(data$RTI ~ data$Allow_Public_Bus,  family = "binomial")
summary(model9)
exp(cbind(OR = coef(model9), confint(model9)))


# Logistic regression of CRoss main road with RTI

model10=glm(data$RTI ~ data$Cross_road,  family = "binomial")
summary(model10)
exp(cbind(OR = coef(model10), confint(model10)))






