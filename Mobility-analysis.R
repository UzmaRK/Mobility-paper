## Statistical analysis script
##Independent mobility of adolescents and road traffic injuries
## Authors:
## Uzma Rahim Khan
## Junaid Razzak
## Martin Gerdin Wärnberg


## Fixing the working director
setwd("C:/Users/uzma.khan/Desktop/R-IM-URK")
## Reading the data from the above mentioned directory
data <- read.csv("AIM-data.csv",header=TRUE)

## Load required packages
install.packages("data.table")
library(data.table)
install.packages("tableone")
library(tableone)
install.packages("survival")
library(survival)
install.packages("ISLR")
library(ISLR)

#data description
#Description of attribute of the variable
head(data)
# Data observation and number of variable 
dim(data)
# Checking qualitative and quantitative variable
str(data)
# Varaiable names
names(data)




head(data)
str(data)
### Get Variable names
dput(names(data))
names(data)
c("STUDYID", "FORMNO", "new_ID", "Age", "Gender", "Grade", "School_Type", "privspublic",
  "Transport_School", "Travel_Accompany", "Travel_Accompany_1", "Time_Duration","Transport_Back",
  "Travel_Accompany_2", "time_school", "Transport_Home", "Returned_Travel_Accompany", 
  "Parents_Trust", "Cross_road", "Visit_Relatives", "Worship_Place","Allow_Public_Bus", 
  "Visit_Shop", "Dineout", "Visit_Cinema", "Friends_Dark", "Play_Ground", 
  "Play_Score", "Went_for_Walk", "Went_Concert", "Went_Club", "Library", 
  "Café", "Tuition", "Part_time_work", "Any_Activity", "RTI", "RTI_Consult")
  
#Categories of Age groups of children

data$agecat <- as.numeric(ifelse(data$Age < 13, 1,ifelse(data$Age >14, 3, 2)))

data$agecat <- as.factor(data$agecat)

levels(data$agecat) <- c("9 to 12","13 to 14","15 to 20")
summary(data$agecat)

## Vector of variable to summarize
myVars <- c("agecat", "Gender", "Grade", 
"type_school", "privspublic", "transport_mode", "Travel_Accompany", 
"Travel_Accompany_1", "Travel_Accompany_2", "Time_Duration", 
"Transport_Back", "Returned_Travel_Accompany", "Parents_Trust", 
"Allow_Public_Bus", "Visit_Relatives", "Worship_Place", "Visit_Shop", 
"Dineout", "Visit_Cinema", "Friends_Dark", "Play_Ground", "Play_Score", 
"Went_for_Walk", "Went_Concert", "Went_Club", "Library", "Café", 
"Tuition", "Part_time_work", "Any_Activity", "RTI", "RTI_Consult"
)
catVar <- c("Grade")

## Create a Tableone object
tab2 <-CreateTableOne(vars = myVars,data = data,factorVar = catVar)
tab2

# Logistic regression of age groups with RTI

model1 <- glm( RTI ~ agecat, data = data, family = "binomial")  

model1=glm(RTI ~ (data$agecat),  
           family = "binomial", data=data)
summary(model1)
exp(cbind(OR = coef(model1), confint(model1)))


# Factor Categories of Grade of the students
data$Grade <- as.factor(data$Grade)
levels(data$Grade) <- c("G6","G7","G8","G9","G10")
attach(data)
# Logistic regression of public versus private with RTI
print("Logistic regression of public versus private with RTI")
model2=glm(data$RTI ~ , data$privspublic, family = "binomial")
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

#Factor Categories of Grade
data$Grade <- as.factor(data$Grade)
levels(data$Grade) <- c("G6","G7","G8","G9","G10")

# Logistic regression of Grade with RTI
print("Logistic regression of Grade with RTI")
model3=glm(data$RTI ~ relevel(data$Grade, ref = 1),  
           family = "binomial", data=data)
summary(model3)
exp(cbind(OR = coef(model3), confint(model3)))

# Logistic regression of Type of school with RTI
print("Logistic regression of Type of school with RTI")
model4=glm(data$RTI ~ relevel(data$type_school, ref = 6),  
           family = "binomial", data=data)
summary(model4)
exp(cbind(OR = coef(model4), confint(model4)))

# Logistic regression of Gender with RTI
print("Logistic regression of Gender with RTI")
model5=glm(data$RTI ~ data$Gender,  family = "binomial")
summary(model5)
exp(cbind(OR = coef(model5), confint(model5)))

# Logistic regression of Type of school with RTI
print("Logistic regression of Type of school with RTI")
model6=glm(RTI ~ relevel(Travel_Accompany, ref = 1),  
           family = "binomial", data=data)
summary(model6)
exp(cbind(OR = coef(model6), confint(model6)))

# Logistic regression of Returned_Travel_Accompany with RTI
print("Logistic regression of Returned_Travel_Accompany with RTI")
model7=glm(RTI ~ Returned_Travel_Accompany,  family = "binomial")
summary(model7)
exp(cbind(OR = coef(model7), confint(model7)))

# Logistic regression of Parent Trust with RTI
print("Logistic regression of Parent Trust with RTI")
model8=glm(RTI ~ Parents_Trust,  family = "binomial")
summary(model8)
exp(cbind(OR = coef(model8), confint(model8)))

# Logistic regression of Allow_Public_Bus with RTI
print("Logistic regression of Allow_Public_Bus with RTI")
model9=glm(RTI ~ Allow_Public_Bus,  family = "binomial")
summary(model9)
exp(cbind(OR = coef(model9), confint(model9)))


# Logistic regression of Visit Relative with RTI
print("Logistic regression of Visit Relative with RTI")
model10=glm(RTI ~ Visit_Relatives,  family = "binomial")
summary(model10)
exp(cbind(OR = coef(model10), confint(model10)))


# Logistic regression of Any Activity with RTI
print("Logistic regression of Any Activity with RTI")
model11=glm(RTI ~ Any_Activity,  family = "binomial")
summary(model11)
exp(cbind(OR = coef(model11), confint(model11)))

# Logistic regression of Visit Time_Duration with RTI
print("Logistic regression of Time_Duration with RTI")
model12=glm(RTI ~ data$Time_Duration,  family = "binomial")
summary(model12)
exp(cbind(OR = coef(model12), confint(model12)))

# Logistic regression of Transport_Back with RTI
print("Logistic regression of Transport_Back with RTI")
model13=glm(RTI ~ Transport_Back,  family = "binomial")
summary(model13)
exp(cbind(OR = coef(model13), confint(model13)))

##with second outcome

# Logistic regression of age groups with RTI_Consult
print("Logistic regression of age groups with RTI_Consult")
model1=glm(data$RTI_Consult ~ relevel(agecat, ref = 1),  
           family = "binomial", data=data)
summary(model1)
exp(cbind(OR = coef(model1), confint(model1)))



# Logistic regression of age groups with RTI_Consult
print("Logistic regression of age groups with RTI_Consult")
model1=glm(data$RTI_Consult ~ relevel(data$agecat, ref = 1),  
           family = "binomial", data=data)
summary(model1)
exp(cbind(OR = coef(model1), confint(model1)))


# Factor Categories of Grade of the students
data$Grade <- as.factor(data$Grade)
levels(data$Grade) <- c("G6","G7","G8","G9","G10")
attach(data)
# Logistic regression of public versus private with RTI_Consult
print("Logistic regression of public versus private with RTI_Consult")
model2=glm(data$RTI_Consult ~ , data$privspublic, family = "binomial")
summary(mod1)
exp(cbind(OR = coef(model2), confint(model2)))

#Factor Categories of Grade
data$Grade <- as.factor(data$Grade)
levels(data$Grade) <- c("G6","G7","G8","G9","G10")

# Logistic regression of Grade with RTI_Consult
print("Logistic regression of Grade with RTI_Consult")
model3=glm(data$RTI_Consult ~ relevel(data$Grade, ref = 1),  
           family = "binomial", data=data)
summary(model3)
exp(cbind(OR = coef(model3), confint(model3)))

# Logistic regression of Type of school with RTI_Consult
print("Logistic regression of Type of school with RTI_Consult")
model4=glm(data$RTI_Consult ~ relevel(data$type_school, ref = 6),  
           family = "binomial", data=data)
summary(model4)
exp(cbind(OR = coef(model4), confint(model4)))

# Logistic regression of Gender with RTI_Consult
print("Logistic regression of Gender with RTI_Consult")
model5=glm(data$RTI_Consult ~ data$Gender,  family = "binomial")
summary(model5)
exp(cbind(OR = coef(model5), confint(model5)))

# Logistic regression of Type of school with RTI_Consult
print("Logistic regression of Type of school with RTI_Consult")
model6=glm(RTI_Consult ~ relevel(Travel_Accompany, ref = 1),  
           family = "binomial", data=data)
summary(model5)
exp(cbind(OR = coef(model6), confint(model6)))

# Logistic regression of Returned_Travel_Accompany with RTI_Consult
print("Logistic regression of Returned_Travel_Accompany with RTI_Consult")
model7=glm(RTI_Consult ~ Returned_Travel_Accompany,  family = "binomial")
summary(model7)
exp(cbind(OR = coef(model7), confint(model7)))

# Logistic regression of Parent Trust with RTI_Consult
print("Logistic regression of Parent Trust with RTI_Consult")
model8=glm(RTI_Consult ~ Parents_Trust,  family = "binomial")
summary(model8)
exp(cbind(OR = coef(model8), confint(model8)))

# Logistic regression of Allow_Public_Bus with RTI_Consult
print("Logistic regression of Allow_Public_Bus with RTI_Consult")
model9=glm(RTI_Consult ~ Allow_Public_Bus,  family = "binomial")
summary(model9)
exp(cbind(OR = coef(model9), confint(model9)))


# Logistic regression of Visit Relative with RTI_Consult
print("Logistic regression of Visit Relative with RTI_Consult")
model10=glm(RTI_Consult ~ Visit_Relatives,  family = "binomial")
summary(model10)
exp(cbind(OR = coef(model10), confint(model10)))


# Logistic regression of Any Activity with RTI_Consult
print("Logistic regression of Any Activity with RTI_Consult")
model11=glm(RTI_Consult ~ Any_Activity,  family = "binomial")
summary(model11)
exp(cbind(OR = coef(model11), confint(model11)))

# Logistic regression of Visit Time_Duration with RTI_Consult
print("Logistic regression of Time_Duration with RTI_Consult")
model12=glm(RTI_Consult ~ data$Time_Duration,  family = "binomial")
summary(model12)
exp(cbind(OR = coef(model12), confint(model12)))

# Logistic regression of Transport_Back with RTI_Consult
print("Logistic regression of Transport_Back with RTI_Consult")
model13=glm(RTI_Consult ~ Transport_Back,  family = "binomial")
summary(model13)
exp(cbind(OR = coef(model13), confint(model13)))

mylogit <- glm(formula = RTI ~  Gender, data=
                 data, family=binomial)
summary(mylogit)

lreg.or <-exp(cbind(OR = coef(mylogit), confint(mylogit)))
round(lreg.or, digits=4)

mylogit <- glm(formula = RTI ~  agecat, ref = 1, data=
                 data, family=binomial)
summary(mylogit)

lreg.or <-exp(cbind(OR = coef(mylogit), confint(mylogit)))
round(lreg.or, digits=4)


model3=glm(data$RTI ~ relevel(data$agecat, ref = 1),  
           family = "binomial", data=data)
summary(model3)
exp(cbind(OR = coef(model3), confint(model3)))

