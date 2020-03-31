## Statistical analysis script
##Independent mobility of adolescents and road traffic injuries
## Authors:
## Uzma Rahim Khan
## Junaid Razzak
## Martin Gerdin Wärnberg


## Fixing the working director
setwd("C:/Users/uzma.khan/Desktop/R-IM-URK")
## Reading the data from the above mentioned directory
data <- read.csv("IM-data.csv",header=TRUE)

## Load required packages
install.packages("data.table")
library(data.table)
install.packages("tableone")
library(tableone)
install.packages("survival")
library(survival)

#data description
#Description of attribute of the variable
head(data)
# Data observation and number of variable 
dim(data)
# Checking qualitative and quantitative variable
str(data)
# Varaiable names
names(data)
#checking mean with qualitative variables
tapply(data$Q19_age, data$Q20, mean)
tapply(data$Q19_age,data$Q21,mean)



## TO see the frequesncy table'
#Type of School
table(data$privspublic)
# Mode of transport to school
table(data$transport_2or3wheelers)
#Summary of continuous variable of age
summary(data$Q19_age)
#age
table(data$agerecoded)
#gender
table(data$Q20)
#gender
table(data$Q21)
#Whom Traveled to school this morning Travelled on my own
table(data$Q3A)
#parent
table(data$Q3B)
#another adult (more than 18 years)
table(data$Q3C)
#older child/ teenager
table(data$Q3D)
#Child of same age or younger
table(data$Q3E)
# How long to school
table(data$Q4)
table(data$time_school)
#Go back home form school
table(data$Q5)
# Whom Travelled back home today 
#Travelled on my own
table(data$Q7A)
#parent
table(data$Q7B)
#another adult (more than 18 years)
table(data$Q7C)
#older child/ teenager
table(data$Q7D)
#Child of same age or younger
table(data$Q7E)
# Parent trust in Traffic
table(data$Q9)
# Allow to cross main roads
table(data$Q12A_crossroad)
#Allow public buses
table(data$Q14)
# Activity on the weekend
table(data$Q15R)
#Road Traffic Crash
table(data$Q16A)
#Road Traffic Injury
table(data$Q16B)


### Get Variable names
dput(names(data))selection
#Variables 
myVars <- c( "Age","Gender", "Grade", "Type.of.school", "Transport.to.school", 
            "Travelled.on.own.to.school,  "Accompanied.parents.to.school.travel","Time.duration.of.school.travel", 
            "Parents.trust.in.traffic", "Allow.to.cross.main.roads.on.your.own",  "Allow.to.use.public buses", "Road.traffic.injury")
catVar <- c("Grade") 
#Create Table
tab2 <-CreateTableOne(vars = myVars,data = data,factorVars = catVar)
tab2
tab2<-CreateTableOne(vars=myVars,data=data,factorVars = catVars)
tab2

