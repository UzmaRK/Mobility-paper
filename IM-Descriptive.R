## Independent mobility of adolescents and road traffic injuries
##
## Statistical analysis script
##
## Authors:
## Uzma Rahim Khan
## Junaid Razzak
## Martin Gerdin Wärnberg


## Fixing the working director
setwd("C:/Users/uzma.khan/Desktop/R-IM-URK")
## Reading the data from the above mentioned directory
data <- read.csv("IM-Data.csv",header=TRUE)

## Load required packages
install.packages("data.table")
library(data.table)
#data description
#Description of the variable
head(data)
# Data observation and number of variables
dim(data)
# Checking qualitative and quantitative variable
str(data)
# Varaiable names
names(data)
#checking mean  with  categorical variables
tapply(data$Q19_age, data$Q20, mean)
tapply(data$Q19_age,data$Q21,mean)



## TO see the frequesncy table'
table(data$privspublic)
table(data$transport_mode)
table(data$transport_2or3wheelers)
summary(data$Q19_age)
table(data$agerecoded)
table(data$Q20)
table(data$Q21)
#Whom Traveled to school this morning 
#Travelled on my own
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

