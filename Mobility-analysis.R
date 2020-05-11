## Statistical analysis script
##Independent mobility of adolescents and road traffic injuries
## Authors:
## Uzma Rahim Khan
## Junaid Razzak
## Martin Gerdin Wärnberg


## Fixing the working director
setwd("C:/Users/uzma.khan/Desktop/R-IM-URK")     

## Load required packages
##install.packages("data.table")
library(data.table)
##install.packages("tableone")
library(tableone)
##install.packages("survival")
library(survival)
##install.packages("ISLR")
library(ISLR)
install.packages("labelled")
library(labelled)
##install.packages("knitr")
library(knitr)
##install.packages("rmarkdown")
library(rmarkdown)

## Reading the data from the above mentioned directory
data <- read.csv("Mobility-data.csv", header = TRUE)

## Data description
## Description of attribute of the variable
head(data)

## Data observation and number of variable 
dim(data)

## Checking qualitative and quantitative variable
names(data)

##need to rename the Cafe variable because the accent is hard for R to work with
names(data)[names(data) == "Caf\xe9"] <- "Cafe"

## Rename variables
names(data) <- tolower(names(data))
names(data) <- gsub("-", "_", names(data))
names(data) <- gsub(".", "_", names(data), fixed = TRUE)
names(data)[names(data) == "typeofsc"] <- "type_school"
names(data)[names(data) == "schoolhometransport"] <- "school_home_transport"
names(data)[names(data) == "home_schooltransportusual"] <- "home_school_transport_usual"
names(data)[names(data) == "Travel.accompany2"] <- "schooltravel_accompany"
names(data)[names(data) == "Parents_Trust"] <- "parents_trust"
names(data)[names(data) == "Allow_Public_Bus"] <- "allow_public_bus"
names(data)[names(data) == "RTI_Consult"] <- "rti_consult"

## Categories of Age groups of children
data$age_cat <- as.factor(ifelse(data$age < 13, 1, ifelse(data$age > 14, 3, 2)))
levels(data$age_cat) <- c("9 to 12","13 to 14","15 to 20")
summary(data$age_cat)

# replace missing in variable  rti_consult with NA
data$rti_consult[data$rti_consult == "missing"] <- NA

#remove obs missing in rti_consult
data <- data[!is.na(data$rti_consult),]

## Give rti and rti_consult new levels
data$rti_consult <- as.character(data$rti_consult)

data$rti[data$rti_consult == "0"] <- "No road traffic injury"
data$rti[data$rti_consult == "1"] <- "Road traffic injury"

data$rti <- as.factor(data$rti_consult)
table(data$rti_consult)

#Labelling the variables
label.list <-list(age_cat ="Age groups", gender="Gender",grade="Grade",
                  type_school="Type of school", shift_school="Shif of school",school_transport="School transport",
                  transport_2or3wheelers ="Transport 2 or 3 wheelers", travel_accompany ="Travel accompany",
                  travel_accompany1="Travel accompany 1", travel_accompany2="Travel accompany 1", time_school="Time of school",
                  school_home_transport="School home transport",
                  returned_travel_accompany="Returned travel accompany", parents_trust="Parents trust",
                  cross_road="Cross road", allow_public_bus="Allow public bus", visit_friend_home="Visit friend home",
                  visit_relatives="Visit to relatives", worship_place="Visit to worship place", visit_shop="Visit to shop",
                  dineout="Dineout", visit_cinema="Visit cinema", friends_dark="Friends dark", play_ground="Going to play ground",
                  play_score="Paly score", went_for_walk="Went for walk", went_concert="Went to concert",
                  went_club="Went to club", library=" Went to library", cafe="Cafe",
                  tuition="Tuition", part_time_work="Part time work", any_activity="Any activity",
                  activity_alone="Activity alone", activity_independent="Activity independent", weekend="Weekend",
                  rti="Road traffic incident", rti_consult="Road traffic injury consulted")
### Keep only relevant variables in the data
data <-  data[, names(label.list)]
labelled::var_label(data) <- label.list
table1 <- tableone::CreateTableOne(data = data)
pretty.table1 <- tableone:::print.TableOne(table1, varLabels = TRUE, showAllLevels = TRUE)
colnames(pretty.table1)[colnames(pretty.table1) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table1)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 1.docx", output_format = "word_document")
file.remove(tmp.file)

## Cross tab 2
table2 <- tableone::CreateTableOne(strata = "rti", data = data)
pretty.table2 <- tableone:::print.TableOne(table2, varLabels = TRUE, showAllLevels = TRUE)
colnames(pretty.table2)[colnames(pretty.table2) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table2)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 2.docx", output_format = "word_document")
file.remove(tmp.file)

## Univariate Logistic regression of age groups with RTI

model1 <- glm(rti_consult  ~ age_cat, family = "binomial", data = data)
summary(model1)

round(lreg.or, digits=2)

# Logistic regression of public versus private school with RTI
model2 <- glm(rti_consult ~ type_school, family = "binomial", data = data)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))
round(lreg.or, digits=2)


# Logistic regression of Gender with RTI
model3 <- glm(rti_consult ~ gender,  family = "binomial", data = data)
summary(model3)
exp(cbind(OR = coef(model3), confint(model3)))
round(lreg.or, digits=2)

# Logistic regression of Accompany to school with RTI
model4 <- glm(rti_consult ~ relevel(as.factor(travel_accompany), ref =1),  
              family = "binomial", data = data)
summary(model4)
exp(cbind(OR = coef(model4), confint(model4)))
round(lreg.or, digits=2)

# Logistic regression of Returned_Travel_Accompany with RTI
model5=glm(rti_consult ~ returned_travel_accompany,  family = "binomial", data = data)
summary(model5)
exp(cbind(OR = coef(model5), confint(model5)))
round(lreg.or, digits=2)

# Logistic regression of Parent Trust with RTI

model6=glm(data$rti_consult ~ data$parents_trust,  family = "binomial")
summary(model6)
exp(cbind(OR = coef(model6), confint(model6)))
round(lreg.or, digits=2)

# Logistic regression of Allow_Public_Bus with RTI

model7=glm(data$rti_consult ~ data$allow_public_bus,  family = "binomial")
summary(model7)
exp(cbind(OR = coef(model7), confint(model7)))
round(lreg.or, digits=2)


# Logistic regression of CRoss main road with RTI
model8=glm(data$rti_consult ~ data$cross_road,  family = "binomial")
summary(model8)
exp(cbind(OR = coef(model8), confint(model8)))
round(lreg.or, digits=2)

#Multivariate Logistic reg

model9 <- glm(rti_consult ~ age_cat+gender+grade + type_school+shift_school+school_transport+travel_accompany+
              time_school+school_home_transport+returned_travel_accompany+parents_trust+cross_road+
              allow_public_bus+visit_friend_home+visit_relatives+worship_place+visit_shop+dineout+
              visit_cinema+friends_dark+play_ground+play_score+went_for_walk+went_concert+went_club+
              library+Cafe+tuition+part_time_work+any_activity,
              family = "binomial", data = data)
exp(cbind(OR = coef(model9), confint(model9)))
summary(model9)
round(lreg.or, digits=2)



model10 <- glm(rti_consult ~ age_cat+gender + type_school+shift_school+transport_2or3wheelers+travel_accompany+
                time_school++parents_trust+cross_road+
                allow_public_bus+any_activity,
              family = "binomial", data = data)
summary(model10)
exp(cbind(OR = coef(model10), confint(model10)))
round(lreg.or, digits=2)


model11 <- glm(rti_consult ~ age_cat+gender+cross_road+
               +any_activity,
              family = "binomial", data = data)
round(lreg.or, digits=2)
summary(model11)

exp(cbind(OR = coef(model11), confint(model11)))

lreg.or <-exp(cbind(OR = coef(model11), confint(model11)))
