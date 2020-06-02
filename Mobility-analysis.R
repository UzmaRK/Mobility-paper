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
##install.packages("labelled")
library(labelled)
##install.packages("knitr")
library(knitr)
##install.packages("rmarkdown")
library(rmarkdown)

## Reading the data from the above mentioned directory
data <- data.table::fread("Mobility-data.csv", data.table = FALSE)

## Rename cafe variable
names(data)[names(data) == "Caf\xe9"] <- "Cafe"

## Data description
## Description of attribute of the variable
str(data)

## Rename variables
names(data) <- tolower(names(data))
names(data) <- gsub("-", "_", names(data))
names(data) <- gsub(".", "_", names(data), fixed = TRUE)
names(data) <- gsub(" ", "_", names(data), fixed = TRUE)
names(data)[names(data) == "typeofsc"] <- "type_school"
names(data)[names(data) == "transport_2or3wheelers"] <- "home_school_transport"
names(data)[names(data) == "travel_accompany2"] <- "home_school_accompany"
names(data)[names(data) == "timeschool"] <- "time_to_school"
names(data)[names(data) == "schoolhometransport"] <- "school_home_transport"
names(data)[names(data) == "returned_travel_accompany"] <- "returned_travel_accompany"
names(data)[names(data) == "parents_trust"] <- "parents_trust"
names(data)[names(data) == "allow_public_bus"] <- "allow_public_bus"
names(data)[names(data) == "any_activity"] <- "any_activity"
names(data)[names(data) == "rti_consult"] <- "rti_consult"

## Frequency of variables
table(data$age)
table(data$gender)
table(data$grade)
table(data$type_school)
table(data$home_school_transport)
table(data$home_school_accompany)
table(data$time_to_school)
table(data$school_home_transport)
table(data$returned_travel_accompany)
table(data$parents_trust)
table(data$cross_road)
table(data$allow_public_bus)
table(data$any_activity)
table(data$rti_consult)

## Replace missing in variables with NA
data$time_to_school[data$time_to_school == "missing"] <- NA
table(data$time_to_school, useNA = "always")
data$allow_public_bus[data$allow_public_bus == "missing"] <- NA
table(data$allow_public_bus, useNA = "always")
data$rti_consult[data$rti_consult == "missing"] <- NA
table(data$rti_consult, useNA = "always")

## Remove obs missing/NA in variables
data <- data[!is.na(data$time_to_school),]
table(data$time_to_school, useNA = "always")
data <- data[!is.na(data$allow_public_bus),]
table(data$allow_public_bus, useNA = "always")
data <- data[!is.na(data$rti_consult),]
table(data$rti_consult, useNA = "always")

## Categrizing age
data$age_cat <- as.factor(ifelse(data$age < 13, 1, ifelse(data$age > 14, 3, 2)))
levels(data$age_cat) <- c("9 to 12","13 to 14","15 to 20")
summary(data$age_cat)
## Replace blank in variable  time_to_school with NA
#data$time_to_school[data$time_to_school == ""] <- NA
#table(data$time_to_school)

## Give  new levels to some variables
data$rti_consult <- as.character(data$rti_consult)
data$rti_consult[data$rti_consult == "0"] <- "No road traffic injury"
data$rti_consult[data$rti_consult == "1"] <- "Road traffic injury"
data$rti_consult <- as.factor(data$rti_consult)
data$home_school_accompany[data$home_school_accompany == "Merged Parent and Adult"] <- "Either with parent or any other adult"
data$home_school_accompany[data$home_school_accompany == "Alone/Same Age"] <- "Alone or with someone of same age"
data$home_school_accompany[data$home_school_accompany == "Mix"] <- "Mix travel pattern; alone or with parents"
data$any_activity[data$any_activity == "Not Done"] <- "No activity on the weekend"
data$any_activity[data$any_activity == "Mix"] <- "Activities either with parents or alone"
data$any_activity[data$any_activity == "On Your own or with other young person"] <- "On own or with other young person"

# Collapse levels
data$returned_travel_accompany[data$returned_travel_accompany == "Parent"] <- "Parent or adult"
data$returned_travel_accompany[data$returned_travel_accompany == "Another adult"] <- "Parent or adult"
data$returned_travel_accompany[data$returned_travel_accompany == "Travelling home alone"] <- "On own or with other child"
data$returned_travel_accompany[data$returned_travel_accompany == "Older child / teenager"] <- "On own or with other child"
data$returned_travel_accompany[data$returned_travel_accompany == "Child of same age or younger"] <- "On own or with other child"
data$returned_travel_accompany[data$returned_travel_accompany == "Mix"] <- "Mix travel pattern; alone or with parents"
table(data$school_home_transport)
data$school_home_transport<- as.character(data$school_home_transport)
data$school_home_transport[data$school_home_transport == "Cycle"] <- "Two or Three Wheelers"
data$school_home_transport[data$school_home_transport == "Rickshaw"] <- "Two or Three Wheelers"
data$school_home_transport[data$school_home_transport == "Motorbike"] <- "Two or Three Wheelers"
data$school_home_transport[data$school_home_transport== "Car"] <- "Four Wheelers"
data$school_home_transport[data$school_home_transport== "Public Bus"] <- "Four Wheelers"
data$school_home_transport[data$school_home_transport== "School Bus"] <- "Four Wheelers"
data$school_home_transport[data$school_home_transport== "School Van"] <- "Four Wheelers"
data$school_home_transport[data$school_home_transport== "Suzuki"] <- "Four Wheelers"
data$school_home_transport[data$school_home_transport == "Walking"] <- "Walking"
table(data$school_home_transport)

## Categorical variables in the data
catVars <- c("age_cat", 
             "gender", 
             "grade", 
             "type_school", 
             "home_school_transport",
             "home_school_accompany", 
             "time_to_school", 
             "school_home_transport", 
             "returned_travel_accompany",
             "parents_trust", 
             "cross_road", 
             "allow_public_bus", 
             "any_activity", 
             "rti_consult")
data[catVars] <- lapply(data[catVars], as.factor)

## Create a label list
label.list <-list(age_cat = "Age groups",
                  gender = "Gender",
                  grade = "Grade",
                  type_school = "Type of School",
                  home_school_transport = "Mode of transport to school",
                  home_school_accompany = "School travel was alone or accompanied",
                  time_to_school = "Time to reach school",
                  school_home_transport = "Mode of transport on way back to home from school",
                  returned_travel_accompany = "Home travel from school was accompanied with",
                  parents_trust = "Parents trust on child when in traffic alone",
                  cross_road = " Child allowed to cross main roads",
                  allow_public_bus = "Child allowed to go on public bus",
                  any_activity  = "Child activity over the weekend",
                  rti_consult = "Road traffic injury that required medical consultation")

## Keep only relevant variables in the data
data <- data[, names(label.list)]

## Label data
labelled::var_label(data) <- label.list

## Create Table 1
table1 <- tableone::CreateTableOne(data = data)
pretty.table1 <- tableone:::print.TableOne(table1, varLabels = TRUE, showAllLevels = TRUE, test = FALSE)
colnames(pretty.table1)[colnames(pretty.table1) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table1)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 1.docx", output_format = "word_document")
file.remove(tmp.file)

## Create Table 2
table2 <- tableone::CreateTableOne(strata = "rti_consult", data = data, factorVar = catVars)
pretty.table2 <- tableone:::print.TableOne(table2, varLabels = TRUE, showAllLevels = TRUE, test = FALSE)
colnames(pretty.table2)[colnames(pretty.table2) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table2)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 2.docx", output_format = "word_document")
file.remove(tmp.file)

## Univariate Logistic regression of age groups with RTI
table(data$age_cat)
data$age <- relevel(data$age_cat, ref = "15 to 20")
labelled::var_label(data$age_cat) <- "Age"
model1 <- glm(rti_consult ~ age_cat, family = "binomial", data = data)

## Create function that returns a pretty regression table
pretty_regression_table <- function(model.object){
  cis <- confint(model.object)
  or.ci <- round(exp(cbind(OR = coef(model.object), cis)), digits = 2)
  reg.table.names <- c("Parameter", "OR", "95% CI")
  reg.table <- as.data.frame(cbind(rownames(or.ci),
                                   or.ci[, "OR"],
                                   paste0(or.ci[, "2.5 %"], ", ", or.ci[, "97.5 %"])),
                             stringsAsFactors = FALSE)
  reg.table <- reg.table[-grep("(Intercept)", rownames(reg.table)), ]
  colnames(reg.table) <- reg.table.names
  ivar.name <- attr(model.object$terms, "term.labels")
  ivar <- as.character(model.object$model[, ivar.name])
  reg.table$Parameter <- gsub(ivar.name, "", reg.table$Parameter)
  reference <- unique(ivar)[!(unique(ivar) %in% reg.table$Parameter)]
  reference.row <- c(paste0("Reference: ", reference), "1", "", "")
  variable.label <- attr(model.object$model[, ivar.name], "label")
  if (is.null(variable.label))
    variable.label <- ivar.name
  variable.row <- c(variable.label, "", "", "")
  pretty.table <- rbind(variable.row, reference.row, reg.table)
  rownames(pretty.table) <- NULL
  prettier.table <- knitr::kable(pretty.table)
  return (prettier.table)
}
## Create a pretty regression table for model1
pretty_regression_table(model1)

## Logistic regression of  type_school = "Type of school",
labelled::var_label(data$type_school) <- "Type of School"
model2 <- glm(rti_consult ~ type_school, family = "binomial", data = data)
pretty_regression_table(model2)

## Logistic regression of Gender with RTI
labelled::var_label(data$gender) <- "Gender"
model3 <- glm(rti_consult ~ gender,  family = "binomial", data = data)
pretty_regression_table(model3)

## Logistic regression of Accompany to school with RTI
data$home_school_accompany <- as.factor(data$home_school_accompany)
data$home_school_accompany <- relevel(data$home_school_accompany, ref = "Alone or with someone of same age")
labelled::var_label(data$home_school_accompany) <- "School travel was alone or accompanied"
model4 <- glm(rti_consult ~ home_school_accompany, family = "binomial", data = data)
pretty_regression_table(model4)

# Logistic regression of Returned_Travel_Accompany with RTI
data$returned_travel_accompany <- relevel(data$returned_travel_accompany, ref = "On own or with other child")
labelled::var_label(data$returned_travel_accompany) <- "Home travel from school was accompanied with"
model5 <- glm(rti_consult ~ returned_travel_accompany,  family = "binomial", data = data)
pretty_regression_table(model5)

# Logistic regression of parents_trust = "Parents trust on child when in traffic alone" with RTI
labelled::var_label(data$parent_trust) <- "Parents trust on child when in traffic alone"
model6 <- glm(rti_consult ~ parents_trust,  family = "binomial", data = data)
pretty_regression_table(model6)

# Logistic regression of allow_public_bus = "Child allowed to go on public bus" with RTI
labelled::var_label(data$allow_public_bus) <- "Child allowed to go on public bus"
model7 <- glm(rti_consult ~ allow_public_bus,  family = "binomial", data = data)
pretty_regression_table(model7)

# Logistic regression of cross_road = " Child allowed to cross main roads", with RTI
labelled::var_label(data$cross_road) <- "Child allowed to cross main roads"
model8 <- glm(rti_consult ~ cross_road,  family = "binomial", data = data)
pretty_regression_table(model8)

# Logistic regression of home_school_transport = "Mode of transport to school" with RTI
data$home_school_transport <- relevel(data$home_school_transport, ref = "Walking")
labelled::var_label(data$home_school_transport) <- "Mode of transport to school"
model9 <- glm(rti_consult ~ home_school_transport,  family = "binomial", data = data)
pretty_regression_table(model9)

# Logistic regression of   school_home_transport = "Mode of transport on way back to home from school" with RTI
data$school_home_transport <- relevel(data$school_home_transport, ref = "Walking")
labelled::var_label(data$school_home_transport) <- "Mode of transport on way back to home from school"
model10 <- glm(rti_consult ~ school_home_transport,  family = "binomial", data = data)
pretty_regression_table(model10)

# Logistic regression of timeschool = "Time to reach school" with RTI
table(data$time_to_school)
data$time_to_school <- relevel(data$time_to_school, ref = "< 5 mins")
labelled::var_label(data$time_to_school) <- "Time to reach school"
model11 <- glm(rti_consult ~ time_to_school,  family = "binomial", data = data)
pretty_regression_table(model11)

# Logistic regression of any_activity  = "Child activity over the weekend" with RTI
data$any_activity <- relevel(data$any_activity, ref = "No activity on the weekend")
labelled::var_label(data$any_activity) <- "Child activity over the weekend"
model12 <- glm(rti_consult ~ any_activity,  family = "binomial", data = data)
pretty_regression_table(model12)

#Multivariate Logistic reg
model13 <- glm(rti_consult ~ age + gender + type_school + time_to_school + school_home_transport +
                   returned_travel_accompany + parents_trust + cross_road + allow_public_bus +
                   any_activity,
               family = "binomial", data = data)
summary(model13)
## pretty_regression_table does not work for multivariable models yet
## pretty_regression_table(model13)




