
setwd("C:/Users/Seminar/Desktop/R work project 2")

data <- read.csv("Child-Data.csv",header=TRUE)
attach(data)

names(data)

sink("Results.txt")
print("Number of variables in dataset")
ncol(data)

print("Number of observations in dataset")
nrow(data)

print("Summary statistics for hight of child")
summary(Q22A)
sd(Q22A)

print("Summary statistics for Weight of child")
summary(Q22B)
sd(Q22B)

print("Summary statistics for age of child")
summary(Q19_age)
sd(Q19_age, na.rm = T)

print("Summary statistics for BMI of child")
summary(BMI)
sd(BMI, na.rm = T)

print("Frequncy distribution for Age groups")
df1 <- as.data.frame(table(agerecoded, useNA = "ifany"))
colnames(df1) <- c("Variable","Freq")
df1$Percent = round(100*df1$Freq/sum(df1$Freq),1)
df1

print("Frequncy distribution for gender")
df2 <- as.data.frame(table(Q20, useNA = "ifany"))
colnames(df2) <- c("Variable","Freq")
df2$Percent = round(100*df2$Freq/sum(df2$Freq),1)
df2

print("Frequncy distribution for Class of the students")
df3 <- as.data.frame(table(Q21, useNA = "ifany"))
colnames(df3) <- c("Variable","Freq")
df3$Percent = round(100*df3$Freq/sum(df3$Freq),1)
df3

print("# QUESTION-15: Which of these activities did you do the weekend just passed (on Saturday or Sunday")
Act <- c('Visited a friends home',
         'Visited relatives or grown-ups',
         'Visited a place of worship',
         'Went to the shops',
         'Went to dine out',
         'Went to a cinema',
         'Spent time with friends outside after dark',
         'Went to a playground, park or playing fields',
         'Played sport or went swimming',
         'Went for a walk or cycled around',
         'Went to a concert',
         'Went to a youth club',
         'Went to a library',
         'Went to cyber/net cafe',
         'Went for tuition class',
         'Went for my part time work')

for (i in 105:120) {
  j=i-104
  print(paste("Activity = ",Act[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}



print("# QUESTION-03: Who did you travel to school with this morning")
Act2 <- c('Travelled on my own',
          'independ_mobility',
          'Accompany',
          'Parent',
          'Another adult (more than 18 years)',
          'Older child / teenager',
          'Child of same age or younger')

for (i in 46:52) {
  j=i-45
  print(paste(Act2[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}


print("# QUESTION-04: How long did it take you to travel to school this mornings?")
Act3 <- c('How long did it take you to travel to school this morning?',
          'time_school')

for (i in 53:54) {
  j=i-52
  print(paste(Act3[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}

print("# QUESTION-05: How will you go back to home from school today?")
Act4 <- c('How will you go back home from school today?',
          'If other. Please write in')

for (i in 55:56) {
  j=i-54
  print(paste(Act4[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}


print("# QUESTION-06: Is this your usual way to to get to home from school?")
Act5 <- c('Is this your usual way to go back home from school?',
          'If no, then how do you go to school usually?')

for (i in 57:58) {
  j=i-56
  print(paste(Act5[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}


print("# QUESTION-07: Who will you travel home with today?")
Act6 <- c('Travelled on my own',
          'Walk_day',
          'Parent',
          'Another adult (more than 18 years)',
          'Older child / teenager',
          'IM_SH',
          'Child of same age or younger')

for (i in 62:68) {
  j=i-56
  print(paste(Act6[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}


print("# QUESTION-08: How would you like to be able to travel to and from school?")
Act7 <- c('How would you like to be able to travel to and from school?',
          'like_transport mode',
          'like to travel',
          'If other. Please write in')

for (i in 69:72) {
  j=i-68
  print(paste(Act7[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}


print("# QUESTION-09: Do you think your parents trust you when you are by yourself in traffic?")
Act8 <- c('Do you think your parents trust you when you are by yourself in traffic?')

for (i in 73:73) {
  j=i-72
  print(paste(Act8[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}

print("# QUESTION-10: What do your parents think is a appropriate for a child of your age to do on your own?")
Act9 <- c('Travel by public bus in day time',
          'Travel by bus in the evening',
          'Cycle in day time',
          'Cycle in the evening',
          'Cycle_evening',
          'Walk around in day time',
          'Walk around in the evening',
          'None')

for (i in 74:81) {
  j=i-73
  print(paste(Act9[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}

print("# QUESTION-11: What do you think is appropriate for a child of your age to do on his/her own??")
Act10 <- c('Travel by public bus in day time',
           'Travel by bus in the evening',
           'Cycle in day time',
           'Cycle in the evening',
           'Walk around in day time',
           'Walk around in the evening',
           'None')

for (i in 83:89) {
  j=i-82
  print(paste(Act10[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}


print("# QUESTION-12: QUESTIONS#12(a),12(b),12(c),and categories")
Act11 <- c('Are you allowed to cross main roads on your own?',
           'If you dont cross main roads on your own, would you like to be allowed to',
           'How old were you when you first crossed main roads on your own?',
           'crossage_q12c_cat',
           'Never crossed the road on my own')

for (i in 90:94) {
  j=i-89
  print(paste(Act11[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}

print("# QUESTION-13: QUESTIONS#13(a),13(b),13(c),and Q13(d")
Act12 <- c('Do you have a bicycle?',
           'Are you allowed to cycle on main roads by your parents?',
           'If Yes, At what age were you first allowed?',
           'cycleage_q13b1_cat',
           'If you have a bicycle, are you allowed to ride it to go to places (like the',
           'How many times do you cycle in a typical week (both with and without parent')

for (i in 95:100) {
  j=i-94
  print(paste(Act12[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}

print("# QUESTION-14: QUESTIONS#14(a),14(b),14(c),and Q14(d")
Act13 <- c('Are you allowed to go on  public buses on your own (other than a school bus)',
           'FriensIM')

for (i in 95:100) {
  j=i-94
  print(paste(Act13[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}

print("# QUESTION-16: Have you ever involved in a traffic injury?")
Act14 <- c('Have you ever been involved in a road traffic crash?',
           'If yes, were you injured were you injured in that crash?',
           'injury',
           'injury_healthcare',
           'healthcare_injury',
           'injury for which first aid,doctors consult and admitted',
           'First aid in school/home',
           'Doctors consultation',
           'Admitted in hospital',
           'It was not that serious',
           'Have you ever witnessed any road traffic crash?')

for (i in 125:135) {
  j=i-94
  print(paste(Act14[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}


print("# QUESTION-19: Have you ever involved in a traffic injury?")
Act15 <- c('How safe do you feel on your own in your local neighbourhood?',
           'q17_safeneighbour',
           'How safe do you think is the traffic environment in your local neighbourhoo',
           'q17btraffic',
           'Yes',
           'No, because of my age',
           'No, because of my gender',
           'No, other reasons',
           'Dont know',
           'q17c alonefrds',
            'q17c_cat_alonefriends',
            'q17_C',
            'traffic worried',
            'Traffic',
            'Getting lost',
            'Bullying',
            'Strangers',
            'If someone speaks to me',
            'Q17D6')

for (i in 136:154) {
  j=i-133
  print(paste(Act15[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}


print("# QUESTION GENERAL:QUESTIONS?")
Act16 <- c('TYPEOFSC_P',
           'age_cat',
           'class_cat',
           'Q20_sex',
           'mode_tranport',
           'accompany_school',
           'accompany_home')

for (i in 158:164) {
  j=i-94
  print(paste(Act16[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}

#Working Data 1
print("Any activity on weekend")
Act17 <- c('Any activity on weekend')

for (i in 125:125) {
  j=i-125
  print(paste(Act17[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}


#Working Data 2
print("Mode to school")
Act18 <- c('Walked most or all the way',
           'Cycled',
           'School bus',
           'School Van',
           'Public bus',
           'Car',
           'Motorcycle')

for (i in 26:32) {
  j=i-25
  print(paste(Act18[j]))
  df <- as.data.frame(table(data[,i], useNA = "ifany"))
  colnames(df) <- c("Variable","Freq")
  df$Percent = round(100*df$Freq/sum(df$Freq),1)
  print(df)
}
sink()