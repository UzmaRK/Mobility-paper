1. Some variable labels, for example "Friends dark", are difficult to
   understand. Do you mean "Allowed to meet friends after dark"?
2. To rename the levels of a variable you can do what you did for
   rti_consult. For example, the levels of the variable `shift_school`
   are currently "both morning and evening shifts", "morning shift
   only" and "evening shift". You want these levels to start with an
   uppercase letter and have the same structure, i.e. "Both morning
   and evening shifts", "Morning shift only" and "Evening shift
   only". To accomplish that do:
   
   ```{r}
   data$shift_school <- as.character(data$shift_school)
   data$shift_school[data$shift_school == "both morning and evening shifts"] <- "Both morning and evening shifts"
   data$shift_school[data$shift_school == "morning shift only"] <- "Morning shift only"
   data$shift_school[data$shift_school == "evening shift"] <- "Evening shift only"
   ## Note that you also have 67 empty entries in this variable. I
   ## suggest you set those to missing
   data$shift_school[data$shift_school == ""] <- NA
   ## Transform the variable to a factor again
   data$shift_school <- as.factor(data$shift_school)
   ## To confirm that you got it right you can use table
   table(data$shift_school, useNA = "always")
   ```
3. To collapse levels you can use a similar approach. Let's say you
   want to collapse "Morning shift only" and "Evening shift only" to
   "Either morning or evening shift". Then do:
   
   ```{r}
   collapsed.shift.school <- as.character(data$shift_school)
   collapsed.shift.school[collapsed.shift.school == "Morning shift only"] <- "Either morning or evening shift"
   collapsed.shift.school[collapsed.shift.school == "Evening shift only"] <- "Either morning or evening shift"
   ## Check that you got it right
   table(collapsed.shift.school, useNA = "always")
   ## Add it to the data as a factor
   data$collapsed_shift_school <- as.factor(collapsed.shift.school)
   ```
