1. Please keep only one version of the script in the repository,
   i.e. don't use the file name to indicate file version. So, rename
   Mobility-analysis-18-05-20.R to Mobility-analysis.R. 
2. You can use `useNA = "always"` to show `NA`s when you use
   `table`. For example:
   
   ```{r}
   table(data$school_shift, useNA = "always")
   ```
3. You don't make `school_transport` a factor again, once you have
   replaced the old levels with new.
4. The section where you fix factor levels and collapse leves seems to
   produce the desired result, but it looks kind of messy. I suggest
   you structure it according to each variable, for example:
   
   ```{r}
   ## Rename shift_school levels
   data$shift_school[data$shift_school == "both morning and evening shifts"] <- "Both morning and evening shifts"
   data$shift_school[data$shift_school == "morning shift only"] <- "Morning shift only"
   data$shift_school[data$shift_school == "evening shift"] <- "Evening shift only"

   ## Setting shift_school empty entries to missing
   data$shift_school[data$shift_school == ""] <- NA

   ## Transform shift_school to a factor again
   data$shift_school <- as.factor(data$shift_school)
   table(data$shift_school, useNA = "always")
   ```
5. You need to fix the relevel of travel accompany for model 4 as you
   have renamed the levels.
   
