1. The variables are inconsistenly named, some includes dots, other
   hyphens, some are in all lowercase, some in a mix of upper and
   lower case. This cause problems because it seems like you've
   changed the variable names during the course of your coding, for
   example the CreateTableOne function complains that not all
   variables are in the dataset. They are most likely there, only that
   you've not provided the most recent names. I suggest you change to
   a consistent variable naming style, preferably using either
   underscores or dots to indicate spaces and then all lower case. For
   example `r school_home_transport`. 
2. In CreateTableOne you have indicated that only Grade is a
   categorical variable. You have many more categorical variables than
   that. The variable classes of these are not consistent however,
   some are factors and some are character. I suggest you set all the
   categorial variables to factors. You can do that by doing (you need
   to add the names of all categorical variables to catVars, those
   three are just examples):
   
   ```{r}
   catVars <- c("Grade", "agecat", "Gender")
   data[catVars] <- lapply(data[catVars], as.factor)
   ```
   
3. The univariate analysis complains that RTI has to be 0
   and 1. Currently it's coded as a character variable with "0", "1",
   and "missing". I suggest that you early in the script replace all
   "missing" with NA and then decide how to deal with it. There's not
   much missing as far as I can tell so I complete case analysis is
   probably okay. You then have to recode RTI as a numeric variable,
   for example using:
   
   ```{r}
   data$RTI <- as.numeric(data$RTI)
   ```
4. You don't have to use both `data$` and `data = data` in your call
   to glm, one is enough. I suggest you use only `data = data`, as in
   my revisions.
5. To create a pretty table and save it to disk as a docx you can do
   something like this:

	```r
   install.packages("labelled")
   install.packages("knitr")
   install.packages("rmarkdown")
   label.list <-list(agecat = "Age",
                     Gender = "Gender",
                     TYPEOFSC = "Type of school")
   labelled::var_label(data) <- label.list
   test.data <- data[names(label.list)]
   test.table <- tableone::CreateTableOne(data = test.data)
   pretty.table <- tableone:::print.TableOne(test.table, varLabels = TRUE, showAllLevels = TRUE)
   colnames(pretty.table)[colnames(pretty.table) == "level"] <- "Level"
   prettier.table <- knitr::kable(pretty.table)
   tmp.file <- tempfile(tmpdir = ".", fileext = "md")
   write(prettier.table, tmp.file)
   rmarkdown::render(tmp.file, output_file = "pretty-table.docx", output_format = "word_document")
   file.remove(tmp.file)
   ```


   
   
