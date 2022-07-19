library(tidyverse)


# Reading in the data
exit <- read_csv("TAFE Exit Survey.csv", name_repair = "universal")

# First, let's write a function to convert all responses from Likert-type items to numeric

agree_recode <- function(x) {
  as.numeric(case_when(
    x == "Strongly Disagree" ~ 1,
    x == "Disagree" ~ 2,
    x == "Neutral" ~ 3,
    x == "Agree" ~ 4,
    x == "Strongly Agree" ~ 5,
  ))
}

# Data Wrangling
## Our current dataset has all likert-type questions as one question per column.
## We want to reshape the data to make it "tidy". 
## This means that all Likert-type questions for each category should be presented as a table with all questions in one row, and all responses in the next.

exit_longer <- exit %>%
  mutate_if(is.character, factor) %>% # Converted all characters to factors so it's easier to tally responses
  pivot_longer(cols = 6:17, 
               names_to = "contributing_factors", 
               values_to = "contributing_response",
               names_prefix = "Contributing.Factors..") %>%
  # Pivoting all the data from columns 6 to 17 transforms all columns into responses, and places them in column 6
  pivot_longer(cols = 7:19, # That's why we start the counting for the next set of columns here at 7 (and not their original index)
               names_to = "institute_factors", 
               values_to = "institute_response",
               names_prefix = "InstituteViews..") %>%
  pivot_longer(cols = 7:23, names_to = "workunit_factors", values_to = "workunit_response", # Same logic here
               names_prefix = "WorkUnitViews..") %>%
  mutate(contributing_response = agree_recode(contributing_response),
         institute_response = agree_recode(institute_response),
        workunit_response = agree_recode(workunit_response)) 

exit_final <- exit_longer[,c(1:6, 25:36)] # Select only variables we want to include in our dashboard

# Create a vector of column names exit_final_cols
exit_final_cols <- c("ID", "Institute", "WorkArea", "Year", "Reason", "Main_Factor", "Gender", "Age", 
                     "Employment_Type", "Classification", "LOS_Overall", "LOS_Workplace", 
                     "contributing_factors", "contributing_response", "institute_factors", "institute_response",
                     "workunit_factors", "workunit_response")

# Convert column names to the vector we just created
colnames(exit_final) <- exit_final_cols

# Save file as csv
write_csv(exit_final, "TAFE Exit Survey Clean.csv")

