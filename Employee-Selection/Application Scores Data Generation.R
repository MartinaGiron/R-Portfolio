library(MASS)
library(tidyverse)
library(writexl)


# Function to set a maximum score
cap_score <- function(x, cap) {
  ifelse(x > cap, cap, x)
}

# Generate Applicant ID Number
id <- paste0("Applicant #", 1:320)

# Assign Management Experience Randomly
set.seed(0717)
management_random <- rbinom(320, 1, 0.5)
management <- ifelse(management_random == 0, "manager", "non-manager") 

# Situational Judgment Test Scores
set.seed(0717)
situational <- round(runif(320, 60, 100))

# Number of Absences
set.seed(0717)
absences <- rpois(320, lambda = 1)

# Data Viz Exam Scores
set.seed(0717)
coding_viz <- rnorm(320, 85, 5) %>%
  cap_score(., 100) %>% 
  round(2)

# Correlated Variables
cor_var_means <- c(6.8, 7.2, 8.4, 77, 84, 80)
cor_var_matrix <- matrix(
  c(
    0.87, 0.6, 0.7, 0.36, 1.55, 0.57,
    0.6, 1.2, 0.52, 0.5, 1.2, 2.34,
    0.7, 0.52, 0.68, 0.45, 0.89, 0.75,
    0.36, 0.5, 0.45, 15.2, 1.09, 1.64,
    1.55, 1.2, 0.89, 1.09, 17.2, 1.88,
    0.57, 2.34, 0.75, 1.64, 1.88, 9.3
  ), byrow = T, nrow = 6
)


set.seed(0717)
correlated_vars_df <- as.data.frame(mvrnorm(n = 320, mu = cor_var_means, Sigma = cor_var_matrix))
correlated_vars_df_cols <- c("interview_p1", "interview_p2", "interview_p3", "coding_cleaning", "coding_ml", "performance")
colnames(correlated_vars_df) <- correlated_vars_df_cols

correlated_vars_df <- correlated_vars_df %>%
  mutate(interview_p1 = round(cap_score(interview_p1, 10), 1),
         interview_p2 = round(cap_score(interview_p2, 10), 1),
         interview_p3 = round(cap_score(interview_p3, 10), 1),
         coding_cleaning = round(cap_score(coding_cleaning, 100), 2),
         coding_ml = round(cap_score(coding_ml, 100), 2),
         performance = round(cap_score(performance, 100))
         )

# Gather them all into one dataset
applicant_scores <- cbind(
  id, management, situational, coding_viz, correlated_vars_df, absences
)

applicant_final <- applicant_scores[1:300, ]

write_xlsx(applicant_final, "Employee Selection Scores.xlsx")
