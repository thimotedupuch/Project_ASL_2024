# Load required packages
install.packages("mice")
library(mice)
library(dplyr)
library(forcats)

# Load the dataset
Patients_C <- read.csv("SMART.csv", sep = ";")

# Categorical variable conversions
Patients_C <- Patients_C %>%
  mutate(
    SEX = factor(SEX) %>% fct_recode("male" = "1", "female" = "2"),
    DIABETES = factor(DIABETES) %>% fct_recode("No" = "0", "Yes" = "1"),
    SMOKING = factor(SMOKING) %>% fct_recode("never" = "1", "former" = "2", "current" = "3"),
    alcohol = factor(alcohol) %>% fct_recode("never" = "1", "former" = "2", "current" = "3"),
    CEREBRAL = factor(CEREBRAL) %>% fct_recode("No" = "0", "Yes" = "1"),
    CARDIAC = factor(CARDIAC) %>% fct_recode("No" = "0", "Yes" = "1"),
    AAA = factor(AAA) %>% fct_recode("No" = "0", "Yes" = "1"),
    PERIPH = factor(PERIPH) %>% fct_recode("No" = "0", "Yes" = "1"),
    albumin = factor(albumin) %>% fct_recode("no" = "1", "micro" = "2", "macro" = "3"),
    STENOSIS = factor(STENOSIS) %>% fct_recode("No" = "0", "Yes" = "1")
  )

# Final pre-processing
final_part_c <- Patients_C %>%
  mutate(log_CREAT = log(CREAT)) %>%
  select(-c(HISTCARD, CREAT))  # Exclude these columns

# Impute missing values with 'mice' - generate 5 imputed datasets
imputed_data <- mice(final_part_c, m = 5, maxit = 5, method = 'pmm', seed = 123)

# Fit logistic regression model on imputed datasets
log_reg_model <- with(imputed_data, glm(EVENT ~ AGE + SEX + BMI + SYSTH + HDL + DIABETES + HISTCAR2 + HOMOC + log_CREAT + STENOSIS + IMT + SMOKING + alcohol + albumin,
                                        family = binomial))

# Pool the results from the 5 imputed datasets
pooled_results <- pool(log_reg_model)

# View the pooled summary
summary(pooled_results)

# Compare the pooled estimates with the original logistic regression model (from part (c))
