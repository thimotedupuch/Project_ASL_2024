#install.packages("MASS")

library(dplyr)
library(forcats)
library(ggplot2)
library(plotly)
library(broom)
library(boot)
library(caret)
library(pROC)
library(knitr)
library(MASS)

dataset <- read.csv("C:/Users/jurre/Downloads/SMARTc.csv", sep = ";") # Without missing values

dataset <- mutate(dataset,
                  EVENT = factor(EVENT),
                  EVENT = fct_recode(EVENT, "no" = "0", "yes" = "1"),
                  SEX = factor(SEX),
                  SEX = fct_recode(SEX, "male" = "1", "female" = "2"),
                  DIABETES = factor(DIABETES),
                  DIABETES = fct_recode(DIABETES, "no" = "0", "yes" = "1"),
                  SMOKING = factor(SMOKING),
                  SMOKING = fct_recode(SMOKING, "never" = "1", "former" = "2", "current" = "3"),
                  alcohol = factor(alcohol),
                  alcohol = fct_recode(alcohol, "never" = "1", "former" = "2", "current" = "3"),
                  CEREBRAL = factor(CEREBRAL),
                  CEREBRAL = fct_recode(CEREBRAL, "no" = "0", "yes" = "1"),
                  CARDIAC = factor(CARDIAC),
                  CARDIAC = fct_recode(CARDIAC, "no" = "0", "yes" = "1"),
                  AAA = factor(AAA),
                  AAA = fct_recode(AAA, "no" = "0", "yes" = "1"),
                  PERIPH = factor(PERIPH),
                  PERIPH = fct_recode(PERIPH, "no" = "0", "yes" = "1"),
                  albumin = factor(albumin),
                  albumin = fct_recode(albumin, "no" = "1", "micro" = "2", "macro" = "3"),
                  STENOSIS = factor(STENOSIS),
                  STENOSIS = fct_recode(STENOSIS, "no" = "0", "yes" = "1"),
)

#| output: false
k <- 10
set.seed(123)

#shuffle data
Shuffled_data <- dataset[sample(nrow(dataset)), ]

folds <- createFolds(Shuffled_data$EVENT, k = k, list = TRUE, returnTrain = FALSE)
roc_list <- list()
auc_values <- numeric(k)

for (i in 1:k) {
  train <- dataset[-folds[[i]], ]
  test <- dataset[folds[[i]], ]
  
  fit_train <- lda(EVENT ~ AGE + SEX + BMI + SYSTH + HDL + DIABETES +
                     HISTCAR2 + HOMOC + log(CREAT) + STENOSIS + IMT + SMOKING +
                     alcohol + albumin, data = train, family = "binomial")
  
  previous_predict_test <- predict(fit_train, newdata = test, type = "response")
  predict_test <- as.numeric(previous_predict_test$posterior[, 2])
  
  roc_i <- roc(test$EVENT, predict_test)
  roc_list[[i]] <- roc_i
  auc_values[i] <- roc_i$auc
}

#| code-fold: true
#| code-summary: display code to plot the ROC curves
roc_df <- do.call(rbind, lapply(1:length(roc_list), function(i) {
  data.frame(
    Fold = paste("Fold", i),
    Sensitivity = roc_list[[i]]$sensitivities,
    Specificity = 1 - roc_list[[i]]$specificities
  )
}))

roc_plot <- ggplot(roc_df, aes(x = Specificity, y = Sensitivity, color = Fold)) +
  geom_line(linewidth = 0.8) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  labs(
    title = "ROC Curves for Each Fold",
    x = "1 - Specificity",
    y = "Sensitivity",
    color = "Fold"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    legend.position = "bottom"
  ) +
  coord_equal()

ggplotly(roc_plot)

auc_table <- data.frame(Iteration = 1:k, AUC = auc_values)
mean_auc <- mean(auc_values)
auc_table$AUC <- round(auc_table$AUC, 4)
mean_auc <- round(mean_auc, 4)
auc_table <- rbind(auc_table, c("AUC mean", mean_auc))
kable(auc_table, caption = "AUC Values for 10-Fold Cross-Validation")

observed_outcomes <- as.numeric(test$EVENT) - 1
calibration_data <- data.frame(Predicted = predict_test, Observed = observed_outcomes)
calibration_plot <- ggplot(calibration_data, aes(x = Predicted, y = Observed)) + 
  geom_smooth(method = "loess", se = FALSE, color = "#3ca7c7") +
  geom_point(size = 2, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#ff8800") +
  labs(x = "Predicted Probability", y = "Observed Probability", title = "Calibration Plot") +
  theme_minimal()

print(calibration_plot)
