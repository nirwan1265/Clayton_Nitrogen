# Initial split: 90% for training and 10% for final test
set.seed(235)  # For reproducibility
# Adding the training data points to the combined data points from 38
data_combined <- readRDS("data/unl_Ge_N_reflectance.rds")
colnames(data_combined)[1] <- "Nitrogen"
str(data_combined)

reflectance_grouped_avg <- readRDS("data/reflectance_all_grouped_avg.rds")
test_indices <- sample(1:nrow(reflectance_grouped_avg), size = 0.5 * nrow(reflectance_grouped_avg), replace = FALSE)
reflectance_grouped_avg_test_final <- reflectance_grouped_avg[test_indices, ]  # 10% for final testing
reflectance_grouped_avg <- reflectance_grouped_avg[-test_indices, ]      # 90% for training and cross-validation


# Create a random sample of row indices for the training set (80% of the data)
set.seed(13)
trainIndex <- sample(1:n, size = 0.8 * n, replace = FALSE)

# Split the data into training and testing sets
reflectance_grouped_avg_test_final <- reflectance_grouped_avg[trainIndex, ]
reflectance_grouped_avg  <- reflectance_grouped_avg[-trainIndex, ]

# Change Column name from 1000-1009 to avg_1000_1009
colnames(reflectance_grouped_avg_test_final) <- paste0("avg_",gsub("[-.]", "_", colnames(reflectance_grouped_avg_train)))
colnames(reflectance_grouped_avg_test_final)[217] <- "Nitrogen"
colnames(reflectance_grouped_avg) <- paste0("avg_",gsub("[-.]", "_", colnames(reflectance_grouped_avg_test)))
colnames(reflectance_grouped_avg)[217] <- "Nitrogen"

#Combine the data_combined and reflectance_grouped_avg_train data
data_combined_ncomp <- rbind(data_combined, reflectance_grouped_avg)
str(data_combined_ncomp)



reflectance_grouped_avg_train <- data_combined_ncomp

# Run PLSR model on the 90% training set with cross-validation
n_repeats <- 5
optimal_components <- 51

# Initialize vectors to store metrics for each repeat
test_r2 <- numeric(n_repeats)
train_r2cv <- numeric(n_repeats)
test_r2cv <- numeric(n_repeats)
train_rmsecv <- numeric(n_repeats)
test_rmsecv <- numeric(n_repeats)
bias_cv <- numeric(n_repeats)
cv_results <- list()  # Store predicted and actual values across all CV iterations

# Loop through 10 random seeds for cross-validation
for (i in 1:n_repeats) {
  set.seed(44 + i)
  trainIndex <- sample(1:nrow(reflectance_grouped_avg_train), size = 0.8 * nrow(reflectance_grouped_avg_train), replace = FALSE)
  
  # Split 90% training data into CV training and CV testing sets
  reflectance_grouped_avg_cv_train <- reflectance_grouped_avg_train[trainIndex, ]
  reflectance_grouped_avg_cv_test  <- reflectance_grouped_avg_train[-trainIndex, ]
  
  # Fit the PLSR model with LOO cross-validation on CV training set
  plsr_model <- pls::plsr(Nitrogen ~ ., data = reflectance_grouped_avg_cv_train, validation = "LOO")
  
  # Predictions for CV test set
  predicted_N_test <- as.numeric(predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_cv_test))
  
  # Store predictions and actual values for this iteration
  cv_results[[i]] <- data.frame(
    Measured_N = reflectance_grouped_avg_cv_test$Nitrogen,
    Predicted_N = predicted_N_test,
    Sample_ID = rownames(reflectance_grouped_avg_cv_test)
  )
  
  # Calculate metrics for CV iterations
  train_r2cv[i] <- cor(predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_cv_train), reflectance_grouped_avg_cv_train$Nitrogen)^2
  test_r2cv[i] <- cor(predicted_N_test, reflectance_grouped_avg_cv_test$Nitrogen)^2
  train_rmsecv[i] <- sqrt(mean((predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_cv_train) - reflectance_grouped_avg_cv_train$Nitrogen)^2))
  test_rmsecv[i] <- sqrt(mean((predicted_N_test - reflectance_grouped_avg_cv_test$Nitrogen)^2))
  SS_res <- sum((reflectance_grouped_avg_cv_test$Nitrogen - predicted_N_test)^2)
  SS_tot <- sum((reflectance_grouped_avg_cv_test$Nitrogen - mean(reflectance_grouped_avg_cv_test$Nitrogen))^2)
  test_r2[i] <- 1 - (SS_res / SS_tot)
  bias_cv[i] <- mean(predicted_N_test - reflectance_grouped_avg_cv_test$Nitrogen)
  
  print(i)  # Track progress
}
str(cv_results)

# Combine all test set predictions from cross-validation into one data frame for meta-modeling
meta_data <- do.call(rbind, cv_results)  # This will contain Measured_N, Predicted_N, and Sample_ID
str(meta_data)  # Confirm structure

# Define cross-validation control for the meta-model
cv <- caret::trainControl(method = "cv", number = 5)

# Train the meta-model using Random Forest on cross-validation test predictions
meta_model <- caret::train(Measured_N ~ Predicted_N,  # Predicting Measured_N from PLSR's Predicted_N
                           data = meta_data,
                           method = "RRF",
                           trControl = cv)
?caret::train()
# Output model performance metrics
print(meta_model)

# Ensure reflectance_grouped_avg_test_final has `Predicted_N` for final test evaluation
predicted_N_final_plsr <- predict(plsr_model_final, ncomp = optimal_components, newdata = reflectance_grouped_avg_test_final)
reflectance_grouped_avg_test_final <- reflectance_grouped_avg_test_final %>%
  mutate(Predicted_N = as.numeric(predicted_N_final_plsr))

# Evaluate meta-model on this 10% final test set
predicted_N_final <- predict(meta_model, newdata = reflectance_grouped_avg_test_final)

# Calculate final test metrics
rmse_final <- sqrt(mean((reflectance_grouped_avg_test_final$Nitrogen - predicted_N_final)^2))
SS_res_final <- sum((reflectance_grouped_avg_test_final$Nitrogen - predicted_N_final)^2)
SS_tot_final <- sum((reflectance_grouped_avg_test_final$Nitrogen - mean(reflectance_grouped_avg_test_final$Nitrogen))^2)
R2_final <- 1 - (SS_res_final / SS_tot_final)
bias_final <- mean(predicted_N_final - reflectance_grouped_avg_test_final$Nitrogen)
correlation_r2_final <- cor(reflectance_grouped_avg_test_final$Nitrogen, predicted_N_final)^2

# Print results for final test set
cat("RMSE on Final Test Set:", round(rmse_final, 4), "\n")
cat("R² on Final Test Set:", round(R2_final, 4), "\n")
cat("Bias on Final Test Set:", round(bias_final, 4), "\n")
cat("r² (Correlation) on Final Test Set:", round(correlation_r2_final, 4), "\n")

