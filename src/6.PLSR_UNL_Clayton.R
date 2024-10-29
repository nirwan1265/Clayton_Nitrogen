################################################################################
######## RUNNING THE PSLR MODEL WITH YUFENG'S DATA
################################################################################

# Adding the training data points to the combined data points from 38
data_combined <- readRDS("data/unl_Ge_N_reflectance.rds")
str(data_combined)

# Create a random sample of row indices for the training set (80% of the data)
set.seed(13)
trainIndex <- sample(1:n, size = 0.8 * n, replace = FALSE)

# Split the data into training and testing sets
reflectance_grouped_avg_train <- reflectance_grouped_avg[trainIndex, ]
reflectance_grouped_avg_test  <- reflectance_grouped_avg[-trainIndex, ]

# Change Column name from 1000-1009 to avg_1000_1009
colnames(reflectance_grouped_avg_train) <- paste0("avg_",gsub("[-.]", "_", colnames(reflectance_grouped_avg_train)))
colnames(reflectance_grouped_avg_train)[217] <- "response"
colnames(reflectance_grouped_avg_test) <- paste0("avg_",gsub("[-.]", "_", colnames(reflectance_grouped_avg_test)))
colnames(reflectance_grouped_avg_test)[217] <- "response"

#Combine the data_combined and reflectance_grouped_avg_train data
data_combined_ncomp <- rbind(data_combined, reflectance_grouped_avg_train,reflectance_grouped_avg_test)
str(data_combined_ncomp)


########################################
######## Finding the correct no. of components
########################################

# Extract the Reponse and Predictors for finding the correct Number of Components
response <- data_combined_ncomp[, "response"]
predictors <- as.matrix(data_combined_ncomp %>% dplyr::select(-response))
predictors

# Parameters for PLSR
random_seed <- 100000
seg <- 80
maxComps <- 100
iterations <- 100
prop <- 0.70

# Fit the PLSR model for the components
plsr.out <- pls::plsr(response ~ predictors, scale = FALSE, center = TRUE, 
                      ncomp = maxComps, validation = "CV", 
                      segments = seg, segment.type = "interleaved", 
                      trace = FALSE, jackknife = TRUE)

# Determine the optimal number of components
# Used different seeds, got the same results = 51
nComps <- pls::selectNcomp(plsr.out, method = "onesigma", plot = TRUE)
print(paste0("Optimal number of components: ", nComps))

optimal_components <- nComps

########################################
######## Running the PLSR model
########################################

# Number of cross-validation repetitions
n_repeats <- 10
n <- nrow(data_combined)

# Pre-determined optimal number of components
optimal_components <- nComps

# Initialize vectors to store metrics for each repeat
train_r2cv <- numeric(n_repeats)
test_r2cv <- numeric(n_repeats)
train_rmsecv <- numeric(n_repeats)
test_rmsecv <- numeric(n_repeats)

# Loop through each seed for cross-validation
for (i in 1:n_repeats) {
  set.seed(13456 + i)  # Change seed for each iteration
  
  # Create a random sample of row indices for the training set (80% of the data)
  trainIndex <- sample(1:n, size = 0.8 * n, replace = FALSE)
  
  # Split the data into training and testing sets
  reflectance_grouped_avg_train <- reflectance_grouped_avg[trainIndex, ]
  reflectance_grouped_avg_test  <- reflectance_grouped_avg[-trainIndex, ]
  
  # Update column names for training and testing sets
  colnames(reflectance_grouped_avg_train) <- paste0("avg_", gsub("[-.]", "_", colnames(reflectance_grouped_avg_train)))
  colnames(reflectance_grouped_avg_train)[217] <- "response"
  colnames(reflectance_grouped_avg_test) <- paste0("avg_", gsub("[-.]", "_", colnames(reflectance_grouped_avg_test)))
  colnames(reflectance_grouped_avg_test)[217] <- "response"
  
  # Combine `data_combined` with the current training set
  combined_train_data <- rbind(data_combined, reflectance_grouped_avg_train)
  
  # Fit the PLSR model with leave-one-out cross-validation (LOO) on the combined data
  plsr_model <- pls::plsr(response ~ ., data = combined_train_data, validation = "LOO")
  
  # Predict for the combined training data and the separate test set
  predicted_N_train <- predict(plsr_model, ncomp = optimal_components, newdata = combined_train_data)
  predicted_N_test <- predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_test)
  
  # Calculate R²CV for training and test sets
  train_r2cv[i] <- cor(predicted_N_train, combined_train_data$response)^2
  test_r2cv[i] <- cor(predicted_N_test, reflectance_grouped_avg_test$response)^2
  
  # Calculate RMSECV for training and test sets
  train_rmsecv[i] <- sqrt(mean((predicted_N_train - combined_train_data$response)^2))
  test_rmsecv[i] <- sqrt(mean((predicted_N_test - reflectance_grouped_avg_test$response)^2))
}


# Combine results into a data frame for plotting
results_df <- data.frame(
  Repeat = rep(1:n_repeats, 2),
  Set = rep(c("Training", "Testing"), each = n_repeats),
  R2CV = c(train_r2cv, test_r2cv),
  RMSECV = c(train_rmsecv, test_rmsecv)
)

# Saving the file
#saveRDS(results_df, "data/PLSR_results_UNL_Clayton.rds")

# Plot R²CV with average lines
avg_train_r2cv <- mean(train_r2cv)
avg_test_r2cv <- mean(test_r2cv)

r2_plot <- ggplot(results_df, aes(x = Repeat, y = R2CV, color = Set)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(aes(yintercept = avg_train_r2cv), color = "blue", linetype = "dashed") +
  geom_hline(aes(yintercept = avg_test_r2cv), color = "red", linetype = "dashed") +
  labs(title = "Cross-Validation R² for PLSR Model", y = expression(R^2), x = "Iteration") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank()
  )

# Plot RMSECV with average lines
avg_train_rmsecv <- mean(train_rmsecv)
avg_test_rmsecv <- mean(test_rmsecv)

rmse_plot <- ggplot(results_df, aes(x = Repeat, y = RMSECV, color = Set)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(aes(yintercept = avg_train_rmsecv), color = "blue", linetype = "dashed") +
  geom_hline(aes(yintercept = avg_test_rmsecv), color = "red", linetype = "dashed") +
  labs(title = "Cross-Validation RMSE for PLSR Model", y = "RMSE", x = "Iteration") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank()
  )

# Print the plots
quartz()
print(r2_plot)
print(rmse_plot)
