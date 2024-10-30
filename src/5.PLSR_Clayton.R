################################################################################
######## RUNNING THE PSLR MODEL ON CLAYTON DATA
################################################################################


########################################
######## Loading the files
########################################

# Load the files
reflectance_grouped_avg <- readRDS("data/reflectance_all_grouped_avg.rds")
response_N <- readRDS("data/response_N.rds")

# Perform the join for Nitrogen and reflectance for Clayton
reflectance_grouped_avg2 <- inner_join(reflectance_grouped_avg, response_N, by = "file")
# Set the row names to the values in the 'file' column
rownames(reflectance_grouped_avg2) <- reflectance_grouped_avg2$file
# Remove the 'file' column now that it's set as row names
reflectance_grouped_avg2 <- reflectance_grouped_avg2 %>% dplyr::select(-c(221,220,218)) 
reflectance_grouped_avg2 <- reflectance_grouped_avg2 %>% dplyr::select(-file)
reflectance_grouped_avg2 <- reflectance_grouped_avg2[complete.cases(reflectance_grouped_avg2),]

setdiff(response_N$file, reflectance_grouped_avg$file)

# Convert all columns in reflectance_grouped_avg_train to numeric except the 'file' column
# reflectance_grouped_avg2 <- reflectance_grouped_avg2 %>%
#   dplyr::mutate(across(-file, as.numeric))

str(reflectance_grouped_avg2)

# Divide reflectance_grouped_avg into training and testing data sets with 5 CV for PSLR model
set.seed(123)

# Determine the number of rows
n <- nrow(reflectance_grouped_avg2)

# Create a random sample of row indices for the training set (80% of the data)
reflectance_grouped_avg <- reflectance_grouped_avg2


########################################
######## Finding the correct no. of components
########################################

# Extract the Reponse and Predictors for finding the correct Number of Components
response <- reflectance_grouped_avg[, "Nitrogen"]
predictors <- as.matrix(reflectance_grouped_avg %>% select(-Nitrogen))
predictors

# Parameters for PLSR
random_seed <- 7529075
seg <- 80
maxComps <- 10
iterations <- 100
prop <- 0.70

# Fit the PLSR model for the components
plsr.out <- pls::plsr(response ~ predictors, scale = FALSE, center = TRUE, 
                      ncomp = maxComps, validation = "CV", 
                      segments = seg, segment.type = "interleaved", 
                      trace = FALSE, jackknife = TRUE)

# Determine the optimal number of components
nComps <- pls::selectNcomp(plsr.out, method = "onesigma", plot = TRUE)
print(paste0("Optimal number of components: ", nComps))


########################################
######## Running the PLSR model
########################################


### Remove 10% of the data for unknown data/without training:
set.seed(123)  # For reproducibility
test_indices <- sample(1:nrow(reflectance_grouped_avg), size = 0.1 * nrow(reflectance_grouped_avg), replace = FALSE)
reflectance_grouped_avg_test_final <- reflectance_grouped_avg[test_indices, ]  # 10% for final testing
reflectance_grouped_avg <- reflectance_grouped_avg[-test_indices, ]      # 90% for training and cross-validation

# Number of cross-validation repetitions
n_repeats <- 10
n <- nrow(reflectance_grouped_avg)

# Pre-determined optimal number of components
optimal_components <- nComps

# Initialize vectors to store metrics for each repeat
test_r2 <- numeric(n_repeats)       # R-squared (R²) for each CV test set
train_r2cv <- numeric(n_repeats)    # r² for training set (squared correlation)
test_r2cv <- numeric(n_repeats)     # r² for test set (squared correlation)
train_rmsecv <- numeric(n_repeats)
test_rmsecv <- numeric(n_repeats)
bias_cv <- numeric(n_repeats)       # Bias for each test set

# Initialize a list to store predicted and actual values across all CV iterations
cv_results <- list()

# Loop through 10 random seeds
for (i in 1:n_repeats) {
  set.seed(44 + i)  # Change seed for each iteration
  
  # Create a random sample of row indices for the training set (80% of the data)
  trainIndex <- sample(1:n, size = 0.8 * n, replace = FALSE)
  
  # Split the data into training and testing sets
  reflectance_grouped_avg_train <- reflectance_grouped_avg[trainIndex, ]
  reflectance_grouped_avg_test  <- reflectance_grouped_avg[-trainIndex, ]
  
  # Fit the PLSR model with leave-one-out cross-validation (LOO)
  plsr_model <- pls::plsr(Nitrogen ~ ., data = reflectance_grouped_avg_train, validation = "LOO")
  
  # Calculate predictions for the test set with the optimal number of components
  predicted_N_test <- as.numeric(predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_test))
  
  # Store predictions and actual values for this iteration
  cv_results[[i]] <- data.frame(
    Measured_N = reflectance_grouped_avg_test$Nitrogen,
    Predicted_N = predicted_N_test,
    Sample_ID = rownames(reflectance_grouped_avg_test)
  )
  
  # Calculate r² for training and test sets (squared correlation coefficient)
  train_r2cv[i] <- cor(predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_train), reflectance_grouped_avg_train$Nitrogen)^2
  test_r2cv[i] <- cor(predicted_N_test, reflectance_grouped_avg_test$Nitrogen)^2
  
  # Calculate RMSECV for training and test sets
  train_rmsecv[i] <- sqrt(mean((predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_train) - reflectance_grouped_avg_train$Nitrogen)^2))
  test_rmsecv[i] <- sqrt(mean((predicted_N_test - reflectance_grouped_avg_test$Nitrogen)^2))
  
  # Calculate true R-squared (R²) for the test set
  SS_res <- sum((reflectance_grouped_avg_test$Nitrogen - predicted_N_test)^2)
  SS_tot <- sum((reflectance_grouped_avg_test$Nitrogen - mean(reflectance_grouped_avg_test$Nitrogen))^2)
  test_r2[i] <- 1 - (SS_res / SS_tot)
  
  # Calculate Bias for the test set
  bias_cv[i] <- mean(predicted_N_test - reflectance_grouped_avg_test$Nitrogen)
}

# Combine all CV results into a single data frame
cv_results_df <- do.call(rbind, cv_results)

# Calculate the average predicted values per sample
avg_predictions <- aggregate(Predicted_N ~ Sample_ID + Measured_N, data = cv_results_df, FUN = mean)

# Calculate average R²CV, RMSECV, and BiasCV for testing sets across all iterations
avg_test_r2 <- mean(test_r2)            # Average R² for test sets
avg_train_r2cv <- mean(train_r2cv)      # Average r²CV for training sets
avg_test_r2cv <- mean(test_r2cv)        # Average r²CV for test sets
avg_train_rmsecv <- mean(train_rmsecv)  # Average RMSECV for training sets
avg_test_rmsecv <- mean(test_rmsecv)    # Average RMSECV for test sets
avg_bias_cv <- mean(bias_cv)            # Average Bias for test sets

# Print out the averaged metrics
cat("Average Test R² (R²CV):", round(avg_test_r2, 4), "\n")
cat("Average Training r²CV:", round(avg_train_r2cv, 4), "\n")
cat("Average Test r²CV:", round(avg_test_r2cv, 4), "\n")
cat("Average Training RMSECV:", round(avg_train_rmsecv, 4), "\n")
cat("Average Test RMSECV:", round(avg_test_rmsecv, 4), "\n")
cat("Average Bias (BiasCV):", round(avg_bias_cv, 4), "\n")


n <- nrow(avg_predictions)

### Adding low and High N
response_N <- vroom("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/element_analysis.csv")
# Remove all 0's from the true_data's column Nitrogen
response_N <- response_N[response_N$Nitrogen != 0,]
response_N <- response_N[response_N$Hydrogen != 0,]
response_N <- response_N[response_N$Carbon != 0,]

# Convert all to "_" 
response_N$file <- gsub("[-.]", "_", response_N$file)
colnames(response_N)

# Average the duplicate values in the file column of response_N except the Genotype, Stage, High/low N columns
response_N <- response_N %>%
  dplyr::group_by(file) %>%
  dplyr::summarise(
    Genotype = first(Genotype),
    Stage = first(Stage),
    `High/low N` = first(`High/low N`),  # Take the first occurrence of High/low N
    across(-c(Genotype, Stage, `High/low N`), mean, na.rm = TRUE),  # Average other columns
    .groups = "drop"
  )
# Remove leading/trailing spaces and convert to lowercase
response_N$file <- trimws(tolower(response_N$file))

# Check the result
head(response_N)

str(response_N)
str(avg_predictions)
unique(response_N$Stage)

avg_predictions <- inner_join(avg_predictions, response_N, by = c("Sample_ID" = "file"))
str(avg_predictions)

# Recode Stage to Early and Late in avg_predictions
unique(avg_predictions$Stage)

# avg_predictions <- avg_predictions %>%
#   dplyr::mutate(Stage_Category = ifelse(Stage %in% c("R2", "V13", "VT"), "Maturation", "Post-Maturation"))

avg_predictions <- avg_predictions %>%
  dplyr::mutate(Stage_Category = dplyr::case_when(
    Stage == "R2" ~ "R2",
    Stage == "V13" ~ "V13",
    Stage == "VT" ~ "VT",
    Stage == "R4" ~ "R4",
    Stage == "PM" ~ "PM"
  ))

calibration_plot <- ggplot(avg_predictions, aes(x = Measured_N, y = Predicted_N, shape = Stage_Category, fill = `High/low N`)) +
  geom_point(size = 3, color = "black") +
  #scale_shape_manual(values = c("Maturation" = 21, "Post-Maturation" = 24)) +  # Circle for Early, Triangle for Late
  scale_shape_manual(values = c(
    "R2" = 21,  # Circle
    "V13" = 22,     # Square
    "VT" = 23,           # Diamond
    "R4" = 24,      # Triangle
    "PM" = 25      # Downward triangle
  )) +
  scale_fill_manual(values = c("High" = "black", "Low" = NA)) +  # Black for High, no fill for Low
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Measured N content (%)",
    y = "Predicted N content (%)",
    title = "Cross-Validated Calibration Plot with Averaged Predictions"
  ) +
  theme_minimal(base_size = 15) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white")) +
  annotate("text", x = 1, y = 5.5, label = paste("R²CV =", round(avg_test_r2, 4)), hjust = 0, size = 4) +
  annotate("text", x = 1, y = 5, label = paste("RMSECV =", round(avg_test_rmsecv, 2)), hjust = 0, size = 4) +
  annotate("text", x = 1, y = 4.5, label = paste("BiasCV =", round(avg_bias_cv, 4)), hjust = 0, size = 4) +
  annotate("text", x = 1, y = 4, label = paste("n =", n), hjust = 0, size = 4) +
  annotate("text", x = 1, y = 3.5, label = paste(optimal_components, "comp"), hjust = 0, size = 4) +
  guides(fill = guide_legend(override.aes = list(shape = 21)))  # Ensures fill legend shows as circles

# Print the calibration plot
print(calibration_plot)

# Save the calibration plot
ggsave("figures/plsr_calibration_Clayton_log10.png", calibration_plot, width = 10, height = 6, units = "in", dpi = 300, bg = "white")

# Plot R²CV with average lines
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
print(r2_plot)
print(rmse_plot)

# Save the plots
ggsave("figures/plsr_r2_Clayton.png", r2_plot, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/plsr_rmse_Clayton.png", rmse_plot, width = 10, height = 6, units = "in", dpi = 300,bg = "white")



# For untested data:
# Calculate predictions for the test set with the optimal number of components
predicted_N_test_final <- as.numeric(predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_test_final))
cor(reflectance_grouped_avg_test_final$Nitrogen,predicted_N_test_final)