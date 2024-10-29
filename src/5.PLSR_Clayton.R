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

reflectance_grouped_avg


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
maxComps <- 100
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

# Number of cross-validation repetitions
n_repeats <- 10
n <- nrow(reflectance_grouped_avg)

# Pre-determined optimal number of components
optimal_components <- nComps
optimal_components <- 7

# Initialize vectors to store metrics for each repeat
train_r2cv <- numeric(n_repeats)
test_r2cv <- numeric(n_repeats)
train_rmsecv <- numeric(n_repeats)
test_rmsecv <- numeric(n_repeats)

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
  
  # Calculate predictions for training and test sets with the optimal number of components
  predicted_N_train <- predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_train)
  predicted_N_test <- predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_test)
  
  # Calculate R²CV for training and test sets
  train_r2cv[i] <- cor(predicted_N_train, reflectance_grouped_avg_train$Nitrogen)^2
  test_r2cv[i] <- cor(predicted_N_test, reflectance_grouped_avg_test$Nitrogen)^2
  
  # Calculate RMSECV for training and test sets
  train_rmsecv[i] <- sqrt(mean((predicted_N_train - reflectance_grouped_avg_train$Nitrogen)^2))
  test_rmsecv[i] <- sqrt(mean((predicted_N_test - reflectance_grouped_avg_test$Nitrogen)^2))
}

# Combine results into a data frame for plotting
results_df <- data.frame(
  Repeat = rep(1:n_repeats, 2),
  Set = rep(c("Training", "Testing"), each = n_repeats),
  R2CV = c(train_r2cv, test_r2cv),
  RMSECV = c(train_rmsecv, test_rmsecv)
)


# Calculate average R²CV and RMSECV for training and testing sets
avg_train_r2cv <- mean(train_r2cv)
avg_test_r2cv <- mean(test_r2cv)
avg_train_rmsecv <- mean(train_rmsecv)
avg_test_rmsecv <- mean(test_rmsecv)

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





########################################
######## Running the PLSR model taking equal High and Low Nitrogen
########################################


# Load the response variable
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
    `High/low N` = first(`High/low N`),
    across(-c(Genotype, Stage, `High/low N`), mean, na.rm = TRUE),
    .groups = "drop"
  )
unique(table(response_N$file))
# Remove leading/trailing spaces and convert to lowercase
response_N$file <- trimws(tolower(response_N$file))

reflectance_grouped_avg <- readRDS("data/reflectance_all_grouped_avg.rds")

colnames(reflectance_grouped_avg)

# Perform the join for Nitrogen and reflectance for Clayton
reflectance_grouped_avg2 <- inner_join(reflectance_grouped_avg, response_N, by = "file")
#reflectance_grouped_avg2 <- reflectance_grouped_avg2 %>% dplyr::select(-c(221,220,218)) 
reflectance_grouped_avg2 <- reflectance_grouped_avg2 %>% dplyr::select(-c(file, Genotype, Stage, Carbon, Hydrogen))
reflectance_grouped_avg2 <- reflectance_grouped_avg2[complete.cases(reflectance_grouped_avg2),]
reflectance_grouped_avg <- reflectance_grouped_avg2
# Getting the reflectance for Nitrogen only
# reflectance_grouped_avg2 <- reflectance_grouped_avg2[,c(51:141, 217:219)]

# Number of cross-validation repetitions
n_repeats <- 10
n <- nrow(reflectance_grouped_avg)

# Pre-determined optimal number of components
optimal_components <- 7

# Initialize vectors to store metrics for each repeat
train_r2cv <- numeric(n_repeats)   # Cross-validated r² for training
test_r2cv <- numeric(n_repeats)    # Cross-validated r² for testing
train_r2 <- numeric(n_repeats)     # R² for training set
test_r2 <- numeric(n_repeats)      # R² for testing set
train_rmsecv <- numeric(n_repeats) # RMSECV for training
test_rmsecv <- numeric(n_repeats)  # RMSECV for testing

# Cross-validation loop
for (i in 1:n_repeats) {
  set.seed(44 + i)  # Change seed for each iteration
  
  # Split data into balanced High and Low subsets
  high_data <- reflectance_grouped_avg %>% filter(`High/low N` == "High")
  low_data <- reflectance_grouped_avg %>% filter(`High/low N` == "Low")
  min_size <- min(nrow(high_data), nrow(low_data))
  
  # Sampling for training and test sets
  train_high_indices <- sample(1:nrow(high_data), size = 0.9 * min_size, replace = FALSE)
  train_low_indices <- sample(1:nrow(low_data), size = 0.9 * min_size, replace = FALSE)
  reflectance_grouped_avg_train <- rbind(high_data[train_high_indices, ], low_data[train_low_indices, ])
  test_high_indices <- setdiff(1:nrow(high_data), train_high_indices)
  test_low_indices <- setdiff(1:nrow(low_data), train_low_indices)
  reflectance_grouped_avg_test <- rbind(high_data[test_high_indices, ], low_data[test_low_indices, ])
  
  # Remove `High/low N` from training data
  reflectance_grouped_avg_train <- reflectance_grouped_avg_train %>% select(-`High/low N`)
  
  # Fit PLSR model on training set
  plsr_model <- pls::plsr(Nitrogen ~ ., data = reflectance_grouped_avg_train, validation = "LOO", ncomp = optimal_components)
  
  # Predictions for training and test sets
  predicted_N_train <- predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_train)
  predicted_N_test <- predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_test)
  
  # Calculate r² (squared correlation) for training and test sets
  train_r2cv[i] <- cor(predicted_N_train, reflectance_grouped_avg_train$Nitrogen)^2
  test_r2cv[i] <- cor(predicted_N_test, reflectance_grouped_avg_test$Nitrogen)^2
  
  # Calculate R² (explained variance) for training and test sets
  ss_res_train <- sum((reflectance_grouped_avg_train$Nitrogen - predicted_N_train)^2)
  ss_tot_train <- sum((reflectance_grouped_avg_train$Nitrogen - mean(reflectance_grouped_avg_train$Nitrogen))^2)
  train_r2[i] <- 1 - (ss_res_train / ss_tot_train)
  
  ss_res_test <- sum((reflectance_grouped_avg_test$Nitrogen - predicted_N_test)^2)
  ss_tot_test <- sum((reflectance_grouped_avg_test$Nitrogen - mean(reflectance_grouped_avg_test$Nitrogen))^2)
  test_r2[i] <- 1 - (ss_res_test / ss_tot_test)
  
  # Calculate RMSECV for training and test sets
  train_rmsecv[i] <- sqrt(mean((predicted_N_train - reflectance_grouped_avg_train$Nitrogen)^2))
  test_rmsecv[i] <- sqrt(mean((predicted_N_test - reflectance_grouped_avg_test$Nitrogen)^2))
}

# Combine results for plotting
results_df <- data.frame(
  Repeat = rep(1:n_repeats, 6),
  Set = rep(c("Training r2CV", "Testing r2CV", "Training R2", "Testing R2", "Training RMSECV", "Testing RMSECV"), each = n_repeats),
  Value = c(train_r2cv, test_r2cv, train_r2, test_r2, train_rmsecv, test_rmsecv),
  Metric = rep(c("r2CV", "R2", "RMSECV"), each = n_repeats * 2)
)

# Calculate average values for each metric
avg_train_r2cv <- mean(train_r2cv)
avg_test_r2cv <- mean(test_r2cv)
avg_train_r2 <- mean(train_r2)
avg_test_r2 <- mean(test_r2)
avg_train_rmsecv <- mean(train_rmsecv)
avg_test_rmsecv <- mean(test_rmsecv)

# Separate data frames for each metric
r2cv_df <- results_df %>% filter(Metric == "r2CV")
r2_df <- results_df %>% filter(Metric == "R2")
rmsecv_df <- results_df %>% filter(Metric == "RMSECV")

# Plot r²CV with average lines
r2cv_plot <- ggplot(r2cv_df, aes(x = Repeat, y = Value, color = Set)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = avg_train_r2cv, color = "blue", linetype = "dotted") +
  geom_hline(yintercept = avg_test_r2cv, color = "red", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  labs(title = "Cross-Validation r²CV for PLSR Model", y = expression(r^2), x = "Iteration") +
  theme_minimal(base_size = 20) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.title = element_blank())

# Plot R² with average lines
r2_plot <- ggplot(r2_df, aes(x = Repeat, y = Value, color = Set)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = avg_train_r2, color = "purple", linetype = "dotted") +
  geom_hline(yintercept = avg_test_r2, color = "orange", linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  labs(title = "Cross-Validation R² for PLSR Model", y = expression(R^2), x = "Iteration") +
  theme_minimal(base_size = 20) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.title = element_blank())

# Plot RMSECV with average lines
rmsecv_plot <- ggplot(rmsecv_df, aes(x = Repeat, y = Value, color = Set)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = avg_train_rmsecv, color = "blue", linetype = "dotted") +
  geom_hline(yintercept = avg_test_rmsecv, color = "red", linetype = "dotted") +
  labs(title = "Cross-Validation RMSECV for PLSR Model", y = "RMSE", x = "Iteration") +
  theme_minimal(base_size = 20) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.title = element_blank())

# Print the plots
print(r2cv_plot)
print(r2_plot)
print(rmsecv_plot)

# Save the plots
ggsave("figures/plsr_r2_Clayton2.png", r2_plot, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/plsr_rmse_Clayton.png", rmse_plot, width = 10, height = 6, units = "in", dpi = 300,bg = "white")

