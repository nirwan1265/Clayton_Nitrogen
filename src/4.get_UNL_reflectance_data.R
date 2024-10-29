################################################################################
######## LOAD THE YUFENG GE DATA FROM UNL *UNIVERSITY OF NEBRASKA-LINCOLN* 
################################################################################

# Load the Leaf spectral data and subset corn
data <- read.csv("/Users/nirwantandukar/Documents/Research/data/leaf_spectral/UNL_leaf_spectral.csv")
data <- data[which(data$crop == "Corn"), ]
names(data) <- gsub("^X","",names(data))
str(data)

# Separate predictors and response
predictors <- data[, 15:ncol(data)]
response <- data$N

# Function to calculate the moving average with a window size of 10
calculate_window_average <- function(df, window_size) {
  num_columns <- ncol(df)
  num_rows <- nrow(df)
  column_names <- colnames(df)
  new_df <- data.frame(matrix(ncol = 0, nrow = num_rows))  # Initialize an empty data frame with the correct number of rows
  
  # Loop through the columns in steps of window_size
  for (i in seq(1, num_columns, by = window_size)) {
    # Calculate the end index for the current window
    end_index <- min(i + window_size - 1, num_columns)
    
    # Check if the window has more than one column
    if (end_index > i) {
      # Calculate the row-wise average for the current window
      window_avg <- rowMeans(df[, i:end_index], na.rm = TRUE)
    } else {
      # If only one column, use the column values directly
      window_avg <- df[, i]
    }
    
    # Create a new column name using the original column names
    new_col_name <- paste0("avg_", column_names[i], "_", column_names[end_index])
    
    # Add the averaged values as a new column to new_df
    new_df[[new_col_name]] <- window_avg
  }
  
  return(new_df)
}

# Apply the function to the predictors data frame
window_size <- 10
predictors <- calculate_window_average(predictors, window_size)

# Combine predictors and response into one data frame
data_combined <- data.frame(predictors, response)
str(data_combined)

# Put the response column at the first position
data_combined <- data_combined[, c(ncol(data_combined), 1:(ncol(data_combined) - 1))]
str(data_combined)


# Save the data_combined as unl_Ge_N_reflectance.rds in data
saveRDS(data_combined, file = "data/unl_Ge_N_reflectance.rds")

data_combined <- readRDS("data/unl_Ge_N_reflectance.rds")
