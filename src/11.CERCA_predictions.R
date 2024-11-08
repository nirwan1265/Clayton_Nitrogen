#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
######## PREDICTING ALL THE NITROGEN VALUES FOR CERCA
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load the reflectance
reflectance_cerca <- vroom("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/CERCA_reflectance.csv")
reflectance_cerca[1:5,1:5]
ncol(reflectance_cerca)

# Write the loop to remove _n.asd from the colnames. 1 to 250
for (i in 1:10) {
  colnames(reflectance_cerca) <- gsub(paste0("_", i, ".asd"), "", colnames(reflectance_cerca))
}

# Initialize an empty list to store the averaged columns
reflectance_cerca_avg_list <- list()

# First, find the unique column names
unique_cols_cerca <- unique(names(reflectance_cerca))


for (col in unique_cols_cerca) {
  # Find all columns that match the current unique name
  matching_cols <- reflectance_cerca[, names(reflectance_cerca) == col, drop = FALSE]
  
  # If there is more than one column with the same name, average them
  if (ncol(matching_cols) > 1) {
    reflectance_cerca_avg_list[[col]] <- rowMeans(matching_cols, na.rm = TRUE)
  } else {
    # If there's only one column, just keep it as is
    reflectance_cerca_avg_list[[col]] <- matching_cols[[1]]
  }
}

# Convert the list back into a dataframe
reflectance_cerca_avg <- as.data.frame(reflectance_cerca_avg_list)
str(reflectance_cerca_avg)


# Create a new column for grouping, including a case for the last value
max_wavelength <- max(reflectance_cerca_avg$Wavelength)

reflectance_cerca_avg$Group <- cut(reflectance_cerca_avg$Wavelength, 
                                   breaks = c(seq(350, max_wavelength, by = 10), max_wavelength + 1), 
                                   include.lowest = TRUE, labels = FALSE)

# Create the labels for the first column as "350-359", "360-369", etc.
reflectance_cerca_avg$Wavelength_Group <- ifelse(
  reflectance_cerca_avg$Wavelength == max_wavelength, 
  paste0(max_wavelength, "-", max_wavelength),  # Special case for the last value (e.g., 2500-2500)
  paste0(((reflectance_cerca_avg$Group - 1) * 10 + 350), "-", (reflectance_cerca_avg$Group * 10 + 349))
)

# Now, calculate the average for every 10 rows, grouped by the new 'Wavelength_Group'
reflectance_grouped_avg_cerca <- reflectance_cerca_avg %>%
  dplyr::group_by(Wavelength_Group) %>%
  dplyr::summarise(across(starts_with("CLY"), mean, na.rm = TRUE))


# If you want to make sure the Wavelength_Group is the first column
reflectance_grouped_avg_cerca <- reflectance_grouped_avg_cerca %>%
  dplyr::select(Wavelength_Group, everything())


# CHECK
print(reflectance_grouped_avg_cerca)

# Transforming the reflectance_grouped_avg data frame
reflectance_grouped_avg_cerca <- as.data.frame(t(reflectance_grouped_avg_cerca))
colnames(reflectance_grouped_avg_cerca) <- reflectance_grouped_avg_cerca[1,]
reflectance_grouped_avg_cerca <- reflectance_grouped_avg_cerca[-1,]
str(reflectance_grouped_avg_cerca)
reflectance_grouped_avg_cerca$file <- rownames(reflectance_grouped_avg_cerca)


# Replace all hyphens (-) and periods (.) with underscores (_) in the 'file' column of both data frames
reflectance_grouped_avg_cerca$file <- gsub("[-.]", "_", reflectance_grouped_avg_cerca$file)
# change to lower case
reflectance_grouped_avg_cerca$file <- trimws(tolower(reflectance_grouped_avg_cerca$file))

# Change all the "." to "_" in row names in reflectance_grouped_avg
#rownames(reflectance_grouped_avg) <- gsub("[-.]", "_", rownames(reflectance_grouped_avg))

# Convert the df to numeric except file
reflectance_grouped_avg_cerca[, -ncol(reflectance_grouped_avg_cerca)] <- lapply(reflectance_grouped_avg_cerca[, -ncol(reflectance_grouped_avg_cerca)], as.numeric)

# Load the model
plsr_model <- readRDS("data/plsr_model_whole.rds")
optimal_components <- 7

# Predict
predicted_N_cerca <- as.data.frame(as.numeric(predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg_cerca)))
predicted_N_cerca$file <- rownames(reflectance_grouped_avg_cerca)
write.csv(predicted_N_cerca,"predicted_N_cerca.csv",row.names=F,quote = F)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
######## PREDICTING ALL THE NITROGEN VALUES
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load the reflectance
reflectance_grouped_avg <- readRDS("data/reflectance_all_grouped_avg.rds")

# Load the model
plsr_model <- readRDS("data/plsr_model_whole.rds")
optimal_components <- 7

# Predict
predicted_N_all <- as.data.frame(as.numeric(predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg)))




# Separate to CERCA and Zdip
desc <- read.csv("data/cerca_desc.csv")

