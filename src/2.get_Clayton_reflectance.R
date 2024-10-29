################################################################################
######## LOAD THE REFLECTANCE DATA
################################################################################

# Load the Zdip reflectance file
reflectance_zdip1 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/Zdip_7.14-28_reflectance.txt", sep =",", header = T)
reflectance_zdip1[1:5,1:5]
reflectance_zdip2 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/Zdip_6.11-18_set1_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1)) # Removing the Wavelength column
reflectance_zdip2[1:5,1:5]
reflectance_zdip3 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/Zdip_6.11-18_set2_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
reflectance_zdip4 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/Zdip_6.18-28_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))

# Combine the Zdip files by column
reflectance_zdip <- cbind(reflectance_zdip1, reflectance_zdip2, reflectance_zdip3, reflectance_zdip4)
reflectance_zdip[1:5,1:5]

# Load the CERCA reflectance file
reflectance_cerca <- vroom("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/CERCA_reflectance.txt")
reflectance_cerca[1:5,1:5]

# Remove alL columns with the partial string match to "CLY24-C8BC-27_PM_e2_p1_50.asd" from reflectance_cerca cause it had 250 repeats
reflectance_cerca <- reflectance_cerca %>% dplyr::select(-contains("CLY24-C8BC-27_PM_e2_p1"))


# Remove _1.asd and _2.asd from the column names. Did 250 cause there was an error before
for (i in 1:250) {
  colnames(reflectance_zdip) <- gsub(paste0("_", i, ".asd"), "", colnames(reflectance_zdip))
}

# Write the loop to remove _n.asd from the colnames. 1 to 250
for (i in 1:250) {
  colnames(reflectance_cerca) <- gsub(paste0("_", i, ".asd"), "", colnames(reflectance_cerca))
}

# Initialize an empty list to store the averaged columns
reflectance_zdip_avg_list <- list()
reflectance_cerca_avg_list <- list()

# First, find the unique column names
unique_cols <- unique(names(reflectance_zdip))
unique_cols_cerca <- unique(names(reflectance_cerca))

# SANITY CHECK
# Subset the rows that has CLY24.C8BC.7.14.24.151_VT_e1..1_pD column names in the reflectance_cerca data
x <- reflectance_zdip[ , grepl("CLY24.C8BC.7.14.24.132_VT_e3..1_pD1", colnames(reflectance_zdip))]
dim(x)

# Loop through each unique column name
for (col in unique_cols) {
  # Find all columns that match the current unique name
  matching_cols <- reflectance_zdip[, names(reflectance_zdip) == col, drop = FALSE]
  
  # If there is more than one column with the same name, average them
  if (ncol(matching_cols) > 1) {
    reflectance_zdip_avg_list[[col]] <- rowMeans(matching_cols, na.rm = TRUE)
  } else {
    # If there's only one column, just keep it as is
    reflectance_zdip_avg_list[[col]] <- matching_cols[[1]]
  }
}


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
reflectance_zdip_avg <- as.data.frame(reflectance_zdip_avg_list)
str(reflectance_zdip_avg)

reflectance_cerca_avg <- as.data.frame(reflectance_cerca_avg_list)
str(reflectance_cerca_avg)


# Create a new column for grouping, including a case for the last value
max_wavelength <- max(reflectance_zdip_avg$Wavelength)
max_wavelength_cerca <- max(reflectance_cerca_avg$Wavelength)

reflectance_zdip_avg$Group <- cut(reflectance_zdip_avg$Wavelength, 
                                  breaks = c(seq(350, max_wavelength, by = 10), max_wavelength + 1), 
                                  include.lowest = TRUE, labels = FALSE)

reflectance_cerca_avg$Group <- cut(reflectance_cerca_avg$Wavelength, 
                                   breaks = c(seq(350, max_wavelength, by = 10), max_wavelength + 1), 
                                   include.lowest = TRUE, labels = FALSE)

# Create the labels for the first column as "350-359", "360-369", etc.
reflectance_zdip_avg$Wavelength_Group <- ifelse(
  reflectance_zdip_avg$Wavelength == max_wavelength, 
  paste0(max_wavelength, "-", max_wavelength),  # Special case for the last value (e.g., 2500-2500)
  paste0(((reflectance_zdip_avg$Group - 1) * 10 + 350), "-", (reflectance_zdip_avg$Group * 10 + 349))
)

reflectance_cerca_avg$Wavelength_Group <- ifelse(
  reflectance_cerca_avg$Wavelength == max_wavelength, 
  paste0(max_wavelength, "-", max_wavelength),  # Special case for the last value (e.g., 2500-2500)
  paste0(((reflectance_cerca_avg$Group - 1) * 10 + 350), "-", (reflectance_cerca_avg$Group * 10 + 349))
)

# Now, calculate the average for every 10 rows, grouped by the new 'Wavelength_Group'
reflectance_grouped_avg <- reflectance_zdip_avg %>%
  dplyr::group_by(Wavelength_Group) %>%
  dplyr::summarise(across(starts_with("CLY"), mean, na.rm = TRUE))

reflectance_grouped_avg_cerca <- reflectance_cerca_avg %>%
  dplyr::group_by(Wavelength_Group) %>%
  dplyr::summarise(across(starts_with("CLY"), mean, na.rm = TRUE))


# If you want to make sure the Wavelength_Group is the first column
reflectance_grouped_avg <- reflectance_grouped_avg %>%
  dplyr::select(Wavelength_Group, everything())

reflectance_grouped_avg_cerca <- reflectance_grouped_avg_cerca %>%
  dplyr::select(Wavelength_Group, everything())


# Print to check the final grouped averages
print(reflectance_grouped_avg)

# Print the new averaged data
str(reflectance_grouped_avg)

# Transforming the reflectance_grouped_avg data frame
reflectance_grouped_avg <- as.data.frame(t(reflectance_grouped_avg))
colnames(reflectance_grouped_avg) <- reflectance_grouped_avg[1,]
reflectance_grouped_avg <- reflectance_grouped_avg[-1,]
str(reflectance_grouped_avg)
reflectance_grouped_avg$file <- rownames(reflectance_grouped_avg)


reflectance_grouped_avg_cerca <- as.data.frame(t(reflectance_grouped_avg_cerca))
colnames(reflectance_grouped_avg_cerca) <- reflectance_grouped_avg_cerca[1,]
reflectance_grouped_avg_cerca <- reflectance_grouped_avg_cerca[-1,]
str(reflectance_grouped_avg_cerca)
reflectance_grouped_avg_cerca$file <- rownames(reflectance_grouped_avg_cerca)

# Replace all hyphens (-) and periods (.) with underscores (_) in the 'file' column of both data frames
reflectance_grouped_avg$file <- gsub("[-.]", "_", reflectance_grouped_avg$file)
reflectance_grouped_avg_cerca$file <- gsub("[-.]", "_", reflectance_grouped_avg_cerca$file)

# Combine the two data frames
reflectance_grouped_avg <- rbind(reflectance_grouped_avg, reflectance_grouped_avg_cerca)

# Change all the "." to "_" in row names in reflectance_grouped_avg
rownames(reflectance_grouped_avg) <- gsub("[-.]", "_", rownames(reflectance_grouped_avg))

# change to lower case
reflectance_grouped_avg$file <- trimws(tolower(reflectance_grouped_avg$file))
str(reflectance_grouped_avg)

# Convert all the rows of the reflectance_grouped_avg to numeric
reflectance_grouped_avg[, -ncol(reflectance_grouped_avg)] <- sapply(reflectance_grouped_avg[, -ncol(reflectance_grouped_avg)], as.numeric)
str(reflectance_grouped_avg)
is.na(reflectance_grouped_avg)

# Save RDS of reflectance_grouped_avg to data
saveRDS(reflectance_grouped_avg, "/Users/nirwantandukar/Library/Mobile Documents/com~apple~CloudDocs/Github/Clayton_Nitrogen/data/reflectance_all_grouped_avg.rds")

