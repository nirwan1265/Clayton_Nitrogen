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

predicted_N_all$sample_id <- rownames(reflectance_grouped_avg)
colnames(predicted_N_all)[1] <- "predicted_N"
predicted_N_all <- predicted_N_all[,c(2,1)]

# Save the predicted N values
#write.csv(predicted_N_all, "results/predicted_N_all.csv", row.names = FALSE)


# Response_N
response_N <- readRDS("data/response_N.rds")

# Separate to CERCA and Zdip
desc <- read.csv("data/All_spectral_info.csv")
zdip <- desc[desc$experiment == "Zdip",]

# Get CERCA sample names
reflectance_grouped_avg_cerca <- readRDS("data/reflectance_all_grouped_avg.rds")
cerca_samples <- rownames(reflectance_grouped_avg_cerca)
cerca_samples <- gsub("[-.]", "_", cerca_samples)
#cerca_samples <- trimws(tolower(cerca_samples))

# Get the predicted values for the CERCA samples from predicted_N_all
predicted_N_cerca <- predicted_N_all[predicted_N_all$sample_id %in% cerca_samples,]

# Get the remaining samples
predicted_N_remaining <- predicted_N_all[!predicted_N_all$sample_id %in% cerca_samples,]

# Make a histogram plot 
library(ggplot2)

# Define a function to create histograms with a unified style
plot_histogram <- function(data, title) {
  ggplot(data, aes(x = predicted_N)) +
    geom_histogram(bins = 50, fill = "lightblue", color = "black") +
    labs(
      title = title,
      x = "Predicted N values",
      y = "Frequency"
    ) +
    theme_minimal(base_size = 20) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}

write.csv(predicted_N_cerca,"predicted_N_cerca.csv",row.names = F, quote = F)

# Plot histogram for predicted_N_cerca
hist_cerca <- plot_histogram(predicted_N_cerca, "Histogram of Predicted N Values (Cerca)")


# Plot histogram for predicted_N_remaining
hist_remaining <- plot_histogram(predicted_N_remaining, "Histogram of Predicted N Values (Remaining)")

# Print the plots
print(hist_cerca)
print(hist_remaining)

# Save the plots
ggsave("figures/histogram_predicted_N_cerca.png", hist_cerca, width = 10, height = 6, dpi = 300, bg="white")
ggsave("figures/histogram_predicted_N_remaining.png", hist_remaining, width = 10, height = 6, dpi = 300,bg="white")






################################################################################
######## LOAD THE A2 REFLECTANCE DATA
################################################################################


# Load the Zdip reflectance file
GLR <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/GLR_reflectance.txt", sep =",", header = T) 
colnames(GLR)[-1] <- paste0("GLR_", colnames(GLR)[-1])
GLR[1:5,1:5]
# Removing the Wavelength column
NNR2 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/NNR2_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
colnames(NNR2) <- paste0("NNR2_", colnames(NNR2))

IDP8570 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/IDP8570_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
colnames(IDP8570) <- paste0("IDP8570_", colnames(IDP8570))

MN2 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/MN2_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
colnames(MN2) <- paste0("MN2_", colnames(MN2))

RP <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/RP_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
colnames(RP) <- paste0("RP_", colnames(RP))

SE1 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/SE1_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
colnames(SE1) <- paste0("SE1_", colnames(SE1))

SmD1 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/SmD1_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
colnames(SmD1) <- paste0("SmD1_", colnames(SmD1))

TIDP2933 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/TIDP2933_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
colnames(TIDP2933) <- paste0("TIDP2933_", colnames(TIDP2933))

UP1 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/UP1_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
colnames(UP1) <- paste0("UP1_", colnames(UP1))

B73 <- read.table("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/raw_reflectance/A2/B73_reflectance.txt", sep =",", header = T) %>% dplyr::select(-c(1))
colnames(B73) <- paste0("B73_", colnames(B73))


# Combine the Zdip files by column
A2_total <- cbind(GLR, NNR2, IDP8570, MN2,RP, SE1, SmD1, TIDP2933, UP1,B73)
A2_total[1:5,1:5]

# Remove _1.asd and _2.asd from the column names. Did 250 cause there was an error before
for (i in 1:10) {
  colnames(A2_total) <- gsub(paste0("_", i, ".asd"), "", colnames(A2_total))
}

# Initialize an empty list to store the averaged columns
A2_total_avg_list <- list()

# First, find the unique column names
unique_cols_A2 <- unique(names(A2_total))

# Loop through each unique column name
for (col in unique_cols_A2) {
  # Find all columns that match the current unique name
  matching_cols <- A2_total[, names(A2_total) == col, drop = FALSE]
  
  # If there is more than one column with the same name, average them
  if (ncol(matching_cols) > 1) {
    A2_total_avg_list[[col]] <- rowMeans(matching_cols, na.rm = TRUE)
  } else {
    # If there's only one column, just keep it as is
    A2_total_avg_list[[col]] <- matching_cols[[1]]
  }
}

# Convert the list back into a dataframe
A2_total_avg <- as.data.frame(A2_total_avg_list)

# Create a new column for grouping, including a case for the last value
max_wavelength_A2 <- max(A2_total_avg$Wavelength)

A2_total_avg$Group <- cut(A2_total_avg$Wavelength, 
                          breaks = c(seq(350, max_wavelength_A2, by = 10), max_wavelength_A2 + 1), 
                          include.lowest = TRUE, labels = FALSE)

# Create the labels for the first column as "350-359", "360-369", etc.
A2_total_avg$Wavelength_Group <- ifelse(
  A2_total_avg$Wavelength == max_wavelength_A2, 
  paste0(max_wavelength_A2, "-", max_wavelength_A2),  # Special case for the last value (e.g., 2500-2500)
  paste0(((A2_total_avg$Group - 1) * 10 + 350), "-", (A2_total_avg$Group * 10 + 349))
)

# Now, calculate the average for every 10 rows, grouped by the new 'Wavelength_Group'
# A2_total_grouped_avg <- A2_total_avg %>%
#   dplyr::group_by(Wavelength_Group) %>%
#   dplyr::summarise(across(starts_with("CLY"), mean, na.rm = TRUE))
A2_total_grouped_avg <- A2_total_avg %>%
  dplyr::group_by(Wavelength_Group) %>%
  dplyr::summarise(
    across(starts_with("NNR2_"), mean, na.rm = TRUE),
    across(starts_with("GLR_"), mean, na.rm = TRUE),
    across(starts_with("IDP8570_"), mean, na.rm = TRUE),
    across(starts_with("MN2_"), mean, na.rm = TRUE),
    across(starts_with("RP_"), mean, na.rm = TRUE),
    across(starts_with("SE1_"), mean, na.rm = TRUE),
    across(starts_with("SmD1_"), mean, na.rm = TRUE),
    across(starts_with("TIDP2933_"), mean, na.rm = TRUE),
    across(starts_with("UP1_"), mean, na.rm = TRUE),
    across(starts_with("B73_"), mean, na.rm = TRUE)
  )

# If you want to make sure the Wavelength_Group is the first column
A2_total_grouped_avg <- A2_total_grouped_avg %>%
  dplyr::select(Wavelength_Group, everything())

# Print to check the final grouped averages
print(A2_total_grouped_avg)

# Print the new averaged data
str(A2_total_grouped_avg)

# Transforming the reflectance_grouped_avg data frame
A2_total_grouped_avg <- as.data.frame(t(A2_total_grouped_avg))
colnames(A2_total_grouped_avg) <- A2_total_grouped_avg[1,]
A2_total_grouped_avg <- A2_total_grouped_avg[-1,]
str(A2_total_grouped_avg)
A2_total_grouped_avg$file <- rownames(A2_total_grouped_avg)

# Convert all the rows of the reflectance_grouped_avg to numeric
A2_total_grouped_avg[, -ncol(A2_total_grouped_avg)] <- sapply(A2_total_grouped_avg[, -ncol(A2_total_grouped_avg)], as.numeric)

# Load the model
plsr_model <- readRDS("data/plsr_model_whole.rds")
optimal_components <- 7
summary(plsr_model)

# Predict
predicted_N_A2 <- as.data.frame(as.numeric(predict(plsr_model, ncomp = optimal_components, newdata = A2_total_grouped_avg)))
predicted_N_A2$file <- rownames(A2_total_grouped_avg)
predicted_N_A2 <- predicted_N_A2[,c(2,1)]
colnames(predicted_N_A2)[2] <- "predicted_N"

# Subset the predicted N values for the A2 samples for each sample
# Extract the gene type from the 'file' column and create a new column for it
predicted_N_A2$gene_type <- sub("_.+", "", predicted_N_A2$file)

# Split the data frame by the 'gene_type' column into a list of data frames
predicted_N_list <- split(predicted_N_A2, predicted_N_A2$gene_type)

# Print the structure of the list to confirm
str(predicted_N_list)

# If you want to assign each split to a new variable, you can use:
list2env(predicted_N_list, envir = .GlobalEnv)


# Remove B73 entries from the list
predicted_N_list <- predicted_N_list[!names(predicted_N_list) %in% "B73"]

# Iterate over each element in the predicted_N_list
for (gene in names(predicted_N_list)) {
  # Extract the data frame for the current gene type
  gene_data <- predicted_N_list[[gene]]
  
  # Step 1: Remove the "<gene_type>_" prefix from the 'file' column
  gene_data$file <- gsub(paste0("^", gene, "_"), "", gene_data$file)
  
  # Step 2: Create a new column indicating if the entry is "Orig" or "Sis"
  gene_data <- gene_data %>%
    mutate(Orig_Sis = ifelse(grepl("_Orig$", file), "Orig", "Sis"))
  
  # Step 3: Remove "_Orig" and "_Sis" from the 'file' column
  gene_data$file <- gsub("(_Orig|_Sis)$", "", gene_data$file)
  
  # Step 4: Remove the gene type suffix at the end of the 'file' column
  gene_data$file <- gsub(paste0("_", gene, "$"), "", gene_data$file)
  
  # Step 5: Correctly identify if the stage is "VT" or "R4"
  gene_data <- gene_data %>%
    mutate(Stage = ifelse(grepl("_VT", file), "VT",
                          ifelse(grepl("_R4", file), "R4", NA)))
  
  # Assign the processed data frame back to the list
  predicted_N_list[[gene]] <- gene_data
}

# Check the result for one of the gene types
str(predicted_N_list[["GLR"]])


# Iterate over each element in the predicted_N_list to extract the sample number
for (gene in names(predicted_N_list)) {
  # Extract the data frame for the current gene type
  gene_data <- predicted_N_list[[gene]]
  
  # Extract the sample number using a regular expression
  gene_data$sample_number <- sub("^.*\\.([0-9]+)_(VT|R4)$", "\\1", gene_data$file)
  
  # Convert the sample number to numeric for proper sorting and analysis
  gene_data$sample_number <- as.numeric(gene_data$sample_number)
  
  # Assign the updated data frame back to the list
  predicted_N_list[[gene]] <- gene_data
}

# Check the result for one of the gene types
head(predicted_N_list[["SmD1"]])

# Iterate over each element in the predicted_N_list to order by sample_number
for (gene in names(predicted_N_list)) {
  # Extract the data frame for the current gene type
  gene_data <- predicted_N_list[[gene]]
  
  # Order the data frame by sample_number
  gene_data <- gene_data[order(gene_data$sample_number), ]
  
  # Assign the ordered data frame back to the list
  predicted_N_list[[gene]] <- gene_data
}

# Check the result for one of the gene types
head(predicted_N_list[["SmD1"]])

# Iterate over each element in predicted_N_list to separate data by Stage
for (gene in names(predicted_N_list)) {
  # Extract the data frame for the current gene type
  gene_data <- predicted_N_list[[gene]]
  
  # Split the data into VT and R4 subsets
  gene_data_VT <- subset(gene_data, Stage == "VT")
  gene_data_R4 <- subset(gene_data, Stage == "R4")
  
  # Assign the separated data frames back to the list as sublists
  predicted_N_list[[gene]] <- list(VT = gene_data_VT, R4 = gene_data_R4)
}

# Check the result for one of the gene types
str(predicted_N_list[["GLR"]])

library(ggplot2)

# Initialize an empty data frame to store results for the new pairs
sample_pair_table_new <- data.frame(Sample_pair = character(),
                                    predicted_N_diff = numeric(),
                                    gene = character(),
                                    stringsAsFactors = FALSE)

# Iterate over each gene type
for (gene in names(predicted_N_list)) {
  # Extract the data frame for the VT stage only
  if ("VT" %in% names(predicted_N_list[[gene]])) {
    gene_stage_data <- predicted_N_list[[gene]][["VT"]]
    
    # Sort by sample_number
    gene_stage_data <- gene_stage_data[order(gene_stage_data$sample_number), ]
    
    # Check if there are enough samples to create pairs
    if (nrow(gene_stage_data) > 1) {
      # Create sample pairs starting from the second sample
      for (i in 2:(nrow(gene_stage_data) - 1)) {
        if (gene_stage_data$sample_number[i + 1] == gene_stage_data$sample_number[i] + 1) {
          # Construct the sample pair
          sample_pair <- paste(gene_stage_data$sample_number[i], 
                               gene_stage_data$sample_number[i + 1], sep = "-")
          
          # Calculate the predicted_N difference
          predicted_N_diff <- gene_stage_data$predicted_N[i + 1] - gene_stage_data$predicted_N[i]
          
          # Append to the results table
          sample_pair_table_new <- rbind(sample_pair_table_new, 
                                         data.frame(Sample_pair = sample_pair,
                                                    predicted_N_diff = predicted_N_diff,
                                                    gene = gene))
        }
      }
    }
  }
}

# Display the table
head(sample_pair_table_new)
table(sample_pair_table_new$gene)

library(ggplot2)

library(ggplot2)
library(dplyr)

# Order the data frame by 'predicted_N_diff' in descending order within each gene
sample_pair_table_new <- sample_pair_table_new %>%
  group_by(gene) %>%
  arrange(desc(predicted_N_diff), .by_group = TRUE)

# Create the plot
quartz()
ggplot(sample_pair_table_new, aes(x = reorder(Sample_pair, -predicted_N_diff), y = predicted_N_diff, fill = gene)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "", y = "Predicted N Difference", title = "Predicted N Differences by Sample Pair Ordered by Value") +
  facet_wrap(~ gene, ncol = 3, scales = "free_x")




# Extract the data for SmD1 at the VT stage
VT_data <- predicted_N_list[["SE1"]][["VT"]]

# Separate the data into Orig and Sis
VT_Orig <- VT_data[VT_data$Orig_Sis == "Orig", ]
VT_Sis <- VT_data[VT_data$Orig_Sis == "Sis", ]

# Perform unpaired t-test
t_test_result <- t.test(VT_Orig$predicted_N, VT_Sis$predicted_N)

# Output the t-test results
print("Unpaired t-test results for SmD1 (VT Stage):")
print(t_test_result)


# Perform Wilcoxon rank-sum test
wilcox_test_result <- wilcox.test(VT_Orig$predicted_N, VT_Sis$predicted_N)

# Output the Wilcoxon test results
print("Wilcoxon rank-sum test results for SmD1 (VT Stage):")
print(wilcox_test_result)

