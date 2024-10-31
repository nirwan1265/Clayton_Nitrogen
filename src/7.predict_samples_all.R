#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
######## PREDICTING ALL THE NITROGEN VALUES
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load the reflectance
reflectance_grouped_avg <- readRDS("data/reflectance_all_grouped_avg.rds")

# Load the model
plsr_model <- readRDS("data/")
optimal_components <- 7

# Predict
predicted_N_all <- as.data.frame(as.numeric(predict(plsr_model, ncomp = optimal_components, newdata = reflectance_grouped_avg)))

predicted_N_all$sample_id <- rownames(reflectance_grouped_avg)
colnames(predicted_N_all)[1] <- "predicted_N"
predicted_N_all <- predicted_N_all[,c(2,1)]


# Response_N
response_N <- readRDS("data/response_N.rds")

# Get CERCA sample names
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
