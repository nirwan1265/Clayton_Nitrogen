#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
######## ANALYZE THE NITROGEN DATA
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Load the predicted value
predicted_N_all <- read.csv("results/predicted_N_all.csv")

# Load the info file
info <- read.csv("data/All_spectral_info.csv")

# Remove _1.asd and _2.asd from the column names. Did 250 cause there was an error before
for (i in 1:10) {
  info$file <- gsub(paste0("_", i, ".asd"), "", info$file)
}

# Remove the duplicated values rows in the info file
info <- info[!duplicated(info$file), ]

# Replace all hyphens (-) and periods (.) with underscores (_) in the 'file' column of both data frames
info$file <- gsub("[-.]", "_", info$file)

# Merge the predicted values with the info file using dplyr::inner_join
predicted_N_all <- inner_join(predicted_N_all, info, by = c("sample_id" = "file"))

# Subset Zdip from experiment column
predicted_N_all_zdip <- predicted_N_all[predicted_N_all$experiment == "Zdip", ]

# Subset "CERCA modeling" from experiment column
predicted_N_all_modeling <- predicted_N_all[predicted_N_all$experiment != "Zdip", ]

# Subset med and high from N_treatment from predicted_N_all_zdip
predicted_N_all_zdip_med <- predicted_N_all_zdip[predicted_N_all_zdip$N_treatment %in% c("med"), ]
predicted_N_all_zdip_high <- predicted_N_all_zdip[predicted_N_all_zdip$N_treatment %in% c("high"), ]

# Subset med and high from N_treatment from predicted_N_all_modeling
predicted_N_all_modeling_med <- predicted_N_all_modeling[predicted_N_all_modeling$N_treatment %in% c("med"), ]
predicted_N_all_modeling_high <- predicted_N_all_modeling[predicted_N_all_modeling$N_treatment %in% c("high"), ]

# Plot the predicted values 
# Combine the data for the plot
combined_data <- bind_rows(
  predicted_N_all_zdip_med,
  predicted_N_all_zdip_high,
  predicted_N_all_modeling_med,
  predicted_N_all_modeling_high
)
str(combined_data)


# Ensure that `N_treatment` and `experiment` are factors with the correct order
combined_data$N_treatment <- factor(combined_data$N_treatment, levels = c("med", "high"))
combined_data$experiment <- factor(combined_data$experiment, levels = c("CERCA modeling", "Zdip"))

# Create the box plot with jitter
# ggplot(combined_data, aes(x = interaction(experiment, N_treatment, lex.order = TRUE), y = predicted_N, fill = N_treatment)) +
#   geom_boxplot(outlier.shape = NA) +  # Remove outliers
#   geom_jitter(color = "black", size = 1.5, width = 0.2) +
#   scale_fill_manual(values = c("med" = "#56B4E9", "high" = "#E69F00")) +
#   labs(
#     title = "Predicted N Values for Zdip and CERCA Modeling Under Med and High N Treatments",
#     x = "Condition",
#     y = "Predicted N"
#   ) +
#   theme_minimal(base_size = 20) +
#   theme(
#     panel.grid = element_blank(),
#     panel.background = element_rect(fill = "white"),
#     plot.title = element_text(hjust = 0.5),
#     legend.title = element_blank(),
#     legend.position = "top"
#   )


n_plot <- ggplot(combined_data, aes(x = interaction(experiment, N_treatment, lex.order = TRUE), y = predicted_N, fill = N_treatment)) +
  geom_half_boxplot(center = TRUE, errorbar.draw = FALSE, width = 0.4, nudge = 0.05) +
  geom_half_violin(side = "r", nudge = -0.05, alpha = 0.7) +
  scale_fill_manual(values = c("med" = "#56B4E9", "high" = "red")) +
  labs(
    title = "Predicted N Values for Zdip and CERCA Modeling Under Med and High N Treatments",
    x = "Condition",
    y = "Predicted N(%)"
  ) +
  theme_hc(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "top"
  )

# Save the plot
ggsave("figures/predicted_N_all_plot.png", n_plot, width = 14, height = 8, units = "in", dpi = 300, bg="white")
