#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
######## COMPARE THE ZDIP LINES
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Get the data
zdip <- read.csv("results/predicted_N_all_zdip.csv", header = TRUE)

# get the B73 control
zdip_B73 <- zdip[which(zdip$genotype == "B73 x LH287"),]
str(zdip_B73)
table(zdip_B73$leaf)

# remove e1.-1 and e3.-1
zdip_B73 <- zdip_B73[which(zdip_B73$leaf != "e1.-1" & zdip_B73$leaf != "e3.-1"),]
table(zdip_B73$N_treatment)

# Subset high and medium N
zdip_B73_high <- zdip_B73[which(zdip_B73$N_treatment == "high" ),]
zdip_B73_med <- zdip_B73[which(zdip_B73$N_treatment == "med" ),]

# Subset just the control C, C1, C2 without the desinked
zdip_B73_high <- zdip_B73_high[which(zdip_B73_high$plant == "C" | zdip_B73_high$plant == "C1" | zdip_B73_high$plant == "C2"), ]
zdip_B73_med <- zdip_B73_med[which(zdip_B73_med$plant == "C" | zdip_B73_med$plant == "C1" | zdip_B73_med$plant == "C2"), ]

### Remaining Zdip lines
zdip_lines <- zdip[which(zdip$genotype != "B73 x LH287"),]
str(zdip_lines)

# remove e1.-1 and e3.-1
zdip_lines <- zdip_lines[which(zdip_lines$leaf != "e1.-1" & zdip_lines$leaf != "e1.1" & zdip_lines$leaf != "e3.-1" & zdip_lines$leaf != "e2.1"),]

# Subset high and medium N
zdip_lines_high <- zdip_lines[which(zdip_lines$N_treatment == "high" ),]
zdip_lines_med <- zdip_lines[which(zdip_lines$N_treatment == "med" ),]

# Subset just the control C, C1, C2 without the desinked
zdip_lines_high <- zdip_lines_high[which(zdip_lines_high$plant == "C" | zdip_lines_high$plant == "C1" | zdip_lines_high$plant == "C2"), ]
zdip_lines_med <- zdip_lines_med[which(zdip_lines_med$plant == "C" | zdip_lines_med$plant == "C1" | zdip_lines_med$plant == "C2"), ]


str(zdip_B73_med)
str(zdip_lines_med)
table(zdip_B73_med$leaf)
table(zdip_lines_med$genotype)


# Summarize zdip_lines_med to get the mean predicted_N for each genotype across each leaf stage
zdip_lines_med_summary <- zdip_lines_med %>%
  group_by(genotype, leaf) %>%
  summarise(
    mean_predicted_N = mean(predicted_N, na.rm = TRUE),
    .groups = 'drop'  # Remove grouping after summarising
  )

# Print the summarized data to check the output
print(zdip_lines_med_summary)

# Calculate the 95% confidence interval for B73 control across leaf stages
b73_summary <- zdip_B73_med %>%
  group_by(leaf) %>%
  summarise(
    mean_predicted_N = mean(predicted_N, na.rm = TRUE),
    lower_CI = mean_predicted_N - qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    upper_CI = mean_predicted_N + qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    .groups = 'drop'
  )

# Print the summary to check the mean and confidence intervals
print(b73_summary)

# Rename the mean_predicted_N column in b73_summary to avoid name conflicts
b73_summary <- b73_summary %>%
  rename(mean_predicted_N_B73 = mean_predicted_N)

# Identify genotypes whose mean predicted N values are completely outside the 95% confidence interval of B73
different_genotypes <- zdip_lines_med_summary %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI | mean_predicted_N > upper_CI) %>%
  dplyr::select(genotype, leaf, mean_predicted_N)

# Print the results
print(different_genotypes)
str(b73_summary)


# Merge different_genotypes with b73_summary to get the B73 mean for comparison
genotype_diff_summary <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::mutate(abs_diff = abs(mean_predicted_N - mean_predicted_N_B73)) %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(total_abs_diff = sum(abs_diff, na.rm = TRUE)) %>%
  dplyr::arrange(desc(total_abs_diff))

# Select the top 25 genotypes with the highest total absolute difference
top_10_genotypes <- genotype_diff_summary %>%
  dplyr::slice_head(n = 25)

# Print the top 10 genotypes
print(top_10_genotypes)

# Filter data for the top 10 genotypes to plot
top_10_genotypes_data <- zdip_lines_med_summary %>%
  dplyr::filter(genotype %in% top_10_genotypes$genotype)

# Create a color palette for the top 10 genotypes
#genotype_colors <- RColorBrewer::brewer.pal(n = 25, name = "Set3")
genotype_colors <- c(
  RColorBrewer::brewer.pal(n = 12, name = "Set3"),  # 12 colors from Set3
  RColorBrewer::brewer.pal(n = 8, name = "Dark2"),  # 8 colors from Dark2
  RColorBrewer::brewer.pal(n = 5, name = "Set1")    # 5 colors from Set1
)

# Modify the plot to include a legend entry for B73
b73_plot <- ggplot(b73_summary, aes(x = leaf, y = mean_predicted_N_B73, group = 1)) +
  geom_line(color = "blue", size = 1.2, aes(linetype = "B73xLH287")) +  # Mean line for B73 with legend entry
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = "B73xLH287"), alpha = 0.3) +  # Confidence interval band
  geom_point(color = "blue", size = 2) +  # Points for the mean
  geom_line(data = top_10_genotypes_data, aes(x = leaf, y = mean_predicted_N, group = genotype, color = genotype), size = 1) +  # Lines for each genotype
  geom_point(data = top_10_genotypes_data, aes(x = leaf, y = mean_predicted_N, color = genotype), size = 2) +  # Points for each genotype
  scale_color_manual(values = genotype_colors) +  # Custom colors for genotypes
  scale_fill_manual(name = "Control with CI", values = c("B73xLH287" = "lightblue")) +  # Color for the CI
  scale_linetype_manual(name = "Control with CI", values = c("B73xLH287" = "solid")) +  # Line type for the B73 mean line
  labs(
    title = "95% Confidence Interval for B73xLH287 with the Top 25 Genotypes Showing the Greatest Deviation under\n Medium N Treatment",
    x = "Leaf Stage",
    y = "Predicted N (%)",
    color = "Genotype"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right"
  )

# Print the plot
print(b73_plot)

# Save the plot as a PNG file
#ggsave("figures/Zdip_N_Control_med.png", plot = b73_plot, width = 14, height = 8, units = "in", dpi = 300, bg = "white")


# Filter genotypes that are above the upper confidence interval for all leaf stages
genotypes_above_CI <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::filter(mean_predicted_N > upper_CI) %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(count_above_CI = n()) %>%
  dplyr::filter(count_above_CI == 3)  # Only keep genotypes that are above CI for all 3 leaf stages

# Print the result
print(genotypes_above_CI)

# Filter genotypes that are below the lower confidence interval for all leaf stages
genotypes_below_CI <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI) %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(count_below_CI = n()) %>%
  dplyr::filter(count_below_CI == 3)  # Only keep genotypes that are below CI for all 3 leaf stages

# Print the result
print(genotypes_below_CI)


# Filter genotypes that have e1 and e2 above the upper CI and e3 below the lower CI
genotypes_mixed_CI <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::mutate(condition = case_when(
    (leaf == "e1" & mean_predicted_N > upper_CI) ~ "above",
    (leaf == "e2" & mean_predicted_N > upper_CI) ~ "above",
    (leaf == "e3" & mean_predicted_N < lower_CI) ~ "below",
    TRUE ~ "other"
  )) %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(
    e1_status = any(condition == "above" & leaf == "e1"),
    e2_status = any(condition == "above" & leaf == "e2"),
    e3_status = any(condition == "below" & leaf == "e3")
  ) %>%
  dplyr::filter(e1_status & e2_status & e3_status)  # Keep only genotypes meeting all conditions

# View the result
genotypes_mixed_CI


# Filter genotypes where e1 > e2 > e3
genotypes_decreasing_pattern <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::select(genotype, leaf, mean_predicted_N) %>%
  tidyr::spread(key = leaf, value = mean_predicted_N) %>%
  dplyr::filter(e1 > e2 & e2 > e3)  # Keep only genotypes with e1 > e2 > e3

# View the result
genotypes_decreasing_pattern


# Filter genotypes where e2 > e1 > e3
genotypes_decreasing_pattern <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::select(genotype, leaf, mean_predicted_N) %>%
  tidyr::spread(key = leaf, value = mean_predicted_N) %>%
  dplyr::filter(e2 > e1 & e1 > e3)  # Keep only genotypes with e1 > e2 > e3

# View the result
genotypes_decreasing_pattern


# Filter genotypes where e3 > e2 > e1
genotypes_increasing_pattern <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::select(genotype, leaf, mean_predicted_N) %>%
  tidyr::spread(key = leaf, value = mean_predicted_N) %>%
  dplyr::filter(e3 > e2 & e2 > e1)  # Keep only genotypes with e1 > e2 > e3

# View the result
genotypes_increasing_pattern


#### High 
# Summarize zdip_lines_high to get the mean predicted_N for each genotype across each leaf stage
zdip_lines_high_summary <- zdip_lines_high %>%
  group_by(genotype, leaf) %>%
  summarise(
    mean_predicted_N = mean(predicted_N, na.rm = TRUE),
    .groups = 'drop'  # Remove grouping after summarising
  )

# Print the summarized data to check the output
print(zdip_lines_high_summary)

# Calculate the 95% confidence interval for B73 control across leaf stages
b73_summary <- zdip_B73_high %>%
  group_by(leaf) %>%
  summarise(
    mean_predicted_N = mean(predicted_N, na.rm = TRUE),
    lower_CI = mean_predicted_N - qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    upper_CI = mean_predicted_N + qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    .groups = 'drop'
  )

# Print the summary to check the mean and confidence intervals
print(b73_summary)

# Rename the mean_predicted_N column in b73_summary to avoid name conflicts
b73_summary <- b73_summary %>%
  rename(mean_predicted_N_B73 = mean_predicted_N)

# Identify genotypes whose mean predicted N values are completely outside the 95% confidence interval of B73
different_genotypes <- zdip_lines_high_summary %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI | mean_predicted_N > upper_CI) %>%
  dplyr::select(genotype, leaf, mean_predicted_N)

# Print the results
print(different_genotypes)
str(b73_summary)


# Merge different_genotypes with b73_summary to get the B73 mean for comparison
genotype_diff_summary <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::mutate(abs_diff = abs(mean_predicted_N - mean_predicted_N_B73)) %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(total_abs_diff = sum(abs_diff, na.rm = TRUE)) %>%
  dplyr::arrange(desc(total_abs_diff))

# Select the top 25 genotypes with the highest total absolute difference
top_10_genotypes <- genotype_diff_summary %>%
  dplyr::slice_head(n = 25)
top_10_genotypes <- top_10_genotypes[-1,]

# Print the top 10 genotypes
print(top_10_genotypes)
top_10_genotypes$genotype
# Filter data for the top 10 genotypes to plot
top_10_genotypes_data <- zdip_lines_high_summary %>%
  dplyr::filter(genotype %in% top_10_genotypes$genotype)

# Create a color palette for the top 10 genotypes
#genotype_colors <- RColorBrewer::brewer.pal(n = 25, name = "Set3")
genotype_colors <- c(
  RColorBrewer::brewer.pal(n = 12, name = "Set3"),  # 12 colors from Set3
  RColorBrewer::brewer.pal(n = 8, name = "Dark2"),  # 8 colors from Dark2
  RColorBrewer::brewer.pal(n = 5, name = "Set1")    # 5 colors from Set1
)

# Modify the plot to include a legend entry for B73
b73_plot <- ggplot(b73_summary, aes(x = leaf, y = mean_predicted_N_B73, group = 1)) +
  geom_line(color = "blue", size = 1.2, aes(linetype = "B73xLH287")) +  # Mean line for B73 with legend entry
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = "B73xLH287"), alpha = 0.3) +  # Confidence interval band
  geom_point(color = "blue", size = 2) +  # Points for the mean
  geom_line(data = top_10_genotypes_data, aes(x = leaf, y = mean_predicted_N, group = genotype, color = genotype), size = 1) +  # Lines for each genotype
  geom_point(data = top_10_genotypes_data, aes(x = leaf, y = mean_predicted_N, color = genotype), size = 2) +  # Points for each genotype
  scale_color_manual(values = genotype_colors) +  # Custom colors for genotypes
  scale_fill_manual(name = "Control with CI", values = c("B73xLH287" = "lightblue")) +  # Color for the CI
  scale_linetype_manual(name = "Control with CI", values = c("B73xLH287" = "solid")) +  # Line type for the B73 mean line
  labs(
    title = "95% Confidence Interval for B73xLH287 with the Top 25 Genotypes Showing the Greatest Deviation under\n highium N Treatment",
    x = "Leaf Stage",
    y = "Predicted N (%)",
    color = "Genotype"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right"
  )

# Print the plot
print(b73_plot)

# Save the plot as a PNG file
#ggsave("figures/Zdip_N_Control_high.png", plot = b73_plot, width = 14, height = 8, units = "in", dpi = 300, bg = "white")


# Filter genotypes that are above the upper confidence interval for all leaf stages
genotypes_above_CI <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::filter(mean_predicted_N > upper_CI) %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(count_above_CI = n()) %>%
  dplyr::filter(count_above_CI == 3)  # Only keep genotypes that are above CI for all 3 leaf stages

# Print the result
print(genotypes_above_CI)

# Filter genotypes that are below the lower confidence interval for all leaf stages
genotypes_below_CI <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI) %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(count_below_CI = n()) %>%
  dplyr::filter(count_below_CI == 3)  # Only keep genotypes that are below CI for all 3 leaf stages

# Print the result
print(genotypes_below_CI)



# Filter genotypes that have e1 and e2 above the upper CI and e3 below the lower CI
genotypes_mixed_CI <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::mutate(condition = case_when(
    (leaf == "e1" & mean_predicted_N > upper_CI) ~ "above",
    (leaf == "e2" & mean_predicted_N > upper_CI) ~ "above",
    (leaf == "e3" & mean_predicted_N < lower_CI) ~ "below",
    TRUE ~ "other"
  )) %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(
    e1_status = any(condition == "above" & leaf == "e1"),
    e2_status = any(condition == "above" & leaf == "e2"),
    e3_status = any(condition == "below" & leaf == "e3")
  ) %>%
  dplyr::filter(e1_status & e2_status & e3_status)  # Keep only genotypes meeting all conditions

# View the result
genotypes_mixed_CI


# Filter genotypes where e1 > e2 > e3
genotypes_decreasing_pattern <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::select(genotype, leaf, mean_predicted_N) %>%
  tidyr::spread(key = leaf, value = mean_predicted_N) %>%
  dplyr::filter(e1 > e2 & e2 > e3)  # Keep only genotypes with e1 > e2 > e3

# View the result
genotypes_decreasing_pattern


# Filter genotypes where e2 > e1 > e3
genotypes_decreasing_pattern <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::select(genotype, leaf, mean_predicted_N) %>%
  tidyr::spread(key = leaf, value = mean_predicted_N) %>%
  dplyr::filter(e2 > e1 & e1 > e3)  # Keep only genotypes with e1 > e2 > e3

# View the result
genotypes_decreasing_pattern


# Filter genotypes where e3 > e2 > e1
genotypes_increasing_pattern <- different_genotypes %>%
  dplyr::inner_join(b73_summary, by = "leaf") %>%
  dplyr::select(genotype, leaf, mean_predicted_N) %>%
  tidyr::spread(key = leaf, value = mean_predicted_N) %>%
  dplyr::filter(e3 > e2 & e2 > e1)  # Keep only genotypes with e1 > e2 > e3

# View the result
genotypes_increasing_pattern

