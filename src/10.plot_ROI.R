# Load your reference GFF file - replace with the actual path
ref_GRanges <- rtracklayer::import("/Users/nirwantandukar/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/Maize/Maize.annotation/Zm-B73-REFERENCE-NAM-5.0_Zm00001eb.1.gff3")
# Filter ref_GRanges for only genes
genes_only <- ref_GRanges[mcols(ref_GRanges)$type == "gene"]
genes_only

# ROI for chr10 for Zdip med
chr2 <- c(1000000,	7000000)


# Load necessary library
library(GenomicRanges)

# Subset the genes from chromosome 10 within the specified range
genes_chr10_in_range <- subset(genes_only, seqnames == "chr2" & start >= 1000000 & end <= 7000000)

# Print the genes
genes_chr10_in_range

# Create a data frame with gene ID and range information
genes_chr10_df <- data.frame(
  gene = mcols(genes_chr10_in_range)$ID,
  range = paste(start(genes_chr10_in_range), end(genes_chr10_in_range), sep = "-")
)

# Print the data frame
print(genes_chr10_df)

write.csv(genes_chr10_df, file = "Zdip_N_ROI_chr2_med__below_CI_Control.csv", row.names = FALSE)
