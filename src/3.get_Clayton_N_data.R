################################################################################
######## LOAD THE NITROGEN DATA
################################################################################

# Load the response variable
response_N <- vroom("/Users/nirwantandukar/Documents/Research/data/Nitrogen_measurement/element_analysis.csv")

# Remove all 0's from the true_data's column Nitrogen
response_N <- response_N[response_N$Nitrogen != 0,]
response_N <- response_N[response_N$Hydrogen != 0,]
response_N <- response_N[response_N$Carbon != 0,]
response_N <- response_N[response_N$Stage != "PM",]

# Convert all to "_" 
response_N$file <- gsub("[-.]", "_", response_N$file)
colnames(response_N)

# Average the duplicate values in the file column of response_N except the Genotype, Stage, High/low N columns
response_N <- response_N %>%
  dplyr::group_by(file) %>%
  dplyr::summarise(across(-c(Genotype, Stage, `High/low N`), mean, na.rm = TRUE), 
                   .groups = "drop")

unique(table(response_N$file))


str(reflectance_grouped_avg)

# Remove leading/trailing spaces and convert to lowercase
response_N$file <- trimws(tolower(response_N$file))

str(response_N)


# Save response_N to data as RDS
#saveRDS(response_N, file = "data/response_N.rds")


