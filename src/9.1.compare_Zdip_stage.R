#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
######## ANALYZE THE NITROGEN DATA
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Load the predicted value
predicted_N_zdip <- read.csv("results/predicted_N_all.csv")

# Load the info file
info <- read.csv("data/All_spectral_info.csv")
info <- info[complete.cases(info),]

# Remove _1.asd and _2.asd from the column names. Did 250 cause there was an error before
for (i in 1:10) {
  info$file <- gsub(paste0("_", i, ".asd"), "", info$file)
}

# Remove the duplicated values rows in the info file
info <- info[!duplicated(info$file), ]

# Replace all hyphens (-) and periods (.) with underscores (_) in the 'file' column of both data frames
info$file <- gsub("[-.]", "_", info$file)

# Merge the predicted values with the info file using dplyr::inner_join
predicted_N_zdip <- inner_join(predicted_N_zdip, info, by = c("sample_id" = "file"))
predicted_N_zdip <- predicted_N_zdip[,c(7,2,5,6,11,12,13)]

# Subset med and high from N_treatment from predicted_N_all_zdip
predicted_N_zdip_med <- predicted_N_zdip[predicted_N_zdip$N_treatment %in% c("med"), ]
predicted_N_zdip_high <- predicted_N_zdip[predicted_N_zdip$N_treatment %in% c("high"), ]

# separate VT, R1, and R4 from stage column for predicted_N_zdip_med and predicted_N_zdip_high
predicted_N_zdip_med_VT <- predicted_N_zdip_med[predicted_N_zdip_med$stage %in% c("VT"), ]
predicted_N_zdip_med_R1 <- predicted_N_zdip_med[predicted_N_zdip_med$stage %in% c("R1"), ]
predicted_N_zdip_med_R4 <- predicted_N_zdip_med[predicted_N_zdip_med$stage %in% c("R4"), ]

predicted_N_zdip_high_VT <- predicted_N_zdip_high[predicted_N_zdip_high$stage %in% c("VT"), ]
predicted_N_zdip_high_R1 <- predicted_N_zdip_high[predicted_N_zdip_high$stage %in% c("R1"), ]
predicted_N_zdip_high_R4 <- predicted_N_zdip_high[predicted_N_zdip_high$stage %in% c("R4"), ]

# separate leaf e1, e2 and e3 from leaf column for predicted_N_zdip_med_VT and predicted_N_zdip_high_VT
predicted_N_zdip_med_VT_e1 <- predicted_N_zdip_med_VT[predicted_N_zdip_med_VT$leaf %in% c("e1"), ]
predicted_N_zdip_med_VT_e2 <- predicted_N_zdip_med_VT[predicted_N_zdip_med_VT$leaf %in% c("e2"), ]
predicted_N_zdip_med_VT_e3 <- predicted_N_zdip_med_VT[predicted_N_zdip_med_VT$leaf %in% c("e3"), ]

predicted_N_zdip_high_VT_e1 <- predicted_N_zdip_high_VT[predicted_N_zdip_high_VT$leaf %in% c("e1"), ]
predicted_N_zdip_high_VT_e2 <- predicted_N_zdip_high_VT[predicted_N_zdip_high_VT$leaf %in% c("e2"), ]
predicted_N_zdip_high_VT_e3 <- predicted_N_zdip_high_VT[predicted_N_zdip_high_VT$leaf %in% c("e3"), ]

# separate leaf e1, e2 and e3 from leaf column for predicted_N_zdip_med_R1 and predicted_N_zdip_high_R1
predicted_N_zdip_med_R1_e1 <- predicted_N_zdip_med_R1[predicted_N_zdip_med_R1$leaf %in% c("e1"), ]
predicted_N_zdip_med_R1_e2 <- predicted_N_zdip_med_R1[predicted_N_zdip_med_R1$leaf %in% c("e2"), ]
predicted_N_zdip_med_R1_e3 <- predicted_N_zdip_med_R1[predicted_N_zdip_med_R1$leaf %in% c("e3"), ]

predicted_N_zdip_high_R1_e1 <- predicted_N_zdip_high_R1[predicted_N_zdip_high_R1$leaf %in% c("e1"), ]
predicted_N_zdip_high_R1_e2 <- predicted_N_zdip_high_R1[predicted_N_zdip_high_R1$leaf %in% c("e2"), ]
predicted_N_zdip_high_R1_e3 <- predicted_N_zdip_high_R1[predicted_N_zdip_high_R1$leaf %in% c("e3"), ]

# separate leaf e1, e2 and e3 from leaf column for predicted_N_zdip_med_R4 and predicted_N_zdip_high_R4
predicted_N_zdip_med_R4_e1 <- predicted_N_zdip_med_R4[predicted_N_zdip_med_R4$leaf %in% c("e1"), ]
predicted_N_zdip_med_R4_e2 <- predicted_N_zdip_med_R4[predicted_N_zdip_med_R4$leaf %in% c("e2"), ]
predicted_N_zdip_med_R4_e3 <- predicted_N_zdip_med_R4[predicted_N_zdip_med_R4$leaf %in% c("e3"), ]

predicted_N_zdip_high_R4_e1 <- predicted_N_zdip_high_R4[predicted_N_zdip_high_R4$leaf %in% c("e1"), ]
predicted_N_zdip_high_R4_e2 <- predicted_N_zdip_high_R4[predicted_N_zdip_high_R4$leaf %in% c("e2"), ]
predicted_N_zdip_high_R4_e3 <- predicted_N_zdip_high_R4[predicted_N_zdip_high_R4$leaf %in% c("e3"), ]

# Separate plant to C, C1, C2, BD, AD1, AD2, BD1, BD2, AD for predicted_N_zdip_med_VT_e1 and predicted_N_zdip_high_VT_e1
predicted_N_zdip_med_VT_e1_C <- predicted_N_zdip_med_VT_e1[predicted_N_zdip_med_VT_e1$plant %in% c("C"), ]
predicted_N_zdip_med_VT_e1_C1 <- predicted_N_zdip_med_VT_e1[predicted_N_zdip_med_VT_e1$plant %in% c("C1"), ]
predicted_N_zdip_med_VT_e1_C2 <- predicted_N_zdip_med_VT_e1[predicted_N_zdip_med_VT_e1$plant %in% c("C2"), ]
predicted_N_zdip_med_VT_e1_BD <- predicted_N_zdip_med_VT_e1[predicted_N_zdip_med_VT_e1$plant %in% c("BD"), ]
predicted_N_zdip_med_VT_e1_AD1 <- predicted_N_zdip_med_VT_e1[predicted_N_zdip_med_VT_e1$plant %in% c("AD1"), ]
predicted_N_zdip_med_VT_e1_AD2 <- predicted_N_zdip_med_VT_e1[predicted_N_zdip_med_VT_e1$plant %in% c("AD2"), ]
predicted_N_zdip_med_VT_e1_BD1 <- predicted_N_zdip_med_VT_e1[predicted_N_zdip_med_VT_e1$plant %in% c("BD1"), ]
predicted_N_zdip_med_VT_e1_BD2 <- predicted_N_zdip_med_VT_e1[predicted_N_zdip_med_VT_e1$plant %in% c("BD2"), ]
predicted_N_zdip_med_VT_e1_AD <- predicted_N_zdip_med_VT_e1[predicted_N_zdip_med_VT_e1$plant %in% c("AD"), ]

predicted_N_zdip_high_VT_e1_C <- predicted_N_zdip_high_VT_e1[predicted_N_zdip_high_VT_e1$plant %in% c("C"), ]
predicted_N_zdip_high_VT_e1_C1 <- predicted_N_zdip_high_VT_e1[predicted_N_zdip_high_VT_e1$plant %in% c("C1"), ]
predicted_N_zdip_high_VT_e1_C2 <- predicted_N_zdip_high_VT_e1[predicted_N_zdip_high_VT_e1$plant %in% c("C2"), ]
predicted_N_zdip_high_VT_e1_BD <- predicted_N_zdip_high_VT_e1[predicted_N_zdip_high_VT_e1$plant %in% c("BD"), ]
predicted_N_zdip_high_VT_e1_AD1 <- predicted_N_zdip_high_VT_e1[predicted_N_zdip_high_VT_e1$plant %in% c("AD1"), ]
predicted_N_zdip_high_VT_e1_AD2 <- predicted_N_zdip_high_VT_e1[predicted_N_zdip_high_VT_e1$plant %in% c("AD2"), ]
predicted_N_zdip_high_VT_e1_BD1 <- predicted_N_zdip_high_VT_e1[predicted_N_zdip_high_VT_e1$plant %in% c("BD1"), ]
predicted_N_zdip_high_VT_e1_BD2 <- predicted_N_zdip_high_VT_e1[predicted_N_zdip_high_VT_e1$plant %in% c("BD2"), ]
predicted_N_zdip_high_VT_e1_AD <- predicted_N_zdip_high_VT_e1[predicted_N_zdip_high_VT_e1$plant %in% c("AD"), ]

# Separate plant to C, C1,C2, BD, AD1, AD2, BD1, BD2, AD for predicted_N_zdip_med_VT_e2 and predicted_N_zdip_high_VT_e2
predicted_N_zdip_med_VT_e2_C <- predicted_N_zdip_med_VT_e2[predicted_N_zdip_med_VT_e2$plant %in% c("C"), ]
predicted_N_zdip_med_VT_e2_C1 <- predicted_N_zdip_med_VT_e2[predicted_N_zdip_med_VT_e2$plant %in% c("C1"), ]
predicted_N_zdip_med_VT_e2_C2 <- predicted_N_zdip_med_VT_e2[predicted_N_zdip_med_VT_e2$plant %in% c("C2"), ]
predicted_N_zdip_med_VT_e2_BD <- predicted_N_zdip_med_VT_e2[predicted_N_zdip_med_VT_e2$plant %in% c("BD"), ]
predicted_N_zdip_med_VT_e2_AD1 <- predicted_N_zdip_med_VT_e2[predicted_N_zdip_med_VT_e2$plant %in% c("AD1"), ]
predicted_N_zdip_med_VT_e2_AD2 <- predicted_N_zdip_med_VT_e2[predicted_N_zdip_med_VT_e2$plant %in% c("AD2"), ]
predicted_N_zdip_med_VT_e2_BD1 <- predicted_N_zdip_med_VT_e2[predicted_N_zdip_med_VT_e2$plant %in% c("BD1"), ]
predicted_N_zdip_med_VT_e2_BD2 <- predicted_N_zdip_med_VT_e2[predicted_N_zdip_med_VT_e2$plant %in% c("BD2"), ]
predicted_N_zdip_med_VT_e2_AD <- predicted_N_zdip_med_VT_e2[predicted_N_zdip_med_VT_e2$plant %in% c("AD"), ]

predicted_N_zdip_high_VT_e2_C <- predicted_N_zdip_high_VT_e2[predicted_N_zdip_high_VT_e2$plant %in% c("C"), ]
predicted_N_zdip_high_VT_e2_C1 <- predicted_N_zdip_high_VT_e2[predicted_N_zdip_high_VT_e2$plant %in% c("C1"), ]
predicted_N_zdip_high_VT_e2_C2 <- predicted_N_zdip_high_VT_e2[predicted_N_zdip_high_VT_e2$plant %in% c("C2"), ]
predicted_N_zdip_high_VT_e2_BD <- predicted_N_zdip_high_VT_e2[predicted_N_zdip_high_VT_e2$plant %in% c("BD"), ]
predicted_N_zdip_high_VT_e2_AD1 <- predicted_N_zdip_high_VT_e2[predicted_N_zdip_high_VT_e2$plant %in% c("AD1"), ]
predicted_N_zdip_high_VT_e2_AD2 <- predicted_N_zdip_high_VT_e2[predicted_N_zdip_high_VT_e2$plant %in% c("AD2"), ]
predicted_N_zdip_high_VT_e2_BD1 <- predicted_N_zdip_high_VT_e2[predicted_N_zdip_high_VT_e2$plant %in% c("BD1"), ]
predicted_N_zdip_high_VT_e2_BD2 <- predicted_N_zdip_high_VT_e2[predicted_N_zdip_high_VT_e2$plant %in% c("BD2"), ]
predicted_N_zdip_high_VT_e2_AD <- predicted_N_zdip_high_VT_e2[predicted_N_zdip_high_VT_e2$plant %in% c("AD"), ]


# Separate plant to C, C1,C2, BD, AD1, AD2, BD1, BD2, AD for predicted_N_zdip_med_VT_e3 and predicted_N_zdip_high_VT_e3
predicted_N_zdip_med_VT_e3_C <- predicted_N_zdip_med_VT_e3[predicted_N_zdip_med_VT_e3$plant %in% c("C"), ]
predicted_N_zdip_med_VT_e3_C1 <- predicted_N_zdip_med_VT_e3[predicted_N_zdip_med_VT_e3$plant %in% c("C1"), ]
predicted_N_zdip_med_VT_e3_C2 <- predicted_N_zdip_med_VT_e3[predicted_N_zdip_med_VT_e3$plant %in% c("C2"), ]
predicted_N_zdip_med_VT_e3_BD <- predicted_N_zdip_med_VT_e3[predicted_N_zdip_med_VT_e3$plant %in% c("BD"), ]
predicted_N_zdip_med_VT_e3_AD1 <- predicted_N_zdip_med_VT_e3[predicted_N_zdip_med_VT_e3$plant %in% c("AD1"), ]
predicted_N_zdip_med_VT_e3_AD2 <- predicted_N_zdip_med_VT_e3[predicted_N_zdip_med_VT_e3$plant %in% c("AD2"), ]
predicted_N_zdip_med_VT_e3_BD1 <- predicted_N_zdip_med_VT_e3[predicted_N_zdip_med_VT_e3$plant %in% c("BD1"), ]
predicted_N_zdip_med_VT_e3_BD2 <- predicted_N_zdip_med_VT_e3[predicted_N_zdip_med_VT_e3$plant %in% c("BD2"), ]
predicted_N_zdip_med_VT_e3_AD <- predicted_N_zdip_med_VT_e3[predicted_N_zdip_med_VT_e3$plant %in% c("AD"), ]


predicted_N_zdip_high_VT_e3_C <- predicted_N_zdip_high_VT_e3[predicted_N_zdip_high_VT_e3$plant %in% c("C"), ]
predicted_N_zdip_high_VT_e3_C1 <- predicted_N_zdip_high_VT_e3[predicted_N_zdip_high_VT_e3$plant %in% c("C1"), ]
predicted_N_zdip_high_VT_e3_C2 <- predicted_N_zdip_high_VT_e3[predicted_N_zdip_high_VT_e3$plant %in% c("C2"), ]
predicted_N_zdip_high_VT_e3_BD <- predicted_N_zdip_high_VT_e3[predicted_N_zdip_high_VT_e3$plant %in% c("BD"), ]
predicted_N_zdip_high_VT_e3_AD1 <- predicted_N_zdip_high_VT_e3[predicted_N_zdip_high_VT_e3$plant %in% c("AD1"), ]
predicted_N_zdip_high_VT_e3_AD2 <- predicted_N_zdip_high_VT_e3[predicted_N_zdip_high_VT_e3$plant %in% c("AD2"), ]
predicted_N_zdip_high_VT_e3_BD1 <- predicted_N_zdip_high_VT_e3[predicted_N_zdip_high_VT_e3$plant %in% c("BD1"), ]
predicted_N_zdip_high_VT_e3_BD2 <- predicted_N_zdip_high_VT_e3[predicted_N_zdip_high_VT_e3$plant %in% c("BD2"), ]
predicted_N_zdip_high_VT_e3_AD <- predicted_N_zdip_high_VT_e3[predicted_N_zdip_high_VT_e3$plant %in% c("AD"), ]


# Separate plant to C, C1, C2, BD, AD1, AD2, BD1, BD2, AD for predicted_N_zdip_med_R1_e1 and predicted_N_zdip_high_R1_e1
predicted_N_zdip_med_R1_e1_C <- predicted_N_zdip_med_R1_e1[predicted_N_zdip_med_R1_e1$plant %in% c("C"), ]
predicted_N_zdip_med_R1_e1_C1 <- predicted_N_zdip_med_R1_e1[predicted_N_zdip_med_R1_e1$plant %in% c("C1"), ]
predicted_N_zdip_med_R1_e1_C2 <- predicted_N_zdip_med_R1_e1[predicted_N_zdip_med_R1_e1$plant %in% c("C2"), ]
predicted_N_zdip_med_R1_e1_BD <- predicted_N_zdip_med_R1_e1[predicted_N_zdip_med_R1_e1$plant %in% c("BD"), ]
predicted_N_zdip_med_R1_e1_AD1 <- predicted_N_zdip_med_R1_e1[predicted_N_zdip_med_R1_e1$plant %in% c("AD1"), ]
predicted_N_zdip_med_R1_e1_AD2 <- predicted_N_zdip_med_R1_e1[predicted_N_zdip_med_R1_e1$plant %in% c("AD2"), ]
predicted_N_zdip_med_R1_e1_BD1 <- predicted_N_zdip_med_R1_e1[predicted_N_zdip_med_R1_e1$plant %in% c("BD1"), ]
predicted_N_zdip_med_R1_e1_BD2 <- predicted_N_zdip_med_R1_e1[predicted_N_zdip_med_R1_e1$plant %in% c("BD2"), ]
predicted_N_zdip_med_R1_e1_AD <- predicted_N_zdip_med_R1_e1[predicted_N_zdip_med_R1_e1$plant %in% c("AD"), ]

predicted_N_zdip_high_R1_e1_C <- predicted_N_zdip_high_R1_e1[predicted_N_zdip_high_R1_e1$plant %in% c("C"), ]
predicted_N_zdip_high_R1_e1_C1 <- predicted_N_zdip_high_R1_e1[predicted_N_zdip_high_R1_e1$plant %in% c("C1"), ]
predicted_N_zdip_high_R1_e1_C2 <- predicted_N_zdip_high_R1_e1[predicted_N_zdip_high_R1_e1$plant %in% c("C2"), ]
predicted_N_zdip_high_R1_e1_BD <- predicted_N_zdip_high_R1_e1[predicted_N_zdip_high_R1_e1$plant %in% c("BD"), ]
predicted_N_zdip_high_R1_e1_AD1 <- predicted_N_zdip_high_R1_e1[predicted_N_zdip_high_R1_e1$plant %in% c("AD1"), ]
predicted_N_zdip_high_R1_e1_AD2 <- predicted_N_zdip_high_R1_e1[predicted_N_zdip_high_R1_e1$plant %in% c("AD2"), ]
predicted_N_zdip_high_R1_e1_BD1 <- predicted_N_zdip_high_R1_e1[predicted_N_zdip_high_R1_e1$plant %in% c("BD1"), ]
predicted_N_zdip_high_R1_e1_BD2 <- predicted_N_zdip_high_R1_e1[predicted_N_zdip_high_R1_e1$plant %in% c("BD2"), ]
predicted_N_zdip_high_R1_e1_AD <- predicted_N_zdip_high_R1_e1[predicted_N_zdip_high_R1_e1$plant %in% c("AD"), ]

# Separate plant to C,C1, C2, BD, AD1, AD2, BD1, BD2,AD for predicted_N_zdip_med_R1_e2 and predicted_N_zdip_high_R1_e2
predicted_N_zdip_med_R1_e2_C <- predicted_N_zdip_med_R1_e2[predicted_N_zdip_med_R1_e2$plant %in% c("C"), ]
predicted_N_zdip_med_R1_e2_C1 <- predicted_N_zdip_med_R1_e2[predicted_N_zdip_med_R1_e2$plant %in% c("C1"), ]
predicted_N_zdip_med_R1_e2_C2 <- predicted_N_zdip_med_R1_e2[predicted_N_zdip_med_R1_e2$plant %in% c("C2"), ]
predicted_N_zdip_med_R1_e2_BD <- predicted_N_zdip_med_R1_e2[predicted_N_zdip_med_R1_e2$plant %in% c("BD"), ]
predicted_N_zdip_med_R1_e2_AD1 <- predicted_N_zdip_med_R1_e2[predicted_N_zdip_med_R1_e2$plant %in% c("AD1"), ]
predicted_N_zdip_med_R1_e2_AD2 <- predicted_N_zdip_med_R1_e2[predicted_N_zdip_med_R1_e2$plant %in% c("AD2"), ]
predicted_N_zdip_med_R1_e2_BD1 <- predicted_N_zdip_med_R1_e2[predicted_N_zdip_med_R1_e2$plant %in% c("BD1"), ]
predicted_N_zdip_med_R1_e2_BD2 <- predicted_N_zdip_med_R1_e2[predicted_N_zdip_med_R1_e2$plant %in% c("BD2"), ]
predicted_N_zdip_med_R1_e2_AD <- predicted_N_zdip_med_R1_e2[predicted_N_zdip_med_R1_e2$plant %in% c("AD"), ]

predicted_N_zdip_high_R1_e2_C <- predicted_N_zdip_high_R1_e2[predicted_N_zdip_high_R1_e2$plant %in% c("C"), ]
predicted_N_zdip_high_R1_e2_C1 <- predicted_N_zdip_high_R1_e2[predicted_N_zdip_high_R1_e2$plant %in% c("C1"), ]
predicted_N_zdip_high_R1_e2_C2 <- predicted_N_zdip_high_R1_e2[predicted_N_zdip_high_R1_e2$plant %in% c("C2"), ]
predicted_N_zdip_high_R1_e2_BD <- predicted_N_zdip_high_R1_e2[predicted_N_zdip_high_R1_e2$plant %in% c("BD"), ]
predicted_N_zdip_high_R1_e2_AD1 <- predicted_N_zdip_high_R1_e2[predicted_N_zdip_high_R1_e2$plant %in% c("AD1"), ]
predicted_N_zdip_high_R1_e2_AD2 <- predicted_N_zdip_high_R1_e2[predicted_N_zdip_high_R1_e2$plant %in% c("AD2"), ]
predicted_N_zdip_high_R1_e2_BD1 <- predicted_N_zdip_high_R1_e2[predicted_N_zdip_high_R1_e2$plant %in% c("BD1"), ]
predicted_N_zdip_high_R1_e2_BD2 <- predicted_N_zdip_high_R1_e2[predicted_N_zdip_high_R1_e2$plant %in% c("BD2"), ]
predicted_N_zdip_high_R1_e2_AD <- predicted_N_zdip_high_R1_e2[predicted_N_zdip_high_R1_e2$plant %in% c("AD"), ]

# Separate plant to C, BD, AD1, AD2, BD1, BD2, AD for predicted_N_zdip_med_R1_e3 and predicted_N_zdip_med_R1_e3
predicted_N_zdip_med_R1_e3_C <- predicted_N_zdip_med_R1_e3[predicted_N_zdip_med_R1_e3$plant %in% c("C"), ]
predicted_N_zdip_med_R1_e3_C1 <- predicted_N_zdip_med_R1_e3[predicted_N_zdip_med_R1_e3$plant %in% c("C1"), ]
predicted_N_zdip_med_R1_e3_C2 <- predicted_N_zdip_med_R1_e3[predicted_N_zdip_med_R1_e3$plant %in% c("C2"), ]
predicted_N_zdip_med_R1_e3_BD <- predicted_N_zdip_med_R1_e3[predicted_N_zdip_med_R1_e3$plant %in% c("BD"), ]
predicted_N_zdip_med_R1_e3_AD1 <- predicted_N_zdip_med_R1_e3[predicted_N_zdip_med_R1_e3$plant %in% c("AD1"), ]
predicted_N_zdip_med_R1_e3_AD2 <- predicted_N_zdip_med_R1_e3[predicted_N_zdip_med_R1_e3$plant %in% c("AD2"), ]
predicted_N_zdip_med_R1_e3_BD1 <- predicted_N_zdip_med_R1_e3[predicted_N_zdip_med_R1_e3$plant %in% c("BD1"), ]
predicted_N_zdip_med_R1_e3_BD2 <- predicted_N_zdip_med_R1_e3[predicted_N_zdip_med_R1_e3$plant %in% c("BD2"), ]
predicted_N_zdip_med_R1_e3_AD <- predicted_N_zdip_med_R1_e3[predicted_N_zdip_med_R1_e3$plant %in% c("AD"), ]

predicted_N_zdip_high_R1_e3_C <- predicted_N_zdip_high_R1_e3[predicted_N_zdip_high_R1_e3$plant %in% c("C"), ]
predicted_N_zdip_high_R1_e3_C1 <- predicted_N_zdip_high_R1_e3[predicted_N_zdip_high_R1_e3$plant %in% c("C1"), ]
predicted_N_zdip_high_R1_e3_C2 <- predicted_N_zdip_high_R1_e3[predicted_N_zdip_high_R1_e3$plant %in% c("C2"), ]
predicted_N_zdip_high_R1_e3_BD <- predicted_N_zdip_high_R1_e3[predicted_N_zdip_high_R1_e3$plant %in% c("BD"), ]
predicted_N_zdip_high_R1_e3_AD1 <- predicted_N_zdip_high_R1_e3[predicted_N_zdip_high_R1_e3$plant %in% c("AD1"), ]
predicted_N_zdip_high_R1_e3_AD2 <- predicted_N_zdip_high_R1_e3[predicted_N_zdip_high_R1_e3$plant %in% c("AD2"), ]
predicted_N_zdip_high_R1_e3_BD1 <- predicted_N_zdip_high_R1_e3[predicted_N_zdip_high_R1_e3$plant %in% c("BD1"), ]
predicted_N_zdip_high_R1_e3_BD2 <- predicted_N_zdip_high_R1_e3[predicted_N_zdip_high_R1_e3$plant %in% c("BD2"), ]
predicted_N_zdip_high_R1_e3_AD <- predicted_N_zdip_high_R1_e3[predicted_N_zdip_high_R1_e3$plant %in% c("AD"), ]

# Separate plant to C, BD, AD1, AD2, BD1, BD2,AD for predicted_N_zdip_med_R4_e1 and predicted_N_zdip_high_R4_e1
predicted_N_zdip_med_R4_e1_C <- predicted_N_zdip_med_R4_e1[predicted_N_zdip_med_R4_e1$plant %in% c("C"), ]
predicted_N_zdip_med_R4_e1_C1 <- predicted_N_zdip_med_R4_e1[predicted_N_zdip_med_R4_e1$plant %in% c("C1"), ]
predicted_N_zdip_med_R4_e1_C2 <- predicted_N_zdip_med_R4_e1[predicted_N_zdip_med_R4_e1$plant %in% c("C2"), ]
predicted_N_zdip_med_R4_e1_BD <- predicted_N_zdip_med_R4_e1[predicted_N_zdip_med_R4_e1$plant %in% c("BD"), ]
predicted_N_zdip_med_R4_e1_AD1 <- predicted_N_zdip_med_R4_e1[predicted_N_zdip_med_R4_e1$plant %in% c("AD1"), ]
predicted_N_zdip_med_R4_e1_AD2 <- predicted_N_zdip_med_R4_e1[predicted_N_zdip_med_R4_e1$plant %in% c("AD2"), ]
predicted_N_zdip_med_R4_e1_BD1 <- predicted_N_zdip_med_R4_e1[predicted_N_zdip_med_R4_e1$plant %in% c("BD1"), ]
predicted_N_zdip_med_R4_e1_BD2 <- predicted_N_zdip_med_R4_e1[predicted_N_zdip_med_R4_e1$plant %in% c("BD2"), ]
predicted_N_zdip_med_R4_e1_AD <- predicted_N_zdip_med_R4_e1[predicted_N_zdip_med_R4_e1$plant %in% c("AD"), ]


predicted_N_zdip_high_R4_e1_C <- predicted_N_zdip_high_R4_e1[predicted_N_zdip_high_R4_e1$plant %in% c("C"), ]
predicted_N_zdip_high_R4_e1_C1 <- predicted_N_zdip_high_R4_e1[predicted_N_zdip_high_R4_e1$plant %in% c("C1"), ]
predicted_N_zdip_high_R4_e1_C2 <- predicted_N_zdip_high_R4_e1[predicted_N_zdip_high_R4_e1$plant %in% c("C2"), ]
predicted_N_zdip_high_R4_e1_BD <- predicted_N_zdip_high_R4_e1[predicted_N_zdip_high_R4_e1$plant %in% c("BD"), ]
predicted_N_zdip_high_R4_e1_AD1 <- predicted_N_zdip_high_R4_e1[predicted_N_zdip_high_R4_e1$plant %in% c("AD1"), ]
predicted_N_zdip_high_R4_e1_AD2 <- predicted_N_zdip_high_R4_e1[predicted_N_zdip_high_R4_e1$plant %in% c("AD2"), ]
predicted_N_zdip_high_R4_e1_BD1 <- predicted_N_zdip_high_R4_e1[predicted_N_zdip_high_R4_e1$plant %in% c("BD1"), ]
predicted_N_zdip_high_R4_e1_BD2 <- predicted_N_zdip_high_R4_e1[predicted_N_zdip_high_R4_e1$plant %in% c("BD2"), ]
predicted_N_zdip_high_R4_e1_AD <- predicted_N_zdip_high_R4_e1[predicted_N_zdip_high_R4_e1$plant %in% c("AD"), ]


# Separate plant to C, BD, AD1, AD2, BD1, BD2, AD for predicted_N_zdip_med_R4_e2 and predicted_N_zdip_high_R4_e2
predicted_N_zdip_med_R4_e2_C <- predicted_N_zdip_med_R4_e2[predicted_N_zdip_med_R4_e2$plant %in% c("C"), ]
predicted_N_zdip_med_R4_e2_C1 <- predicted_N_zdip_med_R4_e2[predicted_N_zdip_med_R4_e2$plant %in% c("C1"), ]
predicted_N_zdip_med_R4_e2_C2 <- predicted_N_zdip_med_R4_e2[predicted_N_zdip_med_R4_e2$plant %in% c("C2"), ]
predicted_N_zdip_med_R4_e2_BD <- predicted_N_zdip_med_R4_e2[predicted_N_zdip_med_R4_e2$plant %in% c("BD"), ]
predicted_N_zdip_med_R4_e2_AD1 <- predicted_N_zdip_med_R4_e2[predicted_N_zdip_med_R4_e2$plant %in% c("AD1"), ]
predicted_N_zdip_med_R4_e2_AD2 <- predicted_N_zdip_med_R4_e2[predicted_N_zdip_med_R4_e2$plant %in% c("AD2"), ]
predicted_N_zdip_med_R4_e2_BD1 <- predicted_N_zdip_med_R4_e2[predicted_N_zdip_med_R4_e2$plant %in% c("BD1"), ]
predicted_N_zdip_med_R4_e2_BD2 <- predicted_N_zdip_med_R4_e2[predicted_N_zdip_med_R4_e2$plant %in% c("BD2"), ]
predicted_N_zdip_med_R4_e2_AD <- predicted_N_zdip_med_R4_e2[predicted_N_zdip_med_R4_e2$plant %in% c("AD"), ]

predicted_N_zdip_high_R4_e2_C <- predicted_N_zdip_high_R4_e2[predicted_N_zdip_high_R4_e2$plant %in% c("C"), ]
predicted_N_zdip_high_R4_e2_C1 <- predicted_N_zdip_high_R4_e2[predicted_N_zdip_high_R4_e2$plant %in% c("C1"), ]
predicted_N_zdip_high_R4_e2_C2 <- predicted_N_zdip_high_R4_e2[predicted_N_zdip_high_R4_e2$plant %in% c("C2"), ]
predicted_N_zdip_high_R4_e2_BD <- predicted_N_zdip_high_R4_e2[predicted_N_zdip_high_R4_e2$plant %in% c("BD"), ]
predicted_N_zdip_high_R4_e2_AD1 <- predicted_N_zdip_high_R4_e2[predicted_N_zdip_high_R4_e2$plant %in% c("AD1"), ]
predicted_N_zdip_high_R4_e2_AD2 <- predicted_N_zdip_high_R4_e2[predicted_N_zdip_high_R4_e2$plant %in% c("AD2"), ]
predicted_N_zdip_high_R4_e2_BD1 <- predicted_N_zdip_high_R4_e2[predicted_N_zdip_high_R4_e2$plant %in% c("BD1"), ]
predicted_N_zdip_high_R4_e2_BD2 <- predicted_N_zdip_high_R4_e2[predicted_N_zdip_high_R4_e2$plant %in% c("BD2"), ]
predicted_N_zdip_high_R4_e2_AD <- predicted_N_zdip_high_R4_e2[predicted_N_zdip_high_R4_e2$plant %in% c("AD"), ]

# Separate plant to C, BD, AD1, AD2, BD1, BD2, AD for predicted_N_zdip_med_R4_e3 and predicted_N_zdip_high_R4_e3
predicted_N_zdip_med_R4_e3_C <- predicted_N_zdip_med_R4_e3[predicted_N_zdip_med_R4_e3$plant %in% c("C"), ]
predicted_N_zdip_med_R4_e3_C1 <- predicted_N_zdip_med_R4_e3[predicted_N_zdip_med_R4_e3$plant %in% c("C1"), ]
predicted_N_zdip_med_R4_e3_C2 <- predicted_N_zdip_med_R4_e3[predicted_N_zdip_med_R4_e3$plant %in% c("C2"), ]
predicted_N_zdip_med_R4_e3_BD <- predicted_N_zdip_med_R4_e3[predicted_N_zdip_med_R4_e3$plant %in% c("BD"), ]
predicted_N_zdip_med_R4_e3_AD1 <- predicted_N_zdip_med_R4_e3[predicted_N_zdip_med_R4_e3$plant %in% c("AD1"), ]
predicted_N_zdip_med_R4_e3_AD2 <- predicted_N_zdip_med_R4_e3[predicted_N_zdip_med_R4_e3$plant %in% c("AD2"), ]
predicted_N_zdip_med_R4_e3_BD1 <- predicted_N_zdip_med_R4_e3[predicted_N_zdip_med_R4_e3$plant %in% c("BD1"), ]
predicted_N_zdip_med_R4_e3_BD2 <- predicted_N_zdip_med_R4_e3[predicted_N_zdip_med_R4_e3$plant %in% c("BD2"), ]
predicted_N_zdip_med_R4_e3_AD <- predicted_N_zdip_med_R4_e3[predicted_N_zdip_med_R4_e3$plant %in% c("AD"), ]


predicted_N_zdip_high_R4_e3_C <- predicted_N_zdip_high_R4_e3[predicted_N_zdip_high_R4_e3$plant %in% c("C"), ]
predicted_N_zdip_high_R4_e3_C1 <- predicted_N_zdip_high_R4_e3[predicted_N_zdip_high_R4_e3$plant %in% c("C1"), ]
predicted_N_zdip_high_R4_e3_C2 <- predicted_N_zdip_high_R4_e3[predicted_N_zdip_high_R4_e3$plant %in% c("C2"), ]
predicted_N_zdip_high_R4_e3_BD <- predicted_N_zdip_high_R4_e3[predicted_N_zdip_high_R4_e3$plant %in% c("BD"), ]
predicted_N_zdip_high_R4_e3_AD1 <- predicted_N_zdip_high_R4_e3[predicted_N_zdip_high_R4_e3$plant %in% c("AD1"), ]
predicted_N_zdip_high_R4_e3_AD2 <- predicted_N_zdip_high_R4_e3[predicted_N_zdip_high_R4_e3$plant %in% c("AD2"), ]
predicted_N_zdip_high_R4_e3_BD1 <- predicted_N_zdip_high_R4_e3[predicted_N_zdip_high_R4_e3$plant %in% c("BD1"), ]
predicted_N_zdip_high_R4_e3_BD2 <- predicted_N_zdip_high_R4_e3[predicted_N_zdip_high_R4_e3$plant %in% c("BD2"), ]
predicted_N_zdip_high_R4_e3_AD <- predicted_N_zdip_high_R4_e3[predicted_N_zdip_high_R4_e3$plant %in% c("AD"), ]


# Combine all the dataframes for AD for med N
all_control_med_VT <- rbind(predicted_N_zdip_med_VT_e1_C,
                            predicted_N_zdip_med_VT_e1_C1,
                            predicted_N_zdip_med_VT_e1_C2,
                            predicted_N_zdip_med_VT_e1_BD,
                            predicted_N_zdip_med_VT_e1_BD1,
                            predicted_N_zdip_med_VT_e1_BD2,
                            predicted_N_zdip_med_VT_e2_C,
                            predicted_N_zdip_med_VT_e2_C1,
                            predicted_N_zdip_med_VT_e2_C2,
                            predicted_N_zdip_med_VT_e2_BD,
                            predicted_N_zdip_med_VT_e2_BD1,
                            predicted_N_zdip_med_VT_e2_BD2,
                            predicted_N_zdip_med_VT_e3_C,
                            predicted_N_zdip_med_VT_e3_C1,
                            predicted_N_zdip_med_VT_e3_C2,
                            predicted_N_zdip_med_VT_e3_BD,
                            predicted_N_zdip_med_VT_e3_BD1,
                            predicted_N_zdip_med_VT_e3_BD2)

all_control_med_R1 <- rbind(predicted_N_zdip_med_R1_e1_C,
                            predicted_N_zdip_med_R1_e1_C1,
                            predicted_N_zdip_med_R1_e1_C2,
                            predicted_N_zdip_med_R1_e1_BD,
                            predicted_N_zdip_med_R1_e1_BD1,
                            predicted_N_zdip_med_R1_e1_BD2,
                            predicted_N_zdip_med_R1_e2_C,
                            predicted_N_zdip_med_R1_e2_C1,
                            predicted_N_zdip_med_R1_e2_C2,
                            predicted_N_zdip_med_R1_e2_BD,
                            predicted_N_zdip_med_R1_e2_BD1,
                            predicted_N_zdip_med_R1_e2_BD2,
                            predicted_N_zdip_med_R1_e3_C,
                            predicted_N_zdip_med_R1_e3_C1,
                            predicted_N_zdip_med_R1_e3_C2,
                            predicted_N_zdip_med_R1_e3_BD,
                            predicted_N_zdip_med_R1_e3_BD1,
                            predicted_N_zdip_med_R1_e3_BD2)

all_control_med_R4 <- rbind(predicted_N_zdip_med_R4_e1_C,
                            predicted_N_zdip_med_R4_e1_C1,
                            predicted_N_zdip_med_R4_e1_C2,
                            predicted_N_zdip_med_R4_e1_BD,
                            predicted_N_zdip_med_R4_e1_BD1,
                            predicted_N_zdip_med_R4_e1_BD2,
                            predicted_N_zdip_med_R4_e2_C,
                            predicted_N_zdip_med_R4_e2_C1,
                            predicted_N_zdip_med_R4_e2_C2,
                            predicted_N_zdip_med_R4_e2_BD,
                            predicted_N_zdip_med_R4_e2_BD1,
                            predicted_N_zdip_med_R4_e2_BD2,
                            predicted_N_zdip_med_R4_e3_C,
                            predicted_N_zdip_med_R4_e3_C1,
                            predicted_N_zdip_med_R4_e3_C2,
                            predicted_N_zdip_med_R4_e3_BD,
                            predicted_N_zdip_med_R4_e3_BD1,
                            predicted_N_zdip_med_R4_e3_BD2)

all_control_high_VT <- rbind(predicted_N_zdip_high_VT_e1_C,
                             predicted_N_zdip_high_VT_e1_C1,
                             predicted_N_zdip_high_VT_e1_C2,
                             predicted_N_zdip_high_VT_e1_BD,
                             predicted_N_zdip_high_VT_e1_BD1,
                             predicted_N_zdip_high_VT_e1_BD2,
                             predicted_N_zdip_high_VT_e2_C,
                             predicted_N_zdip_high_VT_e2_C1,
                             predicted_N_zdip_high_VT_e2_C2,
                             predicted_N_zdip_high_VT_e2_BD,
                             predicted_N_zdip_high_VT_e2_BD1,
                             predicted_N_zdip_high_VT_e2_BD2,
                             predicted_N_zdip_high_VT_e3_C,
                             predicted_N_zdip_high_VT_e3_C1,
                             predicted_N_zdip_high_VT_e3_C2,
                             predicted_N_zdip_high_VT_e3_BD,
                             predicted_N_zdip_high_VT_e3_BD1,
                             predicted_N_zdip_high_VT_e3_BD2)


all_control_high_R1 <- rbind(predicted_N_zdip_high_R1_e1_C,
                             predicted_N_zdip_high_R1_e1_C1,
                             predicted_N_zdip_high_R1_e1_C2,
                             predicted_N_zdip_high_R1_e1_BD,
                             predicted_N_zdip_high_R1_e1_BD1,
                             predicted_N_zdip_high_R1_e1_BD2,
                             predicted_N_zdip_high_R1_e2_C,
                             predicted_N_zdip_high_R1_e2_C1,
                             predicted_N_zdip_high_R1_e2_C2,
                             predicted_N_zdip_high_R1_e2_BD,
                             predicted_N_zdip_high_R1_e2_BD1,
                             predicted_N_zdip_high_R1_e2_BD2,
                             predicted_N_zdip_high_R1_e3_C,
                             predicted_N_zdip_high_R1_e3_C1,
                             predicted_N_zdip_high_R1_e3_C2,
                             predicted_N_zdip_high_R1_e3_BD,
                             predicted_N_zdip_high_R1_e3_BD1,
                             predicted_N_zdip_high_R1_e3_BD2)


all_control_high_R4 <- rbind(predicted_N_zdip_high_R4_e1_C,
                             predicted_N_zdip_high_R4_e1_C1,
                             predicted_N_zdip_high_R4_e1_C2,
                             predicted_N_zdip_high_R4_e1_BD,
                             predicted_N_zdip_high_R4_e1_BD1,
                             predicted_N_zdip_high_R4_e1_BD2,
                             predicted_N_zdip_high_R4_e2_C,
                             predicted_N_zdip_high_R4_e2_C1,
                             predicted_N_zdip_high_R4_e2_C2,
                             predicted_N_zdip_high_R4_e2_BD,
                             predicted_N_zdip_high_R4_e2_BD1,
                             predicted_N_zdip_high_R4_e2_BD2,
                             predicted_N_zdip_high_R4_e3_C,
                             predicted_N_zdip_high_R4_e3_C1,
                             predicted_N_zdip_high_R4_e3_C2,
                             predicted_N_zdip_high_R4_e3_BD,
                             predicted_N_zdip_high_R4_e3_BD1,
                             predicted_N_zdip_high_R4_e3_BD2)




# Plotting
str(all_control_med_VT)
unique(all_control_med_VT$leaf)

# Calculate mean and 95% confidence interval for B73 x LH287
b73_lh287_summary_VT <- all_control_med_VT %>%
  filter(old_genotype == "B73 x LH287") %>%
  group_by(leaf) %>%
  summarise(
    mean_predicted_N_B73 = mean(predicted_N, na.rm = TRUE),
    lower_CI = mean_predicted_N_B73 - qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    upper_CI = mean_predicted_N_B73 + qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n()))
  )

# Calculate mean predicted N for the remaining genotypes
averaged_genotypes_VT <- all_control_med_VT %>%
  filter(old_genotype != "B73 x LH287") %>%
  group_by(old_genotype, leaf) %>%
  summarise(mean_predicted_N = mean(predicted_N, na.rm = TRUE), .groups = 'drop')

table(averaged_genotypes_VT$leaf)

# Identify genotypes whose mean predicted N values are completely outside the 95% confidence interval of B73
# Calculate the absolute differences for each leaf stage and rank by the total divergence
different_genotypes_ranked <- averaged_genotypes_VT %>%
  dplyr::inner_join(b73_lh287_summary_VT, by = "leaf") %>%
  dplyr::mutate(abs_diff = abs(mean_predicted_N - mean_predicted_N_B73)) %>%  # Calculate absolute difference
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(total_abs_diff = sum(abs_diff, na.rm = TRUE)) %>%  # Sum the absolute differences
  dplyr::arrange(desc(total_abs_diff))  # Rank by total divergence

# Get the top 25 most divergent genotypes
top_25_genotypes <- different_genotypes_ranked %>%
  dplyr::slice_head(n = 25)

# Print the top 25 most divergent genotypes
print(top_25_genotypes)

# Subset top_25_genotypes from averaged_genotypes_VT
top_25_genotypes <- averaged_genotypes_VT %>%
  dplyr::filter(old_genotype %in% top_25_genotypes$old_genotype)

# Plot the mean for B73 x LH287 with confidence intervals
b73_plot_VT <- ggplot(b73_lh287_summary_VT, aes(x = leaf, y = mean_predicted_N_B73, group = 1)) +
  geom_line(color = "blue", size = 1.2) +  # Mean line for B73 x LH287
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "lightblue", alpha = 0.3) +  # Confidence interval band
  geom_point(color = "blue", size = 2) +  # Points for the mean
  labs(
    title = "95% Confidence Interval for B73 x LH287 with Zdip Genotypes (VT Stage) for med Nitrogen",
    x = "Leaf Stage",
    y = "Predicted N (%)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

genotype_colors <- c(
  RColorBrewer::brewer.pal(n = 12, name = "Set3"),  # 12 colors from Set3
  RColorBrewer::brewer.pal(n = 8, name = "Dark2"),  # 8 colors from Dark2
  RColorBrewer::brewer.pal(n = 5, name = "Set1"),
  colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(20)# 5 colors from Set1
)
# Add lines for averaged genotypes
final_plot_VT <- b73_plot_VT +
  geom_line(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, group = old_genotype, color = old_genotype), size = 1) +
  geom_point(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, color = old_genotype), size = 2) +
  scale_color_manual(values = genotype_colors) +
  labs(color = "Genotype")

# Print the final plot
quartz()
print(final_plot_VT)

# Filter genotypes where each leaf stage is above the upper CI
all_above_CI_med_VT <- averaged_genotypes_VT %>%
  dplyr::inner_join(b73_lh287_summary_VT, by = "leaf") %>%
  dplyr::filter(mean_predicted_N > upper_CI) %>%  # Only keep values above the upper CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_above_CI = n()) %>%
  dplyr::filter(count_above_CI == 3)  # Keep only genotypes that are above CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_above_CI_med_VT_details <- averaged_genotypes_VT %>%
  dplyr::filter(old_genotype %in% all_above_CI_med_VT$old_genotype)

# Print the result
print(all_above_CI_med_VT_details)


# Filter genotypes where each leaf stage is below the upper CI
all_below_CI_med_VT <- averaged_genotypes_VT %>%
  dplyr::inner_join(b73_lh287_summary_VT, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI) %>%  # Only keep values below the lower CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_below_CI = n()) %>%
  dplyr::filter(count_below_CI == 3)  # Keep only genotypes that are below CI for all 3 leaf stages




# Join back with the original data to get all details for the selected genotypes
all_below_CI_med_VT_details <- averaged_genotypes_VT %>%
  dplyr::filter(old_genotype %in% all_below_CI_med_VT$old_genotype)

# Print the result
print(all_below_CI_med_VT_details)






# Calculate mean and 95% confidence interval for B73 x LH287
b73_lh287_summary_R4 <- all_control_med_R4 %>%
  filter(old_genotype == "B73 x LH287") %>%
  group_by(leaf) %>%
  summarise(
    mean_predicted_N_B73 = mean(predicted_N, na.rm = TRUE),
    lower_CI = mean_predicted_N_B73 - qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    upper_CI = mean_predicted_N_B73 + qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n()))
  )

# Calculate mean predicted N for the remaining genotypes
averaged_genotypes_R4 <- all_control_med_R4 %>%
  filter(old_genotype != "B73 x LH287") %>%
  group_by(old_genotype, leaf) %>%
  summarise(mean_predicted_N = mean(predicted_N, na.rm = TRUE), .groups = 'drop')

table(averaged_genotypes_R4$leaf)

# Identify genotypes whose mean predicted N values are completely outside the 95% confidence interval of B73
# Calculate the absolute differences for each leaf stage and rank by the total divergence
different_genotypes_ranked <- averaged_genotypes_R4 %>%
  dplyr::inner_join(b73_lh287_summary_R4, by = "leaf") %>%
  dplyr::mutate(abs_diff = abs(mean_predicted_N - mean_predicted_N_B73)) %>%  # Calculate absolute difference
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(total_abs_diff = sum(abs_diff, na.rm = TRUE)) %>%  # Sum the absolute differences
  dplyr::arrange(desc(total_abs_diff))  # Rank by total divergence

# Get the top 25 most divergent genotypes
top_25_genotypes <- different_genotypes_ranked %>%
  dplyr::slice_head(n = 25)

# Print the top 25 most divergent genotypes
print(top_25_genotypes)

# Subset top_25_genotypes from averaged_genotypes_R4
top_25_genotypes <- averaged_genotypes_R4 %>%
  dplyr::filter(old_genotype %in% top_25_genotypes$old_genotype)

# Plot the mean for B73 x LH287 with confidence intervals
b73_plot_R4 <- ggplot(b73_lh287_summary_R4, aes(x = leaf, y = mean_predicted_N_B73, group = 1)) +
  geom_line(color = "blue", size = 1.2) +  # Mean line for B73 x LH287
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "lightblue", alpha = 0.3) +  # Confidence interval band
  geom_point(color = "blue", size = 2) +  # Points for the mean
  labs(
    title = "95% Confidence Interval for B73 x LH287 with Zdip Genotypes (R4 Stage) for med Nitrogen",
    x = "Leaf Stage",
    y = "Predicted N (%)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

genotype_colors <- c(
  RColorBrewer::brewer.pal(n = 12, name = "Set3"),  # 12 colors from Set3
  RColorBrewer::brewer.pal(n = 8, name = "Dark2"),  # 8 colors from Dark2
  RColorBrewer::brewer.pal(n = 5, name = "Set1"),
  colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(20)# 5 colors from Set1
)
# Add lines for averaged genotypes
final_plot_R4 <- b73_plot_R4 +
  geom_line(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, group = old_genotype, color = old_genotype), size = 1) +
  geom_point(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, color = old_genotype), size = 2) +
  scale_color_manual(values = genotype_colors) +
  labs(color = "Genotype")

# Print the final plot
quartz()
print(final_plot_R4)

# Filter genotypes where each leaf stage is above the upper CI
all_above_CI_med_R4 <- averaged_genotypes_R4 %>%
  dplyr::inner_join(b73_lh287_summary_R4, by = "leaf") %>%
  dplyr::filter(mean_predicted_N > upper_CI) %>%  # Only keep values above the upper CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_above_CI = n()) %>%
  dplyr::filter(count_above_CI == 3)  # Keep only genotypes that are above CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_above_CI_med_R4_details <- averaged_genotypes_R4 %>%
  dplyr::filter(old_genotype %in% all_above_CI_med_R4$old_genotype)

# Print the result
print(all_above_CI_med_R4_details)


# Filter genotypes where each leaf stage is below the upper CI
all_below_CI_med_R4 <- averaged_genotypes_R4 %>%
  dplyr::inner_join(b73_lh287_summary_R4, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI) %>%  # Only keep values below the lower CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_below_CI = n()) %>%
  dplyr::filter(count_below_CI == 3)  # Keep only genotypes that are below CI for all 3 leaf stages




# Join back with the original data to get all details for the selected genotypes
all_below_CI_med_R4_details <- averaged_genotypes_R4 %>%
  dplyr::filter(old_genotype %in% all_below_CI_med_R4$old_genotype)

# Print the result
print(all_below_CI_med_R4_details)
