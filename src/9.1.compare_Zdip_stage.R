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

all_desinked_med_R4 <- rbind(predicted_N_zdip_med_R4_e1_AD1,
                             predicted_N_zdip_med_R4_e1_AD2,
                             predicted_N_zdip_med_R4_e1_AD)


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




all_desinked_med_R1 <- rbind(predicted_N_zdip_med_R1_e1_AD1,
                             predicted_N_zdip_med_R1_e1_AD2,
                             predicted_N_zdip_med_R1_e1_AD,
                             predicted_N_zdip_med_R1_e2_AD1,
                             predicted_N_zdip_med_R1_e2_AD2,
                             predicted_N_zdip_med_R1_e2_AD,
                             predicted_N_zdip_med_R1_e3_AD1,
                             predicted_N_zdip_med_R1_e3_AD2,
                             predicted_N_zdip_med_R1_e3_AD)

all_desinked_high_R1 <- rbind(predicted_N_zdip_high_R1_e1_AD1,
                              predicted_N_zdip_high_R1_e1_AD2,
                              predicted_N_zdip_high_R1_e1_AD,
                              predicted_N_zdip_high_R1_e2_AD1,
                              predicted_N_zdip_high_R1_e2_AD2,
                              predicted_N_zdip_high_R1_e2_AD,
                              predicted_N_zdip_high_R1_e3_AD1,
                              predicted_N_zdip_high_R1_e3_AD2,
                              predicted_N_zdip_high_R1_e3_AD)

all_desinked_med_R4 <- rbind(predicted_N_zdip_med_R4_e1_AD1,
                             predicted_N_zdip_med_R4_e1_AD2,
                             predicted_N_zdip_med_R4_e1_AD,
                             predicted_N_zdip_med_R4_e2_AD1,
                             predicted_N_zdip_med_R4_e2_AD2,
                             predicted_N_zdip_med_R4_e2_AD,
                             predicted_N_zdip_med_R4_e3_AD1,
                             predicted_N_zdip_med_R4_e3_AD2,
                             predicted_N_zdip_med_R4_e3_AD)

all_desinked_high_R4 <- rbind(predicted_N_zdip_high_R4_e1_AD1,
                              predicted_N_zdip_high_R4_e1_AD2,
                              predicted_N_zdip_high_R4_e1_AD,
                              predicted_N_zdip_high_R4_e2_AD1,
                              predicted_N_zdip_high_R4_e2_AD2,
                              predicted_N_zdip_high_R4_e2_AD,
                              predicted_N_zdip_high_R4_e3_AD1,
                              predicted_N_zdip_high_R4_e3_AD2,
                              predicted_N_zdip_high_R4_e3_AD)


# Not good data:
# Remove 454 from plot
all_control_high_R4 <- all_control_high_R4[all_control_high_R4$old_genotype != 454, ]
str(all_control_high_R4)



##### looping through it all
# List of stages and treatments to loop through
stages <- c("VT", "R1", "R4")
treatments <- c("med", "high")

# Loop through each stage and treatment combination
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# List of stages and treatments to loop through
stages <- c("VT", "R1", "R4")
treatments <- c("med", "high")

# Loop through each stage and treatment combination
for (stage in stages) {
  for (treatment in treatments) {
    # Filter the data for the current stage and treatment
    data_filtered <- all_control_med %>%
      dplyr::filter(stage == stage, N_treatment == treatment)
    
    # Calculate mean and 95% CI for B73 x LH287
    b73_summary <- data_filtered %>%
      dplyr::filter(old_genotype == "B73 x LH287") %>%
      dplyr::group_by(leaf) %>%
      dplyr::summarise(
        mean_predicted_N_B73 = mean(predicted_N, na.rm = TRUE),
        lower_CI = mean_predicted_N_B73 - qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
        upper_CI = mean_predicted_N_B73 + qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n()))
      )
    
    # Calculate mean predicted N for the remaining genotypes
    averaged_genotypes <- data_filtered %>%
      dplyr::filter(old_genotype != "B73 x LH287") %>%
      dplyr::group_by(old_genotype, leaf) %>%
      dplyr::summarise(
        mean_predicted_N = mean(predicted_N, na.rm = TRUE),
        plant = paste(unique(plant), collapse = ", "),
        .groups = 'drop'
      )
    
    # Save averaged genotypes data
    write.csv(averaged_genotypes, paste0("averaged_genotypes_", treatment, "_control_", stage, ".csv"))
    
    # Identify top 25 most divergent genotypes
    different_genotypes_ranked <- averaged_genotypes %>%
      dplyr::inner_join(b73_summary, by = "leaf") %>%
      dplyr::mutate(abs_diff = abs(mean_predicted_N - mean_predicted_N_B73)) %>%
      dplyr::group_by(old_genotype) %>%
      dplyr::summarise(total_abs_diff = sum(abs_diff, na.rm = TRUE)) %>%
      dplyr::arrange(desc(total_abs_diff))
    
    top_25_genotypes <- different_genotypes_ranked %>%
      dplyr::slice_head(n = 25)
    
    # Subset top 25 genotypes
    top_25_genotypes <- averaged_genotypes %>%
      dplyr::filter(old_genotype %in% top_25_genotypes$old_genotype)
    
    # Plot B73 and top 25 genotypes
    b73_plot <- ggplot2::ggplot(b73_summary, aes(x = leaf, y = mean_predicted_N_B73, group = 1)) +
      ggplot2::geom_line(color = "blue", size = 1.2) +
      ggplot2::geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "lightblue", alpha = 0.3) +
      ggplot2::geom_point(color = "blue", size = 2) +
      ggplot2::labs(
        title = paste("95% Confidence Interval for B73 x LH287 with Zdip Genotypes (", stage, " Stage) for", treatment, "Nitrogen"),
        x = "Leaf Stage",
        y = "Predicted N (%)"
      ) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = "black", fill = NA)
      )
    
    # Add lines for top 25 genotypes
    final_plot <- b73_plot +
      ggplot2::geom_line(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, group = old_genotype, color = old_genotype), size = 1) +
      ggplot2::geom_point(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, color = old_genotype), size = 2) +
      ggplot2::scale_color_manual(values = genotype_colors) +
      ggplot2::labs(color = "Genotype")
    
    # Save the plot
    ggplot2::ggsave(
      filename = paste0("Top25_divergent_genotype_", treatment, "_", stage, ".png"),
      plot = final_plot,
      device = "png",
      width = 14,
      height = 6,
      units = "in",
      dpi = 300,
      bg = "white"
    )
    
    # Identify genotypes above and below CI, and other conditions
    all_above_CI <- averaged_genotypes %>%
      dplyr::inner_join(b73_summary, by = "leaf") %>%
      dplyr::filter(mean_predicted_N > upper_CI) %>%
      dplyr::group_by(old_genotype) %>%
      dplyr::summarise(count_above_CI = n()) %>%
      dplyr::filter(count_above_CI == 3)
    
    all_above_CI_details <- averaged_genotypes %>%
      dplyr::filter(old_genotype %in% all_above_CI$old_genotype)
    
    all_above_CI_details$condition <- paste0("all_above_CI_", treatment, "_", stage)
    
    all_below_CI <- averaged_genotypes %>%
      dplyr::inner_join(b73_summary, by = "leaf") %>%
      dplyr::filter(mean_predicted_N < lower_CI) %>%
      dplyr::group_by(old_genotype) %>%
      dplyr::summarise(count_below_CI = n()) %>%
      dplyr::filter(count_below_CI == 3)
    
    all_below_CI_details <- averaged_genotypes %>%
      dplyr::filter(old_genotype %in% all_below_CI$old_genotype)
    
    all_below_CI_details$condition <- paste0("all_below_CI_", treatment, "_", stage)
    
    e3_lower_genotypes <- averaged_genotypes %>%
      tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
      dplyr::filter(e3 < e1 - 0.25 & e3 < e2 - 0.25) %>%
      tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")
    
    e3_lower_genotypes$condition <- paste0("e3_lowest_", treatment, "_", stage)
    
    e3_higher_genotypes <- averaged_genotypes %>%
      tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
      dplyr::filter(e3 > e1 + 0.50 & e3 > e2 + 0.50) %>%
      tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")
    
    e3_higher_genotypes$condition <- paste0("e3_highest_", treatment, "_", stage)
    
    # Combine all the data frames
    total_combined <- rbind(all_above_CI_details, all_below_CI_details, e3_lower_genotypes, e3_higher_genotypes)
    
    # Save the combined data for further analysis
    write.csv(total_combined, paste0("Total_combined_", treatment, "_", stage, ".csv"), row.names = FALSE)
  }
}



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
  summarise(mean_predicted_N = mean(predicted_N, na.rm = TRUE), 
            plant = paste(unique(plant), collapse = ", "),
            .groups = 'drop'
            )

str(averaged_genotypes_VT)


write.csv(averaged_genotypes_VT, "averaged_genotypes_VT_control_med.csv")
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
  #scale_fill_manual(name = "Control with CI", values = c("B73xLH287" = "lightblue")) +  # Color for the CI
  #scale_linetype_manual(name = "Control with CI", values = c("B73xLH287" = "solid")) +  # Line type for the B73 mean line
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

#save the plot
ggsave("Top25_divergent_genotype_Control_med_VT.png", plot = final_plot_VT, device = "png", width = 14, height = 6, units = "in", dpi = 300, bg= "white")

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

all_above_CI_med_VT_details$condition <- "all_above_CI_med_VT"

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

all_below_CI_med_VT_details$condition <- "all_below_CI_med_VT"

# Print the result
print(all_below_CI_med_VT_details)



# Filter genotypes where e3 is lower than both e1 and e2 by atleast 0.25%
e3_lower_genotypes <- averaged_genotypes_VT %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 < e1-0.25 & e3 < e2-0.25) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

# Print the result
print(e3_lower_genotypes)

e3_lower_genotypes$condition <- "e3_lowest_med_VT"

# Filter genotypes where e3 is higher than both e1 and e2 by atleast 0.25%
e3_higher_genotypes <- averaged_genotypes_VT %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 > e1+0.50 & e3 > e2+0.50) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")


e3_higher_genotypes$condition <- "e3_highest_med_VT"

# Print the result
print(e3_higher_genotypes)


# Combine all the dataframes 
total_VT_med <- rbind(all_above_CI_med_VT_details, all_below_CI_med_VT_details, e3_lower_genotypes, e3_higher_genotypes)




# Calculate mean and 95% confidence interval for B73 x LH287
b73_lh287_summary_R1 <- all_control_med_R1 %>%
  filter(old_genotype == "B73 x LH287") %>%
  group_by(leaf) %>%
  summarise(
    mean_predicted_N_B73 = mean(predicted_N, na.rm = TRUE),
    lower_CI = mean_predicted_N_B73 - qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    upper_CI = mean_predicted_N_B73 + qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n()))
  )

# Calculate mean predicted N for the remaining genotypes
averaged_genotypes_R1 <- all_control_med_R1 %>%
  filter(old_genotype != "B73 x LH287") %>%
  group_by(old_genotype, leaf) %>%
  summarise(mean_predicted_N = mean(predicted_N, na.rm = TRUE), .groups = 'drop')

table(averaged_genotypes_R1$leaf)

# save
write.csv(averaged_genotypes_R1, "averaged_genotypes_R1_control_med.csv")

# Identify genotypes whose mean predicted N values are completely outside the 95% confidence interval of B73
# Calculate the absolute differences for each leaf stage and rank by the total divergence
different_genotypes_ranked <- averaged_genotypes_R1 %>%
  dplyr::inner_join(b73_lh287_summary_R1, by = "leaf") %>%
  dplyr::mutate(abs_diff = abs(mean_predicted_N - mean_predicted_N_B73)) %>%  # Calculate absolute difference
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(total_abs_diff = sum(abs_diff, na.rm = TRUE)) %>%  # Sum the absolute differences
  dplyr::arrange(desc(total_abs_diff))  # Rank by total divergence

# Get the top 25 most divergent genotypes
top_25_genotypes <- different_genotypes_ranked %>%
  dplyr::slice_head(n = 25)

# Print the top 25 most divergent genotypes
print(top_25_genotypes)

# Subset top_25_genotypes from averaged_genotypes_R1
top_25_genotypes <- averaged_genotypes_R1 %>%
  dplyr::filter(old_genotype %in% top_25_genotypes$old_genotype)


# Plot the mean for B73 x LH287 with confidence intervals
b73_plot_R1 <- ggplot(b73_lh287_summary_R1, aes(x = leaf, y = mean_predicted_N_B73, group = 1)) +
  geom_line(color = "blue", size = 1.2) +  # Mean line for B73 x LH287
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "lightblue", alpha = 0.3) +  # Confidence interval band
  geom_point(color = "blue", size = 2) +  # Points for the mean
  #scale_fill_manual(name = "Control with CI", values = c("B73xLH287" = "lightblue")) +  # Color for the CI
  #scale_linetype_manual(name = "Control with CI", values = c("B73xLH287" = "solid")) +  # Line type for the B73 mean line
  labs(
    title = "95% Confidence Interval for B73 x LH287 with Zdip Genotypes (R1 Stage) for med Nitrogen",
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
final_plot_R1 <- b73_plot_R1 +
  geom_line(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, group = old_genotype, color = old_genotype), size = 1) +
  geom_point(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, color = old_genotype), size = 2) +
  scale_color_manual(values = genotype_colors) +
  labs(color = "Genotype")

# Print the final plot
quartz()
print(final_plot_R1)

#save the plot
ggsave("Top25_divergent_genotype_Control_med_R1.png", plot = final_plot_R1, device = "png", width = 14, height = 6, units = "in", dpi = 300, bg= "white")

# Filter genotypes where each leaf stage is above the upper CI
all_above_CI_med_R1 <- averaged_genotypes_R1 %>%
  dplyr::inner_join(b73_lh287_summary_R1, by = "leaf") %>%
  dplyr::filter(mean_predicted_N > upper_CI) %>%  # Only keep values above the upper CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_above_CI = n()) %>%
  dplyr::filter(count_above_CI == 3)  # Keep only genotypes that are above CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_above_CI_med_R1_details <- averaged_genotypes_R1 %>%
  dplyr::filter(old_genotype %in% all_above_CI_med_R1$old_genotype)

all_above_CI_med_R1_details$condition <- "all_above_CI_med_R1"

# Print the result
print(all_above_CI_med_R1_details)


# Filter genotypes where each leaf stage is below the upper CI
all_below_CI_med_R1 <- averaged_genotypes_R1 %>%
  dplyr::inner_join(b73_lh287_summary_R1, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI) %>%  # Only keep values below the lower CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_below_CI = n()) %>%
  dplyr::filter(count_below_CI == 3)  # Keep only genotypes that are below CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_below_CI_med_R1_details <- averaged_genotypes_R1 %>%
  dplyr::filter(old_genotype %in% all_below_CI_med_R1$old_genotype)

all_below_CI_med_R1_details$condition <- "all_below_CI_med_R1"

# Print the result
print(all_below_CI_med_R1_details)



# Filter genotypes where e3 is lower than both e1 and e2 by atleast 0.25%
e3_lower_genotypes <- averaged_genotypes_R1 %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 < e1-0.25 & e3 < e2-0.25) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

# Print the result
print(e3_lower_genotypes)

e3_lower_genotypes$condition <- "e3_lowest_med_R1"

# Filter genotypes where e3 is higher than both e1 and e2 by atleast 0.25%
e3_higher_genotypes <- averaged_genotypes_R1 %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 > e1+0.50 & e3 > e2+0.50) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

e3_higher_genotypes$condition <- "e3_highest_med_R1"

# Print the result
print(e3_higher_genotypes)



# Combine all the dataframes 
total_R1_med <- rbind(all_above_CI_med_R1_details, all_below_CI_med_R1_details, e3_lower_genotypes, e3_higher_genotypes)


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

# save
#write.csv(averaged_genotypes_R4, "averaged_genotypes_R4_control_med.csv")

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
  #scale_fill_manual(name = "Control with CI", values = c("B73xLH287" = "lightblue")) +  # Color for the CI
  #scale_linetype_manual(name = "Control with CI", values = c("B73xLH287" = "solid")) +  # Line type for the B73 mean line
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

#save the plot
ggsave("Top25_divergent_genotype_Control_med_R4.png", plot = final_plot_R4, device = "png", width = 14, height = 6, units = "in", dpi = 300, bg= "white")

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

all_above_CI_med_R4_details$condition <- "all_above_CI_med_R4"

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

all_below_CI_med_R4_details$condition <- "all_below_CI_med_R4"

# Print the result
print(all_below_CI_med_R4_details)



# Filter genotypes where e3 is lower than both e1 and e2 by atleast 0.25%
e3_lower_genotypes <- averaged_genotypes_R4 %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 < e1-0.25 & e3 < e2-0.25) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

# Print the result
print(e3_lower_genotypes)

e3_lower_genotypes$condition <- "e3_lowest_med_R4"

# Filter genotypes where e3 is higher than both e1 and e2 by atleast 0.25%
e3_higher_genotypes <- averaged_genotypes_R4 %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 > e1+0.50 & e3 > e2+0.50) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

e3_higher_genotypes$condition <- "e3_highest_med_R4"

# Print the result
print(e3_higher_genotypes)



# Combine all the dataframes 
total_R4_med <- rbind(all_above_CI_med_R4_details, all_below_CI_med_R4_details, e3_lower_genotypes, e3_higher_genotypes)


# Combine all the VT, R1, and R4 dataframe
total_VT_med$stage <- "VT_med"
total_R1_med$stage <- "R1_med"
total_R4_med$stage <- "R4_med"

total_VT_med$N_condition <- "med"
total_R1_med$N_condition <- "med"
total_R4_med$N_condition <- "med"

total_med <- rbind(total_VT_med, total_R1_med, total_R4_med)



# Calculate mean and 95% confidence interval for B73 x LH287
b73_lh287_summary_VT <- all_control_high_VT %>%
  filter(old_genotype == "B73 x LH287") %>%
  group_by(leaf) %>%
  summarise(
    mean_predicted_N_B73 = mean(predicted_N, na.rm = TRUE),
    lower_CI = mean_predicted_N_B73 - qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    upper_CI = mean_predicted_N_B73 + qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n()))
  )

# Calculate mean predicted N for the remaining genotypes
averaged_genotypes_VT <- all_control_high_VT %>%
  filter(old_genotype != "B73 x LH287") %>%
  group_by(old_genotype, leaf) %>%
  summarise(mean_predicted_N = mean(predicted_N, na.rm = TRUE), .groups = 'drop')

table(averaged_genotypes_VT$leaf)

# Save
write.csv(averaged_genotypes_VT, "averaged_genotypes_VT_control_high.csv")

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
  #scale_fill_manual(name = "Control with CI", values = c("B73xLH287" = "lightblue")) +  # Color for the CI
  #scale_linetype_manual(name = "Control with CI", values = c("B73xLH287" = "solid")) +  # Line type for the B73 mean line
  labs(
    title = "95% Confidence Interval for B73 x LH287 with Zdip Genotypes (VT Stage) for high Nitrogen",
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

#save the plot
ggsave("Top25_divergent_genotype_Control_high_VT.png", plot = final_plot_VT, device = "png", width = 14, height = 6, units = "in", dpi = 300, bg= "white")

# Filter genotypes where each leaf stage is above the upper CI
all_above_CI_high_VT <- averaged_genotypes_VT %>%
  dplyr::inner_join(b73_lh287_summary_VT, by = "leaf") %>%
  dplyr::filter(mean_predicted_N > upper_CI) %>%  # Only keep values above the upper CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_above_CI = n()) %>%
  dplyr::filter(count_above_CI == 3)  # Keep only genotypes that are above CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_above_CI_high_VT_details <- averaged_genotypes_VT %>%
  dplyr::filter(old_genotype %in% all_above_CI_high_VT$old_genotype)

all_above_CI_high_VT_details$condition <- "all_above_CI_high_VT"

# Print the result
print(all_above_CI_high_VT_details)


# Filter genotypes where each leaf stage is below the upper CI
all_below_CI_high_VT <- averaged_genotypes_VT %>%
  dplyr::inner_join(b73_lh287_summary_VT, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI) %>%  # Only keep values below the lower CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_below_CI = n()) %>%
  dplyr::filter(count_below_CI == 3)  # Keep only genotypes that are below CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_below_CI_high_VT_details <- averaged_genotypes_VT %>%
  dplyr::filter(old_genotype %in% all_below_CI_high_VT$old_genotype)

all_below_CI_high_VT_details$condition <- "all_below_CI_high_VT"

# Print the result
print(all_below_CI_high_VT_details)



# Filter genotypes where e3 is lower than both e1 and e2 by atleast 0.25%
e3_lower_genotypes <- averaged_genotypes_VT %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 < e1-0.25 & e3 < e2-0.25) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

# Print the result
print(e3_lower_genotypes)

e3_lower_genotypes$condition <- "e3_lowest_high_VT"

# Filter genotypes where e3 is higher than both e1 and e2 by atleast 0.25%
e3_higher_genotypes <- averaged_genotypes_VT %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 > e1+0.50 & e3 > e2+0.50) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

e3_higher_genotypes$condition <- "e3_highest_high_VT"

# Print the result
print(e3_higher_genotypes)



# Combine all the dataframes 
total_VT_high <- rbind(all_above_CI_high_VT_details, all_below_CI_high_VT_details, e3_lower_genotypes, e3_higher_genotypes)




# Calculate mean and 95% confidence interval for B73 x LH287
b73_lh287_summary_R1 <- all_control_high_R1 %>%
  filter(old_genotype == "B73 x LH287") %>%
  group_by(leaf) %>%
  summarise(
    mean_predicted_N_B73 = mean(predicted_N, na.rm = TRUE),
    lower_CI = mean_predicted_N_B73 - qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    upper_CI = mean_predicted_N_B73 + qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n()))
  )

# Calculate mean predicted N for the remaining genotypes
averaged_genotypes_R1 <- all_control_high_R1 %>%
  filter(old_genotype != "B73 x LH287") %>%
  group_by(old_genotype, leaf) %>%
  summarise(mean_predicted_N = mean(predicted_N, na.rm = TRUE), .groups = 'drop')

table(averaged_genotypes_R1$leaf)

# save
write.csv(averaged_genotypes_R1, "averaged_genotypes_R1_control_high.csv")

# Identify genotypes whose mean predicted N values are completely outside the 95% confidence interval of B73
# Calculate the absolute differences for each leaf stage and rank by the total divergence
different_genotypes_ranked <- averaged_genotypes_R1 %>%
  dplyr::inner_join(b73_lh287_summary_R1, by = "leaf") %>%
  dplyr::mutate(abs_diff = abs(mean_predicted_N - mean_predicted_N_B73)) %>%  # Calculate absolute difference
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(total_abs_diff = sum(abs_diff, na.rm = TRUE)) %>%  # Sum the absolute differences
  dplyr::arrange(desc(total_abs_diff))  # Rank by total divergence

# Get the top 25 most divergent genotypes
top_25_genotypes <- different_genotypes_ranked %>%
  dplyr::slice_head(n = 25)

# Print the top 25 most divergent genotypes
print(top_25_genotypes)

# Subset top_25_genotypes from averaged_genotypes_R1
top_25_genotypes <- averaged_genotypes_R1 %>%
  dplyr::filter(old_genotype %in% top_25_genotypes$old_genotype)


# Plot the mean for B73 x LH287 with confidence intervals
b73_plot_R1 <- ggplot(b73_lh287_summary_R1, aes(x = leaf, y = mean_predicted_N_B73, group = 1)) +
  geom_line(color = "blue", size = 1.2) +  # Mean line for B73 x LH287
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "lightblue", alpha = 0.3) +  # Confidence interval band
  geom_point(color = "blue", size = 2) +  # Points for the mean
  #scale_fill_manual(name = "Control with CI", values = c("B73xLH287" = "lightblue")) +  # Color for the CI
  #scale_linetype_manual(name = "Control with CI", values = c("B73xLH287" = "solid")) +  # Line type for the B73 mean line
  labs(
    title = "95% Confidence Interval for B73 x LH287 with Zdip Genotypes (R1 Stage) for high Nitrogen",
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
final_plot_R1 <- b73_plot_R1 +
  geom_line(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, group = old_genotype, color = old_genotype), size = 1) +
  geom_point(data = top_25_genotypes, aes(x = leaf, y = mean_predicted_N, color = old_genotype), size = 2) +
  scale_color_manual(values = genotype_colors) +
  labs(color = "Genotype")

# Print the final plot
quartz()
print(final_plot_R1)

#save the plot
ggsave("Top25_divergent_genotype_Control_high_R1.png", plot = final_plot_R1, device = "png", width = 14, height = 6, units = "in", dpi = 300, bg= "white")

# Filter genotypes where each leaf stage is above the upper CI
all_above_CI_high_R1 <- averaged_genotypes_R1 %>%
  dplyr::inner_join(b73_lh287_summary_R1, by = "leaf") %>%
  dplyr::filter(mean_predicted_N > upper_CI) %>%  # Only keep values above the upper CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_above_CI = n()) %>%
  dplyr::filter(count_above_CI == 3)  # Keep only genotypes that are above CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_above_CI_high_R1_details <- averaged_genotypes_R1 %>%
  dplyr::filter(old_genotype %in% all_above_CI_high_R1$old_genotype)

all_above_CI_high_R1_details$condition <- "all_above_CI_high_R1"

# Print the result
print(all_above_CI_high_R1_details)


# Filter genotypes where each leaf stage is below the upper CI
all_below_CI_high_R1 <- averaged_genotypes_R1 %>%
  dplyr::inner_join(b73_lh287_summary_R1, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI) %>%  # Only keep values below the lower CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_below_CI = n()) %>%
  dplyr::filter(count_below_CI == 3)  # Keep only genotypes that are below CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_below_CI_high_R1_details <- averaged_genotypes_R1 %>%
  dplyr::filter(old_genotype %in% all_below_CI_high_R1$old_genotype)

all_below_CI_high_R1_details$condition <- "all_below_CI_high_R1"

# Print the result
print(all_below_CI_high_R1_details)



# Filter genotypes where e3 is lower than both e1 and e2 by atleast 0.25%
e3_lower_genotypes <- averaged_genotypes_R1 %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 < e1-0.25 & e3 < e2-0.25) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

# Print the result
print(e3_lower_genotypes)

e3_lower_genotypes$condition <- "e3_lowest_high_R1"

# Filter genotypes where e3 is higher than both e1 and e2 by atleast 0.25%
e3_higher_genotypes <- averaged_genotypes_R1 %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 > e1+0.50 & e3 > e2+0.50) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

e3_higher_genotypes$condition <- "e3_highest_high_R1"

# Print the result
print(e3_higher_genotypes)



# Combine all the dataframes 
total_R1_high <- rbind(all_above_CI_high_R1_details, all_below_CI_high_R1_details, e3_lower_genotypes, e3_higher_genotypes)


# Calculate mean and 95% confidence interval for B73 x LH287
b73_lh287_summary_R4 <- all_control_high_R4 %>%
  filter(old_genotype == "B73 x LH287") %>%
  group_by(leaf) %>%
  summarise(
    mean_predicted_N_B73 = mean(predicted_N, na.rm = TRUE),
    lower_CI = mean_predicted_N_B73 - qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n())),
    upper_CI = mean_predicted_N_B73 + qt(0.975, df = n() - 1) * (sd(predicted_N, na.rm = TRUE) / sqrt(n()))
  )

# Calculate mean predicted N for the remaining genotypes
averaged_genotypes_R4 <- all_control_high_R4 %>%
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
  #scale_fill_manual(name = "Control with CI", values = c("B73xLH287" = "lightblue")) +  # Color for the CI
  #scale_linetype_manual(name = "Control with CI", values = c("B73xLH287" = "solid")) +  # Line type for the B73 mean line
  labs(
    title = "95% Confidence Interval for B73 x LH287 with Zdip Genotypes (R4 Stage) for high Nitrogen",
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

#save the plot
ggsave("Top25_divergent_genotype_Control_high_R4.png", plot = final_plot_R4, device = "png", width = 14, height = 6, units = "in", dpi = 300, bg= "white")

# Filter genotypes where each leaf stage is above the upper CI
all_above_CI_high_R4 <- averaged_genotypes_R4 %>%
  dplyr::inner_join(b73_lh287_summary_R4, by = "leaf") %>%
  dplyr::filter(mean_predicted_N > upper_CI) %>%  # Only keep values above the upper CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_above_CI = n()) %>%
  dplyr::filter(count_above_CI == 3)  # Keep only genotypes that are above CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_above_CI_high_R4_details <- averaged_genotypes_R4 %>%
  dplyr::filter(old_genotype %in% all_above_CI_high_R4$old_genotype)

all_above_CI_high_R4_details$condition <- "all_above_CI_high_R4"

# Print the result
print(all_above_CI_high_R4_details)


# Filter genotypes where each leaf stage is below the upper CI
all_below_CI_high_R4 <- averaged_genotypes_R4 %>%
  dplyr::inner_join(b73_lh287_summary_R4, by = "leaf") %>%
  dplyr::filter(mean_predicted_N < lower_CI) %>%  # Only keep values below the lower CI
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(count_below_CI = n()) %>%
  dplyr::filter(count_below_CI == 3)  # Keep only genotypes that are below CI for all 3 leaf stages

# Join back with the original data to get all details for the selected genotypes
all_below_CI_high_R4_details <- averaged_genotypes_R4 %>%
  dplyr::filter(old_genotype %in% all_below_CI_high_R4$old_genotype)

all_below_CI_high_R4_details$condition <- "all_below_CI_high_R4"

# Print the result
print(all_below_CI_high_R4_details)



# Filter genotypes where e3 is lower than both e1 and e2 by atleast 0.25%
e3_lower_genotypes <- averaged_genotypes_R4 %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 < e1-0.25 & e3 < e2-0.25) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

# Print the result
print(e3_lower_genotypes)

e3_lower_genotypes$condition <- "e3_lowest_high_R4"

# Filter genotypes where e3 is higher than both e1 and e2 by atleast 0.25%
e3_higher_genotypes <- averaged_genotypes_R4 %>%
  tidyr::pivot_wider(names_from = leaf, values_from = mean_predicted_N) %>%
  dplyr::filter(e3 > e1+0.50 & e3 > e2+0.50) %>%
  tidyr::pivot_longer(cols = c(e1, e2, e3), names_to = "leaf", values_to = "mean_predicted_N")

e3_higher_genotypes$condition <- "e3_highest_high_R4"

# Print the result
print(e3_higher_genotypes)



# Combine all the dataframes 
total_R4_high <- rbind(all_above_CI_high_R4_details, all_below_CI_high_R4_details, e3_lower_genotypes, e3_higher_genotypes)


# Combine all the VT, R1, and R4 dataframe
total_VT_high$stage <- "VT_high"
total_R1_high$stage <- "R1_high"
total_R4_high$stage <- "R4_high"

total_R4_high$N_condition <- "high"
total_R1_high$N_condition <- "high"
total_VT_high$N_condition <- "high"

total_high <- rbind(total_VT_high, total_R1_high, total_R4_high)


# total all
total_all <- rbind(total_med, total_high)

str(total_all)


# Summarize the data by old_genotype and collapse other columns
summarized_total_all <- total_all %>%
  dplyr::group_by(old_genotype) %>%
  dplyr::summarise(
    leaf = paste(unique(leaf), collapse = ", "),
    mean_predicted_N = paste(unique(mean_predicted_N), collapse = ", "),
    condition = paste(unique(condition), collapse = ", "),
    stage = paste(unique(stage), collapse = ", "),
    N_condition = paste(unique(N_condition), collapse = ", "),
    count_condition = length(unique(unlist(strsplit(condition, ", "))))
  )

# Print the summarized data
print(summarized_total_all)


# Save the summarized data
write.csv(summarized_total_all, "summarized_total_Zdip_all.csv", row.names = FALSE)

