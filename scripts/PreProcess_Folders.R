#table join the 10 VZA to rest of the data
library(tidyverse)

LT_other <- read.csv('/Users/simon/Documents/Master/Masterarbeit/aktuelles Thema/LT_EcosenseNEW_DEC.csv', header = T)
LT_10 <- read.csv('/Users/simon/Documents/Master/Masterarbeit/aktuelles Thema/LT_Ecosense10only.csv')

FULL <- bind_rows(LT_other, LT_10)

write_csv(FULL, '/Users/simon/Documents/Master/Masterarbeit/aktuelles Thema/LT_Ecosense10FULL.csv')


# Define source and destination directories
destination_dir <- "/Users/simon/Documents/Master/Masterarbeit/LAMeasurement/Bilder_Blätter_LAI/LT_processed"
source_dir <- "/Users/simon/Documents/Master/Masterarbeit/LAMeasurement/Bilder_Blätter_LAI/TL"

# List all files in the source directory
all_files <- list.files(source_dir, full.names = TRUE)

# Filter files containing "processed" in their filenames
processed_files <- all_files[grepl("processed", all_files)]

# Move each file to the destination directory
for (file in processed_files) {
  file.rename(file, file.path(destination_dir, basename(file)))
}

cat("All processed files have been moved.\n")