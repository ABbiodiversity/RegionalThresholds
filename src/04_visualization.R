#
# Title: Visualization of the regional thresholds
# Created: December 8th, 2022
# Last Updated: December 20th, 2022
# Author: Brandon Allen
# Objective: Visualize the various approaches for determining regional thresholds.
# Keywords: Simulated Regions
#

#####################
# Simulated Regions #
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggnewscale)
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(sf)
library(tidyverse)

# Source functions
source("src/visualization_functions.R")

# Load summarized results
load("data/processed/aggregated-species-predictions.Rdata")

# For each species, visualize the various thresholds
region.list <- c("Region_25", "Region_100", "Region_1000", 
                 "Region_10000", "Region_100000")

# If no cluster was able to be created, it was given a value of 0. Therefore, define that group as NA
kgrid$Region_25[kgrid$Region_25 == 0] <- NA
kgrid$Region_100[kgrid$Region_100 == 0] <- NA
kgrid$Region_1000[kgrid$Region_1000 == 0] <- NA
kgrid$Region_10000[kgrid$Region_10000 == 0] <- NA
kgrid$Region_100000[kgrid$Region_100000 == 0] <- NA

# Define the species list
species.list <- colnames(kgrid)[16:1219]
species.list <- gsub("_Cur", "", species.list)
species.list <- unique(gsub("_Ref", "", species.list))

# Update rows for properly alignment
kgrid$Row2 <- 1235 - kgrid$Row

for (species in species.list) {
            
            for(region in region.list) {
                        
                        # Create cluster map
                        cluster.map <- regional.plot(species.data = kgrid, 
                                                     region = region)
                        
                        # Create current map
                        current.map <- abundance.plot(species.data = kgrid, 
                                                      species.id = species,
                                                      landscape.state = "Current")

                        # Create reference map
                        reference.map <- abundance.plot(species.data = kgrid, 
                                                        species.id = species,
                                                        landscape.state = "Reference")
                        
                        #
                        # Create Original threshold
                        #
                        
                        mean.values <- mean.threshold[[region]]
                        
                        # Define max abundance
                        max.abund <- max(c(kgrid[, paste0(species, "_Ref")], kgrid[, paste0(species, "_Cur")]), na.rm = TRUE)
                        
                        # Determine inclusion
                        mean.values$Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] > (max.abund * 0.01) |
                                                                    mean.values[, paste0(species, "_Ref")] > (max.abund * 0.01), 1, 0)
                        
                        # Merge with data frame
                        mean.values <- data.frame(region = mean.values$region,
                                                  Inclusion = mean.values$Inclusion)
                        colnames(mean.values)[1] <- region
                        
                        kgrid.inclusion <- merge(kgrid[, c("Row2", "Col", region)], mean.values, by = region)
                        
                        # Create map
                        mean.map <- inclusion.plot(species.data = kgrid.inclusion, 
                                                   threshold = "Original")
                        
                        #
                        # Create Proportion threshold (100%) 
                        #
                        
                        mean.values <- mean.threshold[[region]]
                        
                        # Calculate inclusion
                        mean.values$Cur_Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] >= mean(kgrid[, paste0(species, "_Cur")], na.rm = TRUE), 1, 0)
                        mean.values$Ref_Inclusion <- ifelse(mean.values[, paste0(species, "_Ref")] >= mean(kgrid[, paste0(species, "_Ref")], na.rm = TRUE), 1, 0)
                        mean.values$Inclusion <- ifelse(mean.values$Cur_Inclusion + mean.values$Ref_Inclusion > 0, 1, 0)
                        
                        # Merge with data frame
                        mean.values <- data.frame(region = mean.values$region,
                                                 Inclusion = mean.values$Inclusion)
                        colnames(mean.values)[1] <- region
                        
                        kgrid.inclusion <- merge(kgrid[, c("Row2", "Col", region)], mean.values, by = region)
                        
                        # Create map
                        prop.100.map <- inclusion.plot(species.data = kgrid.inclusion,
                                                       threshold = "Ratio Threshold (100%)")
                        
                        #
                        # Create Proportion threshold (75%) 
                        #
                        
                        mean.values <- mean.threshold[[region]]
                        
                        # Calculate inclusion
                        mean.values$Cur_Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] >= (mean(kgrid[, paste0(species, "_Cur")], na.rm = TRUE) * 0.75), 1, 0)
                        mean.values$Ref_Inclusion <- ifelse(mean.values[, paste0(species, "_Ref")] >= (mean(kgrid[, paste0(species, "_Ref")], na.rm = TRUE) * 0.75), 1, 0)
                        mean.values$Inclusion <- ifelse(mean.values$Cur_Inclusion + mean.values$Ref_Inclusion > 0, 1, 0)
                        
                        # Merge with data frame
                        mean.values <- data.frame(region = mean.values$region,
                                                  Inclusion = mean.values$Inclusion)
                        colnames(mean.values)[1] <- region
                        
                        kgrid.inclusion <- merge(kgrid[, c("Row2", "Col", region)], mean.values, by = region)
                        
                        # Create map
                        prop.75.map <- inclusion.plot(species.data = kgrid.inclusion,
                                                       threshold = "Ratio Threshold (75%)")
                        
                        #
                        # Create Proportion threshold (750%) 
                        #
                        
                        mean.values <- mean.threshold[[region]]
                        
                        # Calculate inclusion
                        mean.values$Cur_Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] >= (mean(kgrid[, paste0(species, "_Cur")], na.rm = TRUE) * 0.50), 1, 0)
                        mean.values$Ref_Inclusion <- ifelse(mean.values[, paste0(species, "_Ref")] >= (mean(kgrid[, paste0(species, "_Ref")], na.rm = TRUE) * 0.50), 1, 0)
                        mean.values$Inclusion <- ifelse(mean.values$Cur_Inclusion + mean.values$Ref_Inclusion > 0, 1, 0)
                        
                        # Merge with data frame
                        mean.values <- data.frame(region = mean.values$region,
                                                  Inclusion = mean.values$Inclusion)
                        colnames(mean.values)[1] <- region
                        
                        kgrid.inclusion <- merge(kgrid[, c("Row2", "Col", region)], mean.values, by = region)
                        
                        # Create map
                        prop.50.map <- inclusion.plot(species.data = kgrid.inclusion,
                                                       threshold = "PRatio Threshold (50%)")
                        
                        #
                        # Create Proportion threshold (25%) 
                        #
                        
                        mean.values <- mean.threshold[[region]]
                        
                        # Calculate inclusion
                        mean.values$Cur_Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] >= (mean(kgrid[, paste0(species, "_Cur")], na.rm = TRUE) * 0.25), 1, 0)
                        mean.values$Ref_Inclusion <- ifelse(mean.values[, paste0(species, "_Ref")] >= (mean(kgrid[, paste0(species, "_Ref")], na.rm = TRUE) * 0.25), 1, 0)
                        mean.values$Inclusion <- ifelse(mean.values$Cur_Inclusion + mean.values$Ref_Inclusion > 0, 1, 0)
                        
                        # Merge with data frame
                        mean.values <- data.frame(region = mean.values$region,
                                                  Inclusion = mean.values$Inclusion)
                        colnames(mean.values)[1] <- region
                        
                        kgrid.inclusion <- merge(kgrid[, c("Row2", "Col", region)], mean.values, by = region)
                        
                        # Create map
                        prop.25.map <- inclusion.plot(species.data = kgrid.inclusion,
                                                       threshold = "Ratio Threshold (25%)")
                        
                        jpeg(paste0("results/figures/simulated/", species, "_", region, ".jpg"),
                            height = 1800,
                            width = 2500,
                            quality = 25)
                        
                        print(ggarrange(cluster.map, reference.map,
                                        current.map, mean.map,
                                        prop.100.map, prop.75.map, 
                                        prop.50.map, prop.25.map, 
                                        ncol = 4, nrow = 2))
                        
                        dev.off()
                        
            }
            
            print(species)
            
}
