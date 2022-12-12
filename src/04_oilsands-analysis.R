#
# Title: Calculate species thresholds for the Oilsands management region
# Created: December 12th, 2022
# Last Updated: December 12th, 2022
# Author: Brandon Allen
# Objective: Using the species model predictions, assess the species lists for the oilsands region using multiple thresholds
# Keywords: Aggregate predictions, Species lists
#

#########################
# Aggregate predictions #
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

#load libraries
library(landscapeR)
library(raster)
library(sf)

# Load kgrid and overlap region
kgrid <- read_sf(dsn = "data/base/gis/ABMI.gdb", layer = "Grid_1KM")
load("data/base/kgrid/overlap-region.Rdata")
overlap.region <- overlap.region[kgrid$GRID_LABEL,] # Align grids

species.path <- c(list.files("data/base/species/birds/", full.names = TRUE),
                  list.files("data/base/species/mammals/", full.names = TRUE))

# Combine the predictions with the kgrid
for (spp in species.path) {
            
            # Load data
            load(spp)
            
            # Define species name
            species.name <- gsub(".RData", "", spp)
            species.name <- gsub("data/base/species/birds/", "", species.name)
            species.name <- gsub("data/base/species/mammals/", "", species.name)
            
            # If north and south regions are available, merge. Otherwise, use what is available
            if(!is.null(north.sector)) {
                        
                        sector.effect <- north.sector
                        
            }
            
            if(!is.null(south.sector)) {
                        
                        sector.effect <- south.sector
                        
            }
            
            if(!(is.null(north.sector)) & !(is.null(south.sector))) {
                        
                        # averaging comes here
                        north.sector <- north.sector[match(rownames(overlap.region), rownames(north.sector)), ]
                        north.sector[is.na(north.sector)] <- 0
                        rownames(north.sector) <- rownames(overlap.region)
                        
                        south.sector <- south.sector[match(rownames(overlap.region), rownames(south.sector)), ]
                        south.sector[is.na(south.sector)] <- 0
                        rownames(south.sector) <- rownames(overlap.region)
                        
                        sector.effect <- north.sector
                        
                        for (i in colnames(sector.effect)) {
                                    
                                    sector.effect[, i] <- overlap.region$wN * north.sector[, i] + (1-overlap.region$wN) * south.sector[, i]
                                    
                        }
                        
            }
            
            # Calculate current and reference abundances
            sector.effect <- data.frame(LinkID = rownames(sector.effect),
                                        Cur = rowSums(sector.effect[, grep("Cur_", colnames(sector.effect))]),
                                        Ref = rowSums(sector.effect[, grep("Ref_", colnames(sector.effect))]))
            rownames(sector.effect) <- sector.effect$LinkID
            
            # Truncate based on 99th percentile
            q <- min(quantile(sector.effect$Ref, 0.999), quantile(sector.effect$Cur, 0.999))
            sector.effect$Ref <- ifelse(sector.effect$Ref > q, q, sector.effect$Ref)
            sector.effect$Cur <- ifelse(sector.effect$Cur > q, q, sector.effect$Cur)
            
            # Merge with LinkIDs
            kgrid[paste0(species.name, "_Cur")] <- sector.effect$Cur[match(kgrid$GRID_LABEL, rownames(sector.effect))]
            kgrid[paste0(species.name, "_Ref")] <- sector.effect$Ref[match(kgrid$GRID_LABEL, rownames(sector.effect))]
            
            rm(north.sector, south.sector, sector.effect, q)
            
            print(species.name)
            
}

# Update column name
colnames(kgrid)[1] <- "LinkID"

#
# Load predefined analysis unit and subset
#

load("data/base/kgrid/kgrid-reporting-lookup_2022.RData")
km2.info <- km2.info[, c("LinkID", "OSMR_Complete")]

# Merge the kgrid with the regions
kgrid <- merge(kgrid, km2.info, by = "LinkID")

# Create a data frame version so the aggregate function doesn't panic from the geometry column
kgrid.df <- as.data.frame(kgrid)
kgrid.df <- kgrid.df[, -ncol(kgrid.df)]

# Add a provincial column so we can calculate total sum and mean
kgrid.df$Provincial <- 1
kgrid.df$OSMR_Complete <- ifelse(kgrid.df$OSMR_Complete == TRUE, 1, 0)

# Aggregate by thresholds
mean.threshold <- list()
sum.threshold <- list()

region.list <- c("OSMR_Complete", "Provincial")

for (region in region.list) {
            
            mean.threshold[[region]] <- aggregate(kgrid.df[, c(3, 5:296)], 
                                                  by = list(region = kgrid.df[, region]),
                                                  FUN = function(x) mean(x, na.rm = TRUE))
            
            sum.threshold[[region]] <- aggregate(x = kgrid.df[, c(3, 5:296)], 
                                                 by = list(region = kgrid.df[, region]),
                                                 FUN = function(x) sum(x, na.rm = TRUE))
            
            print(region)
            
}

# Update the kgrid so we have one for mapping
colnames(overlap.region)[1] <- "LinkID"
kgrid.df <- merge.data.frame(kgrid.df, overlap.region, by = "LinkID")
kgrid.df <- kgrid.df[, c(1:4, 297:303, 5:296)]
kgrid <- kgrid.df

# Save results and clear memory
save(mean.threshold, sum.threshold, kgrid, file = "data/processed/osm-species-predictions.Rdata")

rm(list=ls())
gc()

#################
# Species lists #
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load data
load("data/processed/osm-species-predictions.Rdata")

# Define the species list
species.list <- colnames(kgrid)[12:303]
species.list <- gsub("_Cur", "", species.list)
species.list <- unique(gsub("_Ref", "", species.list))

# Create data frame to store the results
species.list <- data.frame(Species = species.list,
                           Original = FALSE,
                           Proportion.100 = FALSE,
                           Proportion.75 = FALSE,
                           Proportion.50 = FALSE,
                           Proportion.25 = FALSE)

# For the OSM region, calculate which species would be included and store
region <- "OSMR_Complete"
            
for (species in species.list$Species) {
            
            #
            # Original 
            #
            
            mean.values <- mean.threshold[[region]]
            mean.values <- mean.values[mean.values$region == 1, ]
            
            # Define max abundance
            max.abund <- max(c(kgrid[, paste0(species, "_Ref")], kgrid[, paste0(species, "_Cur")]), na.rm = TRUE)
            
            # Determine inclusion
            mean.values$Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] > (max.abund * 0.01) |
                                                        mean.values[, paste0(species, "_Ref")] > (max.abund * 0.01), 1, 0)
            
            # Store if species is present
            if(mean.values$Inclusion == 1) {
                        
                        species.list[species.list$Species == species, "Original"] <- TRUE
                        
            }
            
            #
            # Proportion 100
            #
            
            mean.values <- mean.threshold[[region]]
            mean.values <- mean.values[mean.values$region == 1, ]
            
            # Calculate inclusion
            mean.values$Cur_Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] >= mean(kgrid[, paste0(species, "_Cur")], na.rm = TRUE), 1, 0)
            mean.values$Ref_Inclusion <- ifelse(mean.values[, paste0(species, "_Ref")] >= mean(kgrid[, paste0(species, "_Ref")], na.rm = TRUE), 1, 0)
            mean.values$Inclusion <- ifelse(mean.values$Cur_Inclusion + mean.values$Ref_Inclusion > 0, 1, 0)
            
            # Store if species is present
            if(mean.values$Inclusion == 1) {
                        
                        species.list[species.list$Species == species, "Proportion.100"] <- TRUE
                        
            }
            
            #
            # Proportion 75
            #
            
            mean.values <- mean.threshold[[region]]
            mean.values <- mean.values[mean.values$region == 1, ]
            
            # Calculate inclusion
            mean.values$Cur_Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] >= (mean(kgrid[, paste0(species, "_Cur")], na.rm = TRUE) * 0.75), 1, 0)
            mean.values$Ref_Inclusion <- ifelse(mean.values[, paste0(species, "_Ref")] >= (mean(kgrid[, paste0(species, "_Ref")], na.rm = TRUE) * 0.75), 1, 0)
            mean.values$Inclusion <- ifelse(mean.values$Cur_Inclusion + mean.values$Ref_Inclusion > 0, 1, 0)
            
            # Store if species is present
            if(mean.values$Inclusion == 1) {
                        
                        species.list[species.list$Species == species, "Proportion.75"] <- TRUE
                        
            }
            
            #
            # Proportion 50
            #
            
            mean.values <- mean.threshold[[region]]
            mean.values <- mean.values[mean.values$region == 1, ]
            
            # Calculate inclusion
            mean.values$Cur_Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] >= (mean(kgrid[, paste0(species, "_Cur")], na.rm = TRUE) * 0.50), 1, 0)
            mean.values$Ref_Inclusion <- ifelse(mean.values[, paste0(species, "_Ref")] >= (mean(kgrid[, paste0(species, "_Ref")], na.rm = TRUE) * 0.50), 1, 0)
            mean.values$Inclusion <- ifelse(mean.values$Cur_Inclusion + mean.values$Ref_Inclusion > 0, 1, 0)
            
            # Store if species is present
            if(mean.values$Inclusion == 1) {
                        
                        species.list[species.list$Species == species, "Proportion.50"] <- TRUE
                        
            }
            
            #
            # Proportion 25
            #
            
            mean.values <- mean.threshold[[region]]
            mean.values <- mean.values[mean.values$region == 1, ]
            
            # Calculate inclusion
            mean.values$Cur_Inclusion <- ifelse(mean.values[, paste0(species, "_Cur")] >= (mean(kgrid[, paste0(species, "_Cur")], na.rm = TRUE) * 0.25), 1, 0)
            mean.values$Ref_Inclusion <- ifelse(mean.values[, paste0(species, "_Ref")] >= (mean(kgrid[, paste0(species, "_Ref")], na.rm = TRUE) * 0.25), 1, 0)
            mean.values$Inclusion <- ifelse(mean.values$Cur_Inclusion + mean.values$Ref_Inclusion > 0, 1, 0)
            
            # Store if species is present
            if(mean.values$Inclusion == 1) {
                        
                        species.list[species.list$Species == species, "Proportion.25"] <- TRUE
                        
            }
        
            print(species)
            
}

# Visualize and make not of notable species

table(species.list$Original)
table(species.list$Proportion.100)
table(species.list$Proportion.75)
table(species.list$Proportion.50)
table(species.list$Proportion.25)
