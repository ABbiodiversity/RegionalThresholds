#
# Title: Calculate species thresholds for simulated reporting regions
# Created: December 8th, 2022
# Last Updated: December 12th, 2022
# Author: Brandon Allen
# Objective: Using the species model predictions, calculate the various species threshold methods
# Keywords: Species Thresholds
#

######################
# Species Thresholds #
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#
# Load simulated regions and bind to the predictions
#

load("data/processed/simulated-regions.Rdata")

label.vector <- c()

for(y in 1:max(overlap.region$Col)) {
            
            label.vector <- c(label.vector, paste0(seq(from = 1, to = max(overlap.region$Row), by = 1), "_", y))
            
}

region.cells <- data.frame(GRID_LABEL = label.vector,
                           Region_25 = as.vector(raster::as.matrix(simulated.region$Region_25)),
                           Region_100 = as.vector(raster::as.matrix(simulated.region$Region_100)),
                           Region_1000 = as.vector(raster::as.matrix(simulated.region$Region_1000)),
                           Region_10000 = as.vector(raster::as.matrix(simulated.region$Region_10000)),
                           Region_100000 = as.vector(raster::as.matrix(simulated.region$Region_100000)))

# Merge the kgrid with the regions
kgrid <- merge(kgrid, region.cells, by = "GRID_LABEL")

# Create a data frame version so the aggregate function doesn't panic from the geometry column
kgrid.df <- as.data.frame(kgrid)
kgrid.df <- kgrid.df[, -ncol(kgrid.df)]

# Add a provincial column so we can calculate total sum and mean
kgrid.df$Provincial <- 1

# Aggregate by thresholds
mean.threshold <- list()
sum.threshold <- list()

region.list <- c("Region_25", "Region_100", "Region_1000", 
                 "Region_10000", "Region_100000", "Provincial")

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
colnames(kgrid.df)[1] <- "LinkID"
colnames(overlap.region)[1] <- "LinkID"
kgrid.df <- merge.data.frame(kgrid.df, overlap.region, by = "LinkID")
kgrid.df <- kgrid.df[, c(1:4, 297:314, 5:296)]
kgrid <- kgrid.df

# Save results and clear memory
save(mean.threshold, sum.threshold, kgrid, file = "data/processed/aggregated-species-predictions.Rdata")

rm(list=ls())
gc()
