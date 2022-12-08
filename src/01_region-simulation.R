#
# Title: Creating simulated reporting regions
# Created: December 8th, 2022
# Last Updated: December 8th, 2022
# Author: Brandon Allen
# Objective: Create a set of simulated reporting regions of various sizes
# Keywords: Simulations
#

###############
# Simulations #
###############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

#load libraries
library(sf)

# Load Kgrid region
load("data/base/kgrid/overlap-region.Rdata")

# Create template for storing the simulated regions
simulated.region <- list()

# Create blank raster template
raster.template <- raster(nrows = max(overlap.region$Row), 
                          ncols = max(overlap.region$Col), 
                          xmn=1, 
                          xmx=max(overlap.region$Row), 
                          ymn=1, 
                          ymx=max(overlap.region$Col))
values(raster.template) <- 0

# Loop through the five different region sizes            
for(region.size in c(25, 100, 1000, 10000, 100000)) {
            
            # Define region size, and evenly divide raster cells
            n.regions <- round(max(overlap.region$Row) * max(overlap.region$Col) / region.size)
            
            # Create first patch
            region.raster <- makeClass(context = raster.template, npatch = 1, size = region.size, val = 1)
            
            # Iteratively add patches
            for (x in 2:n.regions) {
                        
                        region.raster <- makeClass(context = region.raster, npatch = 1, size = region.size, val = x) 
            }
            
            simulated.region[[paste0("Region_", region.size)]] <- region.raster 
            rm(region.raster)
            
}

save(simulated.region, file = "data/processed/simulated-regions.Rdata")

rm(list=ls())
gc()
