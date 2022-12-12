#
# Title: Functions for visualization the regional threshold analysis
# Created: December 8th, 2022
# Last Updated: December 8th, 2022
# Author: Brandon Allen
# Objective: Define the different types of figures that will be used to assess regional connectivity
# Keywords: Regional, Abundance, Inclusion
#

############
# Regional #
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regional.plot <- function(species.data, region) {
            
            # Define number of regions
            n.regions <- length(unique(as.data.frame(species.data[, region])[,1]))
            
            # Create plot
            return(ggplot() +
                        geom_raster(data = species.data, 
                                aes_string(x = "Col", y = "Row2", fill = region), 
                                show.legend = TRUE) +
                        scale_fill_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = n.regions, type = "continuous")), 
                                             limits = c(0,n.regions), 
                                             guide = "none") +
                        scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = n.regions, type = "continuous")), 
                                              limits = c(0,n.regions), 
                                              guide = "none") +
                        ggtitle("Regions") +
                        theme_light() +
                        theme(axis.title = element_blank(),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              title = element_text(size=12),
                              axis.line = element_line(colour = "black"),
                              panel.border = element_rect(colour = "black", fill=NA, size=1)))
            
}


#############
# Abundance #
#############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

abundance.plot <- function(species.data, species.id, landscape.state) {
            
            # Define column name
            if(landscape.state == "Current") {
                        
                        col.id <- paste0(species.id, "_Cur")
                        
            }
            
            if(landscape.state == "Reference") {
                        
                        col.id <- paste0(species.id, "_Ref")
                        
            }
            
            # Define max abundance
            max.abund <- max(c(species.data[, paste0(species.id, "_Ref")], species.data[, paste0(species.id, "_Cur")]), na.rm = TRUE)
            
            # Create plot
            return(ggplot() +
                        geom_raster(data = species.data, 
                                    aes_string(x = "Col", y = "Row2", fill = col.id), 
                                    show.legend = TRUE) +
                        scale_fill_gradientn(name = paste0("Relative\nAbundance"), 
                                             colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), 
                                             limits = c(0,max.abund), 
                                             guide = "none") +
                        scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), 
                                              limits = c(0,max.abund), 
                                              guide = "none") +
                        ggtitle(landscape.state) +
                        theme_light() +
                        theme(axis.title = element_blank(),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              title = element_text(size=12),
                              axis.line = element_line(colour = "black"),
                              panel.border = element_rect(colour = "black", fill=NA, size=1)))
            
}

#############
# Inclusion #
#############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

inclusion.plot <- function(species.data, threshold) {
            
            # Determine the number of groups so colors are assigned appropriately
            if(length(table(species.data$Inclusion)) == 1) {
                        
                        col.value <- "#ef8a47"
            } else {
                        
                        col.value <- c("#72bcd5", "#ef8a47")
                        
            }
            
            # Create plot
            return(ggplot() +
                        geom_raster(data = species.data, 
                                    aes_string(x = "Col", y = "Row2", fill = "Inclusion"), 
                                    show.legend = TRUE) +
                        scale_fill_gradientn(colors = col.value, 
                                             guide = "none") +
                        scale_color_gradientn(colors = col.value,  
                                              guide = "none") +
                        ggtitle(threshold) +
                        theme_light() +
                        theme(axis.title = element_blank(),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              title = element_text(size=12),
                              axis.line = element_line(colour = "black"),
                              panel.border = element_rect(colour = "black", fill=NA, size=1)))
            
}
