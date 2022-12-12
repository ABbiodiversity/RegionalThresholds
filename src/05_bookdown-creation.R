#
# Title: Create the bookdown document
# Created: December 8th, 2022
# Last Updated: December 8th, 2022
# Author: Brandon Allen
# Objective: Stitch together the bookdown pages as preparation for the GitHub page
# Keywords: Bookdown
# Notes: Resources for bookdown documents can be found at https://bookdown.org/yihui/bookdown/github.html
#

############
# Bookdown #
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load libraries
library(bookdown)
library(dplyr)
library(kableExtra)

# Define the working directory
working.dir <- getwd()

# Confirms the nojekyll is created
file.create(paste0(working.dir, "/docs/.nojekyll"))

# Render bookdown
bookdown::render_book(input = paste0(working.dir, "/bookdown/"), 
                      output_format = "bookdown::gitbook",
                      output_dir = paste0(working.dir, "/docs/"))




