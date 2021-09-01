# Example Preamble Code for Spatial Class
# Created: 9/1/21
# Last Edited: 9/1/21
# Author: Noel Johnson
# Notes: This is example preamble code to be used in the exercises for Spatial Class.

####################################
# global libraries used everywhere #
####################################


pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  return("OK")
}

global.libraries <- c("tidyverse", "sf", "haven", "raster", "gstat", "viridis", "tmap")

results <- sapply(as.list(global.libraries), pkgTest)

# library(tidyverse)
# library(sf)
# library(haven)
# library(raster)
# library(gstat)
# library(viridis)
# library(tmap)

dir <- "/Users/noeljohnson/Dropbox/Teaching/Spatial Fall2021/Exercises/Exercise 1"

setwd(dir)
getwd()


#



# End Code