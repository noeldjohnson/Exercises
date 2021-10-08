# Code for Exercises in Lecture 4: Spatial Econometrics
# 9/17/19
# Author: Noel D. Johnson

library(raster)
library(sf)
library(tidyverse)
library(stargazer)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/")

# Exercise 1: Raster Subsetting
# 1. Open the Kenya population GeoTiff (pop_2000.tif). How many rows, columns and cells does the raster have? Which native projection are the data in?

r.pop <- raster("./raster_kenya/pop_2000.tif")
nrow(r.pop)
ncol(r.pop)
ncell(r.pop)
crs(r.pop)
#plot(r.pop)

#2. Calculate the total population in the raster using cell statistics.
cellStats(r.pop, 'sum')
# or, in millions...
cellStats(r.pop, 'sum')/1e6

# 3. Figure out the bounding box of Nairobi using the "kenya_counties" data set. Subset the raster to this extent and then calculate the total population in this new raster using cell statistics. Does this match your idea of the size of Nairobi?
counties <- st_read("./raster_kenya/kenya_counties.shp")
counties %>% names()
glimpse(counties)
nairobi <- counties %>%
  filter(ADM1_EN == "Nairobi") %>%
  st_transform("+proj=moll")
nairobi
# bbox: xmin: 3673915 ymin: -178307 xmax: 3718075 ymax: -143174.2
n.box <- extent(3673915, 3718075, -178307, -143174.2)
r.nai <- r.pop[n.box, drop=F]
cellStats(r.nai, 'sum')/1e6
plot(r.nai, reset=F)
plot(nairobi[1], col=NA, add=T)

# 4. Open the light raster from 2000 (F142000.tif) and try to align the population raster to match it. Should you resample or project? Compare the total population to your answer from 2.
r.light <- raster("./raster_kenya/F142000.tif")
r.light
r.pop
r.pop.wgs <- projectRaster(r.pop, r.light)
r.pop.wgs
r.light
ncell(r.pop)
ncell(r.pop.wgs)
cellStats(r.pop.wgs, 'sum')/1e6
plot(r.light, reset=F)
plot(r.pop.wgs, alpha=.5, add=T)


# 5. Aggregate the aligned population grid by factor 2, compute the total population and compare the total population again.
r.pop.wgs.coarse <- aggregate(r.pop.wgs, fact=2, fun=sum)
ncell(r.pop.wgs.coarse)
cellStats(r.pop.wgs.coarse, 'sum')/1e6


# Exercise 2

rm(list=ls())

library(raster)
library(sf)
library(tidyverse)
library(stargazer)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/")

# 1. Reclassify the raster values and create a new raster which codes urban (2) as >= 150 people per km2, rural (1) as < 150 people per km2 and unpopulated (0) as a population of zero. What is the degree of urbanization in the entire raster?
r.pop <- raster("./raster_kenya/pop_2000.tif")
r.pop.class <- reclassify(r.pop, c(-Inf,0,0, 0,149, 1, 149, Inf, 2))
freq.urban <- freq(r.pop.class)

freq(r.pop.class)[3,2]/ncell(r.pop.class)*100 # in %
# Or...
42521/ncell(r.pop.class)*100

plot(r.pop.class)

# 2. Smooth your urban and rural clusters a bit by calculating the maximum of the set {0,1,2} in the nine surrounding cells. Compare the degree of urbanization.
r.pop.foc <- focal(r.pop.class, fun = max,
                   w = matrix(1, nrow = 3, ncol = 3))
freq(r.pop.foc)[3,2]/ncell(r.pop.class)*100 # in %
plot(r.pop.foc)

# 3. Align the population raster to the light raster. Use overlay() to calculate light per capita in each aligned cell.
r.light <- raster("./raster_kenya/F142000.tif")
r.pop.foc.wgs <- projectRaster(r.pop.foc, r.light, method='ngb')
r.pop.wgs <- projectRaster(r.pop, r.light, method='ngb')
r.lpc <- overlay(r.light, r.pop.wgs,
                 fun = function(x,y) { x/y })
r.lpc
# to make the infinities zeroes...
r.lpc[r.lpc==Inf] <- 0
r.lpc
plot(r.lpc)

#4. Use zonal statistics to calculate average light per capita in unpopulated, rural and urban areas.
zonal(r.light, r.pop.foc.wgs, fun = 'sum')
zonal(r.lpc, r.pop.foc.wgs, fun = 'mean')


# Exercise 3
# Let’s learn how to properly calculate light densities.

rm(list=ls())

library(raster)
library(sf)
library(tidyverse)
library(stargazer)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/")

#1. Open the lights raster (F142000.tif), the land area raster (land_area.tif) and the county shapes of Kenya (kenya_counties.shp). Stack both rasters. Crop and then mask the stack using Kenya’s county boundaries.
r.light <- raster("./raster_kenya/F142000.tif")
r.area <- raster("./raster_kenya/land_area.tif")
v.ken.sf <- st_read("./raster_kenya/kenya_counties.shp", quiet=T)

# get adm name only and make sure that dplyr::select works
v.ken.sp <- v.ken.sf %>% select(ADM1_EN) %>% as("Spatial")
r.stack <- stack(r.light, r.area)
r.stack <- crop(r.stack, v.ken.sp)
r.stack <- mask(r.stack, v.ken.sp)

#2. Compute a new raster by multiplying the light intensity raster times the land area raster using overlay(). Add this new layer to the previous stack.
newlayer <- overlay(r.stack, fun=function(x,y) {x*y} )
r.stack <- stack(r.stack, newlayer)
names(r.stack) <- c("light", "area", "light.area")
r.stack

# 3. Extract the sum of all three layers within each county and add these data to the administrative boundaries file. Compute a new variable by dividing the sum of light intensities times land areas by the total land area in each region.

# call as raster::extract to avoid package conflict
ext.df <- raster::extract(r.stack, v.ken.sp, fun='sum', na.rm=T, df=T)
v.ken.sf <- v.ken.sf %>% mutate(ID = 1:nrow(.)) %>%
  left_join(ext.df, by = "ID") %>% mutate(meanlight = light.area/area)

# 4. Plot this new variable or the log of this variable plus 0.01. Think about what you calculated. How does this calculation take care of the spherical nature of the degree grid?
v.ken.sf <- v.ken.sf %>% mutate(lnlightdens = log(meanlight + 0.01))
plot(v.ken.sf["lnlightdens"], main="Luminosity", breaks="kmeans")


# Exercise 4
# We will now try to detect cities using the population raster

rm(list=ls())

library(raster)
library(sf)
library(tidyverse)
library(stargazer)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/")

#1. Open the Kenya population raster (pop_2000.tif). Crop and mask it to the Kenyan county boundaries.
r.pop <- raster("./raster_kenya/pop_2000.tif")
v.ken.sf <- st_read("./raster_kenya/kenya_counties.shp", quiet=T) %>% st_transform("+proj=moll")
v.ken.sp <- v.ken.sf %>% as("Spatial")
r.pop <- crop(r.pop, v.ken.sp)
r.pop <- mask(r.pop, v.ken.sp)

#2. Reclassify the raster, setting all pixels with a population below 1500 people per km2 to NA. Find clusters of pixels and vectorize the result.
r.pop.class <- reclassify(r.pop, c(-Inf,1499, NA))
r.pop.clump <- clump(r.pop.class)
v.ken.pol <- rasterToPolygons(r.pop.clump) %>% st_as_sf()
v.ken.pol <- v.ken.pol %>%
  group_by(clumps) %>% summarize()
plot(v.ken.pol)

#3. Open the African cities data (africa_ctys.shp) and keep only cities located in Kenya. Select the variables name, pop and iso3v10 only. Mind the projection.
afr.ctys <- st_read("./data/africa_ctys.shp", quiet=T,
                    stringsAsFactors = F) %>%
  st_transform("+proj=moll") %>%
  select(name, pop, iso3v10) %>%
  filter(iso3v10=="KEN")

# 4. Spatially join the clustered polygons to the city points if they are within 1 km of each other. Retain only the biggest city in terms of their population if there are multiple matches for some clusters. How many cities were you able to match? Is the matching unique in terms of city names?
cities.sf <- v.ken.pol %>% st_join(afr.ctys, join = st_is_within_distance, dist=1000) %>%
  group_by(clumps) %>% mutate(biggest = max(pop)) %>%
  filter(pop == biggest) %>% ungroup()
# matching is unique
table(cities.sf$name)

#5. If you have time, extract the population in each city polygon from the raster. Compare this estimate to the variable pop.
cities.ex <- raster::extract(r.pop, cities.sf %>% as("Spatial"),
                             fun=sum, na.rm=T, df=T)
cities.sf <- cities.sf %>% mutate(ID = 1:nrow(.)) %>%
  left_join(cities.ex, by = "ID") %>% mutate(popdiff = (pop_2000-pop)/1000)
summary(cities.sf$popdiff)
# not bad on average but some major discrepancies in some cities
# between our cluster and the administrative definition

#


















# End Code