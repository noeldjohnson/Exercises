# These are the solved exercises from Exercise 1
# Created: 9/20/21
# Last Updated: 9/20/21
# Author: Noel Johnson

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

global.libraries <- c("tidyverse", "sf", "stargazer", "units")

results <- sapply(as.list(global.libraries), pkgTest)

# Exercise: Tidyverse
ratings <- read_csv("http://www.stern.nyu.edu/~wgreene/Text/Edition7/TableF4-3.csv")

ratings

ratings_5 <- ratings[,1:5] %>%
  mutate(BOX=BOX/1000000)
ratings_5

ratings_factor <- ratings_5 %>%
  mutate_at(vars(MPRATING),c(factor)) %>%
  mutate(MPAA = fct_recode(MPRATING,
                           "G" = "1",
                           "PG" = "2",
                           "PG13" = "3",
                           "R" = "4"
  ))
ratings_factor

ratings_avgs <- ratings_factor %>% 
  group_by(MPAA) %>%
  select(c(BOX, BUDGET)) %>%
  summarise(mean_BOX = mean(BOX), mean_BUDGET = mean(BUDGET), mean_BOX_BUDGET = mean(BOX/BUDGET)) %>%
  ungroup()

ratings_new <- left_join(ratings_factor, ratings_avgs, by = "MPAA")

ratings_new %>%
  mutate(BOX_BUDGET_dev = mean_BOX_BUDGET - BOX/BUDGET) %>%
  select(-c(mean_BOX,mean_BUDGET,BOX,BUDGET)) %>%
  arrange(MPAA, BOX_BUDGET_dev)

# Exercise: Map Making
africa_sf <- st_read("./data/africa_scale.shp")
head(africa_sf)

# Subset, there are way too many variables
africa_sf <- africa_sf %>%
  select(admin, type, iso_a3, region_wb, pop_est)

# read the csv of african cities
cities_csv <- read_csv("./data/africa_cities.csv")

# filter out missing coords
cities_csv <- cities_csv %>%
  filter( !( is.na(lon) & is.na(lat) ) )

head(cities_csv)

# Find the largest city in each country
largest_city <- cities_csv %>%
  select(iso3v10, pop, nametmp) %>%
  group_by(iso3v10) %>%
  filter(pop == max(pop)) %>%
  ungroup()

# Summary stats for the largest cities in each country
stargazer(as.data.frame(largest_city), type = "text")

# Find the smallest city in each country
smallest_city <- cities_csv %>%
  select(iso3v10, pop, nametmp) %>%
  group_by(iso3v10) %>%
  filter(pop == min(pop)) %>%
  ungroup()

# Summary stats for the smallest cities in each country
stargazer(as.data.frame(smallest_city), type = "text")

# Calculate total urban population for each country
total_urban <- cities_csv %>%
  select(iso3v10, pop, nametmp) %>%
  group_by(iso3v10) %>%
  mutate(total_pop = sum(pop)) %>%
  ungroup

# Create a new variable for each country that is the largest city's population and calculate share of largest city
total_urban <- total_urban %>%
  group_by(iso3v10) %>%
  mutate(primate_pop = max(pop)) %>%
  mutate(primate_share = primate_pop/total_pop) %>%
  ungroup

# Summary stats for the share of largest city in each country (in html--so can be view
stargazer(as.data.frame(total_urban), type = "html", out = "/Users/noeljohnson/Dropbox/Teaching/Spatial Fall2021/Exercises Answers/Excersise_ans_1/largest.html")

# Merge the primate_share variable with the africa_sf shapefile
head(africa_sf)
head(total_urban)

# Looks like the key variable is iso3v10 in the cities data and iso_a3 in the Africa_sf data

# Change the name of the variable in the cities data and drop some variables and rows
merge_urban <- total_urban %>%
  rename(iso_a3 = iso3v10) %>%
  select(iso_a3, pop, primate_share) %>%
  group_by(iso_a3) %>%
  filter(pop == max(pop)) %>%
  ungroup() %>%
  select(iso_a3, primate_share)

# Merge the cities data to the Africa_sf data
africa_new <- left_join(africa_sf, merge_urban, by = "iso_a3")

# Make the plot
plot(africa_new["primate_share"], axes = T,
     main = "Largest City by Share of Urban Pop", key.pos = 1,
     key.width = lcm(1.3), key.length = 1.0)



# Exercise: Projections
#Google the coordinates for Dar es Salaam and Moshi
# Dar Es Salaam lat = -6.8, long = 39.283333
# Moshi lat = -3.334883, long = 37.340381

# Create a tibble and then a simple feature data set containing both cities in EPSG 4326
east_africa <- tibble(
  id = 1:2,
  city = c("Dar Es Salaam", "Moshi"), 
  latitude = c(-6.8, -3.334883), 
  longitude = c(39.283333, 37.340381)
)
east_africa
east_africa_sf <- st_as_sf(east_africa,
                           coords = c("longitude", "latitude"),
                           crs = 4326)

plot(east_africa_sf["city"], axes = T, pch = 20,
     col = "black", main = "Cities")

# Calculate the distance between these two cities using st_distance()
st_distance(east_africa_sf)

#Compare this to the Euclidean distance (use dist() from base R with st_coordinates() and think about angles)
st_coordinates(east_africa_sf)
dist(st_coordinates(east_africa_sf), method = "euclidean")

east_africa_moll <- east_africa_sf %>% st_transform(crs = "+proj=moll +lat_0=-5 +lon_0=38 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
st_crs(east_africa_moll)
st_coordinates(east_africa_moll)
st_distance(east_africa_moll)
dist(st_coordinates(east_africa_moll), method = "euclidean")

east_africa_aeqd <- east_africa_sf %>% st_transform(crs = "+proj=aeqd +lat_0=-5 +lon_0=38 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
st_crs(east_africa_aeqd)
st_coordinates(east_africa_moll)
st_distance(east_africa_aeqd)
dist(st_coordinates(east_africa_aeqd), method = "euclidean")


# Exercise: Road Densities
# Read in the Africa boundaries layer
africa_sf <- st_read("./data/africa_scale.shp")
head(africa_sf)

# Subset, there are way too many variables
africa_sf <- africa_sf %>%
  select(admin, type, iso_a3, region_wb, pop_est)
glimpse(africa_sf)

# Read in the Africa roads layer
africa_roads <- st_read("./data/africa_roads.shp")
glimpse(africa_roads)

# Plot the roads and boundaries in the same map
plot(africa_roads["type"], reset=F, main = "Africa's Roads by Type", key.pos=1)
plot(africa_sf["type"], add=T, col=NA)

# Add the length of each African road as a new variable in the data set and set units to be km's
africa_roads$road_length <- africa_roads %>%
  st_length() %>%
  set_units(km)

# Copy the roads data and delete the geometry column (st_set_geometry(NULL))
africa_roads_df <- africa_roads %>%
  st_set_geometry(NULL)

# Use dplyr verbs to calculate the total length of the road network by country (adm0_a3)
glimpse(africa_roads_df)
africa_roads_df <- africa_roads_df %>%
  group_by(adm0_a3) %>%
  mutate(total_road_length = sum(road_length))
  ungroup()

# Add the area of each country as a new variable to the Africa countries data and set the units to km2
  africa_sf$country_area <- africa_sf %>%
    st_area() %>%
    set_units(km^2)

# What is the correlation between country area and the length of the road network?
# Identify the key variable between the two data sets
glimpse(africa_sf)
glimpse(africa_roads_df)
# Rename adm0_a3 to iso_a3 in the africa_roads_df df
africa_roads_df <- africa_roads_df %>%
  rename(iso_a3 = adm0_a3)
# Collapse the africa_roads_df by country (so there is just one observation per country) and select the variables you want
africa_roads_collapsed <- africa_roads_df %>%
  group_by(iso_a3) %>%
  summarize_all(mean) %>%
  select(-type, -road_length) %>%
  ungroup()

# Merge the two data sets using iso_a3 as the key
africa_new <- left_join(africa_sf, africa_roads_collapsed, by = "iso_a3")
# Calculate the correlation
glimpse(africa_new)
africa_new <- africa_new %>%
  select(admin, iso_a3, country_area, total_road_length)

africa_new %>%
  st_set_geometry(NULL) %>%
  select(-admin, -iso_a3) %>%
  filter( !( is.na(country_area) | is.na(total_road_length))) %>%
  cor()

# Calculate the road density (length of the road network divided by country area)
glimpse(africa_new)
africa_new <- africa_new %>%
  mutate(road_density = total_road_length/country_area)

# Plot these data on a map
plot(africa_new["road_density"], reset=T, main = "African Road Density", key.pos=1)

#

# End Code