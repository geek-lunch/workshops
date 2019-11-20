# Spatial data visualisation
# Alexis Brodeur
# November 20th

## Useful links
# https://www.jessesadler.com/post/geocoding-with-r/
# https://www.jessesadler.com/post/gis-with-r-intro/
# https://r-spatial.github.io/mapview/
# https://geocompr.robinlovelace.net
# https://rstudio.github.io/leaflet/
# https://epsg.io

### 0 : Get Ready ----
library(tidyverse)
library(sf) 
library(osmdata)
library(mapedit)
library(mapview)
library(sp)
library(raster)
library(move)
library(leaflet)
library(leafem)
library(spData)

### 1 : What is spatial data? ----
 # Points : zero-dimensional geometry containing a single point
 # Lines : sequence of points connected by straight, non-self intersecting line pieces; one-dimensional geometry
 # Polygon : geometry with a positive area (two-dimensional); sequence of points from a closed, non-self intersecting ring; the first ring denotes the exterior ring, zero or more subsequent rings denote holes in this exterior ring
 # Rasters : Rectangular grid where each cell contains values. 

### 2 : Spatial data basics  ----
# --> See PDF files sent via email

# Cartesian coordinate system : basic method to locate a point in space.
# Has a unit of measure (cm, m, km)

# GCS : Geographic coordinate system : basic method to locate a point on Earth
  # GSC has a unit of measure (ex : degree), 1st meridian and datum. !!!NO GEOMETRIC PROPERTIES!!! (Do not compute distances, area, etc..)
  # Datum : Used to approximate the surface of the Earth (not a true sphere, ex : Himalaya). Ex : North American Datum (NAD) 1983
  # Longitude, latitude.
  #   As you get closer to the poles, distance between meridians are lower. DO NOT COMPUTE DISTANCE OR AREAS IN A GEOGRAPHIC COORDINATE SYSTEM
  
# This is why there is something called : Projected coordinate systems
  # It restore SOME of the mathematical properties
  # Azimutales :plan plat, 
  # Coniques : cône
  # Cylindrique :cylindre 

# No system is PERFECT (shapes, distances, areas)
  # Conforme : formes et angles
  # Équidistants : distances
  # Équivalents (equal area) : aires

# Example for Quebec : 
  # UTM (17 to 21) : Covers 6ª longitude.
  # MTM (8 zones) : Covers 3ª longitude.
  # Quebec Lambert : Good for the entire province. Centered near Kuujjuaq

# How do you identify these?
  # Each projected system has a code established by different organisms
    # EPSG - European Petroleum Survey Group
    # IGNF - Institut Géographique National de France
    # OSGEO - Open Source Geospatial Foundation
  # Frequently used coordinate systems : 
# EPSG: 4326 - WGS 84
# EPSG: 4269 - NAD83
# EPSG: 26901 à 26923 - NAD83 / UTM zones 1N à 23N
# EPSG: 3347 - NAD83 / Statistics Canada Lambert
# EPSG: 32181 à 32197 - NAD83 / MTM zones 1 à 17
# EPSG: 32198 - NAD83 / Quebec Lambert

# Useful links :
  # https://epsg.io



### 3 : Spatial data in R -----
  #sf : simple features --> adds column called "geometry" to data frame
require(sf)
nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = TRUE)
nc
class(nc) # "sf"         "data.frame"
View(nc) # Notice the data frame format and the geom column
plot(nc) # Plot spatial objects with the first 10 attributes. Be careful with long datasets. It takes time
plot(nc[1]) # Plot spatial object with thematic derived from the first column

#sp : Classes and Methods for Spatial Data
require(sp)
nc_sp <- as_Spatial(nc) # I'll explain later but I transform the sf object to sp
View(nc_sp) # Notice the differences
plot(nc_sp)
plot(nc_sp[1]) # Notice the difference?

## How do you access the data
  # sf
    nc %>%head()
    nc %>% mutate(., new_column =1) %>% head()

  # sp
    nc_sp %>% head()
    nc_sp %>% mutate(., new_column =1) %>% head()
    nc_sp@data %>% mutate(., new_column =1) %>% head()
    # sp are S4 object. It is a bit more complicated. It works with multiple "slots"
    class(nc_sp)
    isS4(nc_sp)
    isS4(nc)
    nc_sp@polygons
    slot(nc_sp,"data")

  #raster
    # You can manage rasters with sf and sp, but for now, the package raster is the best way to go
require(raster)
    # Create raster object
      r <- raster(ncol=10, nrow=10, xmx=-80, xmn=-150, ymn=20, ymx=60)
      r
      plot(r) # There are no assigned values!
      values(r) <- runif(ncell(r))
      r
      plot(r)
      mapview(r)
      # Why is the map over North America!?!?!
      
      # A rasterlayer has only 1 layer
      # Rasterstacks or raster bricks have multiple layers
      r2 <- r * r
      r3  <- sqrt(r)
      s <- stack(r, r2, r3)
      s
      plot(s)
      mapview(s) #Notice how mapview deals with the layers? Wow.
      

      
### 4 : Quick and easy ways to investigate spatial data ----
 # Create spatial data 
villes <- tribble(
  ~what, ~longitude, ~latitude,
  "Verdun", -73.574991,45.4610036,
  "AngeGardien",-73.178624,45.3637861,
  "UL", -71.2769311, 46.7817463,
  "Temiscouata", -68.9726971,47.6733252,
  "SaintRaymond",-71.861434,46.8762241,
  "FermeNeuve",-75.4654088,46.701977,
  "Cantley",-75.9307538, 45.5729678,
  )

print(villes)

### 5 : Draw on a map ---- 
villes_sf =villes %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) #What does crs mean?
mapview(villes_sf)

### 5.5 : Projections in R ---- 
# These are all equal : 
  # +proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
  # EPSG:32198
  # crs = 32198

villes_sf2 =villes %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 32198) 
mapview(villes_sf2)

villes_sf3 =villes %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 
mapview(villes_sf3)

### Be careful when dealing with projections
  # Get and set coordinate reference system
print(villes)
st_crs(villes) # Coordinate Reference System: NA
# But I know these are in Lon/Lat and they refer to WGS84. But when you receive spatial data, you dont always know the projection. This is both risky and tricky!
# How do you assing projection?
# GOOD
(villes %>% st_as_sf(coords = c("longitude", "latitude")))
(villes %>% st_as_sf(coords = c("longitude", "latitude")) %>% st_set_crs(4326))
(villes %>% st_as_sf(coords = c("longitude", "latitude")) %>% st_set_crs("+proj=longlat +datum=WGS84"))
(villes %>% st_as_sf(coords = c("longitude", "latitude")) %>% st_set_crs("+proj=longlat +datum=WGS84")) %>% mapview()

# BAD
(villes %>% st_as_sf(coords = c("longitude", "latitude")) %>% st_set_crs(32198))
(villes %>% st_as_sf(coords = c("longitude", "latitude")) %>% st_set_crs(32198)) %>% mapview()

# How to transform the projection into the desired coordinet system?
  # To carry out a coordinate conversion or transformation, we usest_transform
(villes_sf$geometry)
villes_sf_proj <- villes_sf %>% st_transform(32198)
villes_sf_proj
mapview(villes_sf_proj)

# Usefil tip : create new columns with the x,y data in the new projection
  # st_coordinates() -> get coordinates in matrix form
villes_sf_proj$x_32198 <- st_coordinates(villes_sf_proj)[,1]
villes_sf_proj$y_32198 <- st_coordinates(villes_sf_proj)[,2]
  # Next time you can skip the transformation part

### How to do this with sp?
villes_df <- as.data.frame(villes)
villes_sp <- villes_df
coordinates(villes_sp)<- villes_df[c("longitude", "latitude")]
crs(villes_sp) <- CRS("+proj=longlat +datum=WGS84")
print(villes_sp)
villes_sp_proj <- spTransform(villes_sp, "+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
mapview(villes_sp_proj)

### How to do this with raster?
r
newproj <- "+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
pr1 <- projectRaster(r, crs=newproj)
crs(pr1)
mapview(pr1)

### 6 : Create lines ----
links <- villes_sf %>%
  summarise(st_cast(st_combine(geometry), "LINESTRING"))
list(links, villes_sf) %>%
  mapview()

### 7 : Create polygons ----
polygon <- villes_sf %>%
  slice(c(1:7, 1)) %>%
  summarise(geometry = st_cast(st_combine(geometry), "POLYGON"))
list(links, polygon) %>%
  mapview()

### 8 : Interactive spatial data visualisation -----
# Using mapview
# https://r-spatial.github.io/mapview/
# https://geocompr.robinlovelace.net
require(mapview)
map1 = mapview(villes_sf) 
map2 = mapview(links)
map3 = mapview(polygon)

map1 + map2 + map3

list(villes_sf,links, polygon) %>%
  mapview()

# Example with the brewery dataset and the franconia dataset
require(mapview)
data(breweries)
head(breweries) # sf object with points
data(franconia)
head(franconia) # sf object with polygons

mapview(breweries)

# Select elements from the attribute table to render as a separate layer
mapview(breweries, zcol = c("brewery", "village", "founded"))
 
# Display legend but with many layers, you get too many legends 
mapview(breweries, zcol = "founded", legend = TRUE)

# Change the color of the points, the outsite color and the width of the line
mapview(breweries, color = "cyan", col.regions = "white", lwd = 3)

# There is a lot more you can do. See mapview vignette for more information
  # Change map type : 
mapview(breweries, map.types = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"), color = "grey40")

  # Change layer name
mapview(list(franconia, breweries),
        layer.name = c("Franconian districts", "Franconian breweries"))

  # Use Burst to plot all layers of an object
mapview(breweries, burst = TRUE)

  # All layers shown by default. It makes it hard to see
mapview(breweries, burst = TRUE, hide = TRUE) # Just the 1st layer is shown

  # Use burst and zcol together
breweries %>%
  st_intersection(franconia) %>%
  mapview(zcol = "district", burst = TRUE)

  # Change point size
mapview(breweries, cex = "number.of.types")

  # Change opacity
mapview(breweries, alpha = 0)

  # Multiple layers
m1 = mapview(franconia, zcol = "district", col.regions = c("snow", "grey", "red"))
m1 + breweries

  # Popups
mapview(breweries, popup = popupTable(breweries,
                                      zcol = c("brewery",
                                               "village",
                                               "founded"))) # Click on a point

# View raster
mapview(s, legend = TRUE) # ALL Layers
mapview(s[[2]], legend = TRUE) # 2nd layer


# Multiple panels
sync(map1, map2)
sync(list(map1, map2, map3), ncol =1)

# Using leaflet
# https://rstudio.github.io/leaflet/
require(leaflet) #If you have big dataframes, use leafgl
leaflet(villes_sf) %>% 
  addTiles() %>% 
  addMarkers() %>% 
  leafem::addMouseCoordinates()

# In words : 
# 1. Create a map widget by calling leaflet().
# 2. Add layers (i.e., features) to the map by using layer functions (e.g. addTiles, addMarkers, addPolygons) to modify the map widget.
# 3. Repeat step 2 as desired.
# 4 Print the map widget to display it.

# Leaflet is the most commonly used Javascript library used for maps #think journals
# I wont go into much details because I mostly use mapview but there is a lot of options with leaflet
require(spData)
data("cycle_hire")

pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)
leaflet(data = cycle_hire) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addCircles(col = ~pal(nbikes), opacity = 0.9) %>% 
  addPolygons(data = lnd, fill = FALSE) %>% 
  addLegend(pal = pal, values = ~nbikes) %>% 
  setView(lng = -0.1, 51.5, zoom = 12) %>% 
  addMiniMap()

# Enjoy :) 

### 9 : Edit spatial data ----
require(mapedit)
# Create a study area
study_area <- editMap()
# Edit features
editFeatures(villes_sf)

#Select features
selectFeatures(villes_sf)



### 10 : View graticules  ----
require(leaflet)
leaflet() %>%
  addTiles() %>%
  setView(0, 0, 2) %>%
  addGraticule(interval = 6, style = list(color = "#FF0000", weight = 1))


### 11 : Create quick static maps using ggplot2 and tmaps ----
  #tmap
require(tmap)
# tmap functions like ggplot2 with a grammar of graphics
#The basic building block is tm_shape() (which defines input data, raster and vector objects), 
#followed by one or more layer elements such as tm_fill() and tm_dots()

# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 

# I wont go into much further details but here are some cool things
map_nz = tm_shape(nz) + tm_polygons()

#Multiple maps
tmap_arrange(map_nz, map_nz, map_nz)

# Aesthetics
ma1 = tm_shape(nz) + tm_fill(col = "red")
ma2 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
ma3 = tm_shape(nz) + tm_borders(col = "blue")
ma4 = tm_shape(nz) + tm_borders(lwd = 3)
ma5 = tm_shape(nz) + tm_borders(lty = 2)
ma6 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) +
  tm_borders(col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

# Change colors
tm_shape(nz) + tm_fill(col = "Land_area")
tm_shape(nz) + tm_polygons(col = "Median_income")
breaks = c(0, 3, 4, 5) * 10000
tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks)
tm_shape(nz) + tm_polygons(col = "Median_income", n = 10)
tm_shape(nz) + tm_polygons(col = "Median_income", palette = "BuGn")

# Arrow and scale
map_nz + 
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200))

# Fun stuff
map_nz + tm_style("classic")

## gglot2
# There are lot of functions with ggplot2 to work with sf objects
require(ggplot2)
g1 = ggplot() + geom_sf(data = nz, aes(fill = Median_income)) +
  geom_sf(data = nz_height) +
  scale_x_continuous(breaks = c(170, 175))
g1

# I wont cover mapping with ggplot2 but it is relatively easy with all that new info!
# BE CAREFUL IF GGPLOT2 MAKES ON THE FLY PROJECTIONS. THIS CAN TAKE A LONG LONG LONG TIME


### 12: Export data -----
 # html -> interactive maps
map1
mapshot(map1, url = paste0(getwd(), "/map.html"))
mapshot(map1, file = "map2.html")

# For Rmarkdown
knit_print.mapview(x, ...)

 # thematic maps
# option 1
png(filename = "map3.png", width = 500, height = 350)
plot(villes_sf)
dev.off()

# tmap option
require(tmap)
tmap_obj = tm_shape(world) + tm_polygons(col = "lifeExp")
tmap_save(tm = tmap_obj, filename = "lifeExp_tmap.png")

 # shapefile or other spatial formats
  # with sf
st_write(nc, paste0(tempdir(), "/", "nc.shp"))

# with rgdal
writeOGR(villes_sf, getwd(), "cities", driver="ESRI Shapefile")

 # rasters
x <- writeRaster(s, paste0(getwd(),'/output.tif'))

### 13: Import data ----
# Read SHAPEFILE.shp from the current working directory (".")
require(rgdal)
shape <- readOGR(dsn = ".", layer = "SHAPEFILE")
require(sf)
shape <- read_sf(dsn = ".", layer = "SHAPEFILE")

### 14: Transform between sf, sp and other data types ----
methods(st_as_sf)
methods(st_as_sfc)

# Or
#You can also use as(x, "Spatial") 
#To transform sp objects to sf and sfc with as(x, "sf").

# Since we did not talk a lot about raster data, I'll leave that for an other time :)
