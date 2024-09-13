# Read in KML, convert to SHP file

# installing packages
install.packages("rworldmap")
install.packages("raster")
install.packages("rgdal")
install.packages("plotKML")
install.packages("maptools")
install.packages("sp")

# loading the library
library(raster)
library(rgdal)
library(plotKML)
library(rworldmap)
library(sp)

# read kml file
require(sf)
kml <- read_sf("data/raw/agrocortex/agrocortex.kml")

# convert to shape file
require(sf)
shape <- read_sf(dsn = "data/processed/agrocortex/agrocortex_1_1-polygon.shp", layer = "agrocortex_1_1-polygon")

