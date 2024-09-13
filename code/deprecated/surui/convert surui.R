# Purpose: Read in KML, visualizes it in two ways, and then convert and write it as a SHP file
# Author: Henry Chen
# Date: 7/18/2023

library(sf)
library(ggplot2)
library(rnaturalearth)

# describe and read in KML
st_layers("data/raw/surui/surui.kml")
kml = read_sf("data/raw/surui/surui.kml")

# just the project boundary
plot(kml)

# Convert 3D polygons to 2D polygons by dropping the Z dimension
kml <- st_zm(kml, drop = TRUE)

# write to SHP file
shp_file <- "data/processed/surui/surui.shp"
st_write(kml, shp_file)

