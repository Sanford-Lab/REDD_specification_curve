# Purpose: Read in KML, visualizes it in two ways, and then convert and write it as a SHP file
# Author: Henry Chen
# Date: 6/20/2023

library(sf)
library(ggplot2)
library(rnaturalearth)

# describe and read in KML
st_layers("data/raw/florestal_santa_maria/PA_FSM.kml")
kml = read_sf("data/raw/florestal_santa_maria/PA_FSM.kml")

# visualize
# with brazil backdrop
brazil_data <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")

ggplot() +
  geom_sf(data = brazil_data, fill = "gray", color = "gray") +
  geom_sf(data = kml, fill = "red", color = "red") +
  theme_void() +
  coord_sf(xlim = c(-75, -30), ylim = c(-35, 10), expand = FALSE)

# just the project boundary
plot(kml)

# Convert 3D polygons to 2D polygons by dropping the Z dimension
kml <- st_zm(kml, drop = TRUE)

# write to SHP file
shp_file <- "data/processed/florestal_santa_maria/PA_FSM.shp"
st_write(kml, shp_file)

