# Converting russas KML file into shape file

library(sf)

# describe and read in KML
kml <- read_sf("data/raw/russas/russas.kml")

# Convert 3D polygons to 2D polygons by dropping the Z dimension
kml <- st_zm(kml, drop = TRUE)

# write to SHP file
shp_file <- "data/processed/russas/russas.shp"
st_write(kml, shp_file)

