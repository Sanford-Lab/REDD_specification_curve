# Converting ADPML KML file into shape file

library(sf)

# describe and read in KML
kml <- read_sf("data/raw/maisa/maisa.kml")

# Convert 3D polygons to 2D polygons by dropping the Z dimension
kml <- st_zm(kml, drop = TRUE)

# write to SHP file
shp_file <- "data/processed/maisa/maisa.shp"
st_write(kml, shp_file)

