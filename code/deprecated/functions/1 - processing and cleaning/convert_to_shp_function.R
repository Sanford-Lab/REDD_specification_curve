# Author: Henry Chen
# Date: 11/4/2023

# Assumptions:
# - kml file is saved in /data/raw/<project_name>/<project_name>.kml

library(sf)

convert_to_shp <- function(project_name) {
  # describe and read in KML
  raw_file_path <- paste("data/raw/", project_name, "/", project_name, ".kml", sep = "")
  processed_file_path <- paste("data/processed/", project_name, "/", sep = "")
  
  shp_file <- paste("data/processed/", project_name, "/", project_name, ".shp", sep = "")
  
  if (file.exists(shp_file)) {
    # file exists no need to create it again.
    print("File exists!")
  } else {
    # file doesn't exist, so create it...
    kml <- read_sf(raw_file_path)
    
    # Convert 3D polygons to 2D polygons by dropping the Z dimension
    kml <- st_zm(kml, drop = TRUE)
    
    # write to SHP file
    st_write(kml, processed_file_path)
  }
}