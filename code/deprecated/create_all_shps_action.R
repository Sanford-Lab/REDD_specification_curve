# Author: Henry Chen
# Date: 11/5/2023
# Function: Runs all the kml -> shp file conversions so we don't need all the 12
# folders for each project.

source("code/functions/1 - processing and cleaning/convert_to_shp_function.R")

projects <- list(c("adpml", 9),
                 c("agrocortex", 14),
                 c("florestal", 9),
                 c("jari", 11),
                 c("maisa", 12),
                 c("manoa", 13),
                 c("purus", 11),
                 c("riopreto", 12),
                 c("rmdlt", 8),
                 c("russas", 11),
                 c("surui", 9),
                 c("valparaiso", 11))

for (project in projects) {
  name <- project[1]
  convert_to_shp(name)
}