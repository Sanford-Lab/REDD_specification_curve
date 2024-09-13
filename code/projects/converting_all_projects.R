# Author: Henry Chen
# Date: 11/5/2023
# Purpose: Runs all the KML -> SHP file conversions so we don't need all the 12
# folders for each project.

library(here)

source(here("code", "projects", "convert_to_shp_function.R"))
source(here("code", "projects", "universal_list_of_projects.R"))


# to contribute a new project, just simply add the relevant KML files inside the
# /data/ directory and add the project name to this list (along with project start date)

projects <- get_projects() # this is all the projects. alternatively, you can run
                           # each new project individually (e.g. convert_to_shp(new_project))

for (project in projects) {
  name <- project[1]
  convert_to_shp(name)
}
