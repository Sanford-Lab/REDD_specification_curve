library(sf)
library(tidyverse)

# Function to merge all rows of a shapefile and assign the shapefile name as the feature name
merge_shapefile <- function(shapefile_path) {
  shapefile_name <- tools::file_path_sans_ext(basename(shapefile_path))
  single_feature <- st_read(shapefile_path, quiet = TRUE) %>%
    # Repair invalid geometries
    mutate(geometry = st_make_valid(geometry)) %>%
    # Union the geometries
    summarise(geometry = st_union(geometry)) %>%
    # Add the feature name
    mutate(feature_name = shapefile_name)
  single_feature
}

# Get the list of shapefiles
shapefiles <- list.files(path = "data/processed", pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

# Apply the merge_shapefile function to each shapefile and combine them into a single sf object
final_sf_object <- shapefiles %>%
  map(merge_shapefile) %>%
  bind_rows()

# Resulting sf object containing the merged features
final_sf_object


# Get Brazil's outline
brazil_outline <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "Brazil")



library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(grid) # For using grob


# Capitalize the first letter of all feature names
final_sf_object$feature_name <- tools::toTitleCase(final_sf_object$feature_name)
# Change feature name "PA_FSM" to "Florestal"
final_sf_object$feature_name[final_sf_object$feature_name == "PA_FSM"] <- "Florestal"
final_sf_object$feature_name[final_sf_object$feature_name == "Adpml"] <- "ADPML"

# Get coordinates of the PA_FSM feature
PA_FSM_coords <- st_coordinates(st_geometry(final_sf_object[final_sf_object$feature_name == "Florestal",]))[1,]

# Make the inset 2x larger
inset_width <- (bbox[3] - bbox[1]) * 0.4 # 2x the previous width
inset_height <- (bbox[4] - bbox[2]) * 0.4 # 2x the previous height
inset_xmin <- bbox[3] - inset_width
inset_ymin <- bbox[2]
inset_xmax <- bbox[3]
inset_ymax <- bbox[2] + inset_height
inset_ymax <- bbox[2] + inset_height

# Create the main plot
main_plot <- ggplot() +
  geom_sf(data = brazil_outline, color = "black", fill = "grey90", size = 1) + # Actual outline of Brazil
  geom_sf(data = final_sf_object, aes(color = feature_name), size = 1) +      # Merged shapefiles
  theme_bw() +
  coord_sf() +
  theme(legend.title = element_text(size = 10), # Legend title size adjustment
        axis.title.x = element_blank(),         # Remove X axis title
        axis.title.y = element_blank()) +       # Remove Y axis title
  labs(title = "Locations of Existing Carbon Projects for which we have Data, \n with inset for Florestal Santa Maria",
       color = "Project Name") # Change legend title to "Project Name"


# Extract the third color from the default ggplot2 color palette
florestal_color <- scales::hue_pal()(12)[3]

# Create zoom plot with the extracted Florestal color for shading and outline
zoom_plot <- ggplot() +
  geom_sf(data = final_sf_object[final_sf_object$feature_name == "Florestal",],
          fill = florestal_color, color = florestal_color, size = 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey90"),
        plot.title = element_text(size = 12)) +
  coord_sf(xlim = c(zoom_bbox[1], zoom_bbox[3]), ylim = c(zoom_bbox[2], zoom_bbox[4])) +
  labs(title = "Florestal Santa Maria")

# Rest of your plotting code


final_plot <- main_plot +
  annotation_custom(grob = ggplotGrob(zoom_plot), xmin = inset_xmin, xmax = inset_xmax, ymin = inset_ymin, ymax = inset_ymax)

# Add guidelines
final_plot_with_guidelines <- final_plot +
  geom_segment(data = guideline_coords, aes(x = c(PA_FSM_coords[1]), y = c(PA_FSM_coords[2]),
                                            xend = inset_xmin, yend = inset_ymin), color = "grey40", linetype = "dashed") +
  geom_segment(data = guideline_coords, aes(x = c(PA_FSM_coords[1]), y = c(PA_FSM_coords[2]),
                                            xend = inset_xmax, yend = inset_ymax), color = "grey40", linetype = "dashed")

# Print the final plot
final_plot_with_guidelines


