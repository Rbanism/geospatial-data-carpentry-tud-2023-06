# Load packages ----
library(tidyverse)
library(here)
library(sf)

# 1. Open and plot shapefiles ---- 
## Import shapefile
boundary_Delft <- st_read(here("data", "delft-boundary.shp"))

## Shapefile metadata and attributes
st_geometry_type(boundary_Delft)

st_crs(boundary_Delft)
st_bbox(boundary_Delft)

boundary_Delft <- st_transform(boundary_Delft, crs = 28992)
st_crs(boundary_Delft)
st_bbox(boundary_Delft)

boundary_Delft

## Plot a shapefile
ggplot(data = boundary_Delft) +
  geom_sf(size = 3, color = "black", fill = "cyan1") +
  ggtitle("Delft Administrative Boundary") +
  coord_sf(datum = st_crs(28992))

## Challenge 1
lines_Delft <- st_read(here("data", "delft-streets.shp"))
point_Delft <- st_read(here("data", "delft-leisure.shp"))

class(lines_Delft)
class(point_Delft)

st_crs(lines_Delft)
st_crs(point_Delft)

st_bbox(lines_Delft)
st_bbox(point_Delft)

# 2. Explore and plot by vector layer attributes ----
lines_Delft

ncol(lines_Delft)
names(lines_Delft)
head(lines_Delft)

## Challenge 2
ncol(point_Delft)
ncol(boundary_Delft)

head(point_Delft)
head(point_Delft, 10)

point_Delft

names(point_Delft)

## Explore values within one attribute
head(lines_Delft$highway, 10)

levels(factor(lines_Delft$highway))

## Subset features
cycleway_Delft <- lines_Delft %>% 
  filter(highway == "cycleway")

nrow(lines_Delft)

nrow(cycleway_Delft)

### We can also calculate the total length of cycleways
cycleway_Delft <- cycleway_Delft %>% 
  mutate(length = st_length(.))

cycleway_Delft %>%
  summarise(total_length = sum(length))

ggplot(data = cycleway_Delft) +
  geom_sf() +
  ggtitle("Slow mobility network of Delft", subtitle = "Cycleways") +
  coord_sf()

## Challenge 3
levels(factor(lines_Delft$highway))

motorway_Delft <- lines_Delft %>% 
  filter(highway == "motorway")

motorway_Delft %>% 
  mutate(length = st_length(.)) %>% 
  select(everything(), geometry) %>%
  summarise(total_length = sum(length))

nrow(motorway_Delft)

ggplot(data = motorway_Delft) +
  geom_sf(size = 1.5) +
  ggtitle("Mobility network of Delft", subtitle = "Motorways") +
  coord_sf()

pedestrian_Delft <- lines_Delft %>% 
  filter(highway == "pedestrian")

nrow(pedestrian_Delft)

ggplot(data = pedestrian_Delft) +
  geom_sf() +
  ggtitle("Slow mobility network of Delft", subtitle = "Pedestrian") +
  coord_sf()

## Customize plots
levels(factor(lines_Delft$highway))

### Select subset of road types
road_types <- c("motorway", "primary", "secondary", "cycleway")

lines_Delft_selection <- lines_Delft %>% 
  filter(highway %in% road_types) %>% 
  mutate(highway = factor(highway, levels = road_types))

### Customize colors
road_colors <- c("blue", "green", "navy", "purple")

ggplot(data = lines_Delft_selection) +
  geom_sf(aes(color = highway)) +
  scale_color_manual(values = road_colors) +
  labs(color = "Road Type") +
  ggtitle("Mobility network of Delft", subtitle = "Roads & Cycleways") +
  coord_sf()

### Adjust line width
line_widths <- c(1, 0.75, 0.5, 0.25)

ggplot(data = lines_Delft_selection) +
  geom_sf(aes(color = highway, size = highway)) +
  scale_color_manual(values = road_colors) +
  scale_size_manual(values = line_widths) +
  labs(color = "Road Type", size = "Road Size") +
  ggtitle("Mobility network of Delft", subtitle = "Roads & Cycleways") +
  coord_sf()

## Challenge 4
levels(factor(lines_Delft$highway))

line_widths <- c(0.25, 0.75, 0.5, 1)

ggplot(data = lines_Delft_selection) +
  geom_sf(aes(size = highway)) +
  scale_size_manual(values = line_widths) +
  labs(size = "Road Size") +
  ggtitle("Mobility network of Delft", subtitle = "Roads & Cycleways - Line width varies") +
  coord_sf()

## Add plot legend
ggplot(data = lines_Delft_selection) +
  geom_sf(aes(color = highway), size = 1.5) +
  scale_color_manual(values = road_colors) +
  labs(color = "Road Type") +
  ggtitle("Mobility network of Delft", subtitle = "Roads & Cycleways - Default legend") +
  coord_sf()

ggplot(data = lines_Delft_selection) +
  geom_sf(aes(color = highway), size = 1.5) +
  scale_color_manual(values = road_colors) +
  labs(color = "Road Type") +
  theme(legend.text = element_text(size = 20),
        legend.box.background = element_rect(size = 1)) +
  ggtitle("Mobility network of Delft", subtitle = "Roads & Cycleways - Modified legend") +
  coord_sf()

new_colors <- c("springgreen", "blue", "magenta", "orange")

ggplot(data = lines_Delft_selection) +
  geom_sf(aes(color = highway), size = 1.5) +
  scale_color_manual(values = new_colors) +
  labs(color = "Road Type") +
  theme(legend.text = element_text(size = 20),
        legend.box.background = element_rect(size = 1)) +
  ggtitle("Mobility network of Delft", subtitle = "Roads & Cycleways - Modified legend") +
  coord_sf()

## Challenge 5
class(lines_Delft_selection$highway)

levels(factor(lines_Delft_selection$highway))

lines_Delft_bicycle <- lines_Delft %>% 
  filter(highway == "cycleway")

ggplot() +
  geom_sf(data = lines_Delft) +
  geom_sf(data = lines_Delft_bicycle, color = "magenta", size = 2) +
  ggtitle("Mobility network of Delft", subtitle = "Roads dedicated to bikes") +
  coord_sf()

## Challenge 6
municipal_boundaries_NL <- st_read(here("data", "nl-gemeenten.shp"))
str(municipal_boundaries_NL)
levels(factor(municipal_boundaries_NL$ligtInPr_1))

ggplot(data = municipal_boundaries_NL) +
  geom_sf(aes(color = ligtInPr_1), size = 1) +
  ggtitle("Contiguous NL Municipal Boundaries") +
  coord_sf()

# 3. Plot multiple shapefiles ----
ggplot() +
  geom_sf(data = boundary_Delft, fill = "grey", color = "grey") +
  geom_sf(data = lines_Delft_selection, aes(color = highway), size = 1) +
  geom_sf(data = point_Delft, aes(fill = leisure), shape = 22) +
  scale_color_manual(values = road_colors, name = "Road Type") +
  scale_fill_manual(values = leisure_colors, name = "Leisure Location") +
  ggtitle("Mobility network and leisure in Delft") +
  coord_sf()

## Challenge 7

### Subset of leisure locations
leisure_locations_selection <- st_read(here("data", "delft-leisure.shp")) %>% 
  filter(leisure %in% c("playground", "picnic_table"))

levels(factor(leisure_locations_selection$leisure))

blue_orange <- c("cornflowerblue", "darkorange")

ggplot() + 
  geom_sf(data = lines_Delft_selection, aes(color = highway)) + 
  geom_sf(data = leisure_locations_selection, aes(fill = leisure), 
          shape = 21, show.legend = 'point') + 
  scale_color_manual(name = "Line Type", values = road_colors,
                     guide = guide_legend(override.aes = list(linetype = "solid", shape = NA))) + 
  scale_fill_manual(name = "Soil Type", values = blue_orange,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = 21, colour = NA))) + 
  ggtitle("Traffic and leisure") + 
  coord_sf()

ggplot() + 
  geom_sf(data = lines_Delft_selection, aes(color = highway), size = 1) + 
  geom_sf(data = leisure_locations_selection, aes(fill = leisure, shape = leisure), size = 3) + 
  scale_shape_manual(name = "Leisure Type", values = c(21, 22)) +
  scale_color_manual(name = "Line Type", values = road_colors) + 
  scale_fill_manual(name = "Leisure Type", values = rainbow(15),
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = c(21, 22),
                                                             color = "black"))) + 
  ggtitle("Road network and leisure") + 
  coord_sf()

# 4. Handle spatial projections ----

municipal_boundary_NL <- st_read(here("data","nl-gemeenten.shp"))

ggplot() +
  geom_sf(data = municipal_boundary_NL) +
  ggtitle("Map of Contiguous NL Municipal Boundaries") +
  coord_sf()

country_boundary_NL <- st_read(here("data", "nl-boundary.shp"))

ggplot() +
  geom_sf(data = country_boundary_NL, color = "gray18", size = 2) +
  geom_sf(data = municipal_boundary_NL, color = "gray40") +
  ggtitle("Map of Contiguous NL Municipal Boundaries") +
  coord_sf()

st_crs(municipal_boundary_NL)
st_crs(country_boundary_NL)

boundary_Delft <- st_read(here("data", "delft-boundary.shp"))
st_crs(boundary_Delft)  ## Different projection
boundary_Delft <- st_transform(boundary_Delft, 28992)

ggplot() +
  geom_sf(data = country_boundary_NL, size = 2, color = "gray18") +
  geom_sf(data = municipal_boundary_NL, color = "gray40") +
  geom_sf(data = boundary_Delft, color = "purple", fill = "purple") +
  ggtitle("Map of Contiguous NL Municipal Boundaries") +
  coord_sf()

## Challenge 8
boundary_ZH <- municipal_boundary_NL %>% 
  filter(ligtInPr_1 == "Zuid-Holland")

ggplot() +
  geom_sf(data = boundary_ZH, aes(color ="color"), show.legend = "line") +
  scale_color_manual(name = "", labels = "Municipal Boundaries", values = c("color" = "gray18")) +
  geom_sf(data = boundary_Delft, aes(shape = "shape"), color = "purple", fill = "purple") +
  scale_shape_manual(name = "", labels = "Municipality of Delft", values = c("shape" = 19)) +
  ggtitle("Delft location") +
  theme(legend.background = element_rect(color = NA)) +
  coord_sf()

# 5. Export a shapefile ----
st_write(leisure_locations_selection,
         here("data_output","leisure_locations_selection.shp"), driver = "ESRI Shapefile")
