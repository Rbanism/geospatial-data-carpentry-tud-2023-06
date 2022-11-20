# Load packages ----
library(tidyverse)  # Meta package for data science
library(here)       # Working with paths
library(raster)     # Accessing and manipulating raster data
library(rgdal)      # Interface to the GDAL utility

# 1. Intro to raster data ----

## View Raster File Attributes
GDALinfo(here("data","tud-dsm.tif"))

## Open a Raster
DSM_TUD <- raster(here("data","tud-dsm.tif"))
DSM_TUD

### More summary statistics
summary(DSM_TUD)
summary(DSM_TUD, maxsamp = ncell(DSM_TUD))

### Visualise the rater
DSM_TUD_df <- as.data.frame(DSM_TUD, xy = TRUE)
str(DSM_TUD_df)

ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y, fill = tud.dsm)) +
  scale_fill_viridis_c() +  # remember, the this color palette was introduced in the first lesson
  coord_quickmap() 

#### Or quick preview
plot(DSM_TUD)

## View raster CRS
crs(DSM_TUD)

## Calculate Min and Max values
minValue(DSM_TUD)
maxValue(DSM_TUD)

DSM_TUD <- raster::setMinMax(DSM_TUD)

minValue(DSM_TUD)
maxValue(DSM_TUD)

## Raster bands
nlayers(DSM_TUD)

## Creating a histogram of raster values
ggplot() +
  geom_histogram(data = DSM_TUD_df, aes(tud.dsm))

ggplot() +
  geom_histogram(data = DSM_TUD_df, aes(tud.dsm), bins = 40)

### Challenge 1 (2 minutes)

# Use `GDALinfo()` to determine the following about the `tud-dsm-hill.tif` file:
#   
#   1. Does this file have the same CRS as `DSM_TUD`?
#   2. What is resolution of the raster data?
#   3. How large would a 5x5 pixel area be on the Earth’s surface?
#   4. Is the file a multi- or single-band raster?

GDALinfo(here("data","tud-dsm-hill.tif"))


# 2. Plot raster data ----

DSM_TUD_df <- DSM_TUD_df %>%
  mutate(fct_elevation = cut(tud.dsm, breaks = 3))

ggplot() +
  geom_bar(data = DSM_TUD_df, aes(fct_elevation))

unique(DSM_TUD_df$fct_elevation)

DSM_TUD_df %>%
  group_by(fct_elevation) %>%
  count()

## Customize cutoff values
custom_bins <- c(-10, 0, 5, 100)

DSM_TUD_df

DSM_TUD_df <- DSM_TUD_df %>%
  mutate(fct_elevation_cb = cut(tud.dsm, breaks = custom_bins))

DSM_TUD_df

unique(DSM_TUD_df$fct_elevation_cb)

ggplot() +
  geom_bar(data = DSM_TUD_df, aes(fct_elevation_cb))

DSM_TUD_df %>%
  group_by(fct_elevation_cb) %>%
  count()

ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y, fill = fct_elevation_cb)) + 
  coord_quickmap()

## Customize colors

terrain.colors(3)

ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y,
                                      fill = fct_elevation_cb)) + 
  scale_fill_manual(values = terrain.colors(3)) + 
  coord_quickmap()

### Or store colors in a variable
my_col <- terrain.colors(3)

ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y,
                                      fill = fct_elevation_cb)) + 
  scale_fill_manual(values = my_col, name = "Elevation") + 
  coord_quickmap()

### Challenge 2 (5 minutes)

# Create a plot of the TU Delft Digital Surface Model (`DSM_TUD`) that has:
#   
# 1. Six classified ranges of values (break points) that are evenly divided among the range of pixel values.
# 2. Axis labels.
# 3. A plot title.

DSM_TUD_df <- DSM_TUD_df %>%
  mutate(fct_elevation_6 = cut(tud.dsm, breaks = 6))

unique(DSM_TUD_df$fct_elevation_6)

my_col <- terrain.colors(6)

ggplot() +
  geom_raster(data = DSM_TUD_df, aes(x = x, y = y,
                                     fill = fct_elevation_6)) +
  scale_fill_manual(values = my_col, name = "Elevation") +
  coord_quickmap() +
  xlab("X") +
  ylab("Y") +
  labs(title = "Elevation Classes of the Digital Surface Model (DSM)")

## Layering rasters
DSM_hill_TUD <- raster(here("data","tud-dsm-hill.tif"))
DSM_hill_TUD

DSM_hill_TUD_df <- as.data.frame(DSM_hill_TUD, xy = TRUE)
str(DSM_hill_TUD_df)

ggplot() +
  geom_raster(data = DSM_hill_TUD_df,
              aes(x = x, y = y, alpha = tud.dsm.hill)) + 
  scale_alpha(range =  c(0.15, 0.65), guide = "none") + 
  coord_quickmap()

ggplot() +
  geom_raster(data = DSM_TUD_df , 
              aes(x = x, y = y, 
                  fill = tud.dsm)) + 
  geom_raster(data = DSM_hill_TUD_df, 
              aes(x = x, y = y, 
                  alpha = tud.dsm.hill)) +  
  scale_fill_viridis_c() +  
  scale_alpha(range = c(0.15, 0.65), guide = "none") +  
  ggtitle("Elevation with hillshade") +
  coord_quickmap()

### Challenge 3 (8 minutes)
# Use the `tud-dtm.tif` and `tud-dtm-hill.tif` files from the `data` directory to create a Digital Terrain Model map of the TU Delft area.
# 
# Make sure to:
#   
# - include hillshade in the maps,
# - label axes,
# - include a title for each map,
# - experiment with various alpha values and color palettes to represent the data.

# import DTM
DTM_TUD <- raster(here("data","tud-dtm.tif"))
DTM_TUD_df <- as.data.frame(DTM_TUD, xy = TRUE)

# DTM Hillshade
DTM_hill_TUD <- raster(here("data","tud-dtm-hill.tif"))
DTM_hill_TUD_df <- as.data.frame(DTM_hill_TUD, xy = TRUE)

ggplot() +
  geom_raster(data = DTM_TUD_df ,
              aes(x = x, y = y,
                  fill = tud.dtm,
                  alpha = 2.0)
  ) +
  geom_raster(data = DTM_hill_TUD_df,
              aes(x = x, y = y,
                  alpha = tud.dtm.hill)
  ) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar()) +
  scale_alpha(range = c(0.4, 0.7), guide = "none") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("DTM with Hillshade") +
  coord_quickmap()

# 3. Reproject raster data ----

DTM_TUD <- raster(here("data","tud-dtm.tif"))
DTM_hill_TUD <- raster(here("data","tud-dtm-hill-ETRS89.tif"))

DTM_TUD_df <- as.data.frame(DTM_TUD, xy = TRUE)
DTM_hill_TUD_df <- as.data.frame(DTM_hill_TUD, xy = TRUE)

# try to plot them together
ggplot() +
  geom_raster(data = DTM_TUD_df , 
              aes(x = x, y = y, 
                  fill = tud.dtm)) + 
  geom_raster(data = DTM_hill_TUD_df, 
              aes(x = x, y = y, 
                  alpha = tud.dtm.hill.ETRS89)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()

# examine the rasters separately
ggplot() +
  geom_raster(data = DTM_TUD_df,
              aes(x = x, y = y,
                  fill = tud.dtm)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) +
  coord_quickmap()

ggplot() +
  geom_raster(data = DTM_hill_TUD_df,
              aes(x = x, y = y,
                  alpha = tud.dtm.hill.ETRS89)) +
  coord_quickmap()

### Challenge 4 (2 minutes)

# View the CRS for each of these two datasets. What projection does each use?

crs(DTM_TUD)
crs(DTM_hill_TUD)

## Reproject rasters

DTM_hill_EPSG28992_TUD <- projectRaster(DTM_hill_TUD,
                                        crs = crs(DTM_TUD))
crs(DTM_hill_EPSG28992_TUD)
crs(DTM_hill_TUD)

extent(DTM_hill_EPSG28992_TUD)
extent(DTM_hill_TUD)

### Question
# Why do you think the two extents differ?

## Dealing with raster resolution
res(DTM_hill_EPSG28992_TUD)
res(DTM_TUD)

DTM_hill_EPSG28992_TUD <- projectRaster(DTM_hill_TUD,
                                        crs = crs(DTM_TUD),
                                        res = res(DTM_TUD))

res(DTM_hill_EPSG28992_TUD)
res(DTM_TUD)

### Plot the two layers
DTM_hill_TUD_2_df <- as.data.frame(DTM_hill_EPSG28992_TUD, xy = TRUE)
ggplot() +
  geom_raster(data = DTM_TUD_df , 
              aes(x = x, y = y, 
                  fill = tud.dtm)) + 
  geom_raster(data = DTM_hill_TUD_2_df, 
              aes(x = x, y = y, 
                  alpha = tud.dtm.hill.ETRS89)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()

# 4. Raster calculations ----
## See slides with explanation of DSM and DTM

GDALinfo(here("data","tud-dtm.tif"))
GDALinfo(here("data","tud-dsm.tif"))

ggplot() +
  geom_raster(data = DTM_TUD_df , 
              aes(x = x, y = y, fill = tud.dtm)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()

ggplot() +
  geom_raster(data = DSM_TUD_df , 
              aes(x = x, y = y, fill = tud.dsm)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()

## Raster math and Canopy Height Models
CHM_TUD <- DSM_TUD - DTM_TUD
CHM_TUD_df <- as.data.frame(CHM_TUD, xy = TRUE)

ggplot() +
  geom_raster(data = CHM_TUD_df , 
              aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) + 
  coord_quickmap()

ggplot(CHM_TUD_df) +
  geom_histogram(aes(layer))

### Challenge 5 (5 minutes)

# It’s often a good idea to explore the range of values in a raster dataset just like we might 
# explore a dataset that we collected in the field.
# 
# 1. What is the min and maximum value for the Canopy Height Model `CHM_TUD` that we just created?
# 2. What are two ways you can check this range of data for `CHM_TUD`?
# 3. What is the distribution of all the pixel values in the CHM?
# 4. Plot a histogram with 6 bins instead of the default and change the color of the histogram.
# 5. Plot the CHM_HARV raster using breaks that make sense for the data. Include an appropriate 
# color palette for the data, plot title and no axes ticks / labels.

min(CHM_TUD_df$layer, na.rm = TRUE)
max(CHM_TUD_df$layer, na.rm = TRUE)

ggplot(CHM_TUD_df) +
  geom_histogram(aes(layer))

ggplot(CHM_TUD_df) +
  geom_histogram(aes(layer), colour="black", 
                 fill="darkgreen", bins = 6)

custom_bins <- c(0, 10, 20, 30, 100)
CHM_TUD_df <- CHM_TUD_df %>%
  mutate(canopy_discrete = cut(layer, breaks = custom_bins))

ggplot() +
  geom_raster(data = CHM_TUD_df , aes(x = x, y = y,
                                      fill = canopy_discrete)) + 
  scale_fill_manual(values = terrain.colors(4)) + 
  coord_quickmap()

## Export a GeoTIFF
writeRaster(CHM_TUD, here("fig_output","CHM_TUD.tiff"),
            format="GTiff",
            overwrite=TRUE,
            NAflag=-9999)

# 5. Work with multi-band rasters ----

RGB_band1_TUD <- raster(here("data","tudlib-rgb.tif"))
RGB_band1_TUD_df  <- as.data.frame(RGB_band1_TUD, xy = TRUE)

ggplot() +
  geom_raster(data = RGB_band1_TUD_df,
              aes(x = x, y = y, alpha = tudlib.rgb)) + 
  coord_quickmap()  # use `coord_equal()` instead

RGB_band1_TUD

nbands(RGB_band1_TUD)

RGB_band2_TUD <- raster(here("data","tudlib-rgb.tif"), band = 2)
RGB_band2_TUD_df <- as.data.frame(RGB_band2_TUD, xy = TRUE)

ggplot() +
  geom_raster(data = RGB_band2_TUD_df,
              aes(x = x, y = y, alpha = tudlib.rgb)) + 
  coord_equal() 

## Raster stacks
RGB_stack_TUD <- stack(here("data","tudlib-rgb.tif"))
RGB_stack_TUD

RGB_stack_TUD@layers

RGB_stack_TUD[[2]]

RGB_stack_TUD_df  <- as.data.frame(RGB_stack_TUD, xy = TRUE)

str(RGB_stack_TUD_df)

ggplot() +
  geom_histogram(data = RGB_stack_TUD_df, aes(tudlib.rgb.1))

ggplot() +
  geom_raster(data = RGB_stack_TUD_df,
              aes(x = x, y = y, alpha = tudlib.rgb.2)) + 
  coord_equal()

## Create a three-band image

plotRGB(RGB_stack_TUD,
        r = 1, g = 2, b = 3)

plotRGB(RGB_stack_TUD,
        r = 1, g = 2, b = 3,
        scale = 800,
        stretch = "lin")

plotRGB(RGB_stack_TUD,
        r = 1, g = 2, b = 3,
        scale = 800,
        stretch = "hist")

## RasterStack vs. RasterBrick

object.size(RGB_stack_TUD)

RGB_brick_TUD <- brick(RGB_stack_TUD)

object.size(RGB_brick_TUD)

plotRGB(RGB_brick_TUD)

