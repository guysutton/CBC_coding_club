###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #10: Import and plot shapefile 
# - Rhodes Universtiy
# - Script written: 30/06/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Import a shapefile into R 
# - 2. Plot the shapefile as a map
# - 3. Edit map
# - 4. Plot some points (e.g. GPS points for sample sites) over 
#      the shapefile 
############################################################

# Load required packages ---------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               spocc, 
               ggmap, 
               maptools, 
               maps, 
               ggplot2,
               scrubr, 
               mapr, 
               tidyr, 
               stringr,
               rnaturalearth, 
               rnaturalearthdata, 
               rlang, 
               sf, 
               ggspatial,
               raster,
               here)

# Set global ggplot theme --------------------------------------------

# All the plots will automatically take on my custom theme
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", 
                                              fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), 
                                                            "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), 
                                                            "mm")),
                  legend.position = "none"))

# Import shapefile ---------------------------------------------------

# Here, we are going to import the shapefile for the biomes of RSA 
# Shapefile (.shp) must be in a folder, with all the other 
# files (e.g. .ndf, .lyr,)
biome_shp <- here::here("./shapefiles/vegm2006_biomes_withforests/vegm2006_biomes_withforests.shp") %>%
  st_read()

# Plot basic biome map
ggplot(data = biome_shp) +
  geom_sf()

# How do we colour by biome? 
# We will need to look at the structure of the shapefile,
# and find a categorical variable with the biomes names in. 
str(biome_shp)

# What levels are present within BIOME?
# First, convert shapefile into a dataframe (i.e. tibble)
biome_df <- as_tibble(biome_shp)

biome_df %>%
  distinct(BIOME)

# What levels are present within BIOME?
biome_df %>%
  distinct(BIOMENAME)

# Same thing. So let's use BIOME as our fill variable. 

# Plot biome map coloured by biome  ----------------------------------

# Plot basic biome map coloured by biome 
ggplot(data = biome_shp) +
  geom_sf(aes(fill = BIOME),
          alpha = 0.6) +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "YlOrBr") +
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.175, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

# Save a high-quality graph --------------------------------------

ggsave("./figures/fig_3_biome_map.png",
       # Quality/resolution of fig - 600 = publication quality
       dpi = 600,
       # Control figure height and width
       # Usually have to play around with this 
       height = 7, 
       width = 9)

# Import shapefile of provinces --------------------------------------

province_shp <- here::here("./shapefiles/ZAF_adm/ZAF_adm1.shp") %>%
  st_read()

# Add provinces shapefile to map -------------------------------------

# Plot basic biome map coloured by biome 
# - Here, we are also adding a province shapefile 
ggplot(data = biome_shp) +
  geom_sf(aes(fill = BIOME),
          alpha = 0.6) +
  geom_sf(data = province_shp, aes(fill = NA),
          colour = "black",
          size = 0.35) +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "YlOrBr") +
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.175, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave("./figures/fig_3_biome_map.png",
       # Quality/resolution of fig - 600 = publication quality
       dpi = 600,
       # Control figure height and width
       # Usually have to play around with this 
       height = 7, 
       width = 9)

# Import sample GPS points and add to map ----------------------------

# Import GPS points from excel file
sample_gps <- readr::read_csv("./data_raw/snake_gps_points_chad.csv")

# Check data import 
head(sample_gps)

# Drop the unnecessary columns
sample_gps <- sample_gps %>%
  janitor::clean_names() %>%
  dplyr::select(-c(x5:x8))
head(sample_gps)

# Here, I manually create a vector of the colours I want to use 
# for the different biomes, as I can't find a palette that reproduces 
# Chad's original colour scheme. 
# - We will pass this vector to scale_fill_manual to say that 
#   the fill option should take our manual colours 
cols <- c("Albany Thicket Biome" = "lightgoldenrodyellow", 
          "Desert Biome" = "lightgoldenrod3", 
          "Forests" = "darkgoldenrod2", 
          "Fynbos Biome" = "lightgoldenrod1",
          "Grassland Biome" = "peachpuff1",
          "Indian Ocean Coastal Belt" = "grey80",
          "Nama-Karoo Biome" = "olivedrab3",
          "Savanna Biome" = "goldenrod4",
          "Succulent Karoo Biome" = "navajowhite4")

# Plot our final map
ggplot() +
  geom_sf(data = biome_shp,
          aes(fill = BIOME),
          alpha = 0.4) +
  geom_sf(data = province_shp, 
          aes(fill = NA),
          colour = "black",
          size = 0.35) +
  geom_point(data = sample_gps, aes(x = longitude, 
                                    y = latitude,
                                    colour = grouping),
             size = 5) +
  scale_colour_manual(values = c("red", "blue", "green", "orange")) +
  theme(legend.position = "right") +
  scale_fill_manual(values = cols) + 
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome",
       colour = "Clade") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.325, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

# Save your map to PC 
ggsave("./figures/fig_3_biome_map.png",
       # Quality/resolution of fig - 600 = publication quality
       dpi = 600,
       # Control figure height and width
       # Usually have to play around with this 
       height = 12, 
       width = 12)
