###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #4: Download GBIF data and make basic distribution map
# - Rhodes Universtiy
# - Script written: 21/05/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Download GBIF data for a single species into R
# - 2. Download GBIF data for multiple species simultaneously into R 
# - 3. Plot three types of distribution map
#      - i) All records
#      - ii) Presence/absence per quarter degree grid cell (like SAPIA maps)
#      - iii) Abundance per quarter degree grid cell
# - 4. Combine two maps into a single figure
# - 5. Save images from R to your PC (NB!!!)
# - 6. Import your own GPS coords and plot maps 
############################################################

# This new line of code should install and load all the packages you require
# No need to call library(package_name) - pacman does this for us 
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
               raster)

#################################################
# Section 1: Downloading GPS occurences from GBIF - single species
#################################################

# 1. Downloading GPS for a single species 
#    - Extract all Opuntia stricta records from GBIF 
#    - Limit to only South African records

# List the countries we want records from 
gbifopts <- list(country = "ZA") 

# Now extract records
df <- occ(query = "Opuntia stricta", 
          from = c("gbif"),
          gbifopts = gbifopts,
          # Limit the maximum number of records
          limit = 10000)
df

# Process data 
df <- fixnames(df, 
               how = 'shortest')
df_comb <- occ2df(df)
df_comb <- dframe(df_comb) %>%
  coord_impossible() %>%
  coord_incomplete() %>%
  coord_unlikely()

df_comb$name <- "Opuntia stricta"
df_comb$plant_species <- df_comb$name
df_comb <- df_comb %>%
  dplyr::select(plant_species, longitude, latitude)
df_comb

#################################################
# Section 2: Downloading GPS occurences from GBIF - multiple species
#################################################

# 2. Downloading GPS for multiple species in same genus 
#    - Extract all Opuntia spp. records from GBIF 
#    - Limit to only South African records

# Limit to only South African records
gbifopts <- list(country = "ZA") 

# Now extract records
df <- occ(query = "Opuntia", 
          from = c("gbif"),
          gbifopts = gbifopts,
          limit = 10000)
df

# What about records for different genera? 
#df <- occ(query = c("Opuntia stricta", "Acacia cyclops"), 
#          from = c("gbif"),
#          gbifopts = gbifopts,
#          limit = 10000)
#df

# Process data 
df_comb <- occ2df(df)
df_comb
df_comb<- dframe(df_comb) %>%
  coord_impossible() %>%
  coord_incomplete() %>%
  coord_unlikely()

# Remove all the authorities 
# - Extract only the first two words 
df_comb <- df_comb %>%
  mutate(plant_species = stringr::word(name, 1, 2))
df_comb

# Keep only columns of interest
df_comb <- df_comb %>%
  dplyr::select(plant_species, longitude, latitude)
df_comb

# Which species do we have in our dataset? 
df_comb %>% count(plant_species)

#################################################
# Section 3: Make basic distribution maps
#################################################

# DISCLAIMER: 
# - These are my own personal functions that I have written.
# - They are not yet available in R, and all rights belong to
#   me, so please use the functions, 
#   but please just chat to me before you publish 
#   any maps using these functions, if you choose to do so. 

# Load function from my GitHub page
devtools::source_url("https://raw.githubusercontent.com/guysutton/sapia_type_maps/master/scripts/working_functions/01_sapia_type_map.R")

# Data format required:
# Data frame (check using class(object_name))
# Must have a column containing species names
# Must have latitude and longitude column, pay attention to capitalisation
# Must be: longitude, latitude, not lat/long, Longitude/Latitude
head(df_comb)

# 1. Plot all GPS records for O. ficus-indica
map_sapia(data = df_comb, 
          # Which map do you want?
          map_type = "abundance",
          # Which column contains the species to plot
          col = plant_species,
          # Which species should we plot? 
          species = c("Opuntia ficus-indica"))

ggsave("./figures/tut3_all_records_map.png",
       width = 8,
       height = 6,
       dpi = 600)

# 2. Presence/absense plot for O. ficus-indica
map_sapia(data = df_comb, 
          # Which map do you want?
          map_type = "presence",
          # Which column contains the species to plot
          col = plant_species,
          # Which species should we plot? 
          species = "Opuntia ficus-indica")

ggsave("./figures/tut3_pres_abs_map.png",
       width = 8,
       height = 6,
       dpi = 600)

# 3. Abundance plot for O. ficus-indica
map_sapia(data = df_comb, 
          # Which map do you want?
          map_type = "abundance",
          # Which column contains the species to plot
          col = plant_species,
          # Which species should we plot? 
          species = "Opuntia ficus-indica") 

ggsave("./figures/tut3_abundance_map.png",
       width = 8,
       height = 6,
       dpi = 600)

#################################################
# Section 4: Combine maps in single figure
#################################################

# Let's say we want to plot two maps, one for O. ficus-indica,
# and one for O. stricta in the same figure. 

# Assign each map to a variable 
map_ficus <- map_sapia(data = df_comb, 
                       # Which map do you want? 
                       map_type = "abundance", 
                       # Which column contains the species to plot
                       col = plant_species,
                       # Which species should we plot? 
                       species = "Opuntia ficus-indica") + 
  labs(title = "(a)")

map_stricta <- map_sapia(data = df_comb, 
                         # Which map do you want? 
                         map_type = "abundance", 
                         # Which column contains the species to plot
                         col = plant_species,
                         # Which species should we plot? 
                         species = "Opuntia stricta") +
  labs(title = "(b)")

# Plot maps on same figure 
library(cowplot)
cowplot::plot_grid(map_ficus, 
                   map_stricta)

# Save map to PC
ggsave("./figures/tut3_combined_map.png",
       width = 15,
       height = 10,
       dpi = 600)

#################################################
# Section 5: Mapping your own data 
#################################################

# Import your own GPS records
bp_gps <- readxl::read_xlsx("./data_raw/peppertree_gps.xlsx")

# Check data import
head(bp_gps)

# Process data to usable format
bp_gps_clean <- bp_gps %>%
  # Clean column names
  janitor::clean_names() %>%
  # Change character columns to numeric
  dplyr::mutate(latitude = as.numeric(latitude),
                longitude = as.numeric(longitude),
                plant_species = "Schinus terebinthifolia")
head(bp_gps_clean)

# Now make maps 
map_sapia(data = bp_gps_clean, 
          col = plant_species,
          species = "Schinus terebinthifolia",
          map_type = "abundance") 

# Save map to PC
ggsave("./tables/tut3_schinus_pres_map.png",
       width = 8,
       height = 6,
       dpi = 600)

# Just leaving these here for now
# Load required libraries
library(tidyverse)
library(spocc)
library(ggmap)
library(maptools)
library(maps)
library(ggplot2) 
library(scrubr)
library(mapr)
library(dplyr)
library(tidyr)
library(stringr) 

# These are required to run map_sapia function
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rlang)
library(ggspatial)
library(raster)
