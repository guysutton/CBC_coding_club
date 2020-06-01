###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #5: Logistic/binomial regression (general linear model)
# - Rhodes Universtiy
# - Script written: 27/05/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Understand when to use a logistic regression 
# - 2. Run a logistic regression model
# - 3. Evaulate the fit of your model (perform model diagnostics)
# - 4. Extract summary statistics from your model
# - 5. Make predictions from your model 
############################################################

# Load required libraries
library(tidyverse)
library(tidyr)

# Import data
data <- readxl::read_excel("./data_raw/messy_data.xlsx")

# Check data imported properly
str(data)
head(data)

# This data is a MESS!!! We need to clean this before we can do anything. 

###
# - Basics of dplyr - data manipulation and cleaning 
###

# 1. Cleaning column names 
#    - janitor::clean_names() to do a broad-scale cleaning of column names
data <- data %>%
  janitor::clean_names()
head(data)

# 2. Column names are still inconsistent
#    - dplyr::rename to rename column header
data <- data %>%
  dplyr::rename(insect_sp1 = insect_species1)
head(data)

# 3. Site and year are in the same column - 
#    - tidyr::seperate to split into two columns
data <- data %>%
  # Split column site_year into two columns, called site and year, separating based on the _. 
  tidyr::separate(site_year, c("site", "year"), "_")
head(data)

# 4. Year is a character - should probably be numeric 
#    - dplyr::mutate to change a column into something else
data <- data %>%
  dplyr::mutate(year = as.numeric(year))
head(data)

# 5. Check the structure of each important column
data %>% count(month)
data %>% count(plant_species)
