###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #8: Intro to dplyr (part #2)
# - Rhodes Universtiy
# - Script written: 19/06/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Learn basic dplyr verbs for data processing
# - 2. Understand what the pipe operator does
# - 3. Understand how to make conditional columns 
# - 4. Group your data 
# - 5. Produce basic summary statistics across groups 
############################################################

# Load required libraries
library(tidyverse)
library(tidyr)
library(dplyr) # dplyr 1.0.0. 
# citation("dplyr")

# Import data
#data <- readxl::read_excel("./data_raw/messy_data.xlsx")
data <- readr::read_csv2("https://raw.githubusercontent.com/guysutton/CBC_coding_club/master/data_raw/messy_data.csv")

# Check data imported properly
str(data)
head(data)

# This data is a MESS!!! We need to clean this before we can do anything. 

###
# - Some teaser tricks 
###

# 1. Broad-scale cleaning of column names
#    - janitor::clean_names()
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
  # Split column site_year into two columns, called site and year, 
  # separating based on the '_' character. 
  tidyr::separate(site_year, c("site", "year"), "_")
# "c(_" | " ")
head(data)

######################################################################
# 1. USING THE PIPE OPERATOR -----------------------------------------
######################################################################

# Firstly, I want to go over what the pipe (%>%) operator does.
# The pipe allows us to put multiple arguments together (i.e. chain together)
# When you see a pipe, you can read it as "and then do this...

# For example, 
data %>%
  dplyr::filter(year == 2019)

# This can be read as:
# Take 'data' and then filter to keep only those rows where year is equal to 2019

# But, what if we wanted to filter for year 2019 and add a new column saying who collected the data.

# Option 1:
# Step 1: Take 'data' and then filter to keep only those rows where year 
# is equal to 2019
a <- data %>%
  dplyr::filter(year == 2019)

# Step 2: add a new column saying who collected the data.
a <- a %>%
  dplyr::mutate(collector = "Me")
a

# Option 2: Do this in one step
# Take 'data' 
data %>% # and then...
  # Filter for year = 2019
  dplyr::filter(year == 2019) %>% # and then...
  # Make a new column called collected, with "Me" as the name. 
  dplyr::mutate(collector = "Me")

#########################################################################
# 2. REFRESHER ON CONDITIONALS ------------------------------------------
#########################################################################

# These are really helpful functions to make your factors (i.e. your treatment 
# variables) or your response variables more user-friendly or cleaning up 
# your coding of variables 
# - (1) Say you have a plant_species column, and in that column, you have entered
#   the same plant name in multiple ways (e.g. Lantana camara, L. camara and L. Camara)
#   - R will treat these as 3 different plant species 
#   - Conditionals allow us to quickly convert these into a consistent naming
#     style (i.e. make all three == Lantana camara)

# - (2) Say you have abundance data for a species (i.e. counts of individuals),
#       but want to classify this into categories (e.g. SAPIA data takes counts of 
#       invasive plants and classifies them into rare, absent, abundant, very abundant).
#       - Conditionals allow us to take these counts and make them into a factor 
#       - e.g. if count > 0 but less than 5 then classify as "rare"
#              if count > 5 but less than 10, then classify as "abundant"
#       - This would be extremely painfail to do row-by-row in excel for 60 000 rows 
#         as contained in the SAPIA database currently. 


# We have two options here, 
# (1) ifelse - when we only have two outcomes (yes or no, 1/0, present/absent)
# (2) case_when - when we have > 2 outcomes (absent, present, abundant)

# (1) ifelse function
# - If x, then x, if not x, then y
# - e.g. If we want to create a new column which classifies whether an insect species       was present or absent, based on counts of its abundance. 
#   - So, if insect_sp1 > 0, we want the new column to say 1 or present,
#     and if insect_sp1 = 0, we want the new column to say 0 or absent. 

# How to return 1/0's (detailed explanation)
data %>%
  dplyr::mutate(insect_sp1_present = ifelse(
    # Which column to look in, and what is the condition?
    insect_sp1 > 0, 
    # If this condition is met, then return this,
    1, 
    # If this condition is not met, then return this,
    0)) %>%
  dplyr::select(insect_sp1, insect_sp1_present)

# How to return 1/0's (without the explanation)
data %>%
  dplyr::mutate(insect_sp1_present = ifelse(insect_sp1 > 0, 1, 0)) %>%
  dplyr::select(insect_sp1, insect_sp1_present)

# How to return present/absent's
data %>%
  dplyr::mutate(insect_sp1_present = ifelse(insect_sp1 > 0, "present", "absent")) %>%
  dplyr::select(insect_sp1, insect_sp1_present)

# (2) case_when - basically, ifelse for > 2 conditions 

# - If x, then x, if not x, then y
# - e.g. If we want to create a new column which classifies whether an insect species       was present, absent or abundant, based on counts of its abundance. 
#   - So, if insect_sp1 = 0, we want the new column to say "absent", 
#     and if insect_sp1 > 0 but less than 6, we want the new column to say "present",
#     and if insect_sp1 > 6, we want the new column to say "abundant"

data %>%
  dplyr::mutate(insect_sp1_abundance = case_when(
    # if insect_sp1 = 0, we want the new column to say "absent"
    insect_sp1 == 0 ~ "Absent",
    # if insect_sp1 > 0 but less than 6, we want the new column to say "present"
    insect_sp1 > 0 & insect_sp1 < 6 ~ "Present",
    # if insect_sp1 > 6, we want the new column to say "abundant"
    TRUE ~ "Abundant")) %>%
  dplyr::select(insect_sp1, insect_sp1_abundance)

# A more complex example using case_when, where we use multiple conditions
# within the same call. 
# - case_when() with conditional elements 
data %>%
  dplyr::mutate(which_insects_found = case_when(
    # if insect_sp1 > 0 and insect_sp2 > 0, say "both"
    insect_sp1 > 0 & insect_sp2 > 0 ~ "Both",
    # if insect_sp1 > 0 but insect_sp2 = 0, say "Only sp. 1"
    insect_sp1 > 0 & insect_sp2 == 0 ~ "Only sp. 1",
    # if insect_sp1 = 0 and insect_sp2 > 0, say "Only sp. 2"
    insect_sp1 == 0 & insect_sp2 > 0 ~ "Only sp. 2",
    # # if insect_sp1 = 0 and insect_sp1 = 0, say "Neither"
    insect_sp1 == 0 & insect_sp2 == 0 ~ "Neither")) %>%
  dplyr::select(insect_sp1, insect_sp2, which_insects_found)

# (v) Mutate across multiple columns 

# - Change all character columns into factors
data %>%
  dplyr::mutate(across(where(is.character), as.factor))

# - Sum across multiple columns
data %>%
  rowwise() %>%
  dplyr::mutate(total_abun = sum(across(starts_with("insect_")))) %>%
  dplyr::select(insect_sp1:total_abun)

# - Count distinct levels within multiple factors  
data %>%
  dplyr::summarise(across(where(is.character), n_distinct))

# (vi) Recoding factor variable levels 
a <- data %>%
  #dplyr::count(plant_species) %>%
  dplyr::mutate(plant_species = recode(plant_species,
                                       "Lantana camara" = "Lantana camara",
                                       "Lantana urtica" = "Lantana sp. 1"))

data %>%
  dplyr::mutate(month = recode(month,     
                               "Jan" = "January")) %>%
  dplyr::distinct(month)

##################### End of session # 1 #####################

##############################################################
### - Verb #3 dplyr::filter = select but for rows, not columns 
##############################################################

# (i) Filter rows based on a numeric variable 
#     - Familiar math operators for this are >, >=, <, <=, == 
#       and != (not equal to)
#     - Here, filter rows where insect_sp1 is greater than 8. 
data %>%
  dplyr::filter(insect_sp1 > 8)

# (ii) Filter rows between two numeric values
#      - Here filter rows where insect_sp1 is between 1 and 8. 
data %>%
  dplyr::filter(between(insect_sp1, 1, 8))

# (iii) Filter rows based on an exact character match 
#       - Here, filter rows for Lantana camara only 
#       - Note the use of == 
data %>%
  dplyr::filter(plant_species == "Lantana camara")

# (iv) Filter rows matching multiple character matches 
#      - Not ideal for this dataset, but say we had lots of plant
#        species and only wanted Lantana camara and Lantana urtica 
#      - Note the use of %in% - i.e. look for these characters 
#        in plant_species column. 
data %>%
  dplyr::filter(plant_species %in% c("Lantana camara",
                                     "Lantana urtica"))

# (v) Filter rows not including a match 
#     - Here, lots filter all the plant species, except Lantana camara 
data %>%
  dplyr::filter(plant_species != "Lantana camara")

# (vi) Filter rows not including a match for multipe characters (exception)
#      - Say we wanted to remove 2018 and 2019 years, and only keep 2017 data 
data %>%
  dplyr::filter(year !%in% c("2018",
                             "2019")) # !%in% is not a valid operator

data %>%
  dplyr::filter(!year %in% c("2018",
                             "2019"))

# (vii) Filter rows based on regex (regular expressions)
#       - Say we wanted to filter rows containing 'Lantana' only, 
#         let's just assume there are species other than Lantana in the data. 
#       - We say filter within plant_species column, and only return rows
#         where the character string 'Lantana' is present. 
data %>%
  dplyr::filter(str_detect(plant_species, "Lantana"))

# (viii) Filter based on multiple conditions 

# Example 1: Filter for 2017 data for Lantana urtica only 
data %>%
  dplyr::filter(year == "2017" & plant_species == "Lantana urtica")

# Example 2" Filter for 2017 data and where insect_sp1 abundance was not 0
data %>%
  dplyr::filter(year == "2017" & insect_sp1 != 0)

##############################################################
### - Verb #4 +5 dplyr::group_by and summarise = produce summary stats for groups 
##############################################################

# (i) Basic use 
#     - Here, we want to calculate mean insect_abundance for different years
data %>%
  group_by(year) %>%
  dplyr::summarise(mean_sp1 = mean(insect_sp1))

#     - Another example. calculate min insect_abundance for different years
#       and plant species. 
data %>%
  group_by(year, plant_species) %>%
  dplyr::summarise(min_sp1 = min(insect_sp1))

# (ii) Can produce multiple statistics at once 
#     - Here, we want to calculate mean and sd insect_abundance 
#       for different years
data %>%
  group_by(year) %>%
  dplyr::summarise(mean_sp1 = mean(insect_sp1),
                   sd_sp1   = sd(insect_sp1))

# (iii) Summarise across multiple columns 
#       - Here, we want to calculate mean insect_abundance 
#         for different years, for each insect sp individually 
data %>%
  group_by(year) %>%
  dplyr::summarise(across(insect_sp1:insect_sp2, mean))

# (iii) Summarise multiple variables across multiple columns 
#       - Here, we want to calculate mean and sd insect_abundance 
#         for different years, for each insect sp individually 
#       - Note the use of list to specify whihc summary statistics we want. 
data %>%
  group_by(year) %>%
  dplyr::summarise(across(insect_sp1:insect_sp2, list(mean = mean,
                                                      sd = sd)))

# What if we wanted these stats across years and plant_species? 
data %>%
  group_by(year, plant_species) %>%
  dplyr::summarise(across(insect_sp1:insect_sp2, list(mean = mean,
                                                      sd = sd)))

a <- data %>%
  group_by(year, plant_species) %>%
  dplyr::summarise(across(insect_sp1:insect_sp2, list(mean = mean,
                                                      sd = sd)))