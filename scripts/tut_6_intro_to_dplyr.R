###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #6: Intro to dplyr
# - Rhodes Universtiy
# - Script written: 02/06/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Learn basic dplyr verbs for data processing
# - 2. Select specific columns
# - 3. Filter specific rows
# - 4. Make new or edit existing columns
# - 5. Produce basic summary statistics across groups 
############################################################

# Load required libraries
library(tidyverse)
library(tidyr)
library(dplyr)

# Import data
#data <- readxl::read_excel("./data_raw/messy_data.xlsx")
data <- readr::read_csv2("https://raw.githubusercontent.com/guysutton/CBC_coding_club/master/data_raw/messy_data.csv")

# Check data imported properly
str(data)
head(data)

# This data is a MESS!!! We need to clean this before we can do anything. 

###
# - Some teaster tricks 
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
  # Split column site_year into two columns, called site and year, separating based on the _. 
  tidyr::separate(site_year, c("site", "year"), "_")
head(data)

##############################################################
# - The basic 'verbs' of dplyr 
##############################################################

head(data)

##############################################################
### - Verb #1 dplyr::select = select columns of interest
##############################################################

# (i) Basic use
#     - To select a few columns just add their names in the select 
#     - The order in which you add them, will determine the order
#       in which they appear in the output.
data %>%
  dplyr::select(site, insect_sp1)

# (ii) Using chunks to save typing many column names.
#      - There are many ways to do this,
#      - Some common examples below. 

# - To select a chunk of columns, use start_col:end_col syntax
data %>%
  dplyr::select(site:month)

# - Can combine chunks and individual columns
data %>%
  dplyr::select(site:month, insect_sp1)

# - We can also deselect columns by adding a minus sign in 
#   front of the column name. 
data %>%
  dplyr::select(-month)

# - We can also deselect chunks of columns.
data %>%
  dplyr::select(-site:plant_number)

# Hmmmm. Not what you expected, right? 

# - To deselect column chunks, we must put columns into brackets
data %>%
  dplyr::select(-c(site:plant_number))

# - Deselect multiple columns 
data %>%
  dplyr::select(-c(site, plant_number))

# (iii) Selecting columns based on partial column names
#       - If you have a lot of columns with a similar structure,
#         you can use partial matching by adding:
#         - starts_with(),
#         - ends_with() or 
#         - contains() in your select statement.

# - Select using starts_with()
data %>%
  dplyr::select(starts_with("insect_"))

# - Select using ends_with() - not really helpful in this dataset
data %>%
  dplyr::select(ends_with("e"))

# - Select using contains()
data %>%
  dplyr::select(contains("_sp"))

data %>%
  dplyr::select(contains("_sp"), -plant_species)

# (iv) Selecting columns based on their data type
data %>%
  dplyr::select(where(is_numeric))

# (v) Reorder columns 
#     - Order of items in select will determine placement 
data %>%
  dplyr::select(year, site:insect_sp2)

# Select all the columns after some column, 
# wihtout specifying last column name 
data %>%
  dplyr::select(year, site:ncol(.))

# (vi) Rename some column names 
data %>%
  dplyr::select(site, 
                year, 
                month, 
                insect_sp1)

data %>%
  dplyr::select(site, 
                year, 
                month, 
                insect_sp3 = insect_sp1)

##############################################################
### - Verb #2 dplyr::mutate = manipulating columns 
##############################################################

# (i) Basic use
#     - 1. Make a new column
data %>%
  dplyr::mutate(sp1_present = insect_sp1 > 0) %>%
  select(insect_sp1, sp1_present)

#     - 2. Update an existing column 
data %>%
  dplyr::mutate(year = year + 1)

# (ii) Row-wise calculations 
data %>%
  dplyr::mutate(total_ins_abun = sum(insect_sp1, insect_sp2)) %>%
  select(insect_sp1, insect_sp2, total_ins_abun)

data %>%
  # Must add rowwise() %>%
  rowwise() %>%
  dplyr::mutate(total_ins_abun = sum(insect_sp1, insect_sp2)) %>%
  select(insect_sp1, insect_sp2, total_ins_abun)

# (iii) ifelse
data %>%
  dplyr::mutate(insect_sp1_present = ifelse(insect_sp1 > 0, 1, 0)) %>%
  dplyr::select(insect_sp1, insect_sp1_present)

# (iv) ifelse for > 2 conditions 
data %>%
  dplyr::mutate(insect_sp1_abundance = case_when(
    insect_sp1 == 0 ~ "Absent",
    insect_sp1 > 0 & insect_sp1 < 6 ~ "Present",
    TRUE ~ "Abundant")) %>%
  dplyr::select(insect_sp1, insect_sp1_abundance)

# - case_when() with conditional elements 
data %>%
  dplyr::mutate(which_insects_found = case_when(
    insect_sp1 > 0 & insect_sp2 > 0 ~ "Both",
    insect_sp1 > 0 & insect_sp2 == 0 ~ "Only sp. 1",
    insect_sp1 == 0 & insect_sp2 > 0 ~ "Only sp. 2",
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

# - Count distinct levels within multiple factors (more on summarise later) 
data %>%
  dplyr::summarise(across(where(is.character), n_distinct))

# (vi) Recoding factor variable levels 
data %>%
  dplyr::count(plant_species) %>%
  dplyr::mutate(plant_species = recode(plant_species,
                                       "Lantana camara" = "Lantana camara",
                                       "Lantana urtica" = "Lantana sp. 1"))


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
