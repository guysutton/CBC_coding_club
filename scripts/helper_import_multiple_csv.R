# Import multiple .csv files simultaeously and combine
# into a single dataframe

# Load required packages  -------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               fs)

# Now import files --------------------------------------------------------

# Step 1: Give R the path to the folder containing the .csvs (data directory)
data_dir <- "./data_raw/import_multiple_csv_data/"

# Step 2: Store file names in a list 
# regexp tells R we only want the .csv files (NB when many file types in 
# the same yellow folder)
files_to_import <- fs::dir_ls(data_dir, 
                              regexp = "\\.csv$")

# Step 3: Import all the .csv files and store in one data frame 
data_combined <- files_to_import %>% 
  # For each .csv, import it into R and store in the data frame called 
  # data_combined
  # ~ means apply the function to ... 
  # So we are applying the readr::read_csv2 function to each element of 
  # files_to_import 
  purrr::map_dfr(~ readr::read_csv2(.))
head(data_combined)

