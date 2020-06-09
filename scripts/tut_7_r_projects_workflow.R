######################################################################
# Things not to include in your R script  ----------------------------
######################################################################

# 1. rm(list = ls())

# Bad 
# Supposed to clear your R session of all previous items.
# Doesn't actually do this - will keep all changes your make 
# to default R parameters, ect... Very dangerous! 
# Will also wreck your collaborators R sessions. 

# Good
# Restart your R session frequently (I do this every 10-20 minutes, 
# and re-run my script from the start)
# - Restarting R (i.e. Click on Session > Restart R, or CTRL + SHIFT + F10),
#   will clear everything from your R console (it does what you hoped 
#   rm(list...) would do!)
# - Obvs, if you are running computationally difficult analyses (i.e.
#   something that requires hours/days to run, then this advice does 
#   not apply) - Restarting your R session often would be a disaster! 

# 2. Absolute file paths 

# Bad 
mydata <- readr::read_csv("C:/User/h1f41/My Documents/PhD/experiment 1/data/experiment_data_raw.csv")

# This is not reproducible, and will make your life difficult when you 
# inevitably move the folder for this analysis, or rename/move the data 
# Also, this will make your collaborators very angry. 
# - Remember, I will have to change the file path to somewhere on MY pc.
# - The time required and frustration to get the file path correct all adds up. 

# Good 
# Use an R project for your analyses.
# The R project is a self-contained analysis script. 
# I use an individual R project for each paper/chapter I write. 
# - I will analyse all the data/experiments from a particular chapter in 
#   one R project.
# - Seem people may prefer a project per experiment - that seems unnecessary to 
#   me, but you do you! 

# Remember to always open your R scripts/markdown documents using the R project
# file (i.e. your_analysis.Rproj) - look for that file extension. 
# - Refer to the video on this session for a reminder. 

# R projects allow us to specifcy a relative file path 
# Remember, this is relative to the yellow folder which contains the 
# .Rproj file. 

# Good data import practise for a .csv
# use read_csv when your file is seperated by commas
# use read_csv2 when your file is seperated by semi colons (;)
data <- readr::read_csv2("./data_raw/my_dataset.csv")

# Good data import practise for a .xslx
data <- readxl::read_excel("./data_raw/my_dataset.xlsx")

# You can also use base R alternatives, and as Richard pointed out,
# you can specificy the seperator
data <- read.csv("./data_raw/my_dataset.xlsx", sep = ",")
data <- read.csv("./data_raw/my_dataset.xlsx", sep = ";")

# I use tidyverse-centric functions for pretty much everything
# (eg read_csv instead of read.csv) - you pick whatever you prefer. 
# Much over muchness. 

# 3. Loading packages

# Remember, you only need to install a package once 
# e.g. 
install.packages("raster")

# Thereafter, at the top of your script, you need to 
# load in that package each time you restart R
library(raster)

# DO NOT use require(raster)
# - require does not return an error if the package did not load correctly.

# DO NOT have install.packages in your script
# install.packages in the console 

# Bad 
install.packages("raster")
library(raster)

# Also bad
require(raster)

# Good
library(raster)

######################################################################
# How to access R's help file? ---------------------------------------
######################################################################

# Really good suggestion from Kim about learning how to figure out 
# how to use particular functions
# - See what arguments the function takes 
# - Structure of each argument 
# - Example usage at the bottom of the page 

# To access R's built-in help files:
?read.csv

# If you don't have the package loaded containing that function, 
# it will not work. 
# In this case, either load that library and try again, 
# or you can use:
??[function_name]

# e.g. If we wanted to read up about a function called 'mask'
??mask # This should be a function in the raster function. 


######################################################################
# Tidy data ----------------------------------------------------------
######################################################################

# See the video for example.
# I will also put a link onto the Git to Hadley Wickham's tidy data paper.
# Really good paper, but I guess you need to be seriously interested 
# in this sort of stuff to read it. 

# Just remember:
# 1. Each variable in its own column
# 2. Each observation should be its own row. 

# I will put an example on the Git of good (tidy) vs bad (untidy) 
# data sheets. 

