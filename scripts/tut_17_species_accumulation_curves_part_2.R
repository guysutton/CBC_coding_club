###########################################################################
###########################################################################
###########################################################################
# Computing and plotting species accumulation curves in R -----------------
###########################################################################
###########################################################################
###########################################################################

# Today, we are going to look at how to compute and plot species 
# accumulation curves in R, using the amazing 'vegan' package. 
# Last week we covered the most basic SAC - observed richness.
# - Observed species richness (hereafter 'S') simply tells us:
#   (1) how many species we have recorded, to date, and 
#   (2) whether more surveys could yield new additional species. 
# - It DOES NOT tell us how many species could be in the community 
#   (i.e. we CANNOT extrapolate species richness estimates from S).

# This week, we are going to calculate a range of species richness 
# estimators (e.g. chao, jackknife, bootstrap). 
# - These estimators represent different mathematical models used to predict
#   how many species could be in the community being studied. 
#   - I.e. we CAN now extrapolate potential species richness.
# - You can go look at how the different estimates are calculated
#   (type: ?poolaccum into the console)
# - Briefly, they differ based on how they use the number of species 
#   that only occur at 1 or 2 sites and the number of sites sampled,
#   and the frequency that each species is recorded. 

###########################################################################
# Load packages -----------------------------------------------------------
###########################################################################

# Load packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidyr, 
               janitor,
               vegan)

###########################################################################
# Set global defaults -----------------------------------------------------
###########################################################################

# Set ggplot theme (makes nice plots)
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                  legend.position = "none"))

###########################################################################
# Load and clean data -----------------------------------------------------
###########################################################################

# Load data 
data_raw <- readxl::read_xlsx("./data_raw/tut_16_lycium_comm_data.xlsx")
head(data_raw)

# Clean data 
data_clean <- data_raw %>%
  janitor::clean_names() %>%
  dplyr::mutate(season = if_else(season == 1, "Summer", "Winter"))
head(data_clean)

###########################################################################
# Running SAC -------------------------------------------------------------
###########################################################################

# To run SAC, we need to only keep the columns with species abundances. 
data_sac <- data_clean %>%
  dplyr::select(-c(provinces, climatic_zones, site, season, haplotype))
data_sac <- as.data.frame(data_sac)

###########################################################################
# Option #1: All samples (no groups) --------------------------------------
###########################################################################

# For example, this data represents surveys for insects across RSA on a 
# shrub, Lycium ferocissimum, a biocontrol target in Australia. 
# - Last week, we saw that the 56 surveys performed so far were NOT enough
#   to record the full insect community on these plants.
# - Let's say we now want to estimate how many more insects we could find
#   if we did more surveys? 

# Run SAC
# - We calculated all the estimators last week, just didn't use them. 
# - We feed in a data frame or matrix of a species abundance matrix
#   - We must remove all the columns with site/province/ect data...
class(data_sac)
sac1 <- vegan::poolaccum(data_sac)
plot(sac1)

# This is literally it. 
# - Below, we will extract the data so that we can plot it nicely. 

# Extract chao
chao <- data.frame(summary(sac1)$chao, check.names = FALSE)
colnames(chao) <- c("N", "S", "lower2.5", "higher97.5", "std")
head(chao)

# Plot graph of chao estimated richness +- 95% CI
# - We want our trend line to reach an asymptote (flat horizontal line)
# - This would indicate that the rate at which we accumulate insects 
#   with each additional survey has approximately become 0. 
chao %>%
  ggplot(data = ., aes(x = N,
                       y = S)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = lower2.5,
                  ymax = higher97.5),
              alpha = 0.3) +
  geom_line() +
  labs(x = "No. of surveys",
       y = "Species richness",
       subtitle = "More surveys are required to find all the insects on this plant")

# Not very helpful so far, right? 
# - Estimators are only really relevant once we compare how our observed
#   richness compares with estimates. 

# We could plot observed richness on the same graph, but this doesn't
# really do it for me personally. 

# Extract S
obs <- data.frame(summary(sac1)$S, check.names = FALSE)
colnames(obs) <- c("N", "S", "lower2.5", "higher97.5", "std")
head(obs)

# Now make the plot
chao %>%
  ggplot(data = ., aes(x = N,
                       y = S)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = lower2.5,
                  ymax = higher97.5),
              alpha = 0.3) +
  geom_line() +
  # Add S richness
  geom_line(data= obs, aes(x = N,
                           S),
            linetype = "dashed") +
  labs(x = "No. of surveys",
       y = "Species richness",
       subtitle = "Observed richness (dashed line) is much lower than expected - more surveys are required")

###########################################################################
# Option #2: Plot a couple different estimators + S
###########################################################################

# The code for this is a bit long and finicky, so I have written another
# set of functions which will take care of business for us in the background. 

# Load my custom functions 
source("./functions/sp_accum_group_functions.r")

# Let's use the function to compute all richness estimators (no groups)
# - We have to specify:
#   (1) x = the data frame containing the species-abundance 
#           matrix and column identifiers (e.g. site, month, ect...).
test_est <- sp_est_all(x = data_clean)
test_est

# Now, let's plot the different richness estimators 
# - We have to specify:
#   (1) x = which is now the variable containing the richness estimators 
#           from above. 
test_plot_fun <- plot_sp_est_all(x = test_est) 
test_plot_fun 

###########################################################################
# Option #2: All samples (by group) --------------------------------------
###########################################################################

# What if we wanted to compute and plot the richness estimators for different
# groups, like we did last week for summer vs winter and Eastern Cape vs Western Cape?

# Compute the estimators by season: 

# Let's use the function to compute all richness estimators (by season groups)
# - We have to specify:
#   (1) x = the data frame containing the species-abundance 
#           matrix and column identifiers (e.g. site, month, ect...).
aaa <- sp_est_group(x = data_clean, groups = seasons)

# If we try this with climate zones, we get a awful looking error.
# - The error tells us that the number of samples in at least one of the 
#   climate zones is too small to run the SAC codes.
aaa <- sp_est_group(x = data_clean, groups = climatic_zones)  

# Looking at the raw data, we can see that only two surveys were 
# performed in the 'Bsk' region. 
# - We can filter out this region, and then run the SAC estimators for 
#   the different climate zones. 
data_clean1 <- data_clean %>%
  dplyr::filter(!climatic_zones == "Bsk")
aaa <- sp_est_group(x = data_clean1, groups = climatic_zones) 

# Now, let's plot the different richness estimators 
# - We have to specify:
#   (1) x = which is now the variable containing the richness estimators 
#           from above. 
plot_sp_est_group(x = aaa) +
  labs(fill = "Climate zones") 
