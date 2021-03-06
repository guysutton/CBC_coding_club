###########################################################################
###########################################################################
###########################################################################
# Computing and plotting species accumulation curves in R -----------------
###########################################################################
###########################################################################
###########################################################################

# Today, we are going to look at how to compute and plot species 
# accumulation curves in R, using the amazing 'vegan' package. 
# - This analysis is particularly relevant in our research group
#   because we are often looking for potential biocontrol agents
#   in the native range, and SAC's allow us to evaluate when to stop 
#   surveys, or where to continue or surveys. 
# - For example, (1) We could ask whether the 40 surveys we performed
#                    over the past year were sufficient to record all species
#                    on a host plant? If so, we could then stop our surveys. 
# - Or,          (2) we could do 20 surveys during each of summer and winter
#                    this year, and want to ask whether we should be surveying 
#                    in both seasons again next winter.
#                       - SAC's allow us to answer these questions. 

# In this session, we are going to cover the most basic SAC - observed richness.
# - Observed species richness (hereafter 'S') simply tells us:
#   (1) how many species we have recorded, to date, and 
#   (2) whether more surveys could yield new additional species. 
# - It DOES NOT tell us how many species could be in the community 
#   (i.e. we CANNOT extrapolate species richness estimates from S).
#   - Next week, we will cover how to extrapolate species richness 
#     (part 2). 

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
  janitor::clean_names()
head(data_clean)

# Extra credit points:
# What is the coding practice that is being broken here? 
# - Hint: look at the column names and types (<dbl>)
# - Fly-swotter time!!!
data_clean <- data_clean %>%
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
# - Here, we are asking, across all provinces, surveys, climatic_zones,
#   sites and plant haplotypes, have we recorded the full insect community
#   (i.e. all the potential biocontrol agents)? 

# Run SAC
# - We feed in a data frame or matrix of a species abundance matrix
#   - We must remove all the columns with site/province/ect data...
class(data_sac)
sac1 <- vegan::poolaccum(data_sac)
plot(sac1)

# This is literally it. 
# - Below, we will extract the data so that we can plot it nicely. 

# Extract observed richness (S) estimate 
obs <- data.frame(summary(sac1)$S, check.names = FALSE)
colnames(obs) <- c("N", "S", "lower2.5", "higher97.5", "std")
head(obs)

# Plot graph of observed richness +- 95% CI
# - We want our trend line to reach an asymptote (flat horizontal line)
# - This would indicate that the rate at which we accumulate insects 
#   with each additional survey has approximately become 0. 
obs %>%
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

###########################################################################
# Option #2: All samples (by group) --------------------------------------
###########################################################################

# Run SAC by group
# - Below, I have written a chain that will:
#   (1) split our data into groups (group_by and nest),
#   (2) apply the species accumulation curve code to each group (first mutate),
#   (3) extract the SAC data for each group into a data frame 
#       (as.data.frame and second mutate), and 
#   (4) clean the extracted data frame so it is user-friendly 

# Here, let's ask whether we should be surveying in both 
# seasons (summer and winter). 
sac_by_group <- data_clean %>%
  # Which groups do you want different SAC's for? 
  dplyr::group_by(season) %>%
  # Splits the data into the different groups 
  tidyr::nest() %>%
  # Run the species accumulation curves by group
  dplyr::mutate(data = purrr::map(data, vegan::poolaccum)) %>%
  # Extract the observed species richness estimator (denoted by S)
  dplyr::mutate(data_df = purrr::map(data,
                                     ~ data.frame(summary(.)$S,
                                                  check.names = FALSE))) %>%
  # Drop unnecessary columns
  dplyr::select(-c(data)) %>%
  # Convert the lists back into a data frame
  unnest(cols = c(data_df)) %>%
  # Rename the columns
  dplyr::rename(group = season,
                N = N,
                S = S,
                lower_ci = `2.5%`,
                upper_ci = `97.5%`,
                std_dev = Std.Dev) %>%
  # Make the groups into a factor
  dplyr::mutate(group = as.factor(group))
head(sac_by_group)

# Now let's make a plot 
# Plot graph of observed richness +- 95% CI (both lines on one plot)
sac_by_group %>%
  ggplot(data = ., aes(x = N,
                       y = S,
                       group = group)) +
  geom_ribbon(aes(ymin = lower_ci,
                  ymax = upper_ci,
                  fill = group),
              alpha = 0.3) +
  geom_line(aes(colour = group)) +
  labs(x = "No. of surveys",
       y = "Species richness",
       fill = "Season") +
  guides(colour = FALSE) + 
  theme(legend.position = "right")

# Save graph to PC
ggsave("./figures/sac_by_group_same_plot.png",
       dpi = 600,
       height = 6, 
       width = 6)

# Plot graph of observed richness +- 95% CI (different panels for groups)
sac_by_group %>%
  ggplot(data = ., aes(x = N,
                       y = S,
                       group = group)) +
  geom_ribbon(aes(ymin = lower_ci,
                  ymax = upper_ci,
                  fill = group),
              alpha = 0.3) +
  geom_line(aes(colour = group)) +
  labs(x = "No. of surveys",
       y = "Species richness",
       fill = "Season") +
  guides(colour = FALSE) + 
  # Different panels for summer and winter
  facet_wrap(~ group, ncol = 2) + 
  theme(legend.position = "right")

# Save graph to PC
ggsave("./figures/sac_by_group_facet.png",
       dpi = 600,
       height = 6, 
       width = 8)

###########################################################################
# Section #3: Using custom built functions --------------------------------
###########################################################################

# Load my custom functions 
source("./functions/sp_accum_group_functions.r")

# Let's use the function to plot SAC
# - We have to specify:
#   (1) x = the data frame containing the species-abundance 
#           matrix and column identifiers (e.g. site, month, ect...).
#   (2) groups = the column name which we want to compute different
#                SAC's for. 
test_sac <- sp_accum_group(x = data_clean, 
                           groups = provinces)
test_sac

# Now, let's plot the SAC's for the different seasons 
# - We have to specify:
#   (1) x = which is now the variable containing the SAC's by groups 
#           from above. 
test_plot_fun <- plot_sp_accum_group(x = test_sac) 
test_plot_fun 

# We can then edit the graph using standard ggplot syntax
test_plot_fun +
  labs(fill = "Season") + scale_x_continuous(limits = c(0, 30),
                                             breaks = seq(0, 30, 10))

# Using the functions is quite a lot easier, and quicker! :) 
# - Computing and plotting the SAC's now requires 2 lines of code. 
# - Lesson: Learn how to write your own functions (Thanks Clarke!!!)
#           (I would never have been able to do so a year ago without 
#            his help)