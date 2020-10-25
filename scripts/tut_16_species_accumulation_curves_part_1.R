###########################################################################
###########################################################################
###########################################################################
# Computing and plotting species accumulation curves in R -----------------
###########################################################################
###########################################################################
###########################################################################

# Today we are going to look at how to compute and plot species 
# accumulation curves in R, using the amazing 'vegan' package. 
# - This analysis is particularly relevant in our research group
#   because we are often looking for potential biocontrol agents
#   in the native range, and SAC's allow us to evaluate when to stop 
#   surveys, or where to continue or surveys. 
# - For example, we could do 20 surveys during each of summer and winter
#   this year, and want to ask whether we should be surveying in both seasons
#   again next winter.
#   - SAC's allow us to answer this question. 

# In this session, we are going to cover the most basic SAC - observed richness.
# - Observed species richness (hereafter 'S') simply tells us:
#   (1) how many species we have recorded, to date, and 
#   (2) whether more surveys could yield new additional species. 
# - It DOES NOT tell us how many species could be in the community 
#   (i.e. we CANNOT extrapolate species richness estimates from S). 

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
sac1 <- poolaccum(data_sac)
head(sac1)
plot(sac1)

# Extract observed richness estimate 
obs <- data.frame(summary(sac1)$S,check.names = FALSE)
colnames(obs) <- c("N", "S", "lower2.5", "higher97.5", "std")
head(obs)

# Plot graph of observed richness +- 95% CI
# - We want our trend line to reach an asymptote (flat horizontal line)
# - This would indicate that the rate at which we accumulate insects 
#   with each additional survey has approximately become 0. 
obs %>%
  ggplot(data = ., aes(x = N,
                       y = S)) +
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
  facet_wrap(~ group, ncol = 2) + 
  theme(legend.position = "right")

# Save graph to PC
ggsave("./figures/sac_by_group_facet.png",
       dpi = 600,
       height = 6, 
       width = 8)



#########################################
# Idea of if you had to do this manually: 
#########################################

# - Bit of a hack for now, but it works. 

# Run SAC for first group (summer)
data_sac_summer <- data_clean %>%
  dplyr::filter(season == "1") %>%
  dplyr::select(-c(provinces, climatic_zones, 
                   season, site, haplotype))

sac1_summer <- poolaccum(data_sac_summer,
                         permutations = 999)
obs_summer <- data.frame(summary(sac1_summer)$S,check.names = FALSE)
colnames(obs_summer) <- c("N", "S", "lower2.5", "higher97.5", "std")

# Run SAC for second group (winter)
data_sac_winter <- data_clean %>%
  dplyr::filter(season == "2") %>%
  dplyr::select(-c(provinces, climatic_zones, 
                   season, site, haplotype))

sac1_winter <- poolaccum(data_sac_winter,
                         permutations = 999)
obs_winter <- data.frame(summary(sac1_winter)$S,check.names = FALSE)
colnames(obs_winter) <- c("N", "S", "lower2.5", "higher97.5", "std")

# Combine summer and winter SAC's
comb_obs <- bind_rows(obs_summer, 
                      obs_winter, 
                      .id = "id") 
head(comb_obs)

